open Current.Syntax

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

module Git = Current_git_test
module Docker = Current_docker_test

let test_commit =
  Git.Commit.v ~repo:"my/project" ~hash:"123"

module Commit_var = Current.Var(Git.Commit)

let with_analysis ~name ~i (t : unit Current.t) =
  let data =
    let+ a = Current.Analysis.get t in
    Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp a);
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let path = Current.return (Fpath.v (Fmt.strf "%s.%d.dot" name !i)) in
  Current_fs.save path data

let current_watches = ref []

let cancel msg =
  let name w = Fmt.strf "%a" Current.Input.pp_watch w in
  match List.find_opt (fun w -> name w = msg) !current_watches with
  | None -> Fmt.failwith "No such watch %S." msg
  | Some w ->
    match w#cancel with
    | Some c -> c ()
    | None -> Fmt.failwith "Watch %S cannot be cancelled" msg

let ready i = Lwt.state i#changed <> Lwt.Sleep

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ~name v actions =
  Git.reset ();
  Docker.reset ();
  (* Perform an initial analysis: *)
  let head = Commit_var.create ~name:"head" (Ok test_commit) in
  let i = ref 1 in
  let trace x watches =
    current_watches := watches;
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.unit "()")) x);
    Logs.info (fun f -> f "@[<v>Depends on: %a@]" Fmt.(Dump.list Current.Input.pp_watch) watches);
    begin
      match List.find_opt ready watches with
      | Some i -> Fmt.failwith "Input already ready! %a" Current.Input.pp_watch i
      | None -> ()
    end;
    begin
      try actions !i
      with Exit ->
        List.iter (fun w -> w#release) watches;
        raise Exit
    end;
    incr i;
    if not (List.exists ready watches) then raise Exit
  in
  try Lwt_main.run @@ Current.Engine.run ~trace (fun () -> with_analysis ~name ~i @@ v (Commit_var.get head))
  with Exit ->
    Docker.assert_finished ()
