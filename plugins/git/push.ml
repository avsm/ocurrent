open Lwt.Infix

type t = unit

let id = "git-push"

module Key = struct
  type t = {
    endpoint: string;
    target_branch: string;
  } [@@deriving yojson]

  let v ~endpoint ~target_branch = { endpoint; target_branch }
  
  let pp f t = Yojson.Safe.pretty_print f (to_yojson t)

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Commit
module Outcome = Current.Unit

let publish () job {Key.endpoint; Key.target_branch} v =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let repo = Commit.repo v |> Fpath.to_string in
  let branch = "+" ^ target_branch in
  let cmd = "git", [|"git"; "-C"; repo; "push"; endpoint; branch |] in
  Current.Process.exec ~cancellable:true ~job cmd

let pp f (key, value) = Fmt.pf f "Push %a: %a" Key.pp key Value.pp value

let auto_cancel = false
