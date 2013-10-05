open Util
(* Add something to the above, make sure it ends with a slash. *)
let add str =
  if str <> "" then
    let str =
      if String.get str ((String.length str)-1) = '/'
      then str else str ^ "/" in
    Ref.modify FConfig.include_dirs (fun x -> cons str x)
