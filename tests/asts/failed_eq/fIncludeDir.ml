open Util
let add str =
  if str <> ""
  then
    let str =
      if (str.[(String.length str) - 1]) = '/' then str else str ^ "/" in
    Ref.modify Configf.include_dirs (fun x  -> cons str x)