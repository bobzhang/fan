
include Filename

let find_in_path ~path name =
  if not @@ Filename.is_implicit name then
    if Sys.file_exists name then Some name
    else None
  else
    Listf.find_map
      (fun x ->
        let fullname = concat  x name in
        if Sys.file_exists fullname then
          Some fullname
        else None) path 


let find_in_path_uncap ~path name =
  let uname = String.uncapitalize name in
  let rec try_dir =
    function
      | [] -> raise Not_found
      | dir::rem ->
          let fullname = Filename.concat dir name
          and ufullname = Filename.concat dir uname in
          if Sys.file_exists ufullname then ufullname
          else if Sys.file_exists fullname then fullname
          else try_dir rem
  in try_dir path

let expand_directory ~std s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat std
      (String.sub s 1 (String.length s - 1))
  else s


(* local variables: *)
(* compile-command: "pmake filenamef.cmo" *)
(* end: *)
