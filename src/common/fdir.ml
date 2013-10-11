


let dir_table =
  Hashtbl.create 50
    



let handle_dir (loc:FLoc.t) ((base:Ftoken.name),contents ) : unit =
  (try
     let handler = Hashtbl.find dir_table base in
     fun ()  -> handler loc contents
   with
   | Not_found  ->
       (fun ()  -> FLoc.failf loc "Unfound directive language %s" @@ Ftoken.string_of_name base)
  ) ()



let register (v,f) =
  if Hashtbl.mem dir_table v then
    Format.eprintf "%s already registered" @@ Ftoken.string_of_name v 
  else
    Hashtbl.add dir_table v f;;

(* local variables: *)
(* compile-command: "pmake fdir.cmo" *)
(* end: *)
