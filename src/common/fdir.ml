


let dir_table : (Ftoken.name , FLoc.t -> string option -> string -> unit) Hashtbl.t =
  Hashtbl.create 50
    



let handle_dir (loc:FLoc.t) ((base:Ftoken.name),contents ) : unit =
  (try
     let handler = Hashtbl.find dir_table base in
     fun ()  -> handler loc None contents
   with
   | Not_found  ->
       (fun ()  -> FLoc.failf loc "Unfound directive language %s" @@ Ftoken.string_of_name base)
  ) ()

let handle_quot (x:Ftoken.quot) : unit =
  let handler =
    try Hashtbl.find dir_table x.name
    with Not_found -> FLoc.failf x.loc "Unfound directive language %s"
      @@ Ftoken.string_of_name x.name
  in
  Ftoken.quot_expand handler x 
  (* handler x.loc x.content *)
    

let register (v,f) =
  if Hashtbl.mem dir_table v then
    Format.eprintf "%s already registered" @@ Ftoken.string_of_name v 
  else
    Hashtbl.add dir_table v f;;

(* local variables: *)
(* compile-command: "pmake fdir.cmo" *)
(* end: *)
