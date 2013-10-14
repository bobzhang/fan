


let dir_table : (Ftoken.name , unit Ftoken.expand_fun) Hashtbl.t =
  Hashtbl.create 50
    




let handle_quot (x:Ftoken.quot) : unit =
  let handler =
    try Hashtbl.find dir_table x.name
    with Not_found -> Locf.failf x.loc "Unfound directive language %s"
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
