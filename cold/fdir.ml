let dir_table = Hashtbl.create 50

let handle_dir (loc : FLoc.t) (base,contents) =
  ((try
      let handler = Hashtbl.find dir_table base in
      fun ()  -> handler loc contents
    with
    | Not_found  ->
        (fun ()  -> FLoc.errorf loc "Unfound directive language %s" base)) () : 
  unit )

let register (v,f) =
  if Hashtbl.mem dir_table v
  then Format.eprintf "%s already registered" v
  else Hashtbl.add dir_table v f