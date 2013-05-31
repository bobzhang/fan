
open FToken

let dir_table =
  Hashtbl.create 50
    


let handle (loc:FLoc.t) (v: quotation ) : unit =
  match v with
  | `QUOTATION( (_,base),_,_,contents) ->
      let try handler = Hashtbl.find dir_table base in
      handler contents
      with Not_found -> 
        FLoc.errorf loc "Unfound directive language %s" base


let register_dir (v,f) =
  if Hashtbl.mem dir_table v then
    Format.eprintf "%s already registered" v 
  else
    Hashtbl.add dir_table v f
