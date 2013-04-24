open LibUtil;
type key = string;
type effect = unit -> unit;
  
let applied_parsers: (string * effect) Queue.t  = Queue.create ();

let registered_parsers: (key,effect) Hashtbl.t = Hashtbl.create 40;

let use_parsers ls =
  List.iter (fun s ->
    let try u = Hashtbl.find registered_parsers s in
    let _ = Queue.add (s,u) applied_parsers in
    u ()
    with Not_found -> failwithf "parser %s is not registered" s) ls;
    
let register_parser (k,f) =
  if Hashtbl.mem registered_parsers k then
    Format.eprintf "%s is already a registered parser" k
  else 
    Hashtbl.replace registered_parsers k f;
(*   let u  *)
    



















