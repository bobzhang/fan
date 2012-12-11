open LibUtil
type key = string 
type effect = unit -> unit 
let applied_parsers: (string* effect) Queue.t = Queue.create ()
let registered_parsers: (key,effect) Hashtbl.t = Hashtbl.create 40
let use_parsers ls =
  List.iter
    (fun s  ->
       (try
          let u = Hashtbl.find registered_parsers s in
          fun ()  -> let _ = Queue.add (s, u) applied_parsers in u ()
        with
        | Not_found  ->
            (fun ()  -> failwithf "parser %s is not registered" s)) ()) ls
let register_parser (k,f) = Hashtbl.replace registered_parsers k f