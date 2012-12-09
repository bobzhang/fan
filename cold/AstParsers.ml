open Format
type key = string 
type effect = unit -> unit 
let applied_parsers: (string* effect) Queue.t = Queue.create ()
let registered_parsers: (key,effect) Hashtbl.t = Hashtbl.create 40