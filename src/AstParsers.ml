open Format;

type key = string;
type effect = unit -> unit;
  
let applied_parsers: Queue.t (string * effect) = Queue.create ();

let registered_parsers: Hashtbl.t key effect = Hashtbl.create 40;

(* let use_filter s = *)
(*   let u  *)
    



















