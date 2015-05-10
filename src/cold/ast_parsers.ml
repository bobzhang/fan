open Util
type key = string
type effect = unit -> unit
let applied_parsers: (string* effect) Queue.t = Queue.create ()
let registered_parsers: (key,effect) Hashtbl.t = Hashtbl.create 40
let use_parsers =
  function
  | ls ->
      List.iter
        (function
         | s ->
             ((try
                 let u = Hashtbl.find registered_parsers s in
                 function
                 | () -> let _ = Queue.add (s, u) applied_parsers in u ()
               with
               | Not_found  ->
                   (function
                    | () -> failwithf "parser %s is not registered" s))) ())
        ls
let register_parser =
  function
  | (k,f) ->
      if Hashtbl.mem registered_parsers k
      then Format.eprintf "%s is already a registered parser" k
      else Hashtbl.replace registered_parsers k f
