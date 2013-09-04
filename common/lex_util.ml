

open LibUtil
open Automata_def
exception Memory_overflow



module Id =   struct
  type t = ident
  let compare (x:t) y =
    match x,y with `Lid(_,id1),`Lid(_,id2) -> String.compare id1 id2
end
  
module IdSet = Set.Make (Id)
    
(*********************)
(* Variable cleaning *)
(*********************)

(* Silently eliminate nested variables *)

let rec do_remove_nested (to_remove:IdSet.t) (x:concrete_regexp) =
  match x with 
  | Bind (e,x) ->
      if IdSet.mem x to_remove then
        do_remove_nested to_remove e
      else
        Bind (do_remove_nested (IdSet.add x to_remove) e, x)
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) ->
      Sequence
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Alternative (e1, e2) ->
      Alternative
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Repetition e ->
      Repetition (do_remove_nested to_remove  e)
        
