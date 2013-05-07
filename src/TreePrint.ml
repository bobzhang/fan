

open Format

let pp = fprintf

(* [print_node] prints one node and [print_sons] its children.
   [pref] is the prefix to output at the beginning of line
   and [start] is the branching drawing (["+-"] the first time, 
   and then ["|-"]). *)
let rec print_node decomp pref f t = 
  let (s, sons) = decomp t in begin
    pp f "%s" s;
    if sons <> [] then 
      let w = String.length s in
      let pref' = pref ^ String.make (w + 1) ' ' in
      match sons with
      | [t'] ->  pp f "---%a" (print_node decomp (pref' ^ "  ")) t'
      | _ -> pp f "-%a" (print_sons  "+-" decomp pref') sons 
      else ()
    end
and print_sons (start:string) (decomp:'a -> (string * 'a list))
    (pref:string) f = function
      | [] ->  () (* when entering into foreset *)
      | [s] -> pp f "`-%a" (print_node decomp (pref ^ "  ")) s
      | [s :: sons] ->  
          pp f "%s%a@\n%s%a"
            start (print_node decomp (pref ^ "| ")) s
            pref  (print_sons "|-"  decomp  pref ) sons 

        



