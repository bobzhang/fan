open Astf
open Ast_gen


  
type key = string

type expander =  exp -> exp

let macro_expanders: (key,expander) Hashtbl.t = Hashtbl.create 40 


let register_macro (k,f) =  Hashtbl.replace macro_expanders k f

let rec fib = function
  | 0 | 1 ->  1
  | n when n > 0 -> fib (n-1) + fib (n-2)
  | _ -> invalid_arg "fib" 


let fibm  y =
  match y with
  | %exp{$int:x}  -> %exp{ $int'{fib @@ int_of_string x }}
  |  x -> let _loc = loc_of x in %exp{fib $x } ;;

register_macro ("FIB",fibm);;      


  

let macro_expander = object(self)
  inherit Astf_map.map as super
  method! exp = with exp function
    | %{ $uid:a $y } ->
        (let try f = Hashtbl.find macro_expanders a in
        self#exp (f y)
        with Not_found -> %{ $uid:a ${self#exp y}})
    | e -> super#exp e 
end

(* Ast_filters.register_stru_filter ("macro", macro_expander#stru);   *)

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast_macros.cmo" *)
(* end: *)
