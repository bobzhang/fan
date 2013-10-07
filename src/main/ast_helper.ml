



(** Ast processing library with minimial dependency while not
    complex type signature
 *)  


(** connecting a list of nodes from right to the left
  It assumes that the associativity is right.

  For example,
  {[
   com_of_list [ `Int(_loc,2 ); `Int (_loc,3); `Int (_loc,4) ]
   `Com (, `Int (, 2), `Com (, `Int (, 3), `Int (, 4)))
   ]}
  
  Note that [com_of_list] is making use of [of_listr]

  *)
let rec of_listr f xs =
  match xs with
  | [] -> invalid_arg "of_listr empty"
  | [t] -> t
  | t :: ts -> f t (of_listr f ts)

(**
   connecting a list of nodes from left to right.
   It assumes that the associativity is to the left.
   {[
   Ast_gen.appl_of_list [`Int(_loc,2); `Int (_loc,3); `Int (_loc,4) ];;
   `App (, `App (, `Int (, 2), `Int (, 3)), `Int (, 4))
   ]}
 *)
let rec of_listl f xs =
  match xs with
  | [] -> invalid_arg "of_listl empty"
  | [t] -> t
  | x::y::xs -> of_listl f (f x y :: xs)

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast_helper.cmo" *)
(* end: *)
