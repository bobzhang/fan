(* +-----------------------------------------------------------------+
   | Shared by [ctyp] [expr] and [patt]                              |
   +-----------------------------------------------------------------+ *)



  
(*
  It tries to decompose an app into the first argument, and the rest argument

  Examples:
  {[
  vew_app [] {| a b c d|};
  - : expr * L.Expr.Ast.expr list =
  (ExId (, Lid (, "a")),
  [ExId (, Lid (, "b")); ExId (, Lid (, "c")); ExId (, Lid (, "d"))])
  ]}
 *)
let rec view_app acc = fun
  [{|$f $a |} -> view_app [a::acc] f
  | f -> (f,acc)];
(*
  {[
  ]}
 *)
(* let app_of_list = fun  *)
(*   [ [] -> {| |} *)
(*   | l -> List.reduce_left app l ] ;   *)

  
(*
   {[
   tuple_of_list [ {| a |}; {| b |}; {| c |} ] |> eprint
   (a, b, c)
   ]}
 *)
(* let tuple_of_list = fun *)
(*   [ [] -> invalid_arg "tuple_of_list while list is empty" *)
(*   | [x] -> x *)
(*   | xs -> {| $(tup:com_of_list xs) |} ]; *)

































