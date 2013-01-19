(* +-----------------------------------------------------------------+
   | Shared by [ctyp] [expr] and [patt]                              |
   +-----------------------------------------------------------------+ *)



let app a b = {| $a $b |};

  
(* let comma a b = {| $a, $b |};   *)

(*
   Left associativity
 *)  
(* let ( <$ ) = app; *)


(*
  left associative 
   {[
   apply {| f |} [ {| a |} ; {| b |} ] ;
   f a b
   ]}
 *)      
let rec apply acc = fun
  [ [] -> acc
  | [x::xs] ->  apply (app acc x) xs];

(* let sem a b = *)
(*   let _loc = FanLoc.merge (GETLOC a) (GETLOC b) in *)
(*   {| $a; $b  |} ; *)

  
(*
   @raise Invalid_argument 

  decompose a left associative  application to  an identifier  and
   a list of small ast nodes.

  Unlike com [a,(b,c),d], the Ast node will introduce [Tup],
  for [f (g a) b], there is no intermediate node.
  
  {[
  list_of_app {| (f.g#u) a b |} |> app_of_list;

  f.g#u a b

  list_of_app {| f (g a) b |}  |> app_of_list ;
  f (g a) b
  ]}
 *)          

let  list_of_app  ty  =
  let rec  loop t acc =
    match t with  
    [ {| $t1 $t2 |} -> loop t1 [t2 ::acc]
    | {| |} -> acc (* remove nil case *)
    | i ->  [i::acc] ] in 
  loop ty [];    

  
(* right associative comma
   {[

   list_of_com {| 1,2,3|} |> com_of_list ;
   1, 2, 3

   list_of_com {| 1,(2,3),4|} |> tuple _loc |> FanBasic.p_expr f;
   (1, (2, 3), 4)
   ]}
 *)  
(* let list_of_com ty =  *)
(*   let rec loop t acc = match t with *)
(*     [ {| $t1 , $t2 |} -> (\* loop t2 [t1::acc]*\) *)
(*        [t1 :: (loop t2 acc) ]   *)
(*     | {| |} -> acc  *)
(*     | i -> [i::acc] ] in *)
(*   loop ty []; *)

(*
  right associative sem
  [ {| 1;(2;3);4 |} ] will introduce an intermedate [Seq] here,
  but dumping it is illegal

  Example:
  {[
   list_of_com {| 1;(2;3);4|} |> com_of_list |> eprint;
   1; (2; 3); 4
  ]}
 *)
(* let list_of_sem ty =  *)
(*   let rec loop t acc = *)
(*     match t with *)
(*     [ {| $t1 ; $t2 |} ->  [t1 :: (loop t2 acc)]   *)
(*     | {| |} -> acc  *)
(*     | i -> [i::acc] ] in *)
(*   loop ty []; *)

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
let app_of_list = fun 
  [ [] -> {| |}
  | l -> List.reduce_left app l ] ;  

  
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

































