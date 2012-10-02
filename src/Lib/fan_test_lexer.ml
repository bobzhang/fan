open Format;
(* type_defs := .$ <:x</ type .$ x $ />> />>  >> $ *)
(* ; *)

(* value mee_of_str s =
 *   <:expr< Ast.ExId _loc .$  <:expr< Ast.IdUid _loc .$str:s$. >>   $. >>; *)

value mep_of_str s =
  <:expr< Ast.PaId _loc .$  <:expr< Ast.IdUid _loc .$str:s$. >>   $. >>;
  
(* value mee_of_str2 x =
 *   <:expr< <:expr< .$ .$uid:x$. $. >>  >> ; *)

value mep_of_str2 x =
  <:expr< <:patt<  .$uid: .$x$. $.  >> >> ;
(* "<:expr< <:expr< .$ .$uid:x$.$. >>  >> " *)










