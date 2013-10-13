

(** FAst a module for processing abstract syntax.

    It makes use of  structual polymorphism aggresively,
    the signature is quite complex due the use of [loc_of],
    see [astLibN] for the same functionality but processing ast without locations
    and hence a much simplified signature

    This module is not recommended to be used aggresively, since it complexes type system.
    It's recommended to use the ast processing library [ast_libn] without location considered.
    
 *)
  

  




(** Re-export *)  
let loc_of = Ast_loc.loc_of

let (<+>) = FLoc.Ops.((<+>))
(**   connectives  *)

let (<+>) a b = loc_of a <+> loc_of b

let sem (a:'a) (b:'a) = let _loc =  a <+> b in `Sem(_loc,a,b)

let com a b = let _loc = a <+> b in `Com(_loc,a,b)

let app a b = let _loc = a <+> b in `App(_loc,a,b)

let apply a b = let _loc = a <+> b in `Apply(_loc,a,b)

let sta a b = let _loc = a <+> b in `Sta(_loc,a,b)

let bar a b = let _loc = a <+> b in `Bar(_loc,a,b)

let anda a b = let _loc = a <+> b in `And(_loc,a,b)

let dot a b = let _loc = a <+> b in `Dot (_loc,a,b)

let par x =  let _loc = loc_of x in `Par (_loc,x)

let seq a = let _loc = loc_of a in `Seq (_loc,a) 

let arrow a b = let _loc = a <+> b in `Arrow(_loc,a,b)

let typing a b = let _loc = a<+> b in `Constraint(_loc,a,b)

  
(** [of_list] style function *)
let bar_of_list xs = Ast_basic.of_listr bar xs 

let and_of_list xs = Ast_basic.of_listr anda xs

let sem_of_list xs = Ast_basic.of_listr sem xs 

let com_of_list xs = Ast_basic.of_listr com xs   

let sta_of_list xs = Ast_basic.of_listr sta xs     

let dot_of_list xs = Ast_basic.of_listr dot xs 
  
let appl_of_list xs = Ast_basic.of_listl app xs     

    
    
  
let seq_sem ls = seq (sem_of_list ls)




let binds bs (e: FAst.exp) =
  match bs with
  | [] -> e
  |_ ->
      let binds = and_of_list bs  in
      let _loc = binds <+> e in
      %exp{let $binds in $e } 


let lid _loc n = `Lid(_loc,n)
    
let uid _loc n = `Uid(_loc,n)
let unit _loc = `Uid(_loc,"()")

let ep_of_cons _loc n ps =
  appl_of_list ((uid _loc n) :: ps)

let tuple_com_unit _loc = function
  | [] -> unit _loc
  | [p] -> p
  | y ->
      `Par _loc (com_of_list y)

  
let tuple_com y=
  match y with 
  |[] -> failwith "tuple_com empty"
  |[x] -> x
  | x::_ -> (* FIXME [x::_] still compiles *)
      let _loc = x <+> Listf.last y in
      `Par _loc (com_of_list y) 
    
let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | [x] -> x
  | x::_ ->
       let _loc =  x <+> Listf.last y in 
       `Par (_loc, sta_of_list y)




(*
   For all strings, we don't do parsing at all. So keep your strings
   input as simple as possible
   
   {[
   ( %{blabla} +> ["x0";"x1";"x2"] ) |> eprint;
   blabla x0 x1 x2
   ]}
 *)
let (+>) f names  =
  let _loc = loc_of f in
  appl_of_list (f:: (List.map (lid _loc) names))
         
(**  FIXME more precise API wanted *)
let meta_here _loc location  =
  let (a, b, c, d, e, f, g, h) = FLoc.to_tuple location in
  %exp'{ FLoc.of_tuple
     ($`str:a, $`int:b, $`int:c, $`int:d,
      $`int:e, $`int:f, $`int:g,
      $(if h then %exp'{ true } else %exp'{ false } )) }

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast_gen.cmo" *)
(* end: *)
