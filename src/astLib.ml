

(** FAst a module for processing abstract syntax.

    It makes use of  structual polymorphism aggresively,
    the signature is quite complex due the use of [loc_of],
    see [astLibN] for the same functionality but processing ast without locations
    and hence a much simplified signature *)
  

  
open LibUtil
open FLoc.Ops
open FAst



(** generate [loc_of] function per type
    for example:
    [loc_of (e:exp)]
    [loc_of (p:pat)] will both return the location information *)
{:fans|keep off; derive (GenLoc);|};;
{:ocaml|{:include|"common/fAst.mli"|}|};;




  
(**   connectives  *)

let (<+>) a b = loc_of a <+> loc_of b
let sem a b = let _loc =  a <+> b in `Sem(_loc,a,b)
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
let rec bar_of_list = function
  | [] -> failwithf "bar_of_list empty"
  | [t] -> t
  | t::ts -> bar t (bar_of_list ts)

let rec and_of_list = function
  | [] -> failwithf "and_of_list empty"
  | [t] -> t
  | t::ts -> anda t (and_of_list ts) 


let rec sem_of_list = function
  | [] -> failwithf "sem_of_list empty"
  | [t] -> t
  | t::ts -> sem t (sem_of_list ts)
  
let rec com_of_list = function
  | [] -> failwithf "com_of_list empty"
  | [t] -> t
  | t::ts -> com t (com_of_list ts)
  
let rec sta_of_list = function
  | [] -> failwithf "sta_of_list empty"
  | [t] -> t
  | t::ts -> sta t (sta_of_list ts)

let rec dot_of_list = function
  | [] -> failwithf "dot_of_list empty"
  | [t] -> t
  | t::ts -> dot t (dot_of_list ts)
  

(*
  {[
  with exp appl_of_list [{|f|}; {|a|}; {|b|}] |> Ast2pt.print_exp f;
  f a b
  ]}
 *)
let rec appl_of_list x  =
  match x with
  | [] -> failwithf "appl_of_list empty"
  | [x] -> x
  | x::y::xs -> appl_of_list ((app x y)::xs)

    
    
  
let rec list_of_and x acc =
  match x with
  |`And(_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x::acc

let rec list_of_com x acc =
  match x with
  |`Com(_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x::acc
    
let rec list_of_star x acc =
  match x with
  | `Sta(_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x::acc

let rec list_of_bar x acc =
  match x with
  |`Bar(_,x,y) -> list_of_bar x (list_of_bar y acc)
  | _ -> x::acc

let rec list_of_or x acc =
  match x with
  |`Bar(_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x::acc

    
let rec list_of_sem x acc =
  match x with
  |`Sem(_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x::acc

let rec list_of_dot x acc =
  match x with
  |`Dot(_,x,y) -> list_of_dot x (list_of_dot y acc)
  |x -> x::acc

let rec list_of_app  x acc =
  match x with
  |`App(_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  |x -> x :: acc


(*
  t1 -> t2 -> t3 =>
  [t1::t2::t3::acc]
 *)    
let rec list_of_arrow_r x acc =
  match x with
  |`Arrow(_,t1,t2) -> list_of_arrow_r t1 (list_of_arrow_r t2 acc)
  | x -> x::acc

(*************************************************************************)
(*************************************************************************)
  
let rec view_app acc = function
  |`App (_,f,a) -> view_app (a::acc) f
  | f -> (f,acc)

  
let seq_sem ls = seq (sem_of_list ls)

let binds bs (e:exp) =
  match bs with
  | [] -> e
  |_ ->
      let binds = and_of_list bs  in
      let _loc = binds <+> e in
      {:exp|let $binds in $e |} 


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
      let _loc = x <+> List.last y in
      `Par _loc (com_of_list y) 
    
let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | [x] -> x
  | x::_ ->
       let _loc =  x <+> List.last y in 
       `Par _loc (sta_of_list y)




(*
   For all strings, we don't do parsing at all. So keep your strings
   input as simple as possible
   
   {[
   ( {|blabla|} +> ["x0";"x1";"x2"] ) |> eprint;
   blabla x0 x1 x2
   ]}
 *)
let (+>) f names  =
  let _loc = loc_of f in
  appl_of_list (f:: (List.map (lid _loc) names))
         
(**  FIXME more precise API wanted *)
let meta_here _loc location  =
  let (a, b, c, d, e, f, g, h) = FLoc.to_tuple location in
  {:exp'| FLoc.of_tuple
     ($`str:a, $`int:b, $`int:c, $`int:d,
      $`int:e, $`int:f, $`int:g,
      $(if h then {:exp'| true |} else {:exp'| false |} )) |}
