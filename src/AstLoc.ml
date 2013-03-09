open LibUtil;
open FanLoc.Ops;
include Ast;
{:fans|keep off; derive (GenLoc);|};
{:ocaml|INCLUDE "src/Ast.ml"; |};



let ghost = FanLoc.ghost ; (* to refine *)

let (<+>) a b = loc_of a <+> loc_of b;  

let sem a b = let _loc =  a <+> b in `Sem(_loc,a,b);
let com a b = let _loc = a <+> b in `Com(_loc,a,b);
let app a b = let _loc = a <+> b in `App(_loc,a,b);
let sta a b = let _loc = a <+> b in `Sta(_loc,a,b);
let ora a b = let _loc = a <+> b in `Or(_loc,a,b);
let anda a b = let _loc = a <+> b in `And(_loc,a,b);
let dot a b = let _loc = a <+> b in `Dot (_loc,a,b);
let tup x =  let _loc = loc_of x in `Tup (_loc,x);
let seq a = let _loc = loc_of a in `Seq (_loc,a) ;


let typing a b = let _loc = a<+> b in `Constraint(_loc,a,b);

  

let rec or_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> ora t (or_of_list ts)];


let rec and_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> anda t (and_of_list ts) ];


let rec sem_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> sem t (sem_of_list ts)];
  
let rec com_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> com t (com_of_list ts)];
  
let rec sta_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> sta t (sta_of_list ts)];

let rec dot_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> dot t (dot_of_list ts)];
  

(*
  {[
  with expr appl_of_list [{|f|}; {|a|}; {|b|}] |> Ast2pt.print_expr f;
  f a b
  ]}
 *)
let rec appl_of_list x  =
  match x with
  [[] -> `Nil ghost
  |[x] -> x
  | [x;y::xs] -> appl_of_list [(app x y)::xs]  ]  ;

    
let rec appl_of_list1 x =
  match x with
  [ [] -> failwith "appl_of_list1 empty list"
  | [x] -> x
  | [x;y::xs] -> appl_of_list1 [(app x y)::xs] ]  ;
    
  
  
let rec and_of_list1 = fun
  [ [] -> failwithf "and_of_list1 empty list"
  | [t] -> t
  | [t::ts] ->anda t (and_of_list1 ts)];
  

let rec sem_of_list1 = fun
  [ [] -> failwith "sem_of_list1 empty list"
  | [t] -> t
  | [t::ts] -> sem t (sem_of_list1 ts)];
  

let rec com_of_list1 = fun
  [ [] -> failwith "com_of_list1 empty list"
  | [t] -> t
  | [t::ts] -> com t (com_of_list1 ts)];

let rec dot_of_list1 = fun
  [[] -> failwith "dot_of_list1 empty list"
  |[i] -> i
  |[i::is] -> dot i (dot_of_list1 is)];

let rec sta_of_list1 = fun
  [[] -> failwith "sta_of_list1 empty list"
  |[i] -> i
  |[i::is] -> sta i (sta_of_list1 is)];
  
let tuple_com y=
  match y with 
  [[] -> failwith "tuple_com empty"
  |[x] -> x
  | [x::_] ->
      let _loc = x <+> List.last y in
      `Tup _loc (com_of_list1 y) ];
    
let tuple_sta y =
  match y with
   [ [] -> failwith "tuple_sta empty"
   | [x] -> x
   | [x::_] ->
       let _loc =  x <+> List.last y in 
       `Tup _loc (sta_of_list1 y)];
    
  
let rec list_of_and x acc =
  match x with
  [`And(_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> [x::acc] ];
    
let rec list_of_and' x acc =
  match x with
  [`And(_,x,y) -> list_of_and' x (list_of_and' y acc)
  |`Nil _ -> acc
  | _ -> [x::acc] ]  ;

let rec list_of_com x acc =
  match x with
  [`Com(_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> [x::acc]];
    
let rec list_of_com' x acc =
  match x with
  [`Com(_,x,y) -> list_of_com' x (list_of_com' y acc)
  |`Nil _ -> acc
  | _ -> [x::acc] ]  ;

let rec list_of_star x acc =
  match x with
  [`Sta(_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> [x::acc] ]  ;
    
    
let rec list_of_star' x acc =
  match x with
  [`Sta(_,x,y) -> list_of_star' x (list_of_star' y acc)
  | `Nil _ -> acc
  | _ -> [x::acc]];
    
let rec list_of_or x acc =
  match x with
  [`Or(_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> [x::acc]]  ;

let rec list_of_or' x acc =
  match x with
  [`Or(_,x,y) -> list_of_or' x (list_of_or' y acc)
  | `Nil _ -> acc 
  | _ -> [x::acc]]  ;
    
let rec list_of_sem x acc =
  match x with
  [`Sem(_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> [x::acc]]  ;

let rec list_of_sem' (x:'a) acc =
  match x with
  [`Sem(_,x,y) -> list_of_sem' x (list_of_sem' y acc)
  |`Nil _ -> acc
  | y -> [y::acc] ] ;


let rec list_of_app  x acc =
  match x with
  [`App(_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  |x -> [x :: acc] ];

let rec list_of_app' x acc =
  match x with
  [`App(_,t1,t2) -> list_of_app' t1 (list_of_app' t2 acc)
  | `Nil _ -> acc 
  |x -> [x :: acc] ];

let rec view_app acc = fun
  [`App (_,f,a) -> view_app [a::acc] f
  | f -> (f,acc)];
    
let seq_sem ls = seq (sem_of_list ls);    
