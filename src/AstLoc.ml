open LibUtil;
open FanLoc.Ops;
include Ast;
{:fans|keep off; derive (GenLoc);|};



{:ocaml|{:include|"src/Ast.ml"|};|};

let ghost = FanLoc.ghost ; (* to refine *)

let (<+>) a b = loc_of a <+> loc_of b;  

let sem a b = let _loc =  a <+> b in `Sem(_loc,a,b);
let com a b = let _loc = a <+> b in `Com(_loc,a,b);
let app a b = let _loc = a <+> b in `App(_loc,a,b);
let sta a b = let _loc = a <+> b in `Sta(_loc,a,b);
let ora a b = let _loc = a <+> b in `Or(_loc,a,b);
let anda a b = let _loc = a <+> b in `And(_loc,a,b);
let dot a b = let _loc = a <+> b in `Dot (_loc,a,b);
let tup x =  let _loc = loc_of x in `Par (_loc,x);
let seq a = let _loc = loc_of a in `Seq (_loc,a) ;
let arrow a b = let _loc = a <+> b in `Arrow(_loc,a,b);

let typing a b = let _loc = a<+> b in `Constraint(_loc,a,b);

  

let rec or_of_list = fun
  [ [] -> failwithf "or_of_list empty"
  | [t] -> t
  | [t::ts] -> ora t (or_of_list ts)];


let rec and_of_list = fun
  [ [] -> failwithf "and_of_list empty"
  | [t] -> t
  | [t::ts] -> anda t (and_of_list ts) ];


let rec sem_of_list = fun
  [ [] -> failwithf "sem_of_list empty"
  | [t] -> t
  | [t::ts] -> sem t (sem_of_list ts)];
  
let rec com_of_list = fun
  [ [] -> failwithf "com_of_list empty"
  | [t] -> t
  | [t::ts] -> com t (com_of_list ts)];
  
let rec sta_of_list = fun
  [ [] -> failwithf "sta_of_list empty"
  | [t] -> t
  | [t::ts] -> sta t (sta_of_list ts)];

let rec dot_of_list = fun
  [ [] -> failwithf "dot_of_list empty"
  | [t] -> t
  | [t::ts] -> dot t (dot_of_list ts)];
  

(*
  {[
  with exp appl_of_list [{|f|}; {|a|}; {|b|}] |> Ast2pt.print_exp f;
  f a b
  ]}
 *)
let rec appl_of_list x  =
  match x with
  [[] -> failwithf "appl_of_list empty"
  |[x] -> x
  | [x;y::xs] -> appl_of_list [(app x y)::xs]  ]  ;

    
    
  
let rec list_of_and x acc =
  match x with
  [`And(_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> [x::acc] ];

let rec list_of_com x acc =
  match x with
  [`Com(_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> [x::acc]];
    


let rec list_of_star x acc =
  match x with
  [`Sta(_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> [x::acc] ]  ;
    
    
let rec list_of_or x acc =
  match x with
  [`Or(_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> [x::acc]]  ;

    
let rec list_of_sem x acc =
  match x with
  [`Sem(_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> [x::acc]]  ;


let rec list_of_app  x acc =
  match x with
  [`App(_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  |x -> [x :: acc] ];

let rec view_app acc = fun
  [`App (_,f,a) -> view_app [a::acc] f
  | f -> (f,acc)];

  
let seq_sem ls = seq (sem_of_list ls);

let binds bs (e:exp) =
  match bs with
  [[] -> e
  |_ ->
      let binds = and_of_list bs  in
      let _loc = binds <+> e in
      {:exp'|let $binds in $e |} ];  

let lid _loc n = `Id(_loc,`Lid(_loc,n));
let uid _loc n = `Id(_loc,`Uid(_loc,n));
let unit _loc = `Id(_loc,`Uid(_loc,"()"));

let ep_of_cons _loc n ps =
  appl_of_list [(uid _loc n) :: ps];;

let tuple_com_unit _loc = fun
  [ [] -> unit _loc
  | [p] -> p
  | y ->
      `Par _loc (com_of_list y)
  ];
  
let tuple_com y=
  match y with 
  [[] -> failwith "tuple_com empty"
  |[x] -> x
  | [x::_] ->
      let _loc = x <+> List.last y in
      `Par _loc (com_of_list y) ];
    
let tuple_sta y =
  match y with
   [ [] -> failwith "tuple_sta empty"
   | [x] -> x
   | [x::_] ->
       let _loc =  x <+> List.last y in 
       `Par _loc (sta_of_list y)];
