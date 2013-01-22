include Ast;
module type META_LOC = sig
      (** The first location is where to put the returned pattern.
          Generally it's _loc to match with {:patt| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> patt;
      (** The first location is where to put the returned expression.
          Generally it's _loc to match with {:expr| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> expr;
end;
open FanUtil;
open LibUtil;  
open StdLib;


let safe_string_escaped s =
  if String.length s > 2 && s.[0] = '\\' && s.[1] = '$' then s
  else String.escaped s;

let strip_loc_list f lst =
  List.map f lst ;
  
{:fans|keep off;
 derive
   (Map2
      Fold2 OIter MetaExpr MetaPatt Map Fold Print OPrint OEq
      GenLoc  Strip
   ); |};

  
{:ocaml|INCLUDE "src/Ast.ml"; |};

#default_quotation "expr";;
module MExpr = struct
  INCLUDE "src/MetaTemplate.ml"; (* FIXME INCLUDE as a langauge :default *)
end;

#default_quotation "patt"  ;;
module MPatt = struct
  INCLUDE "src/MetaTemplate.ml";
end;


module Make(MetaLoc:META_LOC) = struct
  module Expr = struct
    open MExpr;
    let meta_loc = MetaLoc.meta_loc_expr;
    __MetaExpr__;
  end;
  module Patt =struct
    open MPatt;
    let meta_loc = MetaLoc.meta_loc_patt;
    __MetaPatt__;
  end;
end;
    


(* +-----------------------------------------------------------------+
   | Other utilities for ast                                         |
   +-----------------------------------------------------------------+ *)
  
(*
  {[
  is_module_longident {:ident| A.B.t |} ; 
  - : bool = false
  is_module_longident {:ident| A.B |} ; 
  - : bool = true
  ]}
 *)
let rec is_module_longident = fun
  [ {:ident| $_.$i |} -> is_module_longident i
  | {:ident| ($i1 $i2) |} ->
      is_module_longident i1 && is_module_longident i2

  | {:ident| $uid:_ |} -> true
  | _ -> false ];

let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =  fun
    [ {:expr@_loc| $e1 $e2 |} -> {:ident| ( $(self e1) $(self e2)) |}
    | {:expr@_loc| $e1.$e2 |} -> {:ident| $(self e1).$(self e2) |}
    | {:expr| $lid:_ |} -> error ()
    | {:expr| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
  fun
    [ {:expr| $id:i |} -> i
    | {:expr| $_ $_ |} -> error ()
    | t -> self t ];

(*
  {[
  ident_of_ctyp {:ctyp| list int |} ; ;
  Exception: Invalid_argument "ident_of_ctyp: this type is not an identifier".

  ident_of_ctyp {:ctyp| A.B |} ; ;     
  - : ident =
  `Dot (, `Uid (, "A"),
  `Uid (, "B"))

  {:ctyp| A.B |} ; ;
  - : ctyp =
  `Id (, `Dot (, `Uid (, "A"), `Uid (, "B")))

  ident_of_ctyp {:ctyp| (A B).t |} ; ;
  - : ident =
  `Dot (, `App (, `Uid (, "A"), `Uid (, "B")), `Lid (, "t"))  ]}
 *)
let ident_of_ctyp =
  let error () =
    invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =   fun
    [ {:ctyp@_loc| $t1 $t2 |} -> {:ident| ( $(self t1) $(self t2) ) |}
    | {:ctyp| $lid:_ |} -> error ()
    | {:ctyp| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
    fun
    [ {:ctyp| $id:i |} -> i
    | t -> self t ];

let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self = fun 
    [ {:patt@_loc| $p1 $p2 |}
      -> {:ident| ( $(self p1) $(self p2) ) |}
    | {:patt| $lid:_ |} -> error ()
    | {:patt| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
    fun
    [ {:patt| $id:i |} -> i
    | p -> self p ];


let rec is_irrefut_patt : patt -> bool = with patt
    fun
    [ {| $lid:_ |} -> true
    | {| () |} -> true
    | {| _ |} -> true
    | {||} -> true (* why not *)
    | {| ($x as $_) |} -> is_irrefut_patt x (* && is_irrefut_patt y *)
    | {| { $p } |} -> is_irrefut_patt p
    | {| $_ = $p |} -> is_irrefut_patt p
    | {| $p1; $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| $p1, $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| $p1 | $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2 (* could be more fine grained *)
    | {| $p1 $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| ($p : $_) |} -> is_irrefut_patt p
    | {| ($tup:pl) |} -> is_irrefut_patt pl
    | {| ? $_ : ($p =  $opt:_ ) |} -> is_irrefut_patt p
    | {| ~ $_ |} -> true
    | {| ~ $_: $p |} -> is_irrefut_patt p
    | {| lazy $p |} -> is_irrefut_patt p
    | {| $id:_ |} -> false (* here one need to know the arity of constructors *)
    | {| (module $_ : $opt:_ ) |} -> true
    | (* {| `$_ |} *) `Vrn (_loc,_)
      (* {| $vrn:_ |} *)
    | {| $str:_ |} | {| $_ .. $_ |} |
      {| $flo:_ |} | {| $nativeint:_ |} | {| $int64:_ |} |
      {| $int32:_ |} | {| $int:_ |} | {| $chr:_ |} |
      {| #$_ |} | {| [| $_ |] |} | {| $anti:_ |} -> false
          (* add here ModuleUnpack *)
    ];      
      

let rec is_constructor =  with ident fun
    [ {| $_.$i |} -> is_constructor i
    | {| $uid:_ |} -> true
    | {| $lid:_ |} | {| ($_ $_) |} -> false
    | {| $anti:_ |} -> assert false ];

let is_patt_constructor = fun
    [ {:patt| $id:i |} -> is_constructor i
    | (* {:patt| `$_ |} *)
      (* {:patt| $vrn:_ |} *)
      `Vrn (_loc,_)
      -> true
    | _ -> false ];

let rec is_expr_constructor = fun
    [ {:expr| $id:i |} -> is_constructor i
    | {:expr| $e1.$e2 |} -> is_expr_constructor e1 && is_expr_constructor e2
    | (* {:expr| `$_ |} *)
      `Vrn(_loc,_)
      (* {:expr| $vrn:_ |} *)
      -> true
    | _ -> false ];

let ghost = FanLoc.ghost ; (* to refine *)

(* RA *)  
let rec or_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] ->
      let _loc = loc_of t in `Or(_loc,t,or_of_list ts)];

(* RA *)  
let rec and_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> let _loc = loc_of t in `And(_loc,t,and_of_list ts)];

(* RA *)  
let rec sem_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> let _loc = loc_of t in `Sem(_loc,t,sem_of_list ts) ];

(* RA *)  
let rec com_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] ->
      let _loc = loc_of t in `Com(_loc,t,com_of_list ts)  ];

let rec com_of_list' = fun
  [ [] -> failwith "com_of_list' empty list"
  | [t] -> t
  | [t::ts] ->
      let _loc = loc_of t in `Com(_loc,t,com_of_list' ts)  ];

(* RA *)  
let rec sta_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] -> let _loc = loc_of t in `Sta(_loc,t,sta_of_list ts)];

(* RA *)  
let rec amp_of_list = fun
  [ [] -> `Nil ghost
  | [t] -> t
  | [t::ts] ->
      let _loc = loc_of t in
      `Amp(_loc,t,amp_of_list ts)];
  

let tuple_com y=
  match y with 
  [[] -> failwith "tuple_com empty"
  |[x] -> x
  | [x::_] ->
      let a = loc_of x in
      let b = loc_of (List.last y) in
      let _loc = FanLoc.merge a b in 
      `Tup _loc (com_of_list y) ];
    
let tuple_sta y =
  match y with
   [ [] -> failwith "tuple_sta empty"
   | [x] -> x
   | [x::_] ->
       let a = loc_of x in
       let b = loc_of (List.last y) in
       let _loc = FanLoc.merge a b in 
       `Tup _loc (sta_of_list y)];



(*
  Given an location, and a list of expression node,
  return an expression node which represents the list
  of the expresson nodes

  Example:
  {[
  mklist _loc [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
  [b; c; d]
  ]}
  (* {:expr| [1;2;3::[]]|} *)
  DoubleColon
 *)
let list_of_list loc =
  let rec loop top =  with expr fun
    [ [] ->   {@ghost| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of e1) loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;

(* It is the inverse operation by [view_app]
   Example:
   {[
   apply {|a|} [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
   a b c d
   ]}
 *)
(* let rec apply accu = fun *)
(*   [ [] -> accu *)
(*   | [x :: xs] -> let _loc = loc_of x in apply {| $accu $x |} xs ]; *)
  
(*
  mk_array [| {| 1 |} ; {| 2 |} ; {| 3 |} |] |> e2s = ({| [|1;2;3|] |} |> e2s);
  True
 *)
let array_of_array loc arr =
  let rec loop top =  with expr fun
    [ [] -> {@ghost| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of e1) loc in
        {| [| $e1 ; $(loop false el) |] |} ] in
  let items = arr |> Array.to_list in 
  loop true items;
  
    
(* RA *)  
let rec dot_of_list' = fun
  [[] -> assert false
  |[i] -> i
  |[i::is] ->
      let _loc = loc_of i in
      `Dot(_loc,i,dot_of_list' is) ];


let ty_of_stl = fun
    [ (_loc, s, []) ->
      `Id(_loc,`Uid(_loc,s))
      (* {:ctyp| $uid:s |} *)
    | (_loc, s, tl) ->
        `Of (_loc, `Id (_loc, `Uid (_loc, s)), and_of_list tl)];
        (* {:ctyp| $uid:s of $(and_of_list tl) |} *) 

let ty_of_sbt = fun
    [ (_loc, s, true, t) ->
      `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), (`Mut (_loc, t)))
      (* {:ctyp| $lid:s : mutable $t |} *)
    | (_loc, s, false, t) ->
        `TyCol (_loc, `Id (_loc, `Lid (_loc, s)), t)];
        (* {:ctyp| $lid:s : $t |}  *)

let bi_of_pe (p, e) = let _loc = loc_of p in {:binding| $p = $e |};

let sum_type_of_list l = or_of_list (List.map ty_of_stl l);

let record_type_of_list l = sem_of_list (List.map ty_of_sbt l);

let binding_of_pel l = and_of_list (List.map bi_of_pe l);

  
let rec list_of_amp x acc =
  match x with
  [`And(_,x,y) ->
    list_of_amp x (list_of_amp y acc)
  | _ -> [x::acc] ];

let rec list_of_amp' x acc =
  match x with
  [`And(_,x,y) ->
    list_of_amp' x (list_of_amp' y acc)
  | `Nil _ -> acc
  | _ -> [x::acc] ];    


let rec list_of_and x acc =
  match x with
  [`And(_,x,y) ->
    list_of_and x (list_of_and y acc)
  | _ -> [x::acc] ];
    
let rec list_of_and' x acc =
  match x with
  [`And(_,x,y) ->
    list_of_and' x (list_of_and' y acc)
  |`Nil _ -> acc
  | _ -> [x::acc] ]  ;
    
let rec list_of_com x acc =
  match x with
  [`Com(_,x,y) ->
    list_of_com x (list_of_com y acc)
  | _ -> [x::acc]];
let rec list_of_com' x acc =
  match x with
  [`Com(_,x,y) ->
    list_of_com' x (list_of_com' y acc)
  |`Nil _ -> acc
  | _ -> [x::acc] ]  ;
    
let rec list_of_star' x acc =
  match x with
  [`Sta(_,x,y) ->
    list_of_star' x (list_of_star' y acc)
  | `Nil _ -> acc
  | _ -> [x::acc]];
    
let rec list_of_star x acc =
  match x with
  [`Sta(_,x,y) ->
    list_of_star x (list_of_star y acc)
  | _ -> [x::acc] ]  ;
    
let rec list_of_or x acc =
  match x with
  [`Or(_,x,y) ->
    list_of_or x (list_of_or y acc)
  | _ -> [x::acc]]  ;

let rec list_of_or' x acc =
  match x with
  [`Or(_,x,y) ->
    list_of_or x (list_of_or' y acc)
  | `Nil _ -> acc 
  | _ -> [x::acc]]  ;
    
let rec list_of_sem x acc =
  match x with
  [`Sem(_,x,y) ->
    list_of_sem x (list_of_sem y acc)
  | _ -> [x::acc]]  ;

let rec list_of_sem' x acc =
  match x with
  [`Sem(_,x,y) ->
    list_of_sem' x (list_of_sem' y acc)
  |`Nil _ -> acc
  | _ -> [x::acc] ] ;
    
let sem a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in
  `Sem(_loc,a,b);

let com a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in
  `Com(_loc,a,b);

let app a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in
  `App(_loc,a,b);

let sta a b =
  let _loc = FanLoc.merge (loc_of a) (loc_of b) in
  `Sta(_loc,a,b);

let typing a b =
  let _loc = FanLoc.merge (loc_of a ) (loc_of b) in
  `Constraint(_loc,a,b);
let rec list_of_app  x acc =
  match x with
  [`App(_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  |x -> [x :: acc] ];

let rec list_of_app' x acc =
  match x with
  [`App(_,t1,t2) -> list_of_app' t1 (list_of_app' t2 acc)
  | `Nil _ -> acc 
  |x -> [x :: acc] ];

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

    
let rec appl_of_list' x =
  match x with
  [ [] -> failwith "appl_of_list' empty list"
  | [x] -> x
  | [x;y::xs] -> appl_of_list' [(app x y)::xs] ]  ;
    
let rec view_app acc = fun
  [`App (_,f,a) -> view_app [a::acc] f
  | f -> (f,acc)];

    
let map_expr f = object
  inherit map as super;
  method! expr x = f (super#expr x);
end;
let map_patt f = object
  inherit map as super;
  method! patt x = f (super#patt x);
end;
let map_ctyp f = object
  inherit map as super;
  method! ctyp x = f (super#ctyp x);
end;
let map_str_item f = object
  inherit map as super;
  method! str_item x = f (super#str_item x);
end;
let map_sig_item f = object
  inherit map as super;
  method! sig_item x = f (super#sig_item x);
end;
let map_loc f = object
  inherit map as super;
  method! loc x = f (super#loc x);
end;


class clean_ast = object
  inherit map as super;
  method! with_constr wc =
    with with_constr
    match super#with_constr wc with
    [ {| $({@_l||})  and $wc |} |
      {| $wc and $({@_l||} ) |} -> wc
    | wc -> wc ];
  method! expr e =
    with expr
    match super#expr e with
    [ {| let $rec:_ $({:binding@_l||}) in $e |} |
      {| { ($e) with $({:rec_binding@_l||})  } |} |
      {| $({@_l||} ), $e |} |
      {| $e, $({@_l||} ) |} |
      {| $({@_l||}); $e |} |
      {| $e; $({@_l||} ) |} -> e
    | e -> e ];
  method! patt p =
    with patt
    match super#patt p with
    [ (* {| ( $p as $({@_l||} ) ) |} | *)
      {| $({@_l||}) | $p |} |
      {| $p | $({@_l||} ) |} |
      {| $({@_l||} ), $p |} |
      {| $p, $({@_l||} ) |} |
      {| $({@_l||} ); $p |} |
      {| $p; $({@_l||} ) |} -> p
    | p -> p ];
  method! match_case mc =
    with match_case
    match super#match_case mc with
    [ {| $({@_l||} ) | $mc |} |
      {| $mc | $({@_l||} ) |} -> mc
    | mc -> mc ];
  method! binding bi =
    with binding
    match super#binding bi with
    [ {| $({@_l||} ) and $bi |} |
      {| $bi and $({@_l||} ) |} -> bi
    | bi -> bi ];
  method! rec_binding rb =
    with rec_binding
    match super#rec_binding rb with
    [ {| $({@_l||} ) ; $bi |} | {| $bi ; $({@_l||} ) |} -> bi
    | bi -> bi ];

  method! module_binding mb =
    with module_binding
    match super#module_binding mb with
    [ {| $({@_l||} ) and $mb |} |
      {| $mb and $({@_l||} ) |} -> mb
    | mb -> mb ];

  method! ctyp t =
    with ctyp
    match super#ctyp t with
    [ {| ! $({@_l||} ) . $t |} |
      {| $({@_l||} ) as $t |} |
      {| $t as $({@_l||} ) |} |
      {| $t -> $({@_l||} ) |} |
      {| $({@_l||} ) -> $t |} |
      {| $({@_l||} ) | $t |} |
      {| $t | $({@_l||} ) |} |
      {| $t of $({@_l||} ) |} |
      {| $({@_l||} ) and $t |} |
      {| $t and $({@_l||} ) |} |
      {| $t; $({@_l||} ) |} |
      {| $({@_l||} ); $t |} |
      {| $({@_l||}), $t |} |
      {| $t, $({@_l||} ) |} |
      {| $t & $({@_l||} ) |} |
      {| $({@_l||} ) & $t |} |
      {| $({@_l||} ) * $t |} |
      {| $t * $({@_l||} ) |} -> t
    | t -> t ];

  method! sig_item sg =
    with sig_item
    match super#sig_item sg with
    [ {| $({@_l||}); $sg |} | {| $sg; $({@_l||} ) |} -> sg
    | {| type $({:ctyp@_l||} ) |} -> {||}
    | sg -> sg ];

  method! str_item st =
    with str_item
    match super#str_item st with
    [ {| $({@_l||} ); $st |} | {| $st; $({@_l||} ) |} -> st
    | {| type $({:ctyp@_l||} ) |} -> {||}
    | {| let $rec:_ $({:binding@_l||} ) |} -> {||}
    | st -> st ];

  method! module_type mt =
    match super#module_type mt with
    [ {:module_type| $mt with $({:with_constr@_l||} ) |} -> mt
    | mt -> mt ];

  method! class_expr ce =
    with class_expr
    match super#class_expr ce with
    [ {| $({@_l||} ) and $ce |} | {| $ce and $({@_l||} ) |} -> ce
    | ce -> ce ];

  method! class_type ct =
    with class_type
    match super#class_type ct with
    [ {| $({@_l||} ) and $ct |} | {| $ct and $({@_l||} ) |} -> ct
    | ct -> ct ];

  method! class_sig_item csg =
    with class_sig_item
    match super#class_sig_item csg with
    [ {| $({@_l||} ); $csg |} | {| $csg; $({@_l||} ) |} -> csg
    | csg -> csg ];

  method! class_str_item cst =
    with class_str_item
    match super#class_str_item cst with
    [ {| $({@_l||} ); $cst |} | {| $cst; $({@_l||} ) |} -> cst
    | cst -> cst ];
end;

(* change all the [loc] to [ghost] *)    
class reloc _loc = object
  inherit map ;
  method! loc _ = _loc;
end;

(*
  {[]}
 *)  
let wildcarder = object (self)
  inherit map as super;
  method! patt = fun
  [ {:patt| $lid:_ |} -> {:patt| _ |}
  | {:patt| ($p as $_) |} -> self#patt p
  | p -> super#patt p ];
end;

(* let normalize = object (self) *)
(*   inherit fold as super; *)
(* end; *)

let match_pre = object (self)
  inherit map; (* as super; *)
  method! match_case = with match_case fun
   [ {| $pat:p -> $e |} -> {| $pat:p -> fun () -> $e |}
   | {| $pat:p when $e -> $e1 |} -> {| $pat:p when $e -> fun () -> $e1 |}
   | {| $a1 | $a2 |} -> {| $(self#match_case a1) | $(self#match_case a2) |}
   | {| |} -> {| |}
   | (* {| $anti:x |} *) `Ant(_loc,x) ->
       `Ant(_loc, add_context x "lettry")
       (* {| $(anti: add_context x "lettry" ) |} *) ];
end;


let dump = new print;



let dump_ctyp = to_string_of_printer dump#ctyp;
let dump_with_constr = to_string_of_printer dump#with_constr;
let dump_module_type = to_string_of_printer dump#module_type;
let dump_expr = to_string_of_printer dump#expr;
let dump_patt = to_string_of_printer dump#patt;
let dump_class_type = to_string_of_printer dump#class_type;
let dump_class_expr = to_string_of_printer dump#class_expr;
let dump_ident = to_string_of_printer dump#ident;
let dump_match_case = to_string_of_printer dump#match_case;
let dump_rec_binding = to_string_of_printer dump#rec_binding;  
let dump_str_item = to_string_of_printer dump#str_item;
let dump_sig_item = to_string_of_printer dump#sig_item;
let dump_module_binding  = to_string_of_printer dump#module_binding;
let dump_module_expr = to_string_of_printer dump#module_expr;  
let dump_class_sig_item = to_string_of_printer dump#class_sig_item;
let dump_class_str_item = to_string_of_printer dump#class_str_item;  
