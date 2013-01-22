
open FanAst;
#default_quotation     "expr";;


(* +-----------------------------------------------------------------+
   | the modules documented with [open Lib.Expr]                     |
   +-----------------------------------------------------------------+ *)

open LibUtil;
open Basic;
open FanUtil;



(*
  The input is either {|$_.$_|} or {|$(id:{:ident| $_.$_|})|}
  the type of return value and [acc] is
  [(loc* string list * expr) list]

  The [string list] is generally a module path, the [expr] is the last field

  Examples:

  {[
  sep_dot_expr [] {|A.B.g.U.E.h.i|};
  - : (loc * string list * expr) list =
  [(, ["A"; "B"], ExId (, Lid (, "g")));
  (, ["U"; "E"], ExId (, Lid (, "h"))); (, [], ExId (, Lid (, "i")))]

  sep_dot_expr [] {|A.B.g.i|};
  - : (loc * string list * expr) list =
  [(, ["A"; "B"], ExId (, Lid (, "g"))); (, [], ExId (, Lid (, "i")))]

  sep_dot_expr [] {|$(uid:"").i|};
  - : (loc * string list * expr) list =
  [(, [""], ExId (, Lid (, "i")))]

  ]}
 *)

let rec sep_dot_expr acc = fun
  [ {| $e1.$e2|} ->
    sep_dot_expr (sep_dot_expr acc e2) e1
  | {@loc| $uid:s |} as e ->
      match acc with
      [ [] -> [(loc, [], e)]
      | [(loc', sl, e) :: l] -> [(FanLoc.merge loc loc', [s :: sl], e) :: l] ]
  | {| $(id:({:ident@_l| $_.$_ |} as i)) |} ->
      sep_dot_expr acc (Ident.normalize_acc i)
  | e -> [(FanAst.loc_of e, [], e) :: acc] ];




(* Add a sequence delimiter to the semi delimiter
   antiquot is also decorated
 *)  
let mksequence ?loc = fun
  [ {| $_; $_ |}
  | {| $anti:_ |} as e ->
      let _loc =
        match loc with [Some x -> x | None -> _loc] in
      {| begin  $e end |}
  | e -> e ];

(* see [mksequence], antiquot is not decoreated *)  
let mksequence' ?loc = fun
  [ {| $_; $_ |} as e ->
    let _loc = match loc with
      [Some x -> x | None -> _loc] in
    {| begin  $e  end |}
  | e -> e ];

  


let mkassert loc = fun
  [ {| false |} -> {@loc| assert false |} 
  | e -> {@loc| assert $e |} ] ;

(*
  Examples:
  {[
  bigarray_get _loc {|a|} {|(b,c,d)|} |> FanBasic.p_expr f;
  ]}
 *)  
let bigarray_get loc arr arg =
  let coords =
    match arg with
    [ {| ($e1, $e2) |} | {| $e1, $e2 |} ->
      FanAst.list_of_com' e1 (FanAst.list_of_com' e2 [])
    | _ -> [arg] ] in
  match coords with
  [ [] -> failwith "bigarray_get null list"
  | [c1] -> {@loc| $arr.{$c1} |}  
  | [c1; c2] -> {@loc| $arr.{$c1,$c2} |}  
  | [c1; c2; c3] -> {@loc| $arr.{$c1,$c2,$c3} |} 
  | [c1;c2;c3::coords] ->
      {@loc| $arr.{$c1,$c2,$c3,$(FanAst.sem_of_list coords) } |} ];


(*
  Example:
  {[
  bigarray_set _loc {|a.{b,c,d}|} {|3+2|} |> Option.get |> FanBasic.p_expr f;
  a.{b,c,d} <- (3 + 2)
  ]}
  FIXME
    1.ExArr, 2. can we just write $list:coords?
    Technically, we cannot, it uses [Pexp_array], pattern match doesnot work here
     {:expr|a.{1,2,3,4,$rest:x}|}
 *)
let bigarray_set loc var newval =
  match var with
  [ {|  $arr.{$c1} |} ->
    (* Some {@loc|Bigarray.Array1.set $arr $c1 $newval |} *)
      Some {@loc| $arr.{$c1} <- $newval |}
  | {|  $arr.{$c1, $c2} |} ->
      (* Some {@loc|Bigarray.Array2.set $arr $c1 $c2 $newval|} *)
      Some {@loc|  $arr.{$c1, $c2} <-  $newval |}
  | {|  $arr.{$c1, $c2, $c3} |} ->
      (* Some {@loc|Bigarray.Array3.set $arr $c1 $c2 $c3 $newval |} *)
      Some {@loc| $arr.{$c1,$c2,$c3} := $newval |}
        (* $arr.{$c1,$c2,$c3,$c4,$list:y}*)
  |  {| Bigarray.Genarray.get $arr [| $coords |] |} -> (* FIXME how to remove Bigarray here?*)
      Some {@loc| Bigarray.Genarray.set $arr [| $coords |] $newval |}
  | _ -> None ];
  

(* Utilities for [Stream] optimizations  *)
let rec pattern_eq_expression p e =
  match (p, e) with
  [ ({:patt| $lid:a |}, {@_| $lid:b |}) 
  | ({:patt| $uid:a |}, {@_| $uid:b |}) -> a = b
  | ({:patt| $p1 $p2 |}, {@_| $e1 $e2 |}) ->
      pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
  | _ -> false ] ;

  
(* +-----------------------------------------------------------------+
   | utilities for list comprehension                                |
   +-----------------------------------------------------------------+ *)
(* loc -> patt -> expr -> expr -> expr     *)
let map loc (p:patt) (e:expr) (l:expr) :expr =
  match (p, e) with
  [ ({:patt| $lid:x |}, {@_| $lid:y |}) when x = y -> l
  | _ ->
      if FanAst.is_irrefut_patt p then
        {@loc| List.map (fun $p -> $e) $l |}
      else
        {@loc| List.fold_right
          (fun
            [ $pat:p when true -> (fun x xs -> [ x :: xs ]) $e
            | _ -> (fun l -> l) ]) $l [] |} ];


let filter loc p b l =
    if FanAst.is_irrefut_patt p then
      {@loc| List.filter (fun $p -> $b) $l |}
    else
      {@loc| List.filter (fun [ $pat:p when true -> $b | _ -> false ]) $l |};
  
let concat _loc l = {| List.concat $l |};

(* only this function needs to be exposed *)
let rec compr _loc e =  fun
    [ [`gen (p, l)] -> map _loc p e l
    | [`gen (p, l); `cond b :: items] ->
        compr _loc e [`gen (p, filter _loc p b l) :: items]
    | [`gen (p, l) :: ([ `gen (_, _) :: _ ] as is )] ->
        concat _loc (map _loc p (compr _loc e is) l)
    | _ -> raise Stream.Failure ];


(* +-----------------------------------------------------------------+
   | Utiliies for macro expansion                                    |
   +-----------------------------------------------------------------+ *)
  
let bad_patt _loc =
  FanLoc.raise _loc
    (Failure
       "this macro cannot be used in a pattern (see its definition)");

(* Environment is a [string*patt] pair,

   Try to convert the 
   [expr] node into [patt] node.
   when do the conversion, if the expr node has an identifier which
   has a special meaning, then that replacment will be used
 *)  
let substp loc env =
  let rec loop = with {patt:expr;expr:patt} fun
    [ {| $e1 $e2 |} -> {@loc| $(loop e1) $(loop e2) |} 
    | {| |} -> {@loc| |}
    | {| $lid:x |} ->
        try List.assoc x env with
          [ Not_found -> {@loc| $lid:x |} ]
    | {| $uid:x |} ->
        try List.assoc x env with
          [ Not_found -> {@loc| $uid:x |} ]
    | {| $int:x |} -> {@loc| $int:x |}
    | {| $str:s |} -> {@loc| $str:s |}
    | {| $tup:x |} -> {@loc| $(tup:loop x) |}
    | {| $x1, $x2 |} -> {@loc| $(loop x1), $(loop x2) |}
    | {| { $bi } |} ->
        let rec substbi = with {patt:rec_binding;expr:patt} fun
          [ {| $b1; $b2 |} -> {@loc| $(substbi b1); $(substbi b2) |}
          | {| $id:i = $e |} -> {@loc| $i = $(loop e) |}
          | _ -> bad_patt _loc ] in
        {@loc| { $(substbi bi) } |}
    | _ -> bad_patt loc ] in loop;

(*
  [env] is a list of [string*expr],

  traverse the [expr] node
  when the identifier in pos expr in the expr has a speical meaning, using that instead
  when the identifier in pos patt in the expr has a special meaning,
  try to convert the expr meaning into patt and use that instead
 *)  
class subst loc env = object
  inherit FanAst.reloc loc as super;
  method! expr =
    fun
    [ {| $lid:x |} | {| $uid:x |} as e ->
        try List.assoc x env with
        [ Not_found -> super#expr e ]
    | {| LOCATION_OF $lid:x |} | {| LOCATION_OF $uid:x |} as e ->
        try
          let loc = FanAst.loc_of (List.assoc x env) in
          let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple loc in
          {| FanLoc.of_tuple
            ($`str:a, $`int:b, $`int:c, $`int:d,
             $`int:e, $`int:f, $`int:g,
             $(if h then {| true |} else {| false |} )) |}
        with [ Not_found -> super#expr e ]
    | e -> super#expr e ];
  method! patt =  fun
    [ {:patt| $lid:x |} | {:patt| $uid:x |} as p ->
      (* convert expression into pattern only *)
       try substp loc [] (List.assoc x env) with 
       [ Not_found -> super#patt p ]
    | p -> super#patt p ];
end;


class type antiquot_filter =object
  inherit FanAst.map;
  method get_captured_variables: list (expr * expr);
  method clear_captured_variables: unit;
end;
  
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot : antiquot_filter = object
  inherit FanAst.map as super;
  val mutable constraints =[];
  method! patt = fun
  [ `Ant(_loc,s)
      (* {:patt@_loc| $anti:s |} *)
  (* | (\* {:patt@_loc| $str:s |} *\) *)
  (*   `Str(_loc,s) as p when is_antiquot s *) -> (* begin *)
      match s with
     [ {content=code;_} ->
       begin 
      (* eprintf "Warning: the antiquot modifier %s is ignored@." name; *)
      let cons = {| $lid:code |} in
      let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
      let cons' = {| $lid:code' |} in 
      let () = constraints <- [(cons,cons')::constraints]in 
      {:patt| $lid:code' |} (* only allows lidentifiers here *)
    end
     ]
    (* match view_antiquot s with *)
    (* [Some(_name,code) -> begin  *)
    (*   (\* eprintf "Warning: the antiquot modifier %s is ignored@." name; *\) *)
    (*   let cons = {| $lid:code |} in *)
    (*   let code' = "__fan__"^code in  (\* prefix "fan__" FIXME *\) *)
    (*   let cons' = {| $lid:code' |} in  *)
    (*   let () = constraints <- [(cons,cons')::constraints]in  *)
    (*   {:patt| $lid:code' |} (\* only allows lidentifiers here *\) *)
    (* end *)
  (* | None -> p ];    *)
  (* end *)
  | p -> super#patt p ];
 method get_captured_variables =
   constraints;
 method clear_captured_variables =
   constraints <- [];
end;

let filter_patt_with_captured_variables patt= begin 
  capture_antiquot#clear_captured_variables;
  let patt=capture_antiquot#patt patt in
  let constraints = capture_antiquot#get_captured_variables in
  (patt,constraints)
end;




(*
  Given [args] and [body], generate an expression
  when [args] is nil, adding a [unit]

  Examples:
  {[
  fun_args _loc [{:patt|a|};{:patt|c|};{:patt|b|}] {|c|} |> FanBasic.p_expr f;
  fun a  c  b  -> c
  ]}
 *)
let fun_args _loc args body =
  if args = [] then {| fun () -> $body |}
  else
    List.fold_right
      (fun arg body ->
	{| fun $arg -> $body |}) args body;
  


let _loc = FanLoc.ghost ;
(* DEFINE GETLOC(expr)= FanAst.loc_of(\* _expr *\) expr;   *)
(* INCLUDE "src/Lib/CommonStructure.ml"; *)
INCLUDE "src/Lib/ExprPatt.ml";

(* Given a [location] and [prefix](generally "-" or "-.")
   The location provided is more precise.
   since ocaml respect [(~-)] as a prefix [(-)]
   and [(~-.)] as a prefix [(-.)]
   {[
   mkumin _loc "-." {| 3 |};
   - : expr = Int (, "-3")
   mkumin _loc "-." {| a |};
   - : expr =
   App (, ExId (, Lid (, "~-.")), ExId (, Lid (, "a")))
   ]}
 *)  
let mkumin loc prefix arg =
  match arg with
  [ {| $int:n |} -> {@loc| $(int:String.neg n) |}
  | {| $int32:n |} -> {@loc| $(int32:String.neg n) |}
  | {| $int64:n |} -> {@loc| $(int64:String.neg n) |}
  | {| $nativeint:n |} -> {@loc| $(nativeint:String.neg n) |}
  | {| $flo:n |} -> {@loc| $(flo:String.neg n) |}
  | _ -> {@loc| $(lid:"~" ^ prefix) $arg |} ];

    

let mk_assert  =  fun
  [ {| false |} ->    {| assert false |} 
  | e -> {| assert $e |} ];


(*
  Example:
  {[
  mk_record [("a",{|3|});("b",{|4|})] ;
  - : expr = { a = 3; b = 4 }

  ]}
  FIXME: label is lid, it can be more precise
  [mk_record] becomes a bit complex when you have to consider
  the arity
 *)
let mk_record label_exprs =
  let rec_bindings = List.map (fun (label, expr) ->
    {:rec_binding| $lid:label = $expr |} ) label_exprs in
  {| { $list:rec_bindings } |};


(* TBD *)
let failure =
  {| raise (Failure "metafilter: Cannot handle that kind of types ") |};       


(*
  Example:
  {[
  ["a";"b"] <+ {|3|};
  - : expr = fun a  b  -> 3
  ]}
 *)
let (<+) names acc  =
  List.fold_right (fun name acc ->  {| fun [ $lid:name -> $acc ]|}) names acc ;

(*
  Example:
  {[
  [{:patt|a|}; {:patt|b|} ] <+< {|3|};
  - : expr = fun a  b  -> 3
  ]}
 *)  
let (<+<) patts acc =
  List.fold_right (fun p acc -> {| fun [ $pat:p -> $acc] |} ) patts acc;



(* +-----------------------------------------------------------------+
   | Multiple staging code generation.                               |
   +-----------------------------------------------------------------+ *)
  



(*
  {[
  mep_app {| a |} {|g |};
  - : expr = Ast.App (_loc, a, g)
  mee_app {:expr|f a |} {:expr|g |};
  - : expr = Ast.App (_loc, (f a), g)
   ]}
 *)
  
let mep_comma x y =  {| {:patt| $($x), $($y) |} |};
  (* {| `PaCom (_loc, $x, $y) |}; *)
let mvep_comma x y =
  {|`PaCom(_loc,$x,$y)|};
  
let mee_comma x y = {| {| $($x), $($y) |} |};
  (* {| `Com (_loc, $x, $y) |}; *)
let mvee_comma x y = {| `Com (_loc,$x,$y) |};

let mee_app x y = {| {| $($x) $($y) |}|};
  (* {| `App(_loc, $x, $y) |}; *)
let vee_app x y = {| `App (_loc,$x,$y) |};
  
let mep_app x y = {| {:patt| $($x) $($y) |}|};
  (* {| `App (_loc, $x, $y) |};        *)
let vep_app x y = {| `App (_loc,$x,$y)|};
  

(*
   
  Example:
  {[
  mep_of_str "B" = {|{:patt| B |}|};
  - : bool = true
  ]}
  FIXME
  {|{:patt|`B|}|}
  {|{:patt|B|}|}
 *)  

let mep_of_str  s =
  let len = String.length s in
  if s.[0] = '`' then
    let s = String.sub s 1 (len - 1 ) in
    (* {| {:patt|`$($str:s)|}|} *)
      {| {:patt|$(vrn:($str:s))|}|}
  else
   let u = {| {:ident| $(uid:$str:s) |} |} in 
   {| {:patt| $(id:$u) |} |};
    (* let u = {| Ast.Uid _loc $str:s |} in *)
  (* {| Ast.PaId _loc $u |}; *)

(*
  FIXME bootstrap
  Here [s] should be a capital alphabet
  {[
  mee_of_str "A" = {| {| A |}|};
  - : bool = true
  ]}
  FIXME
 *)   
let mee_of_str s =
  (* let u = {| |} *)
  let len = String.length s in
  if s.[0]='`' then
    let s = String.sub s 1 (len - 1) in 
    (* {| {| `$($str:s) |} |} *)
    {|{|$(vrn:($str:s))|}|}
  else
    let u = {| {:ident| $(uid:$str:s) |} |} in
    {| {| $(id:$u) |} |};
    (* {| {| $(uid:$s)|}|} *)
      (* {| A |}
           `App
    (_loc,
      (`App
         (_loc, (`Vrn (_loc, "ExId")),
           (`ExId (_loc, (`Lid (_loc, "_loc")))))),
      (`App
         (_loc, (`Vrn (_loc, "Uid")),
           (`Tup
              (_loc,
                (`Com (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))), s)))))))
         
         `ExId (_loc, (`Uid (_loc, "A")))
         {:expr| `Uid (_loc,"A") |}
         {:expr| `ExId (_loc, (`Uid (_loc, "A"))) |}
         {:expr| {:expr| A |}|}
         {| {| A |}|}
       *)


(*
  {|{|A|}|}
 *)  
(*
  {[
  vee_of_str "A" = {| {|`A|} |};
  true
  ]}
  BOOTSTRAPPING
  *)
(* let vee_of_str s = *)
(*   {| Vrn _loc $str:s|}; *)

let vee_of_str s =
  {| `Vrn (_loc,$str:s) |};

let vep_of_str s =
  {| `Vrn (_loc,$str:s)|};
(*
  Examples:
  {[
  meee_of_str "A" = {| {| {| A |}|}|};
  ]}
 *)
let meee_of_str s =
  let u = {| {| {:ident| $(uid:$(str:$(str:s))) |} |} |} in 
  {| {| {| $(id:$($u))|}|}|};


(*
   @raise Invalid_argument
   
   There are 2 stages here 
   We want to generate code  like this
   {[

    {|  {| ( $(meta_int _loc x0), $(meta_int _loc x1) ) |}  |}
   ]}

  Normal practice:
  First print the result, then find a mechanical way to   construct

  Here we should avoid singleton tuple error
  {| $tup:a |} when a is  single, it will cause error FIXME

 *)      

  
(*
  Here we want to generate something like
  
  {[
  ({| {| { u = $meta } |} |} );
  ]}
  [meta] could be parameterized
  
  First we need to construct this part
  {[
  (App 
       (App  (ExId  (IdAcc  (Uid  "Ast") (Uid  "RbEq")))
         (ExId  (Lid  "_loc")))
       (App 
         (App  (ExId  (IdAcc  (Uid  "Ast") (Uid  "Lid")))
           (ExId  (Lid  "_loc")))
         (Str  "u")))
  ]}
  given string input u
  we finally want to make 
  {[
  {| << {u = $meta_u$ ; v = $meta_v$ } |} >> 
  ]}
  given string input "u" and [ {| meta_u |} ]
 *)

(* (\* *)
(*    {[ *)
(*    {| <:patt< { u = $meta_u$ ; v = $meta_v$ } |} >> ; *)
(*    ]} *)
(*  *\)   *)
(* let mep_record_left str = *)
(*   let u = {| Ast.Lid _loc $str:str |} in *)
(*   {| Ast.PaEq _loc $u |}; *)
  
  
  
let mk_tuple_ee = fun 
  [ [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      {| `Tup (_loc, $(List.reduce_right mee_comma xs)) |}];

let mk_tuple_vee = fun 
  [ [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      {| `Tup (_loc, $(List.reduce_right mvee_comma xs)) |}];

(*
  We want to generate code 
   {[
   {:expr| {:patt| (A,B,C) |} |}
   ]}
  But [A],[B],[C] should be parameterized here 
  Example:
  {[
  mk_tuple_ep [{|a|}; {|b|} ] = {| {:patt| ($($(lid:"a")),$($(lid:"b"))) |} |};
  - : bool = true
  ]}
 *)
let mk_tuple_ep = fun 
  [ [] -> assert false
  | [x] -> x
  | xs  ->
    (* {| Ast.PaTup _loc $(List.reduce_right mep_comma xs) |} *)
      {| {:patt|$(tup: $(List.reduce_right mep_comma xs))|} |} ];

let mk_tuple_vep = fun
  [[] -> assert false
  |[x] -> x
  |xs -> {| `PaTup (_loc,$(List.reduce_right mvep_comma xs))|} ];
  
  
(*
  Example:
  {[
  mee_record_col "a" {|3|} = {| {:rec_binding| a = $($({|3|})) |}|};
  ]}
 *)
let mee_record_col label expr =
  {| {:rec_binding| $(lid:($str:label)) = $($expr) |}|};

(*
  Example:
  {[
  mep_record_col "a" {|3|} = {| {:patt| a = $($({|3|}))|}|};
  ]}
  *)  
let mep_record_col label expr =
  {| {:patt| $(lid:$(str:label)) = $($expr) |} |};

let mee_record_semi a b =
  {| {:rec_binding| $($a);$($b) |} |};


let mep_record_semi a b =
  {| {:patt| $($a); $($b) |}|};


(*
  Example:
  {[
  mk_record_ee [("a",{|3|})] = {| {| { a = $($({|3|})) }|}|};
  ]}
 *)  
let mk_record_ee label_exprs = 
  label_exprs
  |> List.map (fun (label,expr) -> mee_record_col label expr)
  |> (fun es -> {| {| { $($(List.reduce_right mee_record_semi es)) } |}|} );

    

(*
  Example:
  {[
  mk_record_ep [("u",  {|3 |})] = {| {:patt| { u = $($({|3|}))} |} |};
  ]}
 *)    
let mk_record_ep label_exprs = let open List in
  label_exprs
  |> map (fun (label,expr) -> mep_record_col label expr)
  |> (fun es -> {| {:patt| { $($(List.reduce_right mep_record_semi es)) } |} |});



(* Mainly used to overcome the value restriction
   {[
    eta_expand {|f |} 3 |> FanBasic.p_expr f;
    fun a0  a1  a2  -> f a0 a1 a2
   ]}
 *)
let eta_expand expr number =
  let names = List.init number (fun i -> x ~off:0 i ) in
  names <+ (expr +> names);


(*
  Example:
  {[
  gen_curry_n {|3|} ~arity:2 "`X" 2 ;
  fun [ `X (a0, a1) -> fun [ `X (b0, b1) -> 3 ] ]

  gen_curry_n {|3|} ~arity:2 "X" 2 ;
  fun (X (a0,a1))  (X (b0,b1))  -> 3
  ]}
  
 *)
let gen_curry_n acc ~arity cons n =
  let args = List.init arity
      (fun i -> List.init n (fun j -> {:patt| $(id:xid ~off:i j) |})) in
  let pat = Patt.of_str cons in
  List.fold_right
    (fun p acc -> {| fun [ $pat:p -> $acc ] |} )
    (List.map (fun lst -> appl_of_list [pat:: lst]) args) acc;

(*
  Example:
  {[
   let u  =  FanAst.list_of_or' {:match_case|
  (A0 (a0, a1),A0 (b0, b1)) -> 1
  |   (A1 (a0, a1), A1 (b0, b1)) -> 2
  |   (A2 (a0, a1), A2 (b0, b1)) -> 3 |} [] in currying ~arity:2 u ;

  fun a0  b0  ->
  match (a0, b0) with
  | (A0 (a0,a1),A0 (b0,b1)) -> 1
  | (A1 (a0,a1),A1 (b0,b1)) -> 2
  | (A2 (a0,a1),A2 (b0,b1)) -> 3
  ]}

  Make Sure the names generated are shadowed by
  gen_tuple_n
 *)  
let currying match_cases ~arity =
  (* let branches = List.length match_cases in *)
  if  arity >= 2 then 
    let names = List.init arity (fun i -> x ~off:i 0) in
    let exprs = List.map (fun s-> {| $lid:s |} ) names in
    names <+ {| match $((* tuple_of_list *)tuple_com exprs) with [ $list:match_cases ] |}
  else {| fun [ $list:match_cases ] |};


let unknown len =
  if len = 0 then
    {| self# $(`Lid (_loc, "unknown")) |} (* FIXME*)
  else {| failwith $(str:"not implemented!") |};

  
(* let normalize = object *)
(*   val expr:Ast.expr; *)
(*   inherit FanAst.fold as super; *)
(*   method! patt = with "patt" fun *)
(*     [ {| $_ |} -> {| "_" |} *)
(*     | {| $lid:_ |} -> {| "_" |} *)
(*     | {| $p as $_ |} -> self#patt p  *)
(*     ] *)
(* end; *)


(* let rec string_of_ident = (\* duplicated with Camlp4Filters remove soon*\) *)
(*   fun *)
(*   [ {:ident| $lid:s |} -> s *)
(*   | {:ident| $uid:s |} -> s *)
(*   | {:ident| $i1.$i2 |} -> "acc_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2) *)
(*   | {:ident| ($i1 $i2) |} -> "app_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2) *)
(*   | {:ident| $anti:_ |} -> assert false ]; *)

    
(* let rec normalize = let _loc = FanLoc.ghost in with "patt" fun *)
(*   [ {| _ |} -> {|"_"|} *)
(*   | {| $id:_|} -> {:expr| "_"|} *)
(*   | {| ($p as $_) |} -> normalize p *)
(*   | {| $p1 $p2 |} -> {:expr| $(normalize p1) ^ $(normalize p2) |} *)
(*   | {| [| $p |]|} -> {:expr| "[|"^ $(normalize p) ^ "|]"|} (\* FIXME ^$ does not work *\) *)
(*   | {| $p1;$p2 |} -> {:expr| $(normalize p1) ^ ";" ^  $(normalize p2) |} *)
(*   | {| $p1,$p2|} ->  {:expr| $(normalize p1) ^ "," ^ $(normalize p2) |} *)
(*   | {| $chr:x |} -> {:expr| "'" ^ String.make 1 $chr:x ^ "'" |} *)
(*   | {| $int:x |} -> {:expr| $str:x |} *)
(*   | {| $int32:x |} -> {:expr| $str:x |} *)
(*   | {| $int64:x |} -> {:expr| $str:x |} *)
(*   | {| $nativeint:x |} -> {:expr| "\"" ^ $str:x ^ "\""|}  *)
(*   | {| $str:s |} -> {:expr| $str:s |} *)
(*   | {| lazy $p |} -> {:expr| "lazy" ^ $(normalize p)|} *)
(*   | {| (module $s) |}  -> {:expr| "(module" ^ $str:s ^")"|} *)
(*   | {| $flo:x |} -> {:expr| $str:x|} *)

(*   | {| $p1 | $p2 |} -> {:expr| $(normalize p1)  ^ "|" ^ $(normalize p2)  |} *)
        
(*   | {| $p1 .. $p2 |} -> {:expr| $(normalize p1) ^ ".." ^ $(normalize p2) |} *)
        
(*   | {| {$p} |} -> {:expr| "{" ^ $(normalize p)^ "}"|} *)
(*   | {| $i = $p |} -> *)
(*       {:expr| $(str:string_of_ident i) ^"=" ^ $(normalize p) |} *)

(*   | {| ($tup:pl) |} -> {:expr| "("^ $(normalize pl) ^")"|} *)
(*   | {| ($p:$_)|} -> normalize p (\* type was ignored *\) *)
(*   | {| `$s |} -> {:expr| "`" ^ $str:s |} *)
(*   (\* | {| $anti:x |} -> Syntax.parse_expr *\) *)
(*   | {|$anti:_|} | {||} *)
(*     | {| ? $_ |} | (\* FIXME ?$ not supported *\) *)
(*       {| ? $_ : ($_) |} | {| ? $_ : ($_ = $_ )|} | *)
(*       {| ~ $_ |} | {| ~ $_ : $_ |} | {| #$_ |}  *)
(*       -> assert false *)
(*   ]; *)


(* depcrated use apply instead *)  
(*  
let fun_apply _loc e args =
  if args = [] then {| $e () |}
  else
    List.fold_left
      (fun e arg ->
        {| $e $arg |}) e args;
*)
