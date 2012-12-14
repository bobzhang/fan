(* open Format; *)
(* open lang "expr"; *)
#default_quotation     "expr";;


(* +-----------------------------------------------------------------+
   | the modules documented with [open Lib.Expr]                     |
   +-----------------------------------------------------------------+ *)




open LibUtil;
open Basic;
open FanUtil;
module Ast= Camlp4Ast; (* it contains a module named Meta *)


(*
  The input is either {|$_.$_|} or {|$(id:{:ident| $_.$_|})|}
  the type of return value and [acc] is
  [(loc* string list * expr) list]

  The [string list] is generally a module path, the [expr] is the last field

  Examples:

  {[
  sep_dot_expr [] {|A.B.g.U.E.h.i|};
  - : (L.Expr.Ast.loc * string list * L.Expr.Ast.expr) list =
  [(, ["A"; "B"], ExId (, IdLid (, "g")));
  (, ["U"; "E"], ExId (, IdLid (, "h"))); (, [], ExId (, IdLid (, "i")))]

  sep_dot_expr [] {|A.B.g.i|};
  - : (L.Expr.Ast.loc * string list * L.Expr.Ast.expr) list =
  [(, ["A"; "B"], ExId (, IdLid (, "g"))); (, [], ExId (, IdLid (, "i")))]

  sep_dot_expr [] {|$(uid:"").i|};
  - : (L.Expr.Ast.loc * string list * L.Expr.Ast.expr) list =
  [(, [""], ExId (, IdLid (, "i")))]

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
  | e -> [(Ast.loc_of_expr e, [], e) :: acc] ];


(* It is the inverse operation by [view_app]
   Example:
   {[
   apply {|a|} [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
   a b c d
   ]}
 *)
let rec apply accu = fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = Ast.loc_of_expr x
      in apply {| $accu $x |} xs ];

(*
  Given an location, and a list of expression node,
  return an expression node which represents the list
  of the expresson nodes

  Example:
  {[
  mklist _loc [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
  [b; c; d]
  ]}
 *)
let mklist loc =
  let rec loop top =  fun
    [ [] -> {| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (Ast.loc_of_expr e1) loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;

(* Add a sequence delimiter to the semi delimiter
   antiquot is also decorated
 *)  
let mksequence loc = fun
  [ {| $_; $_ |}
  | {| $anti:_ |} as e -> {@loc| begin  $e end |}
  | e -> e ];

(* see [mksequence], antiquot is not decoreated *)  
let mksequence' loc = fun
  [ {| $_; $_ |} as e -> {@loc| begin  $e  end |}
  | e -> e ];

  

(* Given a [location] and [prefix](generally "-" or "-.")
   The location provided is more precise
 *)  
let mkumin loc prefix arg =
  match arg with
  [ {| $int:n |} -> {@loc| $(int:neg_string n) |}
  | {| $int32:n |} -> {@loc| $(int32:neg_string n) |}
  | {| $int64:n |} -> {@loc| $(int64:neg_string n) |}
  | {| $nativeint:n |} -> {@loc| $(nativeint:neg_string n) |}
  | {| $flo:n |} -> {@loc| $(flo:neg_string n) |}
  | _ -> {@loc| $(lid:"~" ^ prefix) $arg |} ];


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
      Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
    | _ -> [arg] ] in
  match coords with
  [ [] -> failwith "bigarray_get null list"
  | [c1] -> {@loc| $arr.{$c1} |}  
  | [c1; c2] -> {@loc| $arr.{$c1,$c2} |}  
  | [c1; c2; c3] -> {@loc| $arr.{$c1,$c2,$c3} |} 
  | [c1;c2;c3::coords] ->
      {@loc| $arr.{$c1,$c2,$c3,$(Ast.exSem_of_list coords) } |} ];


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
    
let map loc p e l =
  match (p, e) with
  [ ({:patt| $lid:x |}, {@_| $lid:y |}) when x = y -> l
  | _ ->
      if Ast.is_irrefut_patt p then
        {@loc| List.map (fun $p -> $e) $l |}
      else
        {@loc| List.fold_right
          (fun
            [ $pat:p when true -> (fun x xs -> [ x :: xs ]) $e
            | _ -> (fun l -> l) ]) $l [] |} ];


let filter loc p b l =
    if Ast.is_irrefut_patt p then
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
  let rec loop = fun
    [ {| $e1 $e2 |} -> {:patt@loc| $(loop e1) $(loop e2) |} 
    | {| |} -> {:patt@loc| |}
    | {| $lid:x |} ->
        try List.assoc x env with
          [ Not_found -> {:patt@loc| $lid:x |} ]
    | {| $uid:x |} ->
        try List.assoc x env with
          [ Not_found -> {:patt@loc| $uid:x |} ]
    | {| $int:x |} -> {:patt@loc| $int:x |}
    | {| $str:s |} -> {:patt@loc| $str:s |}
    | {| $tup:x |} -> {:patt@loc| $(tup:loop x) |}
    | {| $x1, $x2 |} -> {:patt@loc| $(loop x1), $(loop x2) |}
    | {| { $bi } |} ->
        let rec substbi = fun
          [ {:rec_binding| $b1; $b2 |} -> {:patt@loc| $(substbi b1); $(substbi b2) |}
          | {:rec_binding| $id:i = $e |} -> {:patt@loc| $i = $(loop e) |}
          | _ -> bad_patt _loc ] in
        {:patt@loc| { $(substbi bi) } |}
    | _ -> bad_patt loc ] in loop;

(*
  [env] is a list of [string*expr],

  traverse the [expr] node
  when the identifier in pos expr in the expr has a speical meaning, using that instead
  when the identifier in pos patt in the expr has a special meaning,
  try to convert the expr meaning into patt and use that instead
 *)  
class subst loc env = object
  inherit Ast.reloc loc as super;
  method! expr =
    fun
    [ {| $lid:x |} | {| $uid:x |} as e ->
        try List.assoc x env with
        [ Not_found -> super#expr e ]
    | {| LOCATION_OF $lid:x |} | {| LOCATION_OF $uid:x |} as e ->
        try
          let loc = Ast.loc_of_expr (List.assoc x env) in
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


  
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot = object
  inherit Camlp4Ast.map as super;
  val mutable constraints =[];
  method! patt = fun
  [ {:patt@_loc| $anti:s |} | {:patt@_loc| $str:s |} as p when is_antiquot s -> begin
    match view_antiquot s with
    [Some(_name,code) -> begin 
      (* eprintf "Warning: the antiquot modifier %s is ignored@." name; *)
      let cons = {| $lid:code |} in
      let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
      let cons' = {| $lid:code' |} in 
      let () = constraints <- [(cons,cons')::constraints]in 
      {:patt| $lid:code' |} (* only allows lidentifiers here *)
    end
  | None -> p ];   
  end
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
  
 *)  
let tuple _loc  =   fun
  [[] -> {|()|}
  |[p] -> p
  | [e::es] -> {| ($e, $list:es) |} ];
  
let fun_args _loc args body =
  if args = [] then {| fun () -> $body |}
  else
    List.fold_right
      (fun arg body ->
	{| fun $arg -> $body |}) args body;
  
let fun_apply _loc e args =
  if args = [] then {| $e () |}
  else
    List.fold_left
      (fun e arg ->
        {| $e $arg |}) e args;


let _loc = FanLoc.ghost ;  
INCLUDE "src/Lib/CommonStructure.ml";
INCLUDE "src/Lib/ExprPatt.ml";

let mk_unary_min f arg =
  match arg with
  [ {| $int:n |} -> {| $(int:String.neg n) |} 
  | {| $int32:n |} -> {| $(int32:String.neg n) |}
  | {| $int64:n |} -> {| $(int64:String.neg n) |}
  | {| $nativeint:n |} -> {| $(nativeint:String.neg n) |}
  | {| $flo:n |} -> {| $(flo:String.neg n) |}
  | _ -> {| $(lid:"~" ^ f) $arg |} ];
(*
   since ocaml respect [(~-)] as a prefix [(-)]
   and [(~-.)] as a prefix [(-.)]
   Example:
   {[
   mk_unary_min "-." <:expr< 3 >>;
    Camlp4.PreCast.Ast.expr = ExInt  "-3"

   mk_unary_min "-." <:expr< a >>;
    Camlp4.PreCast.Ast.expr =
   ExApp  (ExId  (IdLid  "~-.")) (ExId  (IdLid  "a"))

   ]}
 *)  

    

let mk_assert  =
  fun
  [ {| false |} ->    {| assert false |} 
  | e -> {| assert $e |} ];
(*
   Camlp4 treats [assert false] as a special construct [ExAsf]

   {[
   <:expr< assert false>>;

   Camlp4.PreCast.Ast.expr = ExAsf 

   <:expr< assert .$ <:expr< false >> $. >>;
   
   Camlp4.PreCast.Ast.expr = ExAsr  (ExId  (IdUid  "false"))

   e2s {|assert false|};
   [
   {Camlp4_import.Parsetree.pstr_desc=
   Camlp4_import.Parsetree.Pstr_eval
   {Camlp4_import.Parsetree.pexp_desc=
   Camlp4_import.Parsetree.Pexp_assertfalse;
   Camlp4_import.Parsetree.pexp_loc=};
   Camlp4_import.Parsetree.pstr_loc=}]

   e2s    <:expr< assert .$ <:expr< false >> $. >>;

   {Camlp4_import.Parsetree.pstr_desc=
   Camlp4_import.Parsetree.Pstr_eval
    {Camlp4_import.Parsetree.pexp_desc=
      Camlp4_import.Parsetree.Pexp_assert
       {Camlp4_import.Parsetree.pexp_desc=
         Camlp4_import.Parsetree.Pexp_construct
          (Camlp4_import.Longident.Lident "false") None True;
        Camlp4_import.Parsetree.pexp_loc=};
     Camlp4_import.Parsetree.pexp_loc=};
  Camlp4_import.Parsetree.pstr_loc=}
   ]}
 *)      


let mk_record label_exprs =
  let rec_bindings = List.map (fun (label, expr) ->
    {:rec_binding| $lid:label = $expr |} ) label_exprs in
  {| { $list:rec_bindings } |};
(*
   FIXME: label is lid
 *)



let failure =
  {| raise (Failure "metafilter: Cannot handle that kind of types ") |}
;       



let (<+) names acc  =
  List.fold_right (fun name acc ->  {| fun [ $lid:name -> $acc ]|})
    names acc ;
  
let (<+<) patts acc =
  List.fold_right (fun p acc -> {| fun [ $pat:p -> $acc] |} ) patts acc;
(*
  {[
   gen_app_first 3 2  |> eprint;
   c0 c1 c2
   Warning: strange pattern application of a non constructor
  ]}
 *)
(* let gen_app_first ~number ~off =
 *   List.init number (fun i -> {| .$id:xid ~off i$. |}) |> app_of_list
 * ; *)
    
(*
   Add Prefixes to expression
   {[

    ["x0";"x1";"x2"] <+ {| blabla|} |> eprint;
    fun x0 x1 x2 -> blabla

   ]}
 *)  

  
let mk_seq es =
  let _loc = FanLoc.ghost in 
  {| $(seq:Ast.exSem_of_list es) |};
(*
   {[
   mkseq [ <:expr<f >> ;
           <:expr< a >> ; <:expr< b >>
        ] |> eprint
   ;

   (f; a; b)
   ]}
 *)


let mep_comma x y =
  {| Ast.PaCom _loc $x $y |};
(*
   Notice for meta, we should annote quotation expander
   expr explicitly
 *)    

  
let mep_app x y =  {| Ast.PaApp _loc $x $y |};       
(*
   {[
   mep_app <:patt< a >> <:patt<g >> |> eprint 
   Ast.PaApp _loc a g
   ]}
 *)

  

let mk_tuple_ep = fun 
   [ [] -> assert false
   | [x] -> x
   | xs  ->
       {| Ast.PaTup _loc $(List.reduce_right mep_comma xs) |}
   ]
;
(*

   We want to generate code 
   {[
   <:expr< <:patt< (A,B,C) >> >>
   ]}

   {[
   mk_tuple_ep
   [ <:patt< f >> ;
   <:patt< a >> ;
   <:patt< b>> ] |> eprint ;
   
   Ast.PaTup _loc (Ast.PaCom _loc f (Ast.PaCom _loc a b))
   ]}
 *)


(*
   {[
   meta_of_str "A" = <:expr< <:patt< A >> >> ;
   True 
   ]}
 *)  

let mep_of_str  s =
  let u = {| Ast.IdUid _loc $str:s |} in
  {| Ast.PaId _loc $u |}
;

let mee_of_str s =
  let u = {| Ast.IdUid _loc $str:s |} in 
  {| Ast.ExId _loc $u |};
(*
   {[
    meta_of_str "A" = {| << A |} >> ;
    True
    ]}
 *)

(*
   @raise Invalid_argument
   
   There are 2 stages here 
   We want to generate code  like this
   {[
   <<
       {| ($meta_int _loc x0$, $meta_int _loc x1$ ) |}
   >>
   ]}

   {[
   ( <:expr< <:expr< (A,B,C) >> >>   |> eprint );

   Ast.ExTup _loc
   (Ast.ExCom _loc (Ast.ExId _loc (Ast.IdUid _loc "A"))
   (Ast.ExCom _loc (Ast.ExId _loc (Ast.IdUid _loc "B"))
        (Ast.ExId _loc (Ast.IdUid _loc "C"))))

   ]}
   
   Normal practice:
     first print the result, then find a mechanical way to
     construct
   Here we should avoid singleton tuple error
   {| .$tup:a$. |} when a is a single, will cause error

   when dumped
   {[

   mk_tuple_ee
   [ <:expr< f >> ; <:expr< a >> ; <:expr< b>> ] |> eprint;

   Ast.ExTup _loc (Ast.ExCom _loc f (Ast.ExCom _loc a b))

   ]}
 *)      

(*
let mee_semi_col x y =
  {| Ast.RbSem _loc $x$ $y$ |} ;
  
(* let meta_semi_col x y = {| Ast.RbEq   |}*)
let mk_record_ee label_exprs =
  let rec_bindings = List.map
      (fun (label,expr) ->
        <:rec_binding< $lid:label$ = $expr$ >> ) label_exprs in
  {| Ast.ExRec _loc .$ reduce ~dir:`Right mee_semi_col rec_bindings$. |}
;  
*)

  
(*
  {[
  ({| << { u = .$meta $. } |} >> );
  ExApp 
  (ExApp 
   (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "ExRec")))
     (ExId  (IdLid  "_loc")))
   (ExApp 
     (ExApp 
       (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "RbEq")))
         (ExId  (IdLid  "_loc")))
       (ExApp 
         (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "IdLid")))
           (ExId  (IdLid  "_loc")))
         (ExStr  "u")))
     (ExId  (IdLid  "meta"))))
  (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "ExNil")))
  (ExId  (IdLid  "_loc")))

  ]}
  First we need to construct this part
  {[
  (ExApp 
       (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "RbEq")))
         (ExId  (IdLid  "_loc")))
       (ExApp 
         (ExApp  (ExId  (IdAcc  (IdUid  "Ast") (IdUid  "IdLid")))
           (ExId  (IdLid  "_loc")))
         (ExStr  "u")))
  ]}
  given string input u
  we finally want to make 
  {[
  {| << {u = $meta_u$ ; v = $meta_v$ } |} >> 
  ]}
  given string input "u" and [ {| meta_u |} ]
 *)
let mee_record_left str =
  let u = {| Ast.IdLid _loc $str:str |} in 
  {| Ast.RbEq _loc $u |} 
;

(*
   {[
   {| <:patt< { u = $meta_u$ ; v = $meta_v$ } |} >> ;
   ]}
 *)  
let mep_record_left str =
  let u = {| Ast.IdLid _loc $str:str |} in
  {| Ast.PaEq _loc $u |}
;
  
  
let mee_comma x y = {| Ast.ExCom _loc $x $y |};

  
let mee_app x y =   {| Ast.ExApp _loc $x $y |};
(*
   {[
    mee_app <:expr<f a >> <:expr<g >> |> eprint 
    Ast.ExApp _loc (f a) g
    ]}
*)
  
let mk_tuple_ee = fun 
  [ [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      {| Ast.ExTup _loc $(List.reduce_right mee_comma xs) |}];

let mee_record_col label expr =
  {| $(mee_record_left label) $expr |};
  
let mep_record_col label expr =
  {| $(mep_record_left label) $expr |};
  
let mee_record_semi a b =
  {| Ast.RbSem _loc $a $b |};

let mep_record_semi a b =
  {| Ast.PaSem _loc $a $b |};  

(*
   {[
   ( {| << {u = $meta_u$ ; v = $meta_v$ } |} >>
     |> e2s ) =
   (({| << {u = $meta_u$ ; v = $meta_v$ } |} >> |> e2s));

   True
   ]}
   They are syntaxlly different, the first has a trailing Nil.
 *)  
let mk_record_ee label_exprs = let open List in 
  label_exprs
  |> map (fun (label,expr) -> mee_record_col label expr)
  |> (fun es ->
      {|  Ast.ExRec _loc
         $(List.reduce_right mee_record_semi es) {| |} |} );

(*
  Syntactially not equivalent, but dumped result should be the same 
  {[
  (e2s {| <:patt< { u = $meta_u$ ; v = $meta_v$ } |} >>
  = 
  e2s (mk_record_ep [ ("u", {| meta_u|} ) ; ("v", {|meta_v|})]))
  ;
  ]}
 *)    
let mk_record_ep label_exprs = let open List in
  label_exprs
  |> map (fun (label,expr) -> mep_record_col label expr)
  |> (fun es ->
     {| Ast.PaRec _loc
        $(List.reduce_right mep_record_semi es) |} );



(* overcome the monomophism restriction
   {[
   eta_expand {| f|} 3 |> eprint;
    fun a0 a1 a2 -> f a0 a1 a2
   ]}
 *)
let eta_expand expr number =
  let names = List.init number (fun i -> x ~off:0 i ) in
  names <+ (expr +> names);


(*
  {[
  gen_curry_n {|3|} ~arity:2 "`X" 2 |> eprint;
  fun [ `X a0 a1 -> fun [ `X b0 b1 -> 3 ] ]
  ]}
 *)
let gen_curry_n acc ~arity cons n =
  let args = List.init arity
      (fun i -> List.init n (fun j -> {:patt| $(id:xid ~off:i j) |})) in
  let pat = Patt.of_str cons in
  List.fold_right
    (fun p acc -> {| fun [ $pat:p -> $acc ] |} )
    (List.map (fun lst -> Patt.apply pat lst) args) acc;

(*
  
  {[
  currying
  <:match_case<
  (A0 a0 a1,A0 b0 b1) -> 1
  | (A1 a0 a1, A1 b0 b1) -> 2
  | (A2 a0 a1, A2 b0 b1) -> 3
  >> ~arity:2 |> eprint ;
  fun a0 b0 ->
  match (a0, b0) with
  [ (A0 a0 a1, A0 b0 b1) -> 1
  | (A1 a0 a1, A1 b0 b1) -> 2
  | (A2 a0 a1, A2 b0 b1) -> 3 ]
  ]}
  make sure the names generated are shadowed by
  gen_tuple_n
 *)  
let currying match_cases ~arity =
  (* let branches = List.length match_cases in *)
  if  arity >= 2 then 
    let names = List.init arity (fun i -> x ~off:i 0) in
    let exprs = List.map (fun s-> {| $lid:s |} ) names in
    names <+ {| match $(tuple_of_list exprs) with [ $list:match_cases ] |}
  else {| fun [ $list:match_cases ] |};

(*
  {[
  unknown 3 |> eprint;
  fun _ _ _ -> self#unknown
  ]}
    *)
let unknown len =
  if len = 0 then
    {| self#unknown|}
  else {| failwith $(str:"not implemented!") |};

  
(* let normalize = object *)
(*   val expr:Ast.expr; *)
(*   inherit Camlp4Ast.fold as super; *)
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

