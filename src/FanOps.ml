open LibUtil;

open AstLoc;


(* +-----------------------------------------------------------------+
   | Common functions for Ast processing                             |
   +-----------------------------------------------------------------+ *)




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
let list_of_list (loc:loc) =
  let rec loop top =  with expr fun
    [ [] ->   {@ghost| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of e1) loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;

  
#default_quotation "expr";;

let meta_int _loc i =  {|$`int:i|};

let meta_int32 _loc i =  {|$`int32:i|};

let meta_int64 _loc i =  {|$`int64:i|};
  
let meta_nativeint _loc i =  {|$`nativeint:i|};
  
let meta_float _loc i = {|$`flo:i|};
  
let meta_string _loc i = {|$`str:i|};
  
let meta_char _loc i = {|$`chr:i|};
let meta_unit _loc _ =  {|()|};
let meta_bool _loc =  fun [true -> {|true|} | false -> {|false|} ];

let meta_ref mf_a _loc i =
  {| {contents= $(mf_a _loc !i) } |};

(* [mklist] and [mkarray]
   duplicated with ExprPatt to remove cyclic dependency *)
let mklist loc =
  let rec loop top =  fun
    [ [] -> {@loc| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of (e1)) loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;

let meta_list mf_a _loc  ls =
  mklist _loc (List.map (fun x -> mf_a _loc x ) ls ) ;
  
  
let meta_option mf_a _loc  = fun
  [ None -> {|None|}
  | Some x -> {|Some $(mf_a _loc x)|} ];

let meta_arrow (type t)
    (_mf_a: FanLoc.t -> 'a -> t)
    (_mf_b: FanLoc.t -> 'b ->t)
    (_loc: FanLoc.t)  (_x:'a -> 'b) = invalid_arg "meta_arrow not implemented";
    

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
let rec is_module_longident (x:ident) =
  match x with
  [`Dot(_,_,i) -> is_module_longident i
  |`App(_,i1,i2) -> is_module_longident i1 && is_module_longident i2
  | `Uid _ -> true
  | _ -> false ];  

let ident_of_expr =
  let error () = invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self (x:expr)=
    match x with 
    [ `App(_loc,e1,e2) -> `App(_loc,self e1, self e2)
    | `Dot(_loc,e1,e2) -> `Dot(_loc,self e1,self e2)
    | `Id(_loc,`Lid _ ) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () ] in 
  fun
    [ `Id(_loc,i) -> i
    | `App _ -> error ()
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
  let rec self  (x:ctyp) =
    match x with 
    [ `App(_loc,t1,t2) -> `App(_loc,self t1, self t2)
    | `Id(_loc,`Lid _ ) -> error ()
    | `Id(_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () ] in
    fun
    [ `Id(_loc,i) -> i
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



(* let rec is_constructor =  with ident fun *)
(*     [ {| $_.$i |} -> is_constructor i *)
(*     | {| $uid:_ |} -> true *)
(*     | {| $lid:_ |} | {| ($_ $_) |} -> false *)
(*     | {| $anti:_ |} -> assert false ]; *)

(* let is_patt_constructor = fun *)
(*     [ `Id(_,i) -> is_constructor i *)
(*     | `Vrn (_loc,_) *)
(*       -> true *)
(*     | _ -> false ]; *)

(* let rec is_expr_constructor = fun *)
(*     [ `Id(_,i) -> is_constructor i *)
(*     | `Dot(_,e1,e2) -> is_expr_constructor e1 && is_expr_constructor e2 *)
(*     | `Vrn(_loc,_) -> true *)
(*     | _ -> false ]; *)




let ty_of_stl = fun
    [ (_loc, s, []) ->
      `Id(_loc,`Uid(_loc,s))
      (* {:ctyp| $uid:s |} *)
    | (_loc, s, tl) ->
        `Of (_loc, `Id (_loc, `Uid (_loc, s)), and_of_list tl)];
        (* {:ctyp| $uid:s of $(and_of_list tl) |} *) 

let ty_of_sbt = fun
    [ (_loc, s, v, t) ->
      if v then
        `TyColMut (_loc, `Id (_loc, `Lid (_loc, s)), t)
      else
        `TyCol (_loc, `Id (_loc, `Lid (_loc, s)), t)];

let bi_of_pe (p, e) = let _loc = loc_of p in {:binding| $p = $e |};

let sum_type_of_list l = or_of_list (List.map ty_of_stl l);

let record_type_of_list l = sem_of_list (List.map ty_of_sbt l);

let binding_of_pel l = and_of_list (List.map bi_of_pe l);

(* FIXME should be amp *)  
(* let rec list_of_amp x acc = *)
(*   match x with *)
(*   [`Amp(_,x,y) -> list_of_amp x (list_of_amp y acc) *)
(*   | _ -> [x::acc] ]; *)

(* let rec list_of_amp' x acc = *)
(*   match x with *)
(*   [`Amp(_,x,y) -> list_of_amp' x (list_of_amp' y acc) *)
(*   | `Nil _ -> acc *)
(*   | _ -> [x::acc] ];     *)


  

    


let rec is_irrefut_patt (x: patt) = with patt
    match x with
    [
      `ArrayEmpty (_loc)
    | `LabelS (_loc,_)
    | {| $lid:_ |} -> true
    | {| () |} -> true
    | {| _ |} -> true
    (* | {||} -> true (\* why not *\) *)
    | {| ($x as $_) |} -> is_irrefut_patt x (* && is_irrefut_patt y *)
    | {| { $p } |} ->
        List.for_all (fun [`RecBind (_,_,p) -> is_irrefut_patt p | _ -> true])
          (list_of_sem  p [])
    | `Sem(_,p1,p2) -> is_irrefut_patt p1 && is_irrefut_patt p2
    | `Com(_,p1,p2) -> is_irrefut_patt p1 && is_irrefut_patt p2
    | `Or(_,p1,p2) -> is_irrefut_patt p1 && is_irrefut_patt p2 (* could be more fine grained *)
    | `App(_,p1,p2) -> is_irrefut_patt p1 && is_irrefut_patt p2
          
    | `Constraint(_,p,_) -> is_irrefut_patt p
    | `Tup(_,p) -> is_irrefut_patt p
    | `OptLablS _ -> true
    | `OptLabl(_,_,p) | `OptLablExpr(_,_,p,_) -> is_irrefut_patt p
    | `Label(_,_,p) | `Lazy (_,p) ->  is_irrefut_patt p
    | {| $id:_ |} -> false (* here one need to know the arity of constructors *)

    | `ModuleUnpack _ 
    | `ModuleConstraint _  -> true
    (* | {| (module $_ : $opt:_ ) |} -> true *)
    | (* {| `$_ |} *) `Vrn (_loc,_)
      (* {| $vrn:_ |} *)
    | {| $str:_ |} | {| $_ .. $_ |} |
      {| $flo:_ |} | {| $nativeint:_ |} | {| $int64:_ |} |
      {| $int32:_ |} | {| $int:_ |} | {| $chr:_ |} |
      {| #$_ |} | {| [| $_ |] |} | {| $anti:_ |} -> false
          (* add here ModuleUnpack *)
    ];      
      
  





(*
  mk_array [| {| 1 |} ; {| 2 |} ; {| 3 |} |] |> e2s = ({| [|1;2;3|] |} |> e2s);
  True
 *)
let array_of_array arr =
  match arr  with
  [[||] -> `ArrayEmpty (FanLoc.ghost)
  | _ ->   
      let items = arr |> Array.to_list |> sem_of_list in
      let _loc = loc_of items in
      `Array(_loc,items)];
  
let meta_array mf_a _loc ls =
  array_of_array (Array.map (fun x -> mf_a _loc x) ls)  ;


(*
  Examples:
  {[
  bigarray_get _loc {|a|} {|(b,c,d)|} |> FanBasic.p_expr f;
  ]}
 *)  
let bigarray_get loc arr arg = with expr
  let coords =
    match arg with
    [ {| ($e1, $e2) |} | {| $e1, $e2 |} ->
      list_of_com' e1 (list_of_com' e2 [])
    | _ -> [arg] ] in
  match coords with
  [ [] -> failwith "bigarray_get null list"
  | [c1] -> {@loc| $arr.{$c1} |}  
  | [c1; c2] -> {@loc| $arr.{$c1,$c2} |}  
  | [c1; c2; c3] -> {@loc| $arr.{$c1,$c2,$c3} |} 
  | [c1;c2;c3::coords] ->
      {@loc| $arr.{$c1,$c2,$c3,$(sem_of_list coords) } |} ];


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
let bigarray_set loc var newval = with expr
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
  


    
(* Add a sequence delimiter to the semi delimiter
   antiquot is also decorated *)    
let mksequence ?loc  =
  function
  [ `Sem (_loc,_,_)|`Ant (_loc,_) as e ->
      let _loc = match loc with [ Some x -> x | None  -> _loc] in
      `Seq (_loc, e)
  | e -> e ];  

(* see [mksequence], antiquot is not decoreated *)    
let mksequence' ?loc  =
  function
  [ `Sem (_loc,_,_) as e ->
      let _loc =
        match loc with [ Some x -> x | None  -> _loc] in
      `Seq (_loc, e)
  | e -> e];


        
let rec to_lid =
  function
  [ `Dot (_loc,_,i) -> to_lid i
  | `Lid (_loc,lid) -> lid
  | _ -> assert false ];



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
let mkumin loc prefix arg = with expr 
  match arg with
  [ {| $int:n |} -> {@loc| $(int:String.neg n) |}
  | {| $int32:n |} -> {@loc| $(int32:String.neg n) |}
  | {| $int64:n |} -> {@loc| $(int64:String.neg n) |}
  | {| $nativeint:n |} -> {@loc| $(nativeint:String.neg n) |}
  | {| $flo:n |} -> {@loc| $(flo:String.neg n) |}
  | _ -> {@loc| $(lid:"~" ^ prefix) $arg |} ];

      
let mkassert loc =  with expr fun
  [ {| false |} -> {@loc| assert false |} 
  | e -> {@loc| assert $e |} ] ;      


(* here -> can not be used as a delimiter, if we remove quotations.*)  
let rec to_generalized x = with ctyp
  match x with
  [ {| $t1 -> $t2 |} ->
    let (tl, rt) = to_generalized t2 in
    ([t1 :: tl], rt)
  | t -> ([], t) ];
  

  
