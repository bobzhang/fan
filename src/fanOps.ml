#{:control| default "exp'" ;|}

open LibUtil
open FAst
open AstLib


(* +-----------------------------------------------------------------+
   | Common functions for FAst processing                             |
   +-----------------------------------------------------------------+ *)




(*
  Given an location, and a list of expession node,
  return an expession node which represents the list
  of the expesson nodes

  Example:
  {[
  mklist _loc [{|b|}; {|c|}; {|d|}] |> Ast2pt.print_exp f;
  [b; c; d]
  ]}
  (* {:exp| [1;2;3::[]]|} *)
  DoubleColon
 *)
let list_of_list (loc:loc) =
  let rec loop top =  with exp' function
    | [] ->   let ghost = FLoc.ghost in {@ghost| [] |}
    | e1 :: el ->
        let _loc =
          if top then loc else FLoc.merge (loc_of e1) loc in
        {| $e1 :: $(loop false el) |} (* FIXME *)  in
  loop true ;;

(* FIXME  double semi colon needed before *)  


let meta_int _loc i =  {|$`int:i|}

let meta_int32 _loc i =  {|$`int32:i|}

let meta_int64 _loc i =  {|$`int64:i|}
  
let meta_nativeint _loc i =  {|$`nativeint:i|}
  
let meta_float _loc i = {|$`flo:i|}
  
let meta_string _loc i = {|$`str:i|}
  
let meta_char _loc i = {|$`chr:i|}
let meta_unit _loc _ =  {|()|}
let meta_bool _loc =  function | true -> {|true|} | false -> {|false|} 

let meta_ref mf_a _loc i =
  {| {contents= $(mf_a _loc !i) } |}


  
(* [mklist] and [mkarray]
   duplicated with ExprPatt to remove cyclic dependency *)
let mklist loc =
  let rec loop top =  function
    | [] -> {@loc| [] |}
    | e1 :: el ->
        let _loc =
          if top then loc else FLoc.merge (loc_of (e1)) loc in
        `App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), (loop false el))
          (* {| [$e1 :: $(loop false el)] |} *)
  in loop true 

let meta_list mf_a _loc  ls =
  mklist _loc (List.map (fun x -> mf_a _loc x ) ls ) 
  
  
let meta_option mf_a _loc  = function
  | None -> {|None|}
  | Some x -> {|Some $(mf_a _loc x)|} 

let meta_arrow (type t)
    (_mf_a: FLoc.t -> 'a -> t)
    (_mf_b: FLoc.t -> 'b ->t)
    (_loc: FLoc.t)  (_x:'a -> 'b) = invalid_arg "meta_arrow not implemented"
    

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
  |`Dot(_,_,i) -> is_module_longident i
  |`Apply(_,i1,i2) -> is_module_longident i1 && is_module_longident i2
  | `Uid _ -> true
  | _ -> false 

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
let ident_of_ctyp : ctyp -> ident =
  let error x =
    invalid_argf  "ident_of_ctyp: this type %s is not an identifier"  (Objs.dump_ctyp x) in
  let rec self  (x:ctyp) =
    match x with 
    | `Apply(_loc,t1,t2) -> `Apply(_loc,self (t1:>ctyp), self (t2:>ctyp))
    | `Lid _  -> error x
    | #ident' as i -> if is_module_longident i then i else error x
    | _ -> error x  in
  function
    | #ident as i  -> i (* allow antiquot here *)
    | t -> self t 

(* let ident_of_pat = *)
(*   let error () = *)
(*     invalid_arg "ident_of_pat: this pattern is not an identifier" in *)
(*   let rec self = fun  *)
(*     [ {:pat@_loc| $p1 $p2 |} *)
(*       -> {:ident| ( $(self p1) $(self p2) ) |} *)
(*     | {:pat| $lid:_ |} -> error () *)
(*     | {:pat| $id:i |} -> if is_module_longident i then i else error () *)
(*     | _ -> error () ] in *)
(*     fun *)
(*     [ {:pat| $id:i |} -> i *)
(*     | p -> self p ];; *)



(* let rec is_constructor =  with ident fun *)
(*     [ {| $_.$i |} -> is_constructor i *)
(*     | {| $uid:_ |} -> true *)
(*     | {| $lid:_ |} | {| ($_ $_) |} -> false *)
(*     | {| $anti:_ |} -> assert false ]; *)

(* let is_pat_constructor = fun *)
(*     [ `Id(_,i) -> is_constructor i *)
(*     | `Vrn (_loc,_) *)
(*       -> true *)
(*     | _ -> false ]; *)

(* let rec is_exp_constructor = fun *)
(*     [ `Id(_,i) -> is_constructor i *)
(*     | `Dot(_,e1,e2) -> is_exp_constructor e1 && is_exp_constructor e2 *)
(*     | `Vrn(_loc,_) -> true *)
(*     | _ -> false ]; *)




(* let ty_of_stl = fun *)
(*     [ (_loc, s, []) -> *)
(*       `Id(_loc,`Uid(_loc,s)) *)
(*       (\* {:ctyp| $uid:s |} *\) *)
(*     | (_loc, s, tl) -> *)
(*         `Of (_loc, `Id (_loc, `Uid (_loc, s)), and_of_list tl)]; *)
          (* {:ctyp| $uid:s of $(and_of_list tl) |} *) 

(* let ty_of_sbt = fun *)
(*     [ (_loc, s, v, t) -> *)
(*       if v then *)
(*         `TyColMut (_loc, `Id (_loc, `Lid (_loc, s)), t) *)
(*       else *)
(*         `TyCol (_loc, `Id (_loc, `Lid (_loc, s)), t)]; *)

(* let bi_of_pe (p, e) = let _loc = loc_of p in {:bind| $p = $e |}; *)

(* let sum_type_of_list l = bar_of_list (List.map ty_of_stl l); *)

(* let record_type_of_list l = sem_of_list (List.map ty_of_sbt l); *)

(* let bind_of_pel l = and_of_list (List.map bi_of_pe l); *)

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


          

          


let rec is_irrefut_pat (x: pat) = with pat
    match x with
    | `Lid _ ->  true 
    | `ArrayEmpty (_loc)
    | `LabelS (_loc,_)
    | {| () |} -> true
    | {| _ |} -> true
    | `Dot(_,_,y) -> is_irrefut_pat (y:vid:>pat) 
    | {| ($x as $_) |} -> is_irrefut_pat x
    | {| { $p } |} ->
        List.for_all (function |`RecBind (_,_,p) -> is_irrefut_pat p | _ -> true)
          (list_of_sem  p [])
    | `Sem(_,p1,p2) -> is_irrefut_pat p1 && is_irrefut_pat p2
    | `Com(_,p1,p2) -> is_irrefut_pat p1 && is_irrefut_pat p2
    | `Bar(_,p1,p2) -> is_irrefut_pat p1 && is_irrefut_pat p2 (* could be more fine grained *)
    | `App(_,p1,p2) -> is_irrefut_pat p1 && is_irrefut_pat p2
          
    | `Constraint(_,p,_) -> is_irrefut_pat p
    | `Par(_,p) -> is_irrefut_pat p
    | `OptLablS _ -> true
    | `OptLabl(_,_,p) | `OptLablExpr(_,_,p,_) -> is_irrefut_pat p
    | `Label(_,_,p) | `Lazy (_,p) ->  is_irrefut_pat p
    | `Uid _ -> false
    | `ModuleUnpack _ 
    | `ModuleConstraint _  -> true
    (* | {| (module $_ : $opt:_ ) |} -> true *)
    | `Ant _ -> false 
    | (* {| `$_ |} *) `Vrn (_loc,_)
      (* {| $vrn:_ |} *)
    | {| $str:_ |} | {| $_ .. $_ |} |
      {| $flo:_ |} | {| $nativeint:_ |} | {| $int64:_ |} |
      {| $int32:_ |} | {| $int:_ |} | {| $chr:_ |} |
      {| #$_ |} | {| [| $_ |] |}  -> false
          (* add here ModuleUnpack *)

      
  





(*
  mk_array [| {| 1 |} ; {| 2 |} ; {| 3 |} |] |> e2s = ({| [|1;2;3|] |} |> e2s);
  True
 *)
let array_of_array arr =
  match arr  with
  | [||] -> `ArrayEmpty (FLoc.ghost)
  | _ ->   
      let items = arr |> Array.to_list |> sem_of_list in
      let _loc = loc_of items in
      `Array(_loc,items)
  
let meta_array mf_a _loc ls =
  array_of_array (Array.map (fun x -> mf_a _loc x) ls)  


(*
  Examples:
  {[
  bigarray_get _loc {|a|} {|(b,c,d)|} |> Ast2pt.print_exp f;
  ]}
 *)  
let bigarray_get loc arr (arg (* :exp  *))  (* : exp  *)= with exp
  let coords =
    match arg with
    | {| ($e1, $e2) |} | {| $e1, $e2 |} ->
      list_of_com e1 (list_of_com e2 [])
    | _ -> [arg]  in
  match coords with
  | [] -> failwith "bigarray_get null list"
  | [c1] -> {@loc| $arr.{$c1} |}  
  | [c1; c2] -> {@loc| $arr.{$c1,$c2} |}  
  | [c1; c2; c3] -> {@loc| $arr.{$c1,$c2,$c3} |} 
  | c1::c2::c3::coords ->
      {@loc| $arr.{$c1,$c2,$c3,$(sem_of_list coords) } |} 


(*
  Example:
  {[
  bigarray_set _loc {|a.{b,c,d}|} {|3+2|} |> Option.get |> Ast2pt.print_exp f;
  a.{b,c,d} <- (3 + 2)
  ]}
  FIXME
    1.ExArr, 2. can we just write $list:coords?
    Technically, we cannot, it uses [Pexp_array], pattern match doesnot work here
     {:exp|a.{1,2,3,4,$rest:x}|}
 *)
let bigarray_set loc (var) newval (* : option exp *) = with exp
  match var with
  | {|  $arr.{$c1} |} ->
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
  | _ -> None 
  


    
(* Add a sequence delimiter to the semi delimiter
   antiquot is also decorated *)    
let mksequence ?loc  = function
  | `Sem (_loc,_,_)|`Ant (_loc,_) as e ->
      let _loc =
        match loc with
        | Some x -> x | None  -> _loc in
      `Seq (_loc, e)
  | e -> e 

(* see [mksequence], antiquot is not decoreated *)    
let mksequence' ?loc  = function
  | `Sem (_loc,_,_) as e ->
      let _loc =
        match loc with
        |Some x -> x
        | None  -> _loc in
      `Seq (_loc, e)
  | e -> e


        
let rec to_lid = function
  | `Dot (_loc,_,i) -> to_lid i
  | `Lid (_loc,lid) -> lid
  | _ -> assert false 


(* Given a [location] and [prefix](generally "-" or "-.")
   The location provided is more precise.
   since ocaml respect [(~-)] as a prefix [(-)]
   and [(~-.)] as a prefix [(-.)]
   {[
   mkumin _loc "-." {| 3 |};
   - : exp = Int (, "-3")
   mkumin _loc "-." {| a |};
   - : exp =
   App (, ExId (, Lid (, "~-.")), ExId (, Lid (, "a")))
   ]}
 *)  
let mkumin loc prefix arg = with exp 
  match arg with
  | {| $int:n |} -> {@loc| $(int:String.neg n) |}
  | {| $int32:n |} -> {@loc| $(int32:String.neg n) |}
  | {| $int64:n |} -> {@loc| $(int64:String.neg n) |}
  | {| $nativeint:n |} -> {@loc| $(nativeint:String.neg n) |}
  | {| $flo:n |} -> {@loc| $(flo:String.neg n) |}
  | _ -> {@loc| $(lid:"~" ^ prefix) $arg |}

(* {:exp|assert l|}*)      
(* let mkassert loc =  with exp fun *)
(*   [ {| false |} -> {@loc| assert false |}  *)
(*   | e -> {@loc| assert $e |} ] ;       *)


(* here -> can not be used as a delimiter, if we remove quotations.*)  
(* let rec to_generalized x = with ctyp' *)
(*   match x with *)
(*   [ {| $t1 -> $t2 |} -> *)
(*     let (tl, rt) = to_generalized t2 in *)
(*     ([t1 :: tl], rt) *)
(*   | t -> ([], t) ]; *)
  

  
