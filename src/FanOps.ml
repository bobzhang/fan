open LibUtil;

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
  
