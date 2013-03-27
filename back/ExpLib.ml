

(* (\* Add a sequence delimiter to the semi delimiter *)
(*    antiquot is also decorated *)
(*  *\)   *)
(* let mksequence ?loc = fun *)
(*   [ {| $_; $_ |} *)
(*   | {| $anti:_ |} as e -> *)
(*       let _loc = *)
(*         match loc with [Some x -> x | None -> _loc] in *)
(*       {| begin  $e end |} *)
(*   | e -> e ]; *)


(* (\* see [mksequence], antiquot is not decoreated *\)   *)
(* let mksequence' ?loc = fun *)
(*   [ {| $_; $_ |} as e -> *)
(*     let _loc = match loc with *)
(*       [Some x -> x | None -> _loc] in *)
(*     {| begin  $e  end |} *)
(*   | e -> e ]; *)

  


(* let mkassert loc = fun *)
(*   [ {| false |} -> {@loc| assert false |}  *)
(*   | e -> {@loc| assert $e |} ] ; *)



(* (\* Given a [location] and [prefix](generally "-" or "-.") *)
(*    The location provided is more precise. *)
(*    since ocaml respect [(~-)] as a prefix [(-)] *)
(*    and [(~-.)] as a prefix [(-.)] *)
(*    {[ *)
(*    mkumin _loc "-." {| 3 |}; *)
(*    - : exp = Int (, "-3") *)
(*    mkumin _loc "-." {| a |}; *)
(*    - : exp = *)
(*    App (, ExId (, Lid (, "~-.")), ExId (, Lid (, "a"))) *)
(*    ]} *)
(*  *\)   *)
(* let mkumin loc prefix arg = *)
(*   match arg with *)
(*   [ {| $int:n |} -> {@loc| $(int:String.neg n) |} *)
(*   | {| $int32:n |} -> {@loc| $(int32:String.neg n) |} *)
(*   | {| $int64:n |} -> {@loc| $(int64:String.neg n) |} *)
(*   | {| $nativeint:n |} -> {@loc| $(nativeint:String.neg n) |} *)
(*   | {| $flo:n |} -> {@loc| $(flo:String.neg n) |} *)
(*   | _ -> {@loc| $(lid:"~" ^ prefix) $arg |} ]; *)

    

(* let mk_assert  =  fun *)
(*   [ {| false |} ->    {| assert false |}  *)
(*   | e -> {| assert $e |} ]; *)


  (* {|`App _loc $x $y |} *)
  (*
      `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "App")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
   *)
  (* {| `App(_loc, $x, $y) |}; *)

(* let vee_app x y = {| `App (_loc,$x,$y) |}; *)

  (*
      `App
    (_loc, (`Vrn (_loc, "App")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
   *)
(* let mep_app x y = {| {:pat| $($x) $($y) |}|}; *)
  (* {| `App (_loc, $x, $y) |};        *)
(* let vep_app x y = {| `App (_loc,$x,$y)|}; *)
  

(*
   
  Example:
  {[
  mep_of_str "B" = {|{:pat| B |}|};
  - : bool = true
  ]}
  FIXME
  {|{:pat|`B|}|}
  {|{:pat|B|}|}
 *)  

(* let mep_of_str  s = *)
(*   let len = String.length s in *)
(*   if s.[0] = '`' then *)
(*     let s = String.sub s 1 (len - 1 ) in *)
(*     (\* {| {:pat|`$($str:s)|}|} *\) *)
(*       {| {:pat|$(vrn:($str:s))|}|} *)
(*   else *)
(*    let u = {| {:ident| $(uid:$str:s) |} |} in  *)
(*    {| {:pat| $(id:$u) |} |}; *)
(*     (\* let u = {| Ast.Uid _loc $str:s |} in *\) *)
(*   (\* {| Ast.PaId _loc $u |}; *\) *)
