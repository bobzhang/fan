

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


