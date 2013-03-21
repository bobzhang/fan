

(* never used before *)    
(* module MetaLocVar : FanAst.META_LOC= struct *)
(*   let meta_loc _loc _ = {:patt| $(lid:!FanLoc.name) |}; *)
(*   (\* let meta_loc_patt _loc _ = {:patt| $(lid:!FanLoc.name) |}; *\) *)
(*   (\* let meta_loc_exp _loc _ = {:exp| $(lid:!FanLoc.name) |}; *\) *)
(* end; *)
    
(* module MetaLoc *)

(* module MetaGhostLoc *)

(* module MetaLoc : FanAst.META_LOC= struct *)
(*   (\* FIXME *\) *)
(*   (\* let meta_loc_patt _loc _location = *\) *)
(*   (\*   failwith  "MetaLoc.meta_loc_patt not implemented yet"  ; *\) *)
(*     (\* let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in *\) *)
(*     (\* {:patt| FanLoc.of_tuple *\) *)
(*     (\*   ($`str:a, $`int:b, $`int:c, $`int:d, *\) *)
(*     (\*    $`int:e, $`int:f, $`int:g, *\) *)
(*     (\*    $(if h then {:patt| true |} else {:patt| false |} )) |}; *\) *)
(*     (\* *)
(*   Ast.App *)
(*     (_loc, *)
(*       (Ast.PaId *)
(*          (_loc, *)
(*            (Ast.IdAcc *)
(*               (_loc, (Ast.Uid (_loc, "FanLoc")), *)
(*                 (Ast.Lid (_loc, "of_tuple")))))), *)
(*       (Ast.PaTup *)
(*          (_loc, *)
(*            (Ast.PaCom *)
(*               (_loc, (Ast.Str (_loc, (Ast.safe_string_escaped a))), *)
(*                 (Ast.PaCom *)
(*                    (_loc, *)
(*                      (Ast.PaCom *)
(*                         (_loc, *)
(*                           (Ast.PaCom *)
(*                              (_loc, *)
(*                                (Ast.PaCom *)
(*                                   (_loc, *)
(*                                     (Ast.PaCom *)
(*                                        (_loc, *)
(*                                          (Ast.PaCom *)
(*                                             (_loc, *)
(*                                               (Ast.Int *)
(*                                                  (_loc, (string_of_int b))), *)
(*                                               (Ast.Int *)
(*                                                  (_loc, (string_of_int c))))), *)
(*                                          (Ast.Int (_loc, (string_of_int d))))), *)
(*                                     (Ast.Int (_loc, (string_of_int e))))), *)
(*                                (Ast.Int (_loc, (string_of_int f))))), *)
(*                           (Ast.Int (_loc, (string_of_int g))))), *)
(*                      (if h *)
(*                       then Ast.PaId (_loc, (Ast.Lid (_loc, "true"))) *)
(*                       else Ast.PaId (_loc, (Ast.Lid (_loc, "false"))))))))))) *)
(*      *\) *)
(* end; *)

let meta_loc _loc location =
  let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in
  {:exp| FanLoc.of_tuple
    ($`str:a, $`int:b, $`int:c, $`int:d,
     $`int:e, $`int:f, $`int:g,
     $(if h then {:exp| true |} else {:exp| false |} )) |};
  
(* module MetaGhostLoc : FanAst.META_LOC= struct (\* MetaAction *\) *)
(*   (\* let meta_loc_patt _loc _ = *\) *)
(*   (\*   {:patt| FanLoc.ghost |}; (\\* FIXME *\\) *\) *)
(*   (\* let meta_loc_patt _loc _ = failwith "MetaGhostLoc.meta_loc_patt not implemented"; *\) *)
(*   let meta_loc(\* _exp *\) _loc _ = {:exp| FanLoc.ghost |}; *)
(* end; *)



