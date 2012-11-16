
module Ast = Camlp4Ast; (* FIXME: rename it into Camlp4Ast later *)

(* never used before *)    
module MetaLocVar : Ast.META_LOC= struct
  let meta_loc_patt _loc _ = {:patt| $(lid:!FanLoc.name) |};
  let meta_loc_expr _loc _ = {:expr| $(lid:!FanLoc.name) |};
end;
    
(* module MetaLoc *)

(* module MetaGhostLoc *)

module MetaLoc : Ast.META_LOC= struct
  (* FIXME *)
  let meta_loc_patt _loc _location =
    failwith  "MetaLoc.meta_loc_patt not implemented yet"  ;
    (* let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in *)
    (* {:patt| FanLoc.of_tuple *)
    (*   ($`str:a, $`int:b, $`int:c, $`int:d, *)
    (*    $`int:e, $`int:f, $`int:g, *)
    (*    $(if h then {:patt| true |} else {:patt| false |} )) |}; *)
    (*
  Ast.PaApp
    (_loc,
      (Ast.PaId
         (_loc,
           (Ast.IdAcc
              (_loc, (Ast.IdUid (_loc, "FanLoc")),
                (Ast.IdLid (_loc, "of_tuple")))))),
      (Ast.PaTup
         (_loc,
           (Ast.PaCom
              (_loc, (Ast.PaStr (_loc, (Ast.safe_string_escaped a))),
                (Ast.PaCom
                   (_loc,
                     (Ast.PaCom
                        (_loc,
                          (Ast.PaCom
                             (_loc,
                               (Ast.PaCom
                                  (_loc,
                                    (Ast.PaCom
                                       (_loc,
                                         (Ast.PaCom
                                            (_loc,
                                              (Ast.PaInt
                                                 (_loc, (string_of_int b))),
                                              (Ast.PaInt
                                                 (_loc, (string_of_int c))))),
                                         (Ast.PaInt (_loc, (string_of_int d))))),
                                    (Ast.PaInt (_loc, (string_of_int e))))),
                               (Ast.PaInt (_loc, (string_of_int f))))),
                          (Ast.PaInt (_loc, (string_of_int g))))),
                     (if h
                      then Ast.PaId (_loc, (Ast.IdLid (_loc, "true")))
                      else Ast.PaId (_loc, (Ast.IdLid (_loc, "false")))))))))))
     *)
  let meta_loc_expr _loc location =
    let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in
    {:expr| FanLoc.of_tuple
      ($`str:a, $`int:b, $`int:c, $`int:d,
       $`int:e, $`int:f, $`int:g,
       $(if h then {:expr| true |} else {:expr| false |} )) |};
end;
  
module MetaGhostLoc : Ast.META_LOC= struct (* MetaAction *)
  (* let meta_loc_patt _loc _ = *)
  (*   {:patt| FanLoc.ghost |}; (\* FIXME *\) *)
  let meta_loc_patt _loc _ = failwith "MetaGhostLoc.meta_loc_patt not implemented";
  let meta_loc_expr _loc _ = {:expr| FanLoc.ghost |};
end;


module MetaLocQuotation = struct
  let loc_name = ref None;
  let meta_loc_expr _loc loc =
    match !loc_name with
    [ None -> {:expr| $(lid:!FanLoc.name) |}
    | Some "here" -> MetaLoc.meta_loc_expr _loc loc
    | Some x -> {:expr| $lid:x |} ];
   (* FIXME track the location of the quotation
      read the list for the detailed usage
    *)   
  let meta_loc_patt _loc _ = {:patt| _ |};
end;

module MetaQAst = Ast.Meta.Make MetaLocQuotation;
module ME = MetaQAst.Expr;
module MP = MetaQAst.Patt;

