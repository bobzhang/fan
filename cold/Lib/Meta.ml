(* open Ast *)
module Ast = FanAst
module MetaLocVar : Ast.META_LOC =
  struct
  let meta_loc_patt _loc _ =
    `PaId (_loc, (`IdLid (_loc, (FanLoc.name.contents))))
  let meta_loc_expr _loc _ =
    `ExId (_loc, (`IdLid (_loc, (FanLoc.name.contents))))
  end 
module MetaLoc : Ast.META_LOC =
  struct
  let meta_loc_patt _loc _location =
    failwith "MetaLoc.meta_loc_patt not implemented yet"
  let meta_loc_expr _loc location =
    let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple location in
    `ExApp
      (_loc,
        (`ExId
           (_loc,
             (`IdAcc
                (_loc, (`IdUid (_loc, "FanLoc")), (`IdLid (_loc, "of_tuple")))))),
        (`ExTup
           (_loc,
             (`ExCom
                (_loc, (`ExStr (_loc, (Ast.safe_string_escaped a))),
                  (`ExCom
                     (_loc,
                       (`ExCom
                          (_loc,
                            (`ExCom
                               (_loc,
                                 (`ExCom
                                    (_loc,
                                      (`ExCom
                                         (_loc,
                                           (`ExCom
                                              (_loc,
                                                (`ExInt
                                                   (_loc, (string_of_int b))),
                                                (`ExInt
                                                   (_loc, (string_of_int c))))),
                                           (`ExInt (_loc, (string_of_int d))))),
                                      (`ExInt (_loc, (string_of_int e))))),
                                 (`ExInt (_loc, (string_of_int f))))),
                            (`ExInt (_loc, (string_of_int g))))),
                       (if h
                        then `ExId (_loc, (`IdLid (_loc, "true")))
                        else `ExId (_loc, (`IdLid (_loc, "false")))))))))))
  end 
module MetaGhostLoc : Ast.META_LOC =
  struct
  let meta_loc_patt _loc _ =
    failwith "MetaGhostLoc.meta_loc_patt not implemented"
  let meta_loc_expr _loc _ =
    `ExId
      (_loc,
        (`IdAcc (_loc, (`IdUid (_loc, "FanLoc")), (`IdLid (_loc, "ghost")))))
  end 
