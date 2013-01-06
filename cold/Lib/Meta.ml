module Ast = FanAst
module MetaLocVar : FanAst.META_LOC =
  struct
    let meta_loc_patt _loc _ =
      `PaId (_loc, (`Lid (_loc, (FanLoc.name.contents))))
    let meta_loc_expr _loc _ =
      `ExId (_loc, (`Lid (_loc, (FanLoc.name.contents))))
  end 
module MetaLoc : FanAst.META_LOC =
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
                  (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "of_tuple")))))),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`Str (_loc, (FanAst.safe_string_escaped a))),
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
                                                  (`Int
                                                     (_loc,
                                                       (string_of_int b))),
                                                  (`Int
                                                     (_loc,
                                                       (string_of_int c))))),
                                             (`Int (_loc, (string_of_int d))))),
                                        (`Int (_loc, (string_of_int e))))),
                                   (`Int (_loc, (string_of_int f))))),
                              (`Int (_loc, (string_of_int g))))),
                         (if h
                          then `ExId (_loc, (`Lid (_loc, "true")))
                          else `ExId (_loc, (`Lid (_loc, "false")))))))))))
  end 
module MetaGhostLoc : FanAst.META_LOC =
  struct
    let meta_loc_patt _loc _ =
      failwith "MetaGhostLoc.meta_loc_patt not implemented"
    let meta_loc_expr _loc _ =
      `ExId
        (_loc,
          (`IdAcc (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "ghost")))))
  end 