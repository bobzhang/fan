open FanUtil
open Lib.Meta
module MetaLocQuotation =
  struct
    let meta_loc_expr _loc loc =
      match AstQuotation.current_loc_name.contents with
      | None  -> `Id (_loc, (`Lid (_loc, (FanLoc.name.contents))))
      | Some "here" -> MetaLoc.meta_loc_expr _loc loc
      | Some x -> `Id (_loc, (`Lid (_loc, x)))
    let meta_loc_patt _loc _ = `Any _loc
  end
let gm () =
  match FanConfig.compilation_unit.contents with
  | Some "FanAst" -> ""
  | Some _ -> "FanAst"
  | None  -> "FanAst"
let antiquot_expander ~parse_patt  ~parse_expr  =
  object 
    inherit  FanAst.map as super
    method! patt =
      function
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in
          let e = parse_patt _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Ant")), (mloc _loc))),
                   e)
           | ("uid",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Uid")), (mloc _loc))),
                   e)
           | ("lid",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Lid")), (mloc _loc))),
                   e)
           | ("tup",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Tup")), (mloc _loc))),
                   e)
           | ("seq",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Seq")), (mloc _loc))),
                   e)
           | ("flo",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Flo")), (mloc _loc))),
                   e)
           | ("int",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Int")), (mloc _loc))),
                   e)
           | ("int32",_,_) ->
               `PaApp
                 (_loc,
                   (`PaApp (_loc, (`PaVrn (_loc, "Int32")), (mloc _loc))), e)
           | ("int64",_,_) ->
               `PaApp
                 (_loc,
                   (`PaApp (_loc, (`PaVrn (_loc, "Int64")), (mloc _loc))), e)
           | ("nativeint",_,_) ->
               `PaApp
                 (_loc,
                   (`PaApp (_loc, (`PaVrn (_loc, "NativeInt")), (mloc _loc))),
                   e)
           | ("chr",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Chr")), (mloc _loc))),
                   e)
           | ("str",_,_) ->
               `PaApp
                 (_loc, (`PaApp (_loc, (`PaVrn (_loc, "Str")), (mloc _loc))),
                   e)
           | ("vrn","expr",_) ->
               `PaApp
                 (_loc,
                   (`PaApp (_loc, (`PaVrn (_loc, "ExVrn")), (mloc _loc))), e)
           | ("vrn","patt",_) ->
               `PaApp
                 (_loc,
                   (`PaApp (_loc, (`PaVrn (_loc, "PaVrn")), (mloc _loc))), e)
           | _ -> super#patt e)
      | e -> super#patt e
    method! expr =
      function
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in
          let e = parse_expr _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,__) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Ant")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("tup",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Tup")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("seq",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Seq")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("vrn","expr",_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "ExVrn")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("vrn","patt",_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "PaVrn")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("lid",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Lid")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("uid",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Uid")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("str",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Str")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("chr",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Chr")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int32",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int32")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int64",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int64")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("flo",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "Flo")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("nativeint",_,_) ->
               `ExApp
                 (_loc, (`ExVrn (_loc, "NativeInt")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`nativeint",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Nativeint")),
                               (`Lid (_loc, "to_string")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "NativeInt")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int",_,_) ->
               let e =
                 `ExApp
                   (_loc, (`Id (_loc, (`Lid (_loc, "string_of_int")))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int32",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int32")),
                               (`Lid (_loc, "to_string")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int32")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int64",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int64")),
                               (`Lid (_loc, "to_string")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Int64")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`chr",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Char")),
                               (`Lid (_loc, "escaped")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Chr")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`str",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, (gm ()))),
                               (`Lid (_loc, "safe_string_escaped")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Str")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`flo",_,_) ->
               let e =
                 `ExApp
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "FanUtil")),
                               (`Lid (_loc, "float_repres")))))), e) in
               `ExApp
                 (_loc, (`ExVrn (_loc, "Flo")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`bool",_,_) ->
               let x =
                 `ExApp
                   (_loc, (`ExVrn (_loc, "Lid")),
                     (`Tup
                        (_loc,
                          (`Com
                             (_loc, (mloc _loc),
                               (`IfThenElse
                                  (_loc, e, (`Str (_loc, "true")),
                                    (`Str (_loc, "false"))))))))) in
               `ExApp
                 (_loc,
                   (`ExApp
                      (_loc, (`ExVrn (_loc, "Id")),
                        (`Id (_loc, (`Lid (_loc, "_loc")))))), x)
           | ("list","module_expr",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "app_of_list")))))), e)
           | ("list","module_type",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "mtApp_of_list")))))), e)
           | ("list","ident",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "dot_of_list'")))))), e)
           | ("list",("binding"|"module_binding"|"with_constr"|"class_type"
                      |"class_expr"|"ctypand"),_)
               ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "and_of_list")))))), e)
           | ("list","ctyp*",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "sta_of_list")))))), e)
           | ("list","ctyp|",_)|("list","match_case",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "or_of_list")))))), e)
           | ("list","ctyp&",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "amp_of_list")))))), e)
           | ("listlettry","match_case",_) ->
               `ExApp
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "match_case")))),
                   (`ExApp
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "or_of_list")))))), e)))
           | ("antilettry","match_case",_) ->
               `ExApp
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "match_case")))),
                   (`ExApp
                      (_loc, (`ExVrn (_loc, "Ant")),
                        (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))))
           | ("lettry","match_case",_) ->
               `ExApp
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "match_case")))), e)
           | ("list",("ctyp,"|"patt,"|"expr,"),_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "com_of_list")))))), e)
           | ("list",("binding;"|"str_item"|"sig_item"|"class_sig_item"
                      |"class_str_item"|"rec_binding"|"ctyp;"|"patt;"|"expr;"),_)
               ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "sem_of_list")))))), e)
           | ("list","forall",_) ->
               `ExApp
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "tyVarApp_of_list")))))), e)
           | _ -> super#expr e)
      | e -> super#expr e
  end