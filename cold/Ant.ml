open Ast
open FanUtil
open Lib.Meta
let meta_loc_exp _loc loc =
  match AstQuotation.current_loc_name.contents with
  | None  -> `Id (_loc, (`Lid (_loc, (FanLoc.name.contents))))
  | Some "here" -> meta_loc _loc loc
  | Some x -> `Id (_loc, (`Lid (_loc, x)))
let meta_loc_patt _loc _ = `Any _loc
let gm () =
  match FanConfig.compilation_unit.contents with
  | Some "FanAst" -> ""
  | Some _ -> "FanAst"
  | None  -> "FanAst"
let antiquot_expander ~parse_patt  ~parse_exp  =
  object 
    inherit  Objs.map as super
    method! patt (x : patt) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = meta_loc_patt _loc _loc in
          let e = parse_patt _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Ant")), (mloc _loc))), e)
           | ("uid",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Uid")), (mloc _loc))), e)
           | ("lid",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Lid")), (mloc _loc))), e)
           | ("tup",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (mloc _loc))), e)
           | ("seq",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Seq")), (mloc _loc))), e)
           | ("flo",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Flo")), (mloc _loc))), e)
           | ("int",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Int")), (mloc _loc))), e)
           | ("int32",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Int32")), (mloc _loc))),
                   e)
           | ("int64",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Int64")), (mloc _loc))),
                   e)
           | ("nativeint",_,_) ->
               `App
                 (_loc,
                   (`App (_loc, (`Vrn (_loc, "NativeInt")), (mloc _loc))), e)
           | ("chr",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Chr")), (mloc _loc))), e)
           | ("str",_,_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Str")), (mloc _loc))), e)
           | ("vrn","exp",_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (mloc _loc))), e)
           | ("vrn","patt",_) ->
               `App
                 (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (mloc _loc))), e)
           | _ -> super#patt e)
      | e -> super#patt e
    method! exp (x : exp) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = (meta_loc_exp _loc _loc :>exp) in
          let e = parse_exp _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,__) ->
               `App
                 (_loc, (`Vrn (_loc, "Ant")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("tup",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Tup")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("seq",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Seq")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("vrn","exp",_) ->
               `App
                 (_loc, (`Vrn (_loc, "Vrn")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("vrn","patt",_) ->
               `App
                 (_loc, (`Vrn (_loc, "Vrn")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("lid",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Lid")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("uid",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Uid")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("str",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Str")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("chr",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Chr")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Int")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int32",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Int32")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("int64",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Int64")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("flo",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "Flo")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("nativeint",_,_) ->
               `App
                 (_loc, (`Vrn (_loc, "NativeInt")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`nativeint",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Nativeint")),
                               (`Lid (_loc, "to_string")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "NativeInt")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int",_,_) ->
               let e =
                 `App (_loc, (`Id (_loc, (`Lid (_loc, "string_of_int")))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Int")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int32",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int32")),
                               (`Lid (_loc, "to_string")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Int32")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`int64",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int64")),
                               (`Lid (_loc, "to_string")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Int64")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`chr",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Char")),
                               (`Lid (_loc, "escaped")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Chr")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`str",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "String")),
                               (`Lid (_loc, "escaped")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Str")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`flo",_,_) ->
               let e =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "FanUtil")),
                               (`Lid (_loc, "float_repres")))))), e) in
               `App
                 (_loc, (`Vrn (_loc, "Flo")),
                   (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))
           | ("`bool",_,_) ->
               let x =
                 `App
                   (_loc, (`Vrn (_loc, "Lid")),
                     (`Tup
                        (_loc,
                          (`Com
                             (_loc, (mloc _loc),
                               (`IfThenElse
                                  (_loc, e, (`Str (_loc, "true")),
                                    (`Str (_loc, "false"))))))))) in
               `App
                 (_loc,
                   (`App
                      (_loc, (`Vrn (_loc, "Id")),
                        (`Id (_loc, (`Lid (_loc, "_loc")))))), x)
           | ("list","module_exp",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "app_of_list")))))), e)
           | ("list","module_type",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "mtApp_of_list")))))), e)
           | ("list","ident",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "dot_of_list")))))), e)
           | ("list",("binding"|"module_binding"|"with_constr"|"class_type"
                      |"class_exp"|"ctypand"),_)
               ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "and_of_list")))))), e)
           | ("list","ctyp*",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "sta_of_list")))))), e)
           | ("list","ctyp|",_)|("list","case",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "or_of_list")))))), e)
           | ("list","ctyp&",_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "amp_of_list")))))), e)
           | ("listlettry","case",_) ->
               `App
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "case")))),
                   (`App
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "or_of_list")))))), e)))
           | ("antilettry","case",_) ->
               `App
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "case")))),
                   (`App
                      (_loc, (`Vrn (_loc, "Ant")),
                        (`Tup (_loc, (`Com (_loc, (mloc _loc), e)))))))
           | ("lettry","case",_) ->
               `App
                 (_loc,
                   (`Send
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, (gm ()))),
                                  (`Lid (_loc, "match_pre")))))),
                        (`Lid (_loc, "case")))), e)
           | ("list",("ctyp,"|"patt,"|"exp,"),_) ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "com_of_list")))))), e)
           | ("list",("binding;"|"stru"|"sig_item"|"class_sig_item"|"cstru"
                      |"rec_exp"|"ctyp;"|"patt;"|"exp;"),_)
               ->
               `App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Lid (_loc, "sem_of_list")))))), e)
           | _ -> super#exp e)
      | e -> super#exp e
  end
