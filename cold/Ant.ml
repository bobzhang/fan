open Ast
open FanUtil
open Meta
open AstLoc
let meta_loc_exp _loc loc =
  match AstQuotation.current_loc_name.contents with
  | None  -> lid _loc FanLoc.name.contents
  | Some "here" -> meta_loc _loc loc
  | Some x -> lid _loc x
let meta_loc_pat _loc _ = `Any _loc
let gm () =
  match FanConfig.compilation_unit.contents with
  | Some "FanAst" -> ""
  | Some _ -> "FanAst"
  | None  -> "FanAst"
let antiquot_expander ~parse_pat  ~parse_exp  =
  object 
    inherit  Objs.map as super
    method! pat (x : pat) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = meta_loc_pat _loc _loc in
          let e = parse_pat _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Ant")), (mloc _loc))), e) : 
               Ast.pat )
           | ("uid",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Uid")), (mloc _loc))), e) : 
               Ast.pat )
           | ("lid",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Lid")), (mloc _loc))), e) : 
               Ast.pat )
           | ("par",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Par")), (mloc _loc))), e) : 
               Ast.pat )
           | ("seq",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Seq")), (mloc _loc))), e) : 
               Ast.pat )
           | ("flo",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Flo")), (mloc _loc))), e) : 
               Ast.pat )
           | ("int",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Int")), (mloc _loc))), e) : 
               Ast.pat )
           | ("int32",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Int32")), (mloc _loc))),
                    e) : Ast.pat )
           | ("int64",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Int64")), (mloc _loc))),
                    e) : Ast.pat )
           | ("nativeint",_,_) ->
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "NativeInt")), (mloc _loc))),
                    e) : Ast.pat )
           | ("chr",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Chr")), (mloc _loc))), e) : 
               Ast.pat )
           | ("str",_,_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Str")), (mloc _loc))), e) : 
               Ast.pat )
           | ("vrn","exp",_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (mloc _loc))), e) : 
               Ast.pat )
           | ("vrn","pat",_) ->
               (`App
                  (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (mloc _loc))), e) : 
               Ast.pat )
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : exp) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let mloc _loc = (meta_loc_exp _loc _loc :>exp) in
          let e = parse_exp _loc code in
          (match (decorations, cxt, sep) with
           | ("anti",_,__) ->
               (`App
                  (_loc, (`Vrn (_loc, "Ant")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("par",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Par")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("seq",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Seq")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("vrn","exp",_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Vrn")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("vrn","pat",_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Vrn")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("lid",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Lid")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("uid",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Uid")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("str",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Str")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("chr",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Chr")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("int",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Int")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("int32",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Int32")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("int64",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Int64")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("flo",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Flo")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("nativeint",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "NativeInt")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`nativeint",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Nativeint")),
                               (`Lid (_loc, "to_string")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "NativeInt")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`int",_,_) ->
               let e: Ast.exp =
                 `App (_loc, (`Id (_loc, (`Lid (_loc, "string_of_int")))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Int")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`int32",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int32")),
                               (`Lid (_loc, "to_string")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Int32")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`int64",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Int64")),
                               (`Lid (_loc, "to_string")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Int64")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`chr",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "Char")),
                               (`Lid (_loc, "escaped")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Chr")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`str",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "String")),
                               (`Lid (_loc, "escaped")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Str")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`flo",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, "FanUtil")),
                               (`Lid (_loc, "float_repres")))))), e) in
               (`App
                  (_loc, (`Vrn (_loc, "Flo")),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Ast.exp )
           | ("`bool",_,_) ->
               let x: Ast.exp =
                 `App
                   (_loc, (`Vrn (_loc, "Lid")),
                     (`Par
                        (_loc,
                          (`Com
                             (_loc, (mloc _loc),
                               (`IfThenElse
                                  (_loc, e, (`Str (_loc, "true")),
                                    (`Str (_loc, "false"))))))))) in
               (`Constraint
                  (_loc,
                    (`App
                       (_loc,
                         (`App
                            (_loc, (`Vrn (_loc, "Id")),
                              (`Id (_loc, (`Lid (_loc, "_loc")))))), x)),
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, "Ast")),
                              (`Lid (_loc, "exp"))))))) : Ast.exp )
           | ("list","module_exp",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "app_of_list")))))), e) : 
               Ast.exp )
           | ("list","module_type",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "mtApp_of_list")))))), e) : 
               Ast.exp )
           | ("list","ident",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "dot_of_list")))))), e) : 
               Ast.exp )
           | ("list",("binding"|"module_binding"|"with_constr"|"class_type"
                      |"class_exp"|"ctypand"),_)
               ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "and_of_list")))))), e) : 
               Ast.exp )
           | ("list","ctyp*",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "sta_of_list")))))), e) : 
               Ast.exp )
           | ("list","ctyp|",_)|("list","case",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "bar_of_list")))))), e) : 
               Ast.exp )
           | ("list","ctyp&",_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "amp_of_list")))))), e) : 
               Ast.exp )
           | ("listlettry","case",_) ->
               (`App
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
                                   (`Lid (_loc, "bar_of_list")))))), e))) : 
               Ast.exp )
           | ("antilettry","case",_) ->
               (`App
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
                         (`Par (_loc, (`Com (_loc, (mloc _loc), e))))))) : 
               Ast.exp )
           | ("lettry","case",_) ->
               (`App
                  (_loc,
                    (`Send
                       (_loc,
                         (`Id
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, (gm ()))),
                                   (`Lid (_loc, "match_pre")))))),
                         (`Lid (_loc, "case")))), e) : Ast.exp )
           | ("list",("ctyp,"|"pat,"|"exp,"),_) ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "com_of_list")))))), e) : 
               Ast.exp )
           | ("list",("binding;"|"stru"|"sig_item"|"class_sig_item"|"cstru"
                      |"rec_exp"|"ctyp;"|"pat;"|"exp;"),_)
               ->
               (`App
                  (_loc,
                    (`Id
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "sem_of_list")))))), e) : 
               Ast.exp )
           | _ -> super#exp e)
      | e -> super#exp e
  end