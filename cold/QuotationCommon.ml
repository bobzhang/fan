open FanSig

module Id =
              struct
               let name = "Camlp4QuotationCommon"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     functor (TheAntiquotSyntax : Camlp4.Sig.Parser(Syntax.Ast).SIMPLE) ->
                      struct
                       open Camlp4.Sig

                       include Syntax

                       module MetaLocHere = Ast.Meta.MetaLoc

                       module MetaLoc =
                        struct
                         module Ast = Ast

                         let loc_name = (ref None )

                         let meta_loc_expr =
                          fun _loc ->
                           fun loc ->
                            (match !loc_name with
                             | None ->
                                (Ast.ExId
                                  (_loc, ( (Ast.IdLid (_loc, ( !Loc.name )))
                                   )))
                             | Some ("here") ->
                                (MetaLocHere.meta_loc_expr _loc loc)
                             | Some (x) ->
                                (Ast.ExId (_loc, ( (Ast.IdLid (_loc, x)) ))))

                         let meta_loc_patt =
                          fun _loc -> fun _ -> (Ast.PaAny (_loc))

                        end

                       module MetaAst = (Ast.Meta.Make)(MetaLoc)

                       module ME = MetaAst.Expr

                       module MP = MetaAst.Patt

                       let is_antiquot =
                        fun s ->
                         let len = (String.length s) in
                         (( (len > 2) ) && (
                           (( (( (String.get s 0) ) = '\\') ) && (
                             (( (String.get s 1) ) = '$') )) ))

                       let handle_antiquot_in_string =
                        fun s ->
                         fun term ->
                          fun parse ->
                           fun loc ->
                            fun decorate ->
                             if (is_antiquot s) then
                              (
                              let pos = (String.index s ':') in
                              let name = (String.sub s 2 ( (pos - 2) ))
                              and code =
                               (String.sub s ( (pos + 1) ) (
                                 (( (( (String.length s) ) - pos) ) - 1) )) in
                              (decorate name ( (parse loc code) ))
                              )
                             else term

                       let antiquot_expander =
                        object
                         inherit Ast.map as super
                         method patt =
                          function
                          | ((Ast.PaAnt (_loc, s) | Ast.PaStr (_loc, s)) as
                             p) ->
                             let mloc =
                              fun _loc -> (MetaLoc.meta_loc_patt _loc _loc) in
                             (handle_antiquot_in_string s p
                               TheAntiquotSyntax.parse_patt _loc (
                               fun n ->
                                fun p ->
                                 (match n with
                                  | "antisig_item" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "SgAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antistr_item" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "StAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antictyp" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "TyAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antipatt" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "PaAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antiexpr" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "ExAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antimodule_type" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MtAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antimodule_expr" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MeAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "anticlass_type" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CtAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "anticlass_expr" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CeAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "anticlass_sig_item" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CgAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "anticlass_str_item" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CrAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antiwith_constr" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "WcAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antibinding" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "BiAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antirec_binding" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "RbAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antimatch_case" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "McAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antimodule_binding" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MbAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | "antiident" ->
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "IdAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), p))
                                  | _ -> p) ))
                          | p -> (super#patt p)
                         method expr =
                          function
                          | ((Ast.ExAnt (_loc, s) | Ast.ExStr (_loc, s)) as
                             e) ->
                             let mloc =
                              fun _loc -> (MetaLoc.meta_loc_expr _loc _loc) in
                             (handle_antiquot_in_string s e
                               TheAntiquotSyntax.parse_expr _loc (
                               fun n ->
                                fun e ->
                                 (match n with
                                  | "`int" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdLid (_loc, "string_of_int"))
                                           ))) ), e))
                                  | "`int32" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Int32")) ),
                                              (
                                              (Ast.IdLid (_loc, "to_string"))
                                              ))) ))) ), e))
                                  | "`int64" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Int64")) ),
                                              (
                                              (Ast.IdLid (_loc, "to_string"))
                                              ))) ))) ), e))
                                  | "`nativeint" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Nativeint"))
                                              ), (
                                              (Ast.IdLid (_loc, "to_string"))
                                              ))) ))) ), e))
                                  | "`flo" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "FanUtil"))
                                              ), (
                                              (Ast.IdLid
                                                (_loc, "float_repres")) )))
                                           ))) ), e))
                                  | "`str" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "safe_string_escaped"))
                                              ))) ))) ), e))
                                  | "`chr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Char")) ), (
                                              (Ast.IdLid (_loc, "escaped"))
                                              ))) ))) ), e))
                                  | "`bool" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "IdUid"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), (
                                        (Ast.ExIfe
                                          (_loc, e, (
                                           (Ast.ExStr (_loc, "True")) ), (
                                           (Ast.ExStr (_loc, "False")) ))) )))
                                  | "liststr_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "stSem_of_list")) )))
                                           ))) ), e))
                                  | "listsig_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "sgSem_of_list")) )))
                                           ))) ), e))
                                  | "listclass_sig_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "cgSem_of_list")) )))
                                           ))) ), e))
                                  | "listclass_str_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "crSem_of_list")) )))
                                           ))) ), e))
                                  | "listmodule_expr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "meApp_of_list")) )))
                                           ))) ), e))
                                  | "listmodule_type" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "mtApp_of_list")) )))
                                           ))) ), e))
                                  | "listmodule_binding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "mbAnd_of_list")) )))
                                           ))) ), e))
                                  | "listbinding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "biAnd_of_list")) )))
                                           ))) ), e))
                                  | "listbinding;" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "biSem_of_list")) )))
                                           ))) ), e))
                                  | "listrec_binding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "rbSem_of_list")) )))
                                           ))) ), e))
                                  | "listclass_type" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "ctAnd_of_list")) )))
                                           ))) ), e))
                                  | "listclass_expr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "ceAnd_of_list")) )))
                                           ))) ), e))
                                  | "listident" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "idAcc_of_list")) )))
                                           ))) ), e))
                                  | "listctypand" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tyAnd_of_list")) )))
                                           ))) ), e))
                                  | "listctyp;" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tySem_of_list")) )))
                                           ))) ), e))
                                  | "listctyp*" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tySta_of_list")) )))
                                           ))) ), e))
                                  | "listctyp|" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tyOr_of_list")) )))
                                           ))) ), e))
                                  | "listctyp," ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tyCom_of_list")) )))
                                           ))) ), e))
                                  | "listctyp&" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "tyAmp_of_list")) )))
                                           ))) ), e))
                                  | "listwith_constr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "wcAnd_of_list")) )))
                                           ))) ), e))
                                  | "listmatch_case" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "mcOr_of_list")) )))
                                           ))) ), e))
                                  | "listpatt," ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "paCom_of_list")) )))
                                           ))) ), e))
                                  | "listpatt;" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "paSem_of_list")) )))
                                           ))) ), e))
                                  | "listexpr," ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "exCom_of_list")) )))
                                           ))) ), e))
                                  | "listexpr;" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Ast")) ), (
                                              (Ast.IdLid
                                                (_loc, "exSem_of_list")) )))
                                           ))) ), e))
                                  | "antisig_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "SgAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antistr_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "StAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antictyp" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "TyAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antipatt" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "PaAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antiexpr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "ExAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antimodule_type" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MtAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antimodule_expr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MeAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "anticlass_type" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CtAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "anticlass_expr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CeAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "anticlass_sig_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CgAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "anticlass_str_item" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "CrAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antiwith_constr" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "WcAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antibinding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "BiAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antirec_binding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "RbAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antimatch_case" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "McAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antimodule_binding" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "MbAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | "antiident" ->
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 (
                                                 (Ast.IdUid (_loc, "IdAnt"))
                                                 ))) ))) ), ( (mloc _loc) )))
                                        ), e))
                                  | _ -> e) ))
                          | e -> (super#expr e)
                        end

                       let add_quotation =
                        fun name ->
                         fun entry ->
                          fun mexpr ->
                           fun mpatt ->
                            let entry_eoi =
                             (Gram.Entry.mk ( (Gram.Entry.name entry) )) in
                            let parse_quot_string =
                             fun entry ->
                              fun loc ->
                               fun s ->
                                let q = !FanConfig.antiquotations in
                                let () = (FanConfig.antiquotations := true ) in
                                let res = (Gram.parse_string entry loc s) in
                                let () = (FanConfig.antiquotations := q) in
                                res in
                            let expand_expr =
                             fun loc ->
                              fun loc_name_opt ->
                               fun s ->
                                let ast = (parse_quot_string entry_eoi loc s) in
                                let () = (MetaLoc.loc_name := loc_name_opt) in
                                let meta_ast = (mexpr loc ast) in
                                let exp_ast =
                                 (antiquot_expander#expr meta_ast) in
                                exp_ast in
                            let expand_str_item =
                             fun loc ->
                              fun loc_name_opt ->
                               fun s ->
                                let exp_ast =
                                 (expand_expr loc loc_name_opt s) in
                                (Ast.StExp (loc, exp_ast)) in
                            let expand_patt =
                             fun _loc ->
                              fun loc_name_opt ->
                               fun s ->
                                let ast =
                                 (parse_quot_string entry_eoi _loc s) in
                                let meta_ast = (mpatt _loc ast) in
                                let exp_ast =
                                 (antiquot_expander#patt meta_ast) in
                                (match loc_name_opt with
                                 | None -> exp_ast
                                 | Some (name) ->
                                    let rec subst_first_loc =
                                     function
                                     | Ast.PaApp
                                        (_loc,
                                         Ast.PaId
                                          (_,
                                           Ast.IdAcc
                                            (_, Ast.IdUid (_, "Ast"),
                                             Ast.IdUid (_, u))), _) ->
                                        (Ast.PaApp
                                          (_loc, (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Ast")) ),
                                                 ( (Ast.IdUid (_loc, u)) )))
                                              ))) ), (
                                           (Ast.PaId
                                             (_loc, (
                                              (Ast.IdLid (_loc, name)) ))) )))
                                     | Ast.PaApp (_loc, a, b) ->
                                        (Ast.PaApp
                                          (_loc, ( (subst_first_loc a) ), b))
                                     | p -> p in
                                    (subst_first_loc exp_ast)) in
                            (
                            (Gram.extend (
                              (entry_eoi : 'entry_eoi Gram.Entry.t) ) (
                              ((fun ()
                                  ->
                                 (None , (
                                  [(None , None , (
                                    [((
                                      [(
                                       (Gram.Snterm
                                         (Gram.Entry.obj (
                                           (entry : 'entry Gram.Entry.t) )))
                                       ); (
                                       (Gram.Stoken
                                         ((
                                          function
                                          | EOI -> (true)
                                          | _ -> (false) ), "EOI")) )] ), (
                                      (Gram.Action.mk (
                                        fun (__camlp4_0 :
                                          Gram.Token.t) ->
                                         fun (x :
                                           'entry) ->
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (match __camlp4_0 with
                                            | EOI -> (x : 'entry_eoi)
                                            | _ -> assert false) )) ))] ))]
                                  ))) () ) ))
                            );
                            (
                            (Quotation.add name Quotation.DynAst.expr_tag
                              expand_expr)
                            );
                            (
                            (Quotation.add name Quotation.DynAst.patt_tag
                              expand_patt)
                            );
                            (Quotation.add name Quotation.DynAst.str_item_tag
                              expand_str_item)

                       let _ = (add_quotation "sig_item" sig_item_quot
                                 ME.meta_sig_item MP.meta_sig_item)

                       let _ = (add_quotation "str_item" str_item_quot
                                 ME.meta_str_item MP.meta_str_item)

                       let _ = (add_quotation "ctyp" ctyp_quot ME.meta_ctyp
                                 MP.meta_ctyp)

                       let _ = (add_quotation "patt" patt_quot ME.meta_patt
                                 MP.meta_patt)

                       let _ = (add_quotation "expr" expr_quot ME.meta_expr
                                 MP.meta_expr)

                       let _ = (add_quotation "module_type" module_type_quot
                                 ME.meta_module_type MP.meta_module_type)

                       let _ = (add_quotation "module_expr" module_expr_quot
                                 ME.meta_module_expr MP.meta_module_expr)

                       let _ = (add_quotation "class_type" class_type_quot
                                 ME.meta_class_type MP.meta_class_type)

                       let _ = (add_quotation "class_expr" class_expr_quot
                                 ME.meta_class_expr MP.meta_class_expr)

                       let _ = (add_quotation "class_sig_item"
                                 class_sig_item_quot ME.meta_class_sig_item
                                 MP.meta_class_sig_item)

                       let _ = (add_quotation "class_str_item"
                                 class_str_item_quot ME.meta_class_str_item
                                 MP.meta_class_str_item)

                       let _ = (add_quotation "with_constr" with_constr_quot
                                 ME.meta_with_constr MP.meta_with_constr)

                       let _ = (add_quotation "binding" binding_quot
                                 ME.meta_binding MP.meta_binding)

                       let _ = (add_quotation "rec_binding" rec_binding_quot
                                 ME.meta_rec_binding MP.meta_rec_binding)

                       let _ = (add_quotation "match_case" match_case_quot
                                 ME.meta_match_case MP.meta_match_case)

                       let _ = (add_quotation "module_binding"
                                 module_binding_quot ME.meta_module_binding
                                 MP.meta_module_binding)

                       let _ = (add_quotation "ident" ident_quot
                                 ME.meta_ident MP.meta_ident)

                       let _ = (add_quotation "rec_flag" rec_flag_quot
                                 ME.meta_rec_flag MP.meta_rec_flag)

                       let _ = (add_quotation "private_flag"
                                 private_flag_quot ME.meta_private_flag
                                 MP.meta_private_flag)

                       let _ = (add_quotation "row_var_flag"
                                 row_var_flag_quot ME.meta_row_var_flag
                                 MP.meta_row_var_flag)

                       let _ = (add_quotation "mutable_flag"
                                 mutable_flag_quot ME.meta_mutable_flag
                                 MP.meta_mutable_flag)

                       let _ = (add_quotation "virtual_flag"
                                 virtual_flag_quot ME.meta_virtual_flag
                                 MP.meta_virtual_flag)

                       let _ = (add_quotation "override_flag"
                                 override_flag_quot ME.meta_override_flag
                                 MP.meta_override_flag)

                       let _ = (add_quotation "direction_flag"
                                 direction_flag_quot ME.meta_direction_flag
                                 MP.meta_direction_flag)

                      end
