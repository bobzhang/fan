open Astf
class primitive =
  object 
    method int _loc (i : int) =
      ((`Int (_loc, (string_of_int i)) :>Astf.ep) : ep )
    method int32 _loc (i : int32) =
      ((`Int32 (_loc, (Int32.to_string i)) :>Astf.ep) : ep )
    method int64 _loc (i : int64) =
      ((`Int64 (_loc, (Int64.to_string i)) :>Astf.ep) : ep )
    method nativeint _loc (i : nativeint) =
      ((`Nativeint (_loc, (Nativeint.to_string i)) :>Astf.ep) : ep )
    method float _loc (i : float) =
      ((`Flo (_loc, (string_of_float i)) :>Astf.ep) : ep )
    method string _loc (i : string) =
      ((`Str (_loc, (String.escaped i)) :>Astf.ep) : ep )
    method char _loc (i : char) =
      ((`Chr (_loc, (Char.escaped i)) :>Astf.ep) : ep )
    method unit _loc (_ : unit) = ((`Unit _loc : ep ) : ep )
    method loc _loc (_l : loc) =
      (let n = !Locf.name in (`Lid (_loc, n) :>Astf.ep) : ep )
    method ant (_loc : loc) (x : ant) = ((x :>ep) : ep )
    method bool _loc x = (`Bool (_loc, x) : ep )
  end
class meta =
  object (self : 'self_type)
    inherit  primitive
    method literal : 'loc -> literal -> Astf.ep=
      fun _loc  ->
        function
        | `Chr (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Chr")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Int (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Int")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Int32 (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Int32")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Int64 (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Int64")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Flo (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Flo")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Nativeint (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Nativeint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Str (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Str")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Bool (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Bool")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#bool _loc _a1))))))
        | `Unit _a0 ->
            `App (_loc, (`Vrn (_loc, "Unit")), (self#loc _loc _a0))
    method flag : 'loc -> flag -> Astf.ep=
      fun _loc  ->
        function
        | `Positive _a0 ->
            `App (_loc, (`Vrn (_loc, "Positive")), (self#loc _loc _a0))
        | `Negative _a0 ->
            `App (_loc, (`Vrn (_loc, "Negative")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method position_flag : 'loc -> position_flag -> Astf.ep=
      fun _loc  ->
        function
        | `Positive _a0 ->
            `App (_loc, (`Vrn (_loc, "Positive")), (self#loc _loc _a0))
        | `Negative _a0 ->
            `App (_loc, (`Vrn (_loc, "Negative")), (self#loc _loc _a0))
        | `Normal _a0 ->
            `App (_loc, (`Vrn (_loc, "Normal")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method strings : 'loc -> strings -> Astf.ep=
      fun _loc  ->
        function
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#strings _loc _a1),
                               (self#strings _loc _a2))))))))
        | `Str (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Str")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method lident : 'loc -> lident -> Astf.ep=
      fun _loc  (`Lid (_a0,_a1))  ->
        `App
          (_loc, (`Vrn (_loc, "Lid")),
            (`Par
               (_loc,
                 (`Com (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
    method alident : 'loc -> alident -> Astf.ep=
      fun _loc  ->
        function
        | `Lid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method auident : 'loc -> auident -> Astf.ep=
      fun _loc  ->
        function
        | `Uid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Uid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method aident : 'loc -> aident -> Astf.ep=
      fun _loc  ->
        function
        | #alident as _a0 -> (self#alident _loc _a0 :>Astf.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>Astf.ep)
    method astring : 'loc -> astring -> Astf.ep=
      fun _loc  ->
        function
        | `C (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "C")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method uident : 'loc -> uident -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#uident _loc _a1),
                               (self#uident _loc _a2))))))))
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#uident _loc _a1),
                               (self#uident _loc _a2))))))))
        | #auident as _a0 -> (self#auident _loc _a0 :>Astf.ep)
    method ident : 'loc -> ident -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | `Apply (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Apply")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | #alident as _a0 -> (self#alident _loc _a0 :>Astf.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>Astf.ep)
    method ident' : 'loc -> ident' -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | `Apply (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Apply")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | `Lid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Uid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Uid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
    method vid : 'loc -> vid -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1), (self#vid _loc _a2))))))))
        | `Lid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Uid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Uid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method vid' : 'loc -> vid' -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1), (self#vid _loc _a2))))))))
        | `Lid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Uid (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Uid")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
    method dupath : 'loc -> dupath -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#dupath _loc _a1),
                               (self#dupath _loc _a2))))))))
        | #auident as _a0 -> (self#auident _loc _a0 :>Astf.ep)
    method dlpath : 'loc -> dlpath -> Astf.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Dot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#dupath _loc _a1),
                               (self#alident _loc _a2))))))))
        | #alident as _a0 -> (self#alident _loc _a0 :>Astf.ep)
    method any : 'loc -> any -> Astf.ep=
      fun _loc  (`Any _a0)  ->
        `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
    method ctyp : 'loc -> ctyp -> Astf.ep=
      fun _loc  ->
        function
        | `Alias (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Alias")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#alident _loc _a2))))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Arrow (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Arrow")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `ClassPath (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ClassPath")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ident _loc _a1))))))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Label")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "OptLabl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #ident' as _a0 -> (self#ident' _loc _a0 :>Astf.ep)
        | `TyObj (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyObj")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#name_ctyp _loc _a1),
                               (self#flag _loc _a2))))))))
        | `TyObjEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "TyObjEnd")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#flag _loc _a1))))))
        | `TyPol (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyPol")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `TyPolEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "TyPolEnd")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ctyp _loc _a1))))))
        | `TyTypePol (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyTypePol")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Quote")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#position_flag _loc _a1),
                               (self#alident _loc _a2))))))))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#position_flag _loc _a1))))))
        | `Par (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Par")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ctyp _loc _a1))))))
        | `Sta (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sta")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `PolyEq (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "PolyEq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#row_field _loc _a1))))))
        | `PolySup (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "PolySup")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#row_field _loc _a1))))))
        | `PolyInf (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "PolyInf")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#row_field _loc _a1))))))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `PolyInfSup (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "PolyInfSup")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#row_field _loc _a1),
                               (self#tag_names _loc _a2))))))))
        | `Package (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Package")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mtyp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method type_parameters : 'loc -> type_parameters -> Astf.ep=
      fun _loc  ->
        function
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#type_parameters _loc _a1),
                               (self#type_parameters _loc _a2))))))))
        | `Ctyp (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Ctyp")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ctyp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method row_field : 'loc -> row_field -> Astf.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Bar")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#row_field _loc _a1),
                               (self#row_field _loc _a2))))))))
        | `TyVrn (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "TyVrn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#astring _loc _a1))))))
        | `TyVrnOf (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyVrnOf")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#astring _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Ctyp (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Ctyp")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ctyp _loc _a1))))))
    method tag_names : 'loc -> tag_names -> Astf.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#tag_names _loc _a1),
                               (self#tag_names _loc _a2))))))))
        | `TyVrn (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "TyVrn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#astring _loc _a1))))))
    method decl : 'loc -> decl -> Astf.ep=
      fun _loc  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "TyDcl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#opt_decl_params _loc _a2),
                                    (`Com
                                       (_loc, (self#type_info _loc _a3),
                                         (self#opt_type_constr _loc _a4))))))))))))
        | `TyAbstr (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "TyAbstr")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#opt_decl_params _loc _a2),
                                    (self#opt_type_constr _loc _a3))))))))))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#decl _loc _a1),
                               (self#decl _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method type_constr : 'loc -> type_constr -> Astf.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#type_constr _loc _a1),
                               (self#type_constr _loc _a2))))))))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Eq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method opt_type_constr : 'loc -> opt_type_constr -> Astf.ep=
      fun _loc  ->
        function
        | `Some (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Some")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#type_constr _loc _a1))))))
        | `None _a0 ->
            `App (_loc, (`Vrn (_loc, "None")), (self#loc _loc _a0))
    method decl_param : 'loc -> decl_param -> Astf.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Quote")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#position_flag _loc _a1),
                               (self#alident _loc _a2))))))))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#position_flag _loc _a1))))))
        | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method decl_params : 'loc -> decl_params -> Astf.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Quote")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#position_flag _loc _a1),
                               (self#alident _loc _a2))))))))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "QuoteAny")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#position_flag _loc _a1))))))
        | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#decl_params _loc _a1),
                               (self#decl_params _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method opt_decl_params : 'loc -> opt_decl_params -> Astf.ep=
      fun _loc  ->
        function
        | `Some (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Some")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#decl_params _loc _a1))))))
        | `None _a0 ->
            `App (_loc, (`Vrn (_loc, "None")), (self#loc _loc _a0))
    method type_info : 'loc -> type_info -> Astf.ep=
      fun _loc  ->
        function
        | `TyMan (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "TyMan")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (self#type_repr _loc _a3))))))))))
        | `TyRepr (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyRepr")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#type_repr _loc _a2))))))))
        | `TyEq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyEq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method type_repr : 'loc -> type_repr -> Astf.ep=
      fun _loc  ->
        function
        | `Record (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Record")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (self#name_ctyp _loc _a1))))))
        | `Sum (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Sum")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#or_ctyp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method name_ctyp : 'loc -> name_ctyp -> Astf.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#name_ctyp _loc _a1),
                               (self#name_ctyp _loc _a2))))))))
        | `TyCol (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyCol")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `TyColMut (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyColMut")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method or_ctyp : 'loc -> or_ctyp -> Astf.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Bar")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#or_ctyp _loc _a1),
                               (self#or_ctyp _loc _a2))))))))
        | `TyCol (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TyCol")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Of")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #auident as _a0 -> (self#auident _loc _a0 :>Astf.ep)
    method of_ctyp : 'loc -> of_ctyp -> Astf.ep=
      fun _loc  ->
        function
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Of")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Astf.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method pat : 'loc -> pat -> Astf.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#pat _loc _a2))))))))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Vrn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#pat _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#pat _loc _a2))))))))
        | `Par (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Par")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#pat _loc _a1))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Record")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#rec_pat _loc _a1))))))
        | #literal as _a0 -> (self#literal _loc _a0 :>Astf.ep)
        | `Alias (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Alias")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (self#alident _loc _a2))))))))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Array")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#pat _loc _a1))))))
        | `LabelS (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "LabelS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Label")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#pat _loc _a2))))))))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "OptLabl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#pat _loc _a2))))))))
        | `OptLablS (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "OptLablS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `OptLablExpr (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "OptLablExpr")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#pat _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Bar")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#pat _loc _a2))))))))
        | `PaRng (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "PaRng")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#pat _loc _a2))))))))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `ClassPath (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ClassPath")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ident _loc _a1))))))
        | `Lazy (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lazy")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#pat _loc _a1))))))
        | `ModuleUnpack (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleUnpack")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#auident _loc _a1))))))
        | `ModuleConstraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleConstraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#ctyp _loc _a2))))))))
    method rec_pat : 'loc -> rec_pat -> Astf.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "RecBind")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1), (self#pat _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#rec_pat _loc _a1),
                               (self#rec_pat _loc _a2))))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method exp : 'loc -> exp -> Astf.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Vrn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `Par (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Par")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Record")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#rec_exp _loc _a1))))))
        | #literal as _a0 -> (self#literal _loc _a0 :>Astf.ep)
        | `RecordWith (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "RecordWith")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#rec_exp _loc _a1),
                               (self#exp _loc _a2))))))))
        | `Field (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Field")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#vid _loc _a2))))))))
        | `ArrayDot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ArrayDot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Array")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | `Assert (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Assert")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | `Assign (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Assign")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
            `App
              (_loc, (`Vrn (_loc, "For")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#exp _loc _a2),
                                    (`Com
                                       (_loc, (self#exp _loc _a3),
                                         (`Com
                                            (_loc, (self#flag _loc _a4),
                                              (self#exp _loc _a5))))))))))))))
        | `Fun (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Fun")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#case _loc _a1))))))
        | `IfThenElse (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "IfThenElse")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (`Com
                                  (_loc, (self#exp _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | `IfThen (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "IfThen")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `LabelS (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "LabelS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Label")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#exp _loc _a2))))))))
        | `Lazy (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Lazy")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "LetIn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#bind _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "LetTryInWith")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#bind _loc _a2),
                                    (`Com
                                       (_loc, (self#exp _loc _a3),
                                         (self#case _loc _a4))))))))))))
        | `LetModule (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "LetModule")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (`Com
                                  (_loc, (self#mexp _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | `Match (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Match")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (self#case _loc _a2))))))))
        | `New (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "New")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ident _loc _a1))))))
        | `Obj (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Obj")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#clfield _loc _a1))))))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ObjPat")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (self#clfield _loc _a2))))))))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ObjPatEnd")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#pat _loc _a1))))))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "OptLabl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#exp _loc _a2))))))))
        | `OptLablS (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "OptLablS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `OvrInst (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "OvrInst")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#rec_exp _loc _a1))))))
        | `OvrInstEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "OvrInstEmpty")), (self#loc _loc _a0))
        | `Seq (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Seq")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | `Send (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Send")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (self#alident _loc _a2))))))))
        | `StringDot (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "StringDot")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `Try (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Try")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (self#case _loc _a2))))))))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Coercion (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "Coercion")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (`Com
                                  (_loc, (self#ctyp _loc _a2),
                                    (self#ctyp _loc _a3))))))))))
        | `Subtype (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Subtype")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `While (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "While")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#exp _loc _a1), (self#exp _loc _a2))))))))
        | `LetOpen (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "LetOpen")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#ident _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | `LocalTypeFun (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "LocalTypeFun")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#exp _loc _a2))))))))
        | `Package_exp (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Package_exp")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mexp _loc _a1))))))
    method rec_exp : 'loc -> rec_exp -> Astf.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#rec_exp _loc _a1),
                               (self#rec_exp _loc _a2))))))))
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "RecBind")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1), (self#exp _loc _a2))))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method mtyp : 'loc -> mtyp -> Astf.ep=
      fun _loc  ->
        function
        | #ident' as _a0 -> (self#ident' _loc _a0 :>Astf.ep)
        | `Sig (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Sig")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#sigi _loc _a1))))))
        | `SigEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "SigEnd")), (self#loc _loc _a0))
        | `Functor (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "Functor")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (`Com
                                  (_loc, (self#mtyp _loc _a2),
                                    (self#mtyp _loc _a3))))))))))
        | `With (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "With")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#mtyp _loc _a1),
                               (self#constr _loc _a2))))))))
        | `ModuleTypeOf (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleTypeOf")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mexp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method sigi : 'loc -> sigi -> Astf.ep=
      fun _loc  ->
        function
        | `Val (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Val")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `External (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "External")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#ctyp _loc _a2),
                                    (self#strings _loc _a3))))))))))
        | `Type (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Type")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#decl _loc _a1))))))
        | `Exception (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Exception")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#of_ctyp _loc _a1))))))
        | `Class (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Class")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#cltdecl _loc _a1))))))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ClassType")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#cltdecl _loc _a1))))))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Module")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#mtyp _loc _a2))))))))
        | `ModuleTypeEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleTypeEnd")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#auident _loc _a1))))))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleType")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#mtyp _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#sigi _loc _a1),
                               (self#sigi _loc _a2))))))))
        | `DirectiveSimple (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "DirectiveSimple")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `Directive (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Directive")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#exp _loc _a2))))))))
        | `Open (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Open")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#ident _loc _a2))))))))
        | `Include (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Include")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mtyp _loc _a1))))))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "RecModule")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mbind _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method mbind : 'loc -> mbind -> Astf.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#mbind _loc _a1),
                               (self#mbind _loc _a2))))))))
        | `ModuleBind (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleBind")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (`Com
                                  (_loc, (self#mtyp _loc _a2),
                                    (self#mexp _loc _a3))))))))))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#mtyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method constr : 'loc -> constr -> Astf.ep=
      fun _loc  ->
        function
        | `TypeEq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TypeEq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `ModuleEq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleEq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | `TypeEqPriv (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TypeEqPriv")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `TypeSubst (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TypeSubst")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `ModuleSubst (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleSubst")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ident _loc _a1),
                               (self#ident _loc _a2))))))))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#constr _loc _a1),
                               (self#constr _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method bind : 'loc -> bind -> Astf.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#bind _loc _a1),
                               (self#bind _loc _a2))))))))
        | `Bind (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Bind")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#exp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method case : 'loc -> case -> Astf.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Bar")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#case _loc _a1),
                               (self#case _loc _a2))))))))
        | `Case (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Case")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1), (self#exp _loc _a2))))))))
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "CaseWhen")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (`Com
                                  (_loc, (self#exp _loc _a2),
                                    (self#exp _loc _a3))))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method mexp : 'loc -> mexp -> Astf.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#mexp _loc _a1),
                               (self#mexp _loc _a2))))))))
        | `Functor (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "Functor")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (`Com
                                  (_loc, (self#mtyp _loc _a2),
                                    (self#mexp _loc _a3))))))))))
        | `Struct (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Struct")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#stru _loc _a1))))))
        | `StructEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "StructEnd")), (self#loc _loc _a0))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#mexp _loc _a1),
                               (self#mtyp _loc _a2))))))))
        | `PackageModule (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "PackageModule")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method stru : 'loc -> stru -> Astf.ep=
      fun _loc  ->
        function
        | `Class (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Class")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#cldecl _loc _a1))))))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ClassType")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#cltdecl _loc _a1))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#stru _loc _a1),
                               (self#stru _loc _a2))))))))
        | `DirectiveSimple (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "DirectiveSimple")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#alident _loc _a1))))))
        | `Directive (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Directive")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (self#exp _loc _a2))))))))
        | `Exception (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Exception")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#of_ctyp _loc _a1))))))
        | `StExp (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "StExp")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | `External (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "External")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#ctyp _loc _a2),
                                    (self#strings _loc _a3))))))))))
        | `Include (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Include")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mexp _loc _a1))))))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Module")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#mexp _loc _a2))))))))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "RecModule")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#mbind _loc _a1))))))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ModuleType")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#auident _loc _a1),
                               (self#mtyp _loc _a2))))))))
        | `Open (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Open")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#ident _loc _a2))))))))
        | `Type (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Type")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#decl _loc _a1))))))
        | `TypeWith (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "TypeWith")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#decl _loc _a1),
                               (self#strings _loc _a2))))))))
        | `Value (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Value")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#bind _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method cltdecl : 'loc -> cltdecl -> Astf.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#cltdecl _loc _a1),
                               (self#cltdecl _loc _a2))))))))
        | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "CtDecl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#ident _loc _a2),
                                    (`Com
                                       (_loc,
                                         (self#type_parameters _loc _a3),
                                         (self#cltyp _loc _a4))))))))))))
        | `CtDeclS (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "CtDeclS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#ident _loc _a2),
                                    (self#cltyp _loc _a3))))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method cltyp : 'loc -> cltyp -> Astf.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Astf.ep)
        | `ClApply (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ClApply")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1),
                               (self#type_parameters _loc _a2))))))))
        | `CtFun (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "CtFun")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#cltyp _loc _a2))))))))
        | `ObjTy (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ObjTy")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#clsigi _loc _a2))))))))
        | `ObjTyEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ObjTyEnd")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ctyp _loc _a1))))))
        | `Obj (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Obj")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#clsigi _loc _a1))))))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#cltyp _loc _a1),
                               (self#cltyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method clsigi : 'loc -> clsigi -> Astf.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#clsigi _loc _a1),
                               (self#clsigi _loc _a2))))))))
        | `SigInherit (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "SigInherit")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#cltyp _loc _a1))))))
        | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "CgVal")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (`Com
                                       (_loc, (self#flag _loc _a3),
                                         (self#ctyp _loc _a4))))))))))))
        | `Method (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "Method")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (self#ctyp _loc _a3))))))))))
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "VirMeth")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (self#ctyp _loc _a3))))))))))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Eq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method cldecl : 'loc -> cldecl -> Astf.ep=
      fun _loc  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "ClDecl")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#ident _loc _a2),
                                    (`Com
                                       (_loc,
                                         (self#type_parameters _loc _a3),
                                         (self#clexp _loc _a4))))))))))))
        | `ClDeclS (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "ClDeclS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#ident _loc _a2),
                                    (self#clexp _loc _a3))))))))))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "And")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#cldecl _loc _a1),
                               (self#cldecl _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method clexp : 'loc -> clexp -> Astf.ep=
      fun _loc  ->
        function
        | `CeApp (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "CeApp")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#clexp _loc _a1),
                               (self#exp _loc _a2))))))))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>Astf.ep)
        | `ClApply (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ClApply")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1),
                               (self#type_parameters _loc _a2))))))))
        | `CeFun (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "CeFun")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (self#clexp _loc _a2))))))))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "LetIn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#bind _loc _a2),
                                    (self#clexp _loc _a3))))))))))
        | `Obj (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Obj")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#clfield _loc _a1))))))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "ObjPat")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#pat _loc _a1),
                               (self#clfield _loc _a2))))))))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "ObjPatEnd")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#pat _loc _a1))))))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#clexp _loc _a1),
                               (self#cltyp _loc _a2))))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method clfield : 'loc -> clfield -> Astf.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#clfield _loc _a1),
                               (self#clfield _loc _a2))))))))
        | `Inherit (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Inherit")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (self#clexp _loc _a2))))))))
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "InheritAs")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#flag _loc _a1),
                               (`Com
                                  (_loc, (self#clexp _loc _a2),
                                    (self#alident _loc _a3))))))))))
        | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "CrVal")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (`Com
                                       (_loc, (self#flag _loc _a3),
                                         (self#exp _loc _a4))))))))))))
        | `VirVal (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "VirVal")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (self#ctyp _loc _a3))))))))))
        | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
            `App
              (_loc, (`Vrn (_loc, "CrMth")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (`Com
                                       (_loc, (self#flag _loc _a3),
                                         (`Com
                                            (_loc, (self#exp _loc _a4),
                                              (self#ctyp _loc _a5))))))))))))))
        | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc, (`Vrn (_loc, "CrMthS")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (`Com
                                       (_loc, (self#flag _loc _a3),
                                         (self#exp _loc _a4))))))))))))
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            `App
              (_loc, (`Vrn (_loc, "VirMeth")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#alident _loc _a1),
                               (`Com
                                  (_loc, (self#flag _loc _a2),
                                    (self#ctyp _loc _a3))))))))))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Eq")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ctyp _loc _a1),
                               (self#ctyp _loc _a2))))))))
        | `Initializer (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Initializer")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#exp _loc _a1))))))
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
    method ep : 'loc -> ep -> Astf.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>Astf.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "App")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ep _loc _a1), (self#ep _loc _a2))))))))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Vrn")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#string _loc _a1))))))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Com")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ep _loc _a1), (self#ep _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ep _loc _a1), (self#ep _loc _a2))))))))
        | `Par (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Par")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ep _loc _a1))))))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Constraint")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#ep _loc _a1), (self#ctyp _loc _a2))))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Array")),
                (`Par
                   (_loc,
                     (`Com (_loc, (self#loc _loc _a0), (self#ep _loc _a1))))))
        | `Record (_a0,_a1) ->
            `App
              (_loc, (`Vrn (_loc, "Record")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0), (self#rec_bind _loc _a1))))))
        | #literal as _a0 -> (self#literal _loc _a0 :>Astf.ep)
    method rec_bind : 'loc -> rec_bind -> Astf.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "RecBind")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#vid _loc _a1), (self#ep _loc _a2))))))))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Par
                   (_loc,
                     (`Com
                        (_loc, (self#loc _loc _a0),
                          (`Com
                             (_loc, (self#rec_bind _loc _a1),
                               (self#rec_bind _loc _a2))))))))
        | #any as _a0 -> (self#any _loc _a0 :>Astf.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>Astf.ep)
  end
