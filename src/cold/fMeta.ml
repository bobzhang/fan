open FAst
class primitive =
  object 
    method int _loc (i : int) =
      ((`Int (_loc, (string_of_int i)) : FAst.ep ) : ep )
    method int32 _loc (i : int32) =
      ((`Int32 (_loc, (Int32.to_string i)) : FAst.ep ) : ep )
    method int64 _loc (i : int64) =
      ((`Int64 (_loc, (Int64.to_string i)) : FAst.ep ) : ep )
    method nativeint _loc (i : nativeint) =
      ((`Nativeint (_loc, (Nativeint.to_string i)) : FAst.ep ) : ep )
    method float _loc (i : float) =
      ((`Flo (_loc, (string_of_float i)) : FAst.ep ) : ep )
    method string _loc (i : string) =
      ((`Str (_loc, (String.escaped i)) : FAst.ep ) : ep )
    method char _loc (i : char) =
      ((`Chr (_loc, (Char.escaped i)) : FAst.ep ) : ep )
    method unit _loc (_ : unit) = ((`Uid (_loc, "()") : FAst.ep ) : ep )
    method loc _loc (_l : loc) =
      (let n = Locf.name.contents in (`Lid (_loc, n) : FAst.ep ) : ep )
    method ant (_loc : loc) (x : ant) = ((x :>ep) : ep )
    method bool _loc x =
      (match x with
       | true  -> (`Lid (_loc, "true") : FAst.ep )
       | false  -> (`Lid (_loc, "false") : FAst.ep ) : ep )
  end
class meta =
  object (self : 'self_type)
    inherit  primitive
    method literal : 'loc -> literal -> FAst.ep=
      fun _loc  ->
        function
        | `Chr (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Chr")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Int (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Int")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Int32 (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Int32")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Int64 (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Int64")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Flo (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Flo")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Nativeint (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Nativeint")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Str (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Str")), (self#loc _loc _a0))),
                (self#string _loc _a1))
    method flag : 'loc -> flag -> FAst.ep=
      fun _loc  ->
        function
        | `Positive _a0 ->
            `App (_loc, (`Vrn (_loc, "Positive")), (self#loc _loc _a0))
        | `Negative _a0 ->
            `App (_loc, (`Vrn (_loc, "Negative")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method position_flag : 'loc -> position_flag -> FAst.ep=
      fun _loc  ->
        function
        | `Positive _a0 ->
            `App (_loc, (`Vrn (_loc, "Positive")), (self#loc _loc _a0))
        | `Negative _a0 ->
            `App (_loc, (`Vrn (_loc, "Negative")), (self#loc _loc _a0))
        | `Normal _a0 ->
            `App (_loc, (`Vrn (_loc, "Normal")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method strings : 'loc -> strings -> FAst.ep=
      fun _loc  ->
        function
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#strings _loc _a1))), (self#strings _loc _a2))
        | `Str (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Str")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method lident : 'loc -> lident -> FAst.ep=
      fun _loc  (`Lid (_a0,_a1))  ->
        `App
          (_loc, (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
            (self#string _loc _a1))
    method alident : 'loc -> alident -> FAst.ep=
      fun _loc  ->
        function
        | `Lid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method auident : 'loc -> auident -> FAst.ep=
      fun _loc  ->
        function
        | `Uid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Uid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method aident : 'loc -> aident -> FAst.ep=
      fun _loc  ->
        function
        | #alident as _a0 -> (self#alident _loc _a0 :>FAst.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>FAst.ep)
    method astring : 'loc -> astring -> FAst.ep=
      fun _loc  ->
        function
        | `C (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "C")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method uident : 'loc -> uident -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#uident _loc _a1))), (self#uident _loc _a2))
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#uident _loc _a1))), (self#uident _loc _a2))
        | #auident as _a0 -> (self#auident _loc _a0 :>FAst.ep)
    method ident : 'loc -> ident -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | `Apply (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Apply")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | #alident as _a0 -> (self#alident _loc _a0 :>FAst.ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>FAst.ep)
    method ident' : 'loc -> ident' -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | `Apply (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Apply")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | `Lid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Uid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Uid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
    method vid : 'loc -> vid -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#vid _loc _a2))
        | `Lid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Uid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Uid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method vid' : 'loc -> vid' -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#vid _loc _a2))
        | `Lid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Uid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Uid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
    method dupath : 'loc -> dupath -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#dupath _loc _a1))), (self#dupath _loc _a2))
        | #auident as _a0 -> (self#auident _loc _a0 :>FAst.ep)
    method dlpath : 'loc -> dlpath -> FAst.ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#dupath _loc _a1))), (self#alident _loc _a2))
        | #alident as _a0 -> (self#alident _loc _a0 :>FAst.ep)
    method any : 'loc -> any -> FAst.ep=
      fun _loc  (`Any _a0)  ->
        `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
    method ctyp : 'loc -> ctyp -> FAst.ep=
      fun _loc  ->
        function
        | `Alias (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Alias")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#alident _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `Arrow (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Arrow")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `ClassPath (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassPath")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Label")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#ctyp _loc _a2))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLabl")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#ctyp _loc _a2))
        | #ident' as _a0 -> (self#ident' _loc _a0 :>FAst.ep)
        | `TyObj (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyObj")), (self#loc _loc _a0))),
                     (self#name_ctyp _loc _a1))), (self#flag _loc _a2))
        | `TyObjEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyObjEnd")), (self#loc _loc _a0))),
                (self#flag _loc _a1))
        | `TyPol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyPol")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `TyPolEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyPolEnd")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
        | `TyTypePol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyTypePol")),
                          (self#loc _loc _a0))), (self#ctyp _loc _a1))),
                (self#ctyp _loc _a2))
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Quote")), (self#loc _loc _a0))),
                     (self#position_flag _loc _a1))),
                (self#alident _loc _a2))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "QuoteAny")), (self#loc _loc _a0))),
                (self#position_flag _loc _a1))
        | `Par (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Par")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
        | `Sta (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sta")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `PolyEq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "PolyEq")), (self#loc _loc _a0))),
                (self#row_field _loc _a1))
        | `PolySup (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "PolySup")), (self#loc _loc _a0))),
                (self#row_field _loc _a1))
        | `PolyInf (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "PolyInf")), (self#loc _loc _a0))),
                (self#row_field _loc _a1))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `PolyInfSup (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "PolyInfSup")),
                          (self#loc _loc _a0))), (self#row_field _loc _a1))),
                (self#tag_names _loc _a2))
        | `Package (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Package")), (self#loc _loc _a0))),
                (self#mtyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method type_parameters : 'loc -> type_parameters -> FAst.ep=
      fun _loc  ->
        function
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#type_parameters _loc _a1))),
                (self#type_parameters _loc _a2))
        | `Ctyp (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Ctyp")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method row_field : 'loc -> row_field -> FAst.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bar")), (self#loc _loc _a0))),
                     (self#row_field _loc _a1))), (self#row_field _loc _a2))
        | `TyVrn (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyVrn")), (self#loc _loc _a0))),
                (self#astring _loc _a1))
        | `TyVrnOf (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyVrnOf")), (self#loc _loc _a0))),
                     (self#astring _loc _a1))), (self#ctyp _loc _a2))
        | `Ctyp (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Ctyp")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
    method tag_names : 'loc -> tag_names -> FAst.ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#tag_names _loc _a1))), (self#tag_names _loc _a2))
        | `TyVrn (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyVrn")), (self#loc _loc _a0))),
                (self#astring _loc _a1))
    method typedecl : 'loc -> typedecl -> FAst.ep=
      fun _loc  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "TyDcl")),
                                    (self#loc _loc _a0))),
                               (self#alident _loc _a1))),
                          (self#opt_decl_params _loc _a2))),
                     (self#type_info _loc _a3))),
                (self#opt_type_constr _loc _a4))
        | `TyAbstr (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "TyAbstr")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))),
                     (self#opt_decl_params _loc _a2))),
                (self#opt_type_constr _loc _a3))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#typedecl _loc _a1))), (self#typedecl _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method type_constr : 'loc -> type_constr -> FAst.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#type_constr _loc _a1))),
                (self#type_constr _loc _a2))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method opt_type_constr : 'loc -> opt_type_constr -> FAst.ep=
      fun _loc  ->
        function
        | `Some (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Some")), (self#loc _loc _a0))),
                (self#type_constr _loc _a1))
        | `None _a0 ->
            `App (_loc, (`Vrn (_loc, "None")), (self#loc _loc _a0))
    method decl_param : 'loc -> decl_param -> FAst.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Quote")), (self#loc _loc _a0))),
                     (self#position_flag _loc _a1))),
                (self#alident _loc _a2))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "QuoteAny")), (self#loc _loc _a0))),
                (self#position_flag _loc _a1))
        | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method decl_params : 'loc -> decl_params -> FAst.ep=
      fun _loc  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Quote")), (self#loc _loc _a0))),
                     (self#position_flag _loc _a1))),
                (self#alident _loc _a2))
        | `QuoteAny (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "QuoteAny")), (self#loc _loc _a0))),
                (self#position_flag _loc _a1))
        | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#decl_params _loc _a1))),
                (self#decl_params _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method opt_decl_params : 'loc -> opt_decl_params -> FAst.ep=
      fun _loc  ->
        function
        | `Some (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Some")), (self#loc _loc _a0))),
                (self#decl_params _loc _a1))
        | `None _a0 ->
            `App (_loc, (`Vrn (_loc, "None")), (self#loc _loc _a0))
    method type_info : 'loc -> type_info -> FAst.ep=
      fun _loc  ->
        function
        | `TyMan (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "TyMan")),
                               (self#loc _loc _a0))), (self#ctyp _loc _a1))),
                     (self#flag _loc _a2))), (self#type_repr _loc _a3))
        | `TyRepr (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyRepr")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#type_repr _loc _a2))
        | `TyEq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "TyEq")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method type_repr : 'loc -> type_repr -> FAst.ep=
      fun _loc  ->
        function
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#name_ctyp _loc _a1))
        | `Sum (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sum")), (self#loc _loc _a0))),
                (self#or_ctyp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method name_ctyp : 'loc -> name_ctyp -> FAst.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#name_ctyp _loc _a1))), (self#name_ctyp _loc _a2))
        | `TyCol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyCol")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#ctyp _loc _a2))
        | `TyColMut (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyColMut")),
                          (self#loc _loc _a0))), (self#alident _loc _a1))),
                (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method or_ctyp : 'loc -> or_ctyp -> FAst.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bar")), (self#loc _loc _a0))),
                     (self#or_ctyp _loc _a1))), (self#or_ctyp _loc _a2))
        | `TyCol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyCol")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#ctyp _loc _a2))
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Of")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#ctyp _loc _a2))
        | #auident as _a0 -> (self#auident _loc _a0 :>FAst.ep)
    method of_ctyp : 'loc -> of_ctyp -> FAst.ep=
      fun _loc  ->
        function
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Of")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#ctyp _loc _a2))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>FAst.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method pat : 'loc -> pat -> FAst.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#pat _loc _a2))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Vrn")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#pat _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#pat _loc _a2))
        | `Par (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Par")), (self#loc _loc _a0))),
                (self#pat _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#rec_pat _loc _a1))
        | #literal as _a0 -> (self#literal _loc _a0 :>FAst.ep)
        | `Alias (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Alias")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#alident _loc _a2))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Array")), (self#loc _loc _a0))),
                (self#pat _loc _a1))
        | `LabelS (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "LabelS")), (self#loc _loc _a0))),
                (self#alident _loc _a1))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Label")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#pat _loc _a2))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLabl")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#pat _loc _a2))
        | `OptLablS (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "OptLablS")), (self#loc _loc _a0))),
                (self#alident _loc _a1))
        | `OptLablExpr (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "OptLablExpr")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#pat _loc _a2))),
                (self#exp _loc _a3))
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bar")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#pat _loc _a2))
        | `PaRng (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "PaRng")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#pat _loc _a2))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#pat _loc _a1))),
                (self#ctyp _loc _a2))
        | `ClassPath (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassPath")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Lazy (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lazy")), (self#loc _loc _a0))),
                (self#pat _loc _a1))
        | `ModuleUnpack (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleUnpack")), (self#loc _loc _a0))),
                (self#auident _loc _a1))
        | `ModuleConstraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleConstraint")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#ctyp _loc _a2))
    method rec_pat : 'loc -> rec_pat -> FAst.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#pat _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_pat _loc _a1))), (self#rec_pat _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method exp : 'loc -> exp -> FAst.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Vrn")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `Par (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Par")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#rec_exp _loc _a1))
        | #literal as _a0 -> (self#literal _loc _a0 :>FAst.ep)
        | `RecordWith (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecordWith")),
                          (self#loc _loc _a0))), (self#rec_exp _loc _a1))),
                (self#exp _loc _a2))
        | `Field (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Field")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `ArrayDot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ArrayDot")),
                          (self#loc _loc _a0))), (self#exp _loc _a1))),
                (self#exp _loc _a2))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Array")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | `Assert (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Assert")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | `Assign (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Assign")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc,
                                    (`App
                                       (_loc, (`Vrn (_loc, "For")),
                                         (self#loc _loc _a0))),
                                    (self#alident _loc _a1))),
                               (self#exp _loc _a2))), (self#exp _loc _a3))),
                     (self#flag _loc _a4))), (self#exp _loc _a5))
        | `Fun (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Fun")), (self#loc _loc _a0))),
                (self#case _loc _a1))
        | `IfThenElse (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "IfThenElse")),
                               (self#loc _loc _a0))), (self#exp _loc _a1))),
                     (self#exp _loc _a2))), (self#exp _loc _a3))
        | `IfThen (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "IfThen")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `LabelS (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "LabelS")), (self#loc _loc _a0))),
                (self#alident _loc _a1))
        | `Label (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Label")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#exp _loc _a2))
        | `Lazy (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lazy")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetIn")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#bind _loc _a2))), (self#exp _loc _a3))
        | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "LetTryInWith")),
                                    (self#loc _loc _a0))),
                               (self#flag _loc _a1))), (self#bind _loc _a2))),
                     (self#exp _loc _a3))), (self#case _loc _a4))
        | `LetModule (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetModule")),
                               (self#loc _loc _a0))),
                          (self#auident _loc _a1))), (self#mexp _loc _a2))),
                (self#exp _loc _a3))
        | `Match (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Match")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#case _loc _a2))
        | `New (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "New")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#clfield _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjPat")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#clfield _loc _a2))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#loc _loc _a0))),
                (self#pat _loc _a1))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLabl")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#exp _loc _a2))
        | `OptLablS (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "OptLablS")), (self#loc _loc _a0))),
                (self#alident _loc _a1))
        | `OvrInst (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "OvrInst")), (self#loc _loc _a0))),
                (self#rec_exp _loc _a1))
        | `OvrInstEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "OvrInstEmpty")), (self#loc _loc _a0))
        | `Seq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Seq")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | `Send (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Send")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#alident _loc _a2))
        | `StringDot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "StringDot")),
                          (self#loc _loc _a0))), (self#exp _loc _a1))),
                (self#exp _loc _a2))
        | `Try (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Try")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#case _loc _a2))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#exp _loc _a1))),
                (self#ctyp _loc _a2))
        | `Coercion (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Coercion")),
                               (self#loc _loc _a0))), (self#exp _loc _a1))),
                     (self#ctyp _loc _a2))), (self#ctyp _loc _a3))
        | `Subtype (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Subtype")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#ctyp _loc _a2))
        | `While (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "While")), (self#loc _loc _a0))),
                     (self#exp _loc _a1))), (self#exp _loc _a2))
        | `LetOpen (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetOpen")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#ident _loc _a2))), (self#exp _loc _a3))
        | `LocalTypeFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LocalTypeFun")),
                          (self#loc _loc _a0))), (self#alident _loc _a1))),
                (self#exp _loc _a2))
        | `Package_exp (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Package_exp")), (self#loc _loc _a0))),
                (self#mexp _loc _a1))
    method rec_exp : 'loc -> rec_exp -> FAst.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_exp _loc _a1))), (self#rec_exp _loc _a2))
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#exp _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method mtyp : 'loc -> mtyp -> FAst.ep=
      fun _loc  ->
        function
        | #ident' as _a0 -> (self#ident' _loc _a0 :>FAst.ep)
        | `Sig (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sig")), (self#loc _loc _a0))),
                (self#sigi _loc _a1))
        | `SigEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "SigEnd")), (self#loc _loc _a0))
        | `Functor (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Functor")),
                               (self#loc _loc _a0))),
                          (self#auident _loc _a1))), (self#mtyp _loc _a2))),
                (self#mtyp _loc _a3))
        | `With (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "With")), (self#loc _loc _a0))),
                     (self#mtyp _loc _a1))), (self#constr _loc _a2))
        | `ModuleTypeOf (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleTypeOf")), (self#loc _loc _a0))),
                (self#mexp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method sigi : 'loc -> sigi -> FAst.ep=
      fun _loc  ->
        function
        | `Val (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Val")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#ctyp _loc _a2))
        | `External (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "External")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#ctyp _loc _a2))),
                (self#strings _loc _a3))
        | `Type (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Type")), (self#loc _loc _a0))),
                (self#typedecl _loc _a1))
        | `Exception (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Exception")), (self#loc _loc _a0))),
                (self#of_ctyp _loc _a1))
        | `Class (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Class")), (self#loc _loc _a0))),
                (self#cltdecl _loc _a1))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassType")), (self#loc _loc _a0))),
                (self#cltdecl _loc _a1))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Module")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#mtyp _loc _a2))
        | `ModuleTypeEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleTypeEnd")),
                     (self#loc _loc _a0))), (self#auident _loc _a1))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleType")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#mtyp _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#sigi _loc _a1))), (self#sigi _loc _a2))
        | `DirectiveSimple (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "DirectiveSimple")),
                     (self#loc _loc _a0))), (self#alident _loc _a1))
        | `Directive (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Directive")),
                          (self#loc _loc _a0))), (self#alident _loc _a1))),
                (self#exp _loc _a2))
        | `Open (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Open")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#ident _loc _a2))
        | `Include (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Include")), (self#loc _loc _a0))),
                (self#mtyp _loc _a1))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecModule")), (self#loc _loc _a0))),
                (self#mbind _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method mbind : 'loc -> mbind -> FAst.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#mbind _loc _a1))), (self#mbind _loc _a2))
        | `ModuleBind (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "ModuleBind")),
                               (self#loc _loc _a0))),
                          (self#auident _loc _a1))), (self#mtyp _loc _a2))),
                (self#mexp _loc _a3))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#mtyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method constr : 'loc -> constr -> FAst.ep=
      fun _loc  ->
        function
        | `TypeEq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TypeEq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `ModuleEq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleEq")),
                          (self#loc _loc _a0))), (self#ident _loc _a1))),
                (self#ident _loc _a2))
        | `TypeEqPriv (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TypeEqPriv")),
                          (self#loc _loc _a0))), (self#ctyp _loc _a1))),
                (self#ctyp _loc _a2))
        | `TypeSubst (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TypeSubst")),
                          (self#loc _loc _a0))), (self#ctyp _loc _a1))),
                (self#ctyp _loc _a2))
        | `ModuleSubst (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleSubst")),
                          (self#loc _loc _a0))), (self#ident _loc _a1))),
                (self#ident _loc _a2))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#constr _loc _a1))), (self#constr _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method bind : 'loc -> bind -> FAst.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#bind _loc _a1))), (self#bind _loc _a2))
        | `Bind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bind")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#exp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method case : 'loc -> case -> FAst.ep=
      fun _loc  ->
        function
        | `Bar (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bar")), (self#loc _loc _a0))),
                     (self#case _loc _a1))), (self#case _loc _a2))
        | `Case (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Case")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#exp _loc _a2))
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CaseWhen")),
                               (self#loc _loc _a0))), (self#pat _loc _a1))),
                     (self#exp _loc _a2))), (self#exp _loc _a3))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method mexp : 'loc -> mexp -> FAst.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#mexp _loc _a1))), (self#mexp _loc _a2))
        | `Functor (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Functor")),
                               (self#loc _loc _a0))),
                          (self#auident _loc _a1))), (self#mtyp _loc _a2))),
                (self#mexp _loc _a3))
        | `Struct (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Struct")), (self#loc _loc _a0))),
                (self#stru _loc _a1))
        | `StructEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "StructEnd")), (self#loc _loc _a0))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#mexp _loc _a1))),
                (self#mtyp _loc _a2))
        | `PackageModule (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "PackageModule")),
                     (self#loc _loc _a0))), (self#exp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method stru : 'loc -> stru -> FAst.ep=
      fun _loc  ->
        function
        | `Class (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Class")), (self#loc _loc _a0))),
                (self#cldecl _loc _a1))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassType")), (self#loc _loc _a0))),
                (self#cltdecl _loc _a1))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#stru _loc _a1))), (self#stru _loc _a2))
        | `DirectiveSimple (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "DirectiveSimple")),
                     (self#loc _loc _a0))), (self#alident _loc _a1))
        | `Directive (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Directive")),
                          (self#loc _loc _a0))), (self#alident _loc _a1))),
                (self#exp _loc _a2))
        | `Exception (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Exception")), (self#loc _loc _a0))),
                (self#of_ctyp _loc _a1))
        | `StExp (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "StExp")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | `External (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "External")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#ctyp _loc _a2))),
                (self#strings _loc _a3))
        | `Include (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Include")), (self#loc _loc _a0))),
                (self#mexp _loc _a1))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Module")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#mexp _loc _a2))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecModule")), (self#loc _loc _a0))),
                (self#mbind _loc _a1))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleType")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#mtyp _loc _a2))
        | `Open (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Open")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#ident _loc _a2))
        | `Type (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Type")), (self#loc _loc _a0))),
                (self#typedecl _loc _a1))
        | `TypeWith (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TypeWith")),
                          (self#loc _loc _a0))), (self#typedecl _loc _a1))),
                (self#strings _loc _a2))
        | `Value (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Value")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#bind _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method cltdecl : 'loc -> cltdecl -> FAst.ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#cltdecl _loc _a1))), (self#cltdecl _loc _a2))
        | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "CtDecl")),
                                    (self#loc _loc _a0))),
                               (self#flag _loc _a1))), (self#ident _loc _a2))),
                     (self#type_parameters _loc _a3))),
                (self#cltyp _loc _a4))
        | `CtDeclS (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CtDeclS")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#ident _loc _a2))), (self#cltyp _loc _a3))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method cltyp : 'loc -> cltyp -> FAst.ep=
      fun _loc  ->
        function
        | #vid' as _a0 -> (self#vid' _loc _a0 :>FAst.ep)
        | `ClApply (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ClApply")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#type_parameters _loc _a2))
        | `CtFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CtFun")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#cltyp _loc _a2))
        | `ObjTy (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjTy")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#clsigi _loc _a2))
        | `ObjTyEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjTyEnd")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#clsigi _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#cltyp _loc _a1))), (self#cltyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method clsigi : 'loc -> clsigi -> FAst.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#clsigi _loc _a1))), (self#clsigi _loc _a2))
        | `SigInherit (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "SigInherit")), (self#loc _loc _a0))),
                (self#cltyp _loc _a1))
        | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "CgVal")),
                                    (self#loc _loc _a0))),
                               (self#alident _loc _a1))),
                          (self#flag _loc _a2))), (self#flag _loc _a3))),
                (self#ctyp _loc _a4))
        | `Method (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Method")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#flag _loc _a2))),
                (self#ctyp _loc _a3))
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "VirMeth")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#flag _loc _a2))),
                (self#ctyp _loc _a3))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method cldecl : 'loc -> cldecl -> FAst.ep=
      fun _loc  ->
        function
        | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "ClDecl")),
                                    (self#loc _loc _a0))),
                               (self#flag _loc _a1))), (self#ident _loc _a2))),
                     (self#type_parameters _loc _a3))),
                (self#clexp _loc _a4))
        | `ClDeclS (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "ClDeclS")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#ident _loc _a2))), (self#clexp _loc _a3))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#cldecl _loc _a1))), (self#cldecl _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method clexp : 'loc -> clexp -> FAst.ep=
      fun _loc  ->
        function
        | `CeApp (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CeApp")), (self#loc _loc _a0))),
                     (self#clexp _loc _a1))), (self#exp _loc _a2))
        | #vid' as _a0 -> (self#vid' _loc _a0 :>FAst.ep)
        | `ClApply (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ClApply")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#type_parameters _loc _a2))
        | `CeFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CeFun")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#clexp _loc _a2))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetIn")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#bind _loc _a2))), (self#clexp _loc _a3))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#clfield _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjPat")), (self#loc _loc _a0))),
                     (self#pat _loc _a1))), (self#clfield _loc _a2))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#loc _loc _a0))),
                (self#pat _loc _a1))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#clexp _loc _a1))),
                (self#cltyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method clfield : 'loc -> clfield -> FAst.ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#clfield _loc _a1))), (self#clfield _loc _a2))
        | `Inherit (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Inherit")), (self#loc _loc _a0))),
                     (self#flag _loc _a1))), (self#clexp _loc _a2))
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "InheritAs")),
                               (self#loc _loc _a0))), (self#flag _loc _a1))),
                     (self#clexp _loc _a2))), (self#alident _loc _a3))
        | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "CrVal")),
                                    (self#loc _loc _a0))),
                               (self#alident _loc _a1))),
                          (self#flag _loc _a2))), (self#flag _loc _a3))),
                (self#exp _loc _a4))
        | `VirVal (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "VirVal")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#flag _loc _a2))),
                (self#ctyp _loc _a3))
        | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc,
                                    (`App
                                       (_loc, (`Vrn (_loc, "CrMth")),
                                         (self#loc _loc _a0))),
                                    (self#alident _loc _a1))),
                               (self#flag _loc _a2))), (self#flag _loc _a3))),
                     (self#exp _loc _a4))), (self#ctyp _loc _a5))
        | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc, (`Vrn (_loc, "CrMthS")),
                                    (self#loc _loc _a0))),
                               (self#alident _loc _a1))),
                          (self#flag _loc _a2))), (self#flag _loc _a3))),
                (self#exp _loc _a4))
        | `VirMeth (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "VirMeth")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))), (self#flag _loc _a2))),
                (self#ctyp _loc _a3))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `Initializer (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Initializer")), (self#loc _loc _a0))),
                (self#exp _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
    method ep : 'loc -> ep -> FAst.ep=
      fun _loc  ->
        function
        | #vid as _a0 -> (self#vid _loc _a0 :>FAst.ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#ep _loc _a1))), (self#ep _loc _a2))
        | `Vrn (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Vrn")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Com (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Com")), (self#loc _loc _a0))),
                     (self#ep _loc _a1))), (self#ep _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#ep _loc _a1))), (self#ep _loc _a2))
        | `Par (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Par")), (self#loc _loc _a0))),
                (self#ep _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Array")), (self#loc _loc _a0))),
                (self#ep _loc _a1))
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#rec_bind _loc _a1))
        | #literal as _a0 -> (self#literal _loc _a0 :>FAst.ep)
    method rec_bind : 'loc -> rec_bind -> FAst.ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#vid _loc _a1))), (self#ep _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_bind _loc _a1))), (self#rec_bind _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>FAst.ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>FAst.ep)
  end