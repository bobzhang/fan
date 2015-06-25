open Astfn
class primitive =
  object
    method int =
      function
      | _loc ->
          (function
           | (i : int) -> (`Int (_loc, (string_of_int i)) :> Astf.ep))
    method int32 =
      function
      | _loc ->
          (function
           | (i : int32) -> (`Int32 (_loc, (Int32.to_string i)) :> Astf.ep))
    method int64 =
      function
      | _loc ->
          (function
           | (i : int64) -> (`Int64 (_loc, (Int64.to_string i)) :> Astf.ep))
    method nativeint =
      function
      | _loc ->
          (function
           | (i : nativeint) ->
               (`Nativeint (_loc, (Nativeint.to_string i)) :> Astf.ep))
    method float =
      function
      | _loc ->
          (function
           | (i : float) -> (`Flo (_loc, (string_of_float i)) :> Astf.ep))
    method string =
      function
      | _loc ->
          (function
           | (i : string) -> (`Str (_loc, (String.escaped i)) :> Astf.ep))
    method char =
      function
      | _loc ->
          (function
           | (i : char) -> (`Chr (_loc, (Char.escaped i)) :> Astf.ep))
    method unit =
      function | _loc -> (function | (_ : unit) -> (`Unit _loc : Astf.ep))
    method ant =
      function | (_loc : loc) -> (function | (x : ant) -> (x :> Astf.ep))
    method bool =
      function | _loc -> (function | x -> (`Bool (_loc, x) : Astf.ep))
  end
class meta =
  object (self : 'self_type)
    inherit  primitive
    method literal : 'loc -> literal -> Astf.ep=
      function
      | _loc ->
          (function
           | `Chr _a0 ->
               `App (_loc, (`Vrn (_loc, "Chr")), (self#string _loc _a0))
           | `Int _a0 ->
               `App (_loc, (`Vrn (_loc, "Int")), (self#string _loc _a0))
           | `Int32 _a0 ->
               `App (_loc, (`Vrn (_loc, "Int32")), (self#string _loc _a0))
           | `Int64 _a0 ->
               `App (_loc, (`Vrn (_loc, "Int64")), (self#string _loc _a0))
           | `Flo _a0 ->
               `App (_loc, (`Vrn (_loc, "Flo")), (self#string _loc _a0))
           | `Nativeint _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Nativeint")), (self#string _loc _a0))
           | `Str _a0 ->
               `App (_loc, (`Vrn (_loc, "Str")), (self#string _loc _a0))
           | `Bool _a0 ->
               `App (_loc, (`Vrn (_loc, "Bool")), (self#bool _loc _a0))
           | `Unit -> `Vrn (_loc, "Unit"))
    method flag : 'loc -> flag -> Astf.ep=
      function
      | _loc ->
          (function
           | `Positive -> `Vrn (_loc, "Positive")
           | `Negative -> `Vrn (_loc, "Negative")
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method position_flag : 'loc -> position_flag -> Astf.ep=
      function
      | _loc ->
          (function
           | `Positive -> `Vrn (_loc, "Positive")
           | `Negative -> `Vrn (_loc, "Negative")
           | `Normal -> `Vrn (_loc, "Normal")
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method strings : 'loc -> strings -> Astf.ep=
      function
      | _loc ->
          (function
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#strings _loc _a0),
                             (self#strings _loc _a1))))))
           | `Str _a0 ->
               `App (_loc, (`Vrn (_loc, "Str")), (self#string _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method lident : 'loc -> lident -> Astf.ep=
      function
      | _loc ->
          (function
           | `Lid _a0 ->
               `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0)))
    method alident : 'loc -> alident -> Astf.ep=
      function
      | _loc ->
          (function
           | `Lid _a0 ->
               `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method auident : 'loc -> auident -> Astf.ep=
      function
      | _loc ->
          (function
           | `Uid _a0 ->
               `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method aident : 'loc -> aident -> Astf.ep=
      function
      | _loc ->
          (function
           | #alident as _a0 -> (self#alident _loc _a0 :> Astf.ep)
           | #auident as _a0 -> (self#auident _loc _a0 :> Astf.ep))
    method astring : 'loc -> astring -> Astf.ep=
      function
      | _loc ->
          (function
           | `C _a0 ->
               `App (_loc, (`Vrn (_loc, "C")), (self#string _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method uident : 'loc -> uident -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#uident _loc _a0),
                             (self#uident _loc _a1))))))
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#uident _loc _a0),
                             (self#uident _loc _a1))))))
           | #auident as _a0 -> (self#auident _loc _a0 :> Astf.ep))
    method ident : 'loc -> ident -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | `Apply (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Apply")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | #alident as _a0 -> (self#alident _loc _a0 :> Astf.ep)
           | #auident as _a0 -> (self#auident _loc _a0 :> Astf.ep))
    method ident' : 'loc -> ident' -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | `Apply (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Apply")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | `Lid _a0 ->
               `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
           | `Uid _a0 ->
               `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0)))
    method vid : 'loc -> vid -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0), (self#vid _loc _a1))))))
           | `Lid _a0 ->
               `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
           | `Uid _a0 ->
               `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method vid' : 'loc -> vid' -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0), (self#vid _loc _a1))))))
           | `Lid _a0 ->
               `App (_loc, (`Vrn (_loc, "Lid")), (self#string _loc _a0))
           | `Uid _a0 ->
               `App (_loc, (`Vrn (_loc, "Uid")), (self#string _loc _a0)))
    method dupath : 'loc -> dupath -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#dupath _loc _a0),
                             (self#dupath _loc _a1))))))
           | #auident as _a0 -> (self#auident _loc _a0 :> Astf.ep))
    method dlpath : 'loc -> dlpath -> Astf.ep=
      function
      | _loc ->
          (function
           | `Dot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Dot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#dupath _loc _a0),
                             (self#alident _loc _a1))))))
           | #alident as _a0 -> (self#alident _loc _a0 :> Astf.ep))
    method any : 'loc -> any -> Astf.ep=
      function | _loc -> (function | `Any -> `Vrn (_loc, "Any"))
    method ctyp : 'loc -> ctyp -> Astf.ep=
      function
      | _loc ->
          (function
           | `Alias (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Alias")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0),
                             (self#alident _loc _a1))))))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `Arrow (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Arrow")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `ClassPath _a0 ->
               `App (_loc, (`Vrn (_loc, "ClassPath")), (self#ident _loc _a0))
           | `Label (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Label")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | `OptLabl (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "OptLabl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | #ident' as _a0 -> (self#ident' _loc _a0 :> Astf.ep)
           | `TyObj (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyObj")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#name_ctyp _loc _a0),
                             (self#flag _loc _a1))))))
           | `TyObjEnd _a0 ->
               `App (_loc, (`Vrn (_loc, "TyObjEnd")), (self#flag _loc _a0))
           | `TyPol (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyPol")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `TyPolEnd _a0 ->
               `App (_loc, (`Vrn (_loc, "TyPolEnd")), (self#ctyp _loc _a0))
           | `TyTypePol (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyTypePol")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `Quote (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Quote")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#position_flag _loc _a0),
                             (self#alident _loc _a1))))))
           | `QuoteAny _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "QuoteAny")),
                   (self#position_flag _loc _a0))
           | `Par _a0 ->
               `App (_loc, (`Vrn (_loc, "Par")), (self#ctyp _loc _a0))
           | `Sta (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sta")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `PolyEq _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "PolyEq")), (self#row_field _loc _a0))
           | `PolySup _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "PolySup")), (self#row_field _loc _a0))
           | `PolyInf _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "PolyInf")), (self#row_field _loc _a0))
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `PolyInfSup (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "PolyInfSup")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#row_field _loc _a0),
                             (self#tag_names _loc _a1))))))
           | `Package _a0 ->
               `App (_loc, (`Vrn (_loc, "Package")), (self#mtyp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method type_parameters : 'loc -> type_parameters -> Astf.ep=
      function
      | _loc ->
          (function
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#type_parameters _loc _a0),
                             (self#type_parameters _loc _a1))))))
           | `Ctyp _a0 ->
               `App (_loc, (`Vrn (_loc, "Ctyp")), (self#ctyp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method row_field : 'loc -> row_field -> Astf.ep=
      function
      | _loc ->
          (function
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep)
           | `Bar (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Bar")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#row_field _loc _a0),
                             (self#row_field _loc _a1))))))
           | `TyVrn _a0 ->
               `App (_loc, (`Vrn (_loc, "TyVrn")), (self#astring _loc _a0))
           | `TyVrnOf (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyVrnOf")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#astring _loc _a0),
                             (self#ctyp _loc _a1))))))
           | `Ctyp _a0 ->
               `App (_loc, (`Vrn (_loc, "Ctyp")), (self#ctyp _loc _a0)))
    method tag_names : 'loc -> tag_names -> Astf.ep=
      function
      | _loc ->
          (function
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#tag_names _loc _a0),
                             (self#tag_names _loc _a1))))))
           | `TyVrn _a0 ->
               `App (_loc, (`Vrn (_loc, "TyVrn")), (self#astring _loc _a0)))
    method typedecl : 'loc -> typedecl -> Astf.ep=
      function
      | _loc ->
          (function
           | `TyDcl (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "TyDcl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#opt_decl_params _loc _a1),
                                  (`Com
                                     (_loc, (self#type_info _loc _a2),
                                       (self#opt_type_constr _loc _a3))))))))))
           | `TyAbstr (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "TyAbstr")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#opt_decl_params _loc _a1),
                                  (self#opt_type_constr _loc _a2))))))))
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#typedecl _loc _a0),
                             (self#typedecl _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method type_constr : 'loc -> type_constr -> Astf.ep=
      function
      | _loc ->
          (function
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#type_constr _loc _a0),
                             (self#type_constr _loc _a1))))))
           | `Eq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Eq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method opt_type_constr : 'loc -> opt_type_constr -> Astf.ep=
      function
      | _loc ->
          (function
           | `Some _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Some")), (self#type_constr _loc _a0))
           | `None -> `Vrn (_loc, "None"))
    method decl_param : 'loc -> decl_param -> Astf.ep=
      function
      | _loc ->
          (function
           | `Quote (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Quote")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#position_flag _loc _a0),
                             (self#alident _loc _a1))))))
           | `QuoteAny _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "QuoteAny")),
                   (self#position_flag _loc _a0))
           | `Any -> `Vrn (_loc, "Any")
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method decl_params : 'loc -> decl_params -> Astf.ep=
      function
      | _loc ->
          (function
           | `Quote (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Quote")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#position_flag _loc _a0),
                             (self#alident _loc _a1))))))
           | `QuoteAny _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "QuoteAny")),
                   (self#position_flag _loc _a0))
           | `Any -> `Vrn (_loc, "Any")
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#decl_params _loc _a0),
                             (self#decl_params _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method opt_decl_params : 'loc -> opt_decl_params -> Astf.ep=
      function
      | _loc ->
          (function
           | `Some _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Some")), (self#decl_params _loc _a0))
           | `None -> `Vrn (_loc, "None"))
    method type_info : 'loc -> type_info -> Astf.ep=
      function
      | _loc ->
          (function
           | `TyMan (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "TyMan")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (self#type_repr _loc _a2))))))))
           | `TyRepr (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyRepr")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (self#type_repr _loc _a1))))))
           | `TyEq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyEq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0), (self#ctyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method type_repr : 'loc -> type_repr -> Astf.ep=
      function
      | _loc ->
          (function
           | `Record _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Record")), (self#name_ctyp _loc _a0))
           | `Sum _a0 ->
               `App (_loc, (`Vrn (_loc, "Sum")), (self#or_ctyp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method name_ctyp : 'loc -> name_ctyp -> Astf.ep=
      function
      | _loc ->
          (function
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#name_ctyp _loc _a0),
                             (self#name_ctyp _loc _a1))))))
           | `TyCol (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyCol")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | `TyColMut (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyColMut")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method or_ctyp : 'loc -> or_ctyp -> Astf.ep=
      function
      | _loc ->
          (function
           | `Bar (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Bar")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#or_ctyp _loc _a0),
                             (self#or_ctyp _loc _a1))))))
           | `TyCol (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TyCol")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | `Of (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Of")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | #auident as _a0 -> (self#auident _loc _a0 :> Astf.ep))
    method of_ctyp : 'loc -> of_ctyp -> Astf.ep=
      function
      | _loc ->
          (function
           | `Of (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Of")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0), (self#ctyp _loc _a1))))))
           | #vid' as _a0 -> (self#vid' _loc _a0 :> Astf.ep)
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method pat : 'loc -> pat -> Astf.ep=
      function
      | _loc ->
          (function
           | #vid as _a0 -> (self#vid _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#pat _loc _a1))))))
           | `Vrn _a0 ->
               `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#pat _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#pat _loc _a1))))))
           | `Par _a0 ->
               `App (_loc, (`Vrn (_loc, "Par")), (self#pat _loc _a0))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | `Record _a0 ->
               `App (_loc, (`Vrn (_loc, "Record")), (self#rec_pat _loc _a0))
           | #literal as _a0 -> (self#literal _loc _a0 :> Astf.ep)
           | `Alias (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Alias")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0),
                             (self#alident _loc _a1))))))
           | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
           | `Array _a0 ->
               `App (_loc, (`Vrn (_loc, "Array")), (self#pat _loc _a0))
           | `LabelS _a0 ->
               `App (_loc, (`Vrn (_loc, "LabelS")), (self#alident _loc _a0))
           | `Label (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Label")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#pat _loc _a1))))))
           | `OptLabl (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "OptLabl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#pat _loc _a1))))))
           | `OptLablS _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "OptLablS")), (self#alident _loc _a0))
           | `OptLablExpr (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "OptLablExpr")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#pat _loc _a1),
                                  (self#exp _loc _a2))))))))
           | `Bar (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Bar")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#pat _loc _a1))))))
           | `PaRng (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "PaRng")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#pat _loc _a1))))))
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#ctyp _loc _a1))))))
           | `ClassPath _a0 ->
               `App (_loc, (`Vrn (_loc, "ClassPath")), (self#ident _loc _a0))
           | `Lazy _a0 ->
               `App (_loc, (`Vrn (_loc, "Lazy")), (self#pat _loc _a0))
           | `ModuleUnpack _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleUnpack")),
                   (self#auident _loc _a0))
           | `ModuleConstraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleConstraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#ctyp _loc _a1)))))))
    method rec_pat : 'loc -> rec_pat -> Astf.ep=
      function
      | _loc ->
          (function
           | `RecBind (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "RecBind")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0), (self#pat _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#rec_pat _loc _a0),
                             (self#rec_pat _loc _a1))))))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method exp : 'loc -> exp -> Astf.ep=
      function
      | _loc ->
          (function
           | #vid as _a0 -> (self#vid _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `Vrn _a0 ->
               `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `Par _a0 ->
               `App (_loc, (`Vrn (_loc, "Par")), (self#exp _loc _a0))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | `Record _a0 ->
               `App (_loc, (`Vrn (_loc, "Record")), (self#rec_exp _loc _a0))
           | #literal as _a0 -> (self#literal _loc _a0 :> Astf.ep)
           | `RecordWith (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "RecordWith")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#rec_exp _loc _a0),
                             (self#exp _loc _a1))))))
           | `Field (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Field")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#vid _loc _a1))))))
           | `ArrayDot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ArrayDot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
           | `Array _a0 ->
               `App (_loc, (`Vrn (_loc, "Array")), (self#exp _loc _a0))
           | `Assert _a0 ->
               `App (_loc, (`Vrn (_loc, "Assert")), (self#exp _loc _a0))
           | `Assign (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Assign")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `For (_a0,_a1,_a2,_a3,_a4) ->
               `App
                 (_loc, (`Vrn (_loc, "For")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#exp _loc _a1),
                                  (`Com
                                     (_loc, (self#exp _loc _a2),
                                       (`Com
                                          (_loc, (self#flag _loc _a3),
                                            (self#exp _loc _a4))))))))))))
           | `Fun _a0 ->
               `App (_loc, (`Vrn (_loc, "Fun")), (self#case _loc _a0))
           | `IfThenElse (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "IfThenElse")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0),
                             (`Com
                                (_loc, (self#exp _loc _a1),
                                  (self#exp _loc _a2))))))))
           | `IfThen (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "IfThen")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `LabelS _a0 ->
               `App (_loc, (`Vrn (_loc, "LabelS")), (self#alident _loc _a0))
           | `Label (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Label")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#exp _loc _a1))))))
           | `Lazy _a0 ->
               `App (_loc, (`Vrn (_loc, "Lazy")), (self#exp _loc _a0))
           | `LetIn (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "LetIn")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#bind _loc _a1),
                                  (self#exp _loc _a2))))))))
           | `LetTryInWith (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "LetTryInWith")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#bind _loc _a1),
                                  (`Com
                                     (_loc, (self#exp _loc _a2),
                                       (self#case _loc _a3))))))))))
           | `LetModule (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "LetModule")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (`Com
                                (_loc, (self#mexp _loc _a1),
                                  (self#exp _loc _a2))))))))
           | `Match (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Match")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#case _loc _a1))))))
           | `New _a0 ->
               `App (_loc, (`Vrn (_loc, "New")), (self#ident _loc _a0))
           | `Obj _a0 ->
               `App (_loc, (`Vrn (_loc, "Obj")), (self#clfield _loc _a0))
           | `ObjEnd -> `Vrn (_loc, "ObjEnd")
           | `ObjPat (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ObjPat")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0),
                             (self#clfield _loc _a1))))))
           | `ObjPatEnd _a0 ->
               `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#pat _loc _a0))
           | `OptLabl (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "OptLabl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#exp _loc _a1))))))
           | `OptLablS _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "OptLablS")), (self#alident _loc _a0))
           | `OvrInst _a0 ->
               `App (_loc, (`Vrn (_loc, "OvrInst")), (self#rec_exp _loc _a0))
           | `OvrInstEmpty -> `Vrn (_loc, "OvrInstEmpty")
           | `Seq _a0 ->
               `App (_loc, (`Vrn (_loc, "Seq")), (self#exp _loc _a0))
           | `Send (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Send")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0),
                             (self#alident _loc _a1))))))
           | `StringDot (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "StringDot")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `Try (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Try")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#case _loc _a1))))))
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#ctyp _loc _a1))))))
           | `Coercion (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "Coercion")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0),
                             (`Com
                                (_loc, (self#ctyp _loc _a1),
                                  (self#ctyp _loc _a2))))))))
           | `Subtype (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Subtype")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#ctyp _loc _a1))))))
           | `While (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "While")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#exp _loc _a0), (self#exp _loc _a1))))))
           | `LetOpen (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "LetOpen")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#ident _loc _a1),
                                  (self#exp _loc _a2))))))))
           | `LocalTypeFun (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "LocalTypeFun")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#exp _loc _a1))))))
           | `Package_exp _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Package_exp")), (self#mexp _loc _a0)))
    method rec_exp : 'loc -> rec_exp -> Astf.ep=
      function
      | _loc ->
          (function
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#rec_exp _loc _a0),
                             (self#rec_exp _loc _a1))))))
           | `RecBind (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "RecBind")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0), (self#exp _loc _a1))))))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method mtyp : 'loc -> mtyp -> Astf.ep=
      function
      | _loc ->
          (function
           | #ident' as _a0 -> (self#ident' _loc _a0 :> Astf.ep)
           | `Sig _a0 ->
               `App (_loc, (`Vrn (_loc, "Sig")), (self#sigi _loc _a0))
           | `SigEnd -> `Vrn (_loc, "SigEnd")
           | `Functor (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "Functor")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (`Com
                                (_loc, (self#mtyp _loc _a1),
                                  (self#mtyp _loc _a2))))))))
           | `With (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "With")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#mtyp _loc _a0),
                             (self#constr _loc _a1))))))
           | `ModuleTypeOf _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleTypeOf")), (self#mexp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method sigi : 'loc -> sigi -> Astf.ep=
      function
      | _loc ->
          (function
           | `Val (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Val")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#ctyp _loc _a1))))))
           | `External (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "External")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#ctyp _loc _a1),
                                  (self#strings _loc _a2))))))))
           | `Type _a0 ->
               `App (_loc, (`Vrn (_loc, "Type")), (self#typedecl _loc _a0))
           | `Exception _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Exception")), (self#of_ctyp _loc _a0))
           | `Class _a0 ->
               `App (_loc, (`Vrn (_loc, "Class")), (self#cltdecl _loc _a0))
           | `ClassType _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "ClassType")), (self#cltdecl _loc _a0))
           | `Module (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Module")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#mtyp _loc _a1))))))
           | `ModuleTypeEnd _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleTypeEnd")),
                   (self#auident _loc _a0))
           | `ModuleType (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleType")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#mtyp _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#sigi _loc _a0), (self#sigi _loc _a1))))))
           | `DirectiveSimple _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "DirectiveSimple")),
                   (self#alident _loc _a0))
           | `Directive (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Directive")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#exp _loc _a1))))))
           | `Open (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Open")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (self#ident _loc _a1))))))
           | `Include _a0 ->
               `App (_loc, (`Vrn (_loc, "Include")), (self#mtyp _loc _a0))
           | `RecModule _a0 ->
               `App (_loc, (`Vrn (_loc, "RecModule")), (self#mbind _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method mbind : 'loc -> mbind -> Astf.ep=
      function
      | _loc ->
          (function
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#mbind _loc _a0),
                             (self#mbind _loc _a1))))))
           | `ModuleBind (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleBind")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (`Com
                                (_loc, (self#mtyp _loc _a1),
                                  (self#mexp _loc _a2))))))))
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#mtyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method constr : 'loc -> constr -> Astf.ep=
      function
      | _loc ->
          (function
           | `TypeEq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TypeEq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `ModuleEq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleEq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | `TypeEqPriv (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TypeEqPriv")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `TypeSubst (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TypeSubst")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `ModuleSubst (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleSubst")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ident _loc _a0),
                             (self#ident _loc _a1))))))
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#constr _loc _a0),
                             (self#constr _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method bind : 'loc -> bind -> Astf.ep=
      function
      | _loc ->
          (function
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#bind _loc _a0), (self#bind _loc _a1))))))
           | `Bind (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Bind")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#exp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method case : 'loc -> case -> Astf.ep=
      function
      | _loc ->
          (function
           | `Bar (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Bar")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#case _loc _a0), (self#case _loc _a1))))))
           | `Case (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Case")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#exp _loc _a1))))))
           | `CaseWhen (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "CaseWhen")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0),
                             (`Com
                                (_loc, (self#exp _loc _a1),
                                  (self#exp _loc _a2))))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method mexp : 'loc -> mexp -> Astf.ep=
      function
      | _loc ->
          (function
           | #vid' as _a0 -> (self#vid' _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#mexp _loc _a0), (self#mexp _loc _a1))))))
           | `Functor (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "Functor")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (`Com
                                (_loc, (self#mtyp _loc _a1),
                                  (self#mexp _loc _a2))))))))
           | `Struct _a0 ->
               `App (_loc, (`Vrn (_loc, "Struct")), (self#stru _loc _a0))
           | `StructEnd -> `Vrn (_loc, "StructEnd")
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#mexp _loc _a0), (self#mtyp _loc _a1))))))
           | `PackageModule _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "PackageModule")), (self#exp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method stru : 'loc -> stru -> Astf.ep=
      function
      | _loc ->
          (function
           | `Class _a0 ->
               `App (_loc, (`Vrn (_loc, "Class")), (self#cldecl _loc _a0))
           | `ClassType _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "ClassType")), (self#cltdecl _loc _a0))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#stru _loc _a0), (self#stru _loc _a1))))))
           | `DirectiveSimple _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "DirectiveSimple")),
                   (self#alident _loc _a0))
           | `Directive (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Directive")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (self#exp _loc _a1))))))
           | `Exception _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "Exception")), (self#of_ctyp _loc _a0))
           | `StExp _a0 ->
               `App (_loc, (`Vrn (_loc, "StExp")), (self#exp _loc _a0))
           | `External (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "External")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#ctyp _loc _a1),
                                  (self#strings _loc _a2))))))))
           | `Include _a0 ->
               `App (_loc, (`Vrn (_loc, "Include")), (self#mexp _loc _a0))
           | `Module (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Module")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#mexp _loc _a1))))))
           | `RecModule _a0 ->
               `App (_loc, (`Vrn (_loc, "RecModule")), (self#mbind _loc _a0))
           | `ModuleType (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ModuleType")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#auident _loc _a0),
                             (self#mtyp _loc _a1))))))
           | `Open (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Open")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (self#ident _loc _a1))))))
           | `Type _a0 ->
               `App (_loc, (`Vrn (_loc, "Type")), (self#typedecl _loc _a0))
           | `TypeWith (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "TypeWith")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#typedecl _loc _a0),
                             (self#strings _loc _a1))))))
           | `Value (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Value")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0), (self#bind _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method cltdecl : 'loc -> cltdecl -> Astf.ep=
      function
      | _loc ->
          (function
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#cltdecl _loc _a0),
                             (self#cltdecl _loc _a1))))))
           | `CtDecl (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "CtDecl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#ident _loc _a1),
                                  (`Com
                                     (_loc, (self#type_parameters _loc _a2),
                                       (self#cltyp _loc _a3))))))))))
           | `CtDeclS (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "CtDeclS")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#ident _loc _a1),
                                  (self#cltyp _loc _a2))))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method cltyp : 'loc -> cltyp -> Astf.ep=
      function
      | _loc ->
          (function
           | #vid' as _a0 -> (self#vid' _loc _a0 :> Astf.ep)
           | `ClApply (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ClApply")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0),
                             (self#type_parameters _loc _a1))))))
           | `CtFun (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "CtFun")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0),
                             (self#cltyp _loc _a1))))))
           | `ObjTy (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ObjTy")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0),
                             (self#clsigi _loc _a1))))))
           | `ObjTyEnd _a0 ->
               `App (_loc, (`Vrn (_loc, "ObjTyEnd")), (self#ctyp _loc _a0))
           | `Obj _a0 ->
               `App (_loc, (`Vrn (_loc, "Obj")), (self#clsigi _loc _a0))
           | `ObjEnd -> `Vrn (_loc, "ObjEnd")
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#cltyp _loc _a0),
                             (self#cltyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method clsigi : 'loc -> clsigi -> Astf.ep=
      function
      | _loc ->
          (function
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#clsigi _loc _a0),
                             (self#clsigi _loc _a1))))))
           | `SigInherit _a0 ->
               `App
                 (_loc, (`Vrn (_loc, "SigInherit")), (self#cltyp _loc _a0))
           | `CgVal (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "CgVal")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (`Com
                                     (_loc, (self#flag _loc _a2),
                                       (self#ctyp _loc _a3))))))))))
           | `Method (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "Method")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (self#ctyp _loc _a2))))))))
           | `VirMeth (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "VirMeth")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (self#ctyp _loc _a2))))))))
           | `Eq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Eq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method cldecl : 'loc -> cldecl -> Astf.ep=
      function
      | _loc ->
          (function
           | `ClDecl (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "ClDecl")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#ident _loc _a1),
                                  (`Com
                                     (_loc, (self#type_parameters _loc _a2),
                                       (self#clexp _loc _a3))))))))))
           | `ClDeclS (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "ClDeclS")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#ident _loc _a1),
                                  (self#clexp _loc _a2))))))))
           | `And (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "And")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#cldecl _loc _a0),
                             (self#cldecl _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method clexp : 'loc -> clexp -> Astf.ep=
      function
      | _loc ->
          (function
           | `CeApp (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "CeApp")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#clexp _loc _a0), (self#exp _loc _a1))))))
           | #vid' as _a0 -> (self#vid' _loc _a0 :> Astf.ep)
           | `ClApply (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ClApply")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#vid _loc _a0),
                             (self#type_parameters _loc _a1))))))
           | `CeFun (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "CeFun")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0), (self#clexp _loc _a1))))))
           | `LetIn (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "LetIn")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#bind _loc _a1),
                                  (self#clexp _loc _a2))))))))
           | `Obj _a0 ->
               `App (_loc, (`Vrn (_loc, "Obj")), (self#clfield _loc _a0))
           | `ObjEnd -> `Vrn (_loc, "ObjEnd")
           | `ObjPat (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "ObjPat")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#pat _loc _a0),
                             (self#clfield _loc _a1))))))
           | `ObjPatEnd _a0 ->
               `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#pat _loc _a0))
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#clexp _loc _a0),
                             (self#cltyp _loc _a1))))))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method clfield : 'loc -> clfield -> Astf.ep=
      function
      | _loc ->
          (function
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#clfield _loc _a0),
                             (self#clfield _loc _a1))))))
           | `Inherit (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Inherit")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (self#clexp _loc _a1))))))
           | `InheritAs (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "InheritAs")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#flag _loc _a0),
                             (`Com
                                (_loc, (self#clexp _loc _a1),
                                  (self#alident _loc _a2))))))))
           | `CrVal (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "CrVal")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (`Com
                                     (_loc, (self#flag _loc _a2),
                                       (self#exp _loc _a3))))))))))
           | `VirVal (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "VirVal")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (self#ctyp _loc _a2))))))))
           | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
               `App
                 (_loc, (`Vrn (_loc, "CrMth")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (`Com
                                     (_loc, (self#flag _loc _a2),
                                       (`Com
                                          (_loc, (self#exp _loc _a3),
                                            (self#ctyp _loc _a4))))))))))))
           | `CrMthS (_a0,_a1,_a2,_a3) ->
               `App
                 (_loc, (`Vrn (_loc, "CrMthS")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (`Com
                                     (_loc, (self#flag _loc _a2),
                                       (self#exp _loc _a3))))))))))
           | `VirMeth (_a0,_a1,_a2) ->
               `App
                 (_loc, (`Vrn (_loc, "VirMeth")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#alident _loc _a0),
                             (`Com
                                (_loc, (self#flag _loc _a1),
                                  (self#ctyp _loc _a2))))))))
           | `Eq (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Eq")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ctyp _loc _a0), (self#ctyp _loc _a1))))))
           | `Initializer _a0 ->
               `App (_loc, (`Vrn (_loc, "Initializer")), (self#exp _loc _a0))
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
    method ep : 'loc -> ep -> Astf.ep=
      function
      | _loc ->
          (function
           | #vid as _a0 -> (self#vid _loc _a0 :> Astf.ep)
           | `App (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "App")),
                   (`Par
                      (_loc,
                        (`Com (_loc, (self#ep _loc _a0), (self#ep _loc _a1))))))
           | `Vrn _a0 ->
               `App (_loc, (`Vrn (_loc, "Vrn")), (self#string _loc _a0))
           | `Com (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Com")),
                   (`Par
                      (_loc,
                        (`Com (_loc, (self#ep _loc _a0), (self#ep _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com (_loc, (self#ep _loc _a0), (self#ep _loc _a1))))))
           | `Par _a0 ->
               `App (_loc, (`Vrn (_loc, "Par")), (self#ep _loc _a0))
           | `Constraint (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Constraint")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#ep _loc _a0), (self#ctyp _loc _a1))))))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
           | `Array _a0 ->
               `App (_loc, (`Vrn (_loc, "Array")), (self#ep _loc _a0))
           | `Record _a0 ->
               `App (_loc, (`Vrn (_loc, "Record")), (self#rec_bind _loc _a0))
           | #literal as _a0 -> (self#literal _loc _a0 :> Astf.ep))
    method rec_bind : 'loc -> rec_bind -> Astf.ep=
      function
      | _loc ->
          (function
           | `RecBind (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "RecBind")),
                   (`Par
                      (_loc,
                        (`Com (_loc, (self#vid _loc _a0), (self#ep _loc _a1))))))
           | `Sem (_a0,_a1) ->
               `App
                 (_loc, (`Vrn (_loc, "Sem")),
                   (`Par
                      (_loc,
                        (`Com
                           (_loc, (self#rec_bind _loc _a0),
                             (self#rec_bind _loc _a1))))))
           | #any as _a0 -> (self#any _loc _a0 :> Astf.ep)
           | #ant as _a0 -> (self#ant _loc _a0 :> Astf.ep))
  end
