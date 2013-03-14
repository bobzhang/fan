open Ast
class primitive =
  object 
    method int _loc (i : int) = (`Int (_loc, (string_of_int i)) : ep )
    method int32 _loc (i : int32) =
      (`Int32 (_loc, (Int32.to_string i)) : ep )
    method int64 _loc (i : int64) =
      (`Int64 (_loc, (Int64.to_string i)) : ep )
    method nativeint _loc (i : nativeint) =
      (`NativeInt (_loc, (Nativeint.to_string i)) : ep )
    method float _loc (i : float) =
      (`Flo (_loc, (FanUtil.float_repres i)) : ep )
    method string _loc (i : string) = (`Str (_loc, (String.escaped i)) : ep )
    method char _loc (i : char) = (`Chr (_loc, (Char.escaped i)) : ep )
    method unit _loc (_ : unit) = (`Id (_loc, (`Uid (_loc, "()"))) : ep )
    method bool _loc x =
      (match x with
       | true  -> `Id (_loc, (`Lid (_loc, "true")))
       | false  -> `Id (_loc, (`Lid (_loc, "false"))) : ep )
  end
let _ = (); ()
let _ = ()
class meta =
  object (self : 'self_type)
    inherit  primitive
    method loc : 'loc -> loc -> ep= fun _loc  _a0  -> self#fanloc_t _loc _a0
    method ant : 'loc -> ant -> ep=
      fun _loc  (`Ant (_a0,_a1))  -> `Ant (_a0, _a1)
    method nil : 'loc -> nil -> ep=
      fun _loc  (`Nil _a0)  ->
        `App (_loc, (`Vrn (_loc, "Nil")), (self#loc _loc _a0))
    method literal : 'loc -> literal -> ep=
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
        | `NativeInt (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "NativeInt")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | `Str (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Str")), (self#loc _loc _a0))),
                (self#string _loc _a1))
    method rec_flag : 'loc -> rec_flag -> ep=
      fun _loc  ->
        function
        | `Recursive _a0 ->
            `App (_loc, (`Vrn (_loc, "Recursive")), (self#loc _loc _a0))
        | `ReNil _a0 ->
            `App (_loc, (`Vrn (_loc, "ReNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method direction_flag : 'loc -> direction_flag -> ep=
      fun _loc  ->
        function
        | `To _a0 -> `App (_loc, (`Vrn (_loc, "To")), (self#loc _loc _a0))
        | `Downto _a0 ->
            `App (_loc, (`Vrn (_loc, "Downto")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method mutable_flag : 'loc -> mutable_flag -> ep=
      fun _loc  ->
        function
        | `Mutable _a0 ->
            `App (_loc, (`Vrn (_loc, "Mutable")), (self#loc _loc _a0))
        | `MuNil _a0 ->
            `App (_loc, (`Vrn (_loc, "MuNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method private_flag : 'loc -> private_flag -> ep=
      fun _loc  ->
        function
        | `Private _a0 ->
            `App (_loc, (`Vrn (_loc, "Private")), (self#loc _loc _a0))
        | `PrNil _a0 ->
            `App (_loc, (`Vrn (_loc, "PrNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method virtual_flag : 'loc -> virtual_flag -> ep=
      fun _loc  ->
        function
        | `Virtual _a0 ->
            `App (_loc, (`Vrn (_loc, "Virtual")), (self#loc _loc _a0))
        | `ViNil _a0 ->
            `App (_loc, (`Vrn (_loc, "ViNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method override_flag : 'loc -> override_flag -> ep=
      fun _loc  ->
        function
        | `Override _a0 ->
            `App (_loc, (`Vrn (_loc, "Override")), (self#loc _loc _a0))
        | `OvNil _a0 ->
            `App (_loc, (`Vrn (_loc, "OvNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method row_var_flag : 'loc -> row_var_flag -> ep=
      fun _loc  ->
        function
        | `RowVar _a0 ->
            `App (_loc, (`Vrn (_loc, "RowVar")), (self#loc _loc _a0))
        | `RvNil _a0 ->
            `App (_loc, (`Vrn (_loc, "RvNil")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method position_flag : 'loc -> position_flag -> ep=
      fun _loc  ->
        function
        | `Positive _a0 ->
            `App (_loc, (`Vrn (_loc, "Positive")), (self#loc _loc _a0))
        | `Negative _a0 ->
            `App (_loc, (`Vrn (_loc, "Negative")), (self#loc _loc _a0))
        | `Normal _a0 ->
            `App (_loc, (`Vrn (_loc, "Normal")), (self#loc _loc _a0))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method strings : 'loc -> strings -> ep=
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
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method alident : 'loc -> alident -> ep=
      fun _loc  ->
        function
        | `Lid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method auident : 'loc -> auident -> ep=
      fun _loc  ->
        function
        | `Uid (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Uid")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method aident : 'loc -> aident -> ep=
      fun _loc  ->
        function
        | #alident as _a0 -> (self#alident _loc _a0 :>ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>ep)
    method astring : 'loc -> astring -> ep=
      fun _loc  ->
        function
        | `C (_a0,_a1) ->
            `App
              (_loc, (`App (_loc, (`Vrn (_loc, "C")), (self#loc _loc _a0))),
                (self#string _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method uident : 'loc -> uident -> ep=
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
        | #auident as _a0 -> (self#auident _loc _a0 :>ep)
    method ident : 'loc -> ident -> ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ident _loc _a2))
        | #alident as _a0 -> (self#alident _loc _a0 :>ep)
        | #auident as _a0 -> (self#auident _loc _a0 :>ep)
    method dupath : 'loc -> dupath -> ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#dupath _loc _a1))), (self#dupath _loc _a2))
        | #auident as _a0 -> (self#auident _loc _a0 :>ep)
    method dlpath : 'loc -> dlpath -> ep=
      fun _loc  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#dupath _loc _a1))), (self#alident _loc _a2))
        | #alident as _a0 -> (self#alident _loc _a0 :>ep)
    method sid : 'loc -> sid -> ep=
      fun _loc  (`Id (_a0,_a1))  ->
        `App
          (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (self#loc _loc _a0))),
            (self#ident _loc _a1))
    method any : 'loc -> any -> ep=
      fun _loc  (`Any _a0)  ->
        `App (_loc, (`Vrn (_loc, "Any")), (self#loc _loc _a0))
    method ctyp : 'loc -> ctyp -> ep=
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
        | #any as _a0 -> (self#any _loc _a0 :>ep)
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
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | `TyObj (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyObj")), (self#loc _loc _a0))),
                     (self#name_ctyp _loc _a1))),
                (self#row_var_flag _loc _a2))
        | `TyObjEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "TyObjEnd")), (self#loc _loc _a0))),
                (self#row_var_flag _loc _a1))
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
        | `Tup (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Tup")), (self#loc _loc _a0))),
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
                (self#module_type _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method type_parameters : 'loc -> type_parameters -> ep=
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
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method row_field : 'loc -> row_field -> ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
        | `Or (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Or")), (self#loc _loc _a0))),
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
    method tag_names : 'loc -> tag_names -> ep=
      fun _loc  ->
        function
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
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
    method typedecl : 'loc -> typedecl -> ep=
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
                          (self#list (fun self  -> self#ctyp) _loc _a2))),
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
                     (self#list (fun self  -> self#ctyp) _loc _a2))),
                (self#opt_type_constr _loc _a3))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#typedecl _loc _a1))), (self#typedecl _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method type_constr : 'loc -> type_constr -> ep=
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
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method opt_type_constr : 'loc -> opt_type_constr -> ep=
      fun _loc  ->
        function
        | `Constr (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Constr")), (self#loc _loc _a0))),
                (self#type_constr _loc _a1))
        | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (self#loc _loc _a0))
    method opt_type_params : 'loc -> opt_type_params -> ep=
      fun _loc  (`Nil _a0)  ->
        `App (_loc, (`Vrn (_loc, "Nil")), (self#loc _loc _a0))
    method type_info : 'loc -> type_info -> ep=
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
                     (self#private_flag _loc _a2))),
                (self#type_repr _loc _a3))
        | `TyRepr (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyRepr")), (self#loc _loc _a0))),
                     (self#private_flag _loc _a1))),
                (self#type_repr _loc _a2))
        | `TyEq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "TyEq")), (self#loc _loc _a0))),
                     (self#private_flag _loc _a1))), (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method type_repr : 'loc -> type_repr -> ep=
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
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method name_ctyp : 'loc -> name_ctyp -> ep=
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
                     (self#sid _loc _a1))), (self#ctyp _loc _a2))
        | `TyColMut (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyColMut")),
                          (self#loc _loc _a0))), (self#sid _loc _a1))),
                (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method or_ctyp : 'loc -> or_ctyp -> ep=
      fun _loc  ->
        function
        | `Or (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Or")), (self#loc _loc _a0))),
                     (self#or_ctyp _loc _a1))), (self#or_ctyp _loc _a2))
        | `TyCol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TyCol")), (self#loc _loc _a0))),
                     (self#sid _loc _a1))), (self#ctyp _loc _a2))
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Of")), (self#loc _loc _a0))),
                     (self#sid _loc _a1))), (self#ctyp _loc _a2))
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method of_ctyp : 'loc -> of_ctyp -> ep=
      fun _loc  ->
        function
        | `Of (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Of")), (self#loc _loc _a0))),
                     (self#sid _loc _a1))), (self#ctyp _loc _a2))
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method patt : 'loc -> patt -> ep=
      fun _loc  ->
        function
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#patt _loc _a2))
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
                     (self#patt _loc _a1))), (self#patt _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#patt _loc _a2))
        | `Tup (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Tup")), (self#loc _loc _a0))),
                (self#patt _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#rec_patt _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
        | #literal as _a0 -> (self#literal _loc _a0 :>ep)
        | `Alias (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Alias")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#alident _loc _a2))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Array")), (self#loc _loc _a0))),
                (self#patt _loc _a1))
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
                     (self#alident _loc _a1))), (self#patt _loc _a2))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLabl")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#patt _loc _a2))
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
                          (self#alident _loc _a1))), (self#patt _loc _a2))),
                (self#expr _loc _a3))
        | `Or (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Or")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#patt _loc _a2))
        | `PaRng (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "PaRng")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#patt _loc _a2))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#patt _loc _a1))),
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
                (self#patt _loc _a1))
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
    method rec_patt : 'loc -> rec_patt -> ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#patt _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_patt _loc _a1))), (self#rec_patt _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method expr : 'loc -> expr -> ep=
      fun _loc  ->
        function
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
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
                     (self#expr _loc _a1))), (self#expr _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
        | `Tup (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Tup")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
        | `Record (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Record")), (self#loc _loc _a0))),
                (self#rec_expr _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
        | #literal as _a0 -> (self#literal _loc _a0 :>ep)
        | `RecordWith (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecordWith")),
                          (self#loc _loc _a0))), (self#rec_expr _loc _a1))),
                (self#expr _loc _a2))
        | `Dot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Dot")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
        | `ArrayDot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ArrayDot")),
                          (self#loc _loc _a0))), (self#expr _loc _a1))),
                (self#expr _loc _a2))
        | `ArrayEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "ArrayEmpty")), (self#loc _loc _a0))
        | `Array (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Array")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
        | `ExAsf _a0 ->
            `App (_loc, (`Vrn (_loc, "ExAsf")), (self#loc _loc _a0))
        | `ExAsr (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ExAsr")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
        | `Assign (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Assign")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
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
                               (self#expr _loc _a2))), (self#expr _loc _a3))),
                     (self#direction_flag _loc _a4))), (self#expr _loc _a5))
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
                               (self#loc _loc _a0))), (self#expr _loc _a1))),
                     (self#expr _loc _a2))), (self#expr _loc _a3))
        | `IfThen (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "IfThen")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
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
                     (self#alident _loc _a1))), (self#expr _loc _a2))
        | `Lazy (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Lazy")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetIn")),
                               (self#loc _loc _a0))),
                          (self#rec_flag _loc _a1))),
                     (self#binding _loc _a2))), (self#expr _loc _a3))
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
                          (self#auident _loc _a1))),
                     (self#module_expr _loc _a2))), (self#expr _loc _a3))
        | `Match (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Match")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#case _loc _a2))
        | `New (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "New")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#class_str_item _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjPat")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#class_str_item _loc _a2))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#loc _loc _a0))),
                (self#patt _loc _a1))
        | `OptLabl (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "OptLabl")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#expr _loc _a2))
        | `OptLablS (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "OptLablS")), (self#loc _loc _a0))),
                (self#alident _loc _a1))
        | `OvrInst (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "OvrInst")), (self#loc _loc _a0))),
                (self#rec_expr _loc _a1))
        | `OvrInstEmpty _a0 ->
            `App (_loc, (`Vrn (_loc, "OvrInstEmpty")), (self#loc _loc _a0))
        | `Seq (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Seq")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
        | `Send (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Send")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#alident _loc _a2))
        | `StringDot (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "StringDot")),
                          (self#loc _loc _a0))), (self#expr _loc _a1))),
                (self#expr _loc _a2))
        | `Try (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Try")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#case _loc _a2))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#expr _loc _a1))),
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
                               (self#loc _loc _a0))), (self#expr _loc _a1))),
                     (self#ctyp _loc _a2))), (self#ctyp _loc _a3))
        | `Subtype (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Subtype")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#ctyp _loc _a2))
        | `While (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "While")), (self#loc _loc _a0))),
                     (self#expr _loc _a1))), (self#expr _loc _a2))
        | `LetOpen (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LetOpen")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#expr _loc _a2))
        | `LocalTypeFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "LocalTypeFun")),
                          (self#loc _loc _a0))), (self#alident _loc _a1))),
                (self#expr _loc _a2))
        | `Package_expr (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Package_expr")), (self#loc _loc _a0))),
                (self#module_expr _loc _a1))
    method rec_expr : 'loc -> rec_expr -> ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_expr _loc _a1))), (self#rec_expr _loc _a2))
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#expr _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method module_type : 'loc -> module_type -> ep=
      fun _loc  ->
        function
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
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
                          (self#auident _loc _a1))),
                     (self#module_type _loc _a2))),
                (self#module_type _loc _a3))
        | `Sig (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Sig")), (self#loc _loc _a0))),
                (self#sig_item _loc _a1))
        | `SigEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "SigEnd")), (self#loc _loc _a0))
        | `With (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "With")), (self#loc _loc _a0))),
                     (self#module_type _loc _a1))),
                (self#with_constr _loc _a2))
        | `ModuleTypeOf (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleTypeOf")), (self#loc _loc _a0))),
                (self#module_expr _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method sig_item : 'loc -> sig_item -> ep=
      fun _loc  ->
        function
        | `Class (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Class")), (self#loc _loc _a0))),
                (self#class_type _loc _a1))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassType")), (self#loc _loc _a0))),
                (self#class_type _loc _a1))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#sig_item _loc _a1))), (self#sig_item _loc _a2))
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
                (self#expr _loc _a2))
        | `Exception (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Exception")), (self#loc _loc _a0))),
                (self#of_ctyp _loc _a1))
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
                (self#module_type _loc _a1))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Module")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#module_type _loc _a2))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecModule")), (self#loc _loc _a0))),
                (self#module_binding _loc _a1))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleType")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#module_type _loc _a2))
        | `ModuleTypeEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "ModuleTypeEnd")),
                     (self#loc _loc _a0))), (self#auident _loc _a1))
        | `Open (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Open")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Type (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Type")), (self#loc _loc _a0))),
                (self#typedecl _loc _a1))
        | `Val (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Val")), (self#loc _loc _a0))),
                     (self#alident _loc _a1))), (self#ctyp _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method with_constr : 'loc -> with_constr -> ep=
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
        | `TypeEqPriv (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "TypeEqPriv")),
                          (self#loc _loc _a0))), (self#ctyp _loc _a1))),
                (self#ctyp _loc _a2))
        | `ModuleEq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleEq")),
                          (self#loc _loc _a0))), (self#ident _loc _a1))),
                (self#ident _loc _a2))
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
                     (self#with_constr _loc _a1))),
                (self#with_constr _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method binding : 'loc -> binding -> ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#binding _loc _a1))), (self#binding _loc _a2))
        | `Bind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Bind")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#expr _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method module_binding : 'loc -> module_binding -> ep=
      fun _loc  ->
        function
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#module_binding _loc _a1))),
                (self#module_binding _loc _a2))
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
                          (self#auident _loc _a1))),
                     (self#module_type _loc _a2))),
                (self#module_expr _loc _a3))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#module_type _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method case : 'loc -> case -> ep=
      fun _loc  ->
        function
        | `Or (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Or")), (self#loc _loc _a0))),
                     (self#case _loc _a1))), (self#case _loc _a2))
        | `Case (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Case")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#expr _loc _a2))
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CaseWhen")),
                               (self#loc _loc _a0))), (self#patt _loc _a1))),
                     (self#expr _loc _a2))), (self#expr _loc _a3))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method module_expr : 'loc -> module_expr -> ep=
      fun _loc  ->
        function
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
        | `App (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "App")), (self#loc _loc _a0))),
                     (self#module_expr _loc _a1))),
                (self#module_expr _loc _a2))
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
                          (self#auident _loc _a1))),
                     (self#module_type _loc _a2))),
                (self#module_expr _loc _a3))
        | `Struct (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Struct")), (self#loc _loc _a0))),
                (self#str_item _loc _a1))
        | `StructEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "StructEnd")), (self#loc _loc _a0))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#module_expr _loc _a1))),
                (self#module_type _loc _a2))
        | `PackageModule (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "PackageModule")),
                     (self#loc _loc _a0))), (self#expr _loc _a1))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method str_item : 'loc -> str_item -> ep=
      fun _loc  ->
        function
        | `Class (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Class")), (self#loc _loc _a0))),
                (self#class_expr _loc _a1))
        | `ClassType (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ClassType")), (self#loc _loc _a0))),
                (self#class_type _loc _a1))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#str_item _loc _a1))), (self#str_item _loc _a2))
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
                (self#expr _loc _a2))
        | `Exception (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Exception")), (self#loc _loc _a0))),
                (self#of_ctyp _loc _a1))
        | `StExp (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "StExp")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
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
                (self#module_expr _loc _a1))
        | `Module (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Module")), (self#loc _loc _a0))),
                     (self#auident _loc _a1))), (self#module_expr _loc _a2))
        | `RecModule (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "RecModule")), (self#loc _loc _a0))),
                (self#module_binding _loc _a1))
        | `ModuleType (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ModuleType")),
                          (self#loc _loc _a0))), (self#auident _loc _a1))),
                (self#module_type _loc _a2))
        | `Open (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Open")), (self#loc _loc _a0))),
                (self#ident _loc _a1))
        | `Type (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Type")), (self#loc _loc _a0))),
                (self#typedecl _loc _a1))
        | `Value (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Value")), (self#loc _loc _a0))),
                     (self#rec_flag _loc _a1))), (self#binding _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method class_type : 'loc -> class_type -> ep=
      fun _loc  ->
        function
        | `ClassCon (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "ClassCon")),
                               (self#loc _loc _a0))),
                          (self#virtual_flag _loc _a1))),
                     (self#ident _loc _a2))),
                (self#type_parameters _loc _a3))
        | `ClassConS (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ClassConS")),
                          (self#loc _loc _a0))),
                     (self#virtual_flag _loc _a1))), (self#ident _loc _a2))
        | `CtFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CtFun")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#class_type _loc _a2))
        | `ObjTy (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjTy")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#class_sig_item _loc _a2))
        | `ObjTyEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjTyEnd")), (self#loc _loc _a0))),
                (self#ctyp _loc _a1))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#class_sig_item _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#class_type _loc _a1))),
                (self#class_type _loc _a2))
        | `CtCol (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CtCol")), (self#loc _loc _a0))),
                     (self#class_type _loc _a1))),
                (self#class_type _loc _a2))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#class_type _loc _a1))),
                (self#class_type _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method class_sig_item : 'loc -> class_sig_item -> ep=
      fun _loc  ->
        function
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#class_sig_item _loc _a1))),
                (self#class_sig_item _loc _a2))
        | `SigInherit (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "SigInherit")), (self#loc _loc _a0))),
                (self#class_type _loc _a1))
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
                          (self#alident _loc _a1))),
                     (self#private_flag _loc _a2))), (self#ctyp _loc _a3))
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
                          (self#mutable_flag _loc _a2))),
                     (self#virtual_flag _loc _a3))), (self#ctyp _loc _a4))
        | `CgVir (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CgVir")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))),
                     (self#private_flag _loc _a2))), (self#ctyp _loc _a3))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method class_expr : 'loc -> class_expr -> ep=
      fun _loc  ->
        function
        | `CeApp (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CeApp")), (self#loc _loc _a0))),
                     (self#class_expr _loc _a1))), (self#expr _loc _a2))
        | `ClassCon (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "ClassCon")),
                               (self#loc _loc _a0))),
                          (self#virtual_flag _loc _a1))),
                     (self#ident _loc _a2))),
                (self#type_parameters _loc _a3))
        | `ClassConS (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ClassConS")),
                          (self#loc _loc _a0))),
                     (self#virtual_flag _loc _a1))), (self#ident _loc _a2))
        | `CeFun (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "CeFun")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#class_expr _loc _a2))
        | `LetIn (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "LetIn")),
                               (self#loc _loc _a0))),
                          (self#rec_flag _loc _a1))),
                     (self#binding _loc _a2))), (self#class_expr _loc _a3))
        | `Obj (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Obj")), (self#loc _loc _a0))),
                (self#class_str_item _loc _a1))
        | `ObjEnd _a0 ->
            `App (_loc, (`Vrn (_loc, "ObjEnd")), (self#loc _loc _a0))
        | `ObjPat (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "ObjPat")), (self#loc _loc _a0))),
                     (self#patt _loc _a1))), (self#class_str_item _loc _a2))
        | `ObjPatEnd (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "ObjPatEnd")), (self#loc _loc _a0))),
                (self#patt _loc _a1))
        | `Constraint (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Constraint")),
                          (self#loc _loc _a0))), (self#class_expr _loc _a1))),
                (self#class_type _loc _a2))
        | `And (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "And")), (self#loc _loc _a0))),
                     (self#class_expr _loc _a1))),
                (self#class_expr _loc _a2))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#class_expr _loc _a1))),
                (self#class_expr _loc _a2))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method class_str_item : 'loc -> class_str_item -> ep=
      fun _loc  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#class_str_item _loc _a1))),
                (self#class_str_item _loc _a2))
        | `Eq (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Eq")), (self#loc _loc _a0))),
                     (self#ctyp _loc _a1))), (self#ctyp _loc _a2))
        | `Inherit (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Inherit")), (self#loc _loc _a0))),
                     (self#override_flag _loc _a1))),
                (self#class_expr _loc _a2))
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "InheritAs")),
                               (self#loc _loc _a0))),
                          (self#override_flag _loc _a1))),
                     (self#class_expr _loc _a2))), (self#alident _loc _a3))
        | `Initializer (_a0,_a1) ->
            `App
              (_loc,
                (`App
                   (_loc, (`Vrn (_loc, "Initializer")), (self#loc _loc _a0))),
                (self#expr _loc _a1))
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
                               (self#override_flag _loc _a2))),
                          (self#private_flag _loc _a3))),
                     (self#expr _loc _a4))), (self#ctyp _loc _a5))
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
                          (self#override_flag _loc _a2))),
                     (self#private_flag _loc _a3))), (self#expr _loc _a4))
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
                          (self#override_flag _loc _a2))),
                     (self#mutable_flag _loc _a3))), (self#expr _loc _a4))
        | `CrVir (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CrVir")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))),
                     (self#private_flag _loc _a2))), (self#ctyp _loc _a3))
        | `CrVvr (_a0,_a1,_a2,_a3) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "CrVvr")),
                               (self#loc _loc _a0))),
                          (self#alident _loc _a1))),
                     (self#mutable_flag _loc _a2))), (self#ctyp _loc _a3))
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method ep : 'loc -> ep -> ep=
      fun _loc  ->
        function
        | #sid as _a0 -> (self#sid _loc _a0 :>ep)
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
        | `Tup (_a0,_a1) ->
            `App
              (_loc,
                (`App (_loc, (`Vrn (_loc, "Tup")), (self#loc _loc _a0))),
                (self#ep _loc _a1))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
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
        | #literal as _a0 -> (self#literal _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method rec_bind : 'loc -> rec_bind -> ep=
      fun _loc  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "RecBind")), (self#loc _loc _a0))),
                     (self#ident _loc _a1))), (self#ep _loc _a2))
        | `Sem (_a0,_a1,_a2) ->
            `App
              (_loc,
                (`App
                   (_loc,
                     (`App (_loc, (`Vrn (_loc, "Sem")), (self#loc _loc _a0))),
                     (self#rec_bind _loc _a1))), (self#rec_bind _loc _a2))
        | #any as _a0 -> (self#any _loc _a0 :>ep)
        | #ant as _a0 -> (self#ant _loc _a0 :>ep)
    method fanloc_t : 'loc -> FanLoc.t -> ep= self#unknown
    method fanutil_anti_cxt : 'loc -> FanUtil.anti_cxt -> ep= self#unknown
  end