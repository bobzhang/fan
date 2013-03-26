open LibUtil
open StdLib
open Ast
let strip_loc_list f lst = List.map f lst
let strip_loc_ant ant = ant
let _ = (); ()
let _ = ()
class map2 =
  object (self : 'self_type)
    inherit  mapbase2
    method loc : loc -> loc -> loc= fun _a0  _a1  -> self#fanloc_t _a0 _a1
    method ant : ant -> ant -> ant=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#fanutil_anti_cxt _a1 _b1 in `Ant (_a0, _a1)
    method nil : nil -> nil -> nil=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> let _a0 = self#loc _a0 _b0 in `Nil _a0
    method literal : literal -> literal -> literal=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Chr (_a0,_a1),`Chr (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Chr (_a0, _a1)
        | (`Int (_a0,_a1),`Int (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Int (_a0, _a1)
        | (`Int32 (_a0,_a1),`Int32 (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Int32 (_a0, _a1)
        | (`Int64 (_a0,_a1),`Int64 (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Int64 (_a0, _a1)
        | (`Flo (_a0,_a1),`Flo (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Flo (_a0, _a1)
        | (`NativeInt (_a0,_a1),`NativeInt (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `NativeInt (_a0, _a1)
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Str (_a0, _a1)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_flag : rec_flag -> rec_flag -> rec_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive _a0,`Recursive _b0) ->
            let _a0 = self#loc _a0 _b0 in `Recursive _a0
        | (`ReNil _a0,`ReNil _b0) -> let _a0 = self#loc _a0 _b0 in `ReNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method direction_flag :
      direction_flag -> direction_flag -> direction_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To _a0,`To _b0) -> let _a0 = self#loc _a0 _b0 in `To _a0
        | (`Downto _a0,`Downto _b0) ->
            let _a0 = self#loc _a0 _b0 in `Downto _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>direction_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method mutable_flag : mutable_flag -> mutable_flag -> mutable_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable _a0,`Mutable _b0) ->
            let _a0 = self#loc _a0 _b0 in `Mutable _a0
        | (`MuNil _a0,`MuNil _b0) -> let _a0 = self#loc _a0 _b0 in `MuNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>mutable_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method private_flag : private_flag -> private_flag -> private_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private _a0,`Private _b0) ->
            let _a0 = self#loc _a0 _b0 in `Private _a0
        | (`PrNil _a0,`PrNil _b0) -> let _a0 = self#loc _a0 _b0 in `PrNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>private_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method virtual_flag : virtual_flag -> virtual_flag -> virtual_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual _a0,`Virtual _b0) ->
            let _a0 = self#loc _a0 _b0 in `Virtual _a0
        | (`ViNil _a0,`ViNil _b0) -> let _a0 = self#loc _a0 _b0 in `ViNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>virtual_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method override_flag : override_flag -> override_flag -> override_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override _a0,`Override _b0) ->
            let _a0 = self#loc _a0 _b0 in `Override _a0
        | (`OvNil _a0,`OvNil _b0) -> let _a0 = self#loc _a0 _b0 in `OvNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>override_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method row_var_flag : row_var_flag -> row_var_flag -> row_var_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar _a0,`RowVar _b0) ->
            let _a0 = self#loc _a0 _b0 in `RowVar _a0
        | (`RvNil _a0,`RvNil _b0) -> let _a0 = self#loc _a0 _b0 in `RvNil _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>row_var_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method position_flag : position_flag -> position_flag -> position_flag=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive _a0,`Positive _b0) ->
            let _a0 = self#loc _a0 _b0 in `Positive _a0
        | (`Negative _a0,`Negative _b0) ->
            let _a0 = self#loc _a0 _b0 in `Negative _a0
        | (`Normal _a0,`Normal _b0) ->
            let _a0 = self#loc _a0 _b0 in `Normal _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>position_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method strings : strings -> strings -> strings=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#strings _a1 _b1 in
            let _a2 = self#strings _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Str (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>strings)
        | (_,_) -> invalid_arg "map2 failure"
    method alident : alident -> alident -> alident=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Lid (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>alident)
        | (_,_) -> invalid_arg "map2 failure"
    method auident : auident -> auident -> auident=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Uid (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>auident)
        | (_,_) -> invalid_arg "map2 failure"
    method aident : aident -> aident -> aident=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 : alident  :>aident)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 : auident  :>aident)
        | (_,_) -> invalid_arg "map2 failure"
    method astring : astring -> astring -> astring=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C (_a0,_a1),`C (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `C (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>astring)
        | (_,_) -> invalid_arg "map2 failure"
    method uident : uident -> uident -> uident=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#uident _a1 _b1 in
            let _a2 = self#uident _a2 _b2 in `Dot (_a0, _a1, _a2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#uident _a1 _b1 in
            let _a2 = self#uident _a2 _b2 in `App (_a0, _a1, _a2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 : auident  :>uident)
        | (_,_) -> invalid_arg "map2 failure"
    method ident : ident -> ident -> ident=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `Dot (_a0, _a1, _a2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `App (_a0, _a1, _a2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 : alident  :>ident)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 : auident  :>ident)
        | (_,_) -> invalid_arg "map2 failure"
    method dupath : dupath -> dupath -> dupath=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#dupath _a1 _b1 in
            let _a2 = self#dupath _a2 _b2 in `Dot (_a0, _a1, _a2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 : auident  :>dupath)
        | (_,_) -> invalid_arg "map2 failure"
    method dlpath : dlpath -> dlpath -> dlpath=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#dupath _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Dot (_a0, _a1, _a2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 : alident  :>dlpath)
        | (_,_) -> invalid_arg "map2 failure"
    method any : any -> any -> any=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Any _a0,`Any _b0) -> let _a0 = self#loc _a0 _b0 in `Any _a0
    method sid : sid -> sid -> sid=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `Id (_a0, _a1)
    method ctyp : ctyp -> ctyp -> ctyp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Alias (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>ctyp)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Arrow (_a0,_a1,_a2),`Arrow (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Arrow (_a0, _a1, _a2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `ClassPath (_a0, _a1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Label (_a0, _a1, _a2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `OptLabl (_a0, _a1, _a2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>ctyp)
        | (`TyObj (_a0,_a1,_a2),`TyObj (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#name_ctyp _a1 _b1 in
            let _a2 = self#row_var_flag _a2 _b2 in `TyObj (_a0, _a1, _a2)
        | (`TyObjEnd (_a0,_a1),`TyObjEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_var_flag _a1 _b1 in `TyObjEnd (_a0, _a1)
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyPol (_a0, _a1, _a2)
        | (`TyPolEnd (_a0,_a1),`TyPolEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in `TyPolEnd (_a0, _a1)
        | (`TyTypePol (_a0,_a1,_a2),`TyTypePol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyTypePol (_a0, _a1, _a2)
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Quote (_a0, _a1, _a2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in `QuoteAny (_a0, _a1)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in `Tup (_a0, _a1)
        | (`Sta (_a0,_a1,_a2),`Sta (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Sta (_a0, _a1, _a2)
        | (`PolyEq (_a0,_a1),`PolyEq (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_field _a1 _b1 in `PolyEq (_a0, _a1)
        | (`PolySup (_a0,_a1),`PolySup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_field _a1 _b1 in `PolySup (_a0, _a1)
        | (`PolyInf (_a0,_a1),`PolyInf (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_field _a1 _b1 in `PolyInf (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`PolyInfSup (_a0,_a1,_a2),`PolyInfSup (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_field _a1 _b1 in
            let _a2 = self#tag_names _a2 _b2 in `PolyInfSup (_a0, _a1, _a2)
        | (`Package (_a0,_a1),`Package (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_type _a1 _b1 in `Package (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method type_parameters :
      type_parameters -> type_parameters -> type_parameters=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#type_parameters _a1 _b1 in
            let _a2 = self#type_parameters _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in `Ctyp (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>type_parameters)
        | (_,_) -> invalid_arg "map2 failure"
    method row_field : row_field -> row_field -> row_field=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>row_field)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#row_field _a1 _b1 in
            let _a2 = self#row_field _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#astring _a1 _b1 in `TyVrn (_a0, _a1)
        | (`TyVrnOf (_a0,_a1,_a2),`TyVrnOf (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#astring _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyVrnOf (_a0, _a1, _a2)
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in `Ctyp (_a0, _a1)
        | (_,_) -> invalid_arg "map2 failure"
    method tag_names : tag_names -> tag_names -> tag_names=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>tag_names)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#tag_names _a1 _b1 in
            let _a2 = self#tag_names _a2 _b2 in `App (_a0, _a1, _a2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#astring _a1 _b1 in `TyVrn (_a0, _a1)
        | (_,_) -> invalid_arg "map2 failure"
    method typedecl : typedecl -> typedecl -> typedecl=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyDcl (_a0,_a1,_a2,_a3,_a4),`TyDcl (_b0,_b1,_b2,_b3,_b4)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#opt_decl_params _a2 _b2 in
            let _a3 = self#type_info _a3 _b3 in
            let _a4 = self#opt_type_constr _a4 _b4 in
            `TyDcl (_a0, _a1, _a2, _a3, _a4)
        | (`TyAbstr (_a0,_a1,_a2,_a3),`TyAbstr (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#opt_decl_params _a2 _b2 in
            let _a3 = self#opt_type_constr _a3 _b3 in
            `TyAbstr (_a0, _a1, _a2, _a3)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#typedecl _a1 _b1 in
            let _a2 = self#typedecl _a2 _b2 in `And (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>typedecl)
        | (_,_) -> invalid_arg "map2 failure"
    method type_constr : type_constr -> type_constr -> type_constr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#type_constr _a1 _b1 in
            let _a2 = self#type_constr _a2 _b2 in `And (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Eq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>type_constr)
        | (_,_) -> invalid_arg "map2 failure"
    method opt_type_constr :
      opt_type_constr -> opt_type_constr -> opt_type_constr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#type_constr _a1 _b1 in `Some (_a0, _a1)
        | (`None _a0,`None _b0) -> let _a0 = self#loc _a0 _b0 in `None _a0
        | (_,_) -> invalid_arg "map2 failure"
    method decl_param : decl_param -> decl_param -> decl_param=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Quote (_a0, _a1, _a2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in `QuoteAny (_a0, _a1)
        | (`Any _a0,`Any _b0) -> let _a0 = self#loc _a0 _b0 in `Any _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>decl_param)
        | (_,_) -> invalid_arg "map2 failure"
    method decl_params : decl_params -> decl_params -> decl_params=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Quote (_a0, _a1, _a2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#position_flag _a1 _b1 in `QuoteAny (_a0, _a1)
        | (`Any _a0,`Any _b0) -> let _a0 = self#loc _a0 _b0 in `Any _a0
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#decl_params _a1 _b1 in
            let _a2 = self#decl_params _a2 _b2 in `Com (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>decl_params)
        | (_,_) -> invalid_arg "map2 failure"
    method opt_decl_params :
      opt_decl_params -> opt_decl_params -> opt_decl_params=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#decl_params _a1 _b1 in `Some (_a0, _a1)
        | (`None _a0,`None _b0) -> let _a0 = self#loc _a0 _b0 in `None _a0
        | (_,_) -> invalid_arg "map2 failure"
    method type_info : type_info -> type_info -> type_info=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyMan (_a0,_a1,_a2,_a3),`TyMan (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#private_flag _a2 _b2 in
            let _a3 = self#type_repr _a3 _b3 in `TyMan (_a0, _a1, _a2, _a3)
        | (`TyRepr (_a0,_a1,_a2),`TyRepr (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#private_flag _a1 _b1 in
            let _a2 = self#type_repr _a2 _b2 in `TyRepr (_a0, _a1, _a2)
        | (`TyEq (_a0,_a1,_a2),`TyEq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#private_flag _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyEq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>type_info)
        | (_,_) -> invalid_arg "map2 failure"
    method type_repr : type_repr -> type_repr -> type_repr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#name_ctyp _a1 _b1 in `Record (_a0, _a1)
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#or_ctyp _a1 _b1 in `Sum (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>type_repr)
        | (_,_) -> invalid_arg "map2 failure"
    method name_ctyp : name_ctyp -> name_ctyp -> name_ctyp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#name_ctyp _a1 _b1 in
            let _a2 = self#name_ctyp _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sid _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyCol (_a0, _a1, _a2)
        | (`TyColMut (_a0,_a1,_a2),`TyColMut (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sid _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyColMut (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>name_ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method or_ctyp : or_ctyp -> or_ctyp -> or_ctyp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#or_ctyp _a1 _b1 in
            let _a2 = self#or_ctyp _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sid _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyCol (_a0, _a1, _a2)
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sid _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Of (_a0, _a1, _a2)
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>or_ctyp)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>or_ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method of_ctyp : of_ctyp -> of_ctyp -> of_ctyp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sid _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Of (_a0, _a1, _a2)
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>of_ctyp)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>of_ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method pat : pat -> pat -> pat=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>pat)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Vrn (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in `Tup (_a0, _a1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>pat)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_pat _a1 _b1 in `Record (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>pat)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 : literal  :>pat)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Alias (_a0, _a1, _a2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `ArrayEmpty _a0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in `Array (_a0, _a1)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `LabelS (_a0, _a1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `Label (_a0, _a1, _a2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `OptLabl (_a0, _a1, _a2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `OptLablS (_a0, _a1)
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in `OptLablExpr (_a0, _a1, _a2, _a3)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `PaRng (_a0, _a1, _a2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `ClassPath (_a0, _a1)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in `Lazy (_a0, _a1)
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in `ModuleUnpack (_a0, _a1)
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `ModuleConstraint (_a0, _a1, _a2)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_pat : rec_pat -> rec_pat -> rec_pat=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#pat _a2 _b2 in `RecBind (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_pat _a1 _b1 in
            let _a2 = self#rec_pat _a2 _b2 in `Sem (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) ->
            (self#any _a0 _b0 : any  :>rec_pat)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_pat)
        | (_,_) -> invalid_arg "map2 failure"
    method exp : exp -> exp -> exp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>exp)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Vrn (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `Tup (_a0, _a1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>exp)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_exp _a1 _b1 in `Record (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>exp)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 : literal  :>exp)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `RecordWith (_a0, _a1, _a2)
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Dot (_a0, _a1, _a2)
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `ArrayDot (_a0, _a1, _a2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `ArrayEmpty _a0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `Array (_a0, _a1)
        | (`ExAsf _a0,`ExAsf _b0) -> let _a0 = self#loc _a0 _b0 in `ExAsf _a0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `ExAsr (_a0, _a1)
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Assign (_a0, _a1, _a2)
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in
            let _a4 = self#direction_flag _a4 _b4 in
            let _a5 = self#exp _a5 _b5 in `For (_a0, _a1, _a2, _a3, _a4, _a5)
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#case _a1 _b1 in `Fun (_a0, _a1)
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in `IfThenElse (_a0, _a1, _a2, _a3)
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `IfThen (_a0, _a1, _a2)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `LabelS (_a0, _a1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Label (_a0, _a1, _a2)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `Lazy (_a0, _a1)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_flag _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in `LetIn (_a0, _a1, _a2, _a3)
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_exp _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in `LetModule (_a0, _a1, _a2, _a3)
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#case _a2 _b2 in `Match (_a0, _a1, _a2)
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `New (_a0, _a1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#cstru _a1 _b1 in `Obj (_a0, _a1)
        | (`ObjEnd _a0,`ObjEnd _b0) ->
            let _a0 = self#loc _a0 _b0 in `ObjEnd _a0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#cstru _a2 _b2 in `ObjPat (_a0, _a1, _a2)
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in `ObjPatEnd (_a0, _a1)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `OptLabl (_a0, _a1, _a2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `OptLablS (_a0, _a1)
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_exp _a1 _b1 in `OvrInst (_a0, _a1)
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `OvrInstEmpty _a0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `Seq (_a0, _a1)
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Send (_a0, _a1, _a2)
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `StringDot (_a0, _a1, _a2)
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#case _a2 _b2 in `Try (_a0, _a1, _a2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `Coercion (_a0, _a1, _a2, _a3)
        | (`Subtype (_a0,_a1,_a2),`Subtype (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Subtype (_a0, _a1, _a2)
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `While (_a0, _a1, _a2)
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `LetOpen (_a0, _a1, _a2)
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `LocalTypeFun (_a0, _a1, _a2)
        | (`Package_exp (_a0,_a1),`Package_exp (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_exp _a1 _b1 in `Package_exp (_a0, _a1)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_exp : rec_exp -> rec_exp -> rec_exp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_exp _a1 _b1 in
            let _a2 = self#rec_exp _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `RecBind (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) ->
            (self#any _a0 _b0 : any  :>rec_exp)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_exp)
        | (_,_) -> invalid_arg "map2 failure"
    method module_type : module_type -> module_type -> module_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>module_type)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_type _a3 _b3 in
            `Functor (_a0, _a1, _a2, _a3)
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sig_item _a1 _b1 in `Sig (_a0, _a1)
        | (`SigEnd _a0,`SigEnd _b0) ->
            let _a0 = self#loc _a0 _b0 in `SigEnd _a0
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_type _a1 _b1 in
            let _a2 = self#with_constr _a2 _b2 in `With (_a0, _a1, _a2)
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_exp _a1 _b1 in `ModuleTypeOf (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_type)
        | (_,_) -> invalid_arg "map2 failure"
    method sig_item : sig_item -> sig_item -> sig_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in `Class (_a0, _a1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in `ClassType (_a0, _a1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sig_item _a1 _b1 in
            let _a2 = self#sig_item _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `DirectiveSimple (_a0, _a1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Directive (_a0, _a1, _a2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#of_ctyp _a1 _b1 in `Exception (_a0, _a1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in
            let _a3 = self#strings _a3 _b3 in `External (_a0, _a1, _a2, _a3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_type _a1 _b1 in `Include (_a0, _a1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `Module (_a0, _a1, _a2)
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_binding _a1 _b1 in `RecModule (_a0, _a1)
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `ModuleType (_a0, _a1, _a2)
        | (`ModuleTypeEnd (_a0,_a1),`ModuleTypeEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in `ModuleTypeEnd (_a0, _a1)
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `Open (_a0, _a1)
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#typedecl _a1 _b1 in `Type (_a0, _a1)
        | (`Val (_a0,_a1,_a2),`Val (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Val (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>sig_item)
        | (_,_) -> invalid_arg "map2 failure"
    method with_constr : with_constr -> with_constr -> with_constr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TypeEq (_a0,_a1,_a2),`TypeEq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TypeEq (_a0, _a1, _a2)
        | (`TypeEqPriv (_a0,_a1,_a2),`TypeEqPriv (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TypeEqPriv (_a0, _a1, _a2)
        | (`ModuleEq (_a0,_a1,_a2),`ModuleEq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `ModuleEq (_a0, _a1, _a2)
        | (`TypeSubst (_a0,_a1,_a2),`TypeSubst (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TypeSubst (_a0, _a1, _a2)
        | (`ModuleSubst (_a0,_a1,_a2),`ModuleSubst (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `ModuleSubst (_a0, _a1, _a2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#with_constr _a1 _b1 in
            let _a2 = self#with_constr _a2 _b2 in `And (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>with_constr)
        | (_,_) -> invalid_arg "map2 failure"
    method binding : binding -> binding -> binding=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#binding _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in `And (_a0, _a1, _a2)
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Bind (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>binding)
        | (_,_) -> invalid_arg "map2 failure"
    method module_binding :
      module_binding -> module_binding -> module_binding=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_binding _a1 _b1 in
            let _a2 = self#module_binding _a2 _b2 in `And (_a0, _a1, _a2)
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_exp _a3 _b3 in
            `ModuleBind (_a0, _a1, _a2, _a3)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_binding)
        | (_,_) -> invalid_arg "map2 failure"
    method case : case -> case -> case=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#case _a1 _b1 in
            let _a2 = self#case _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Case (_a0, _a1, _a2)
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in
            let _a3 = self#exp _a3 _b3 in `CaseWhen (_a0, _a1, _a2, _a3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>case)
        | (_,_) -> invalid_arg "map2 failure"
    method module_exp : module_exp -> module_exp -> module_exp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>module_exp)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_exp _a1 _b1 in
            let _a2 = self#module_exp _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_exp _a3 _b3 in
            `Functor (_a0, _a1, _a2, _a3)
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#stru _a1 _b1 in `Struct (_a0, _a1)
        | (`StructEnd _a0,`StructEnd _b0) ->
            let _a0 = self#loc _a0 _b0 in `StructEnd _a0
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_exp _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `PackageModule (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_exp)
        | (_,_) -> invalid_arg "map2 failure"
    method stru : stru -> stru -> stru=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_exp _a1 _b1 in `Class (_a0, _a1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in `ClassType (_a0, _a1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#stru _a1 _b1 in
            let _a2 = self#stru _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `DirectiveSimple (_a0, _a1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `Directive (_a0, _a1, _a2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#of_ctyp _a1 _b1 in `Exception (_a0, _a1)
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `StExp (_a0, _a1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in
            let _a3 = self#strings _a3 _b3 in `External (_a0, _a1, _a2, _a3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_exp _a1 _b1 in `Include (_a0, _a1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_exp _a2 _b2 in `Module (_a0, _a1, _a2)
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_binding _a1 _b1 in `RecModule (_a0, _a1)
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `ModuleType (_a0, _a1, _a2)
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `Open (_a0, _a1)
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#typedecl _a1 _b1 in `Type (_a0, _a1)
        | (`Value (_a0,_a1,_a2),`Value (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_flag _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in `Value (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>stru)
        | (_,_) -> invalid_arg "map2 failure"
    method class_type : class_type -> class_type -> class_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in
            let _a3 = self#type_parameters _a3 _b3 in
            `ClassCon (_a0, _a1, _a2, _a3)
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `ClassConS (_a0, _a1, _a2)
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CtFun (_a0, _a1, _a2)
        | (`ObjTy (_a0,_a1,_a2),`ObjTy (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#class_sig_item _a2 _b2 in `ObjTy (_a0, _a1, _a2)
        | (`ObjTyEnd (_a0,_a1),`ObjTyEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in `ObjTyEnd (_a0, _a1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_sig_item _a1 _b1 in `Obj (_a0, _a1)
        | (`ObjEnd _a0,`ObjEnd _b0) ->
            let _a0 = self#loc _a0 _b0 in `ObjEnd _a0
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `And (_a0, _a1, _a2)
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CtCol (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `Eq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_type)
        | (_,_) -> invalid_arg "map2 failure"
    method class_sig_item :
      class_sig_item -> class_sig_item -> class_sig_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Eq (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_sig_item _a1 _b1 in
            let _a2 = self#class_sig_item _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`SigInherit (_a0,_a1),`SigInherit (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in `SigInherit (_a0, _a1)
        | (`Method (_a0,_a1,_a2,_a3),`Method (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#private_flag _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `Method (_a0, _a1, _a2, _a3)
        | (`CgVal (_a0,_a1,_a2,_a3,_a4),`CgVal (_b0,_b1,_b2,_b3,_b4)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#mutable_flag _a2 _b2 in
            let _a3 = self#virtual_flag _a3 _b3 in
            let _a4 = self#ctyp _a4 _b4 in `CgVal (_a0, _a1, _a2, _a3, _a4)
        | (`CgVir (_a0,_a1,_a2,_a3),`CgVir (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#private_flag _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `CgVir (_a0, _a1, _a2, _a3)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_sig_item)
        | (_,_) -> invalid_arg "map2 failure"
    method class_exp : class_exp -> class_exp -> class_exp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_exp _a1 _b1 in
            let _a2 = self#exp _a2 _b2 in `CeApp (_a0, _a1, _a2)
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in
            let _a3 = self#type_parameters _a3 _b3 in
            `ClassCon (_a0, _a1, _a2, _a3)
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in `ClassConS (_a0, _a1, _a2)
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#class_exp _a2 _b2 in `CeFun (_a0, _a1, _a2)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_flag _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in
            let _a3 = self#class_exp _a3 _b3 in `LetIn (_a0, _a1, _a2, _a3)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#cstru _a1 _b1 in `Obj (_a0, _a1)
        | (`ObjEnd _a0,`ObjEnd _b0) ->
            let _a0 = self#loc _a0 _b0 in `ObjEnd _a0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in
            let _a2 = self#cstru _a2 _b2 in `ObjPat (_a0, _a1, _a2)
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#pat _a1 _b1 in `ObjPatEnd (_a0, _a1)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_exp _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_exp _a1 _b1 in
            let _a2 = self#class_exp _a2 _b2 in `And (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_exp _a1 _b1 in
            let _a2 = self#class_exp _a2 _b2 in `Eq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_exp)
        | (_,_) -> invalid_arg "map2 failure"
    method cstru : cstru -> cstru -> cstru=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#cstru _a1 _b1 in
            let _a2 = self#cstru _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Eq (_a0, _a1, _a2)
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#override_flag _a1 _b1 in
            let _a2 = self#class_exp _a2 _b2 in `Inherit (_a0, _a1, _a2)
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#override_flag _a1 _b1 in
            let _a2 = self#class_exp _a2 _b2 in
            let _a3 = self#alident _a3 _b3 in `InheritAs (_a0, _a1, _a2, _a3)
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#exp _a1 _b1 in `Initializer (_a0, _a1)
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#override_flag _a2 _b2 in
            let _a3 = self#private_flag _a3 _b3 in
            let _a4 = self#exp _a4 _b4 in
            let _a5 = self#ctyp _a5 _b5 in
            `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
        | (`CrMthS (_a0,_a1,_a2,_a3,_a4),`CrMthS (_b0,_b1,_b2,_b3,_b4)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#override_flag _a2 _b2 in
            let _a3 = self#private_flag _a3 _b3 in
            let _a4 = self#exp _a4 _b4 in `CrMthS (_a0, _a1, _a2, _a3, _a4)
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#override_flag _a2 _b2 in
            let _a3 = self#mutable_flag _a3 _b3 in
            let _a4 = self#exp _a4 _b4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
        | (`CrVir (_a0,_a1,_a2,_a3),`CrVir (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#private_flag _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `CrVir (_a0, _a1, _a2, _a3)
        | (`CrVvr (_a0,_a1,_a2,_a3),`CrVvr (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#mutable_flag _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `CrVvr (_a0, _a1, _a2, _a3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>cstru)
        | (_,_) -> invalid_arg "map2 failure"
    method ep : ep -> ep -> ep=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>ep)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ep _a1 _b1 in
            let _a2 = self#ep _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Vrn (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ep _a1 _b1 in
            let _a2 = self#ep _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ep _a1 _b1 in
            let _a2 = self#ep _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ep _a1 _b1 in `Tup (_a0, _a1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>ep)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `ArrayEmpty _a0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ep _a1 _b1 in `Array (_a0, _a1)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_bind _a1 _b1 in `Record (_a0, _a1)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 : literal  :>ep)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>ep)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_bind : rec_bind -> rec_bind -> rec_bind=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#ep _a2 _b2 in `RecBind (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_bind _a1 _b1 in
            let _a2 = self#rec_bind _a2 _b2 in `Sem (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) ->
            (self#any _a0 _b0 : any  :>rec_bind)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_bind)
        | (_,_) -> invalid_arg "map2 failure"
    method fanloc_t : FanLoc.t -> FanLoc.t -> FanLoc.t= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> FanUtil.anti_cxt= self#unknown
  end
class fold2 =
  object (self : 'self_type)
    inherit  foldbase2
    method loc : loc -> loc -> 'self_type=
      fun _a0  _a1  -> self#fanloc_t _a0 _a1
    method ant : ant -> ant -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#fanutil_anti_cxt _a1 _b1
    method nil : nil -> nil -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
    method literal : literal -> literal -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Chr (_a0,_a1),`Chr (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Int (_a0,_a1),`Int (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Int32 (_a0,_a1),`Int32 (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Int64 (_a0,_a1),`Int64 (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Flo (_a0,_a1),`Flo (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`NativeInt (_a0,_a1),`NativeInt (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_flag : rec_flag -> rec_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive _a0,`Recursive _b0) -> self#loc _a0 _b0
        | (`ReNil _a0,`ReNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method direction_flag : direction_flag -> direction_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To _a0,`To _b0) -> self#loc _a0 _b0
        | (`Downto _a0,`Downto _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method mutable_flag : mutable_flag -> mutable_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable _a0,`Mutable _b0) -> self#loc _a0 _b0
        | (`MuNil _a0,`MuNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method private_flag : private_flag -> private_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private _a0,`Private _b0) -> self#loc _a0 _b0
        | (`PrNil _a0,`PrNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method virtual_flag : virtual_flag -> virtual_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual _a0,`Virtual _b0) -> self#loc _a0 _b0
        | (`ViNil _a0,`ViNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method override_flag : override_flag -> override_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override _a0,`Override _b0) -> self#loc _a0 _b0
        | (`OvNil _a0,`OvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method row_var_flag : row_var_flag -> row_var_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar _a0,`RowVar _b0) -> self#loc _a0 _b0
        | (`RvNil _a0,`RvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method position_flag : position_flag -> position_flag -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive _a0,`Positive _b0) -> self#loc _a0 _b0
        | (`Negative _a0,`Negative _b0) -> self#loc _a0 _b0
        | (`Normal _a0,`Normal _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method strings : strings -> strings -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#strings _a1 _b1 in self#strings _a2 _b2
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method alident : alident -> alident -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method auident : auident -> auident -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method aident : aident -> aident -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'self_type)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method astring : astring -> astring -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C (_a0,_a1),`C (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method uident : uident -> uident -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#uident _a1 _b1 in self#uident _a2 _b2
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#uident _a1 _b1 in self#uident _a2 _b2
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method ident : ident -> ident -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#ident _a2 _b2
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#ident _a2 _b2
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'self_type)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method dupath : dupath -> dupath -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#dupath _a1 _b1 in self#dupath _a2 _b2
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method dlpath : dlpath -> dlpath -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#dupath _a1 _b1 in self#alident _a2 _b2
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method any : any -> any -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
    method sid : sid -> sid -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
    method ctyp : ctyp -> ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#alident _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`Arrow (_a0,_a1,_a2),`Arrow (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#ctyp _a2 _b2
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#ctyp _a2 _b2
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`TyObj (_a0,_a1,_a2),`TyObj (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#name_ctyp _a1 _b1 in self#row_var_flag _a2 _b2
        | (`TyObjEnd (_a0,_a1),`TyObjEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#row_var_flag _a1 _b1
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`TyPolEnd (_a0,_a1),`TyPolEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ctyp _a1 _b1
        | (`TyTypePol (_a0,_a1,_a2),`TyTypePol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#position_flag _a1 _b1 in self#alident _a2 _b2
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#position_flag _a1 _b1
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ctyp _a1 _b1
        | (`Sta (_a0,_a1,_a2),`Sta (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`PolyEq (_a0,_a1),`PolyEq (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#row_field _a1 _b1
        | (`PolySup (_a0,_a1),`PolySup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#row_field _a1 _b1
        | (`PolyInf (_a0,_a1),`PolyInf (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#row_field _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`PolyInfSup (_a0,_a1,_a2),`PolyInfSup (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#row_field _a1 _b1 in self#tag_names _a2 _b2
        | (`Package (_a0,_a1),`Package (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_type _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method type_parameters :
      type_parameters -> type_parameters -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#type_parameters _a1 _b1 in
            self#type_parameters _a2 _b2
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ctyp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method row_field : row_field -> row_field -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#row_field _a1 _b1 in self#row_field _a2 _b2
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#astring _a1 _b1
        | (`TyVrnOf (_a0,_a1,_a2),`TyVrnOf (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#astring _a1 _b1 in self#ctyp _a2 _b2
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ctyp _a1 _b1
        | (_,_) -> invalid_arg "fold2 failure"
    method tag_names : tag_names -> tag_names -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#tag_names _a1 _b1 in self#tag_names _a2 _b2
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#astring _a1 _b1
        | (_,_) -> invalid_arg "fold2 failure"
    method typedecl : typedecl -> typedecl -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyDcl (_a0,_a1,_a2,_a3,_a4),`TyDcl (_b0,_b1,_b2,_b3,_b4)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#opt_decl_params _a2 _b2 in
            let self = self#type_info _a3 _b3 in self#opt_type_constr _a4 _b4
        | (`TyAbstr (_a0,_a1,_a2,_a3),`TyAbstr (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#opt_decl_params _a2 _b2 in
            self#opt_type_constr _a3 _b3
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#typedecl _a1 _b1 in self#typedecl _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method type_constr : type_constr -> type_constr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#type_constr _a1 _b1 in self#type_constr _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method opt_type_constr :
      opt_type_constr -> opt_type_constr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#type_constr _a1 _b1
        | (`None _a0,`None _b0) -> self#loc _a0 _b0
        | (_,_) -> invalid_arg "fold2 failure"
    method decl_param : decl_param -> decl_param -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#position_flag _a1 _b1 in self#alident _a2 _b2
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#position_flag _a1 _b1
        | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method decl_params : decl_params -> decl_params -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#position_flag _a1 _b1 in self#alident _a2 _b2
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#position_flag _a1 _b1
        | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#decl_params _a1 _b1 in self#decl_params _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method opt_decl_params :
      opt_decl_params -> opt_decl_params -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#decl_params _a1 _b1
        | (`None _a0,`None _b0) -> self#loc _a0 _b0
        | (_,_) -> invalid_arg "fold2 failure"
    method type_info : type_info -> type_info -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyMan (_a0,_a1,_a2,_a3),`TyMan (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in
            let self = self#private_flag _a2 _b2 in self#type_repr _a3 _b3
        | (`TyRepr (_a0,_a1,_a2),`TyRepr (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#private_flag _a1 _b1 in self#type_repr _a2 _b2
        | (`TyEq (_a0,_a1,_a2),`TyEq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#private_flag _a1 _b1 in self#ctyp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method type_repr : type_repr -> type_repr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#name_ctyp _a1 _b1
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#or_ctyp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method name_ctyp : name_ctyp -> name_ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#name_ctyp _a1 _b1 in self#name_ctyp _a2 _b2
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | (`TyColMut (_a0,_a1,_a2),`TyColMut (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method or_ctyp : or_ctyp -> or_ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#or_ctyp _a1 _b1 in self#or_ctyp _a2 _b2
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method of_ctyp : of_ctyp -> of_ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method pat : pat -> pat -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#pat _a2 _b2
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#pat _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#pat _a2 _b2
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#pat _a1 _b1
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_pat _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'self_type)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#alident _a2 _b2
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#pat _a1 _b1
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#pat _a2 _b2
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#pat _a2 _b2
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#pat _a2 _b2 in self#exp _a3 _b3
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#pat _a2 _b2
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#pat _a2 _b2
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#ctyp _a2 _b2
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#pat _a1 _b1
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#auident _a1 _b1
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#ctyp _a2 _b2
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_pat : rec_pat -> rec_pat -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#pat _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_pat _a1 _b1 in self#rec_pat _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method exp : exp -> exp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_exp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'self_type)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_exp _a1 _b1 in self#exp _a2 _b2
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`ExAsf _a0,`ExAsf _b0) -> self#loc _a0 _b0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#exp _a2 _b2 in
            let self = self#exp _a3 _b3 in
            let self = self#direction_flag _a4 _b4 in self#exp _a5 _b5
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#case _a1 _b1
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in
            let self = self#exp _a2 _b2 in self#exp _a3 _b3
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#exp _a2 _b2
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_flag _a1 _b1 in
            let self = self#binding _a2 _b2 in self#exp _a3 _b3
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_exp _a2 _b2 in self#exp _a3 _b3
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#case _a2 _b2
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#cstru _a1 _b1
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#cstru _a2 _b2
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#pat _a1 _b1
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#exp _a2 _b2
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_exp _a1 _b1
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) -> self#loc _a0 _b0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#alident _a2 _b2
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#case _a2 _b2
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#ctyp _a2 _b2
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in
            let self = self#ctyp _a2 _b2 in self#ctyp _a3 _b3
        | (`Subtype (_a0,_a1,_a2),`Subtype (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#ctyp _a2 _b2
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#exp _a1 _b1 in self#exp _a2 _b2
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#exp _a2 _b2
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#exp _a2 _b2
        | (`Package_exp (_a0,_a1),`Package_exp (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_exp _a1 _b1
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_exp : rec_exp -> rec_exp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_exp _a1 _b1 in self#rec_exp _a2 _b2
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#exp _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_type : module_type -> module_type -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_type _a3 _b3
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#sig_item _a1 _b1
        | (`SigEnd _a0,`SigEnd _b0) -> self#loc _a0 _b0
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_type _a1 _b1 in self#with_constr _a2 _b2
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_exp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method sig_item : sig_item -> sig_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_type _a1 _b1
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_type _a1 _b1
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sig_item _a1 _b1 in self#sig_item _a2 _b2
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#exp _a2 _b2
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#of_ctyp _a1 _b1
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#ctyp _a2 _b2 in self#strings _a3 _b3
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_type _a1 _b1
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_type _a2 _b2
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_binding _a1 _b1
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_type _a2 _b2
        | (`ModuleTypeEnd (_a0,_a1),`ModuleTypeEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#auident _a1 _b1
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#typedecl _a1 _b1
        | (`Val (_a0,_a1,_a2),`Val (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#ctyp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method with_constr : with_constr -> with_constr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TypeEq (_a0,_a1,_a2),`TypeEq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`TypeEqPriv (_a0,_a1,_a2),`TypeEqPriv (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`ModuleEq (_a0,_a1,_a2),`ModuleEq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#ident _a2 _b2
        | (`TypeSubst (_a0,_a1,_a2),`TypeSubst (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`ModuleSubst (_a0,_a1,_a2),`ModuleSubst (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#ident _a2 _b2
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#with_constr _a1 _b1 in self#with_constr _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method binding : binding -> binding -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#binding _a1 _b1 in self#binding _a2 _b2
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#exp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_binding : module_binding -> module_binding -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_binding _a1 _b1 in
            self#module_binding _a2 _b2
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_exp _a3 _b3
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_type _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method case : case -> case -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#case _a1 _b1 in self#case _a2 _b2
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#exp _a2 _b2
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in
            let self = self#exp _a2 _b2 in self#exp _a3 _b3
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_exp : module_exp -> module_exp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_exp _a1 _b1 in self#module_exp _a2 _b2
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_exp _a3 _b3
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#stru _a1 _b1
        | (`StructEnd _a0,`StructEnd _b0) -> self#loc _a0 _b0
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_exp _a1 _b1 in self#module_type _a2 _b2
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method stru : stru -> stru -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_exp _a1 _b1
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_type _a1 _b1
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#stru _a1 _b1 in self#stru _a2 _b2
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#exp _a2 _b2
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#of_ctyp _a1 _b1
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#ctyp _a2 _b2 in self#strings _a3 _b3
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_exp _a1 _b1
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_exp _a2 _b2
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_binding _a1 _b1
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_type _a2 _b2
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#typedecl _a1 _b1
        | (`Value (_a0,_a1,_a2),`Value (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_flag _a1 _b1 in self#binding _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_type : class_type -> class_type -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in
            let self = self#ident _a2 _b2 in self#type_parameters _a3 _b3
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in self#ident _a2 _b2
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#class_type _a2 _b2
        | (`ObjTy (_a0,_a1,_a2),`ObjTy (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#class_sig_item _a2 _b2
        | (`ObjTyEnd (_a0,_a1),`ObjTyEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ctyp _a1 _b1
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_sig_item _a1 _b1
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_sig_item : class_sig_item -> class_sig_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_sig_item _a1 _b1 in
            self#class_sig_item _a2 _b2
        | (`SigInherit (_a0,_a1),`SigInherit (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_type _a1 _b1
        | (`Method (_a0,_a1,_a2,_a3),`Method (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#private_flag _a2 _b2 in self#ctyp _a3 _b3
        | (`CgVal (_a0,_a1,_a2,_a3,_a4),`CgVal (_b0,_b1,_b2,_b3,_b4)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#mutable_flag _a2 _b2 in
            let self = self#virtual_flag _a3 _b3 in self#ctyp _a4 _b4
        | (`CgVir (_a0,_a1,_a2,_a3),`CgVir (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#private_flag _a2 _b2 in self#ctyp _a3 _b3
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_exp : class_exp -> class_exp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_exp _a1 _b1 in self#exp _a2 _b2
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in
            let self = self#ident _a2 _b2 in self#type_parameters _a3 _b3
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in self#ident _a2 _b2
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#class_exp _a2 _b2
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_flag _a1 _b1 in
            let self = self#binding _a2 _b2 in self#class_exp _a3 _b3
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#cstru _a1 _b1
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#pat _a1 _b1 in self#cstru _a2 _b2
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#pat _a1 _b1
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_exp _a1 _b1 in self#class_type _a2 _b2
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_exp _a1 _b1 in self#class_exp _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_exp _a1 _b1 in self#class_exp _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method cstru : cstru -> cstru -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#cstru _a1 _b1 in self#cstru _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#override_flag _a1 _b1 in self#class_exp _a2 _b2
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#override_flag _a1 _b1 in
            let self = self#class_exp _a2 _b2 in self#alident _a3 _b3
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#exp _a1 _b1
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#override_flag _a2 _b2 in
            let self = self#private_flag _a3 _b3 in
            let self = self#exp _a4 _b4 in self#ctyp _a5 _b5
        | (`CrMthS (_a0,_a1,_a2,_a3,_a4),`CrMthS (_b0,_b1,_b2,_b3,_b4)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#override_flag _a2 _b2 in
            let self = self#private_flag _a3 _b3 in self#exp _a4 _b4
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#override_flag _a2 _b2 in
            let self = self#mutable_flag _a3 _b3 in self#exp _a4 _b4
        | (`CrVir (_a0,_a1,_a2,_a3),`CrVir (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#private_flag _a2 _b2 in self#ctyp _a3 _b3
        | (`CrVvr (_a0,_a1,_a2,_a3),`CrVvr (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#mutable_flag _a2 _b2 in self#ctyp _a3 _b3
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method ep : ep -> ep -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ep _a1 _b1 in self#ep _a2 _b2
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ep _a1 _b1 in self#ep _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ep _a1 _b1 in self#ep _a2 _b2
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ep _a1 _b1
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ep _a1 _b1
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_bind _a1 _b1
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_bind : rec_bind -> rec_bind -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#ep _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_bind _a1 _b1 in self#rec_bind _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'self_type= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'self_type= self#unknown
  end
class iter =
  object (self : 'self_type)
    inherit  iterbase
    method loc : loc -> 'result116= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> 'result117=
      fun (`Ant (_a0,_a1))  -> self#loc _a0; self#fanutil_anti_cxt _a1
    method nil : nil -> 'result118= fun (`Nil _a0)  -> self#loc _a0
    method literal : literal -> 'result119=
      function
      | `Chr (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int32 (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int64 (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Flo (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `NativeInt (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Str (_a0,_a1) -> (self#loc _a0; self#string _a1)
    method rec_flag : rec_flag -> 'result120=
      function
      | `Recursive _a0 -> self#loc _a0
      | `ReNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result120)
    method direction_flag : direction_flag -> 'result121=
      function
      | `To _a0 -> self#loc _a0
      | `Downto _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result121)
    method mutable_flag : mutable_flag -> 'result122=
      function
      | `Mutable _a0 -> self#loc _a0
      | `MuNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result122)
    method private_flag : private_flag -> 'result123=
      function
      | `Private _a0 -> self#loc _a0
      | `PrNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result123)
    method virtual_flag : virtual_flag -> 'result124=
      function
      | `Virtual _a0 -> self#loc _a0
      | `ViNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result124)
    method override_flag : override_flag -> 'result125=
      function
      | `Override _a0 -> self#loc _a0
      | `OvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result125)
    method row_var_flag : row_var_flag -> 'result126=
      function
      | `RowVar _a0 -> self#loc _a0
      | `RvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result126)
    method position_flag : position_flag -> 'result127=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | `Normal _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result127)
    method strings : strings -> 'result128=
      function
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#strings _a1; self#strings _a2)
      | `Str (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result128)
    method alident : alident -> 'result129=
      function
      | `Lid (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result129)
    method auident : auident -> 'result130=
      function
      | `Uid (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result130)
    method aident : aident -> 'result131=
      function
      | #alident as _a0 -> (self#alident _a0 :>'result131)
      | #auident as _a0 -> (self#auident _a0 :>'result131)
    method astring : astring -> 'result132=
      function
      | `C (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result132)
    method uident : uident -> 'result133=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#uident _a1; self#uident _a2)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#uident _a1; self#uident _a2)
      | #auident as _a0 -> (self#auident _a0 :>'result133)
    method ident : ident -> 'result134=
      function
      | `Dot (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ident _a2)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ident _a2)
      | #alident as _a0 -> (self#alident _a0 :>'result134)
      | #auident as _a0 -> (self#auident _a0 :>'result134)
    method dupath : dupath -> 'result135=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#dupath _a1; self#dupath _a2)
      | #auident as _a0 -> (self#auident _a0 :>'result135)
    method dlpath : dlpath -> 'result136=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#dupath _a1; self#alident _a2)
      | #alident as _a0 -> (self#alident _a0 :>'result136)
    method any : any -> 'result137= fun (`Any _a0)  -> self#loc _a0
    method sid : sid -> 'result138=
      fun (`Id (_a0,_a1))  -> self#loc _a0; self#ident _a1
    method ctyp : ctyp -> 'result139=
      function
      | `Alias (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#alident _a2)
      | #any as _a0 -> (self#any _a0 :>'result139)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Arrow (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `ClassPath (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result139)
      | `TyObj (_a0,_a1,_a2) ->
          (self#loc _a0; self#name_ctyp _a1; self#row_var_flag _a2)
      | `TyObjEnd (_a0,_a1) -> (self#loc _a0; self#row_var_flag _a1)
      | `TyPol (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `TyPolEnd (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
      | `TyTypePol (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Quote (_a0,_a1,_a2) ->
          (self#loc _a0; self#position_flag _a1; self#alident _a2)
      | `QuoteAny (_a0,_a1) -> (self#loc _a0; self#position_flag _a1)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
      | `Sta (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `PolyEq (_a0,_a1) -> (self#loc _a0; self#row_field _a1)
      | `PolySup (_a0,_a1) -> (self#loc _a0; self#row_field _a1)
      | `PolyInf (_a0,_a1) -> (self#loc _a0; self#row_field _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `PolyInfSup (_a0,_a1,_a2) ->
          (self#loc _a0; self#row_field _a1; self#tag_names _a2)
      | `Package (_a0,_a1) -> (self#loc _a0; self#module_type _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result139)
    method type_parameters : type_parameters -> 'result140=
      function
      | `Com (_a0,_a1,_a2) ->
          (self#loc _a0; self#type_parameters _a1; self#type_parameters _a2)
      | `Ctyp (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result140)
    method row_field : row_field -> 'result141=
      function
      | #ant as _a0 -> (self#ant _a0 :>'result141)
      | `Or (_a0,_a1,_a2) ->
          (self#loc _a0; self#row_field _a1; self#row_field _a2)
      | `TyVrn (_a0,_a1) -> (self#loc _a0; self#astring _a1)
      | `TyVrnOf (_a0,_a1,_a2) ->
          (self#loc _a0; self#astring _a1; self#ctyp _a2)
      | `Ctyp (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
    method tag_names : tag_names -> 'result142=
      function
      | #ant as _a0 -> (self#ant _a0 :>'result142)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#tag_names _a1; self#tag_names _a2)
      | `TyVrn (_a0,_a1) -> (self#loc _a0; self#astring _a1)
    method typedecl : typedecl -> 'result143=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#opt_decl_params _a2;
           self#type_info _a3;
           self#opt_type_constr _a4)
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#alident _a1;
           self#opt_decl_params _a2;
           self#opt_type_constr _a3)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#typedecl _a1; self#typedecl _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result143)
    method type_constr : type_constr -> 'result144=
      function
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#type_constr _a1; self#type_constr _a2)
      | `Eq (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result144)
    method opt_type_constr : opt_type_constr -> 'result145=
      function
      | `Some (_a0,_a1) -> (self#loc _a0; self#type_constr _a1)
      | `None _a0 -> self#loc _a0
    method decl_param : decl_param -> 'result146=
      function
      | `Quote (_a0,_a1,_a2) ->
          (self#loc _a0; self#position_flag _a1; self#alident _a2)
      | `QuoteAny (_a0,_a1) -> (self#loc _a0; self#position_flag _a1)
      | `Any _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result146)
    method decl_params : decl_params -> 'result147=
      function
      | `Quote (_a0,_a1,_a2) ->
          (self#loc _a0; self#position_flag _a1; self#alident _a2)
      | `QuoteAny (_a0,_a1) -> (self#loc _a0; self#position_flag _a1)
      | `Any _a0 -> self#loc _a0
      | `Com (_a0,_a1,_a2) ->
          (self#loc _a0; self#decl_params _a1; self#decl_params _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result147)
    method opt_decl_params : opt_decl_params -> 'result148=
      function
      | `Some (_a0,_a1) -> (self#loc _a0; self#decl_params _a1)
      | `None _a0 -> self#loc _a0
    method type_info : type_info -> 'result149=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#ctyp _a1;
           self#private_flag _a2;
           self#type_repr _a3)
      | `TyRepr (_a0,_a1,_a2) ->
          (self#loc _a0; self#private_flag _a1; self#type_repr _a2)
      | `TyEq (_a0,_a1,_a2) ->
          (self#loc _a0; self#private_flag _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result149)
    method type_repr : type_repr -> 'result150=
      function
      | `Record (_a0,_a1) -> (self#loc _a0; self#name_ctyp _a1)
      | `Sum (_a0,_a1) -> (self#loc _a0; self#or_ctyp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result150)
    method name_ctyp : name_ctyp -> 'result151=
      function
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#name_ctyp _a1; self#name_ctyp _a2)
      | `TyCol (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | `TyColMut (_a0,_a1,_a2) ->
          (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result151)
    method or_ctyp : or_ctyp -> 'result152=
      function
      | `Or (_a0,_a1,_a2) ->
          (self#loc _a0; self#or_ctyp _a1; self#or_ctyp _a2)
      | `TyCol (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | `Of (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result152)
      | #ant as _a0 -> (self#ant _a0 :>'result152)
    method of_ctyp : of_ctyp -> 'result153=
      function
      | `Of (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result153)
      | #ant as _a0 -> (self#ant _a0 :>'result153)
    method pat : pat -> 'result154=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result154)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#pat _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#pat _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#pat _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#pat _a1)
      | #any as _a0 -> (self#any _a0 :>'result154)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_pat _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result154)
      | #literal as _a0 -> (self#literal _a0 :>'result154)
      | `Alias (_a0,_a1,_a2) ->
          (self#loc _a0; self#pat _a1; self#alident _a2)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#pat _a1)
      | `LabelS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#pat _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#pat _a2)
      | `OptLablS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#pat _a2; self#exp _a3)
      | `Or (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#pat _a2)
      | `PaRng (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#pat _a2)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#pat _a1; self#ctyp _a2)
      | `ClassPath (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Lazy (_a0,_a1) -> (self#loc _a0; self#pat _a1)
      | `ModuleUnpack (_a0,_a1) -> (self#loc _a0; self#auident _a1)
      | `ModuleConstraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#ctyp _a2)
    method rec_pat : rec_pat -> 'result155=
      function
      | `RecBind (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#pat _a2)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_pat _a1; self#rec_pat _a2)
      | #any as _a0 -> (self#any _a0 :>'result155)
      | #ant as _a0 -> (self#ant _a0 :>'result155)
    method exp : exp -> 'result156=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result156)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | #any as _a0 -> (self#any _a0 :>'result156)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_exp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result156)
      | #literal as _a0 -> (self#literal _a0 :>'result156)
      | `RecordWith (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_exp _a1; self#exp _a2)
      | `Dot (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `ArrayDot (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `ExAsf _a0 -> self#loc _a0
      | `ExAsr (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `Assign (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          (self#loc _a0;
           self#alident _a1;
           self#exp _a2;
           self#exp _a3;
           self#direction_flag _a4;
           self#exp _a5)
      | `Fun (_a0,_a1) -> (self#loc _a0; self#case _a1)
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#exp _a1; self#exp _a2; self#exp _a3)
      | `IfThen (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `LabelS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#exp _a2)
      | `Lazy (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#rec_flag _a1; self#binding _a2; self#exp _a3)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#auident _a1; self#module_exp _a2; self#exp _a3)
      | `Match (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#case _a2)
      | `New (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Obj (_a0,_a1) -> (self#loc _a0; self#cstru _a1)
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#cstru _a2)
      | `ObjPatEnd (_a0,_a1) -> (self#loc _a0; self#pat _a1)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#exp _a2)
      | `OptLablS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `OvrInst (_a0,_a1) -> (self#loc _a0; self#rec_exp _a1)
      | `OvrInstEmpty _a0 -> self#loc _a0
      | `Seq (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `Send (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#alident _a2)
      | `StringDot (_a0,_a1,_a2) ->
          (self#loc _a0; self#exp _a1; self#exp _a2)
      | `Try (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#case _a2)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#exp _a1; self#ctyp _a2)
      | `Coercion (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#exp _a1; self#ctyp _a2; self#ctyp _a3)
      | `Subtype (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#ctyp _a2)
      | `While (_a0,_a1,_a2) -> (self#loc _a0; self#exp _a1; self#exp _a2)
      | `LetOpen (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#exp _a2)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#exp _a2)
      | `Package_exp (_a0,_a1) -> (self#loc _a0; self#module_exp _a1)
    method rec_exp : rec_exp -> 'result157=
      function
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_exp _a1; self#rec_exp _a2)
      | `RecBind (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#exp _a2)
      | #any as _a0 -> (self#any _a0 :>'result157)
      | #ant as _a0 -> (self#ant _a0 :>'result157)
    method module_type : module_type -> 'result158=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result158)
      | `Functor (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_type _a3)
      | `Sig (_a0,_a1) -> (self#loc _a0; self#sig_item _a1)
      | `SigEnd _a0 -> self#loc _a0
      | `With (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_type _a1; self#with_constr _a2)
      | `ModuleTypeOf (_a0,_a1) -> (self#loc _a0; self#module_exp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result158)
    method sig_item : sig_item -> 'result159=
      function
      | `Class (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `ClassType (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#sig_item _a1; self#sig_item _a2)
      | `DirectiveSimple (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Directive (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#exp _a2)
      | `Exception (_a0,_a1) -> (self#loc _a0; self#of_ctyp _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2; self#strings _a3)
      | `Include (_a0,_a1) -> (self#loc _a0; self#module_type _a1)
      | `Module (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `RecModule (_a0,_a1) -> (self#loc _a0; self#module_binding _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `ModuleTypeEnd (_a0,_a1) -> (self#loc _a0; self#auident _a1)
      | `Open (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Type (_a0,_a1) -> (self#loc _a0; self#typedecl _a1)
      | `Val (_a0,_a1,_a2) -> (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result159)
    method with_constr : with_constr -> 'result160=
      function
      | `TypeEq (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `TypeEqPriv (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `ModuleEq (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#ident _a2)
      | `TypeSubst (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `ModuleSubst (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#ident _a2)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#with_constr _a1; self#with_constr _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result160)
    method binding : binding -> 'result161=
      function
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#binding _a1; self#binding _a2)
      | `Bind (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#exp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result161)
    method module_binding : module_binding -> 'result162=
      function
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_binding _a1; self#module_binding _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_exp _a3)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result162)
    method case : case -> 'result163=
      function
      | `Or (_a0,_a1,_a2) -> (self#loc _a0; self#case _a1; self#case _a2)
      | `Case (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#exp _a2)
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#pat _a1; self#exp _a2; self#exp _a3)
      | #ant as _a0 -> (self#ant _a0 :>'result163)
    method module_exp : module_exp -> 'result164=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result164)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_exp _a1; self#module_exp _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_exp _a3)
      | `Struct (_a0,_a1) -> (self#loc _a0; self#stru _a1)
      | `StructEnd _a0 -> self#loc _a0
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_exp _a1; self#module_type _a2)
      | `PackageModule (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result164)
    method stru : stru -> 'result165=
      function
      | `Class (_a0,_a1) -> (self#loc _a0; self#class_exp _a1)
      | `ClassType (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#stru _a1; self#stru _a2)
      | `DirectiveSimple (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Directive (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#exp _a2)
      | `Exception (_a0,_a1) -> (self#loc _a0; self#of_ctyp _a1)
      | `StExp (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2; self#strings _a3)
      | `Include (_a0,_a1) -> (self#loc _a0; self#module_exp _a1)
      | `Module (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_exp _a2)
      | `RecModule (_a0,_a1) -> (self#loc _a0; self#module_binding _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `Open (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Type (_a0,_a1) -> (self#loc _a0; self#typedecl _a1)
      | `Value (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_flag _a1; self#binding _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result165)
    method class_type : class_type -> 'result166=
      function
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#virtual_flag _a1;
           self#ident _a2;
           self#type_parameters _a3)
      | `ClassConS (_a0,_a1,_a2) ->
          (self#loc _a0; self#virtual_flag _a1; self#ident _a2)
      | `CtFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#class_type _a2)
      | `ObjTy (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#class_sig_item _a2)
      | `ObjTyEnd (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
      | `Obj (_a0,_a1) -> (self#loc _a0; self#class_sig_item _a1)
      | `ObjEnd _a0 -> self#loc _a0
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | `CtCol (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | `Eq (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result166)
    method class_sig_item : class_sig_item -> 'result167=
      function
      | `Eq (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_sig_item _a1; self#class_sig_item _a2)
      | `SigInherit (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `Method (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#alident _a1;
           self#private_flag _a2;
           self#ctyp _a3)
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#mutable_flag _a2;
           self#virtual_flag _a3;
           self#ctyp _a4)
      | `CgVir (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#alident _a1;
           self#private_flag _a2;
           self#ctyp _a3)
      | #ant as _a0 -> (self#ant _a0 :>'result167)
    method class_exp : class_exp -> 'result168=
      function
      | `CeApp (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_exp _a1; self#exp _a2)
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#virtual_flag _a1;
           self#ident _a2;
           self#type_parameters _a3)
      | `ClassConS (_a0,_a1,_a2) ->
          (self#loc _a0; self#virtual_flag _a1; self#ident _a2)
      | `CeFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#pat _a1; self#class_exp _a2)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#rec_flag _a1;
           self#binding _a2;
           self#class_exp _a3)
      | `Obj (_a0,_a1) -> (self#loc _a0; self#cstru _a1)
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) -> (self#loc _a0; self#pat _a1; self#cstru _a2)
      | `ObjPatEnd (_a0,_a1) -> (self#loc _a0; self#pat _a1)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_exp _a1; self#class_type _a2)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_exp _a1; self#class_exp _a2)
      | `Eq (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_exp _a1; self#class_exp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result168)
    method cstru : cstru -> 'result169=
      function
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#cstru _a1; self#cstru _a2)
      | `Eq (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Inherit (_a0,_a1,_a2) ->
          (self#loc _a0; self#override_flag _a1; self#class_exp _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#override_flag _a1;
           self#class_exp _a2;
           self#alident _a3)
      | `Initializer (_a0,_a1) -> (self#loc _a0; self#exp _a1)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          (self#loc _a0;
           self#alident _a1;
           self#override_flag _a2;
           self#private_flag _a3;
           self#exp _a4;
           self#ctyp _a5)
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#override_flag _a2;
           self#private_flag _a3;
           self#exp _a4)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#override_flag _a2;
           self#mutable_flag _a3;
           self#exp _a4)
      | `CrVir (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#alident _a1;
           self#private_flag _a2;
           self#ctyp _a3)
      | `CrVvr (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#alident _a1;
           self#mutable_flag _a2;
           self#ctyp _a3)
      | #ant as _a0 -> (self#ant _a0 :>'result169)
    method ep : ep -> 'result170=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result170)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#ep _a1)
      | #any as _a0 -> (self#any _a0 :>'result170)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#ep _a1)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_bind _a1)
      | #literal as _a0 -> (self#literal _a0 :>'result170)
      | #ant as _a0 -> (self#ant _a0 :>'result170)
    method rec_bind : rec_bind -> 'result171=
      function
      | `RecBind (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ep _a2)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_bind _a1; self#rec_bind _a2)
      | #any as _a0 -> (self#any _a0 :>'result171)
      | #ant as _a0 -> (self#ant _a0 :>'result171)
    method fanloc_t : FanLoc.t -> 'result172= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> 'result173= self#unknown
  end
class map =
  object (self : 'self_type)
    inherit  mapbase
    method loc : loc -> loc= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> ant=
      fun (`Ant (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#fanutil_anti_cxt _a1 in `Ant (_a0, _a1)
    method nil : nil -> nil=
      fun (`Nil _a0)  -> let _a0 = self#loc _a0 in `Nil _a0
    method literal : literal -> literal=
      function
      | `Chr (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Chr (_a0, _a1)
      | `Int (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int (_a0, _a1)
      | `Int32 (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int32 (_a0, _a1)
      | `Int64 (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Int64 (_a0, _a1)
      | `Flo (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Flo (_a0, _a1)
      | `NativeInt (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `NativeInt (_a0, _a1)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
    method rec_flag : rec_flag -> rec_flag=
      function
      | `Recursive _a0 -> let _a0 = self#loc _a0 in `Recursive _a0
      | `ReNil _a0 -> let _a0 = self#loc _a0 in `ReNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_flag)
    method direction_flag : direction_flag -> direction_flag=
      function
      | `To _a0 -> let _a0 = self#loc _a0 in `To _a0
      | `Downto _a0 -> let _a0 = self#loc _a0 in `Downto _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>direction_flag)
    method mutable_flag : mutable_flag -> mutable_flag=
      function
      | `Mutable _a0 -> let _a0 = self#loc _a0 in `Mutable _a0
      | `MuNil _a0 -> let _a0 = self#loc _a0 in `MuNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>mutable_flag)
    method private_flag : private_flag -> private_flag=
      function
      | `Private _a0 -> let _a0 = self#loc _a0 in `Private _a0
      | `PrNil _a0 -> let _a0 = self#loc _a0 in `PrNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>private_flag)
    method virtual_flag : virtual_flag -> virtual_flag=
      function
      | `Virtual _a0 -> let _a0 = self#loc _a0 in `Virtual _a0
      | `ViNil _a0 -> let _a0 = self#loc _a0 in `ViNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>virtual_flag)
    method override_flag : override_flag -> override_flag=
      function
      | `Override _a0 -> let _a0 = self#loc _a0 in `Override _a0
      | `OvNil _a0 -> let _a0 = self#loc _a0 in `OvNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>override_flag)
    method row_var_flag : row_var_flag -> row_var_flag=
      function
      | `RowVar _a0 -> let _a0 = self#loc _a0 in `RowVar _a0
      | `RvNil _a0 -> let _a0 = self#loc _a0 in `RvNil _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>row_var_flag)
    method position_flag : position_flag -> position_flag=
      function
      | `Positive _a0 -> let _a0 = self#loc _a0 in `Positive _a0
      | `Negative _a0 -> let _a0 = self#loc _a0 in `Negative _a0
      | `Normal _a0 -> let _a0 = self#loc _a0 in `Normal _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>position_flag)
    method strings : strings -> strings=
      function
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#strings _a1 in
          let _a2 = self#strings _a2 in `App (_a0, _a1, _a2)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>strings)
    method alident : alident -> alident=
      function
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>alident)
    method auident : auident -> auident=
      function
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>auident)
    method aident : aident -> aident=
      function
      | #alident as _a0 -> (self#alident _a0 : alident  :>aident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>aident)
    method astring : astring -> astring=
      function
      | `C (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `C (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>astring)
    method uident : uident -> uident=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#uident _a1 in
          let _a2 = self#uident _a2 in `Dot (_a0, _a1, _a2)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#uident _a1 in
          let _a2 = self#uident _a2 in `App (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>uident)
    method ident : ident -> ident=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Dot (_a0, _a1, _a2)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `App (_a0, _a1, _a2)
      | #alident as _a0 -> (self#alident _a0 : alident  :>ident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>ident)
    method dupath : dupath -> dupath=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#dupath _a1 in
          let _a2 = self#dupath _a2 in `Dot (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>dupath)
    method dlpath : dlpath -> dlpath=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#dupath _a1 in
          let _a2 = self#alident _a2 in `Dot (_a0, _a1, _a2)
      | #alident as _a0 -> (self#alident _a0 : alident  :>dlpath)
    method any : any -> any=
      fun (`Any _a0)  -> let _a0 = self#loc _a0 in `Any _a0
    method sid : sid -> sid=
      fun (`Id (_a0,_a1))  ->
        let _a0 = self#loc _a0 in let _a1 = self#ident _a1 in `Id (_a0, _a1)
    method ctyp : ctyp -> ctyp=
      function
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#alident _a2 in `Alias (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>ctyp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `App (_a0, _a1, _a2)
      | `Arrow (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Arrow (_a0, _a1, _a2)
      | `ClassPath (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `ClassPath (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `Label (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `OptLabl (_a0, _a1, _a2)
      | #sid as _a0 -> (self#sid _a0 : sid  :>ctyp)
      | `TyObj (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in
          let _a2 = self#row_var_flag _a2 in `TyObj (_a0, _a1, _a2)
      | `TyObjEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_var_flag _a1 in `TyObjEnd (_a0, _a1)
      | `TyPol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TyPol (_a0, _a1, _a2)
      | `TyPolEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `TyPolEnd (_a0, _a1)
      | `TyTypePol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TyTypePol (_a0, _a1, _a2)
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Tup (_a0, _a1)
      | `Sta (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Sta (_a0, _a1, _a2)
      | `PolyEq (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolyEq (_a0, _a1)
      | `PolySup (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolySup (_a0, _a1)
      | `PolyInf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in `PolyInf (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Com (_a0, _a1, _a2)
      | `PolyInfSup (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in
          let _a2 = self#tag_names _a2 in `PolyInfSup (_a0, _a1, _a2)
      | `Package (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_type _a1 in `Package (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>ctyp)
    method type_parameters : type_parameters -> type_parameters=
      function
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_parameters _a1 in
          let _a2 = self#type_parameters _a2 in `Com (_a0, _a1, _a2)
      | `Ctyp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Ctyp (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_parameters)
    method row_field : row_field -> row_field=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>row_field)
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in
          let _a2 = self#row_field _a2 in `Or (_a0, _a1, _a2)
      | `TyVrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in `TyVrn (_a0, _a1)
      | `TyVrnOf (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in
          let _a2 = self#ctyp _a2 in `TyVrnOf (_a0, _a1, _a2)
      | `Ctyp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Ctyp (_a0, _a1)
    method tag_names : tag_names -> tag_names=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>tag_names)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#tag_names _a1 in
          let _a2 = self#tag_names _a2 in `App (_a0, _a1, _a2)
      | `TyVrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#astring _a1 in `TyVrn (_a0, _a1)
    method typedecl : typedecl -> typedecl=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#opt_decl_params _a2 in
          let _a3 = self#type_info _a3 in
          let _a4 = self#opt_type_constr _a4 in
          `TyDcl (_a0, _a1, _a2, _a3, _a4)
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#opt_decl_params _a2 in
          let _a3 = self#opt_type_constr _a3 in `TyAbstr (_a0, _a1, _a2, _a3)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in
          let _a2 = self#typedecl _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>typedecl)
    method type_constr : type_constr -> type_constr=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_constr _a1 in
          let _a2 = self#type_constr _a2 in `And (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_constr)
    method opt_type_constr : opt_type_constr -> opt_type_constr=
      function
      | `Some (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#type_constr _a1 in `Some (_a0, _a1)
      | `None _a0 -> let _a0 = self#loc _a0 in `None _a0
    method decl_param : decl_param -> decl_param=
      function
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_param)
    method decl_params : decl_params -> decl_params=
      function
      | `Quote (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in
          let _a2 = self#alident _a2 in `Quote (_a0, _a1, _a2)
      | `QuoteAny (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#position_flag _a1 in `QuoteAny (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl_params _a1 in
          let _a2 = self#decl_params _a2 in `Com (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_params)
    method opt_decl_params : opt_decl_params -> opt_decl_params=
      function
      | `Some (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl_params _a1 in `Some (_a0, _a1)
      | `None _a0 -> let _a0 = self#loc _a0 in `None _a0
    method type_info : type_info -> type_info=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#private_flag _a2 in
          let _a3 = self#type_repr _a3 in `TyMan (_a0, _a1, _a2, _a3)
      | `TyRepr (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#private_flag _a1 in
          let _a2 = self#type_repr _a2 in `TyRepr (_a0, _a1, _a2)
      | `TyEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#private_flag _a1 in
          let _a2 = self#ctyp _a2 in `TyEq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_info)
    method type_repr : type_repr -> type_repr=
      function
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in `Record (_a0, _a1)
      | `Sum (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in `Sum (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_repr)
    method name_ctyp : name_ctyp -> name_ctyp=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in
          let _a2 = self#name_ctyp _a2 in `Sem (_a0, _a1, _a2)
      | `TyCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `TyCol (_a0, _a1, _a2)
      | `TyColMut (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `TyColMut (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>name_ctyp)
    method or_ctyp : or_ctyp -> or_ctyp=
      function
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in
          let _a2 = self#or_ctyp _a2 in `Or (_a0, _a1, _a2)
      | `TyCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `TyCol (_a0, _a1, _a2)
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #sid as _a0 -> (self#sid _a0 : sid  :>or_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #sid as _a0 -> (self#sid _a0 : sid  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
    method pat : pat -> pat=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>pat)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Sem (_a0, _a1, _a2)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#pat _a1 in `Tup (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>pat)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in `Record (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>pat)
      | #literal as _a0 -> (self#literal _a0 : literal  :>pat)
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#alident _a2 in `Alias (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `Array (_a0, _a1)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in `Label (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#pat _a2 in
          let _a3 = self#exp _a3 in `OptLablExpr (_a0, _a1, _a2, _a3)
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Or (_a0, _a1, _a2)
      | `PaRng (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `PaRng (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `ClassPath (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `ClassPath (_a0, _a1)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `Lazy (_a0, _a1)
      | `ModuleUnpack (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleUnpack (_a0, _a1)
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `ModuleConstraint (_a0, _a1, _a2)
    method rec_pat : rec_pat -> rec_pat=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#pat _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in
          let _a2 = self#rec_pat _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_pat)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_pat)
    method exp : exp -> exp=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>exp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Sem (_a0, _a1, _a2)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#exp _a1 in `Tup (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>exp)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in `Record (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>exp)
      | #literal as _a0 -> (self#literal _a0 : literal  :>exp)
      | `RecordWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#exp _a2 in `RecordWith (_a0, _a1, _a2)
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Dot (_a0, _a1, _a2)
      | `ArrayDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `ArrayDot (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Array (_a0, _a1)
      | `ExAsf _a0 -> let _a0 = self#loc _a0 in `ExAsf _a0
      | `ExAsr (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `ExAsr (_a0, _a1)
      | `Assign (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Assign (_a0, _a1, _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#direction_flag _a4 in
          let _a5 = self#exp _a5 in `For (_a0, _a1, _a2, _a3, _a4, _a5)
      | `Fun (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#case _a1 in `Fun (_a0, _a1)
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in `IfThenElse (_a0, _a1, _a2, _a3)
      | `IfThen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `IfThen (_a0, _a1, _a2)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Label (_a0, _a1, _a2)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Lazy (_a0, _a1)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_flag _a1 in
          let _a2 = self#binding _a2 in
          let _a3 = self#exp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_exp _a2 in
          let _a3 = self#exp _a3 in `LetModule (_a0, _a1, _a2, _a3)
      | `Match (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#case _a2 in `Match (_a0, _a1, _a2)
      | `New (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `New (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cstru _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#cstru _a2 in `ObjPat (_a0, _a1, _a2)
      | `ObjPatEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `ObjPatEnd (_a0, _a1)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OvrInst (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in `OvrInst (_a0, _a1)
      | `OvrInstEmpty _a0 -> let _a0 = self#loc _a0 in `OvrInstEmpty _a0
      | `Seq (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#exp _a1 in `Seq (_a0, _a1)
      | `Send (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#alident _a2 in `Send (_a0, _a1, _a2)
      | `StringDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `StringDot (_a0, _a1, _a2)
      | `Try (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#case _a2 in `Try (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#ctyp _a3 in `Coercion (_a0, _a1, _a2, _a3)
      | `Subtype (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#ctyp _a2 in `Subtype (_a0, _a1, _a2)
      | `While (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `While (_a0, _a1, _a2)
      | `LetOpen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#exp _a2 in `LetOpen (_a0, _a1, _a2)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `LocalTypeFun (_a0, _a1, _a2)
      | `Package_exp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_exp _a1 in `Package_exp (_a0, _a1)
    method rec_exp : rec_exp -> rec_exp=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#rec_exp _a2 in `Sem (_a0, _a1, _a2)
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#exp _a2 in `RecBind (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_exp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_exp)
    method module_type : module_type -> module_type=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>module_type)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_type _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `Sig (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sig_item _a1 in `Sig (_a0, _a1)
      | `SigEnd _a0 -> let _a0 = self#loc _a0 in `SigEnd _a0
      | `With (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_type _a1 in
          let _a2 = self#with_constr _a2 in `With (_a0, _a1, _a2)
      | `ModuleTypeOf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_exp _a1 in `ModuleTypeOf (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_type)
    method sig_item : sig_item -> sig_item=
      function
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in `ClassType (_a0, _a1)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sig_item _a1 in
          let _a2 = self#sig_item _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Directive (_a0, _a1, _a2)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_type _a1 in `Include (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in `Module (_a0, _a1, _a2)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_binding _a1 in `RecModule (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in `ModuleType (_a0, _a1, _a2)
      | `ModuleTypeEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleTypeEnd (_a0, _a1)
      | `Open (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in `Type (_a0, _a1)
      | `Val (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `Val (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>sig_item)
    method with_constr : with_constr -> with_constr=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEq (_a0, _a1, _a2)
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEqPriv (_a0, _a1, _a2)
      | `ModuleEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `ModuleEq (_a0, _a1, _a2)
      | `TypeSubst (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeSubst (_a0, _a1, _a2)
      | `ModuleSubst (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `ModuleSubst (_a0, _a1, _a2)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#with_constr _a1 in
          let _a2 = self#with_constr _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>with_constr)
    method binding : binding -> binding=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#binding _a1 in
          let _a2 = self#binding _a2 in `And (_a0, _a1, _a2)
      | `Bind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `Bind (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>binding)
    method module_binding : module_binding -> module_binding=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_binding _a1 in
          let _a2 = self#module_binding _a2 in `And (_a0, _a1, _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_exp _a3 in `ModuleBind (_a0, _a1, _a2, _a3)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_binding)
    method case : case -> case=
      function
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#case _a1 in
          let _a2 = self#case _a2 in `Or (_a0, _a1, _a2)
      | `Case (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `Case (_a0, _a1, _a2)
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in `CaseWhen (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>case)
    method module_exp : module_exp -> module_exp=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>module_exp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_exp _a1 in
          let _a2 = self#module_exp _a2 in `App (_a0, _a1, _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_exp _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `Struct (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#stru _a1 in `Struct (_a0, _a1)
      | `StructEnd _a0 -> let _a0 = self#loc _a0 in `StructEnd _a0
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_exp _a1 in
          let _a2 = self#module_type _a2 in `Constraint (_a0, _a1, _a2)
      | `PackageModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `PackageModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_exp)
    method stru : stru -> stru=
      function
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_exp _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in `ClassType (_a0, _a1)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#stru _a1 in
          let _a2 = self#stru _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Directive (_a0, _a1, _a2)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `StExp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `StExp (_a0, _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_exp _a1 in `Include (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_exp _a2 in `Module (_a0, _a1, _a2)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_binding _a1 in `RecModule (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in `ModuleType (_a0, _a1, _a2)
      | `Open (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in `Type (_a0, _a1)
      | `Value (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_flag _a1 in
          let _a2 = self#binding _a2 in `Value (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>stru)
    method class_type : class_type -> class_type=
      function
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          `ClassCon (_a0, _a1, _a2, _a3)
      | `ClassConS (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in `ClassConS (_a0, _a1, _a2)
      | `CtFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#class_type _a2 in `CtFun (_a0, _a1, _a2)
      | `ObjTy (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#class_sig_item _a2 in `ObjTy (_a0, _a1, _a2)
      | `ObjTyEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `ObjTyEnd (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_sig_item _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `And (_a0, _a1, _a2)
      | `CtCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `CtCol (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_type)
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_sig_item _a1 in
          let _a2 = self#class_sig_item _a2 in `Sem (_a0, _a1, _a2)
      | `SigInherit (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in `SigInherit (_a0, _a1)
      | `Method (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#private_flag _a2 in
          let _a3 = self#ctyp _a3 in `Method (_a0, _a1, _a2, _a3)
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#mutable_flag _a2 in
          let _a3 = self#virtual_flag _a3 in
          let _a4 = self#ctyp _a4 in `CgVal (_a0, _a1, _a2, _a3, _a4)
      | `CgVir (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#private_flag _a2 in
          let _a3 = self#ctyp _a3 in `CgVir (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_sig_item)
    method class_exp : class_exp -> class_exp=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_exp _a1 in
          let _a2 = self#exp _a2 in `CeApp (_a0, _a1, _a2)
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          `ClassCon (_a0, _a1, _a2, _a3)
      | `ClassConS (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in `ClassConS (_a0, _a1, _a2)
      | `CeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#class_exp _a2 in `CeFun (_a0, _a1, _a2)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_flag _a1 in
          let _a2 = self#binding _a2 in
          let _a3 = self#class_exp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cstru _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#cstru _a2 in `ObjPat (_a0, _a1, _a2)
      | `ObjPatEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `ObjPatEnd (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_exp _a1 in
          let _a2 = self#class_type _a2 in `Constraint (_a0, _a1, _a2)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_exp _a1 in
          let _a2 = self#class_exp _a2 in `And (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_exp _a1 in
          let _a2 = self#class_exp _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_exp)
    method cstru : cstru -> cstru=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cstru _a1 in
          let _a2 = self#cstru _a2 in `Sem (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | `Inherit (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#override_flag _a1 in
          let _a2 = self#class_exp _a2 in `Inherit (_a0, _a1, _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#override_flag _a1 in
          let _a2 = self#class_exp _a2 in
          let _a3 = self#alident _a3 in `InheritAs (_a0, _a1, _a2, _a3)
      | `Initializer (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Initializer (_a0, _a1)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#override_flag _a2 in
          let _a3 = self#private_flag _a3 in
          let _a4 = self#exp _a4 in
          let _a5 = self#ctyp _a5 in `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#override_flag _a2 in
          let _a3 = self#private_flag _a3 in
          let _a4 = self#exp _a4 in `CrMthS (_a0, _a1, _a2, _a3, _a4)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#override_flag _a2 in
          let _a3 = self#mutable_flag _a3 in
          let _a4 = self#exp _a4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
      | `CrVir (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#private_flag _a2 in
          let _a3 = self#ctyp _a3 in `CrVir (_a0, _a1, _a2, _a3)
      | `CrVvr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#mutable_flag _a2 in
          let _a3 = self#ctyp _a3 in `CrVvr (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cstru)
    method ep : ep -> ep=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>ep)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ep _a2 in `Sem (_a0, _a1, _a2)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#ep _a1 in `Tup (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>ep)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in `Array (_a0, _a1)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>ep)
      | #ant as _a0 -> (self#ant _a0 : ant  :>ep)
    method rec_bind : rec_bind -> rec_bind=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ep _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in
          let _a2 = self#rec_bind _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_bind)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_bind)
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> FanUtil.anti_cxt=
      self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method loc : loc -> 'self_type= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> 'self_type=
      fun (`Ant (_a0,_a1))  ->
        let self = self#loc _a0 in self#fanutil_anti_cxt _a1
    method nil : nil -> 'self_type= fun (`Nil _a0)  -> self#loc _a0
    method literal : literal -> 'self_type=
      function
      | `Chr (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int32 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int64 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Flo (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `NativeInt (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method rec_flag : rec_flag -> 'self_type=
      function
      | `Recursive _a0 -> self#loc _a0
      | `ReNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method direction_flag : direction_flag -> 'self_type=
      function
      | `To _a0 -> self#loc _a0
      | `Downto _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method mutable_flag : mutable_flag -> 'self_type=
      function
      | `Mutable _a0 -> self#loc _a0
      | `MuNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method private_flag : private_flag -> 'self_type=
      function
      | `Private _a0 -> self#loc _a0
      | `PrNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method virtual_flag : virtual_flag -> 'self_type=
      function
      | `Virtual _a0 -> self#loc _a0
      | `ViNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method override_flag : override_flag -> 'self_type=
      function
      | `Override _a0 -> self#loc _a0
      | `OvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method row_var_flag : row_var_flag -> 'self_type=
      function
      | `RowVar _a0 -> self#loc _a0
      | `RvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method position_flag : position_flag -> 'self_type=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | `Normal _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method strings : strings -> 'self_type=
      function
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#strings _a1 in self#strings _a2
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method alident : alident -> 'self_type=
      function
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method auident : auident -> 'self_type=
      function
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method aident : aident -> 'self_type=
      function
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method astring : astring -> 'self_type=
      function
      | `C (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method uident : uident -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method ident : ident -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method dupath : dupath -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#dupath _a2
      | #auident as _a0 -> (self#auident _a0 :>'self_type)
    method dlpath : dlpath -> 'self_type=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#alident _a2
      | #alident as _a0 -> (self#alident _a0 :>'self_type)
    method any : any -> 'self_type= fun (`Any _a0)  -> self#loc _a0
    method sid : sid -> 'self_type=
      fun (`Id (_a0,_a1))  -> let self = self#loc _a0 in self#ident _a1
    method ctyp : ctyp -> 'self_type=
      function
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#alident _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Arrow (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ClassPath (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `TyObj (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#row_var_flag _a2
      | `TyObjEnd (_a0,_a1) ->
          let self = self#loc _a0 in self#row_var_flag _a1
      | `TyPol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `TyPolEnd (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `TyTypePol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `Sta (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `PolyEq (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `PolySup (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `PolyInf (_a0,_a1) -> let self = self#loc _a0 in self#row_field _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `PolyInfSup (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#row_field _a1 in self#tag_names _a2
      | `Package (_a0,_a1) -> let self = self#loc _a0 in self#module_type _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_parameters : type_parameters -> 'self_type=
      function
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_parameters _a1 in self#type_parameters _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method row_field : row_field -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#row_field _a1 in self#row_field _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
      | `TyVrnOf (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#astring _a1 in self#ctyp _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
    method tag_names : tag_names -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#tag_names _a1 in self#tag_names _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
    method typedecl : typedecl -> 'self_type=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#opt_decl_params _a2 in
          let self = self#type_info _a3 in self#opt_type_constr _a4
      | `TyAbstr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#opt_decl_params _a2 in self#opt_type_constr _a3
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#typedecl _a1 in self#typedecl _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_constr : type_constr -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_constr _a1 in self#type_constr _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_type_constr : opt_type_constr -> 'self_type=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#type_constr _a1
      | `None _a0 -> self#loc _a0
    method decl_param : decl_param -> 'self_type=
      function
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Any _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method decl_params : decl_params -> 'self_type=
      function
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Any _a0 -> self#loc _a0
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#decl_params _a1 in self#decl_params _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method opt_decl_params : opt_decl_params -> 'self_type=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#decl_params _a1
      | `None _a0 -> self#loc _a0
    method type_info : type_info -> 'self_type=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in
          let self = self#private_flag _a2 in self#type_repr _a3
      | `TyRepr (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#private_flag _a1 in self#type_repr _a2
      | `TyEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#private_flag _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method type_repr : type_repr -> 'self_type=
      function
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#name_ctyp _a1
      | `Sum (_a0,_a1) -> let self = self#loc _a0 in self#or_ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method name_ctyp : name_ctyp -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#name_ctyp _a2
      | `TyCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | `TyColMut (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method or_ctyp : or_ctyp -> 'self_type=
      function
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#or_ctyp _a1 in self#or_ctyp _a2
      | `TyCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method of_ctyp : of_ctyp -> 'self_type=
      function
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method pat : pat -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_pat _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#alident _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#pat _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#pat _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#pat _a2 in self#exp _a3
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `PaRng (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#ctyp _a2
      | `ClassPath (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `ModuleUnpack (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
    method rec_pat : rec_pat -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_pat _a1 in self#rec_pat _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method exp : exp -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `RecordWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#exp _a2
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `ArrayDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `ExAsf _a0 -> self#loc _a0
      | `ExAsr (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Assign (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#exp _a2 in
          let self = self#exp _a3 in
          let self = self#direction_flag _a4 in self#exp _a5
      | `Fun (_a0,_a1) -> let self = self#loc _a0 in self#case _a1
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in let self = self#exp _a2 in self#exp _a3
      | `IfThen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#rec_flag _a1 in
          let self = self#binding _a2 in self#exp _a3
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_exp _a2 in self#exp _a3
      | `Match (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#case _a2
      | `New (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#cstru _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#cstru _a2
      | `ObjPatEnd (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OvrInst (_a0,_a1) -> let self = self#loc _a0 in self#rec_exp _a1
      | `OvrInstEmpty _a0 -> self#loc _a0
      | `Seq (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Send (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in self#alident _a2
      | `StringDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Try (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#case _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#ctyp _a2
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#exp _a1 in
          let self = self#ctyp _a2 in self#ctyp _a3
      | `Subtype (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#ctyp _a2
      | `While (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `LetOpen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#exp _a2
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Package_exp (_a0,_a1) ->
          let self = self#loc _a0 in self#module_exp _a1
    method rec_exp : rec_exp -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#rec_exp _a2
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#exp _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_type : module_type -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_type _a3
      | `Sig (_a0,_a1) -> let self = self#loc _a0 in self#sig_item _a1
      | `SigEnd _a0 -> self#loc _a0
      | `With (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_type _a1 in self#with_constr _a2
      | `ModuleTypeOf (_a0,_a1) ->
          let self = self#loc _a0 in self#module_exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method sig_item : sig_item -> 'self_type=
      function
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#class_type _a1
      | `ClassType (_a0,_a1) ->
          let self = self#loc _a0 in self#class_type _a1
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#sig_item _a1 in self#sig_item _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#module_type _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_type _a2
      | `RecModule (_a0,_a1) ->
          let self = self#loc _a0 in self#module_binding _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_type _a2
      | `ModuleTypeEnd (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `Open (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
      | `Val (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method with_constr : with_constr -> 'self_type=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `TypeSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#with_constr _a1 in self#with_constr _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method binding : binding -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#binding _a1 in self#binding _a2
      | `Bind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_binding : module_binding -> 'self_type=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_binding _a1 in self#module_binding _a2
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_exp _a3
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_type _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method case : case -> 'self_type=
      function
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#case _a1 in self#case _a2
      | `Case (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in let self = self#exp _a2 in self#exp _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_exp : module_exp -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_exp _a1 in self#module_exp _a2
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_exp _a3
      | `Struct (_a0,_a1) -> let self = self#loc _a0 in self#stru _a1
      | `StructEnd _a0 -> self#loc _a0
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_exp _a1 in self#module_type _a2
      | `PackageModule (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method stru : stru -> 'self_type=
      function
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#class_exp _a1
      | `ClassType (_a0,_a1) ->
          let self = self#loc _a0 in self#class_type _a1
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#stru _a1 in self#stru _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `StExp (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#module_exp _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_exp _a2
      | `RecModule (_a0,_a1) ->
          let self = self#loc _a0 in self#module_binding _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_type _a2
      | `Open (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
      | `Value (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_flag _a1 in self#binding _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method class_type : class_type -> 'self_type=
      function
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in
          let self = self#ident _a2 in self#type_parameters _a3
      | `ClassConS (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in self#ident _a2
      | `CtFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#class_type _a2
      | `ObjTy (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#class_sig_item _a2
      | `ObjTyEnd (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#class_sig_item _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | `CtCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_sig_item _a1 in self#class_sig_item _a2
      | `SigInherit (_a0,_a1) ->
          let self = self#loc _a0 in self#class_type _a1
      | `Method (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#private_flag _a2 in self#ctyp _a3
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#mutable_flag _a2 in
          let self = self#virtual_flag _a3 in self#ctyp _a4
      | `CgVir (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#private_flag _a2 in self#ctyp _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method class_exp : class_exp -> 'self_type=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_exp _a1 in self#exp _a2
      | `ClassCon (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in
          let self = self#ident _a2 in self#type_parameters _a3
      | `ClassConS (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in self#ident _a2
      | `CeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#class_exp _a2
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#rec_flag _a1 in
          let self = self#binding _a2 in self#class_exp _a3
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#cstru _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#cstru _a2
      | `ObjPatEnd (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_exp _a1 in self#class_type _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_exp _a1 in self#class_exp _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_exp _a1 in self#class_exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method cstru : cstru -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cstru _a1 in self#cstru _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Inherit (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#override_flag _a1 in self#class_exp _a2
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#override_flag _a1 in
          let self = self#class_exp _a2 in self#alident _a3
      | `Initializer (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#override_flag _a2 in
          let self = self#private_flag _a3 in
          let self = self#exp _a4 in self#ctyp _a5
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#override_flag _a2 in
          let self = self#private_flag _a3 in self#exp _a4
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#override_flag _a2 in
          let self = self#mutable_flag _a3 in self#exp _a4
      | `CrVir (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#private_flag _a2 in self#ctyp _a3
      | `CrVvr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#mutable_flag _a2 in self#ctyp _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method ep : ep -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_bind _a1
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method rec_bind : rec_bind -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ident _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_bind _a1 in self#rec_bind _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> 'self_type= self#unknown
  end
class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> 'result290=
      fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> 'result291=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> 'result292=
      fun fmt  (`Nil _a0)  ->
        Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
    method literal : 'fmt -> literal -> 'result293=
      fun fmt  ->
        function
        | `Chr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int32 (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Int64 (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Flo (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `NativeInt (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`NativeInt@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Str (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc _a0
              self#string _a1
    method rec_flag : 'fmt -> rec_flag -> 'result294=
      fun fmt  ->
        function
        | `Recursive _a0 ->
            Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" self#loc _a0
        | `ReNil _a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result294)
    method direction_flag : 'fmt -> direction_flag -> 'result295=
      fun fmt  ->
        function
        | `To _a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" self#loc _a0
        | `Downto _a0 ->
            Format.fprintf fmt "@[<1>(`Downto@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result295)
    method mutable_flag : 'fmt -> mutable_flag -> 'result296=
      fun fmt  ->
        function
        | `Mutable _a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" self#loc _a0
        | `MuNil _a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result296)
    method private_flag : 'fmt -> private_flag -> 'result297=
      fun fmt  ->
        function
        | `Private _a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]" self#loc _a0
        | `PrNil _a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result297)
    method virtual_flag : 'fmt -> virtual_flag -> 'result298=
      fun fmt  ->
        function
        | `Virtual _a0 ->
            Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" self#loc _a0
        | `ViNil _a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result298)
    method override_flag : 'fmt -> override_flag -> 'result299=
      fun fmt  ->
        function
        | `Override _a0 ->
            Format.fprintf fmt "@[<1>(`Override@ %a)@]" self#loc _a0
        | `OvNil _a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result299)
    method row_var_flag : 'fmt -> row_var_flag -> 'result300=
      fun fmt  ->
        function
        | `RowVar _a0 ->
            Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" self#loc _a0
        | `RvNil _a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result300)
    method position_flag : 'fmt -> position_flag -> 'result301=
      fun fmt  ->
        function
        | `Positive _a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc _a0
        | `Negative _a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc _a0
        | `Normal _a0 ->
            Format.fprintf fmt "@[<1>(`Normal@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result301)
    method strings : 'fmt -> strings -> 'result302=
      fun fmt  ->
        function
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#strings _a1 self#strings _a2
        | `Str (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result302)
    method alident : 'fmt -> alident -> 'result303=
      fun fmt  ->
        function
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result303)
    method auident : 'fmt -> auident -> 'result304=
      fun fmt  ->
        function
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result304)
    method aident : 'fmt -> aident -> 'result305=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>'result305)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result305)
    method astring : 'fmt -> astring -> 'result306=
      fun fmt  ->
        function
        | `C (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" self#loc _a0 self#string
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result306)
    method uident : 'fmt -> uident -> 'result307=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>'result307)
    method ident : 'fmt -> ident -> 'result308=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>'result308)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result308)
    method dupath : 'fmt -> dupath -> 'result309=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#dupath _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>'result309)
    method dlpath : 'fmt -> dlpath -> 'result310=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#alident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>'result310)
    method any : 'fmt -> any -> 'result311=
      fun fmt  (`Any _a0)  ->
        Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
    method sid : 'fmt -> sid -> 'result312=
      fun fmt  (`Id (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc _a0 self#ident _a1
    method ctyp : 'fmt -> ctyp -> 'result313=
      fun fmt  ->
        function
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#alident _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result313)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Arrow (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `ClassPath (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result313)
        | `TyObj (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1 self#row_var_flag _a2
        | `TyObjEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyObjEnd@ %a@ %a)@]" self#loc _a0
              self#row_var_flag _a1
        | `TyPol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `TyPolEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyPolEnd@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | `TyTypePol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#ctyp
              _a1
        | `Sta (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `PolyEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyEq@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `PolySup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolySup@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `PolyInf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyInf@ %a@ %a)@]" self#loc _a0
              self#row_field _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `PolyInfSup (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" self#loc
              _a0 self#row_field _a1 self#tag_names _a2
        | `Package (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" self#loc _a0
              self#module_type _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result313)
    method type_parameters : 'fmt -> type_parameters -> 'result314=
      fun fmt  ->
        function
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#type_parameters _a1 self#type_parameters _a2
        | `Ctyp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result314)
    method row_field : 'fmt -> row_field -> 'result315=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result315)
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#row_field _a1 self#row_field _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
        | `TyVrnOf (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a@ %a)@]" self#loc _a0
              self#astring _a1 self#ctyp _a2
        | `Ctyp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
    method tag_names : 'fmt -> tag_names -> 'result316=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result316)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#tag_names _a1 self#tag_names _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
    method typedecl : 'fmt -> typedecl -> 'result317=
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#opt_decl_params _a2
              self#type_info _a3 self#opt_type_constr _a4
        | `TyAbstr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#opt_decl_params _a2
              self#opt_type_constr _a3
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1 self#typedecl _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result317)
    method type_constr : 'fmt -> type_constr -> 'result318=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#type_constr _a1 self#type_constr _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result318)
    method opt_type_constr : 'fmt -> opt_type_constr -> 'result319=
      fun fmt  ->
        function
        | `Some (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" self#loc _a0
              self#type_constr _a1
        | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc _a0
    method decl_param : 'fmt -> decl_param -> 'result320=
      fun fmt  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result320)
    method decl_params : 'fmt -> decl_params -> 'result321=
      fun fmt  ->
        function
        | `Quote (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1 self#alident _a2
        | `QuoteAny (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" self#loc _a0
              self#position_flag _a1
        | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#decl_params _a1 self#decl_params _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result321)
    method opt_decl_params : 'fmt -> opt_decl_params -> 'result322=
      fun fmt  ->
        function
        | `Some (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" self#loc _a0
              self#decl_params _a1
        | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc _a0
    method type_info : 'fmt -> type_info -> 'result323=
      fun fmt  ->
        function
        | `TyMan (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#private_flag _a2 self#type_repr _a3
        | `TyRepr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a@ %a)@]" self#loc _a0
              self#private_flag _a1 self#type_repr _a2
        | `TyEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a@ %a)@]" self#loc _a0
              self#private_flag _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result323)
    method type_repr : 'fmt -> type_repr -> 'result324=
      fun fmt  ->
        function
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1
        | `Sum (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" self#loc _a0
              self#or_ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result324)
    method name_ctyp : 'fmt -> name_ctyp -> 'result325=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1 self#name_ctyp _a2
        | `TyCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | `TyColMut (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result325)
    method or_ctyp : 'fmt -> or_ctyp -> 'result326=
      fun fmt  ->
        function
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#or_ctyp _a1 self#or_ctyp _a2
        | `TyCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result326)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result326)
    method of_ctyp : 'fmt -> of_ctyp -> 'result327=
      fun fmt  ->
        function
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result327)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result327)
    method pat : 'fmt -> pat -> 'result328=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result328)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#pat
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result328)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_pat _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result328)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result328)
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#alident _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#pat _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#pat _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OptLablExpr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#pat _a2 self#exp _a3
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `PaRng (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#pat _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#pat _a1 self#ctyp _a2
        | `ClassPath (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0 
              self#pat _a1
        | `ModuleUnpack (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" self#loc _a0
              self#auident _a1
        | `ModuleConstraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#ctyp _a2
    method rec_pat : 'fmt -> rec_pat -> 'result329=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#pat _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_pat _a1 self#rec_pat _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result329)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result329)
    method exp : 'fmt -> exp -> 'result330=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result330)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#exp
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result330)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result330)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result330)
        | `RecordWith (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" self#loc
              _a0 self#rec_exp _a1 self#exp _a2
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `ArrayDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `ExAsf _a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" self#loc _a0
        | `ExAsr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `Assign (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#exp _a2 self#exp _a3
              self#direction_flag _a4 self#exp _a5
        | `Fun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" self#loc _a0 self#case
              _a1
        | `IfThenElse (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#exp _a1 self#exp _a2 self#exp _a3
        | `IfThen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0 
              self#exp _a1
        | `LetIn (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2 self#exp _a3
        | `LetModule (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#auident _a1 self#module_exp _a2 self#exp _a3
        | `Match (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#case _a2
        | `New (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#cstru _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#cstru _a2
        | `ObjPatEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OvrInst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1
        | `OvrInstEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" self#loc _a0
        | `Seq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" self#loc _a0 self#exp
              _a1
        | `Send (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#alident _a2
        | `StringDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `Try (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#case _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#exp _a1 self#ctyp _a2
        | `Coercion (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#exp _a1 self#ctyp _a2 self#ctyp _a3
        | `Subtype (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#ctyp _a2
        | `While (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" self#loc _a0
              self#exp _a1 self#exp _a2
        | `LetOpen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#exp _a2
        | `LocalTypeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#exp _a2
        | `Package_exp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package_exp@ %a@ %a)@]" self#loc _a0
              self#module_exp _a1
    method rec_exp : 'fmt -> rec_exp -> 'result331=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_exp _a1 self#rec_exp _a2
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#exp _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result331)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result331)
    method module_type : 'fmt -> module_type -> 'result332=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result332)
        | `Functor (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2 self#module_type _a3
        | `Sig (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" self#loc _a0
              self#sig_item _a1
        | `SigEnd _a0 ->
            Format.fprintf fmt "@[<1>(`SigEnd@ %a)@]" self#loc _a0
        | `With (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" self#loc _a0
              self#module_type _a1 self#with_constr _a2
        | `ModuleTypeOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" self#loc _a0
              self#module_exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result332)
    method sig_item : 'fmt -> sig_item -> 'result333=
      fun fmt  ->
        function
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#sig_item _a1 self#sig_item _a2
        | `DirectiveSimple (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" self#loc
              _a0 self#alident _a1
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2 self#strings _a3
        | `Include (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc _a0
              self#module_type _a1
        | `Module (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#module_type _a2
        | `RecModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc _a0
              self#module_binding _a1
        | `ModuleType (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2
        | `ModuleTypeEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a@ %a)@]" self#loc _a0
              self#auident _a1
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
        | `Val (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result333)
    method with_constr : 'fmt -> with_constr -> 'result334=
      fun fmt  ->
        function
        | `TypeEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `TypeEqPriv (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a@ %a)@]" self#loc
              _a0 self#ctyp _a1 self#ctyp _a2
        | `ModuleEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `TypeSubst (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `ModuleSubst (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" self#loc
              _a0 self#ident _a1 self#ident _a2
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#with_constr _a1 self#with_constr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result334)
    method binding : 'fmt -> binding -> 'result335=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#binding _a1 self#binding _a2
        | `Bind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#exp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result335)
    method module_binding : 'fmt -> module_binding -> 'result336=
      fun fmt  ->
        function
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#module_binding _a1 self#module_binding _a2
        | `ModuleBind (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#module_type _a2
              self#module_exp _a3
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result336)
    method case : 'fmt -> case -> 'result337=
      fun fmt  ->
        function
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#case _a1 self#case _a2
        | `Case (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#exp _a2
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#pat _a1 self#exp _a2 self#exp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result337)
    method module_exp : 'fmt -> module_exp -> 'result338=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result338)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#module_exp _a1 self#module_exp _a2
        | `Functor (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2 self#module_exp _a3
        | `Struct (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" self#loc _a0
              self#stru _a1
        | `StructEnd _a0 ->
            Format.fprintf fmt "@[<1>(`StructEnd@ %a)@]" self#loc _a0
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#module_exp _a1 self#module_type _a2
        | `PackageModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result338)
    method stru : 'fmt -> stru -> 'result339=
      fun fmt  ->
        function
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#class_exp _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#stru _a1 self#stru _a2
        | `DirectiveSimple (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" self#loc
              _a0 self#alident _a1
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#exp _a2
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `StExp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2 self#strings _a3
        | `Include (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc _a0
              self#module_exp _a1
        | `Module (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#module_exp _a2
        | `RecModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc _a0
              self#module_binding _a1
        | `ModuleType (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
        | `Value (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result339)
    method class_type : 'fmt -> class_type -> 'result340=
      fun fmt  ->
        function
        | `ClassCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#virtual_flag _a1 self#ident _a2 self#type_parameters
              _a3
        | `ClassConS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2
        | `CtFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#class_type _a2
        | `ObjTy (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#class_sig_item _a2
        | `ObjTyEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#class_sig_item _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | `CtCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result340)
    method class_sig_item : 'fmt -> class_sig_item -> 'result341=
      fun fmt  ->
        function
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#class_sig_item _a1 self#class_sig_item _a2
        | `SigInherit (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`SigInherit@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Method (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#private_flag _a2 self#ctyp _a3
        | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#mutable_flag _a2
              self#virtual_flag _a3 self#ctyp _a4
        | `CgVir (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CgVir@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#private_flag _a2 self#ctyp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result341)
    method class_exp : 'fmt -> class_exp -> 'result342=
      fun fmt  ->
        function
        | `CeApp (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" self#loc _a0
              self#class_exp _a1 self#exp _a2
        | `ClassCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#virtual_flag _a1 self#ident _a2 self#type_parameters
              _a3
        | `ClassConS (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2
        | `CeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#class_exp _a2
        | `LetIn (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2 self#class_exp _a3
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#cstru _a1
        | `ObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" self#loc _a0
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#pat _a1 self#cstru _a2
        | `ObjPatEnd (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" self#loc _a0
              self#pat _a1
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#class_exp _a1 self#class_type _a2
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#class_exp _a1 self#class_exp _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#class_exp _a1 self#class_exp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result342)
    method cstru : 'fmt -> cstru -> 'result343=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#cstru _a1 self#cstru _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Inherit (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" self#loc _a0
              self#override_flag _a1 self#class_exp _a2
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#override_flag _a1 self#class_exp _a2
              self#alident _a3
        | `Initializer (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" self#loc _a0
              self#exp _a1
        | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#override_flag _a2
              self#private_flag _a3 self#exp _a4 self#ctyp _a5
        | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#override_flag _a2
              self#private_flag _a3 self#exp _a4
        | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#override_flag _a2
              self#mutable_flag _a3 self#exp _a4
        | `CrVir (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#private_flag _a2 self#ctyp _a3
        | `CrVvr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#mutable_flag _a2 self#ctyp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result343)
    method ep : 'fmt -> ep -> 'result344=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result344)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#ep _a1 self#ep _a2
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#ep
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result344)
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0 
              self#ep _a1
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1
        | #literal as _a0 -> (self#literal fmt _a0 :>'result344)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result344)
    method rec_bind : 'fmt -> rec_bind -> 'result345=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1 self#rec_bind _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result345)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result345)
    method fanloc_t : 'fmt -> FanLoc.t -> 'result346= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> 'result347=
      self#unknown
  end
class eq =
  object (self : 'self_type)
    inherit  eqbase
    method loc : loc -> loc -> 'result348=
      fun _a0  _a1  -> self#fanloc_t _a0 _a1
    method ant : ant -> ant -> 'result349=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#fanutil_anti_cxt _a1 _b1)
    method nil : nil -> nil -> 'result350=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
    method literal : literal -> literal -> 'result351=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Chr (_a0,_a1),`Chr (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Int (_a0,_a1),`Int (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Int32 (_a0,_a1),`Int32 (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Int64 (_a0,_a1),`Int64 (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Flo (_a0,_a1),`Flo (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`NativeInt (_a0,_a1),`NativeInt (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (_,_) -> false
    method rec_flag : rec_flag -> rec_flag -> 'result352=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive _a0,`Recursive _b0) -> self#loc _a0 _b0
        | (`ReNil _a0,`ReNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result352)
        | (_,_) -> false
    method direction_flag : direction_flag -> direction_flag -> 'result353=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To _a0,`To _b0) -> self#loc _a0 _b0
        | (`Downto _a0,`Downto _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result353)
        | (_,_) -> false
    method mutable_flag : mutable_flag -> mutable_flag -> 'result354=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable _a0,`Mutable _b0) -> self#loc _a0 _b0
        | (`MuNil _a0,`MuNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result354)
        | (_,_) -> false
    method private_flag : private_flag -> private_flag -> 'result355=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private _a0,`Private _b0) -> self#loc _a0 _b0
        | (`PrNil _a0,`PrNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result355)
        | (_,_) -> false
    method virtual_flag : virtual_flag -> virtual_flag -> 'result356=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual _a0,`Virtual _b0) -> self#loc _a0 _b0
        | (`ViNil _a0,`ViNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result356)
        | (_,_) -> false
    method override_flag : override_flag -> override_flag -> 'result357=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override _a0,`Override _b0) -> self#loc _a0 _b0
        | (`OvNil _a0,`OvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result357)
        | (_,_) -> false
    method row_var_flag : row_var_flag -> row_var_flag -> 'result358=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar _a0,`RowVar _b0) -> self#loc _a0 _b0
        | (`RvNil _a0,`RvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result358)
        | (_,_) -> false
    method position_flag : position_flag -> position_flag -> 'result359=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive _a0,`Positive _b0) -> self#loc _a0 _b0
        | (`Negative _a0,`Negative _b0) -> self#loc _a0 _b0
        | (`Normal _a0,`Normal _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result359)
        | (_,_) -> false
    method strings : strings -> strings -> 'result360=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#strings _a1 _b1)) &&
              (self#strings _a2 _b2)
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result360)
        | (_,_) -> false
    method alident : alident -> alident -> 'result361=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result361)
        | (_,_) -> false
    method auident : auident -> auident -> 'result362=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result362)
        | (_,_) -> false
    method aident : aident -> aident -> 'result363=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result363)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result363)
        | (_,_) -> false
    method astring : astring -> astring -> 'result364=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C (_a0,_a1),`C (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result364)
        | (_,_) -> false
    method uident : uident -> uident -> 'result365=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#uident _a1 _b1)) &&
              (self#uident _a2 _b2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#uident _a1 _b1)) &&
              (self#uident _a2 _b2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result365)
        | (_,_) -> false
    method ident : ident -> ident -> 'result366=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result366)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result366)
        | (_,_) -> false
    method dupath : dupath -> dupath -> 'result367=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#dupath _a1 _b1)) &&
              (self#dupath _a2 _b2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result367)
        | (_,_) -> false
    method dlpath : dlpath -> dlpath -> 'result368=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#dupath _a1 _b1)) &&
              (self#alident _a2 _b2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result368)
        | (_,_) -> false
    method any : any -> any -> 'result369=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
    method sid : sid -> sid -> 'result370=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
    method ctyp : ctyp -> ctyp -> 'result371=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#alident _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result371)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Arrow (_a0,_a1,_a2),`Arrow (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result371)
        | (`TyObj (_a0,_a1,_a2),`TyObj (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)) &&
              (self#row_var_flag _a2 _b2)
        | (`TyObjEnd (_a0,_a1),`TyObjEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#row_var_flag _a1 _b1)
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`TyPolEnd (_a0,_a1),`TyPolEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | (`TyTypePol (_a0,_a1,_a2),`TyTypePol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#position_flag _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#position_flag _a1 _b1)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Sta (_a0,_a1,_a2),`Sta (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`PolyEq (_a0,_a1),`PolyEq (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#row_field _a1 _b1)
        | (`PolySup (_a0,_a1),`PolySup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#row_field _a1 _b1)
        | (`PolyInf (_a0,_a1),`PolyInf (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#row_field _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`PolyInfSup (_a0,_a1,_a2),`PolyInfSup (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#row_field _a1 _b1)) &&
              (self#tag_names _a2 _b2)
        | (`Package (_a0,_a1),`Package (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_type _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result371)
        | (_,_) -> false
    method type_parameters :
      type_parameters -> type_parameters -> 'result372=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#type_parameters _a1 _b1)) &&
              (self#type_parameters _a2 _b2)
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result372)
        | (_,_) -> false
    method row_field : row_field -> row_field -> 'result373=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result373)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#row_field _a1 _b1)) &&
              (self#row_field _a2 _b2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#astring _a1 _b1)
        | (`TyVrnOf (_a0,_a1,_a2),`TyVrnOf (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#astring _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | (_,_) -> false
    method tag_names : tag_names -> tag_names -> 'result374=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result374)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#tag_names _a1 _b1)) &&
              (self#tag_names _a2 _b2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#astring _a1 _b1)
        | (_,_) -> false
    method typedecl : typedecl -> typedecl -> 'result375=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyDcl (_a0,_a1,_a2,_a3,_a4),`TyDcl (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#opt_decl_params _a2 _b2))
               && (self#type_info _a3 _b3))
              && (self#opt_type_constr _a4 _b4)
        | (`TyAbstr (_a0,_a1,_a2,_a3),`TyAbstr (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#opt_decl_params _a2 _b2))
              && (self#opt_type_constr _a3 _b3)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#typedecl _a1 _b1)) &&
              (self#typedecl _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result375)
        | (_,_) -> false
    method type_constr : type_constr -> type_constr -> 'result376=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#type_constr _a1 _b1)) &&
              (self#type_constr _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result376)
        | (_,_) -> false
    method opt_type_constr :
      opt_type_constr -> opt_type_constr -> 'result377=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#type_constr _a1 _b1)
        | (`None _a0,`None _b0) -> self#loc _a0 _b0
        | (_,_) -> false
    method decl_param : decl_param -> decl_param -> 'result378=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#position_flag _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#position_flag _a1 _b1)
        | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result378)
        | (_,_) -> false
    method decl_params : decl_params -> decl_params -> 'result379=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1,_a2),`Quote (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#position_flag _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`QuoteAny (_a0,_a1),`QuoteAny (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#position_flag _a1 _b1)
        | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#decl_params _a1 _b1)) &&
              (self#decl_params _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result379)
        | (_,_) -> false
    method opt_decl_params :
      opt_decl_params -> opt_decl_params -> 'result380=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some (_a0,_a1),`Some (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#decl_params _a1 _b1)
        | (`None _a0,`None _b0) -> self#loc _a0 _b0
        | (_,_) -> false
    method type_info : type_info -> type_info -> 'result381=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyMan (_a0,_a1,_a2,_a3),`TyMan (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#type_repr _a3 _b3)
        | (`TyRepr (_a0,_a1,_a2),`TyRepr (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#type_repr _a2 _b2)
        | (`TyEq (_a0,_a1,_a2),`TyEq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result381)
        | (_,_) -> false
    method type_repr : type_repr -> type_repr -> 'result382=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#or_ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result382)
        | (_,_) -> false
    method name_ctyp : name_ctyp -> name_ctyp -> 'result383=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)) &&
              (self#name_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`TyColMut (_a0,_a1,_a2),`TyColMut (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result383)
        | (_,_) -> false
    method or_ctyp : or_ctyp -> or_ctyp -> 'result384=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#or_ctyp _a1 _b1)) &&
              (self#or_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result384)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result384)
        | (_,_) -> false
    method of_ctyp : of_ctyp -> of_ctyp -> 'result385=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result385)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result385)
        | (_,_) -> false
    method pat : pat -> pat -> 'result386=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result386)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#pat _a2 _b2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#pat _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#pat _a2 _b2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#pat _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result386)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_pat _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result386)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result386)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#pat _a1 _b1)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#pat _a2 _b2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#pat _a2 _b2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#pat _a2 _b2))
              && (self#exp _a3 _b3)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#pat _a2 _b2)
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#pat _a2 _b2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#pat _a1 _b1)
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#auident _a1 _b1)
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (_,_) -> false
    method rec_pat : rec_pat -> rec_pat -> 'result387=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#pat _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_pat _a1 _b1)) &&
              (self#rec_pat _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result387)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result387)
        | (_,_) -> false
    method exp : exp -> exp -> 'result388=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result388)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result388)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_exp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result388)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result388)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_exp _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`ExAsf _a0,`ExAsf _b0) -> self#loc _a0 _b0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            (((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                 (self#exp _a2 _b2))
                && (self#exp _a3 _b3))
               && (self#direction_flag _a4 _b4))
              && (self#exp _a5 _b5)
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#case _a1 _b1)
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2))
              && (self#exp _a3 _b3)
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
               (self#binding _a2 _b2))
              && (self#exp _a3 _b3)
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_exp _a2 _b2))
              && (self#exp _a3 _b3)
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#case _a2 _b2)
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#cstru _a1 _b1)
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) &&
              (self#cstru _a2 _b2)
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#pat _a1 _b1)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_exp _a1 _b1)
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) -> self#loc _a0 _b0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#case _a2 _b2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#exp _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`Subtype (_a0,_a1,_a2),`Subtype (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Package_exp (_a0,_a1),`Package_exp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_exp _a1 _b1)
        | (_,_) -> false
    method rec_exp : rec_exp -> rec_exp -> 'result389=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_exp _a1 _b1)) &&
              (self#rec_exp _a2 _b2)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result389)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result389)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result390=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result390)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_type _a3 _b3)
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#sig_item _a1 _b1)
        | (`SigEnd _a0,`SigEnd _b0) -> self#loc _a0 _b0
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#with_constr _a2 _b2)
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_exp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result390)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result391=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sig_item _a1 _b1)) &&
              (self#sig_item _a2 _b2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#of_ctyp _a1 _b1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#strings _a3 _b3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_type _a1 _b1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_binding _a1 _b1)
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`ModuleTypeEnd (_a0,_a1),`ModuleTypeEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#auident _a1 _b1)
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#typedecl _a1 _b1)
        | (`Val (_a0,_a1,_a2),`Val (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result391)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result392=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TypeEq (_a0,_a1,_a2),`TypeEq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`TypeEqPriv (_a0,_a1,_a2),`TypeEqPriv (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`ModuleEq (_a0,_a1,_a2),`ModuleEq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`TypeSubst (_a0,_a1,_a2),`TypeSubst (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`ModuleSubst (_a0,_a1,_a2),`ModuleSubst (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#with_constr _a1 _b1)) &&
              (self#with_constr _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result392)
        | (_,_) -> false
    method binding : binding -> binding -> 'result393=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#binding _a2 _b2)
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#exp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result393)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result394=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_binding _a1 _b1)) &&
              (self#module_binding _a2 _b2)
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_exp _a3 _b3)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result394)
        | (_,_) -> false
    method case : case -> case -> 'result395=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#case _a1 _b1)) &&
              (self#case _a2 _b2)
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#exp _a2 _b2)
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#pat _a1 _b1)) && (self#exp _a2 _b2))
              && (self#exp _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result395)
        | (_,_) -> false
    method module_exp : module_exp -> module_exp -> 'result396=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result396)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_exp _a1 _b1)) &&
              (self#module_exp _a2 _b2)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_exp _a3 _b3)
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#stru _a1 _b1)
        | (`StructEnd _a0,`StructEnd _b0) -> self#loc _a0 _b0
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_exp _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result396)
        | (_,_) -> false
    method stru : stru -> stru -> 'result397=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_exp _a1 _b1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#stru _a1 _b1)) &&
              (self#stru _a2 _b2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#of_ctyp _a1 _b1)
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#strings _a3 _b3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_exp _a1 _b1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_exp _a2 _b2)
        | (`RecModule (_a0,_a1),`RecModule (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_binding _a1 _b1)
        | (`ModuleType (_a0,_a1,_a2),`ModuleType (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#typedecl _a1 _b1)
        | (`Value (_a0,_a1,_a2),`Value (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
              (self#binding _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result397)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result398=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#type_parameters _a3 _b3)
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`ObjTy (_a0,_a1,_a2),`ObjTy (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#class_sig_item _a2 _b2)
        | (`ObjTyEnd (_a0,_a1),`ObjTyEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_sig_item _a1 _b1)
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result398)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result399=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_sig_item _a1 _b1)) &&
              (self#class_sig_item _a2 _b2)
        | (`SigInherit (_a0,_a1),`SigInherit (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Method (_a0,_a1,_a2,_a3),`Method (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`CgVal (_a0,_a1,_a2,_a3,_a4),`CgVal (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#mutable_flag _a2 _b2))
               && (self#virtual_flag _a3 _b3))
              && (self#ctyp _a4 _b4)
        | (`CgVir (_a0,_a1,_a2,_a3),`CgVir (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result399)
        | (_,_) -> false
    method class_exp : class_exp -> class_exp -> 'result400=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_exp _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`ClassCon (_a0,_a1,_a2,_a3),`ClassCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#type_parameters _a3 _b3)
        | (`ClassConS (_a0,_a1,_a2),`ClassConS (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) &&
              (self#class_exp _a2 _b2)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
               (self#binding _a2 _b2))
              && (self#class_exp _a3 _b3)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#cstru _a1 _b1)
        | (`ObjEnd _a0,`ObjEnd _b0) -> self#loc _a0 _b0
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#pat _a1 _b1)) &&
              (self#cstru _a2 _b2)
        | (`ObjPatEnd (_a0,_a1),`ObjPatEnd (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#pat _a1 _b1)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_exp _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_exp _a1 _b1)) &&
              (self#class_exp _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_exp _a1 _b1)) &&
              (self#class_exp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result400)
        | (_,_) -> false
    method cstru : cstru -> cstru -> 'result401=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#cstru _a1 _b1)) &&
              (self#cstru _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#override_flag _a1 _b1)) &&
              (self#class_exp _a2 _b2)
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#class_exp _a2 _b2))
              && (self#alident _a3 _b3)
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#exp _a1 _b1)
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            (((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                 (self#override_flag _a2 _b2))
                && (self#private_flag _a3 _b3))
               && (self#exp _a4 _b4))
              && (self#ctyp _a5 _b5)
        | (`CrMthS (_a0,_a1,_a2,_a3,_a4),`CrMthS (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#override_flag _a2 _b2))
               && (self#private_flag _a3 _b3))
              && (self#exp _a4 _b4)
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#override_flag _a2 _b2))
               && (self#mutable_flag _a3 _b3))
              && (self#exp _a4 _b4)
        | (`CrVir (_a0,_a1,_a2,_a3),`CrVir (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`CrVvr (_a0,_a1,_a2,_a3),`CrVvr (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#mutable_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result401)
        | (_,_) -> false
    method ep : ep -> ep -> 'result402=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result402)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ep _a1 _b1)) && (self#ep _a2 _b2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ep _a1 _b1)) && (self#ep _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ep _a1 _b1)) && (self#ep _a2 _b2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ep _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result402)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ep _a1 _b1)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_bind _a1 _b1)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result402)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result402)
        | (_,_) -> false
    method rec_bind : rec_bind -> rec_bind -> 'result403=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) && (self#ep _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_bind _a1 _b1)) &&
              (self#rec_bind _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result403)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result403)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result404= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'result405= self#unknown
  end
let strip_loc_nil (`Nil _a0) = `Nil
let strip_loc_literal =
  function
  | `Chr (_a0,_a1) -> `Chr _a1
  | `Int (_a0,_a1) -> `Int _a1
  | `Int32 (_a0,_a1) -> `Int32 _a1
  | `Int64 (_a0,_a1) -> `Int64 _a1
  | `Flo (_a0,_a1) -> `Flo _a1
  | `NativeInt (_a0,_a1) -> `NativeInt _a1
  | `Str (_a0,_a1) -> `Str _a1
let strip_loc_rec_flag =
  function
  | `Recursive _a0 -> `Recursive
  | `ReNil _a0 -> `ReNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result408)
let strip_loc_direction_flag =
  function
  | `To _a0 -> `To
  | `Downto _a0 -> `Downto
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result409)
let strip_loc_mutable_flag =
  function
  | `Mutable _a0 -> `Mutable
  | `MuNil _a0 -> `MuNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result410)
let strip_loc_private_flag =
  function
  | `Private _a0 -> `Private
  | `PrNil _a0 -> `PrNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result411)
let strip_loc_virtual_flag =
  function
  | `Virtual _a0 -> `Virtual
  | `ViNil _a0 -> `ViNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result412)
let strip_loc_override_flag =
  function
  | `Override _a0 -> `Override
  | `OvNil _a0 -> `OvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result413)
let strip_loc_row_var_flag =
  function
  | `RowVar _a0 -> `RowVar
  | `RvNil _a0 -> `RvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result414)
let strip_loc_position_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result415)
let rec strip_loc_strings =
  function
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_strings _a1 in
      let _a2 = strip_loc_strings _a2 in `App (_a1, _a2)
  | `Str (_a0,_a1) -> `Str _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result416)
let strip_loc_alident =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result417)
let strip_loc_auident =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result418)
let strip_loc_aident =
  function
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result419)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result419)
let strip_loc_astring =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result420)
let rec strip_loc_uident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `App (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result421)
let rec strip_loc_ident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `App (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result422)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result422)
let rec strip_loc_dupath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_dupath _a2 in `Dot (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result423)
let strip_loc_dlpath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_alident _a2 in `Dot (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result424)
let strip_loc_any (`Any _a0) = `Any
let strip_loc_sid (`Id (_a0,_a1)) = let _a1 = strip_loc_ident _a1 in `Id _a1
let rec strip_loc_ctyp =
  function
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result457)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `App (_a1, _a2)
  | `Arrow (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Arrow (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `OptLabl (_a1, _a2)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result457)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_name_ctyp _a1 in
      let _a2 = strip_loc_row_var_flag _a2 in `TyObj (_a1, _a2)
  | `TyObjEnd (_a0,_a1) ->
      let _a1 = strip_loc_row_var_flag _a1 in `TyObjEnd _a1
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyPol (_a1, _a2)
  | `TyPolEnd (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `TyPolEnd _a1
  | `TyTypePol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyTypePol (_a1, _a2)
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Tup _a1
  | `Sta (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Sta (_a1, _a2)
  | `PolyEq (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolyEq _a1
  | `PolySup (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolySup _a1
  | `PolyInf (_a0,_a1) -> let _a1 = strip_loc_row_field _a1 in `PolyInf _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Com (_a1, _a2)
  | `PolyInfSup (_a0,_a1,_a2) ->
      let _a1 = strip_loc_row_field _a1 in
      let _a2 = strip_loc_tag_names _a2 in `PolyInfSup (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = strip_loc_module_type _a1 in `Package _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result457)
and strip_loc_type_parameters =
  function
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_type_parameters _a1 in
      let _a2 = strip_loc_type_parameters _a2 in `Com (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Ctyp _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result456)
and strip_loc_row_field =
  function
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result455)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_row_field _a1 in
      let _a2 = strip_loc_row_field _a2 in `Or (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
  | `TyVrnOf (_a0,_a1,_a2) ->
      let _a1 = strip_loc_astring _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyVrnOf (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Ctyp _a1
and strip_loc_tag_names =
  function
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result454)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_tag_names _a1 in
      let _a2 = strip_loc_tag_names _a2 in `App (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
and strip_loc_typedecl =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_opt_decl_params _a2 in
      let _a3 = strip_loc_type_info _a3 in
      let _a4 = strip_loc_opt_type_constr _a4 in `TyDcl (_a1, _a2, _a3, _a4)
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_opt_decl_params _a2 in
      let _a3 = strip_loc_opt_type_constr _a3 in `TyAbstr (_a1, _a2, _a3)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_typedecl _a1 in
      let _a2 = strip_loc_typedecl _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result453)
and strip_loc_type_constr =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_type_constr _a1 in
      let _a2 = strip_loc_type_constr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result452)
and strip_loc_opt_type_constr =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_loc_type_constr _a1 in `Some _a1
  | `None _a0 -> `None
and strip_loc_decl_param =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result450)
and strip_loc_decl_params =
  function
  | `Quote (_a0,_a1,_a2) ->
      let _a1 = strip_loc_position_flag _a1 in
      let _a2 = strip_loc_alident _a2 in `Quote (_a1, _a2)
  | `QuoteAny (_a0,_a1) ->
      let _a1 = strip_loc_position_flag _a1 in `QuoteAny _a1
  | `Any _a0 -> `Any
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_decl_params _a1 in
      let _a2 = strip_loc_decl_params _a2 in `Com (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result449)
and strip_loc_opt_decl_params =
  function
  | `Some (_a0,_a1) -> let _a1 = strip_loc_decl_params _a1 in `Some _a1
  | `None _a0 -> `None
and strip_loc_type_info =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_type_repr _a3 in `TyMan (_a1, _a2, _a3)
  | `TyRepr (_a0,_a1,_a2) ->
      let _a1 = strip_loc_private_flag _a1 in
      let _a2 = strip_loc_type_repr _a2 in `TyRepr (_a1, _a2)
  | `TyEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_private_flag _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyEq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result447)
and strip_loc_type_repr =
  function
  | `Record (_a0,_a1) -> let _a1 = strip_loc_name_ctyp _a1 in `Record _a1
  | `Sum (_a0,_a1) -> let _a1 = strip_loc_or_ctyp _a1 in `Sum _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result446)
and strip_loc_name_ctyp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_name_ctyp _a1 in
      let _a2 = strip_loc_name_ctyp _a2 in `Sem (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyCol (_a1, _a2)
  | `TyColMut (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyColMut (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result445)
and strip_loc_or_ctyp =
  function
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_or_ctyp _a1 in
      let _a2 = strip_loc_or_ctyp _a2 in `Or (_a1, _a2)
  | `TyCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyCol (_a1, _a2)
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result444)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result444)
and strip_loc_of_ctyp =
  function
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result443)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result443)
and strip_loc_pat =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result442)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Sem (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Tup _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result442)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_pat _a1 in `Record _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result442)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result442)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Array _a1
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_pat _a2 in
      let _a3 = strip_loc_exp _a3 in `OptLablExpr (_a1, _a2, _a3)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `Or (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_pat _a2 in `PaRng (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1) ->
      let _a1 = strip_loc_auident _a1 in `ModuleUnpack _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `ModuleConstraint (_a1, _a2)
and strip_loc_rec_pat =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_pat _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_pat _a1 in
      let _a2 = strip_loc_rec_pat _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result441)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result441)
and strip_loc_exp =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result440)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Sem (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Tup _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result440)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_exp _a1 in `Record _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result440)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result440)
  | `RecordWith (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `RecordWith (_a1, _a2)
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Dot (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `ArrayDot (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Array _a1
  | `ExAsf _a0 -> `ExAsf
  | `ExAsr (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `ExAsr _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in
      let _a4 = strip_loc_direction_flag _a4 in
      let _a5 = strip_loc_exp _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = strip_loc_case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `IfThen (_a1, _a2)
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_exp _a3 in `LetIn (_a1, _a2, _a3)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_exp _a2 in
      let _a3 = strip_loc_exp _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `New _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_cstru _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_cstru _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `ObjPatEnd _a1
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OvrInst (_a0,_a1) -> let _a1 = strip_loc_rec_exp _a1 in `OvrInst _a1
  | `OvrInstEmpty _a0 -> `OvrInstEmpty
  | `Seq (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_case _a2 in `Try (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `Subtype (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Subtype (_a1, _a2)
  | `While (_a0,_a1,_a2) ->
      let _a1 = strip_loc_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_exp _a2 in `LetOpen (_a1, _a2)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_exp (_a0,_a1) ->
      let _a1 = strip_loc_module_exp _a1 in `Package_exp _a1
and strip_loc_rec_exp =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_exp _a1 in
      let _a2 = strip_loc_rec_exp _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_exp _a2 in `RecBind (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result439)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result439)
and strip_loc_module_type =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result438)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_type _a3 in `Functor (_a1, _a2, _a3)
  | `Sig (_a0,_a1) -> let _a1 = strip_loc_sig_item _a1 in `Sig _a1
  | `SigEnd _a0 -> `SigEnd
  | `With (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_type _a1 in
      let _a2 = strip_loc_with_constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) ->
      let _a1 = strip_loc_module_exp _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result438)
and strip_loc_sig_item =
  function
  | `Class (_a0,_a1) -> let _a1 = strip_loc_class_type _a1 in `Class _a1
  | `ClassType (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sig_item _a1 in
      let _a2 = strip_loc_sig_item _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_loc_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_of_ctyp _a1 in `Exception _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_module_type _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) ->
      let _a1 = strip_loc_module_binding _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `ModuleType (_a1, _a2)
  | `ModuleTypeEnd (_a0,_a1) ->
      let _a1 = strip_loc_auident _a1 in `ModuleTypeEnd _a1
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_typedecl _a1 in `Type _a1
  | `Val (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Val (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result437)
and strip_loc_with_constr =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeEq (_a1, _a2)
  | `TypeEqPriv (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeEqPriv (_a1, _a2)
  | `ModuleEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleEq (_a1, _a2)
  | `TypeSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TypeSubst (_a1, _a2)
  | `ModuleSubst (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `ModuleSubst (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_with_constr _a1 in
      let _a2 = strip_loc_with_constr _a2 in `And (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result436)
and strip_loc_binding =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_binding _a1 in
      let _a2 = strip_loc_binding _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result435)
and strip_loc_module_binding =
  function
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_binding _a1 in
      let _a2 = strip_loc_module_binding _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_exp _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result434)
and strip_loc_case =
  function
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_case _a1 in
      let _a2 = strip_loc_case _a2 in `Or (_a1, _a2)
  | `Case (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in `Case (_a1, _a2)
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_exp _a2 in
      let _a3 = strip_loc_exp _a3 in `CaseWhen (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result433)
and strip_loc_module_exp =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result432)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_exp _a1 in
      let _a2 = strip_loc_module_exp _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_exp _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = strip_loc_stru _a1 in `Struct _a1
  | `StructEnd _a0 -> `StructEnd
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_exp _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) ->
      let _a1 = strip_loc_exp _a1 in `PackageModule _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result432)
and strip_loc_stru =
  function
  | `Class (_a0,_a1) -> let _a1 = strip_loc_class_exp _a1 in `Class _a1
  | `ClassType (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_stru _a1 in
      let _a2 = strip_loc_stru _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_loc_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_exp _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_of_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_module_exp _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_exp _a2 in `Module (_a1, _a2)
  | `RecModule (_a0,_a1) ->
      let _a1 = strip_loc_module_binding _a1 in `RecModule _a1
  | `ModuleType (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `ModuleType (_a1, _a2)
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_typedecl _a1 in `Type _a1
  | `Value (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in `Value (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result431)
and strip_loc_class_type =
  function
  | `ClassCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in `ClassCon (_a1, _a2, _a3)
  | `ClassConS (_a0,_a1,_a2) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in `ClassConS (_a1, _a2)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtFun (_a1, _a2)
  | `ObjTy (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_sig_item _a2 in `ObjTy (_a1, _a2)
  | `ObjTyEnd (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `ObjTyEnd _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_class_sig_item _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `And (_a1, _a2)
  | `CtCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtCol (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result430)
and strip_loc_class_sig_item =
  function
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_sig_item _a1 in
      let _a2 = strip_loc_class_sig_item _a2 in `Sem (_a1, _a2)
  | `SigInherit (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `SigInherit _a1
  | `Method (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Method (_a1, _a2, _a3)
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_mutable_flag _a2 in
      let _a3 = strip_loc_virtual_flag _a3 in
      let _a4 = strip_loc_ctyp _a4 in `CgVal (_a1, _a2, _a3, _a4)
  | `CgVir (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CgVir (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result429)
and strip_loc_class_exp =
  function
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_exp _a1 in
      let _a2 = strip_loc_exp _a2 in `CeApp (_a1, _a2)
  | `ClassCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in `ClassCon (_a1, _a2, _a3)
  | `ClassConS (_a0,_a1,_a2) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in `ClassConS (_a1, _a2)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_class_exp _a2 in `CeFun (_a1, _a2)
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_class_exp _a3 in `LetIn (_a1, _a2, _a3)
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_cstru _a1 in `Obj _a1
  | `ObjEnd _a0 -> `ObjEnd
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_pat _a1 in
      let _a2 = strip_loc_cstru _a2 in `ObjPat (_a1, _a2)
  | `ObjPatEnd (_a0,_a1) -> let _a1 = strip_loc_pat _a1 in `ObjPatEnd _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_exp _a1 in
      let _a2 = strip_loc_class_type _a2 in `Constraint (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_exp _a1 in
      let _a2 = strip_loc_class_exp _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_exp _a1 in
      let _a2 = strip_loc_class_exp _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result428)
and strip_loc_cstru =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_cstru _a1 in
      let _a2 = strip_loc_cstru _a2 in `Sem (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Inherit (_a0,_a1,_a2) ->
      let _a1 = strip_loc_override_flag _a1 in
      let _a2 = strip_loc_class_exp _a2 in `Inherit (_a1, _a2)
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_override_flag _a1 in
      let _a2 = strip_loc_class_exp _a2 in
      let _a3 = strip_loc_alident _a3 in `InheritAs (_a1, _a2, _a3)
  | `Initializer (_a0,_a1) -> let _a1 = strip_loc_exp _a1 in `Initializer _a1
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_private_flag _a3 in
      let _a4 = strip_loc_exp _a4 in
      let _a5 = strip_loc_ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_private_flag _a3 in
      let _a4 = strip_loc_exp _a4 in `CrMthS (_a1, _a2, _a3, _a4)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_mutable_flag _a3 in
      let _a4 = strip_loc_exp _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `CrVir (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVir (_a1, _a2, _a3)
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_mutable_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVvr (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result427)
let rec strip_loc_ep =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result459)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ep _a1 in
      let _a2 = strip_loc_ep _a2 in `Sem (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_ep _a1 in `Tup _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result459)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_ep _a1 in `Array _a1
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_bind _a1 in `Record _a1
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result459)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result459)
and strip_loc_rec_bind =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ep _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_bind _a1 in
      let _a2 = strip_loc_rec_bind _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result458)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result458)
let pp_print_loc fmt _a0 = FanLoc.pp_print_t fmt _a0
let pp_print_ant fmt (`Ant (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
    FanUtil.pp_print_anti_cxt _a1
let pp_print_nil fmt (`Nil _a0) =
  Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc _a0
let pp_print_literal fmt =
  function
  | `Chr (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Int (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Int32 (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Int64 (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Flo (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `NativeInt (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`NativeInt@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Str (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
let pp_print_rec_flag fmt =
  function
  | `Recursive _a0 ->
      Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" pp_print_loc _a0
  | `ReNil _a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result464)
let pp_print_direction_flag fmt =
  function
  | `To _a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" pp_print_loc _a0
  | `Downto _a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result465)
let pp_print_mutable_flag fmt =
  function
  | `Mutable _a0 ->
      Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" pp_print_loc _a0
  | `MuNil _a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result466)
let pp_print_private_flag fmt =
  function
  | `Private _a0 ->
      Format.fprintf fmt "@[<1>(`Private@ %a)@]" pp_print_loc _a0
  | `PrNil _a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result467)
let pp_print_virtual_flag fmt =
  function
  | `Virtual _a0 ->
      Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" pp_print_loc _a0
  | `ViNil _a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result468)
let pp_print_override_flag fmt =
  function
  | `Override _a0 ->
      Format.fprintf fmt "@[<1>(`Override@ %a)@]" pp_print_loc _a0
  | `OvNil _a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result469)
let pp_print_row_var_flag fmt =
  function
  | `RowVar _a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" pp_print_loc _a0
  | `RvNil _a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result470)
let pp_print_position_flag fmt =
  function
  | `Positive _a0 ->
      Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
  | `Negative _a0 ->
      Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
  | `Normal _a0 -> Format.fprintf fmt "@[<1>(`Normal@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result471)
let rec pp_print_strings fmt =
  function
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_strings _a1 pp_print_strings _a2
  | `Str (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result472)
let pp_print_alident fmt =
  function
  | `Lid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result473)
let pp_print_auident fmt =
  function
  | `Uid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result474)
let pp_print_aident fmt =
  function
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result475)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result475)
let pp_print_astring fmt =
  function
  | `C (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result476)
let rec pp_print_uident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result477)
let rec pp_print_ident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result478)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result478)
let rec pp_print_dupath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_dupath _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result479)
let pp_print_dlpath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_alident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result480)
let pp_print_any fmt (`Any _a0) =
  Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
let pp_print_sid fmt (`Id (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc _a0 pp_print_ident
    _a1
let rec pp_print_ctyp fmt =
  function
  | `Alias (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_alident _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result513)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Arrow (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `ClassPath (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Label (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_ctyp _a2
  | `OptLabl (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_ctyp _a2
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result513)
  | `TyObj (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1 pp_print_row_var_flag _a2
  | `TyObjEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`TyObjEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_var_flag _a1
  | `TyPol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `TyPolEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`TyPolEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | `TyTypePol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Quote (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1 pp_print_alident _a2
  | `QuoteAny (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | `Sta (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `PolyEq (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PolyEq@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1
  | `PolySup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PolySup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1
  | `PolyInf (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PolyInf@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `PolyInfSup (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1 pp_print_tag_names _a2
  | `Package (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_type _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result513)
and pp_print_type_parameters fmt =
  function
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_parameters _a1 pp_print_type_parameters _a2
  | `Ctyp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result512)
and pp_print_row_field fmt =
  function
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result511)
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1 pp_print_row_field _a2
  | `TyVrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_astring _a1
  | `TyVrnOf (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_astring _a1 pp_print_ctyp _a2
  | `Ctyp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
and pp_print_tag_names fmt =
  function
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result510)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_tag_names _a1 pp_print_tag_names _a2
  | `TyVrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_astring _a1
and pp_print_typedecl fmt =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_opt_decl_params _a2
        pp_print_type_info _a3 pp_print_opt_type_constr _a4
  | `TyAbstr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_opt_decl_params _a2
        pp_print_opt_type_constr _a3
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1 pp_print_typedecl _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result509)
and pp_print_type_constr fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_constr _a1 pp_print_type_constr _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result508)
and pp_print_opt_type_constr fmt =
  function
  | `Some (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_constr _a1
  | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_decl_param fmt =
  function
  | `Quote (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1 pp_print_alident _a2
  | `QuoteAny (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1
  | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result506)
and pp_print_decl_params fmt =
  function
  | `Quote (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1 pp_print_alident _a2
  | `QuoteAny (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`QuoteAny@ %a@ %a)@]" pp_print_loc _a0
        pp_print_position_flag _a1
  | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_decl_params _a1 pp_print_decl_params _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result505)
and pp_print_opt_decl_params fmt =
  function
  | `Some (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Some@ %a@ %a)@]" pp_print_loc _a0
        pp_print_decl_params _a1
  | `None _a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc _a0
and pp_print_type_info fmt =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_private_flag _a2 pp_print_type_repr _a3
  | `TyRepr (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_private_flag _a1 pp_print_type_repr _a2
  | `TyEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_private_flag _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result503)
and pp_print_type_repr fmt =
  function
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1
  | `Sum (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" pp_print_loc _a0
        pp_print_or_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result502)
and pp_print_name_ctyp fmt =
  function
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1 pp_print_name_ctyp _a2
  | `TyCol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | `TyColMut (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result501)
and pp_print_or_ctyp fmt =
  function
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_or_ctyp _a1 pp_print_or_ctyp _a2
  | `TyCol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | `Of (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result500)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result500)
and pp_print_of_ctyp fmt =
  function
  | `Of (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result499)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result499)
and pp_print_pat fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result498)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `Vrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result498)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_pat _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result498)
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result498)
  | `Alias (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_alident _a2
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | `LabelS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Label (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_pat _a2
  | `OptLabl (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_pat _a2
  | `OptLablS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_pat _a2 pp_print_exp _a3
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `PaRng (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_pat _a2
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_ctyp _a2
  | `ClassPath (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Lazy (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | `ModuleUnpack (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
        pp_print_loc _a0 pp_print_auident _a1 pp_print_ctyp _a2
and pp_print_rec_pat fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_pat _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_pat _a1 pp_print_rec_pat _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result497)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result497)
and pp_print_exp fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result496)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `Vrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result496)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result496)
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result496)
  | `RecordWith (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1 pp_print_exp _a2
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `ArrayDot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `ExAsf _a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" pp_print_loc _a0
  | `ExAsr (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `Assign (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_exp _a2 pp_print_exp _a3
        pp_print_direction_flag _a4 pp_print_exp _a5
  | `Fun (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" pp_print_loc _a0
        pp_print_case _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_exp _a1 pp_print_exp _a2 pp_print_exp _a3
  | `IfThen (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `LabelS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Label (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_exp _a2
  | `Lazy (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_flag _a1 pp_print_binding _a2 pp_print_exp _a3
  | `LetModule (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_auident _a1 pp_print_module_exp _a2 pp_print_exp _a3
  | `Match (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_case _a2
  | `New (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Obj (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cstru _a1
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
  | `ObjPat (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_cstru _a2
  | `ObjPatEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | `OptLabl (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_exp _a2
  | `OptLablS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `OvrInst (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1
  | `OvrInstEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" pp_print_loc _a0
  | `Seq (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `Send (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_alident _a2
  | `StringDot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `Try (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_case _a2
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_ctyp _a2
  | `Coercion (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_exp _a1 pp_print_ctyp _a2 pp_print_ctyp _a3
  | `Subtype (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_ctyp _a2
  | `While (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1 pp_print_exp _a2
  | `LetOpen (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_exp _a2
  | `LocalTypeFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_exp _a2
  | `Package_exp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package_exp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_exp _a1
and pp_print_rec_exp fmt =
  function
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_exp _a1 pp_print_rec_exp _a2
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_exp _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result495)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result495)
and pp_print_module_type fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result494)
  | `Functor (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2 pp_print_module_type
        _a3
  | `Sig (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sig_item _a1
  | `SigEnd _a0 -> Format.fprintf fmt "@[<1>(`SigEnd@ %a)@]" pp_print_loc _a0
  | `With (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_type _a1 pp_print_with_constr _a2
  | `ModuleTypeOf (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_exp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result494)
and pp_print_sig_item fmt =
  function
  | `Class (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1
  | `ClassType (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sig_item _a1 pp_print_sig_item _a2
  | `DirectiveSimple (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Directive (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_exp _a2
  | `Exception (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc _a0
        pp_print_of_ctyp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_ctyp _a2 pp_print_strings _a3
  | `Include (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_type _a1
  | `Module (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2
  | `RecModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_binding _a1
  | `ModuleType (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2
  | `ModuleTypeEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1
  | `Open (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Type (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1
  | `Val (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result493)
and pp_print_with_constr fmt =
  function
  | `TypeEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `TypeEqPriv (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `ModuleEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `TypeSubst (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `ModuleSubst (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_with_constr _a1 pp_print_with_constr _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result492)
and pp_print_binding fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_binding _a1 pp_print_binding _a2
  | `Bind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_exp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result491)
and pp_print_module_binding fmt =
  function
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_binding _a1 pp_print_module_binding _a2
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_auident _a1 pp_print_module_type _a2 pp_print_module_exp
        _a3
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result490)
and pp_print_case fmt =
  function
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_case _a1 pp_print_case _a2
  | `Case (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_exp _a2
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_pat _a1 pp_print_exp _a2 pp_print_exp _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result489)
and pp_print_module_exp fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result488)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_exp _a1 pp_print_module_exp _a2
  | `Functor (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2 pp_print_module_exp _a3
  | `Struct (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" pp_print_loc _a0
        pp_print_stru _a1
  | `StructEnd _a0 ->
      Format.fprintf fmt "@[<1>(`StructEnd@ %a)@]" pp_print_loc _a0
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_exp _a1 pp_print_module_type _a2
  | `PackageModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result488)
and pp_print_stru fmt =
  function
  | `Class (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_exp _a1
  | `ClassType (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_stru _a1 pp_print_stru _a2
  | `DirectiveSimple (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Directive (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_exp _a2
  | `Exception (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc _a0
        pp_print_of_ctyp _a1
  | `StExp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_ctyp _a2 pp_print_strings _a3
  | `Include (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_exp _a1
  | `Module (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_exp _a2
  | `RecModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_binding _a1
  | `ModuleType (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2
  | `Open (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Type (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1
  | `Value (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_flag _a1 pp_print_binding _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result487)
and pp_print_class_type fmt =
  function
  | `ClassCon (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_virtual_flag _a1 pp_print_ident _a2
        pp_print_type_parameters _a3
  | `ClassConS (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_virtual_flag _a1 pp_print_ident _a2
  | `CtFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_class_type _a2
  | `ObjTy (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_class_sig_item _a2
  | `ObjTyEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | `Obj (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_sig_item _a1
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | `CtCol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result486)
and pp_print_class_sig_item fmt =
  function
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_sig_item _a1 pp_print_class_sig_item _a2
  | `SigInherit (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`SigInherit@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1
  | `Method (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_private_flag _a2 pp_print_ctyp _a3
  | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_mutable_flag _a2
        pp_print_virtual_flag _a3 pp_print_ctyp _a4
  | `CgVir (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CgVir@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_private_flag _a2 pp_print_ctyp _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result485)
and pp_print_class_exp fmt =
  function
  | `CeApp (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_exp _a1 pp_print_exp _a2
  | `ClassCon (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_virtual_flag _a1 pp_print_ident _a2
        pp_print_type_parameters _a3
  | `ClassConS (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_virtual_flag _a1 pp_print_ident _a2
  | `CeFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_class_exp _a2
  | `LetIn (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_flag _a1 pp_print_binding _a2 pp_print_class_exp _a3
  | `Obj (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cstru _a1
  | `ObjEnd _a0 -> Format.fprintf fmt "@[<1>(`ObjEnd@ %a)@]" pp_print_loc _a0
  | `ObjPat (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1 pp_print_cstru _a2
  | `ObjPatEnd (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a@ %a)@]" pp_print_loc _a0
        pp_print_pat _a1
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_exp _a1 pp_print_class_type _a2
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_exp _a1 pp_print_class_exp _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_exp _a1 pp_print_class_exp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result484)
and pp_print_cstru fmt =
  function
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_cstru _a1 pp_print_cstru _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Inherit (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_override_flag _a1 pp_print_class_exp _a2
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_override_flag _a1 pp_print_class_exp _a2
        pp_print_alident _a3
  | `Initializer (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" pp_print_loc _a0
        pp_print_exp _a1
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
        pp_print_loc _a0 pp_print_alident _a1 pp_print_override_flag _a2
        pp_print_private_flag _a3 pp_print_exp _a4 pp_print_ctyp _a5
  | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_override_flag _a2
        pp_print_private_flag _a3 pp_print_exp _a4
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_override_flag _a2
        pp_print_mutable_flag _a3 pp_print_exp _a4
  | `CrVir (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_private_flag _a2 pp_print_ctyp _a3
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_mutable_flag _a2 pp_print_ctyp _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result483)
let rec pp_print_ep fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result515)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1 pp_print_ep _a2
  | `Vrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1 pp_print_ep _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1 pp_print_ep _a2
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0 pp_print_ep
        _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result515)
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result515)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result515)
and pp_print_rec_bind fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ep _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1 pp_print_rec_bind _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result514)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result514)
let map_loc f =
  object  inherit  map as super method! loc x = f (super#loc x) end
let map_ant f =
  object  inherit  map as super method! ant x = f (super#ant x) end
let map_nil f =
  object  inherit  map as super method! nil x = f (super#nil x) end
let map_literal f =
  object  inherit  map as super method! literal x = f (super#literal x) end
let map_rec_flag f =
  object  inherit  map as super method! rec_flag x = f (super#rec_flag x) end
let map_direction_flag f =
  object 
    inherit  map as super
    method! direction_flag x = f (super#direction_flag x)
  end
let map_mutable_flag f =
  object 
    inherit  map as super
    method! mutable_flag x = f (super#mutable_flag x)
  end
let map_private_flag f =
  object 
    inherit  map as super
    method! private_flag x = f (super#private_flag x)
  end
let map_virtual_flag f =
  object 
    inherit  map as super
    method! virtual_flag x = f (super#virtual_flag x)
  end
let map_override_flag f =
  object 
    inherit  map as super
    method! override_flag x = f (super#override_flag x)
  end
let map_row_var_flag f =
  object 
    inherit  map as super
    method! row_var_flag x = f (super#row_var_flag x)
  end
let map_position_flag f =
  object 
    inherit  map as super
    method! position_flag x = f (super#position_flag x)
  end
let map_strings f =
  object  inherit  map as super method! strings x = f (super#strings x) end
let map_alident f =
  object  inherit  map as super method! alident x = f (super#alident x) end
let map_auident f =
  object  inherit  map as super method! auident x = f (super#auident x) end
let map_aident f =
  object  inherit  map as super method! aident x = f (super#aident x) end
let map_astring f =
  object  inherit  map as super method! astring x = f (super#astring x) end
let map_uident f =
  object  inherit  map as super method! uident x = f (super#uident x) end
let map_ident f =
  object  inherit  map as super method! ident x = f (super#ident x) end
let map_dupath f =
  object  inherit  map as super method! dupath x = f (super#dupath x) end
let map_dlpath f =
  object  inherit  map as super method! dlpath x = f (super#dlpath x) end
let map_any f =
  object  inherit  map as super method! any x = f (super#any x) end
let map_sid f =
  object  inherit  map as super method! sid x = f (super#sid x) end
let map_ctyp f =
  object  inherit  map as super method! ctyp x = f (super#ctyp x) end
let map_type_parameters f =
  object 
    inherit  map as super
    method! type_parameters x = f (super#type_parameters x)
  end
let map_row_field f =
  object  inherit  map as super method! row_field x = f (super#row_field x)
  end
let map_tag_names f =
  object  inherit  map as super method! tag_names x = f (super#tag_names x)
  end
let map_typedecl f =
  object  inherit  map as super method! typedecl x = f (super#typedecl x) end
let map_type_constr f =
  object 
    inherit  map as super
    method! type_constr x = f (super#type_constr x)
  end
let map_opt_type_constr f =
  object 
    inherit  map as super
    method! opt_type_constr x = f (super#opt_type_constr x)
  end
let map_decl_param f =
  object  inherit  map as super method! decl_param x = f (super#decl_param x)
  end
let map_decl_params f =
  object 
    inherit  map as super
    method! decl_params x = f (super#decl_params x)
  end
let map_opt_decl_params f =
  object 
    inherit  map as super
    method! opt_decl_params x = f (super#opt_decl_params x)
  end
let map_type_info f =
  object  inherit  map as super method! type_info x = f (super#type_info x)
  end
let map_type_repr f =
  object  inherit  map as super method! type_repr x = f (super#type_repr x)
  end
let map_name_ctyp f =
  object  inherit  map as super method! name_ctyp x = f (super#name_ctyp x)
  end
let map_or_ctyp f =
  object  inherit  map as super method! or_ctyp x = f (super#or_ctyp x) end
let map_of_ctyp f =
  object  inherit  map as super method! of_ctyp x = f (super#of_ctyp x) end
let map_pat f =
  object  inherit  map as super method! pat x = f (super#pat x) end
let map_rec_pat f =
  object  inherit  map as super method! rec_pat x = f (super#rec_pat x) end
let map_exp f =
  object  inherit  map as super method! exp x = f (super#exp x) end
let map_rec_exp f =
  object  inherit  map as super method! rec_exp x = f (super#rec_exp x) end
let map_module_type f =
  object 
    inherit  map as super
    method! module_type x = f (super#module_type x)
  end
let map_sig_item f =
  object  inherit  map as super method! sig_item x = f (super#sig_item x) end
let map_with_constr f =
  object 
    inherit  map as super
    method! with_constr x = f (super#with_constr x)
  end
let map_binding f =
  object  inherit  map as super method! binding x = f (super#binding x) end
let map_module_binding f =
  object 
    inherit  map as super
    method! module_binding x = f (super#module_binding x)
  end
let map_case f =
  object  inherit  map as super method! case x = f (super#case x) end
let map_module_exp f =
  object  inherit  map as super method! module_exp x = f (super#module_exp x)
  end
let map_stru f =
  object  inherit  map as super method! stru x = f (super#stru x) end
let map_class_type f =
  object  inherit  map as super method! class_type x = f (super#class_type x)
  end
let map_class_sig_item f =
  object 
    inherit  map as super
    method! class_sig_item x = f (super#class_sig_item x)
  end
let map_class_exp f =
  object  inherit  map as super method! class_exp x = f (super#class_exp x)
  end
let map_cstru f =
  object  inherit  map as super method! cstru x = f (super#cstru x) end
let map_ep f =
  object  inherit  map as super method! ep x = f (super#ep x) end
let map_rec_bind f =
  object  inherit  map as super method! rec_bind x = f (super#rec_bind x) end
let dump = new print
let dump_literal = LibUtil.to_string_of_printer dump#literal
let dump_rec_flag = LibUtil.to_string_of_printer dump#rec_flag
let dump_direction_flag = LibUtil.to_string_of_printer dump#direction_flag
let dump_mutable_flag = LibUtil.to_string_of_printer dump#mutable_flag
let dump_private_flag = LibUtil.to_string_of_printer dump#private_flag
let dump_virtual_flag = LibUtil.to_string_of_printer dump#virtual_flag
let dump_override_flag = LibUtil.to_string_of_printer dump#override_flag
let dump_row_var_flag = LibUtil.to_string_of_printer dump#row_var_flag
let dump_position_flag = LibUtil.to_string_of_printer dump#position_flag
let dump_strings = LibUtil.to_string_of_printer dump#strings
let dump_alident = LibUtil.to_string_of_printer dump#alident
let dump_auident = LibUtil.to_string_of_printer dump#auident
let dump_aident = LibUtil.to_string_of_printer dump#aident
let dump_astring = LibUtil.to_string_of_printer dump#astring
let dump_uident = LibUtil.to_string_of_printer dump#uident
let dump_ident = LibUtil.to_string_of_printer dump#ident
let dump_dupath = LibUtil.to_string_of_printer dump#dupath
let dump_dlpath = LibUtil.to_string_of_printer dump#dlpath
let dump_any = LibUtil.to_string_of_printer dump#any
let dump_sid = LibUtil.to_string_of_printer dump#sid
let dump_ctyp = LibUtil.to_string_of_printer dump#ctyp
let dump_type_parameters = LibUtil.to_string_of_printer dump#type_parameters
let dump_row_field = LibUtil.to_string_of_printer dump#row_field
let dump_tag_names = LibUtil.to_string_of_printer dump#tag_names
let dump_typedecl = LibUtil.to_string_of_printer dump#typedecl
let dump_type_constr = LibUtil.to_string_of_printer dump#type_constr
let dump_opt_type_constr = LibUtil.to_string_of_printer dump#opt_type_constr
let dump_decl_param = LibUtil.to_string_of_printer dump#decl_param
let dump_decl_params = LibUtil.to_string_of_printer dump#decl_params
let dump_opt_decl_params = LibUtil.to_string_of_printer dump#opt_decl_params
let dump_type_info = LibUtil.to_string_of_printer dump#type_info
let dump_type_repr = LibUtil.to_string_of_printer dump#type_repr
let dump_name_ctyp = LibUtil.to_string_of_printer dump#name_ctyp
let dump_or_ctyp = LibUtil.to_string_of_printer dump#or_ctyp
let dump_of_ctyp = LibUtil.to_string_of_printer dump#of_ctyp
let dump_pat = LibUtil.to_string_of_printer dump#pat
let dump_rec_pat = LibUtil.to_string_of_printer dump#rec_pat
let dump_exp = LibUtil.to_string_of_printer dump#exp
let dump_rec_exp = LibUtil.to_string_of_printer dump#rec_exp
let dump_module_type = LibUtil.to_string_of_printer dump#module_type
let dump_sig_item = LibUtil.to_string_of_printer dump#sig_item
let dump_with_constr = LibUtil.to_string_of_printer dump#with_constr
let dump_binding = LibUtil.to_string_of_printer dump#binding
let dump_module_binding = LibUtil.to_string_of_printer dump#module_binding
let dump_case = LibUtil.to_string_of_printer dump#case
let dump_module_exp = LibUtil.to_string_of_printer dump#module_exp
let dump_stru = LibUtil.to_string_of_printer dump#stru
let dump_class_type = LibUtil.to_string_of_printer dump#class_type
let dump_class_sig_item = LibUtil.to_string_of_printer dump#class_sig_item
let dump_class_exp = LibUtil.to_string_of_printer dump#class_exp
let dump_cstru = LibUtil.to_string_of_printer dump#cstru
let dump_ep = LibUtil.to_string_of_printer dump#ep
let dump_rec_bind = LibUtil.to_string_of_printer dump#rec_bind
class reloc _loc = object  inherit  map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  map as super
    method! pat =
      function
      | `Id (_loc,`Lid (_,_)) -> `Any _loc
      | `Alias (_loc,p,_) -> self#pat p
      | p -> super#pat p
  end