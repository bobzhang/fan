open LibUtil
open StdLib
open Ast
let strip_loc_list f lst = List.map f lst
let strip_loc_ant ant = ant
let _ = (); ()
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
    method ant_nil : ant_nil -> ant_nil -> ant_nil=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>ant_nil)
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>ant_nil)
        | (_,_) -> invalid_arg "map2 failure"
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
    method meta_bool : meta_bool -> meta_bool -> meta_bool=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`True _a0,`True _b0) -> let _a0 = self#loc _a0 _b0 in `True _a0
        | (`False _a0,`False _b0) -> let _a0 = self#loc _a0 _b0 in `False _a0
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>meta_bool)
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
    method sid : sid -> sid -> sid=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `Id (_a0, _a1)
    method any : any -> any -> any=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Any _a0,`Any _b0) -> let _a0 = self#loc _a0 _b0 in `Any _a0
    method ctyp : ctyp -> ctyp -> ctyp=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 : nil  :>ctyp)
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
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `TyPol (_a0, _a1, _a2)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>type_parameters)
        | (_,_) -> invalid_arg "map2 failure"
    method row_field : row_field -> row_field -> row_field=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 : ant_nil  :>row_field)
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
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 : ant_nil  :>tag_names)
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
            let _a2 = self#list (fun self  -> self#ctyp) _a2 _b2 in
            let _a3 = self#type_info _a3 _b3 in
            let _a4 =
              self#list
                (fun self  _a0  _b0  ->
                   match (_a0, _b0) with
                   | ((_a0,_a1),(_b0,_b1)) ->
                       let _a0 = self#ctyp _a0 _b0 in
                       let _a1 = self#ctyp _a1 _b1 in (_a0, _a1)) _a4 _b4 in
            `TyDcl (_a0, _a1, _a2, _a3, _a4)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#typedecl _a1 _b1 in
            let _a2 = self#typedecl _a2 _b2 in `And (_a0, _a1, _a2)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 : ant_nil  :>typedecl)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>type_info)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>type_repr)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>name_ctyp)
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
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 : ant_nil  :>or_ctyp)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>of_ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method patt : patt -> patt -> patt=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>patt)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Vrn (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in `Tup (_a0, _a1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>patt)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_patt _a1 _b1 in `Record (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>patt)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 : literal  :>patt)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Alias (_a0, _a1, _a2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `ArrayEmpty _a0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in `Array (_a0, _a1)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `LabelS (_a0, _a1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `Label (_a0, _a1, _a2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `OptLabl (_a0, _a1, _a2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `OptLablS (_a0, _a1)
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in `OptLablExpr (_a0, _a1, _a2, _a3)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `PaRng (_a0, _a1, _a2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `ClassPath (_a0, _a1)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in `Lazy (_a0, _a1)
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in `ModuleUnpack (_a0, _a1)
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `ModuleConstraint (_a0, _a1, _a2)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_patt : rec_patt -> rec_patt -> rec_patt=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#patt _a2 _b2 in `RecBind (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_patt _a1 _b1 in
            let _a2 = self#rec_patt _a2 _b2 in `Sem (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) ->
            (self#any _a0 _b0 : any  :>rec_patt)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_patt)
        | (_,_) -> invalid_arg "map2 failure"
    method expr : expr -> expr -> expr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 : sid  :>expr)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#string _a1 _b1 in `Vrn (_a0, _a1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Com (_a0, _a1, _a2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `Tup (_a0, _a1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 : any  :>expr)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_expr _a1 _b1 in `Record (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 : ant  :>expr)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 : literal  :>expr)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `RecordWith (_a0, _a1, _a2)
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Dot (_a0, _a1, _a2)
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `ArrayDot (_a0, _a1, _a2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `ArrayEmpty _a0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `Array (_a0, _a1)
        | (`ExAsf _a0,`ExAsf _b0) -> let _a0 = self#loc _a0 _b0 in `ExAsf _a0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `ExAsr (_a0, _a1)
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Assign (_a0, _a1, _a2)
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in
            let _a4 = self#direction_flag _a4 _b4 in
            let _a5 = self#expr _a5 _b5 in
            `For (_a0, _a1, _a2, _a3, _a4, _a5)
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#match_case _a1 _b1 in `Fun (_a0, _a1)
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in `IfThenElse (_a0, _a1, _a2, _a3)
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `IfThen (_a0, _a1, _a2)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `LabelS (_a0, _a1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Label (_a0, _a1, _a2)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `Lazy (_a0, _a1)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_flag _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in `LetIn (_a0, _a1, _a2, _a3)
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_expr _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in `LetModule (_a0, _a1, _a2, _a3)
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#match_case _a2 _b2 in `Match (_a0, _a1, _a2)
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in `New (_a0, _a1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_str_item _a1 _b1 in `Obj (_a0, _a1)
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#class_str_item _a2 _b2 in `ObjPat (_a0, _a1, _a2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `OptLabl (_a0, _a1, _a2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `OptLablS (_a0, _a1)
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_expr _a1 _b1 in `OvrInst (_a0, _a1)
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) ->
            let _a0 = self#loc _a0 _b0 in `OvrInstEmpty _a0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `Seq (_a0, _a1)
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#alident _a2 _b2 in `Send (_a0, _a1, _a2)
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `StringDot (_a0, _a1, _a2)
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#match_case _a2 _b2 in `Try (_a0, _a1, _a2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in
            let _a3 = self#ctyp _a3 _b3 in `Coercion (_a0, _a1, _a2, _a3)
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `While (_a0, _a1, _a2)
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `LetOpen (_a0, _a1, _a2)
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `LocalTypeFun (_a0, _a1, _a2)
        | (`Package_expr (_a0,_a1),`Package_expr (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_expr _a1 _b1 in `Package_expr (_a0, _a1)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_expr : rec_expr -> rec_expr -> rec_expr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_expr _a1 _b1 in
            let _a2 = self#rec_expr _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `RecBind (_a0, _a1, _a2)
        | ((#any as _a0),(#any as _b0)) ->
            (self#any _a0 _b0 : any  :>rec_expr)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>rec_expr)
        | (_,_) -> invalid_arg "map2 failure"
    method module_type : module_type -> module_type -> module_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>module_type)
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>module_type)
        | (`MtFun (_a0,_a1,_a2,_a3),`MtFun (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_type _a3 _b3 in `MtFun (_a0, _a1, _a2, _a3)
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#sig_item _a1 _b1 in `Sig (_a0, _a1)
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_type _a1 _b1 in
            let _a2 = self#with_constr _a2 _b2 in `With (_a0, _a1, _a2)
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_expr _a1 _b1 in `ModuleTypeOf (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_type)
        | (_,_) -> invalid_arg "map2 failure"
    method sig_item : sig_item -> sig_item -> sig_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>sig_item)
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
            let _a2 = self#expr _a2 _b2 in `Directive (_a0, _a1, _a2)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>with_constr)
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
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>binding)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#binding _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in `And (_a0, _a1, _a2)
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Bind (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>binding)
        | (_,_) -> invalid_arg "map2 failure"
    method module_binding :
      module_binding -> module_binding -> module_binding=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>module_binding)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_binding _a1 _b1 in
            let _a2 = self#module_binding _a2 _b2 in `And (_a0, _a1, _a2)
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_expr _a3 _b3 in
            `ModuleBind (_a0, _a1, _a2, _a3)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_binding)
        | (_,_) -> invalid_arg "map2 failure"
    method match_case : match_case -> match_case -> match_case=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>match_case)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#match_case _a1 _b1 in
            let _a2 = self#match_case _a2 _b2 in `Or (_a0, _a1, _a2)
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Case (_a0, _a1, _a2)
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in
            let _a3 = self#expr _a3 _b3 in `CaseWhen (_a0, _a1, _a2, _a3)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>match_case)
        | (_,_) -> invalid_arg "map2 failure"
    method module_expr : module_expr -> module_expr -> module_expr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>module_expr)
        | ((#sid as _a0),(#sid as _b0)) ->
            (self#sid _a0 _b0 : sid  :>module_expr)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_expr _a1 _b1 in
            let _a2 = self#module_expr _a2 _b2 in `App (_a0, _a1, _a2)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in
            let _a3 = self#module_expr _a3 _b3 in
            `Functor (_a0, _a1, _a2, _a3)
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#str_item _a1 _b1 in `Struct (_a0, _a1)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_expr _a1 _b1 in
            let _a2 = self#module_type _a2 _b2 in `Constraint (_a0, _a1, _a2)
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `PackageModule (_a0, _a1)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>module_expr)
        | (_,_) -> invalid_arg "map2 failure"
    method str_item : str_item -> str_item -> str_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>str_item)
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_expr _a1 _b1 in `Class (_a0, _a1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in `ClassType (_a0, _a1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#str_item _a1 _b1 in
            let _a2 = self#str_item _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in `DirectiveSimple (_a0, _a1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `Directive (_a0, _a1, _a2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#of_ctyp _a1 _b1 in `Exception (_a0, _a1)
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `StExp (_a0, _a1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in
            let _a3 = self#strings _a3 _b3 in `External (_a0, _a1, _a2, _a3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#module_expr _a1 _b1 in `Include (_a0, _a1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#auident _a1 _b1 in
            let _a2 = self#module_expr _a2 _b2 in `Module (_a0, _a1, _a2)
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
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>str_item)
        | (_,_) -> invalid_arg "map2 failure"
    method class_type : class_type -> class_type -> class_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>class_type)
        | (`CtCon (_a0,_a1,_a2,_a3),`CtCon (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in
            let _a3 = self#type_parameters _a3 _b3 in
            `CtCon (_a0, _a1, _a2, _a3)
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CtFun (_a0, _a1, _a2)
        | (`CtSig (_a0,_a1,_a2),`CtSig (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#class_sig_item _a2 _b2 in `CtSig (_a0, _a1, _a2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `And (_a0, _a1, _a2)
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CtCol (_a0, _a1, _a2)
        | (`CtEq (_a0,_a1,_a2),`CtEq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_type _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CtEq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_type)
        | (_,_) -> invalid_arg "map2 failure"
    method class_sig_item :
      class_sig_item -> class_sig_item -> class_sig_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>class_sig_item)
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
    method class_expr : class_expr -> class_expr -> class_expr=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>class_expr)
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_expr _a1 _b1 in
            let _a2 = self#expr _a2 _b2 in `CeApp (_a0, _a1, _a2)
        | (`CeCon (_a0,_a1,_a2,_a3),`CeCon (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#virtual_flag _a1 _b1 in
            let _a2 = self#ident _a2 _b2 in
            let _a3 = self#type_parameters _a3 _b3 in
            `CeCon (_a0, _a1, _a2, _a3)
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#class_expr _a2 _b2 in `CeFun (_a0, _a1, _a2)
        | (`CeLet (_a0,_a1,_a2,_a3),`CeLet (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#rec_flag _a1 _b1 in
            let _a2 = self#binding _a2 _b2 in
            let _a3 = self#class_expr _a3 _b3 in `CeLet (_a0, _a1, _a2, _a3)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_str_item _a1 _b1 in `Obj (_a0, _a1)
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#patt _a1 _b1 in
            let _a2 = self#class_str_item _a2 _b2 in `ObjPat (_a0, _a1, _a2)
        | (`CeTyc (_a0,_a1,_a2),`CeTyc (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_expr _a1 _b1 in
            let _a2 = self#class_type _a2 _b2 in `CeTyc (_a0, _a1, _a2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_expr _a1 _b1 in
            let _a2 = self#class_expr _a2 _b2 in `And (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_expr _a1 _b1 in
            let _a2 = self#class_expr _a2 _b2 in `Eq (_a0, _a1, _a2)
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_expr)
        | (_,_) -> invalid_arg "map2 failure"
    method class_str_item :
      class_str_item -> class_str_item -> class_str_item=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) ->
            (self#nil _a0 _b0 : nil  :>class_str_item)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#class_str_item _a1 _b1 in
            let _a2 = self#class_str_item _a2 _b2 in `Sem (_a0, _a1, _a2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#ctyp _a1 _b1 in
            let _a2 = self#ctyp _a2 _b2 in `Eq (_a0, _a1, _a2)
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#override_flag _a1 _b1 in
            let _a2 = self#class_expr _a2 _b2 in `Inherit (_a0, _a1, _a2)
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#override_flag _a1 _b1 in
            let _a2 = self#class_expr _a2 _b2 in
            let _a3 = self#alident _a3 _b3 in `InheritAs (_a0, _a1, _a2, _a3)
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#expr _a1 _b1 in `Initializer (_a0, _a1)
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#override_flag _a2 _b2 in
            let _a3 = self#private_flag _a3 _b3 in
            let _a4 = self#expr _a4 _b4 in
            let _a5 = self#ctyp _a5 _b5 in
            `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            let _a0 = self#loc _a0 _b0 in
            let _a1 = self#alident _a1 _b1 in
            let _a2 = self#override_flag _a2 _b2 in
            let _a3 = self#mutable_flag _a3 _b3 in
            let _a4 = self#expr _a4 _b4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
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
        | ((#ant as _a0),(#ant as _b0)) ->
            (self#ant _a0 _b0 : ant  :>class_str_item)
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
    method ant_nil : ant_nil -> ant_nil -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
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
    method meta_bool : meta_bool -> meta_bool -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`True _a0,`True _b0) -> self#loc _a0 _b0
        | (`False _a0,`False _b0) -> self#loc _a0 _b0
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
    method sid : sid -> sid -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
    method any : any -> any -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
    method ctyp : ctyp -> ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method row_field : row_field -> row_field -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'self_type)
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
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'self_type)
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
            let self = self#list (fun self  -> self#ctyp) _a2 _b2 in
            let self = self#type_info _a3 _b3 in
            self#list
              (fun self  _a0  _b0  ->
                 match (_a0, _b0) with
                 | ((_a0,_a1),(_b0,_b1)) ->
                     let self = self#ctyp _a0 _b0 in self#ctyp _a1 _b1) _a4
              _b4
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#typedecl _a1 _b1 in self#typedecl _a2 _b2
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'self_type)
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method type_repr : type_repr -> type_repr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#name_ctyp _a1 _b1
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#or_ctyp _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method of_ctyp : of_ctyp -> of_ctyp -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#sid _a1 _b1 in self#ctyp _a2 _b2
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method patt : patt -> patt -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#patt _a2 _b2
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#patt _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#patt _a2 _b2
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#patt _a1 _b1
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_patt _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'self_type)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#alident _a2 _b2
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#patt _a1 _b1
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#patt _a2 _b2
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#patt _a2 _b2
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#patt _a2 _b2 in self#expr _a3 _b3
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#patt _a2 _b2
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#patt _a2 _b2
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#ctyp _a2 _b2
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#patt _a1 _b1
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#auident _a1 _b1
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#ctyp _a2 _b2
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_patt : rec_patt -> rec_patt -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#patt _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_patt _a1 _b1 in self#rec_patt _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method expr : expr -> expr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#string _a1 _b1
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_expr _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'self_type)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_expr _a1 _b1 in self#expr _a2 _b2
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`ExAsf _a0,`ExAsf _b0) -> self#loc _a0 _b0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#expr _a2 _b2 in
            let self = self#expr _a3 _b3 in
            let self = self#direction_flag _a4 _b4 in self#expr _a5 _b5
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#match_case _a1 _b1
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in
            let self = self#expr _a2 _b2 in self#expr _a3 _b3
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#expr _a2 _b2
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_flag _a1 _b1 in
            let self = self#binding _a2 _b2 in self#expr _a3 _b3
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_expr _a2 _b2 in self#expr _a3 _b3
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#match_case _a2 _b2
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#ident _a1 _b1
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_str_item _a1 _b1
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#class_str_item _a2 _b2
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#expr _a2 _b2
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#rec_expr _a1 _b1
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) -> self#loc _a0 _b0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#alident _a2 _b2
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#match_case _a2 _b2
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#ctyp _a2 _b2
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in
            let self = self#ctyp _a2 _b2 in self#ctyp _a3 _b3
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#expr _a1 _b1 in self#expr _a2 _b2
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#expr _a2 _b2
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#expr _a2 _b2
        | (`Package_expr (_a0,_a1),`Package_expr (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_expr _a1 _b1
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_expr : rec_expr -> rec_expr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_expr _a1 _b1 in self#rec_expr _a2 _b2
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ident _a1 _b1 in self#expr _a2 _b2
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'self_type)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_type : module_type -> module_type -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`MtFun (_a0,_a1,_a2,_a3),`MtFun (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_type _a3 _b3
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#sig_item _a1 _b1
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_type _a1 _b1 in self#with_constr _a2 _b2
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_expr _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method sig_item : sig_item -> sig_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
            let self = self#alident _a1 _b1 in self#expr _a2 _b2
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#binding _a1 _b1 in self#binding _a2 _b2
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#expr _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_binding : module_binding -> module_binding -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_binding _a1 _b1 in
            self#module_binding _a2 _b2
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_expr _a3 _b3
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_type _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method match_case : match_case -> match_case -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#match_case _a1 _b1 in self#match_case _a2 _b2
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#expr _a2 _b2
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in
            let self = self#expr _a2 _b2 in self#expr _a3 _b3
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_expr : module_expr -> module_expr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'self_type)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_expr _a1 _b1 in self#module_expr _a2 _b2
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in
            let self = self#module_type _a2 _b2 in self#module_expr _a3 _b3
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#str_item _a1 _b1
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#module_expr _a1 _b1 in self#module_type _a2 _b2
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method str_item : str_item -> str_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_expr _a1 _b1
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_type _a1 _b1
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#str_item _a1 _b1 in self#str_item _a2 _b2
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#alident _a1 _b1
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in self#expr _a2 _b2
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#of_ctyp _a1 _b1
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#ctyp _a2 _b2 in self#strings _a3 _b3
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#module_expr _a1 _b1
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#auident _a1 _b1 in self#module_expr _a2 _b2
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
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`CtCon (_a0,_a1,_a2,_a3),`CtCon (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in
            let self = self#ident _a2 _b2 in self#type_parameters _a3 _b3
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#class_type _a2 _b2
        | (`CtSig (_a0,_a1,_a2),`CtSig (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#class_sig_item _a2 _b2
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | (`CtEq (_a0,_a1,_a2),`CtEq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_type _a1 _b1 in self#class_type _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_sig_item : class_sig_item -> class_sig_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
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
    method class_expr : class_expr -> class_expr -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_expr _a1 _b1 in self#expr _a2 _b2
        | (`CeCon (_a0,_a1,_a2,_a3),`CeCon (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#virtual_flag _a1 _b1 in
            let self = self#ident _a2 _b2 in self#type_parameters _a3 _b3
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#class_expr _a2 _b2
        | (`CeLet (_a0,_a1,_a2,_a3),`CeLet (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#rec_flag _a1 _b1 in
            let self = self#binding _a2 _b2 in self#class_expr _a3 _b3
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#class_str_item _a1 _b1
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#patt _a1 _b1 in self#class_str_item _a2 _b2
        | (`CeTyc (_a0,_a1,_a2),`CeTyc (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_expr _a1 _b1 in self#class_type _a2 _b2
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_expr _a1 _b1 in self#class_expr _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_expr _a1 _b1 in self#class_expr _a2 _b2
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_str_item : class_str_item -> class_str_item -> 'self_type=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'self_type)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#class_str_item _a1 _b1 in
            self#class_str_item _a2 _b2
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#ctyp _a1 _b1 in self#ctyp _a2 _b2
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            let self = self#loc _a0 _b0 in
            let self = self#override_flag _a1 _b1 in self#class_expr _a2 _b2
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            let self = self#loc _a0 _b0 in
            let self = self#override_flag _a1 _b1 in
            let self = self#class_expr _a2 _b2 in self#alident _a3 _b3
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            let self = self#loc _a0 _b0 in self#expr _a1 _b1
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#override_flag _a2 _b2 in
            let self = self#private_flag _a3 _b3 in
            let self = self#expr _a4 _b4 in self#ctyp _a5 _b5
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            let self = self#loc _a0 _b0 in
            let self = self#alident _a1 _b1 in
            let self = self#override_flag _a2 _b2 in
            let self = self#mutable_flag _a3 _b3 in self#expr _a4 _b4
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
    method loc : loc -> 'result110= fun _a0  -> self#fanloc_t _a0
    method ant : ant -> 'result111=
      fun (`Ant (_a0,_a1))  -> self#loc _a0; self#fanutil_anti_cxt _a1
    method nil : nil -> 'result112= fun (`Nil _a0)  -> self#loc _a0
    method ant_nil : ant_nil -> 'result113=
      function
      | #ant as _a0 -> (self#ant _a0 :>'result113)
      | #nil as _a0 -> (self#nil _a0 :>'result113)
    method literal : literal -> 'result114=
      function
      | `Chr (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int32 (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Int64 (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Flo (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `NativeInt (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Str (_a0,_a1) -> (self#loc _a0; self#string _a1)
    method rec_flag : rec_flag -> 'result115=
      function
      | `Recursive _a0 -> self#loc _a0
      | `ReNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result115)
    method direction_flag : direction_flag -> 'result116=
      function
      | `To _a0 -> self#loc _a0
      | `Downto _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result116)
    method mutable_flag : mutable_flag -> 'result117=
      function
      | `Mutable _a0 -> self#loc _a0
      | `MuNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result117)
    method private_flag : private_flag -> 'result118=
      function
      | `Private _a0 -> self#loc _a0
      | `PrNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result118)
    method virtual_flag : virtual_flag -> 'result119=
      function
      | `Virtual _a0 -> self#loc _a0
      | `ViNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result119)
    method override_flag : override_flag -> 'result120=
      function
      | `Override _a0 -> self#loc _a0
      | `OvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result120)
    method row_var_flag : row_var_flag -> 'result121=
      function
      | `RowVar _a0 -> self#loc _a0
      | `RvNil _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result121)
    method position_flag : position_flag -> 'result122=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | `Normal _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result122)
    method meta_bool : meta_bool -> 'result123=
      function
      | `True _a0 -> self#loc _a0
      | `False _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'result123)
    method strings : strings -> 'result124=
      function
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#strings _a1; self#strings _a2)
      | `Str (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result124)
    method alident : alident -> 'result125=
      function
      | `Lid (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result125)
    method auident : auident -> 'result126=
      function
      | `Uid (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result126)
    method aident : aident -> 'result127=
      function
      | #alident as _a0 -> (self#alident _a0 :>'result127)
      | #auident as _a0 -> (self#auident _a0 :>'result127)
    method astring : astring -> 'result128=
      function
      | `C (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result128)
    method uident : uident -> 'result129=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#uident _a1; self#uident _a2)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#uident _a1; self#uident _a2)
      | #auident as _a0 -> (self#auident _a0 :>'result129)
    method ident : ident -> 'result130=
      function
      | `Dot (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ident _a2)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ident _a2)
      | #alident as _a0 -> (self#alident _a0 :>'result130)
      | #auident as _a0 -> (self#auident _a0 :>'result130)
    method dupath : dupath -> 'result131=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#dupath _a1; self#dupath _a2)
      | #auident as _a0 -> (self#auident _a0 :>'result131)
    method dlpath : dlpath -> 'result132=
      function
      | `Dot (_a0,_a1,_a2) ->
          (self#loc _a0; self#dupath _a1; self#alident _a2)
      | #alident as _a0 -> (self#alident _a0 :>'result132)
    method sid : sid -> 'result133=
      fun (`Id (_a0,_a1))  -> self#loc _a0; self#ident _a1
    method any : any -> 'result134= fun (`Any _a0)  -> self#loc _a0
    method ctyp : ctyp -> 'result135=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result135)
      | `Alias (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#alident _a2)
      | #any as _a0 -> (self#any _a0 :>'result135)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Arrow (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `ClassPath (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result135)
      | `TyObj (_a0,_a1,_a2) ->
          (self#loc _a0; self#name_ctyp _a1; self#row_var_flag _a2)
      | `TyPol (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
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
      | `PolyInfSup (_a0,_a1,_a2) ->
          (self#loc _a0; self#row_field _a1; self#tag_names _a2)
      | `Package (_a0,_a1) -> (self#loc _a0; self#module_type _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result135)
    method type_parameters : type_parameters -> 'result136=
      function
      | `Com (_a0,_a1,_a2) ->
          (self#loc _a0; self#type_parameters _a1; self#type_parameters _a2)
      | `Ctyp (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result136)
      | #nil as _a0 -> (self#nil _a0 :>'result136)
    method row_field : row_field -> 'result137=
      function
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'result137)
      | `Or (_a0,_a1,_a2) ->
          (self#loc _a0; self#row_field _a1; self#row_field _a2)
      | `TyVrn (_a0,_a1) -> (self#loc _a0; self#astring _a1)
      | `TyVrnOf (_a0,_a1,_a2) ->
          (self#loc _a0; self#astring _a1; self#ctyp _a2)
      | `Ctyp (_a0,_a1) -> (self#loc _a0; self#ctyp _a1)
    method tag_names : tag_names -> 'result138=
      function
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'result138)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#tag_names _a1; self#tag_names _a2)
      | `TyVrn (_a0,_a1) -> (self#loc _a0; self#astring _a1)
    method typedecl : typedecl -> 'result139=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#list (fun self  -> self#ctyp) _a2;
           self#type_info _a3;
           self#list (fun self  (_a0,_a1)  -> self#ctyp _a0; self#ctyp _a1)
             _a4)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#typedecl _a1; self#typedecl _a2)
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'result139)
    method type_info : type_info -> 'result140=
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
      | #ant as _a0 -> (self#ant _a0 :>'result140)
      | #nil as _a0 -> (self#nil _a0 :>'result140)
    method type_repr : type_repr -> 'result141=
      function
      | `Record (_a0,_a1) -> (self#loc _a0; self#name_ctyp _a1)
      | `Sum (_a0,_a1) -> (self#loc _a0; self#or_ctyp _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result141)
      | #nil as _a0 -> (self#nil _a0 :>'result141)
    method name_ctyp : name_ctyp -> 'result142=
      function
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#name_ctyp _a1; self#name_ctyp _a2)
      | `TyCol (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | `TyColMut (_a0,_a1,_a2) ->
          (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result142)
      | #nil as _a0 -> (self#nil _a0 :>'result142)
    method or_ctyp : or_ctyp -> 'result143=
      function
      | `Or (_a0,_a1,_a2) ->
          (self#loc _a0; self#or_ctyp _a1; self#or_ctyp _a2)
      | `TyCol (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | `Of (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result143)
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'result143)
    method of_ctyp : of_ctyp -> 'result144=
      function
      | `Of (_a0,_a1,_a2) -> (self#loc _a0; self#sid _a1; self#ctyp _a2)
      | #sid as _a0 -> (self#sid _a0 :>'result144)
      | #ant as _a0 -> (self#ant _a0 :>'result144)
      | #nil as _a0 -> (self#nil _a0 :>'result144)
    method patt : patt -> 'result145=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result145)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#patt _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#patt _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#patt _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#patt _a1)
      | #any as _a0 -> (self#any _a0 :>'result145)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_patt _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result145)
      | #literal as _a0 -> (self#literal _a0 :>'result145)
      | `Alias (_a0,_a1,_a2) ->
          (self#loc _a0; self#patt _a1; self#alident _a2)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#patt _a1)
      | `LabelS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#patt _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#patt _a2)
      | `OptLablS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#patt _a2; self#expr _a3)
      | `Or (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#patt _a2)
      | `PaRng (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#patt _a2)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#patt _a1; self#ctyp _a2)
      | `ClassPath (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Lazy (_a0,_a1) -> (self#loc _a0; self#patt _a1)
      | `ModuleUnpack (_a0,_a1) -> (self#loc _a0; self#auident _a1)
      | `ModuleConstraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#ctyp _a2)
    method rec_patt : rec_patt -> 'result146=
      function
      | `RecBind (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#patt _a2)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_patt _a1; self#rec_patt _a2)
      | #any as _a0 -> (self#any _a0 :>'result146)
      | #ant as _a0 -> (self#ant _a0 :>'result146)
    method expr : expr -> 'result147=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result147)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | #any as _a0 -> (self#any _a0 :>'result147)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_expr _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result147)
      | #literal as _a0 -> (self#literal _a0 :>'result147)
      | `RecordWith (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_expr _a1; self#expr _a2)
      | `Dot (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `ArrayDot (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#expr _a2)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `ExAsf _a0 -> self#loc _a0
      | `ExAsr (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `Assign (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          (self#loc _a0;
           self#alident _a1;
           self#expr _a2;
           self#expr _a3;
           self#direction_flag _a4;
           self#expr _a5)
      | `Fun (_a0,_a1) -> (self#loc _a0; self#match_case _a1)
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#expr _a1; self#expr _a2; self#expr _a3)
      | `IfThen (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `LabelS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Label (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#expr _a2)
      | `Lazy (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#rec_flag _a1; self#binding _a2; self#expr _a3)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_expr _a2;
           self#expr _a3)
      | `Match (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#match_case _a2)
      | `New (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Obj (_a0,_a1) -> (self#loc _a0; self#class_str_item _a1)
      | `ObjPat (_a0,_a1,_a2) ->
          (self#loc _a0; self#patt _a1; self#class_str_item _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#expr _a2)
      | `OptLablS (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `OvrInst (_a0,_a1) -> (self#loc _a0; self#rec_expr _a1)
      | `OvrInstEmpty _a0 -> self#loc _a0
      | `Seq (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `Send (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#alident _a2)
      | `StringDot (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#expr _a2)
      | `Try (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#match_case _a2)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#expr _a1; self#ctyp _a2)
      | `Coercion (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#expr _a1; self#ctyp _a2; self#ctyp _a3)
      | `While (_a0,_a1,_a2) -> (self#loc _a0; self#expr _a1; self#expr _a2)
      | `LetOpen (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#expr _a2)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#expr _a2)
      | `Package_expr (_a0,_a1) -> (self#loc _a0; self#module_expr _a1)
    method rec_expr : rec_expr -> 'result148=
      function
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_expr _a1; self#rec_expr _a2)
      | `RecBind (_a0,_a1,_a2) ->
          (self#loc _a0; self#ident _a1; self#expr _a2)
      | #any as _a0 -> (self#any _a0 :>'result148)
      | #ant as _a0 -> (self#ant _a0 :>'result148)
    method module_type : module_type -> 'result149=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result149)
      | #sid as _a0 -> (self#sid _a0 :>'result149)
      | `MtFun (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_type _a3)
      | `Sig (_a0,_a1) -> (self#loc _a0; self#sig_item _a1)
      | `With (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_type _a1; self#with_constr _a2)
      | `ModuleTypeOf (_a0,_a1) -> (self#loc _a0; self#module_expr _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result149)
    method sig_item : sig_item -> 'result150=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result150)
      | `Class (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `ClassType (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#sig_item _a1; self#sig_item _a2)
      | `DirectiveSimple (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Directive (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#expr _a2)
      | `Exception (_a0,_a1) -> (self#loc _a0; self#of_ctyp _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2; self#strings _a3)
      | `Include (_a0,_a1) -> (self#loc _a0; self#module_type _a1)
      | `Module (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `RecModule (_a0,_a1) -> (self#loc _a0; self#module_binding _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `Open (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Type (_a0,_a1) -> (self#loc _a0; self#typedecl _a1)
      | `Val (_a0,_a1,_a2) -> (self#loc _a0; self#alident _a1; self#ctyp _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result150)
    method with_constr : with_constr -> 'result151=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result151)
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
      | #ant as _a0 -> (self#ant _a0 :>'result151)
    method binding : binding -> 'result152=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result152)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#binding _a1; self#binding _a2)
      | `Bind (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#expr _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result152)
    method module_binding : module_binding -> 'result153=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result153)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_binding _a1; self#module_binding _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_expr _a3)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result153)
    method match_case : match_case -> 'result154=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result154)
      | `Or (_a0,_a1,_a2) ->
          (self#loc _a0; self#match_case _a1; self#match_case _a2)
      | `Case (_a0,_a1,_a2) -> (self#loc _a0; self#patt _a1; self#expr _a2)
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#patt _a1; self#expr _a2; self#expr _a3)
      | #ant as _a0 -> (self#ant _a0 :>'result154)
    method module_expr : module_expr -> 'result155=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result155)
      | #sid as _a0 -> (self#sid _a0 :>'result155)
      | `App (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_expr _a1; self#module_expr _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#auident _a1;
           self#module_type _a2;
           self#module_expr _a3)
      | `Struct (_a0,_a1) -> (self#loc _a0; self#str_item _a1)
      | `Constraint (_a0,_a1,_a2) ->
          (self#loc _a0; self#module_expr _a1; self#module_type _a2)
      | `PackageModule (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | #ant as _a0 -> (self#ant _a0 :>'result155)
    method str_item : str_item -> 'result156=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result156)
      | `Class (_a0,_a1) -> (self#loc _a0; self#class_expr _a1)
      | `ClassType (_a0,_a1) -> (self#loc _a0; self#class_type _a1)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#str_item _a1; self#str_item _a2)
      | `DirectiveSimple (_a0,_a1) -> (self#loc _a0; self#alident _a1)
      | `Directive (_a0,_a1,_a2) ->
          (self#loc _a0; self#alident _a1; self#expr _a2)
      | `Exception (_a0,_a1) -> (self#loc _a0; self#of_ctyp _a1)
      | `StExp (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          (self#loc _a0; self#alident _a1; self#ctyp _a2; self#strings _a3)
      | `Include (_a0,_a1) -> (self#loc _a0; self#module_expr _a1)
      | `Module (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_expr _a2)
      | `RecModule (_a0,_a1) -> (self#loc _a0; self#module_binding _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          (self#loc _a0; self#auident _a1; self#module_type _a2)
      | `Open (_a0,_a1) -> (self#loc _a0; self#ident _a1)
      | `Type (_a0,_a1) -> (self#loc _a0; self#typedecl _a1)
      | `Value (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_flag _a1; self#binding _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result156)
    method class_type : class_type -> 'result157=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result157)
      | `CtCon (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#virtual_flag _a1;
           self#ident _a2;
           self#type_parameters _a3)
      | `CtFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#class_type _a2)
      | `CtSig (_a0,_a1,_a2) ->
          (self#loc _a0; self#ctyp _a1; self#class_sig_item _a2)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | `CtCol (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | `CtEq (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_type _a1; self#class_type _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result157)
    method class_sig_item : class_sig_item -> 'result158=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result158)
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
      | #ant as _a0 -> (self#ant _a0 :>'result158)
    method class_expr : class_expr -> 'result159=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result159)
      | `CeApp (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_expr _a1; self#expr _a2)
      | `CeCon (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#virtual_flag _a1;
           self#ident _a2;
           self#type_parameters _a3)
      | `CeFun (_a0,_a1,_a2) ->
          (self#loc _a0; self#patt _a1; self#class_expr _a2)
      | `CeLet (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#rec_flag _a1;
           self#binding _a2;
           self#class_expr _a3)
      | `Obj (_a0,_a1) -> (self#loc _a0; self#class_str_item _a1)
      | `ObjPat (_a0,_a1,_a2) ->
          (self#loc _a0; self#patt _a1; self#class_str_item _a2)
      | `CeTyc (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_expr _a1; self#class_type _a2)
      | `And (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_expr _a1; self#class_expr _a2)
      | `Eq (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_expr _a1; self#class_expr _a2)
      | #ant as _a0 -> (self#ant _a0 :>'result159)
    method class_str_item : class_str_item -> 'result160=
      function
      | #nil as _a0 -> (self#nil _a0 :>'result160)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#class_str_item _a1; self#class_str_item _a2)
      | `Eq (_a0,_a1,_a2) -> (self#loc _a0; self#ctyp _a1; self#ctyp _a2)
      | `Inherit (_a0,_a1,_a2) ->
          (self#loc _a0; self#override_flag _a1; self#class_expr _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          (self#loc _a0;
           self#override_flag _a1;
           self#class_expr _a2;
           self#alident _a3)
      | `Initializer (_a0,_a1) -> (self#loc _a0; self#expr _a1)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          (self#loc _a0;
           self#alident _a1;
           self#override_flag _a2;
           self#private_flag _a3;
           self#expr _a4;
           self#ctyp _a5)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          (self#loc _a0;
           self#alident _a1;
           self#override_flag _a2;
           self#mutable_flag _a3;
           self#expr _a4)
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
      | #ant as _a0 -> (self#ant _a0 :>'result160)
    method ep : ep -> 'result161=
      function
      | #sid as _a0 -> (self#sid _a0 :>'result161)
      | `App (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Vrn (_a0,_a1) -> (self#loc _a0; self#string _a1)
      | `Com (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Sem (_a0,_a1,_a2) -> (self#loc _a0; self#ep _a1; self#ep _a2)
      | `Tup (_a0,_a1) -> (self#loc _a0; self#ep _a1)
      | #any as _a0 -> (self#any _a0 :>'result161)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> (self#loc _a0; self#ep _a1)
      | `Record (_a0,_a1) -> (self#loc _a0; self#rec_bind _a1)
      | #literal as _a0 -> (self#literal _a0 :>'result161)
      | #ant as _a0 -> (self#ant _a0 :>'result161)
    method rec_bind : rec_bind -> 'result162=
      function
      | `RecBind (_a0,_a1,_a2) -> (self#loc _a0; self#ident _a1; self#ep _a2)
      | `Sem (_a0,_a1,_a2) ->
          (self#loc _a0; self#rec_bind _a1; self#rec_bind _a2)
      | #any as _a0 -> (self#any _a0 :>'result162)
      | #ant as _a0 -> (self#ant _a0 :>'result162)
    method fanloc_t : FanLoc.t -> 'result163= self#unknown
    method fanutil_anti_cxt : FanUtil.anti_cxt -> 'result164= self#unknown
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
    method ant_nil : ant_nil -> ant_nil=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>ant_nil)
      | #nil as _a0 -> (self#nil _a0 : nil  :>ant_nil)
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
    method meta_bool : meta_bool -> meta_bool=
      function
      | `True _a0 -> let _a0 = self#loc _a0 in `True _a0
      | `False _a0 -> let _a0 = self#loc _a0 in `False _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>meta_bool)
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
    method sid : sid -> sid=
      fun (`Id (_a0,_a1))  ->
        let _a0 = self#loc _a0 in let _a1 = self#ident _a1 in `Id (_a0, _a1)
    method any : any -> any=
      fun (`Any _a0)  -> let _a0 = self#loc _a0 in `Any _a0
    method ctyp : ctyp -> ctyp=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>ctyp)
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
      | `TyPol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TyPol (_a0, _a1, _a2)
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
      | #nil as _a0 -> (self#nil _a0 : nil  :>type_parameters)
    method row_field : row_field -> row_field=
      function
      | #ant_nil as _a0 -> (self#ant_nil _a0 : ant_nil  :>row_field)
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
      | #ant_nil as _a0 -> (self#ant_nil _a0 : ant_nil  :>tag_names)
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
          let _a2 = self#list (fun self  -> self#ctyp) _a2 in
          let _a3 = self#type_info _a3 in
          let _a4 =
            self#list
              (fun self  (_a0,_a1)  ->
                 let _a0 = self#ctyp _a0 in
                 let _a1 = self#ctyp _a1 in (_a0, _a1)) _a4 in
          `TyDcl (_a0, _a1, _a2, _a3, _a4)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#typedecl _a1 in
          let _a2 = self#typedecl _a2 in `And (_a0, _a1, _a2)
      | #ant_nil as _a0 -> (self#ant_nil _a0 : ant_nil  :>typedecl)
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
      | #nil as _a0 -> (self#nil _a0 : nil  :>type_info)
    method type_repr : type_repr -> type_repr=
      function
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in `Record (_a0, _a1)
      | `Sum (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in `Sum (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_repr)
      | #nil as _a0 -> (self#nil _a0 : nil  :>type_repr)
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
      | #nil as _a0 -> (self#nil _a0 : nil  :>name_ctyp)
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
      | #ant_nil as _a0 -> (self#ant_nil _a0 : ant_nil  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sid _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #sid as _a0 -> (self#sid _a0 : sid  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
      | #nil as _a0 -> (self#nil _a0 : nil  :>of_ctyp)
    method patt : patt -> patt=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>patt)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#patt _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#patt _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#patt _a2 in `Sem (_a0, _a1, _a2)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in `Tup (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>patt)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_patt _a1 in `Record (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>patt)
      | #literal as _a0 -> (self#literal _a0 : literal  :>patt)
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#alident _a2 in `Alias (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in `Array (_a0, _a1)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#patt _a2 in `Label (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#patt _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#patt _a2 in
          let _a3 = self#expr _a3 in `OptLablExpr (_a0, _a1, _a2, _a3)
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#patt _a2 in `Or (_a0, _a1, _a2)
      | `PaRng (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#patt _a2 in `PaRng (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `ClassPath (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `ClassPath (_a0, _a1)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in `Lazy (_a0, _a1)
      | `ModuleUnpack (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleUnpack (_a0, _a1)
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `ModuleConstraint (_a0, _a1, _a2)
    method rec_patt : rec_patt -> rec_patt=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#patt _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_patt _a1 in
          let _a2 = self#rec_patt _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_patt)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_patt)
    method expr : expr -> expr=
      function
      | #sid as _a0 -> (self#sid _a0 : sid  :>expr)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `App (_a0, _a1, _a2)
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `Com (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `Sem (_a0, _a1, _a2)
      | `Tup (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `Tup (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>expr)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_expr _a1 in `Record (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>expr)
      | #literal as _a0 -> (self#literal _a0 : literal  :>expr)
      | `RecordWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_expr _a1 in
          let _a2 = self#expr _a2 in `RecordWith (_a0, _a1, _a2)
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `Dot (_a0, _a1, _a2)
      | `ArrayDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `ArrayDot (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `Array (_a0, _a1)
      | `ExAsf _a0 -> let _a0 = self#loc _a0 in `ExAsf _a0
      | `ExAsr (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `ExAsr (_a0, _a1)
      | `Assign (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `Assign (_a0, _a1, _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#expr _a2 in
          let _a3 = self#expr _a3 in
          let _a4 = self#direction_flag _a4 in
          let _a5 = self#expr _a5 in `For (_a0, _a1, _a2, _a3, _a4, _a5)
      | `Fun (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#match_case _a1 in `Fun (_a0, _a1)
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in
          let _a3 = self#expr _a3 in `IfThenElse (_a0, _a1, _a2, _a3)
      | `IfThen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `IfThen (_a0, _a1, _a2)
      | `LabelS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `LabelS (_a0, _a1)
      | `Label (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#expr _a2 in `Label (_a0, _a1, _a2)
      | `Lazy (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `Lazy (_a0, _a1)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_flag _a1 in
          let _a2 = self#binding _a2 in
          let _a3 = self#expr _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_expr _a2 in
          let _a3 = self#expr _a3 in `LetModule (_a0, _a1, _a2, _a3)
      | `Match (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#match_case _a2 in `Match (_a0, _a1, _a2)
      | `New (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in `New (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_str_item _a1 in `Obj (_a0, _a1)
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#class_str_item _a2 in `ObjPat (_a0, _a1, _a2)
      | `OptLabl (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#expr _a2 in `OptLabl (_a0, _a1, _a2)
      | `OptLablS (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `OptLablS (_a0, _a1)
      | `OvrInst (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_expr _a1 in `OvrInst (_a0, _a1)
      | `OvrInstEmpty _a0 -> let _a0 = self#loc _a0 in `OvrInstEmpty _a0
      | `Seq (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `Seq (_a0, _a1)
      | `Send (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#alident _a2 in `Send (_a0, _a1, _a2)
      | `StringDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `StringDot (_a0, _a1, _a2)
      | `Try (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#match_case _a2 in `Try (_a0, _a1, _a2)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#ctyp _a3 in `Coercion (_a0, _a1, _a2, _a3)
      | `While (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in
          let _a2 = self#expr _a2 in `While (_a0, _a1, _a2)
      | `LetOpen (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#expr _a2 in `LetOpen (_a0, _a1, _a2)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#expr _a2 in `LocalTypeFun (_a0, _a1, _a2)
      | `Package_expr (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_expr _a1 in `Package_expr (_a0, _a1)
    method rec_expr : rec_expr -> rec_expr=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_expr _a1 in
          let _a2 = self#rec_expr _a2 in `Sem (_a0, _a1, _a2)
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#expr _a2 in `RecBind (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_expr)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_expr)
    method module_type : module_type -> module_type=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>module_type)
      | #sid as _a0 -> (self#sid _a0 : sid  :>module_type)
      | `MtFun (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_type _a3 in `MtFun (_a0, _a1, _a2, _a3)
      | `Sig (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sig_item _a1 in `Sig (_a0, _a1)
      | `With (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_type _a1 in
          let _a2 = self#with_constr _a2 in `With (_a0, _a1, _a2)
      | `ModuleTypeOf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_expr _a1 in `ModuleTypeOf (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_type)
    method sig_item : sig_item -> sig_item=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>sig_item)
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
          let _a2 = self#expr _a2 in `Directive (_a0, _a1, _a2)
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
      | #nil as _a0 -> (self#nil _a0 : nil  :>with_constr)
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
      | #nil as _a0 -> (self#nil _a0 : nil  :>binding)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#binding _a1 in
          let _a2 = self#binding _a2 in `And (_a0, _a1, _a2)
      | `Bind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#expr _a2 in `Bind (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>binding)
    method module_binding : module_binding -> module_binding=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>module_binding)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_binding _a1 in
          let _a2 = self#module_binding _a2 in `And (_a0, _a1, _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_expr _a3 in `ModuleBind (_a0, _a1, _a2, _a3)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_binding)
    method match_case : match_case -> match_case=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>match_case)
      | `Or (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#match_case _a1 in
          let _a2 = self#match_case _a2 in `Or (_a0, _a1, _a2)
      | `Case (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#expr _a2 in `Case (_a0, _a1, _a2)
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#expr _a2 in
          let _a3 = self#expr _a3 in `CaseWhen (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>match_case)
    method module_expr : module_expr -> module_expr=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>module_expr)
      | #sid as _a0 -> (self#sid _a0 : sid  :>module_expr)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_expr _a1 in
          let _a2 = self#module_expr _a2 in `App (_a0, _a1, _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_type _a2 in
          let _a3 = self#module_expr _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `Struct (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#str_item _a1 in `Struct (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_expr _a1 in
          let _a2 = self#module_type _a2 in `Constraint (_a0, _a1, _a2)
      | `PackageModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `PackageModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>module_expr)
    method str_item : str_item -> str_item=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>str_item)
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_expr _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in `ClassType (_a0, _a1)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#str_item _a1 in
          let _a2 = self#str_item _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#expr _a2 in `Directive (_a0, _a1, _a2)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `StExp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `StExp (_a0, _a1)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#module_expr _a1 in `Include (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#module_expr _a2 in `Module (_a0, _a1, _a2)
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
      | #ant as _a0 -> (self#ant _a0 : ant  :>str_item)
    method class_type : class_type -> class_type=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>class_type)
      | `CtCon (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in `CtCon (_a0, _a1, _a2, _a3)
      | `CtFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#class_type _a2 in `CtFun (_a0, _a1, _a2)
      | `CtSig (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#class_sig_item _a2 in `CtSig (_a0, _a1, _a2)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `And (_a0, _a1, _a2)
      | `CtCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `CtCol (_a0, _a1, _a2)
      | `CtEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_type _a1 in
          let _a2 = self#class_type _a2 in `CtEq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_type)
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>class_sig_item)
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
    method class_expr : class_expr -> class_expr=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>class_expr)
      | `CeApp (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_expr _a1 in
          let _a2 = self#expr _a2 in `CeApp (_a0, _a1, _a2)
      | `CeCon (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#virtual_flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in `CeCon (_a0, _a1, _a2, _a3)
      | `CeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#class_expr _a2 in `CeFun (_a0, _a1, _a2)
      | `CeLet (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_flag _a1 in
          let _a2 = self#binding _a2 in
          let _a3 = self#class_expr _a3 in `CeLet (_a0, _a1, _a2, _a3)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_str_item _a1 in `Obj (_a0, _a1)
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#patt _a1 in
          let _a2 = self#class_str_item _a2 in `ObjPat (_a0, _a1, _a2)
      | `CeTyc (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_expr _a1 in
          let _a2 = self#class_type _a2 in `CeTyc (_a0, _a1, _a2)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_expr _a1 in
          let _a2 = self#class_expr _a2 in `And (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_expr _a1 in
          let _a2 = self#class_expr _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_expr)
    method class_str_item : class_str_item -> class_str_item=
      function
      | #nil as _a0 -> (self#nil _a0 : nil  :>class_str_item)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#class_str_item _a1 in
          let _a2 = self#class_str_item _a2 in `Sem (_a0, _a1, _a2)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | `Inherit (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#override_flag _a1 in
          let _a2 = self#class_expr _a2 in `Inherit (_a0, _a1, _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#override_flag _a1 in
          let _a2 = self#class_expr _a2 in
          let _a3 = self#alident _a3 in `InheritAs (_a0, _a1, _a2, _a3)
      | `Initializer (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#expr _a1 in `Initializer (_a0, _a1)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#override_flag _a2 in
          let _a3 = self#private_flag _a3 in
          let _a4 = self#expr _a4 in
          let _a5 = self#ctyp _a5 in `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#override_flag _a2 in
          let _a3 = self#mutable_flag _a3 in
          let _a4 = self#expr _a4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
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
      | #ant as _a0 -> (self#ant _a0 : ant  :>class_str_item)
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
    method ant_nil : ant_nil -> 'self_type=
      function
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
    method meta_bool : meta_bool -> 'self_type=
      function
      | `True _a0 -> self#loc _a0
      | `False _a0 -> self#loc _a0
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
    method sid : sid -> 'self_type=
      fun (`Id (_a0,_a1))  -> let self = self#loc _a0 in self#ident _a1
    method any : any -> 'self_type= fun (`Any _a0)  -> self#loc _a0
    method ctyp : ctyp -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
      | `TyPol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
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
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
    method row_field : row_field -> 'self_type=
      function
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'self_type)
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
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#tag_names _a1 in self#tag_names _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
    method typedecl : typedecl -> 'self_type=
      function
      | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#list (fun self  -> self#ctyp) _a2 in
          let self = self#type_info _a3 in
          self#list
            (fun self  (_a0,_a1)  ->
               let self = self#ctyp _a0 in self#ctyp _a1) _a4
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#typedecl _a1 in self#typedecl _a2
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'self_type)
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
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
    method type_repr : type_repr -> 'self_type=
      function
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#name_ctyp _a1
      | `Sum (_a0,_a1) -> let self = self#loc _a0 in self#or_ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
      | #ant_nil as _a0 -> (self#ant_nil _a0 :>'self_type)
    method of_ctyp : of_ctyp -> 'self_type=
      function
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#sid _a1 in self#ctyp _a2
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
    method patt : patt -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#patt _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#patt _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#patt _a2
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#patt _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_patt _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#alident _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#patt _a1
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#patt _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#patt _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OptLablExpr (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#patt _a2 in self#expr _a3
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#patt _a2
      | `PaRng (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#patt _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#ctyp _a2
      | `ClassPath (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#patt _a1
      | `ModuleUnpack (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `ModuleConstraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
    method rec_patt : rec_patt -> 'self_type=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#patt _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_patt _a1 in self#rec_patt _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method expr : expr -> 'self_type=
      function
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `Tup (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_expr _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
      | #literal as _a0 -> (self#literal _a0 :>'self_type)
      | `RecordWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_expr _a1 in self#expr _a2
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `ArrayDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `ExAsf _a0 -> self#loc _a0
      | `ExAsr (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `Assign (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#expr _a2 in
          let self = self#expr _a3 in
          let self = self#direction_flag _a4 in self#expr _a5
      | `Fun (_a0,_a1) -> let self = self#loc _a0 in self#match_case _a1
      | `IfThenElse (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in
          let self = self#expr _a2 in self#expr _a3
      | `IfThen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `LabelS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `Label (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#expr _a2
      | `Lazy (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#rec_flag _a1 in
          let self = self#binding _a2 in self#expr _a3
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_expr _a2 in self#expr _a3
      | `Match (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#match_case _a2
      | `New (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#class_str_item _a1
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#class_str_item _a2
      | `OptLabl (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#expr _a2
      | `OptLablS (_a0,_a1) -> let self = self#loc _a0 in self#alident _a1
      | `OvrInst (_a0,_a1) -> let self = self#loc _a0 in self#rec_expr _a1
      | `OvrInstEmpty _a0 -> self#loc _a0
      | `Seq (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `Send (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#alident _a2
      | `StringDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `Try (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#match_case _a2
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#ctyp _a2
      | `Coercion (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in
          let self = self#ctyp _a2 in self#ctyp _a3
      | `While (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#expr _a1 in self#expr _a2
      | `LetOpen (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#expr _a2
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#expr _a2
      | `Package_expr (_a0,_a1) ->
          let self = self#loc _a0 in self#module_expr _a1
    method rec_expr : rec_expr -> 'self_type=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_expr _a1 in self#rec_expr _a2
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#expr _a2
      | #any as _a0 -> (self#any _a0 :>'self_type)
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_type : module_type -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `MtFun (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_type _a3
      | `Sig (_a0,_a1) -> let self = self#loc _a0 in self#sig_item _a1
      | `With (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_type _a1 in self#with_constr _a2
      | `ModuleTypeOf (_a0,_a1) ->
          let self = self#loc _a0 in self#module_expr _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method sig_item : sig_item -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
          let self = self#alident _a1 in self#expr _a2
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
      | `Open (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#typedecl _a1
      | `Val (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method with_constr : with_constr -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#binding _a1 in self#binding _a2
      | `Bind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#expr _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_binding : module_binding -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_binding _a1 in self#module_binding _a2
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_expr _a3
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_type _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method match_case : match_case -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `Or (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#match_case _a1 in self#match_case _a2
      | `Case (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#expr _a2
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in
          let self = self#expr _a2 in self#expr _a3
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method module_expr : module_expr -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | #sid as _a0 -> (self#sid _a0 :>'self_type)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_expr _a1 in self#module_expr _a2
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#module_type _a2 in self#module_expr _a3
      | `Struct (_a0,_a1) -> let self = self#loc _a0 in self#str_item _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#module_expr _a1 in self#module_type _a2
      | `PackageModule (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method str_item : str_item -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#class_expr _a1
      | `ClassType (_a0,_a1) ->
          let self = self#loc _a0 in self#class_type _a1
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#str_item _a1 in self#str_item _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#expr _a2
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `StExp (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#module_expr _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#module_expr _a2
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
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `CtCon (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in
          let self = self#ident _a2 in self#type_parameters _a3
      | `CtFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#class_type _a2
      | `CtSig (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#class_sig_item _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | `CtCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | `CtEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_type _a1 in self#class_type _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
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
    method class_expr : class_expr -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `CeApp (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_expr _a1 in self#expr _a2
      | `CeCon (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#virtual_flag _a1 in
          let self = self#ident _a2 in self#type_parameters _a3
      | `CeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#class_expr _a2
      | `CeLet (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#rec_flag _a1 in
          let self = self#binding _a2 in self#class_expr _a3
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#class_str_item _a1
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#patt _a1 in self#class_str_item _a2
      | `CeTyc (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_expr _a1 in self#class_type _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_expr _a1 in self#class_expr _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_expr _a1 in self#class_expr _a2
      | #ant as _a0 -> (self#ant _a0 :>'self_type)
    method class_str_item : class_str_item -> 'self_type=
      function
      | #nil as _a0 -> (self#nil _a0 :>'self_type)
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#class_str_item _a1 in self#class_str_item _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Inherit (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#override_flag _a1 in self#class_expr _a2
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#override_flag _a1 in
          let self = self#class_expr _a2 in self#alident _a3
      | `Initializer (_a0,_a1) -> let self = self#loc _a0 in self#expr _a1
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#override_flag _a2 in
          let self = self#private_flag _a3 in
          let self = self#expr _a4 in self#ctyp _a5
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#override_flag _a2 in
          let self = self#mutable_flag _a3 in self#expr _a4
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
    method loc : 'fmt -> loc -> 'result275=
      fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> 'result276=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> 'result277=
      fun fmt  (`Nil _a0)  ->
        Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
    method ant_nil : 'fmt -> ant_nil -> 'result278=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result278)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result278)
    method literal : 'fmt -> literal -> 'result279=
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
    method rec_flag : 'fmt -> rec_flag -> 'result280=
      fun fmt  ->
        function
        | `Recursive _a0 ->
            Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" self#loc _a0
        | `ReNil _a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result280)
    method direction_flag : 'fmt -> direction_flag -> 'result281=
      fun fmt  ->
        function
        | `To _a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" self#loc _a0
        | `Downto _a0 ->
            Format.fprintf fmt "@[<1>(`Downto@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result281)
    method mutable_flag : 'fmt -> mutable_flag -> 'result282=
      fun fmt  ->
        function
        | `Mutable _a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" self#loc _a0
        | `MuNil _a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result282)
    method private_flag : 'fmt -> private_flag -> 'result283=
      fun fmt  ->
        function
        | `Private _a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]" self#loc _a0
        | `PrNil _a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result283)
    method virtual_flag : 'fmt -> virtual_flag -> 'result284=
      fun fmt  ->
        function
        | `Virtual _a0 ->
            Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" self#loc _a0
        | `ViNil _a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result284)
    method override_flag : 'fmt -> override_flag -> 'result285=
      fun fmt  ->
        function
        | `Override _a0 ->
            Format.fprintf fmt "@[<1>(`Override@ %a)@]" self#loc _a0
        | `OvNil _a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result285)
    method row_var_flag : 'fmt -> row_var_flag -> 'result286=
      fun fmt  ->
        function
        | `RowVar _a0 ->
            Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" self#loc _a0
        | `RvNil _a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result286)
    method position_flag : 'fmt -> position_flag -> 'result287=
      fun fmt  ->
        function
        | `Positive _a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc _a0
        | `Negative _a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc _a0
        | `Normal _a0 ->
            Format.fprintf fmt "@[<1>(`Normal@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result287)
    method meta_bool : 'fmt -> meta_bool -> 'result288=
      fun fmt  ->
        function
        | `True _a0 -> Format.fprintf fmt "@[<1>(`True@ %a)@]" self#loc _a0
        | `False _a0 -> Format.fprintf fmt "@[<1>(`False@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result288)
    method strings : 'fmt -> strings -> 'result289=
      fun fmt  ->
        function
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#strings _a1 self#strings _a2
        | `Str (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result289)
    method alident : 'fmt -> alident -> 'result290=
      fun fmt  ->
        function
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result290)
    method auident : 'fmt -> auident -> 'result291=
      fun fmt  ->
        function
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result291)
    method aident : 'fmt -> aident -> 'result292=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>'result292)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result292)
    method astring : 'fmt -> astring -> 'result293=
      fun fmt  ->
        function
        | `C (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" self#loc _a0 self#string
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result293)
    method uident : 'fmt -> uident -> 'result294=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#uident _a1 self#uident _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>'result294)
    method ident : 'fmt -> ident -> 'result295=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>'result295)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result295)
    method dupath : 'fmt -> dupath -> 'result296=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#dupath _a2
        | #auident as _a0 -> (self#auident fmt _a0 :>'result296)
    method dlpath : 'fmt -> dlpath -> 'result297=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#dupath _a1 self#alident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>'result297)
    method sid : 'fmt -> sid -> 'result298=
      fun fmt  (`Id (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc _a0 self#ident _a1
    method any : 'fmt -> any -> 'result299=
      fun fmt  (`Any _a0)  ->
        Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
    method ctyp : 'fmt -> ctyp -> 'result300=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result300)
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#alident _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result300)
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
        | #sid as _a0 -> (self#sid fmt _a0 :>'result300)
        | `TyObj (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1 self#row_var_flag _a2
        | `TyPol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
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
        | `PolyInfSup (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" self#loc
              _a0 self#row_field _a1 self#tag_names _a2
        | `Package (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" self#loc _a0
              self#module_type _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result300)
    method type_parameters : 'fmt -> type_parameters -> 'result301=
      fun fmt  ->
        function
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#type_parameters _a1 self#type_parameters _a2
        | `Ctyp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result301)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result301)
    method row_field : 'fmt -> row_field -> 'result302=
      fun fmt  ->
        function
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result302)
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
    method tag_names : 'fmt -> tag_names -> 'result303=
      fun fmt  ->
        function
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result303)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#tag_names _a1 self#tag_names _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
    method typedecl : 'fmt -> typedecl -> 'result304=
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1
              (self#list (fun self  -> self#ctyp)) _a2 self#type_info _a3
              (self#list
                 (fun self  fmt  (_a0,_a1)  ->
                    Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#ctyp _a0
                      self#ctyp _a1)) _a4
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1 self#typedecl _a2
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result304)
    method type_info : 'fmt -> type_info -> 'result305=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result305)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result305)
    method type_repr : 'fmt -> type_repr -> 'result306=
      fun fmt  ->
        function
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1
        | `Sum (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" self#loc _a0
              self#or_ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result306)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result306)
    method name_ctyp : 'fmt -> name_ctyp -> 'result307=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result307)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result307)
    method or_ctyp : 'fmt -> or_ctyp -> 'result308=
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
        | #sid as _a0 -> (self#sid fmt _a0 :>'result308)
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result308)
    method of_ctyp : 'fmt -> of_ctyp -> 'result309=
      fun fmt  ->
        function
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result309)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result309)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result309)
    method patt : 'fmt -> patt -> 'result310=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result310)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#patt _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#patt _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#patt _a2
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#patt
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result310)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_patt _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result310)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result310)
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#alident _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#patt _a1
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#patt _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#patt _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OptLablExpr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#patt _a2 self#expr _a3
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#patt _a2
        | `PaRng (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#patt _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#patt _a1 self#ctyp _a2
        | `ClassPath (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0
              self#patt _a1
        | `ModuleUnpack (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" self#loc _a0
              self#auident _a1
        | `ModuleConstraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#ctyp _a2
    method rec_patt : 'fmt -> rec_patt -> 'result311=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#patt _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_patt _a1 self#rec_patt _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result311)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result311)
    method expr : 'fmt -> expr -> 'result312=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result312)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `Vrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `Tup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc _a0 self#expr
              _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result312)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result312)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result312)
        | `RecordWith (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" self#loc
              _a0 self#rec_expr _a1 self#expr _a2
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `ArrayDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | `ExAsf _a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" self#loc _a0
        | `ExAsr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | `Assign (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#expr _a2 self#expr _a3
              self#direction_flag _a4 self#expr _a5
        | `Fun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" self#loc _a0
              self#match_case _a1
        | `IfThenElse (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#expr _a1 self#expr _a2 self#expr _a3
        | `IfThen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `LabelS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#expr _a2
        | `Lazy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | `LetIn (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2 self#expr _a3
        | `LetModule (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#auident _a1 self#module_expr _a2 self#expr
              _a3
        | `Match (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#match_case _a2
        | `New (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#class_str_item _a1
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#class_str_item _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#expr _a2
        | `OptLablS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" self#loc _a0
              self#alident _a1
        | `OvrInst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1
        | `OvrInstEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" self#loc _a0
        | `Seq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" self#loc _a0 self#expr
              _a1
        | `Send (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#alident _a2
        | `StringDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `Try (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#match_case _a2
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#expr _a1 self#ctyp _a2
        | `Coercion (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#expr _a1 self#ctyp _a2 self#ctyp _a3
        | `While (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `LetOpen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#expr _a2
        | `LocalTypeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#expr _a2
        | `Package_expr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Package_expr@ %a@ %a)@]" self#loc _a0
              self#module_expr _a1
    method rec_expr : 'fmt -> rec_expr -> 'result313=
      fun fmt  ->
        function
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1 self#rec_expr _a2
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#expr _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result313)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result313)
    method module_type : 'fmt -> module_type -> 'result314=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result314)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result314)
        | `MtFun (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`MtFun@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#module_type _a2 self#module_type _a3
        | `Sig (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" self#loc _a0
              self#sig_item _a1
        | `With (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" self#loc _a0
              self#module_type _a1 self#with_constr _a2
        | `ModuleTypeOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" self#loc _a0
              self#module_expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result314)
    method sig_item : 'fmt -> sig_item -> 'result315=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result315)
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
              self#alident _a1 self#expr _a2
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
        | `Open (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc _a0
              self#ident _a1
        | `Type (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc _a0
              self#typedecl _a1
        | `Val (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result315)
    method with_constr : 'fmt -> with_constr -> 'result316=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result316)
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result316)
    method binding : 'fmt -> binding -> 'result317=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result317)
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#binding _a1 self#binding _a2
        | `Bind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#expr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result317)
    method module_binding : 'fmt -> module_binding -> 'result318=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result318)
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#module_binding _a1 self#module_binding _a2
        | `ModuleBind (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#auident _a1 self#module_type _a2
              self#module_expr _a3
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result318)
    method match_case : 'fmt -> match_case -> 'result319=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result319)
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#match_case _a1 self#match_case _a2
        | `Case (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#expr _a2
        | `CaseWhen (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#patt _a1 self#expr _a2 self#expr _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result319)
    method module_expr : 'fmt -> module_expr -> 'result320=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result320)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result320)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#module_expr _a1 self#module_expr _a2
        | `Functor (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1 self#module_type _a2 self#module_expr _a3
        | `Struct (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" self#loc _a0
              self#str_item _a1
        | `Constraint (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" self#loc
              _a0 self#module_expr _a1 self#module_type _a2
        | `PackageModule (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result320)
    method str_item : 'fmt -> str_item -> 'result321=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result321)
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#str_item _a1 self#str_item _a2
        | `DirectiveSimple (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" self#loc
              _a0 self#alident _a1
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#expr _a2
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `StExp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2 self#strings _a3
        | `Include (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc _a0
              self#module_expr _a1
        | `Module (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc _a0
              self#auident _a1 self#module_expr _a2
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result321)
    method class_type : 'fmt -> class_type -> 'result322=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result322)
        | `CtCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CtCon@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2 self#type_parameters _a3
        | `CtFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#class_type _a2
        | `CtSig (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtSig@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#class_sig_item _a2
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | `CtCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | `CtEq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CtEq@ %a@ %a@ %a)@]" self#loc _a0
              self#class_type _a1 self#class_type _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result322)
    method class_sig_item : 'fmt -> class_sig_item -> 'result323=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result323)
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result323)
    method class_expr : 'fmt -> class_expr -> 'result324=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result324)
        | `CeApp (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1 self#expr _a2
        | `CeCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CeCon@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2 self#type_parameters _a3
        | `CeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#class_expr _a2
        | `CeLet (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CeLet@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2 self#class_expr _a3
        | `Obj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" self#loc _a0
              self#class_str_item _a1
        | `ObjPat (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#class_str_item _a2
        | `CeTyc (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeTyc@ %a@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1 self#class_type _a2
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1 self#class_expr _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1 self#class_expr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result324)
    method class_str_item : 'fmt -> class_str_item -> 'result325=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result325)
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#class_str_item _a1 self#class_str_item _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Inherit (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" self#loc _a0
              self#override_flag _a1 self#class_expr _a2
        | `InheritAs (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#override_flag _a1 self#class_expr _a2
              self#alident _a3
        | `Initializer (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" self#loc _a0
              self#expr _a1
        | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc _a0 self#alident _a1 self#override_flag _a2
              self#private_flag _a3 self#expr _a4 self#ctyp _a5
        | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc _a0 self#alident _a1 self#override_flag _a2
              self#mutable_flag _a3 self#expr _a4
        | `CrVir (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#private_flag _a2 self#ctyp _a3
        | `CrVvr (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#mutable_flag _a2 self#ctyp _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result325)
    method ep : 'fmt -> ep -> 'result326=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result326)
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
        | #any as _a0 -> (self#any fmt _a0 :>'result326)
        | `ArrayEmpty _a0 ->
            Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" self#loc _a0
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0 
              self#ep _a1
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1
        | #literal as _a0 -> (self#literal fmt _a0 :>'result326)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result326)
    method rec_bind : 'fmt -> rec_bind -> 'result327=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1 self#rec_bind _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result327)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result327)
    method fanloc_t : 'fmt -> FanLoc.t -> 'result328= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> 'result329=
      self#unknown
  end
class eq =
  object (self : 'self_type)
    inherit  eqbase
    method loc : loc -> loc -> 'result330=
      fun _a0  _a1  -> self#fanloc_t _a0 _a1
    method ant : ant -> ant -> 'result331=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#fanutil_anti_cxt _a1 _b1)
    method nil : nil -> nil -> 'result332=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
    method ant_nil : ant_nil -> ant_nil -> 'result333=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result333)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result333)
        | (_,_) -> false
    method literal : literal -> literal -> 'result334=
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
    method rec_flag : rec_flag -> rec_flag -> 'result335=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive _a0,`Recursive _b0) -> self#loc _a0 _b0
        | (`ReNil _a0,`ReNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result335)
        | (_,_) -> false
    method direction_flag : direction_flag -> direction_flag -> 'result336=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To _a0,`To _b0) -> self#loc _a0 _b0
        | (`Downto _a0,`Downto _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result336)
        | (_,_) -> false
    method mutable_flag : mutable_flag -> mutable_flag -> 'result337=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable _a0,`Mutable _b0) -> self#loc _a0 _b0
        | (`MuNil _a0,`MuNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result337)
        | (_,_) -> false
    method private_flag : private_flag -> private_flag -> 'result338=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private _a0,`Private _b0) -> self#loc _a0 _b0
        | (`PrNil _a0,`PrNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result338)
        | (_,_) -> false
    method virtual_flag : virtual_flag -> virtual_flag -> 'result339=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual _a0,`Virtual _b0) -> self#loc _a0 _b0
        | (`ViNil _a0,`ViNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result339)
        | (_,_) -> false
    method override_flag : override_flag -> override_flag -> 'result340=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override _a0,`Override _b0) -> self#loc _a0 _b0
        | (`OvNil _a0,`OvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result340)
        | (_,_) -> false
    method row_var_flag : row_var_flag -> row_var_flag -> 'result341=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar _a0,`RowVar _b0) -> self#loc _a0 _b0
        | (`RvNil _a0,`RvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result341)
        | (_,_) -> false
    method position_flag : position_flag -> position_flag -> 'result342=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive _a0,`Positive _b0) -> self#loc _a0 _b0
        | (`Negative _a0,`Negative _b0) -> self#loc _a0 _b0
        | (`Normal _a0,`Normal _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result342)
        | (_,_) -> false
    method meta_bool : meta_bool -> meta_bool -> 'result343=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`True _a0,`True _b0) -> self#loc _a0 _b0
        | (`False _a0,`False _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result343)
        | (_,_) -> false
    method strings : strings -> strings -> 'result344=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#strings _a1 _b1)) &&
              (self#strings _a2 _b2)
        | (`Str (_a0,_a1),`Str (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result344)
        | (_,_) -> false
    method alident : alident -> alident -> 'result345=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result345)
        | (_,_) -> false
    method auident : auident -> auident -> 'result346=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result346)
        | (_,_) -> false
    method aident : aident -> aident -> 'result347=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result347)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result347)
        | (_,_) -> false
    method astring : astring -> astring -> 'result348=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C (_a0,_a1),`C (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result348)
        | (_,_) -> false
    method uident : uident -> uident -> 'result349=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#uident _a1 _b1)) &&
              (self#uident _a2 _b2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#uident _a1 _b1)) &&
              (self#uident _a2 _b2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result349)
        | (_,_) -> false
    method ident : ident -> ident -> 'result350=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result350)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result350)
        | (_,_) -> false
    method dupath : dupath -> dupath -> 'result351=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#dupath _a1 _b1)) &&
              (self#dupath _a2 _b2)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result351)
        | (_,_) -> false
    method dlpath : dlpath -> dlpath -> 'result352=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#dupath _a1 _b1)) &&
              (self#alident _a2 _b2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result352)
        | (_,_) -> false
    method sid : sid -> sid -> 'result353=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
    method any : any -> any -> 'result354=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
    method ctyp : ctyp -> ctyp -> 'result355=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result355)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#alident _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result355)
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
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result355)
        | (`TyObj (_a0,_a1,_a2),`TyObj (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)) &&
              (self#row_var_flag _a2 _b2)
        | (`TyPol (_a0,_a1,_a2),`TyPol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
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
        | (`PolyInfSup (_a0,_a1,_a2),`PolyInfSup (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#row_field _a1 _b1)) &&
              (self#tag_names _a2 _b2)
        | (`Package (_a0,_a1),`Package (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_type _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result355)
        | (_,_) -> false
    method type_parameters :
      type_parameters -> type_parameters -> 'result356=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#type_parameters _a1 _b1)) &&
              (self#type_parameters _a2 _b2)
        | (`Ctyp (_a0,_a1),`Ctyp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result356)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result356)
        | (_,_) -> false
    method row_field : row_field -> row_field -> 'result357=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result357)
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
    method tag_names : tag_names -> tag_names -> 'result358=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result358)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#tag_names _a1 _b1)) &&
              (self#tag_names _a2 _b2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#astring _a1 _b1)
        | (_,_) -> false
    method typedecl : typedecl -> typedecl -> 'result359=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyDcl (_a0,_a1,_a2,_a3,_a4),`TyDcl (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#list (fun self  -> self#ctyp) _a2 _b2))
               && (self#type_info _a3 _b3))
              &&
              (self#list
                 (fun self  _a0  _b0  ->
                    match (_a0, _b0) with
                    | ((_a0,_a1),(_b0,_b1)) ->
                        (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)) _a4 _b4)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#typedecl _a1 _b1)) &&
              (self#typedecl _a2 _b2)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result359)
        | (_,_) -> false
    method type_info : type_info -> type_info -> 'result360=
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result360)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result360)
        | (_,_) -> false
    method type_repr : type_repr -> type_repr -> 'result361=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#or_ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result361)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result361)
        | (_,_) -> false
    method name_ctyp : name_ctyp -> name_ctyp -> 'result362=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)) &&
              (self#name_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`TyColMut (_a0,_a1,_a2),`TyColMut (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result362)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result362)
        | (_,_) -> false
    method or_ctyp : or_ctyp -> or_ctyp -> 'result363=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#or_ctyp _a1 _b1)) &&
              (self#or_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result363)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result363)
        | (_,_) -> false
    method of_ctyp : of_ctyp -> of_ctyp -> 'result364=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result364)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result364)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result364)
        | (_,_) -> false
    method patt : patt -> patt -> 'result365=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result365)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#patt _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result365)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_patt _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result365)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result365)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#patt _a1 _b1)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`OptLablExpr (_a0,_a1,_a2,_a3),`OptLablExpr (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#patt _a2 _b2))
              && (self#expr _a3 _b3)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`PaRng (_a0,_a1,_a2),`PaRng (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`ClassPath (_a0,_a1),`ClassPath (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#patt _a1 _b1)
        | (`ModuleUnpack (_a0,_a1),`ModuleUnpack (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#auident _a1 _b1)
        | (`ModuleConstraint (_a0,_a1,_a2),`ModuleConstraint (_b0,_b1,_b2))
            ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (_,_) -> false
    method rec_patt : rec_patt -> rec_patt -> 'result366=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_patt _a1 _b1)) &&
              (self#rec_patt _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result366)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result366)
        | (_,_) -> false
    method expr : expr -> expr -> 'result367=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result367)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Vrn (_a0,_a1),`Vrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Tup (_a0,_a1),`Tup (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result367)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result367)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result367)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`ExAsf _a0,`ExAsf _b0) -> self#loc _a0 _b0
        | (`ExAsr (_a0,_a1),`ExAsr (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`Assign (_a0,_a1,_a2),`Assign (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`For (_a0,_a1,_a2,_a3,_a4,_a5),`For (_b0,_b1,_b2,_b3,_b4,_b5)) ->
            (((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                 (self#expr _a2 _b2))
                && (self#expr _a3 _b3))
               && (self#direction_flag _a4 _b4))
              && (self#expr _a5 _b5)
        | (`Fun (_a0,_a1),`Fun (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#match_case _a1 _b1)
        | (`IfThenElse (_a0,_a1,_a2,_a3),`IfThenElse (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
               (self#expr _a2 _b2))
              && (self#expr _a3 _b3)
        | (`IfThen (_a0,_a1,_a2),`IfThen (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`LabelS (_a0,_a1),`LabelS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Lazy (_a0,_a1),`Lazy (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`LetIn (_a0,_a1,_a2,_a3),`LetIn (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
               (self#binding _a2 _b2))
              && (self#expr _a3 _b3)
        | (`LetModule (_a0,_a1,_a2,_a3),`LetModule (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_expr _a2 _b2))
              && (self#expr _a3 _b3)
        | (`Match (_a0,_a1,_a2),`Match (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#match_case _a2 _b2)
        | (`New (_a0,_a1),`New (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_str_item _a1 _b1)
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#class_str_item _a2 _b2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`OptLablS (_a0,_a1),`OptLablS (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_expr _a1 _b1)
        | (`OvrInstEmpty _a0,`OvrInstEmpty _b0) -> self#loc _a0 _b0
        | (`Seq (_a0,_a1),`Seq (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`Send (_a0,_a1,_a2),`Send (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`StringDot (_a0,_a1,_a2),`StringDot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Try (_a0,_a1,_a2),`Try (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#match_case _a2 _b2)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Coercion (_a0,_a1,_a2,_a3),`Coercion (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`While (_a0,_a1,_a2),`While (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`LetOpen (_a0,_a1,_a2),`LetOpen (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`LocalTypeFun (_a0,_a1,_a2),`LocalTypeFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Package_expr (_a0,_a1),`Package_expr (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_expr _a1 _b1)
        | (_,_) -> false
    method rec_expr : rec_expr -> rec_expr -> 'result368=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_expr _a1 _b1)) &&
              (self#rec_expr _a2 _b2)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result368)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result368)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result369=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result369)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result369)
        | (`MtFun (_a0,_a1,_a2,_a3),`MtFun (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_type _a3 _b3)
        | (`Sig (_a0,_a1),`Sig (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#sig_item _a1 _b1)
        | (`With (_a0,_a1,_a2),`With (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#with_constr _a2 _b2)
        | (`ModuleTypeOf (_a0,_a1),`ModuleTypeOf (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result369)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result370=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result370)
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
              (self#expr _a2 _b2)
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
        | (`Open (_a0,_a1),`Open (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
        | (`Type (_a0,_a1),`Type (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#typedecl _a1 _b1)
        | (`Val (_a0,_a1,_a2),`Val (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result370)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result371=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result371)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result371)
        | (_,_) -> false
    method binding : binding -> binding -> 'result372=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result372)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#binding _a2 _b2)
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#expr _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result372)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result373=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result373)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_binding _a1 _b1)) &&
              (self#module_binding _a2 _b2)
        | (`ModuleBind (_a0,_a1,_a2,_a3),`ModuleBind (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_expr _a3 _b3)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result373)
        | (_,_) -> false
    method match_case : match_case -> match_case -> 'result374=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result374)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#match_case _a1 _b1)) &&
              (self#match_case _a2 _b2)
        | (`Case (_a0,_a1,_a2),`Case (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`CaseWhen (_a0,_a1,_a2,_a3),`CaseWhen (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
               (self#expr _a2 _b2))
              && (self#expr _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result374)
        | (_,_) -> false
    method module_expr : module_expr -> module_expr -> 'result375=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result375)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result375)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_expr _a1 _b1)) &&
              (self#module_expr _a2 _b2)
        | (`Functor (_a0,_a1,_a2,_a3),`Functor (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
               (self#module_type _a2 _b2))
              && (self#module_expr _a3 _b3)
        | (`Struct (_a0,_a1),`Struct (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#str_item _a1 _b1)
        | (`Constraint (_a0,_a1,_a2),`Constraint (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#module_expr _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`PackageModule (_a0,_a1),`PackageModule (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result375)
        | (_,_) -> false
    method str_item : str_item -> str_item -> 'result376=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result376)
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_expr _a1 _b1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#str_item _a1 _b1)) &&
              (self#str_item _a2 _b2)
        | (`DirectiveSimple (_a0,_a1),`DirectiveSimple (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#alident _a1 _b1)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#of_ctyp _a1 _b1)
        | (`StExp (_a0,_a1),`StExp (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#strings _a3 _b3)
        | (`Include (_a0,_a1),`Include (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#module_expr _a1 _b1)
        | (`Module (_a0,_a1,_a2),`Module (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#module_expr _a2 _b2)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result376)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result377=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result377)
        | (`CtCon (_a0,_a1,_a2,_a3),`CtCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#type_parameters _a3 _b3)
        | (`CtFun (_a0,_a1,_a2),`CtFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`CtSig (_a0,_a1,_a2),`CtSig (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#class_sig_item _a2 _b2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`CtCol (_a0,_a1,_a2),`CtCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`CtEq (_a0,_a1,_a2),`CtEq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_type _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result377)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result378=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result378)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result378)
        | (_,_) -> false
    method class_expr : class_expr -> class_expr -> 'result379=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result379)
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`CeCon (_a0,_a1,_a2,_a3),`CeCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#type_parameters _a3 _b3)
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | (`CeLet (_a0,_a1,_a2,_a3),`CeLet (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
               (self#binding _a2 _b2))
              && (self#class_expr _a3 _b3)
        | (`Obj (_a0,_a1),`Obj (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_str_item _a1 _b1)
        | (`ObjPat (_a0,_a1,_a2),`ObjPat (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#class_str_item _a2 _b2)
        | (`CeTyc (_a0,_a1,_a2),`CeTyc (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#class_type _a2 _b2)
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result379)
        | (_,_) -> false
    method class_str_item : class_str_item -> class_str_item -> 'result380=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result380)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_str_item _a1 _b1)) &&
              (self#class_str_item _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Inherit (_a0,_a1,_a2),`Inherit (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#override_flag _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | (`InheritAs (_a0,_a1,_a2,_a3),`InheritAs (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#class_expr _a2 _b2))
              && (self#alident _a3 _b3)
        | (`Initializer (_a0,_a1),`Initializer (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#expr _a1 _b1)
        | (`CrMth (_a0,_a1,_a2,_a3,_a4,_a5),`CrMth (_b0,_b1,_b2,_b3,_b4,_b5))
            ->
            (((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                 (self#override_flag _a2 _b2))
                && (self#private_flag _a3 _b3))
               && (self#expr _a4 _b4))
              && (self#ctyp _a5 _b5)
        | (`CrVal (_a0,_a1,_a2,_a3,_a4),`CrVal (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
                (self#override_flag _a2 _b2))
               && (self#mutable_flag _a3 _b3))
              && (self#expr _a4 _b4)
        | (`CrVir (_a0,_a1,_a2,_a3),`CrVir (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`CrVvr (_a0,_a1,_a2,_a3),`CrVvr (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#mutable_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result380)
        | (_,_) -> false
    method ep : ep -> ep -> 'result381=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result381)
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
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result381)
        | (`ArrayEmpty _a0,`ArrayEmpty _b0) -> self#loc _a0 _b0
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ep _a1 _b1)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_bind _a1 _b1)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result381)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result381)
        | (_,_) -> false
    method rec_bind : rec_bind -> rec_bind -> 'result382=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) && (self#ep _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_bind _a1 _b1)) &&
              (self#rec_bind _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result382)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result382)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result383= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'result384= self#unknown
  end
let strip_loc_nil (`Nil _a0) = `Nil
let strip_loc_ant_nil =
  function
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result386)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result386)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result388)
let strip_loc_direction_flag =
  function
  | `To _a0 -> `To
  | `Downto _a0 -> `Downto
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result389)
let strip_loc_mutable_flag =
  function
  | `Mutable _a0 -> `Mutable
  | `MuNil _a0 -> `MuNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result390)
let strip_loc_private_flag =
  function
  | `Private _a0 -> `Private
  | `PrNil _a0 -> `PrNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result391)
let strip_loc_virtual_flag =
  function
  | `Virtual _a0 -> `Virtual
  | `ViNil _a0 -> `ViNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result392)
let strip_loc_override_flag =
  function
  | `Override _a0 -> `Override
  | `OvNil _a0 -> `OvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result393)
let strip_loc_row_var_flag =
  function
  | `RowVar _a0 -> `RowVar
  | `RvNil _a0 -> `RvNil
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result394)
let strip_loc_position_flag =
  function
  | `Positive _a0 -> `Positive
  | `Negative _a0 -> `Negative
  | `Normal _a0 -> `Normal
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result395)
let strip_loc_meta_bool =
  function
  | `True _a0 -> `True
  | `False _a0 -> `False
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result396)
let rec strip_loc_strings =
  function
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_strings _a1 in
      let _a2 = strip_loc_strings _a2 in `App (_a1, _a2)
  | `Str (_a0,_a1) -> `Str _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result397)
let strip_loc_alident =
  function
  | `Lid (_a0,_a1) -> `Lid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result398)
let strip_loc_auident =
  function
  | `Uid (_a0,_a1) -> `Uid _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result399)
let strip_loc_aident =
  function
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result400)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result400)
let strip_loc_astring =
  function
  | `C (_a0,_a1) -> `C _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result401)
let rec strip_loc_uident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_uident _a1 in
      let _a2 = strip_loc_uident _a2 in `App (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result402)
let rec strip_loc_ident =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `Dot (_a1, _a2)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ident _a2 in `App (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result403)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result403)
let rec strip_loc_dupath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_dupath _a2 in `Dot (_a1, _a2)
  | #auident as _a0 -> (strip_loc_auident _a0 :>'result404)
let strip_loc_dlpath =
  function
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_dupath _a1 in
      let _a2 = strip_loc_alident _a2 in `Dot (_a1, _a2)
  | #alident as _a0 -> (strip_loc_alident _a0 :>'result405)
let strip_loc_sid (`Id (_a0,_a1)) = let _a1 = strip_loc_ident _a1 in `Id _a1
let strip_loc_any (`Any _a0) = `Any
let rec strip_loc_ctyp =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result433)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result433)
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
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result433)
  | `TyObj (_a0,_a1,_a2) ->
      let _a1 = strip_loc_name_ctyp _a1 in
      let _a2 = strip_loc_row_var_flag _a2 in `TyObj (_a1, _a2)
  | `TyPol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `TyPol (_a1, _a2)
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
  | `PolyInfSup (_a0,_a1,_a2) ->
      let _a1 = strip_loc_row_field _a1 in
      let _a2 = strip_loc_tag_names _a2 in `PolyInfSup (_a1, _a2)
  | `Package (_a0,_a1) -> let _a1 = strip_loc_module_type _a1 in `Package _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result433)
and strip_loc_type_parameters =
  function
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_type_parameters _a1 in
      let _a2 = strip_loc_type_parameters _a2 in `Com (_a1, _a2)
  | `Ctyp (_a0,_a1) -> let _a1 = strip_loc_ctyp _a1 in `Ctyp _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result432)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result432)
and strip_loc_row_field =
  function
  | #ant_nil as _a0 -> (strip_loc_ant_nil _a0 :>'result431)
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
  | #ant_nil as _a0 -> (strip_loc_ant_nil _a0 :>'result430)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_tag_names _a1 in
      let _a2 = strip_loc_tag_names _a2 in `App (_a1, _a2)
  | `TyVrn (_a0,_a1) -> let _a1 = strip_loc_astring _a1 in `TyVrn _a1
and strip_loc_typedecl =
  function
  | `TyDcl (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_list strip_loc_ctyp _a2 in
      let _a3 = strip_loc_type_info _a3 in
      let _a4 =
        strip_loc_list
          (fun (_a0,_a1)  ->
             let _a0 = strip_loc_ctyp _a0 in
             let _a1 = strip_loc_ctyp _a1 in (_a0, _a1)) _a4 in
      `TyDcl (_a1, _a2, _a3, _a4)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_typedecl _a1 in
      let _a2 = strip_loc_typedecl _a2 in `And (_a1, _a2)
  | #ant_nil as _a0 -> (strip_loc_ant_nil _a0 :>'result429)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result428)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result428)
and strip_loc_type_repr =
  function
  | `Record (_a0,_a1) -> let _a1 = strip_loc_name_ctyp _a1 in `Record _a1
  | `Sum (_a0,_a1) -> let _a1 = strip_loc_or_ctyp _a1 in `Sum _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result427)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result427)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result426)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result426)
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
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result425)
  | #ant_nil as _a0 -> (strip_loc_ant_nil _a0 :>'result425)
and strip_loc_of_ctyp =
  function
  | `Of (_a0,_a1,_a2) ->
      let _a1 = strip_loc_sid _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Of (_a1, _a2)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result424)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result424)
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result424)
and strip_loc_patt =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result423)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Sem (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Tup _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result423)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_patt _a1 in `Record _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result423)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result423)
  | `Alias (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_alident _a2 in `Alias (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Array _a1
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_patt _a2 in `Label (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_patt _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_patt _a2 in
      let _a3 = strip_loc_expr _a3 in `OptLablExpr (_a1, _a2, _a3)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `Or (_a1, _a2)
  | `PaRng (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_patt _a2 in `PaRng (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `ClassPath (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `ClassPath _a1
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_patt _a1 in `Lazy _a1
  | `ModuleUnpack (_a0,_a1) ->
      let _a1 = strip_loc_auident _a1 in `ModuleUnpack _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `ModuleConstraint (_a1, _a2)
and strip_loc_rec_patt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_patt _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_patt _a1 in
      let _a2 = strip_loc_rec_patt _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result422)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result422)
and strip_loc_expr =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result421)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `App (_a1, _a2)
  | `Vrn (_a0,_a1) -> `Vrn _a1
  | `Com (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Com (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Sem (_a1, _a2)
  | `Tup (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Tup _a1
  | #any as _a0 -> (strip_loc_any _a0 :>'result421)
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_expr _a1 in `Record _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result421)
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result421)
  | `RecordWith (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `RecordWith (_a1, _a2)
  | `Dot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Dot (_a1, _a2)
  | `ArrayDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `ArrayDot (_a1, _a2)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Array _a1
  | `ExAsf _a0 -> `ExAsf
  | `ExAsr (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `ExAsr _a1
  | `Assign (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `Assign (_a1, _a2)
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in
      let _a4 = strip_loc_direction_flag _a4 in
      let _a5 = strip_loc_expr _a5 in `For (_a1, _a2, _a3, _a4, _a5)
  | `Fun (_a0,_a1) -> let _a1 = strip_loc_match_case _a1 in `Fun _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `IfThenElse (_a1, _a2, _a3)
  | `IfThen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `IfThen (_a1, _a2)
  | `LabelS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `LabelS _a1
  | `Label (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `Label (_a1, _a2)
  | `Lazy (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Lazy _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_expr _a3 in `LetIn (_a1, _a2, _a3)
  | `LetModule (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `LetModule (_a1, _a2, _a3)
  | `Match (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_match_case _a2 in `Match (_a1, _a2)
  | `New (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `New _a1
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_class_str_item _a1 in `Obj _a1
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `ObjPat (_a1, _a2)
  | `OptLabl (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `OptLabl (_a1, _a2)
  | `OptLablS (_a0,_a1) -> let _a1 = strip_loc_alident _a1 in `OptLablS _a1
  | `OvrInst (_a0,_a1) -> let _a1 = strip_loc_rec_expr _a1 in `OvrInst _a1
  | `OvrInstEmpty _a0 -> `OvrInstEmpty
  | `Seq (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `Seq _a1
  | `Send (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_alident _a2 in `Send (_a1, _a2)
  | `StringDot (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `StringDot (_a1, _a2)
  | `Try (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_match_case _a2 in `Try (_a1, _a2)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Constraint (_a1, _a2)
  | `Coercion (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_ctyp _a3 in `Coercion (_a1, _a2, _a3)
  | `While (_a0,_a1,_a2) ->
      let _a1 = strip_loc_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `While (_a1, _a2)
  | `LetOpen (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_expr _a2 in `LetOpen (_a1, _a2)
  | `LocalTypeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `LocalTypeFun (_a1, _a2)
  | `Package_expr (_a0,_a1) ->
      let _a1 = strip_loc_module_expr _a1 in `Package_expr _a1
and strip_loc_rec_expr =
  function
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_expr _a1 in
      let _a2 = strip_loc_rec_expr _a2 in `Sem (_a1, _a2)
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_expr _a2 in `RecBind (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result420)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result420)
and strip_loc_module_type =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result419)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result419)
  | `MtFun (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_type _a3 in `MtFun (_a1, _a2, _a3)
  | `Sig (_a0,_a1) -> let _a1 = strip_loc_sig_item _a1 in `Sig _a1
  | `With (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_type _a1 in
      let _a2 = strip_loc_with_constr _a2 in `With (_a1, _a2)
  | `ModuleTypeOf (_a0,_a1) ->
      let _a1 = strip_loc_module_expr _a1 in `ModuleTypeOf _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result419)
and strip_loc_sig_item =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result418)
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
      let _a2 = strip_loc_expr _a2 in `Directive (_a1, _a2)
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
  | `Open (_a0,_a1) -> let _a1 = strip_loc_ident _a1 in `Open _a1
  | `Type (_a0,_a1) -> let _a1 = strip_loc_typedecl _a1 in `Type _a1
  | `Val (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Val (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result418)
and strip_loc_with_constr =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result417)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result417)
and strip_loc_binding =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result416)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_binding _a1 in
      let _a2 = strip_loc_binding _a2 in `And (_a1, _a2)
  | `Bind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_expr _a2 in `Bind (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result416)
and strip_loc_module_binding =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result415)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_binding _a1 in
      let _a2 = strip_loc_module_binding _a2 in `And (_a1, _a2)
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_expr _a3 in `ModuleBind (_a1, _a2, _a3)
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result415)
and strip_loc_match_case =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result414)
  | `Or (_a0,_a1,_a2) ->
      let _a1 = strip_loc_match_case _a1 in
      let _a2 = strip_loc_match_case _a2 in `Or (_a1, _a2)
  | `Case (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_expr _a2 in `Case (_a1, _a2)
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_expr _a2 in
      let _a3 = strip_loc_expr _a3 in `CaseWhen (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result414)
and strip_loc_module_expr =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result413)
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result413)
  | `App (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_expr _a1 in
      let _a2 = strip_loc_module_expr _a2 in `App (_a1, _a2)
  | `Functor (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_type _a2 in
      let _a3 = strip_loc_module_expr _a3 in `Functor (_a1, _a2, _a3)
  | `Struct (_a0,_a1) -> let _a1 = strip_loc_str_item _a1 in `Struct _a1
  | `Constraint (_a0,_a1,_a2) ->
      let _a1 = strip_loc_module_expr _a1 in
      let _a2 = strip_loc_module_type _a2 in `Constraint (_a1, _a2)
  | `PackageModule (_a0,_a1) ->
      let _a1 = strip_loc_expr _a1 in `PackageModule _a1
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result413)
and strip_loc_str_item =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result412)
  | `Class (_a0,_a1) -> let _a1 = strip_loc_class_expr _a1 in `Class _a1
  | `ClassType (_a0,_a1) ->
      let _a1 = strip_loc_class_type _a1 in `ClassType _a1
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_str_item _a1 in
      let _a2 = strip_loc_str_item _a2 in `Sem (_a1, _a2)
  | `DirectiveSimple (_a0,_a1) ->
      let _a1 = strip_loc_alident _a1 in `DirectiveSimple _a1
  | `Directive (_a0,_a1,_a2) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_expr _a2 in `Directive (_a1, _a2)
  | `Exception (_a0,_a1) -> let _a1 = strip_loc_of_ctyp _a1 in `Exception _a1
  | `StExp (_a0,_a1) -> let _a1 = strip_loc_expr _a1 in `StExp _a1
  | `External (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_ctyp _a2 in
      let _a3 = strip_loc_strings _a3 in `External (_a1, _a2, _a3)
  | `Include (_a0,_a1) -> let _a1 = strip_loc_module_expr _a1 in `Include _a1
  | `Module (_a0,_a1,_a2) ->
      let _a1 = strip_loc_auident _a1 in
      let _a2 = strip_loc_module_expr _a2 in `Module (_a1, _a2)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result412)
and strip_loc_class_type =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result411)
  | `CtCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in `CtCon (_a1, _a2, _a3)
  | `CtFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtFun (_a1, _a2)
  | `CtSig (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_class_sig_item _a2 in `CtSig (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `And (_a1, _a2)
  | `CtCol (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtCol (_a1, _a2)
  | `CtEq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_type _a1 in
      let _a2 = strip_loc_class_type _a2 in `CtEq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result411)
and strip_loc_class_sig_item =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result410)
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
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result410)
and strip_loc_class_expr =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result409)
  | `CeApp (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_expr _a2 in `CeApp (_a1, _a2)
  | `CeCon (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_virtual_flag _a1 in
      let _a2 = strip_loc_ident _a2 in
      let _a3 = strip_loc_type_parameters _a3 in `CeCon (_a1, _a2, _a3)
  | `CeFun (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_expr _a2 in `CeFun (_a1, _a2)
  | `CeLet (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_rec_flag _a1 in
      let _a2 = strip_loc_binding _a2 in
      let _a3 = strip_loc_class_expr _a3 in `CeLet (_a1, _a2, _a3)
  | `Obj (_a0,_a1) -> let _a1 = strip_loc_class_str_item _a1 in `Obj _a1
  | `ObjPat (_a0,_a1,_a2) ->
      let _a1 = strip_loc_patt _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `ObjPat (_a1, _a2)
  | `CeTyc (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_type _a2 in `CeTyc (_a1, _a2)
  | `And (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_expr _a2 in `And (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_expr _a1 in
      let _a2 = strip_loc_class_expr _a2 in `Eq (_a1, _a2)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result409)
and strip_loc_class_str_item =
  function
  | #nil as _a0 -> (strip_loc_nil _a0 :>'result408)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_class_str_item _a1 in
      let _a2 = strip_loc_class_str_item _a2 in `Sem (_a1, _a2)
  | `Eq (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ctyp _a1 in
      let _a2 = strip_loc_ctyp _a2 in `Eq (_a1, _a2)
  | `Inherit (_a0,_a1,_a2) ->
      let _a1 = strip_loc_override_flag _a1 in
      let _a2 = strip_loc_class_expr _a2 in `Inherit (_a1, _a2)
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_override_flag _a1 in
      let _a2 = strip_loc_class_expr _a2 in
      let _a3 = strip_loc_alident _a3 in `InheritAs (_a1, _a2, _a3)
  | `Initializer (_a0,_a1) ->
      let _a1 = strip_loc_expr _a1 in `Initializer _a1
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_private_flag _a3 in
      let _a4 = strip_loc_expr _a4 in
      let _a5 = strip_loc_ctyp _a5 in `CrMth (_a1, _a2, _a3, _a4, _a5)
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_override_flag _a2 in
      let _a3 = strip_loc_mutable_flag _a3 in
      let _a4 = strip_loc_expr _a4 in `CrVal (_a1, _a2, _a3, _a4)
  | `CrVir (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_private_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVir (_a1, _a2, _a3)
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      let _a1 = strip_loc_alident _a1 in
      let _a2 = strip_loc_mutable_flag _a2 in
      let _a3 = strip_loc_ctyp _a3 in `CrVvr (_a1, _a2, _a3)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result408)
let rec strip_loc_ep =
  function
  | #sid as _a0 -> (strip_loc_sid _a0 :>'result435)
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
  | #any as _a0 -> (strip_loc_any _a0 :>'result435)
  | `ArrayEmpty _a0 -> `ArrayEmpty
  | `Array (_a0,_a1) -> let _a1 = strip_loc_ep _a1 in `Array _a1
  | `Record (_a0,_a1) -> let _a1 = strip_loc_rec_bind _a1 in `Record _a1
  | #literal as _a0 -> (strip_loc_literal _a0 :>'result435)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result435)
and strip_loc_rec_bind =
  function
  | `RecBind (_a0,_a1,_a2) ->
      let _a1 = strip_loc_ident _a1 in
      let _a2 = strip_loc_ep _a2 in `RecBind (_a1, _a2)
  | `Sem (_a0,_a1,_a2) ->
      let _a1 = strip_loc_rec_bind _a1 in
      let _a2 = strip_loc_rec_bind _a2 in `Sem (_a1, _a2)
  | #any as _a0 -> (strip_loc_any _a0 :>'result434)
  | #ant as _a0 -> (strip_loc_ant _a0 :>'result434)
let pp_print_loc fmt _a0 = FanLoc.pp_print_t fmt _a0
let pp_print_ant fmt (`Ant (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc _a0
    FanUtil.pp_print_anti_cxt _a1
let pp_print_nil fmt (`Nil _a0) =
  Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc _a0
let pp_print_ant_nil fmt =
  function
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result439)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result439)
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result441)
let pp_print_direction_flag fmt =
  function
  | `To _a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" pp_print_loc _a0
  | `Downto _a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result442)
let pp_print_mutable_flag fmt =
  function
  | `Mutable _a0 ->
      Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" pp_print_loc _a0
  | `MuNil _a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result443)
let pp_print_private_flag fmt =
  function
  | `Private _a0 ->
      Format.fprintf fmt "@[<1>(`Private@ %a)@]" pp_print_loc _a0
  | `PrNil _a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result444)
let pp_print_virtual_flag fmt =
  function
  | `Virtual _a0 ->
      Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" pp_print_loc _a0
  | `ViNil _a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result445)
let pp_print_override_flag fmt =
  function
  | `Override _a0 ->
      Format.fprintf fmt "@[<1>(`Override@ %a)@]" pp_print_loc _a0
  | `OvNil _a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result446)
let pp_print_row_var_flag fmt =
  function
  | `RowVar _a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" pp_print_loc _a0
  | `RvNil _a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result447)
let pp_print_position_flag fmt =
  function
  | `Positive _a0 ->
      Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc _a0
  | `Negative _a0 ->
      Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc _a0
  | `Normal _a0 -> Format.fprintf fmt "@[<1>(`Normal@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result448)
let pp_print_meta_bool fmt =
  function
  | `True _a0 -> Format.fprintf fmt "@[<1>(`True@ %a)@]" pp_print_loc _a0
  | `False _a0 -> Format.fprintf fmt "@[<1>(`False@ %a)@]" pp_print_loc _a0
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result449)
let rec pp_print_strings fmt =
  function
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_strings _a1 pp_print_strings _a2
  | `Str (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result450)
let pp_print_alident fmt =
  function
  | `Lid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result451)
let pp_print_auident fmt =
  function
  | `Uid (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result452)
let pp_print_aident fmt =
  function
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result453)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result453)
let pp_print_astring fmt =
  function
  | `C (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result454)
let rec pp_print_uident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_uident _a1 pp_print_uident _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result455)
let rec pp_print_ident fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result456)
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result456)
let rec pp_print_dupath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_dupath _a2
  | #auident as _a0 -> (pp_print_auident fmt _a0 :>'result457)
let pp_print_dlpath fmt =
  function
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_dupath _a1 pp_print_alident _a2
  | #alident as _a0 -> (pp_print_alident fmt _a0 :>'result458)
let pp_print_sid fmt (`Id (_a0,_a1)) =
  Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc _a0 pp_print_ident
    _a1
let pp_print_any fmt (`Any _a0) =
  Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
let rec pp_print_ctyp fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result486)
  | `Alias (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_alident _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result486)
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
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result486)
  | `TyObj (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1 pp_print_row_var_flag _a2
  | `TyPol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
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
  | `PolyInfSup (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_row_field _a1 pp_print_tag_names _a2
  | `Package (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_type _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result486)
and pp_print_type_parameters fmt =
  function
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_type_parameters _a1 pp_print_type_parameters _a2
  | `Ctyp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Ctyp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result485)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result485)
and pp_print_row_field fmt =
  function
  | #ant_nil as _a0 -> (pp_print_ant_nil fmt _a0 :>'result484)
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
  | #ant_nil as _a0 -> (pp_print_ant_nil fmt _a0 :>'result483)
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
        _a0 pp_print_alident _a1 (pp_print_list pp_print_ctyp) _a2
        pp_print_type_info _a3
        (pp_print_list
           (fun fmt  (_a0,_a1)  ->
              Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_ctyp _a0
                pp_print_ctyp _a1)) _a4
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1 pp_print_typedecl _a2
  | #ant_nil as _a0 -> (pp_print_ant_nil fmt _a0 :>'result482)
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result481)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result481)
and pp_print_type_repr fmt =
  function
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_name_ctyp _a1
  | `Sum (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" pp_print_loc _a0
        pp_print_or_ctyp _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result480)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result480)
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result479)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result479)
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
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result478)
  | #ant_nil as _a0 -> (pp_print_ant_nil fmt _a0 :>'result478)
and pp_print_of_ctyp fmt =
  function
  | `Of (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sid _a1 pp_print_ctyp _a2
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result477)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result477)
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result477)
and pp_print_patt fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result476)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_patt _a2
  | `Vrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_patt _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_patt _a2
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result476)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_patt _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result476)
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result476)
  | `Alias (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_alident _a2
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1
  | `LabelS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Label (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_patt _a2
  | `OptLabl (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_patt _a2
  | `OptLablS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `OptLablExpr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_patt _a2 pp_print_expr _a3
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_patt _a2
  | `PaRng (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_patt _a2
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_ctyp _a2
  | `ClassPath (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Lazy (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1
  | `ModuleUnpack (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1
  | `ModuleConstraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
        pp_print_loc _a0 pp_print_auident _a1 pp_print_ctyp _a2
and pp_print_rec_patt fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_patt _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_patt _a1 pp_print_rec_patt _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result475)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result475)
and pp_print_expr fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result474)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `Vrn (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
        pp_print_string _a1
  | `Com (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `Tup (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result474)
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_expr _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result474)
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result474)
  | `RecordWith (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_expr _a1 pp_print_expr _a2
  | `Dot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `ArrayDot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `ExAsf _a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" pp_print_loc _a0
  | `ExAsr (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `Assign (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Assign@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
      Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_expr _a2 pp_print_expr _a3
        pp_print_direction_flag _a4 pp_print_expr _a5
  | `Fun (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" pp_print_loc _a0
        pp_print_match_case _a1
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_expr _a1 pp_print_expr _a2 pp_print_expr _a3
  | `IfThen (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `LabelS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`LabelS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Label (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_expr _a2
  | `Lazy (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `LetIn (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_flag _a1 pp_print_binding _a2 pp_print_expr _a3
  | `LetModule (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_auident _a1 pp_print_module_expr _a2 pp_print_expr _a3
  | `Match (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_match_case _a2
  | `New (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Obj (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_str_item _a1
  | `ObjPat (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_class_str_item _a2
  | `OptLabl (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_expr _a2
  | `OptLablS (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OptLablS@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `OvrInst (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_expr _a1
  | `OvrInstEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`OvrInstEmpty@ %a)@]" pp_print_loc _a0
  | `Seq (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `Send (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_alident _a2
  | `StringDot (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `Try (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_match_case _a2
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_ctyp _a2
  | `Coercion (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_expr _a1 pp_print_ctyp _a2 pp_print_ctyp _a3
  | `While (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1 pp_print_expr _a2
  | `LetOpen (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_expr _a2
  | `LocalTypeFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_expr _a2
  | `Package_expr (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Package_expr@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_expr _a1
and pp_print_rec_expr fmt =
  function
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_expr _a1 pp_print_rec_expr _a2
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_expr _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result473)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result473)
and pp_print_module_type fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result472)
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result472)
  | `MtFun (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`MtFun@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2 pp_print_module_type
        _a3
  | `Sig (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" pp_print_loc _a0
        pp_print_sig_item _a1
  | `With (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`With@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_type _a1 pp_print_with_constr _a2
  | `ModuleTypeOf (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_expr _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result472)
and pp_print_sig_item fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result471)
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
        pp_print_alident _a1 pp_print_expr _a2
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
  | `Open (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1
  | `Type (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc _a0
        pp_print_typedecl _a1
  | `Val (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_ctyp _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result471)
and pp_print_with_constr fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result470)
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result470)
and pp_print_binding fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result469)
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_binding _a1 pp_print_binding _a2
  | `Bind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_expr _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result469)
and pp_print_module_binding fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result468)
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_binding _a1 pp_print_module_binding _a2
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_auident _a1 pp_print_module_type _a2
        pp_print_module_expr _a3
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result468)
and pp_print_match_case fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result467)
  | `Or (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_match_case _a1 pp_print_match_case _a2
  | `Case (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_expr _a2
  | `CaseWhen (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_patt _a1 pp_print_expr _a2 pp_print_expr _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result467)
and pp_print_module_expr fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result466)
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result466)
  | `App (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_expr _a1 pp_print_module_expr _a2
  | `Functor (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_type _a2 pp_print_module_expr
        _a3
  | `Struct (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" pp_print_loc _a0
        pp_print_str_item _a1
  | `Constraint (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_expr _a1 pp_print_module_type _a2
  | `PackageModule (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result466)
and pp_print_str_item fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result465)
  | `Class (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_expr _a1
  | `ClassType (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_str_item _a1 pp_print_str_item _a2
  | `DirectiveSimple (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1
  | `Directive (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_expr _a2
  | `Exception (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc _a0
        pp_print_of_ctyp _a1
  | `StExp (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `External (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_ctyp _a2 pp_print_strings _a3
  | `Include (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc _a0
        pp_print_module_expr _a1
  | `Module (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_auident _a1 pp_print_module_expr _a2
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result465)
and pp_print_class_type fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result464)
  | `CtCon (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CtCon@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_virtual_flag _a1 pp_print_ident _a2 pp_print_type_parameters
        _a3
  | `CtFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_class_type _a2
  | `CtSig (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtSig@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_class_sig_item _a2
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | `CtCol (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | `CtEq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CtEq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_type _a1 pp_print_class_type _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result464)
and pp_print_class_sig_item fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result463)
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
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result463)
and pp_print_class_expr fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result462)
  | `CeApp (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_expr _a1 pp_print_expr _a2
  | `CeCon (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CeCon@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_virtual_flag _a1 pp_print_ident _a2 pp_print_type_parameters
        _a3
  | `CeFun (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_class_expr _a2
  | `CeLet (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CeLet@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_flag _a1 pp_print_binding _a2 pp_print_class_expr _a3
  | `Obj (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Obj@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_str_item _a1
  | `ObjPat (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_patt _a1 pp_print_class_str_item _a2
  | `CeTyc (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`CeTyc@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_expr _a1 pp_print_class_type _a2
  | `And (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_expr _a1 pp_print_class_expr _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_expr _a1 pp_print_class_expr _a2
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result462)
and pp_print_class_str_item fmt =
  function
  | #nil as _a0 -> (pp_print_nil fmt _a0 :>'result461)
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_class_str_item _a1 pp_print_class_str_item _a2
  | `Eq (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ctyp _a1 pp_print_ctyp _a2
  | `Inherit (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_override_flag _a1 pp_print_class_expr _a2
  | `InheritAs (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_override_flag _a1 pp_print_class_expr _a2
        pp_print_alident _a3
  | `Initializer (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" pp_print_loc _a0
        pp_print_expr _a1
  | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
      Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
        pp_print_loc _a0 pp_print_alident _a1 pp_print_override_flag _a2
        pp_print_private_flag _a3 pp_print_expr _a4 pp_print_ctyp _a5
  | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
      Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
        _a0 pp_print_alident _a1 pp_print_override_flag _a2
        pp_print_mutable_flag _a3 pp_print_expr _a4
  | `CrVir (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_private_flag _a2 pp_print_ctyp _a3
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_alident _a1 pp_print_mutable_flag _a2 pp_print_ctyp _a3
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result461)
let rec pp_print_ep fmt =
  function
  | #sid as _a0 -> (pp_print_sid fmt _a0 :>'result488)
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
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result488)
  | `ArrayEmpty _a0 ->
      Format.fprintf fmt "@[<1>(`ArrayEmpty@ %a)@]" pp_print_loc _a0
  | `Array (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ep _a1
  | `Record (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1
  | #literal as _a0 -> (pp_print_literal fmt _a0 :>'result488)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result488)
and pp_print_rec_bind fmt =
  function
  | `RecBind (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_ident _a1 pp_print_ep _a2
  | `Sem (_a0,_a1,_a2) ->
      Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc _a0
        pp_print_rec_bind _a1 pp_print_rec_bind _a2
  | #any as _a0 -> (pp_print_any fmt _a0 :>'result487)
  | #ant as _a0 -> (pp_print_ant fmt _a0 :>'result487)