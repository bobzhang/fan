open StdFan
open Astf
class map =
  object (self : 'this_type__125_)
    inherit  mapbase
    method loc : loc -> loc= fun eta__016_  -> self#locf_t eta__016_
    method ant : ant -> ant=
      fun (`Ant (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#tokenf_ant _a1 in `Ant (_a0, _a1)
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
      | `Nativeint (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Nativeint (_a0, _a1)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
      | `Bool (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#bool _a1 in `Bool (_a0, _a1)
      | `Unit _a0 -> let _a0 = self#loc _a0 in `Unit _a0
    method flag : flag -> flag=
      function
      | `Positive _a0 -> let _a0 = self#loc _a0 in `Positive _a0
      | `Negative _a0 -> let _a0 = self#loc _a0 in `Negative _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>flag)
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
    method lident : lident -> lident=
      fun (`Lid (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#string _a1 in `Lid (_a0, _a1)
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
      | `Apply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Apply (_a0, _a1, _a2)
      | #alident as _a0 -> (self#alident _a0 : alident  :>ident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>ident)
    method ident' : ident' -> ident'=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Dot (_a0, _a1, _a2)
      | `Apply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `Apply (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
    method vid : vid -> vid=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#vid _a2 in `Dot (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>vid)
    method vid' : vid' -> vid'=
      function
      | `Dot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#vid _a2 in `Dot (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | `Uid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Uid (_a0, _a1)
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
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>ctyp)
      | `TyObj (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#name_ctyp _a1 in
          let _a2 = self#flag _a2 in `TyObj (_a0, _a1, _a2)
      | `TyObjEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in `TyObjEnd (_a0, _a1)
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
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `Par (_a0, _a1)
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
          let _a1 = self#mtyp _a1 in `Package (_a0, _a1)
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
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#row_field _a1 in
          let _a2 = self#row_field _a2 in `Bar (_a0, _a1, _a2)
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
    method decl : decl -> decl=
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
          let _a1 = self#decl _a1 in
          let _a2 = self#decl _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl)
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
          let _a2 = self#flag _a2 in
          let _a3 = self#type_repr _a3 in `TyMan (_a0, _a1, _a2, _a3)
      | `TyRepr (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#type_repr _a2 in `TyRepr (_a0, _a1, _a2)
      | `TyEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
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
      | `RecCol (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#flag _a3 in `RecCol (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>name_ctyp)
    method or_ctyp : or_ctyp -> or_ctyp=
      function
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#or_ctyp _a1 in
          let _a2 = self#or_ctyp _a2 in `Bar (_a0, _a1, _a2)
      | `TyCol (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `TyCol (_a0, _a1, _a2)
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #auident as _a0 -> (self#auident _a0 : auident  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#ctyp _a2 in `Of (_a0, _a1, _a2)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
    method pat : pat -> pat=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>pat)
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
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#pat _a1 in `Par (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>pat)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in `Record (_a0, _a1)
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
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#pat _a2 in `Bar (_a0, _a1, _a2)
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
          let _a1 = self#vid _a1 in
          let _a2 = self#pat _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_pat _a1 in
          let _a2 = self#rec_pat _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_pat)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_pat)
    method exp : exp -> exp=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>exp)
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
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#exp _a1 in `Par (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>exp)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>exp)
      | `RecordWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#exp _a2 in `RecordWith (_a0, _a1, _a2)
      | `Field (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#vid _a2 in `Field (_a0, _a1, _a2)
      | `ArrayDot (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `ArrayDot (_a0, _a1, _a2)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Array (_a0, _a1)
      | `Assert (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Assert (_a0, _a1)
      | `Assign (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `Assign (_a0, _a1, _a2)
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#flag _a4 in
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
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#exp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#case _a4 in `LetTryInWith (_a0, _a1, _a2, _a3, _a4)
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mexp _a2 in
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
          let _a1 = self#clfield _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clfield _a2 in `ObjPat (_a0, _a1, _a2)
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
      | `LetOpen (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#exp _a3 in `LetOpen (_a0, _a1, _a2, _a3)
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `LocalTypeFun (_a0, _a1, _a2)
      | `Package_exp (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in `Package_exp (_a0, _a1)
    method rec_exp : rec_exp -> rec_exp=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_exp _a1 in
          let _a2 = self#rec_exp _a2 in `Sem (_a0, _a1, _a2)
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#exp _a2 in `RecBind (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_exp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_exp)
    method mtyp : mtyp -> mtyp=
      function
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>mtyp)
      | `Sig (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sigi _a1 in `Sig (_a0, _a1)
      | `SigEnd _a0 -> let _a0 = self#loc _a0 in `SigEnd _a0
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mtyp _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `With (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#constr _a2 in `With (_a0, _a1, _a2)
      | `ModuleTypeOf (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in `ModuleTypeOf (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mtyp)
    method sigi : sigi -> sigi=
      function
      | `Val (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in `Val (_a0, _a1, _a2)
      | `External (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#ctyp _a2 in
          let _a3 = self#strings _a3 in `External (_a0, _a1, _a2, _a3)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl _a1 in `Type (_a0, _a1)
      | `Exception (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#of_ctyp _a1 in `Exception (_a0, _a1)
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `ClassType (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `Module (_a0, _a1, _a2)
      | `ModuleTypeEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in `ModuleTypeEnd (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `ModuleType (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#sigi _a1 in
          let _a2 = self#sigi _a2 in `Sem (_a0, _a1, _a2)
      | `DirectiveSimple (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in `DirectiveSimple (_a0, _a1)
      | `Directive (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#exp _a2 in `Directive (_a0, _a1, _a2)
      | `Open (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in `Open (_a0, _a1, _a2)
      | `Include (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mtyp _a1 in `Include (_a0, _a1)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in `RecModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>sigi)
    method mbind : mbind -> mbind=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in
          let _a2 = self#mbind _a2 in `And (_a0, _a1, _a2)
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mexp _a3 in `ModuleBind (_a0, _a1, _a2, _a3)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mbind)
    method constr : constr -> constr=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEq (_a0, _a1, _a2)
      | `ModuleEq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#ident _a2 in `ModuleEq (_a0, _a1, _a2)
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `TypeEqPriv (_a0, _a1, _a2)
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
          let _a1 = self#constr _a1 in
          let _a2 = self#constr _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>constr)
    method bind : bind -> bind=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#bind _a2 in `And (_a0, _a1, _a2)
      | `Bind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `Bind (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>bind)
    method case : case -> case=
      function
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#case _a1 in
          let _a2 = self#case _a2 in `Bar (_a0, _a1, _a2)
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
    method mexp : mexp -> mexp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>mexp)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#mexp _a2 in `App (_a0, _a1, _a2)
      | `Functor (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in
          let _a3 = self#mexp _a3 in `Functor (_a0, _a1, _a2, _a3)
      | `Struct (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#stru _a1 in `Struct (_a0, _a1)
      | `StructEnd _a0 -> let _a0 = self#loc _a0 in `StructEnd _a0
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#mtyp _a2 in `Constraint (_a0, _a1, _a2)
      | `PackageModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `PackageModule (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mexp)
    method stru : stru -> stru=
      function
      | `Class (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cldecl _a1 in `Class (_a0, _a1)
      | `ClassType (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in `ClassType (_a0, _a1)
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
          let _a1 = self#mexp _a1 in `Include (_a0, _a1)
      | `Module (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mexp _a2 in `Module (_a0, _a1, _a2)
      | `RecModule (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#mbind _a1 in `RecModule (_a0, _a1)
      | `ModuleType (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#auident _a1 in
          let _a2 = self#mtyp _a2 in `ModuleType (_a0, _a1, _a2)
      | `Open (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in `Open (_a0, _a1, _a2)
      | `Type (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl _a1 in `Type (_a0, _a1)
      | `TypeWith (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#decl _a1 in
          let _a2 = self#strings _a2 in `TypeWith (_a0, _a1, _a2)
      | `Value (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in `Value (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>stru)
    method cltdecl : cltdecl -> cltdecl=
      function
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltdecl _a1 in
          let _a2 = self#cltdecl _a2 in `And (_a0, _a1, _a2)
      | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          let _a4 = self#cltyp _a4 in `CtDecl (_a0, _a1, _a2, _a3, _a4)
      | `CtDeclS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#cltyp _a3 in `CtDeclS (_a0, _a1, _a2, _a3)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltdecl)
    method cltyp : cltyp -> cltyp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>cltyp)
      | `ClApply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#type_parameters _a2 in `ClApply (_a0, _a1, _a2)
      | `CtFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#cltyp _a2 in `CtFun (_a0, _a1, _a2)
      | `ObjTy (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#clsigi _a2 in `ObjTy (_a0, _a1, _a2)
      | `ObjTyEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in `ObjTyEnd (_a0, _a1)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clsigi _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltyp _a1 in
          let _a2 = self#cltyp _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltyp)
    method clsigi : clsigi -> clsigi=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clsigi _a1 in
          let _a2 = self#clsigi _a2 in `Sem (_a0, _a1, _a2)
      | `SigInherit (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cltyp _a1 in `SigInherit (_a0, _a1)
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#ctyp _a4 in `CgVal (_a0, _a1, _a2, _a3, _a4)
      | `Method (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `Method (_a0, _a1, _a2, _a3)
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirMeth (_a0, _a1, _a2, _a3)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clsigi)
    method cldecl : cldecl -> cldecl=
      function
      | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#type_parameters _a3 in
          let _a4 = self#clexp _a4 in `ClDecl (_a0, _a1, _a2, _a3, _a4)
      | `ClDeclS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ident _a2 in
          let _a3 = self#clexp _a3 in `ClDeclS (_a0, _a1, _a2, _a3)
      | `And (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#cldecl _a1 in
          let _a2 = self#cldecl _a2 in `And (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cldecl)
    method clexp : clexp -> clexp=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#exp _a2 in `CeApp (_a0, _a1, _a2)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>clexp)
      | `ClApply (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#type_parameters _a2 in `ClApply (_a0, _a1, _a2)
      | `CeFun (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clexp _a2 in `CeFun (_a0, _a1, _a2)
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#bind _a2 in
          let _a3 = self#clexp _a3 in `LetIn (_a0, _a1, _a2, _a3)
      | `Obj (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clfield _a1 in `Obj (_a0, _a1)
      | `ObjEnd _a0 -> let _a0 = self#loc _a0 in `ObjEnd _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#clfield _a2 in `ObjPat (_a0, _a1, _a2)
      | `ObjPatEnd (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#pat _a1 in `ObjPatEnd (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#cltyp _a2 in `Constraint (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clexp)
    method clfield : clfield -> clfield=
      function
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#clfield _a1 in
          let _a2 = self#clfield _a2 in `Sem (_a0, _a1, _a2)
      | `Inherit (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#clexp _a2 in `Inherit (_a0, _a1, _a2)
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#clexp _a2 in
          let _a3 = self#alident _a3 in `InheritAs (_a0, _a1, _a2, _a3)
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `CrVal (_a0, _a1, _a2, _a3, _a4)
      | `VirVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirVal (_a0, _a1, _a2, _a3)
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in
          let _a5 = self#ctyp _a5 in `CrMth (_a0, _a1, _a2, _a3, _a4, _a5)
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `CrMthS (_a0, _a1, _a2, _a3, _a4)
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#alident _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `VirMeth (_a0, _a1, _a2, _a3)
      | `Eq (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Eq (_a0, _a1, _a2)
      | `Initializer (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#exp _a1 in `Initializer (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clfield)
    method ep : ep -> ep=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>ep)
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
      | `Par (_a0,_a1) ->
          let _a0 = self#loc _a0 in let _a1 = self#ep _a1 in `Par (_a0, _a1)
      | `Constraint (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in
          let _a2 = self#ctyp _a2 in `Constraint (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>ep)
      | `ArrayEmpty _a0 -> let _a0 = self#loc _a0 in `ArrayEmpty _a0
      | `Array (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#ep _a1 in `Array (_a0, _a1)
      | `Record (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in `Record (_a0, _a1)
      | #literal as _a0 -> (self#literal _a0 : literal  :>ep)
    method rec_bind : rec_bind -> rec_bind=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#vid _a1 in
          let _a2 = self#ep _a2 in `RecBind (_a0, _a1, _a2)
      | `Sem (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#rec_bind _a1 in
          let _a2 = self#rec_bind _a2 in `Sem (_a0, _a1, _a2)
      | #any as _a0 -> (self#any _a0 : any  :>rec_bind)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_bind)
    method tokenf_ant : Tokenf.ant -> Tokenf.ant= self#unknown
    method locf_t : Locf.t -> Locf.t= self#unknown
  end
let map_loc f =
  object (_this__123_ : 'this_type__124_)
    inherit  map as super
    method! loc x = f (super#loc x)
  end
let map_ant f =
  object (_this__121_ : 'this_type__122_)
    inherit  map as super
    method! ant x = f (super#ant x)
  end
let map_literal f =
  object (_this__119_ : 'this_type__120_)
    inherit  map as super
    method! literal x = f (super#literal x)
  end
let map_flag f =
  object (_this__117_ : 'this_type__118_)
    inherit  map as super
    method! flag x = f (super#flag x)
  end
let map_position_flag f =
  object (_this__115_ : 'this_type__116_)
    inherit  map as super
    method! position_flag x = f (super#position_flag x)
  end
let map_strings f =
  object (_this__113_ : 'this_type__114_)
    inherit  map as super
    method! strings x = f (super#strings x)
  end
let map_lident f =
  object (_this__111_ : 'this_type__112_)
    inherit  map as super
    method! lident x = f (super#lident x)
  end
let map_alident f =
  object (_this__109_ : 'this_type__110_)
    inherit  map as super
    method! alident x = f (super#alident x)
  end
let map_auident f =
  object (_this__107_ : 'this_type__108_)
    inherit  map as super
    method! auident x = f (super#auident x)
  end
let map_aident f =
  object (_this__105_ : 'this_type__106_)
    inherit  map as super
    method! aident x = f (super#aident x)
  end
let map_astring f =
  object (_this__103_ : 'this_type__104_)
    inherit  map as super
    method! astring x = f (super#astring x)
  end
let map_uident f =
  object (_this__101_ : 'this_type__102_)
    inherit  map as super
    method! uident x = f (super#uident x)
  end
let map_ident f =
  object (_this__099_ : 'this_type__100_)
    inherit  map as super
    method! ident x = f (super#ident x)
  end
let map_ident' f =
  object (_this__097_ : 'this_type__098_)
    inherit  map as super
    method! ident' x = f (super#ident' x)
  end
let map_vid f =
  object (_this__095_ : 'this_type__096_)
    inherit  map as super
    method! vid x = f (super#vid x)
  end
let map_vid' f =
  object (_this__093_ : 'this_type__094_)
    inherit  map as super
    method! vid' x = f (super#vid' x)
  end
let map_dupath f =
  object (_this__091_ : 'this_type__092_)
    inherit  map as super
    method! dupath x = f (super#dupath x)
  end
let map_dlpath f =
  object (_this__089_ : 'this_type__090_)
    inherit  map as super
    method! dlpath x = f (super#dlpath x)
  end
let map_any f =
  object (_this__087_ : 'this_type__088_)
    inherit  map as super
    method! any x = f (super#any x)
  end
let map_ctyp f =
  object (_this__085_ : 'this_type__086_)
    inherit  map as super
    method! ctyp x = f (super#ctyp x)
  end
let map_type_parameters f =
  object (_this__083_ : 'this_type__084_)
    inherit  map as super
    method! type_parameters x = f (super#type_parameters x)
  end
let map_row_field f =
  object (_this__081_ : 'this_type__082_)
    inherit  map as super
    method! row_field x = f (super#row_field x)
  end
let map_tag_names f =
  object (_this__079_ : 'this_type__080_)
    inherit  map as super
    method! tag_names x = f (super#tag_names x)
  end
let map_decl f =
  object (_this__077_ : 'this_type__078_)
    inherit  map as super
    method! decl x = f (super#decl x)
  end
let map_type_constr f =
  object (_this__075_ : 'this_type__076_)
    inherit  map as super
    method! type_constr x = f (super#type_constr x)
  end
let map_opt_type_constr f =
  object (_this__073_ : 'this_type__074_)
    inherit  map as super
    method! opt_type_constr x = f (super#opt_type_constr x)
  end
let map_decl_param f =
  object (_this__071_ : 'this_type__072_)
    inherit  map as super
    method! decl_param x = f (super#decl_param x)
  end
let map_decl_params f =
  object (_this__069_ : 'this_type__070_)
    inherit  map as super
    method! decl_params x = f (super#decl_params x)
  end
let map_opt_decl_params f =
  object (_this__067_ : 'this_type__068_)
    inherit  map as super
    method! opt_decl_params x = f (super#opt_decl_params x)
  end
let map_type_info f =
  object (_this__065_ : 'this_type__066_)
    inherit  map as super
    method! type_info x = f (super#type_info x)
  end
let map_type_repr f =
  object (_this__063_ : 'this_type__064_)
    inherit  map as super
    method! type_repr x = f (super#type_repr x)
  end
let map_name_ctyp f =
  object (_this__061_ : 'this_type__062_)
    inherit  map as super
    method! name_ctyp x = f (super#name_ctyp x)
  end
let map_or_ctyp f =
  object (_this__059_ : 'this_type__060_)
    inherit  map as super
    method! or_ctyp x = f (super#or_ctyp x)
  end
let map_of_ctyp f =
  object (_this__057_ : 'this_type__058_)
    inherit  map as super
    method! of_ctyp x = f (super#of_ctyp x)
  end
let map_pat f =
  object (_this__055_ : 'this_type__056_)
    inherit  map as super
    method! pat x = f (super#pat x)
  end
let map_rec_pat f =
  object (_this__053_ : 'this_type__054_)
    inherit  map as super
    method! rec_pat x = f (super#rec_pat x)
  end
let map_exp f =
  object (_this__051_ : 'this_type__052_)
    inherit  map as super
    method! exp x = f (super#exp x)
  end
let map_rec_exp f =
  object (_this__049_ : 'this_type__050_)
    inherit  map as super
    method! rec_exp x = f (super#rec_exp x)
  end
let map_mtyp f =
  object (_this__047_ : 'this_type__048_)
    inherit  map as super
    method! mtyp x = f (super#mtyp x)
  end
let map_sigi f =
  object (_this__045_ : 'this_type__046_)
    inherit  map as super
    method! sigi x = f (super#sigi x)
  end
let map_mbind f =
  object (_this__043_ : 'this_type__044_)
    inherit  map as super
    method! mbind x = f (super#mbind x)
  end
let map_constr f =
  object (_this__041_ : 'this_type__042_)
    inherit  map as super
    method! constr x = f (super#constr x)
  end
let map_bind f =
  object (_this__039_ : 'this_type__040_)
    inherit  map as super
    method! bind x = f (super#bind x)
  end
let map_case f =
  object (_this__037_ : 'this_type__038_)
    inherit  map as super
    method! case x = f (super#case x)
  end
let map_mexp f =
  object (_this__035_ : 'this_type__036_)
    inherit  map as super
    method! mexp x = f (super#mexp x)
  end
let map_stru f =
  object (_this__033_ : 'this_type__034_)
    inherit  map as super
    method! stru x = f (super#stru x)
  end
let map_cltdecl f =
  object (_this__031_ : 'this_type__032_)
    inherit  map as super
    method! cltdecl x = f (super#cltdecl x)
  end
let map_cltyp f =
  object (_this__029_ : 'this_type__030_)
    inherit  map as super
    method! cltyp x = f (super#cltyp x)
  end
let map_clsigi f =
  object (_this__027_ : 'this_type__028_)
    inherit  map as super
    method! clsigi x = f (super#clsigi x)
  end
let map_cldecl f =
  object (_this__025_ : 'this_type__026_)
    inherit  map as super
    method! cldecl x = f (super#cldecl x)
  end
let map_clexp f =
  object (_this__023_ : 'this_type__024_)
    inherit  map as super
    method! clexp x = f (super#clexp x)
  end
let map_clfield f =
  object (_this__021_ : 'this_type__022_)
    inherit  map as super
    method! clfield x = f (super#clfield x)
  end
let map_ep f =
  object (_this__019_ : 'this_type__020_)
    inherit  map as super
    method! ep x = f (super#ep x)
  end
let map_rec_bind f =
  object (_this__017_ : 'this_type__018_)
    inherit  map as super
    method! rec_bind x = f (super#rec_bind x)
  end
