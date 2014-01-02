open StdFan
open Astfn
class map =
  object (self : 'this_type__110_)
    inherit  mapbase
    method loc : loc -> loc= fun eta__001_  -> self#locf_t eta__001_
    method ant : ant -> ant=
      fun (`Ant (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#tokenf_ant _a1 in `Ant (_a0, _a1)
    method literal : literal -> literal=
      function
      | `Chr _a0 -> let _a0 = self#string _a0 in `Chr _a0
      | `Int _a0 -> let _a0 = self#string _a0 in `Int _a0
      | `Int32 _a0 -> let _a0 = self#string _a0 in `Int32 _a0
      | `Int64 _a0 -> let _a0 = self#string _a0 in `Int64 _a0
      | `Flo _a0 -> let _a0 = self#string _a0 in `Flo _a0
      | `Nativeint _a0 -> let _a0 = self#string _a0 in `Nativeint _a0
      | `Str _a0 -> let _a0 = self#string _a0 in `Str _a0
      | `Bool _a0 -> let _a0 = self#bool _a0 in `Bool _a0
      | `Unit -> `Unit
    method flag : flag -> flag=
      function
      | `Positive -> `Positive
      | `Negative -> `Negative
      | #ant as _a0 -> (self#ant _a0 : ant  :>flag)
    method position_flag : position_flag -> position_flag=
      function
      | `Positive -> `Positive
      | `Negative -> `Negative
      | `Normal -> `Normal
      | #ant as _a0 -> (self#ant _a0 : ant  :>position_flag)
    method strings : strings -> strings=
      function
      | `App (_a0,_a1) ->
          let _a0 = self#strings _a0 in
          let _a1 = self#strings _a1 in `App (_a0, _a1)
      | `Str _a0 -> let _a0 = self#string _a0 in `Str _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>strings)
    method lident : lident -> lident=
      fun (`Lid _a0)  -> let _a0 = self#string _a0 in `Lid _a0
    method alident : alident -> alident=
      function
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>alident)
    method auident : auident -> auident=
      function
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>auident)
    method aident : aident -> aident=
      function
      | #alident as _a0 -> (self#alident _a0 : alident  :>aident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>aident)
    method astring : astring -> astring=
      function
      | `C _a0 -> let _a0 = self#string _a0 in `C _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>astring)
    method uident : uident -> uident=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#uident _a0 in
          let _a1 = self#uident _a1 in `Dot (_a0, _a1)
      | `App (_a0,_a1) ->
          let _a0 = self#uident _a0 in
          let _a1 = self#uident _a1 in `App (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>uident)
    method ident : ident -> ident=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Dot (_a0, _a1)
      | `Apply (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Apply (_a0, _a1)
      | #alident as _a0 -> (self#alident _a0 : alident  :>ident)
      | #auident as _a0 -> (self#auident _a0 : auident  :>ident)
    method ident' : ident' -> ident'=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Dot (_a0, _a1)
      | `Apply (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `Apply (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
    method vid : vid -> vid=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#vid _a1 in `Dot (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>vid)
    method vid' : vid' -> vid'=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#vid _a1 in `Dot (_a0, _a1)
      | `Lid _a0 -> let _a0 = self#string _a0 in `Lid _a0
      | `Uid _a0 -> let _a0 = self#string _a0 in `Uid _a0
    method dupath : dupath -> dupath=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#dupath _a0 in
          let _a1 = self#dupath _a1 in `Dot (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>dupath)
    method dlpath : dlpath -> dlpath=
      function
      | `Dot (_a0,_a1) ->
          let _a0 = self#dupath _a0 in
          let _a1 = self#alident _a1 in `Dot (_a0, _a1)
      | #alident as _a0 -> (self#alident _a0 : alident  :>dlpath)
    method any : any -> any= fun `Any  -> `Any
    method ctyp : ctyp -> ctyp=
      function
      | `Alias (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#alident _a1 in `Alias (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>ctyp)
      | `App (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `App (_a0, _a1)
      | `Arrow (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Arrow (_a0, _a1)
      | `ClassPath _a0 -> let _a0 = self#ident _a0 in `ClassPath _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `Label (_a0, _a1)
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `OptLabl (_a0, _a1)
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>ctyp)
      | `TyObj (_a0,_a1) ->
          let _a0 = self#name_ctyp _a0 in
          let _a1 = self#flag _a1 in `TyObj (_a0, _a1)
      | `TyObjEnd _a0 -> let _a0 = self#flag _a0 in `TyObjEnd _a0
      | `TyPol (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TyPol (_a0, _a1)
      | `TyPolEnd _a0 -> let _a0 = self#ctyp _a0 in `TyPolEnd _a0
      | `TyTypePol (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TyTypePol (_a0, _a1)
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Par _a0 -> let _a0 = self#ctyp _a0 in `Par _a0
      | `Sta (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Sta (_a0, _a1)
      | `PolyEq _a0 -> let _a0 = self#row_field _a0 in `PolyEq _a0
      | `PolySup _a0 -> let _a0 = self#row_field _a0 in `PolySup _a0
      | `PolyInf _a0 -> let _a0 = self#row_field _a0 in `PolyInf _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Com (_a0, _a1)
      | `PolyInfSup (_a0,_a1) ->
          let _a0 = self#row_field _a0 in
          let _a1 = self#tag_names _a1 in `PolyInfSup (_a0, _a1)
      | `Package _a0 -> let _a0 = self#mtyp _a0 in `Package _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>ctyp)
    method type_parameters : type_parameters -> type_parameters=
      function
      | `Com (_a0,_a1) ->
          let _a0 = self#type_parameters _a0 in
          let _a1 = self#type_parameters _a1 in `Com (_a0, _a1)
      | `Ctyp _a0 -> let _a0 = self#ctyp _a0 in `Ctyp _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_parameters)
    method row_field : row_field -> row_field=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>row_field)
      | `Bar (_a0,_a1) ->
          let _a0 = self#row_field _a0 in
          let _a1 = self#row_field _a1 in `Bar (_a0, _a1)
      | `TyVrn _a0 -> let _a0 = self#astring _a0 in `TyVrn _a0
      | `TyVrnOf (_a0,_a1) ->
          let _a0 = self#astring _a0 in
          let _a1 = self#ctyp _a1 in `TyVrnOf (_a0, _a1)
      | `Ctyp _a0 -> let _a0 = self#ctyp _a0 in `Ctyp _a0
    method tag_names : tag_names -> tag_names=
      function
      | #ant as _a0 -> (self#ant _a0 : ant  :>tag_names)
      | `App (_a0,_a1) ->
          let _a0 = self#tag_names _a0 in
          let _a1 = self#tag_names _a1 in `App (_a0, _a1)
      | `TyVrn _a0 -> let _a0 = self#astring _a0 in `TyVrn _a0
    method decl : decl -> decl=
      function
      | `TyDcl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#opt_decl_params _a1 in
          let _a2 = self#type_info _a2 in
          let _a3 = self#opt_type_constr _a3 in `TyDcl (_a0, _a1, _a2, _a3)
      | `TyAbstr (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#opt_decl_params _a1 in
          let _a2 = self#opt_type_constr _a2 in `TyAbstr (_a0, _a1, _a2)
      | `And (_a0,_a1) ->
          let _a0 = self#decl _a0 in
          let _a1 = self#decl _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl)
    method type_constr : type_constr -> type_constr=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#type_constr _a0 in
          let _a1 = self#type_constr _a1 in `And (_a0, _a1)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_constr)
    method opt_type_constr : opt_type_constr -> opt_type_constr=
      function
      | `Some _a0 -> let _a0 = self#type_constr _a0 in `Some _a0
      | `None -> `None
    method decl_param : decl_param -> decl_param=
      function
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Any -> `Any
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_param)
    method decl_params : decl_params -> decl_params=
      function
      | `Quote (_a0,_a1) ->
          let _a0 = self#position_flag _a0 in
          let _a1 = self#alident _a1 in `Quote (_a0, _a1)
      | `QuoteAny _a0 -> let _a0 = self#position_flag _a0 in `QuoteAny _a0
      | `Any -> `Any
      | `Com (_a0,_a1) ->
          let _a0 = self#decl_params _a0 in
          let _a1 = self#decl_params _a1 in `Com (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>decl_params)
    method opt_decl_params : opt_decl_params -> opt_decl_params=
      function
      | `Some _a0 -> let _a0 = self#decl_params _a0 in `Some _a0
      | `None -> `None
    method type_info : type_info -> type_info=
      function
      | `TyMan (_a0,_a1,_a2) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#type_repr _a2 in `TyMan (_a0, _a1, _a2)
      | `TyRepr (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#type_repr _a1 in `TyRepr (_a0, _a1)
      | `TyEq (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ctyp _a1 in `TyEq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_info)
    method type_repr : type_repr -> type_repr=
      function
      | `Record _a0 -> let _a0 = self#name_ctyp _a0 in `Record _a0
      | `Sum _a0 -> let _a0 = self#or_ctyp _a0 in `Sum _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>type_repr)
    method name_ctyp : name_ctyp -> name_ctyp=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#name_ctyp _a0 in
          let _a1 = self#name_ctyp _a1 in `Sem (_a0, _a1)
      | `RecCol (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#flag _a2 in `RecCol (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>name_ctyp)
    method or_ctyp : or_ctyp -> or_ctyp=
      function
      | `Bar (_a0,_a1) ->
          let _a0 = self#or_ctyp _a0 in
          let _a1 = self#or_ctyp _a1 in `Bar (_a0, _a1)
      | `TyCol (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `TyCol (_a0, _a1)
      | `Of (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `Of (_a0, _a1)
      | #auident as _a0 -> (self#auident _a0 : auident  :>or_ctyp)
    method of_ctyp : of_ctyp -> of_ctyp=
      function
      | `Of (_a0,_a1) ->
          let _a0 = self#vid _a0 in let _a1 = self#ctyp _a1 in `Of (_a0, _a1)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>of_ctyp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>of_ctyp)
    method pat : pat -> pat=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>pat)
      | `App (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#pat _a0 in `Par _a0
      | #any as _a0 -> (self#any _a0 : any  :>pat)
      | `Record _a0 -> let _a0 = self#rec_pat _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>pat)
      | `Alias (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#alident _a1 in `Alias (_a0, _a1)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#pat _a0 in `Array _a0
      | `LabelS _a0 -> let _a0 = self#alident _a0 in `LabelS _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in `Label (_a0, _a1)
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in `OptLabl (_a0, _a1)
      | `OptLablS _a0 -> let _a0 = self#alident _a0 in `OptLablS _a0
      | `OptLablExpr (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#pat _a1 in
          let _a2 = self#exp _a2 in `OptLablExpr (_a0, _a1, _a2)
      | `Bar (_a0,_a1) ->
          let _a0 = self#pat _a0 in let _a1 = self#pat _a1 in `Bar (_a0, _a1)
      | `PaRng (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#pat _a1 in `PaRng (_a0, _a1)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#ctyp _a1 in `Constraint (_a0, _a1)
      | `ClassPath _a0 -> let _a0 = self#ident _a0 in `ClassPath _a0
      | `Lazy _a0 -> let _a0 = self#pat _a0 in `Lazy _a0
      | `ModuleUnpack _a0 -> let _a0 = self#auident _a0 in `ModuleUnpack _a0
      | `ModuleConstraint (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#ctyp _a1 in `ModuleConstraint (_a0, _a1)
    method rec_pat : rec_pat -> rec_pat=
      function
      | `RecBind (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#pat _a1 in `RecBind (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_pat _a0 in
          let _a1 = self#rec_pat _a1 in `Sem (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_pat)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_pat)
    method exp : exp -> exp=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>exp)
      | `App (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#exp _a0 in let _a1 = self#exp _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#exp _a0 in `Par _a0
      | #any as _a0 -> (self#any _a0 : any  :>exp)
      | `Record _a0 -> let _a0 = self#rec_exp _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>exp)
      | `RecordWith (_a0,_a1) ->
          let _a0 = self#rec_exp _a0 in
          let _a1 = self#exp _a1 in `RecordWith (_a0, _a1)
      | `Field (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#vid _a1 in `Field (_a0, _a1)
      | `ArrayDot (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `ArrayDot (_a0, _a1)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#exp _a0 in `Array _a0
      | `Assert _a0 -> let _a0 = self#exp _a0 in `Assert _a0
      | `Assign (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `Assign (_a0, _a1)
      | `For (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#flag _a3 in
          let _a4 = self#exp _a4 in `For (_a0, _a1, _a2, _a3, _a4)
      | `Fun _a0 -> let _a0 = self#case _a0 in `Fun _a0
      | `IfThenElse (_a0,_a1,_a2) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `IfThenElse (_a0, _a1, _a2)
      | `IfThen (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `IfThen (_a0, _a1)
      | `LabelS _a0 -> let _a0 = self#alident _a0 in `LabelS _a0
      | `Label (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Label (_a0, _a1)
      | `Lazy _a0 -> let _a0 = self#exp _a0 in `Lazy _a0
      | `LetIn (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#exp _a2 in `LetIn (_a0, _a1, _a2)
      | `LetTryInWith (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#exp _a2 in
          let _a3 = self#case _a3 in `LetTryInWith (_a0, _a1, _a2, _a3)
      | `LetModule (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mexp _a1 in
          let _a2 = self#exp _a2 in `LetModule (_a0, _a1, _a2)
      | `Match (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#case _a1 in `Match (_a0, _a1)
      | `New _a0 -> let _a0 = self#ident _a0 in `New _a0
      | `Obj _a0 -> let _a0 = self#clfield _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `ObjPat (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clfield _a1 in `ObjPat (_a0, _a1)
      | `ObjPatEnd _a0 -> let _a0 = self#pat _a0 in `ObjPatEnd _a0
      | `OptLabl (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `OptLabl (_a0, _a1)
      | `OptLablS _a0 -> let _a0 = self#alident _a0 in `OptLablS _a0
      | `OvrInst _a0 -> let _a0 = self#rec_exp _a0 in `OvrInst _a0
      | `OvrInstEmpty -> `OvrInstEmpty
      | `Seq _a0 -> let _a0 = self#exp _a0 in `Seq _a0
      | `Send (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#alident _a1 in `Send (_a0, _a1)
      | `StringDot (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `StringDot (_a0, _a1)
      | `Try (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#case _a1 in `Try (_a0, _a1)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in `Constraint (_a0, _a1)
      | `Coercion (_a0,_a1,_a2) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#ctyp _a2 in `Coercion (_a0, _a1, _a2)
      | `Subtype (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#ctyp _a1 in `Subtype (_a0, _a1)
      | `While (_a0,_a1) ->
          let _a0 = self#exp _a0 in
          let _a1 = self#exp _a1 in `While (_a0, _a1)
      | `LetOpen (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#exp _a2 in `LetOpen (_a0, _a1, _a2)
      | `LocalTypeFun (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `LocalTypeFun (_a0, _a1)
      | `Package_exp _a0 -> let _a0 = self#mexp _a0 in `Package_exp _a0
    method rec_exp : rec_exp -> rec_exp=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_exp _a0 in
          let _a1 = self#rec_exp _a1 in `Sem (_a0, _a1)
      | `RecBind (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#exp _a1 in `RecBind (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_exp)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_exp)
    method mtyp : mtyp -> mtyp=
      function
      | #ident' as _a0 -> (self#ident' _a0 : ident'  :>mtyp)
      | `Sig _a0 -> let _a0 = self#sigi _a0 in `Sig _a0
      | `SigEnd -> `SigEnd
      | `Functor (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mtyp _a2 in `Functor (_a0, _a1, _a2)
      | `With (_a0,_a1) ->
          let _a0 = self#mtyp _a0 in
          let _a1 = self#constr _a1 in `With (_a0, _a1)
      | `ModuleTypeOf _a0 -> let _a0 = self#mexp _a0 in `ModuleTypeOf _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>mtyp)
    method sigi : sigi -> sigi=
      function
      | `Val (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in `Val (_a0, _a1)
      | `External (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#strings _a2 in `External (_a0, _a1, _a2)
      | `Type _a0 -> let _a0 = self#decl _a0 in `Type _a0
      | `Exception _a0 -> let _a0 = self#of_ctyp _a0 in `Exception _a0
      | `Class _a0 -> let _a0 = self#cltdecl _a0 in `Class _a0
      | `ClassType _a0 -> let _a0 = self#cltdecl _a0 in `ClassType _a0
      | `Module (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `Module (_a0, _a1)
      | `ModuleTypeEnd _a0 ->
          let _a0 = self#auident _a0 in `ModuleTypeEnd _a0
      | `ModuleType (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `ModuleType (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#sigi _a0 in
          let _a1 = self#sigi _a1 in `Sem (_a0, _a1)
      | `DirectiveSimple _a0 ->
          let _a0 = self#alident _a0 in `DirectiveSimple _a0
      | `Directive (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Directive (_a0, _a1)
      | `Open (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
      | `Include _a0 -> let _a0 = self#mtyp _a0 in `Include _a0
      | `RecModule _a0 -> let _a0 = self#mbind _a0 in `RecModule _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>sigi)
    method mbind : mbind -> mbind=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#mbind _a0 in
          let _a1 = self#mbind _a1 in `And (_a0, _a1)
      | `ModuleBind (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mexp _a2 in `ModuleBind (_a0, _a1, _a2)
      | `Constraint (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `Constraint (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>mbind)
    method constr : constr -> constr=
      function
      | `TypeEq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeEq (_a0, _a1)
      | `ModuleEq (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `ModuleEq (_a0, _a1)
      | `TypeEqPriv (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeEqPriv (_a0, _a1)
      | `TypeSubst (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `TypeSubst (_a0, _a1)
      | `ModuleSubst (_a0,_a1) ->
          let _a0 = self#ident _a0 in
          let _a1 = self#ident _a1 in `ModuleSubst (_a0, _a1)
      | `And (_a0,_a1) ->
          let _a0 = self#constr _a0 in
          let _a1 = self#constr _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>constr)
    method bind : bind -> bind=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#bind _a0 in
          let _a1 = self#bind _a1 in `And (_a0, _a1)
      | `Bind (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in `Bind (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>bind)
    method case : case -> case=
      function
      | `Bar (_a0,_a1) ->
          let _a0 = self#case _a0 in
          let _a1 = self#case _a1 in `Bar (_a0, _a1)
      | `Case (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in `Case (_a0, _a1)
      | `CaseWhen (_a0,_a1,_a2) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#exp _a1 in
          let _a2 = self#exp _a2 in `CaseWhen (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>case)
    method mexp : mexp -> mexp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>mexp)
      | `App (_a0,_a1) ->
          let _a0 = self#mexp _a0 in
          let _a1 = self#mexp _a1 in `App (_a0, _a1)
      | `Functor (_a0,_a1,_a2) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in
          let _a2 = self#mexp _a2 in `Functor (_a0, _a1, _a2)
      | `Struct _a0 -> let _a0 = self#stru _a0 in `Struct _a0
      | `StructEnd -> `StructEnd
      | `Constraint (_a0,_a1) ->
          let _a0 = self#mexp _a0 in
          let _a1 = self#mtyp _a1 in `Constraint (_a0, _a1)
      | `PackageModule _a0 -> let _a0 = self#exp _a0 in `PackageModule _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>mexp)
    method stru : stru -> stru=
      function
      | `Class _a0 -> let _a0 = self#cldecl _a0 in `Class _a0
      | `ClassType _a0 -> let _a0 = self#cltdecl _a0 in `ClassType _a0
      | `Sem (_a0,_a1) ->
          let _a0 = self#stru _a0 in
          let _a1 = self#stru _a1 in `Sem (_a0, _a1)
      | `DirectiveSimple _a0 ->
          let _a0 = self#alident _a0 in `DirectiveSimple _a0
      | `Directive (_a0,_a1) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#exp _a1 in `Directive (_a0, _a1)
      | `Exception _a0 -> let _a0 = self#of_ctyp _a0 in `Exception _a0
      | `StExp _a0 -> let _a0 = self#exp _a0 in `StExp _a0
      | `External (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#ctyp _a1 in
          let _a2 = self#strings _a2 in `External (_a0, _a1, _a2)
      | `Include _a0 -> let _a0 = self#mexp _a0 in `Include _a0
      | `Module (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mexp _a1 in `Module (_a0, _a1)
      | `RecModule _a0 -> let _a0 = self#mbind _a0 in `RecModule _a0
      | `ModuleType (_a0,_a1) ->
          let _a0 = self#auident _a0 in
          let _a1 = self#mtyp _a1 in `ModuleType (_a0, _a1)
      | `Open (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in `Open (_a0, _a1)
      | `Type _a0 -> let _a0 = self#decl _a0 in `Type _a0
      | `TypeWith (_a0,_a1) ->
          let _a0 = self#decl _a0 in
          let _a1 = self#strings _a1 in `TypeWith (_a0, _a1)
      | `Value (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in `Value (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>stru)
    method cltdecl : cltdecl -> cltdecl=
      function
      | `And (_a0,_a1) ->
          let _a0 = self#cltdecl _a0 in
          let _a1 = self#cltdecl _a1 in `And (_a0, _a1)
      | `CtDecl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#type_parameters _a2 in
          let _a3 = self#cltyp _a3 in `CtDecl (_a0, _a1, _a2, _a3)
      | `CtDeclS (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#cltyp _a2 in `CtDeclS (_a0, _a1, _a2)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltdecl)
    method cltyp : cltyp -> cltyp=
      function
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>cltyp)
      | `ClApply (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#type_parameters _a1 in `ClApply (_a0, _a1)
      | `CtFun (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#cltyp _a1 in `CtFun (_a0, _a1)
      | `ObjTy (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#clsigi _a1 in `ObjTy (_a0, _a1)
      | `ObjTyEnd _a0 -> let _a0 = self#ctyp _a0 in `ObjTyEnd _a0
      | `Obj _a0 -> let _a0 = self#clsigi _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `And (_a0,_a1) ->
          let _a0 = self#cltyp _a0 in
          let _a1 = self#cltyp _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cltyp)
    method clsigi : clsigi -> clsigi=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#clsigi _a0 in
          let _a1 = self#clsigi _a1 in `Sem (_a0, _a1)
      | `SigInherit _a0 -> let _a0 = self#cltyp _a0 in `SigInherit _a0
      | `CgVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#ctyp _a3 in `CgVal (_a0, _a1, _a2, _a3)
      | `Method (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `Method (_a0, _a1, _a2)
      | `VirMeth (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirMeth (_a0, _a1, _a2)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clsigi)
    method cldecl : cldecl -> cldecl=
      function
      | `ClDecl (_a0,_a1,_a2,_a3) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#type_parameters _a2 in
          let _a3 = self#clexp _a3 in `ClDecl (_a0, _a1, _a2, _a3)
      | `ClDeclS (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#ident _a1 in
          let _a2 = self#clexp _a2 in `ClDeclS (_a0, _a1, _a2)
      | `And (_a0,_a1) ->
          let _a0 = self#cldecl _a0 in
          let _a1 = self#cldecl _a1 in `And (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>cldecl)
    method clexp : clexp -> clexp=
      function
      | `CeApp (_a0,_a1) ->
          let _a0 = self#clexp _a0 in
          let _a1 = self#exp _a1 in `CeApp (_a0, _a1)
      | #vid' as _a0 -> (self#vid' _a0 : vid'  :>clexp)
      | `ClApply (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#type_parameters _a1 in `ClApply (_a0, _a1)
      | `CeFun (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clexp _a1 in `CeFun (_a0, _a1)
      | `LetIn (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#bind _a1 in
          let _a2 = self#clexp _a2 in `LetIn (_a0, _a1, _a2)
      | `Obj _a0 -> let _a0 = self#clfield _a0 in `Obj _a0
      | `ObjEnd -> `ObjEnd
      | `ObjPat (_a0,_a1) ->
          let _a0 = self#pat _a0 in
          let _a1 = self#clfield _a1 in `ObjPat (_a0, _a1)
      | `ObjPatEnd _a0 -> let _a0 = self#pat _a0 in `ObjPatEnd _a0
      | `Constraint (_a0,_a1) ->
          let _a0 = self#clexp _a0 in
          let _a1 = self#cltyp _a1 in `Constraint (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>clexp)
    method clfield : clfield -> clfield=
      function
      | `Sem (_a0,_a1) ->
          let _a0 = self#clfield _a0 in
          let _a1 = self#clfield _a1 in `Sem (_a0, _a1)
      | `Inherit (_a0,_a1) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#clexp _a1 in `Inherit (_a0, _a1)
      | `InheritAs (_a0,_a1,_a2) ->
          let _a0 = self#flag _a0 in
          let _a1 = self#clexp _a1 in
          let _a2 = self#alident _a2 in `InheritAs (_a0, _a1, _a2)
      | `CrVal (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in `CrVal (_a0, _a1, _a2, _a3)
      | `VirVal (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirVal (_a0, _a1, _a2)
      | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in
          let _a4 = self#ctyp _a4 in `CrMth (_a0, _a1, _a2, _a3, _a4)
      | `CrMthS (_a0,_a1,_a2,_a3) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#flag _a2 in
          let _a3 = self#exp _a3 in `CrMthS (_a0, _a1, _a2, _a3)
      | `VirMeth (_a0,_a1,_a2) ->
          let _a0 = self#alident _a0 in
          let _a1 = self#flag _a1 in
          let _a2 = self#ctyp _a2 in `VirMeth (_a0, _a1, _a2)
      | `Eq (_a0,_a1) ->
          let _a0 = self#ctyp _a0 in
          let _a1 = self#ctyp _a1 in `Eq (_a0, _a1)
      | `Initializer _a0 -> let _a0 = self#exp _a0 in `Initializer _a0
      | #ant as _a0 -> (self#ant _a0 : ant  :>clfield)
    method ep : ep -> ep=
      function
      | #vid as _a0 -> (self#vid _a0 : vid  :>ep)
      | `App (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `App (_a0, _a1)
      | `Vrn _a0 -> let _a0 = self#string _a0 in `Vrn _a0
      | `Com (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `Com (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#ep _a0 in let _a1 = self#ep _a1 in `Sem (_a0, _a1)
      | `Par _a0 -> let _a0 = self#ep _a0 in `Par _a0
      | `Constraint (_a0,_a1) ->
          let _a0 = self#ep _a0 in
          let _a1 = self#ctyp _a1 in `Constraint (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>ep)
      | `ArrayEmpty -> `ArrayEmpty
      | `Array _a0 -> let _a0 = self#ep _a0 in `Array _a0
      | `Record _a0 -> let _a0 = self#rec_bind _a0 in `Record _a0
      | #literal as _a0 -> (self#literal _a0 : literal  :>ep)
    method rec_bind : rec_bind -> rec_bind=
      function
      | `RecBind (_a0,_a1) ->
          let _a0 = self#vid _a0 in
          let _a1 = self#ep _a1 in `RecBind (_a0, _a1)
      | `Sem (_a0,_a1) ->
          let _a0 = self#rec_bind _a0 in
          let _a1 = self#rec_bind _a1 in `Sem (_a0, _a1)
      | #any as _a0 -> (self#any _a0 : any  :>rec_bind)
      | #ant as _a0 -> (self#ant _a0 : ant  :>rec_bind)
    method tokenf_ant : Tokenf.ant -> Tokenf.ant= self#unknown
    method locf_t : Locf.t -> Locf.t= self#unknown
  end
let map_loc f =
  object (_this__108_ : 'this_type__109_)
    inherit  map as super
    method! loc x = f (super#loc x)
  end
let map_ant f =
  object (_this__106_ : 'this_type__107_)
    inherit  map as super
    method! ant x = f (super#ant x)
  end
let map_literal f =
  object (_this__104_ : 'this_type__105_)
    inherit  map as super
    method! literal x = f (super#literal x)
  end
let map_flag f =
  object (_this__102_ : 'this_type__103_)
    inherit  map as super
    method! flag x = f (super#flag x)
  end
let map_position_flag f =
  object (_this__100_ : 'this_type__101_)
    inherit  map as super
    method! position_flag x = f (super#position_flag x)
  end
let map_strings f =
  object (_this__098_ : 'this_type__099_)
    inherit  map as super
    method! strings x = f (super#strings x)
  end
let map_lident f =
  object (_this__096_ : 'this_type__097_)
    inherit  map as super
    method! lident x = f (super#lident x)
  end
let map_alident f =
  object (_this__094_ : 'this_type__095_)
    inherit  map as super
    method! alident x = f (super#alident x)
  end
let map_auident f =
  object (_this__092_ : 'this_type__093_)
    inherit  map as super
    method! auident x = f (super#auident x)
  end
let map_aident f =
  object (_this__090_ : 'this_type__091_)
    inherit  map as super
    method! aident x = f (super#aident x)
  end
let map_astring f =
  object (_this__088_ : 'this_type__089_)
    inherit  map as super
    method! astring x = f (super#astring x)
  end
let map_uident f =
  object (_this__086_ : 'this_type__087_)
    inherit  map as super
    method! uident x = f (super#uident x)
  end
let map_ident f =
  object (_this__084_ : 'this_type__085_)
    inherit  map as super
    method! ident x = f (super#ident x)
  end
let map_ident' f =
  object (_this__082_ : 'this_type__083_)
    inherit  map as super
    method! ident' x = f (super#ident' x)
  end
let map_vid f =
  object (_this__080_ : 'this_type__081_)
    inherit  map as super
    method! vid x = f (super#vid x)
  end
let map_vid' f =
  object (_this__078_ : 'this_type__079_)
    inherit  map as super
    method! vid' x = f (super#vid' x)
  end
let map_dupath f =
  object (_this__076_ : 'this_type__077_)
    inherit  map as super
    method! dupath x = f (super#dupath x)
  end
let map_dlpath f =
  object (_this__074_ : 'this_type__075_)
    inherit  map as super
    method! dlpath x = f (super#dlpath x)
  end
let map_any f =
  object (_this__072_ : 'this_type__073_)
    inherit  map as super
    method! any x = f (super#any x)
  end
let map_ctyp f =
  object (_this__070_ : 'this_type__071_)
    inherit  map as super
    method! ctyp x = f (super#ctyp x)
  end
let map_type_parameters f =
  object (_this__068_ : 'this_type__069_)
    inherit  map as super
    method! type_parameters x = f (super#type_parameters x)
  end
let map_row_field f =
  object (_this__066_ : 'this_type__067_)
    inherit  map as super
    method! row_field x = f (super#row_field x)
  end
let map_tag_names f =
  object (_this__064_ : 'this_type__065_)
    inherit  map as super
    method! tag_names x = f (super#tag_names x)
  end
let map_decl f =
  object (_this__062_ : 'this_type__063_)
    inherit  map as super
    method! decl x = f (super#decl x)
  end
let map_type_constr f =
  object (_this__060_ : 'this_type__061_)
    inherit  map as super
    method! type_constr x = f (super#type_constr x)
  end
let map_opt_type_constr f =
  object (_this__058_ : 'this_type__059_)
    inherit  map as super
    method! opt_type_constr x = f (super#opt_type_constr x)
  end
let map_decl_param f =
  object (_this__056_ : 'this_type__057_)
    inherit  map as super
    method! decl_param x = f (super#decl_param x)
  end
let map_decl_params f =
  object (_this__054_ : 'this_type__055_)
    inherit  map as super
    method! decl_params x = f (super#decl_params x)
  end
let map_opt_decl_params f =
  object (_this__052_ : 'this_type__053_)
    inherit  map as super
    method! opt_decl_params x = f (super#opt_decl_params x)
  end
let map_type_info f =
  object (_this__050_ : 'this_type__051_)
    inherit  map as super
    method! type_info x = f (super#type_info x)
  end
let map_type_repr f =
  object (_this__048_ : 'this_type__049_)
    inherit  map as super
    method! type_repr x = f (super#type_repr x)
  end
let map_name_ctyp f =
  object (_this__046_ : 'this_type__047_)
    inherit  map as super
    method! name_ctyp x = f (super#name_ctyp x)
  end
let map_or_ctyp f =
  object (_this__044_ : 'this_type__045_)
    inherit  map as super
    method! or_ctyp x = f (super#or_ctyp x)
  end
let map_of_ctyp f =
  object (_this__042_ : 'this_type__043_)
    inherit  map as super
    method! of_ctyp x = f (super#of_ctyp x)
  end
let map_pat f =
  object (_this__040_ : 'this_type__041_)
    inherit  map as super
    method! pat x = f (super#pat x)
  end
let map_rec_pat f =
  object (_this__038_ : 'this_type__039_)
    inherit  map as super
    method! rec_pat x = f (super#rec_pat x)
  end
let map_exp f =
  object (_this__036_ : 'this_type__037_)
    inherit  map as super
    method! exp x = f (super#exp x)
  end
let map_rec_exp f =
  object (_this__034_ : 'this_type__035_)
    inherit  map as super
    method! rec_exp x = f (super#rec_exp x)
  end
let map_mtyp f =
  object (_this__032_ : 'this_type__033_)
    inherit  map as super
    method! mtyp x = f (super#mtyp x)
  end
let map_sigi f =
  object (_this__030_ : 'this_type__031_)
    inherit  map as super
    method! sigi x = f (super#sigi x)
  end
let map_mbind f =
  object (_this__028_ : 'this_type__029_)
    inherit  map as super
    method! mbind x = f (super#mbind x)
  end
let map_constr f =
  object (_this__026_ : 'this_type__027_)
    inherit  map as super
    method! constr x = f (super#constr x)
  end
let map_bind f =
  object (_this__024_ : 'this_type__025_)
    inherit  map as super
    method! bind x = f (super#bind x)
  end
let map_case f =
  object (_this__022_ : 'this_type__023_)
    inherit  map as super
    method! case x = f (super#case x)
  end
let map_mexp f =
  object (_this__020_ : 'this_type__021_)
    inherit  map as super
    method! mexp x = f (super#mexp x)
  end
let map_stru f =
  object (_this__018_ : 'this_type__019_)
    inherit  map as super
    method! stru x = f (super#stru x)
  end
let map_cltdecl f =
  object (_this__016_ : 'this_type__017_)
    inherit  map as super
    method! cltdecl x = f (super#cltdecl x)
  end
let map_cltyp f =
  object (_this__014_ : 'this_type__015_)
    inherit  map as super
    method! cltyp x = f (super#cltyp x)
  end
let map_clsigi f =
  object (_this__012_ : 'this_type__013_)
    inherit  map as super
    method! clsigi x = f (super#clsigi x)
  end
let map_cldecl f =
  object (_this__010_ : 'this_type__011_)
    inherit  map as super
    method! cldecl x = f (super#cldecl x)
  end
let map_clexp f =
  object (_this__008_ : 'this_type__009_)
    inherit  map as super
    method! clexp x = f (super#clexp x)
  end
let map_clfield f =
  object (_this__006_ : 'this_type__007_)
    inherit  map as super
    method! clfield x = f (super#clfield x)
  end
let map_ep f =
  object (_this__004_ : 'this_type__005_)
    inherit  map as super
    method! ep x = f (super#ep x)
  end
let map_rec_bind f =
  object (_this__002_ : 'this_type__003_)
    inherit  map as super
    method! rec_bind x = f (super#rec_bind x)
  end
