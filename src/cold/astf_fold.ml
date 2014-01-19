open StdFan
open Astf
class fold =
  object (self : 'this_type__017_)
    inherit  foldbase
    method loc : loc -> 'this_type__017_=
      fun eta__016_  -> self#locf_t eta__016_
    method ant : ant -> 'this_type__017_=
      fun (`Ant (_a0,_a1))  -> let self = self#loc _a0 in self#tokenf_ant _a1
    method literal : literal -> 'this_type__017_=
      function
      | `Chr (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int32 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Int64 (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Flo (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Nativeint (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Bool (_a0,_a1) -> let self = self#loc _a0 in self#bool _a1
      | `Unit _a0 -> self#loc _a0
    method flag : flag -> 'this_type__017_=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method position_flag : position_flag -> 'this_type__017_=
      function
      | `Positive _a0 -> self#loc _a0
      | `Negative _a0 -> self#loc _a0
      | `Normal _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method strings : strings -> 'this_type__017_=
      function
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#strings _a1 in self#strings _a2
      | `Str (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method lident : lident -> 'this_type__017_=
      fun (`Lid (_a0,_a1))  -> let self = self#loc _a0 in self#string _a1
    method alident : alident -> 'this_type__017_=
      function
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method auident : auident -> 'this_type__017_=
      function
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method aident : aident -> 'this_type__017_=
      function
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method astring : astring -> 'this_type__017_=
      function
      | `C (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method uident : uident -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#uident _a1 in self#uident _a2
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method ident : ident -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Apply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method ident' : ident' -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Apply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method vid : vid -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#vid _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method vid' : vid' -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#vid _a2
      | `Lid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Uid (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
    method dupath : dupath -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#dupath _a2
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method dlpath : dlpath -> 'this_type__017_=
      function
      | `Dot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#dupath _a1 in self#alident _a2
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
    method any : any -> 'this_type__017_= fun (`Any _a0)  -> self#loc _a0
    method ctyp : ctyp -> 'this_type__017_=
      function
      | `Alias (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#alident _a2
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
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
      | #ident' as _a0 -> (self#ident' _a0 :>'this_type__017_)
      | `TyObj (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#flag _a2
      | `TyObjEnd (_a0,_a1) -> let self = self#loc _a0 in self#flag _a1
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
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
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
      | `Package (_a0,_a1) -> let self = self#loc _a0 in self#mtyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_parameters : type_parameters -> 'this_type__017_=
      function
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_parameters _a1 in self#type_parameters _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method row_field : row_field -> 'this_type__017_=
      function
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#row_field _a1 in self#row_field _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
      | `TyVrnOf (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#astring _a1 in self#ctyp _a2
      | `Ctyp (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
    method tag_names : tag_names -> 'this_type__017_=
      function
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#tag_names _a1 in self#tag_names _a2
      | `TyVrn (_a0,_a1) -> let self = self#loc _a0 in self#astring _a1
    method decl : decl -> 'this_type__017_=
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
          let self = self#decl _a1 in self#decl _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_constr : type_constr -> 'this_type__017_=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#type_constr _a1 in self#type_constr _a2
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method opt_type_constr : opt_type_constr -> 'this_type__017_=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#type_constr _a1
      | `None _a0 -> self#loc _a0
    method decl_param : decl_param -> 'this_type__017_=
      function
      | `Quote (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#position_flag _a1 in self#alident _a2
      | `QuoteAny (_a0,_a1) ->
          let self = self#loc _a0 in self#position_flag _a1
      | `Any _a0 -> self#loc _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method decl_params : decl_params -> 'this_type__017_=
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
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method opt_decl_params : opt_decl_params -> 'this_type__017_=
      function
      | `Some (_a0,_a1) -> let self = self#loc _a0 in self#decl_params _a1
      | `None _a0 -> self#loc _a0
    method type_info : type_info -> 'this_type__017_=
      function
      | `TyMan (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in
          let self = self#flag _a2 in self#type_repr _a3
      | `TyRepr (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#type_repr _a2
      | `TyEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_repr : type_repr -> 'this_type__017_=
      function
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#name_ctyp _a1
      | `Sum (_a0,_a1) -> let self = self#loc _a0 in self#or_ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method name_ctyp : name_ctyp -> 'this_type__017_=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#name_ctyp _a1 in self#name_ctyp _a2
      | `RecCol (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#flag _a3
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method or_ctyp : or_ctyp -> 'this_type__017_=
      function
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#or_ctyp _a1 in self#or_ctyp _a2
      | `TyCol (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#ctyp _a2
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method of_ctyp : of_ctyp -> 'this_type__017_=
      function
      | `Of (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#ctyp _a2
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method pat : pat -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#pat _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_pat _a1
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
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
      | `Bar (_a0,_a1,_a2) ->
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
    method rec_pat : rec_pat -> 'this_type__017_=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#pat _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_pat _a1 in self#rec_pat _a2
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method exp : exp -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_exp _a1
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
      | `RecordWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#exp _a2
      | `Field (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#vid _a2
      | `ArrayDot (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Assert (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | `Assign (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#exp _a2
      | `For (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#exp _a2 in
          let self = self#exp _a3 in let self = self#flag _a4 in self#exp _a5
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
          let self = self#flag _a1 in
          let self = self#bind _a2 in self#exp _a3
      | `LetTryInWith (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#bind _a2 in
          let self = self#exp _a3 in self#case _a4
      | `LetModule (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mexp _a2 in self#exp _a3
      | `Match (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#exp _a1 in self#case _a2
      | `New (_a0,_a1) -> let self = self#loc _a0 in self#ident _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clfield _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clfield _a2
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
      | `LetOpen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#exp _a3
      | `LocalTypeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Package_exp (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
    method rec_exp : rec_exp -> 'this_type__017_=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_exp _a1 in self#rec_exp _a2
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#exp _a2
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mtyp : mtyp -> 'this_type__017_=
      function
      | #ident' as _a0 -> (self#ident' _a0 :>'this_type__017_)
      | `Sig (_a0,_a1) -> let self = self#loc _a0 in self#sigi _a1
      | `SigEnd _a0 -> self#loc _a0
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mtyp _a3
      | `With (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mtyp _a1 in self#constr _a2
      | `ModuleTypeOf (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method sigi : sigi -> 'this_type__017_=
      function
      | `Val (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#ctyp _a2
      | `External (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#ctyp _a2 in self#strings _a3
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#decl _a1
      | `Exception (_a0,_a1) -> let self = self#loc _a0 in self#of_ctyp _a1
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
      | `ClassType (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `ModuleTypeEnd (_a0,_a1) ->
          let self = self#loc _a0 in self#auident _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#sigi _a1 in self#sigi _a2
      | `DirectiveSimple (_a0,_a1) ->
          let self = self#loc _a0 in self#alident _a1
      | `Directive (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in self#exp _a2
      | `Open (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ident _a2
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#mtyp _a1
      | `RecModule (_a0,_a1) -> let self = self#loc _a0 in self#mbind _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mbind : mbind -> 'this_type__017_=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mbind _a1 in self#mbind _a2
      | `ModuleBind (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mexp _a3
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method constr : constr -> 'this_type__017_=
      function
      | `TypeEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleEq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `TypeEqPriv (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `TypeSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `ModuleSubst (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ident _a1 in self#ident _a2
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#constr _a1 in self#constr _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method bind : bind -> 'this_type__017_=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#bind _a1 in self#bind _a2
      | `Bind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method case : case -> 'this_type__017_=
      function
      | `Bar (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#case _a1 in self#case _a2
      | `Case (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#pat _a1 in self#exp _a2
      | `CaseWhen (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in let self = self#exp _a2 in self#exp _a3
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mexp : mexp -> 'this_type__017_=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mexp _a1 in self#mexp _a2
      | `Functor (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in
          let self = self#mtyp _a2 in self#mexp _a3
      | `Struct (_a0,_a1) -> let self = self#loc _a0 in self#stru _a1
      | `StructEnd _a0 -> self#loc _a0
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#mexp _a1 in self#mtyp _a2
      | `PackageModule (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method stru : stru -> 'this_type__017_=
      function
      | `Class (_a0,_a1) -> let self = self#loc _a0 in self#cldecl _a1
      | `ClassType (_a0,_a1) -> let self = self#loc _a0 in self#cltdecl _a1
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
      | `Include (_a0,_a1) -> let self = self#loc _a0 in self#mexp _a1
      | `Module (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mexp _a2
      | `RecModule (_a0,_a1) -> let self = self#loc _a0 in self#mbind _a1
      | `ModuleType (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#auident _a1 in self#mtyp _a2
      | `Open (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#ident _a2
      | `Type (_a0,_a1) -> let self = self#loc _a0 in self#decl _a1
      | `TypeWith (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#decl _a1 in self#strings _a2
      | `Value (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#bind _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cltdecl : cltdecl -> 'this_type__017_=
      function
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cltdecl _a1 in self#cltdecl _a2
      | `CtDecl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in
          let self = self#type_parameters _a3 in self#cltyp _a4
      | `CtDeclS (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#cltyp _a3
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cltyp : cltyp -> 'this_type__017_=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `ClApply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#vid _a1 in self#type_parameters _a2
      | `CtFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#cltyp _a2
      | `ObjTy (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#clsigi _a2
      | `ObjTyEnd (_a0,_a1) -> let self = self#loc _a0 in self#ctyp _a1
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clsigi _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cltyp _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clsigi : clsigi -> 'this_type__017_=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clsigi _a1 in self#clsigi _a2
      | `SigInherit (_a0,_a1) -> let self = self#loc _a0 in self#cltyp _a1
      | `CgVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#ctyp _a4
      | `Method (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cldecl : cldecl -> 'this_type__017_=
      function
      | `ClDecl (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in
          let self = self#type_parameters _a3 in self#clexp _a4
      | `ClDeclS (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#ident _a2 in self#clexp _a3
      | `And (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#cldecl _a1 in self#cldecl _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clexp : clexp -> 'this_type__017_=
      function
      | `CeApp (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clexp _a1 in self#exp _a2
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `ClApply (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#vid _a1 in self#type_parameters _a2
      | `CeFun (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clexp _a2
      | `LetIn (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#bind _a2 in self#clexp _a3
      | `Obj (_a0,_a1) -> let self = self#loc _a0 in self#clfield _a1
      | `ObjEnd _a0 -> self#loc _a0
      | `ObjPat (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#pat _a1 in self#clfield _a2
      | `ObjPatEnd (_a0,_a1) -> let self = self#loc _a0 in self#pat _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clexp _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clfield : clfield -> 'this_type__017_=
      function
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#clfield _a1 in self#clfield _a2
      | `Inherit (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in self#clexp _a2
      | `InheritAs (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#flag _a1 in
          let self = self#clexp _a2 in self#alident _a3
      | `CrVal (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#exp _a4
      | `VirVal (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `CrMth (_a0,_a1,_a2,_a3,_a4,_a5) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in
          let self = self#exp _a4 in self#ctyp _a5
      | `CrMthS (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in
          let self = self#flag _a3 in self#exp _a4
      | `VirMeth (_a0,_a1,_a2,_a3) ->
          let self = self#loc _a0 in
          let self = self#alident _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Eq (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Initializer (_a0,_a1) -> let self = self#loc _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method ep : ep -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Vrn (_a0,_a1) -> let self = self#loc _a0 in self#string _a1
      | `Com (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ep _a2
      | `Par (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Constraint (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#ep _a1 in self#ctyp _a2
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `ArrayEmpty _a0 -> self#loc _a0
      | `Array (_a0,_a1) -> let self = self#loc _a0 in self#ep _a1
      | `Record (_a0,_a1) -> let self = self#loc _a0 in self#rec_bind _a1
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
    method rec_bind : rec_bind -> 'this_type__017_=
      function
      | `RecBind (_a0,_a1,_a2) ->
          let self = self#loc _a0 in let self = self#vid _a1 in self#ep _a2
      | `Sem (_a0,_a1,_a2) ->
          let self = self#loc _a0 in
          let self = self#rec_bind _a1 in self#rec_bind _a2
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method tokenf_ant : Tokenf.ant -> 'this_type__017_= self#unknown
    method locf_t : Locf.t -> 'this_type__017_= self#unknown
  end
