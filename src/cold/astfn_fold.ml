open StdFan
open Astfn
class fold =
  object (self : 'this_type__017_)
    inherit  foldbase
    method loc : loc -> 'this_type__017_=
      fun eta__016_  -> self#locf_t eta__016_
    method ant : ant -> 'this_type__017_=
      fun (`Ant (_a0,_a1))  -> let self = self#loc _a0 in self#tokenf_ant _a1
    method literal : literal -> 'this_type__017_=
      function
      | `Chr _a0 -> self#string _a0
      | `Int _a0 -> self#string _a0
      | `Int32 _a0 -> self#string _a0
      | `Int64 _a0 -> self#string _a0
      | `Flo _a0 -> self#string _a0
      | `Nativeint _a0 -> self#string _a0
      | `Str _a0 -> self#string _a0
      | `Bool _a0 -> self#bool _a0
      | `Unit -> self
    method flag : flag -> 'this_type__017_=
      function
      | `Positive -> self
      | `Negative -> self
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method position_flag : position_flag -> 'this_type__017_=
      function
      | `Positive -> self
      | `Negative -> self
      | `Normal -> self
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method strings : strings -> 'this_type__017_=
      function
      | `App (_a0,_a1) -> let self = self#strings _a0 in self#strings _a1
      | `Str _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method lident : lident -> 'this_type__017_=
      fun (`Lid _a0)  -> self#string _a0
    method alident : alident -> 'this_type__017_=
      function
      | `Lid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method auident : auident -> 'this_type__017_=
      function
      | `Uid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method aident : aident -> 'this_type__017_=
      function
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method astring : astring -> 'this_type__017_=
      function
      | `C _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method uident : uident -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#uident _a0 in self#uident _a1
      | `App (_a0,_a1) -> let self = self#uident _a0 in self#uident _a1
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method ident : ident -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Apply (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method ident' : ident' -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Apply (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
    method vid : vid -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#vid _a0 in self#vid _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method vid' : vid' -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#vid _a0 in self#vid _a1
      | `Lid _a0 -> self#string _a0
      | `Uid _a0 -> self#string _a0
    method dupath : dupath -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#dupath _a0 in self#dupath _a1
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method dlpath : dlpath -> 'this_type__017_=
      function
      | `Dot (_a0,_a1) -> let self = self#dupath _a0 in self#alident _a1
      | #alident as _a0 -> (self#alident _a0 :>'this_type__017_)
    method any : any -> 'this_type__017_= fun `Any  -> self
    method ctyp : ctyp -> 'this_type__017_=
      function
      | `Alias (_a0,_a1) -> let self = self#ctyp _a0 in self#alident _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Arrow (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ClassPath _a0 -> self#ident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | #ident' as _a0 -> (self#ident' _a0 :>'this_type__017_)
      | `TyObj (_a0,_a1) -> let self = self#name_ctyp _a0 in self#flag _a1
      | `TyObjEnd _a0 -> self#flag _a0
      | `TyPol (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `TyPolEnd _a0 -> self#ctyp _a0
      | `TyTypePol (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Par _a0 -> self#ctyp _a0
      | `Sta (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `PolyEq _a0 -> self#row_field _a0
      | `PolySup _a0 -> self#row_field _a0
      | `PolyInf _a0 -> self#row_field _a0
      | `Com (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `PolyInfSup (_a0,_a1) ->
          let self = self#row_field _a0 in self#tag_names _a1
      | `Package _a0 -> self#mtyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_parameters : type_parameters -> 'this_type__017_=
      function
      | `Com (_a0,_a1) ->
          let self = self#type_parameters _a0 in self#type_parameters _a1
      | `Ctyp _a0 -> self#ctyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method row_field : row_field -> 'this_type__017_=
      function
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
      | `Bar (_a0,_a1) -> let self = self#row_field _a0 in self#row_field _a1
      | `TyVrn _a0 -> self#astring _a0
      | `TyVrnOf (_a0,_a1) -> let self = self#astring _a0 in self#ctyp _a1
      | `Ctyp _a0 -> self#ctyp _a0
    method tag_names : tag_names -> 'this_type__017_=
      function
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#tag_names _a0 in self#tag_names _a1
      | `TyVrn _a0 -> self#astring _a0
    method decl : decl -> 'this_type__017_=
      function
      | `TyDcl (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#opt_decl_params _a1 in
          let self = self#type_info _a2 in self#opt_type_constr _a3
      | `TyAbstr (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#opt_decl_params _a1 in self#opt_type_constr _a2
      | `And (_a0,_a1) -> let self = self#decl _a0 in self#decl _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_constr : type_constr -> 'this_type__017_=
      function
      | `And (_a0,_a1) ->
          let self = self#type_constr _a0 in self#type_constr _a1
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method opt_type_constr : opt_type_constr -> 'this_type__017_=
      function | `Some _a0 -> self#type_constr _a0 | `None -> self
    method decl_param : decl_param -> 'this_type__017_=
      function
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Any -> self
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method decl_params : decl_params -> 'this_type__017_=
      function
      | `Quote (_a0,_a1) ->
          let self = self#position_flag _a0 in self#alident _a1
      | `QuoteAny _a0 -> self#position_flag _a0
      | `Any -> self
      | `Com (_a0,_a1) ->
          let self = self#decl_params _a0 in self#decl_params _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method opt_decl_params : opt_decl_params -> 'this_type__017_=
      function | `Some _a0 -> self#decl_params _a0 | `None -> self
    method type_info : type_info -> 'this_type__017_=
      function
      | `TyMan (_a0,_a1,_a2) ->
          let self = self#ctyp _a0 in
          let self = self#flag _a1 in self#type_repr _a2
      | `TyRepr (_a0,_a1) -> let self = self#flag _a0 in self#type_repr _a1
      | `TyEq (_a0,_a1) -> let self = self#flag _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method type_repr : type_repr -> 'this_type__017_=
      function
      | `Record _a0 -> self#name_ctyp _a0
      | `Sum _a0 -> self#or_ctyp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method name_ctyp : name_ctyp -> 'this_type__017_=
      function
      | `Sem (_a0,_a1) -> let self = self#name_ctyp _a0 in self#name_ctyp _a1
      | `RecCol (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#ctyp _a1 in self#flag _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method or_ctyp : or_ctyp -> 'this_type__017_=
      function
      | `Bar (_a0,_a1) -> let self = self#or_ctyp _a0 in self#or_ctyp _a1
      | `TyCol (_a0,_a1) -> let self = self#auident _a0 in self#ctyp _a1
      | `Of (_a0,_a1) -> let self = self#auident _a0 in self#ctyp _a1
      | #auident as _a0 -> (self#auident _a0 :>'this_type__017_)
    method of_ctyp : of_ctyp -> 'this_type__017_=
      function
      | `Of (_a0,_a1) -> let self = self#vid _a0 in self#ctyp _a1
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method pat : pat -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Sem (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Par _a0 -> self#pat _a0
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `Record _a0 -> self#rec_pat _a0
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
      | `Alias (_a0,_a1) -> let self = self#pat _a0 in self#alident _a1
      | `ArrayEmpty -> self
      | `Array _a0 -> self#pat _a0
      | `LabelS _a0 -> self#alident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#pat _a1
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#pat _a1
      | `OptLablS _a0 -> self#alident _a0
      | `OptLablExpr (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#pat _a1 in self#exp _a2
      | `Bar (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `PaRng (_a0,_a1) -> let self = self#pat _a0 in self#pat _a1
      | `Constraint (_a0,_a1) -> let self = self#pat _a0 in self#ctyp _a1
      | `ClassPath _a0 -> self#ident _a0
      | `Lazy _a0 -> self#pat _a0
      | `ModuleUnpack _a0 -> self#auident _a0
      | `ModuleConstraint (_a0,_a1) ->
          let self = self#auident _a0 in self#ctyp _a1
    method rec_pat : rec_pat -> 'this_type__017_=
      function
      | `RecBind (_a0,_a1) -> let self = self#vid _a0 in self#pat _a1
      | `Sem (_a0,_a1) -> let self = self#rec_pat _a0 in self#rec_pat _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method exp : exp -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Sem (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Par _a0 -> self#exp _a0
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `Record _a0 -> self#rec_exp _a0
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
      | `RecordWith (_a0,_a1) -> let self = self#rec_exp _a0 in self#exp _a1
      | `Field (_a0,_a1) -> let self = self#exp _a0 in self#vid _a1
      | `ArrayDot (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `ArrayEmpty -> self
      | `Array _a0 -> self#exp _a0
      | `Assert _a0 -> self#exp _a0
      | `Assign (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `For (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#alident _a0 in
          let self = self#exp _a1 in
          let self = self#exp _a2 in let self = self#flag _a3 in self#exp _a4
      | `Fun _a0 -> self#case _a0
      | `IfThenElse (_a0,_a1,_a2) ->
          let self = self#exp _a0 in let self = self#exp _a1 in self#exp _a2
      | `IfThen (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `LabelS _a0 -> self#alident _a0
      | `Label (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Lazy _a0 -> self#exp _a0
      | `LetIn (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in self#exp _a2
      | `LetTryInWith (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in
          let self = self#exp _a2 in self#case _a3
      | `LetModule (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mexp _a1 in self#exp _a2
      | `Match (_a0,_a1) -> let self = self#exp _a0 in self#case _a1
      | `New _a0 -> self#ident _a0
      | `Obj _a0 -> self#clfield _a0
      | `ObjEnd -> self
      | `ObjPat (_a0,_a1) -> let self = self#pat _a0 in self#clfield _a1
      | `ObjPatEnd _a0 -> self#pat _a0
      | `OptLabl (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `OptLablS _a0 -> self#alident _a0
      | `OvrInst _a0 -> self#rec_exp _a0
      | `OvrInstEmpty -> self
      | `Seq _a0 -> self#exp _a0
      | `Send (_a0,_a1) -> let self = self#exp _a0 in self#alident _a1
      | `StringDot (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `Try (_a0,_a1) -> let self = self#exp _a0 in self#case _a1
      | `Constraint (_a0,_a1) -> let self = self#exp _a0 in self#ctyp _a1
      | `Coercion (_a0,_a1,_a2) ->
          let self = self#exp _a0 in
          let self = self#ctyp _a1 in self#ctyp _a2
      | `Subtype (_a0,_a1) -> let self = self#exp _a0 in self#ctyp _a1
      | `While (_a0,_a1) -> let self = self#exp _a0 in self#exp _a1
      | `LetOpen (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in self#exp _a2
      | `LocalTypeFun (_a0,_a1) ->
          let self = self#alident _a0 in self#exp _a1
      | `Package_exp _a0 -> self#mexp _a0
    method rec_exp : rec_exp -> 'this_type__017_=
      function
      | `Sem (_a0,_a1) -> let self = self#rec_exp _a0 in self#rec_exp _a1
      | `RecBind (_a0,_a1) -> let self = self#vid _a0 in self#exp _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mtyp : mtyp -> 'this_type__017_=
      function
      | #ident' as _a0 -> (self#ident' _a0 :>'this_type__017_)
      | `Sig _a0 -> self#sigi _a0
      | `SigEnd -> self
      | `Functor (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mtyp _a2
      | `With (_a0,_a1) -> let self = self#mtyp _a0 in self#constr _a1
      | `ModuleTypeOf _a0 -> self#mexp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method sigi : sigi -> 'this_type__017_=
      function
      | `Val (_a0,_a1) -> let self = self#alident _a0 in self#ctyp _a1
      | `External (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#ctyp _a1 in self#strings _a2
      | `Type _a0 -> self#decl _a0
      | `Exception _a0 -> self#of_ctyp _a0
      | `Class _a0 -> self#cltdecl _a0
      | `ClassType _a0 -> self#cltdecl _a0
      | `Module (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `ModuleTypeEnd _a0 -> self#auident _a0
      | `ModuleType (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `Sem (_a0,_a1) -> let self = self#sigi _a0 in self#sigi _a1
      | `DirectiveSimple _a0 -> self#alident _a0
      | `Directive (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Open (_a0,_a1) -> let self = self#flag _a0 in self#ident _a1
      | `Include _a0 -> self#mtyp _a0
      | `RecModule _a0 -> self#mbind _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mbind : mbind -> 'this_type__017_=
      function
      | `And (_a0,_a1) -> let self = self#mbind _a0 in self#mbind _a1
      | `ModuleBind (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mexp _a2
      | `Constraint (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method constr : constr -> 'this_type__017_=
      function
      | `TypeEq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ModuleEq (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `TypeEqPriv (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `TypeSubst (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `ModuleSubst (_a0,_a1) -> let self = self#ident _a0 in self#ident _a1
      | `And (_a0,_a1) -> let self = self#constr _a0 in self#constr _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method bind : bind -> 'this_type__017_=
      function
      | `And (_a0,_a1) -> let self = self#bind _a0 in self#bind _a1
      | `Bind (_a0,_a1) -> let self = self#pat _a0 in self#exp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method case : case -> 'this_type__017_=
      function
      | `Bar (_a0,_a1) -> let self = self#case _a0 in self#case _a1
      | `Case (_a0,_a1) -> let self = self#pat _a0 in self#exp _a1
      | `CaseWhen (_a0,_a1,_a2) ->
          let self = self#pat _a0 in let self = self#exp _a1 in self#exp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method mexp : mexp -> 'this_type__017_=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#mexp _a0 in self#mexp _a1
      | `Functor (_a0,_a1,_a2) ->
          let self = self#auident _a0 in
          let self = self#mtyp _a1 in self#mexp _a2
      | `Struct _a0 -> self#stru _a0
      | `StructEnd -> self
      | `Constraint (_a0,_a1) -> let self = self#mexp _a0 in self#mtyp _a1
      | `PackageModule _a0 -> self#exp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method stru : stru -> 'this_type__017_=
      function
      | `Class _a0 -> self#cldecl _a0
      | `ClassType _a0 -> self#cltdecl _a0
      | `Sem (_a0,_a1) -> let self = self#stru _a0 in self#stru _a1
      | `DirectiveSimple _a0 -> self#alident _a0
      | `Directive (_a0,_a1) -> let self = self#alident _a0 in self#exp _a1
      | `Exception _a0 -> self#of_ctyp _a0
      | `StExp _a0 -> self#exp _a0
      | `External (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#ctyp _a1 in self#strings _a2
      | `Include _a0 -> self#mexp _a0
      | `Module (_a0,_a1) -> let self = self#auident _a0 in self#mexp _a1
      | `RecModule _a0 -> self#mbind _a0
      | `ModuleType (_a0,_a1) -> let self = self#auident _a0 in self#mtyp _a1
      | `Open (_a0,_a1) -> let self = self#flag _a0 in self#ident _a1
      | `Type _a0 -> self#decl _a0
      | `TypeWith (_a0,_a1) -> let self = self#decl _a0 in self#strings _a1
      | `Value (_a0,_a1) -> let self = self#flag _a0 in self#bind _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cltdecl : cltdecl -> 'this_type__017_=
      function
      | `And (_a0,_a1) -> let self = self#cltdecl _a0 in self#cltdecl _a1
      | `CtDecl (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in
          let self = self#type_parameters _a2 in self#cltyp _a3
      | `CtDeclS (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in self#cltyp _a2
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cltyp : cltyp -> 'this_type__017_=
      function
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `ClApply (_a0,_a1) ->
          let self = self#vid _a0 in self#type_parameters _a1
      | `CtFun (_a0,_a1) -> let self = self#ctyp _a0 in self#cltyp _a1
      | `ObjTy (_a0,_a1) -> let self = self#ctyp _a0 in self#clsigi _a1
      | `ObjTyEnd _a0 -> self#ctyp _a0
      | `Obj _a0 -> self#clsigi _a0
      | `ObjEnd -> self
      | `And (_a0,_a1) -> let self = self#cltyp _a0 in self#cltyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clsigi : clsigi -> 'this_type__017_=
      function
      | `Sem (_a0,_a1) -> let self = self#clsigi _a0 in self#clsigi _a1
      | `SigInherit _a0 -> self#cltyp _a0
      | `CgVal (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#ctyp _a3
      | `Method (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `VirMeth (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method cldecl : cldecl -> 'this_type__017_=
      function
      | `ClDecl (_a0,_a1,_a2,_a3) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in
          let self = self#type_parameters _a2 in self#clexp _a3
      | `ClDeclS (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#ident _a1 in self#clexp _a2
      | `And (_a0,_a1) -> let self = self#cldecl _a0 in self#cldecl _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clexp : clexp -> 'this_type__017_=
      function
      | `CeApp (_a0,_a1) -> let self = self#clexp _a0 in self#exp _a1
      | #vid' as _a0 -> (self#vid' _a0 :>'this_type__017_)
      | `ClApply (_a0,_a1) ->
          let self = self#vid _a0 in self#type_parameters _a1
      | `CeFun (_a0,_a1) -> let self = self#pat _a0 in self#clexp _a1
      | `LetIn (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#bind _a1 in self#clexp _a2
      | `Obj _a0 -> self#clfield _a0
      | `ObjEnd -> self
      | `ObjPat (_a0,_a1) -> let self = self#pat _a0 in self#clfield _a1
      | `ObjPatEnd _a0 -> self#pat _a0
      | `Constraint (_a0,_a1) -> let self = self#clexp _a0 in self#cltyp _a1
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method clfield : clfield -> 'this_type__017_=
      function
      | `Sem (_a0,_a1) -> let self = self#clfield _a0 in self#clfield _a1
      | `Inherit (_a0,_a1) -> let self = self#flag _a0 in self#clexp _a1
      | `InheritAs (_a0,_a1,_a2) ->
          let self = self#flag _a0 in
          let self = self#clexp _a1 in self#alident _a2
      | `CrVal (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#exp _a3
      | `VirVal (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in
          let self = self#exp _a3 in self#ctyp _a4
      | `CrMthS (_a0,_a1,_a2,_a3) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in
          let self = self#flag _a2 in self#exp _a3
      | `VirMeth (_a0,_a1,_a2) ->
          let self = self#alident _a0 in
          let self = self#flag _a1 in self#ctyp _a2
      | `Eq (_a0,_a1) -> let self = self#ctyp _a0 in self#ctyp _a1
      | `Initializer _a0 -> self#exp _a0
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method ep : ep -> 'this_type__017_=
      function
      | #vid as _a0 -> (self#vid _a0 :>'this_type__017_)
      | `App (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Vrn _a0 -> self#string _a0
      | `Com (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Sem (_a0,_a1) -> let self = self#ep _a0 in self#ep _a1
      | `Par _a0 -> self#ep _a0
      | `Constraint (_a0,_a1) -> let self = self#ep _a0 in self#ctyp _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | `ArrayEmpty -> self
      | `Array _a0 -> self#ep _a0
      | `Record _a0 -> self#rec_bind _a0
      | #literal as _a0 -> (self#literal _a0 :>'this_type__017_)
    method rec_bind : rec_bind -> 'this_type__017_=
      function
      | `RecBind (_a0,_a1) -> let self = self#vid _a0 in self#ep _a1
      | `Sem (_a0,_a1) -> let self = self#rec_bind _a0 in self#rec_bind _a1
      | #any as _a0 -> (self#any _a0 :>'this_type__017_)
      | #ant as _a0 -> (self#ant _a0 :>'this_type__017_)
    method tokenf_ant : Tokenf.ant -> 'this_type__017_= self#unknown
    method locf_t : Locf.t -> 'this_type__017_= self#unknown
  end
