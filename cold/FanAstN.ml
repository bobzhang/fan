open StdLib

include AstN

let _ = (); ()

let _ = ()

class eq =
  object (self : 'self_type)
    inherit  eqbase
    method loc : loc -> loc -> 'result0=
      fun _a0  _a1  -> self#fanloc_t _a0 _a1
    method ant : ant -> ant -> 'result1=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Ant (_a0,_a1),`Ant (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#fanutil_anti_cxt _a1 _b1)
    method nil : nil -> nil -> 'result2=
      fun _a0  _b0  -> match (_a0, _b0) with | (`Nil,`Nil) -> true
    method literal : literal -> literal -> 'result3=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Chr _a0,`Chr _b0) -> self#string _a0 _b0
        | (`Int _a0,`Int _b0) -> self#string _a0 _b0
        | (`Int32 _a0,`Int32 _b0) -> self#string _a0 _b0
        | (`Int64 _a0,`Int64 _b0) -> self#string _a0 _b0
        | (`Flo _a0,`Flo _b0) -> self#string _a0 _b0
        | (`Nativeint _a0,`Nativeint _b0) -> self#string _a0 _b0
        | (`Str _a0,`Str _b0) -> self#string _a0 _b0
        | (_,_) -> false
    method rec_flag : rec_flag -> rec_flag -> 'result4=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive,`Recursive) -> true
        | (`ReNil,`ReNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result4)
        | (_,_) -> false
    method direction_flag : direction_flag -> direction_flag -> 'result5=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To,`To) -> true
        | (`Downto,`Downto) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result5)
        | (_,_) -> false
    method mutable_flag : mutable_flag -> mutable_flag -> 'result6=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable,`Mutable) -> true
        | (`MuNil,`MuNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result6)
        | (_,_) -> false
    method private_flag : private_flag -> private_flag -> 'result7=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private,`Private) -> true
        | (`PrNil,`PrNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result7)
        | (_,_) -> false
    method virtual_flag : virtual_flag -> virtual_flag -> 'result8=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual,`Virtual) -> true
        | (`ViNil,`ViNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result8)
        | (_,_) -> false
    method override_flag : override_flag -> override_flag -> 'result9=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override,`Override) -> true
        | (`OvNil,`OvNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result9)
        | (_,_) -> false
    method row_var_flag : row_var_flag -> row_var_flag -> 'result10=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar,`RowVar) -> true
        | (`RvNil,`RvNil) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result10)
        | (_,_) -> false
    method position_flag : position_flag -> position_flag -> 'result11=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive,`Positive) -> true
        | (`Negative,`Negative) -> true
        | (`Normal,`Normal) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result11)
        | (_,_) -> false
    method strings : strings -> strings -> 'result12=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#strings _a0 _b0) && (self#strings _a1 _b1)
        | (`Str _a0,`Str _b0) -> self#string _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result12)
        | (_,_) -> false
    method alident : alident -> alident -> 'result13=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid _a0,`Lid _b0) -> self#string _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result13)
        | (_,_) -> false
    method auident : auident -> auident -> 'result14=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid _a0,`Uid _b0) -> self#string _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result14)
        | (_,_) -> false
    method aident : aident -> aident -> 'result15=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result15)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result15)
        | (_,_) -> false
    method astring : astring -> astring -> 'result16=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C _a0,`C _b0) -> self#string _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result16)
        | (_,_) -> false
    method uident : uident -> uident -> 'result17=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#uident _a0 _b0) && (self#uident _a1 _b1)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#uident _a0 _b0) && (self#uident _a1 _b1)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result17)
        | (_,_) -> false
    method ident : ident -> ident -> 'result18=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ident _a1 _b1)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ident _a1 _b1)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result18)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result18)
        | (_,_) -> false
    method vid : vid -> vid -> 'result19=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#vid _a0 _b0) && (self#vid _a1 _b1)
        | (`Lid _a0,`Lid _b0) -> self#string _a0 _b0
        | (`Uid _a0,`Uid _b0) -> self#string _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result19)
        | (_,_) -> false
    method dupath : dupath -> dupath -> 'result20=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#dupath _a0 _b0) && (self#dupath _a1 _b1)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result20)
        | (_,_) -> false
    method dlpath : dlpath -> dlpath -> 'result21=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#dupath _a0 _b0) && (self#alident _a1 _b1)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result21)
        | (_,_) -> false
    method any : any -> any -> 'result22=
      fun _a0  _b0  -> match (_a0, _b0) with | (`Any,`Any) -> true
    method sid : sid -> sid -> 'result23=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Id _a0,`Id _b0) -> self#ident _a0 _b0
    method ctyp : ctyp -> ctyp -> 'result24=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Alias (_a0,_a1),`Alias (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#alident _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result24)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Arrow (_a0,_a1),`Arrow (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ClassPath _a0,`ClassPath _b0) -> self#ident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#ctyp _a1 _b1)
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result24)
        | (`TyObj (_a0,_a1),`TyObj (_b0,_b1)) ->
            (self#name_ctyp _a0 _b0) && (self#row_var_flag _a1 _b1)
        | (`TyObjEnd _a0,`TyObjEnd _b0) -> self#row_var_flag _a0 _b0
        | (`TyPol (_a0,_a1),`TyPol (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`TyPolEnd _a0,`TyPolEnd _b0) -> self#ctyp _a0 _b0
        | (`TyTypePol (_a0,_a1),`TyTypePol (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Quote (_a0,_a1),`Quote (_b0,_b1)) ->
            (self#position_flag _a0 _b0) && (self#alident _a1 _b1)
        | (`QuoteAny _a0,`QuoteAny _b0) -> self#position_flag _a0 _b0
        | (`Par _a0,`Par _b0) -> self#ctyp _a0 _b0
        | (`Sta (_a0,_a1),`Sta (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`PolyEq _a0,`PolyEq _b0) -> self#row_field _a0 _b0
        | (`PolySup _a0,`PolySup _b0) -> self#row_field _a0 _b0
        | (`PolyInf _a0,`PolyInf _b0) -> self#row_field _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`PolyInfSup (_a0,_a1),`PolyInfSup (_b0,_b1)) ->
            (self#row_field _a0 _b0) && (self#tag_names _a1 _b1)
        | (`Package _a0,`Package _b0) -> self#module_type _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result24)
        | (_,_) -> false
    method type_parameters : type_parameters -> type_parameters -> 'result25=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#type_parameters _a0 _b0) && (self#type_parameters _a1 _b1)
        | (`Ctyp _a0,`Ctyp _b0) -> self#ctyp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result25)
        | (_,_) -> false
    method row_field : row_field -> row_field -> 'result26=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result26)
        | (`Bar (_a0,_a1),`Bar (_b0,_b1)) ->
            (self#row_field _a0 _b0) && (self#row_field _a1 _b1)
        | (`TyVrn _a0,`TyVrn _b0) -> self#astring _a0 _b0
        | (`TyVrnOf (_a0,_a1),`TyVrnOf (_b0,_b1)) ->
            (self#astring _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Ctyp _a0,`Ctyp _b0) -> self#ctyp _a0 _b0
        | (_,_) -> false
    method tag_names : tag_names -> tag_names -> 'result27=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result27)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#tag_names _a0 _b0) && (self#tag_names _a1 _b1)
        | (`TyVrn _a0,`TyVrn _b0) -> self#astring _a0 _b0
        | (_,_) -> false
    method typedecl : typedecl -> typedecl -> 'result28=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyDcl (_a0,_a1,_a2,_a3),`TyDcl (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#opt_decl_params _a1 _b1)) &&
               (self#type_info _a2 _b2))
              && (self#opt_type_constr _a3 _b3)
        | (`TyAbstr (_a0,_a1,_a2),`TyAbstr (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#opt_decl_params _a1 _b1)) &&
              (self#opt_type_constr _a2 _b2)
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#typedecl _a0 _b0) && (self#typedecl _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result28)
        | (_,_) -> false
    method type_constr : type_constr -> type_constr -> 'result29=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#type_constr _a0 _b0) && (self#type_constr _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result29)
        | (_,_) -> false
    method opt_type_constr : opt_type_constr -> opt_type_constr -> 'result30=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some _a0,`Some _b0) -> self#type_constr _a0 _b0
        | (`None,`None) -> true
        | (_,_) -> false
    method decl_param : decl_param -> decl_param -> 'result31=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1),`Quote (_b0,_b1)) ->
            (self#position_flag _a0 _b0) && (self#alident _a1 _b1)
        | (`QuoteAny _a0,`QuoteAny _b0) -> self#position_flag _a0 _b0
        | (`Any,`Any) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result31)
        | (_,_) -> false
    method decl_params : decl_params -> decl_params -> 'result32=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1),`Quote (_b0,_b1)) ->
            (self#position_flag _a0 _b0) && (self#alident _a1 _b1)
        | (`QuoteAny _a0,`QuoteAny _b0) -> self#position_flag _a0 _b0
        | (`Any,`Any) -> true
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#decl_params _a0 _b0) && (self#decl_params _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result32)
        | (_,_) -> false
    method opt_decl_params : opt_decl_params -> opt_decl_params -> 'result33=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some _a0,`Some _b0) -> self#decl_params _a0 _b0
        | (`None,`None) -> true
        | (_,_) -> false
    method type_info : type_info -> type_info -> 'result34=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyMan (_a0,_a1,_a2),`TyMan (_b0,_b1,_b2)) ->
            ((self#ctyp _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#type_repr _a2 _b2)
        | (`TyRepr (_a0,_a1),`TyRepr (_b0,_b1)) ->
            (self#private_flag _a0 _b0) && (self#type_repr _a1 _b1)
        | (`TyEq (_a0,_a1),`TyEq (_b0,_b1)) ->
            (self#private_flag _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result34)
        | (_,_) -> false
    method type_repr : type_repr -> type_repr -> 'result35=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record _a0,`Record _b0) -> self#name_ctyp _a0 _b0
        | (`Sum _a0,`Sum _b0) -> self#or_ctyp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result35)
        | (_,_) -> false
    method name_ctyp : name_ctyp -> name_ctyp -> 'result36=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#name_ctyp _a0 _b0) && (self#name_ctyp _a1 _b1)
        | (`TyCol (_a0,_a1),`TyCol (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | (`TyColMut (_a0,_a1),`TyColMut (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result36)
        | (_,_) -> false
    method or_ctyp : or_ctyp -> or_ctyp -> 'result37=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Bar (_a0,_a1),`Bar (_b0,_b1)) ->
            (self#or_ctyp _a0 _b0) && (self#or_ctyp _a1 _b1)
        | (`TyCol (_a0,_a1),`TyCol (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Of (_a0,_a1),`Of (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result37)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result37)
        | (_,_) -> false
    method of_ctyp : of_ctyp -> of_ctyp -> 'result38=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1),`Of (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result38)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result38)
        | (_,_) -> false
    method pat : pat -> pat -> 'result39=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#vid as _a0),(#vid as _b0)) -> (self#vid _a0 _b0 :>'result39)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#pat _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#pat _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#pat _a1 _b1)
        | (`Par _a0,`Par _b0) -> self#pat _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result39)
        | (`Record _a0,`Record _b0) -> self#rec_pat _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result39)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result39)
        | (`Alias (_a0,_a1),`Alias (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#alident _a1 _b1)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#pat _a0 _b0
        | (`LabelS _a0,`LabelS _b0) -> self#alident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#pat _a1 _b1)
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#pat _a1 _b1)
        | (`OptLablS _a0,`OptLablS _b0) -> self#alident _a0 _b0
        | (`OptLablExpr (_a0,_a1,_a2),`OptLablExpr (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#pat _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Bar (_a0,_a1),`Bar (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#pat _a1 _b1)
        | (`PaRng (_a0,_a1),`PaRng (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#pat _a1 _b1)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ClassPath _a0,`ClassPath _b0) -> self#ident _a0 _b0
        | (`Lazy _a0,`Lazy _b0) -> self#pat _a0 _b0
        | (`ModuleUnpack _a0,`ModuleUnpack _b0) -> self#auident _a0 _b0
        | (`ModuleConstraint (_a0,_a1),`ModuleConstraint (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#ctyp _a1 _b1)
        | (_,_) -> false
    method rec_pat : rec_pat -> rec_pat -> 'result40=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#pat _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_pat _a0 _b0) && (self#rec_pat _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result40)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result40)
        | (_,_) -> false
    method exp : exp -> exp -> 'result41=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#vid as _a0),(#vid as _b0)) -> (self#vid _a0 _b0 :>'result41)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`Par _a0,`Par _b0) -> self#exp _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result41)
        | (`Record _a0,`Record _b0) -> self#rec_exp _a0 _b0
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result41)
        | (`RecordWith (_a0,_a1),`RecordWith (_b0,_b1)) ->
            (self#rec_exp _a0 _b0) && (self#exp _a1 _b1)
        | (`Field (_a0,_a1),`Field (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`ArrayDot (_a0,_a1),`ArrayDot (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#exp _a0 _b0
        | (`Assert _a0,`Assert _b0) -> self#exp _a0 _b0
        | (`Assign (_a0,_a1),`Assign (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`For (_a0,_a1,_a2,_a3,_a4),`For (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#alident _a0 _b0) && (self#exp _a1 _b1)) &&
                (self#exp _a2 _b2))
               && (self#direction_flag _a3 _b3))
              && (self#exp _a4 _b4)
        | (`Fun _a0,`Fun _b0) -> self#case _a0 _b0
        | (`IfThenElse (_a0,_a1,_a2),`IfThenElse (_b0,_b1,_b2)) ->
            ((self#exp _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | (`IfThen (_a0,_a1),`IfThen (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`LabelS _a0,`LabelS _b0) -> self#alident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#exp _a1 _b1)
        | (`Lazy _a0,`Lazy _b0) -> self#exp _a0 _b0
        | (`LetIn (_a0,_a1,_a2),`LetIn (_b0,_b1,_b2)) ->
            ((self#rec_flag _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`LetTryInWith (_a0,_a1,_a2,_a3),`LetTryInWith (_b0,_b1,_b2,_b3))
            ->
            (((self#rec_flag _a0 _b0) && (self#binding _a1 _b1)) &&
               (self#exp _a2 _b2))
              && (self#case _a3 _b3)
        | (`LetModule (_a0,_a1,_a2),`LetModule (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_exp _a1 _b1)) &&
              (self#exp _a2 _b2)
        | (`Match (_a0,_a1),`Match (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#case _a1 _b1)
        | (`New _a0,`New _b0) -> self#ident _a0 _b0
        | (`Obj _a0,`Obj _b0) -> self#cstru _a0 _b0
        | (`ObjEnd,`ObjEnd) -> true
        | (`ObjPat (_a0,_a1),`ObjPat (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#cstru _a1 _b1)
        | (`ObjPatEnd _a0,`ObjPatEnd _b0) -> self#pat _a0 _b0
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#exp _a1 _b1)
        | (`OptLablS _a0,`OptLablS _b0) -> self#alident _a0 _b0
        | (`OvrInst _a0,`OvrInst _b0) -> self#rec_exp _a0 _b0
        | (`OvrInstEmpty,`OvrInstEmpty) -> true
        | (`Seq _a0,`Seq _b0) -> self#exp _a0 _b0
        | (`Send (_a0,_a1),`Send (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#alident _a1 _b1)
        | (`StringDot (_a0,_a1),`StringDot (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`Try (_a0,_a1),`Try (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#case _a1 _b1)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Coercion (_a0,_a1,_a2),`Coercion (_b0,_b1,_b2)) ->
            ((self#exp _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Subtype (_a0,_a1),`Subtype (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`While (_a0,_a1),`While (_b0,_b1)) ->
            (self#exp _a0 _b0) && (self#exp _a1 _b1)
        | (`LetOpen (_a0,_a1),`LetOpen (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#exp _a1 _b1)
        | (`LocalTypeFun (_a0,_a1),`LocalTypeFun (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#exp _a1 _b1)
        | (`Package_exp _a0,`Package_exp _b0) -> self#module_exp _a0 _b0
        | (_,_) -> false
    method rec_exp : rec_exp -> rec_exp -> 'result42=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_exp _a0 _b0) && (self#rec_exp _a1 _b1)
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#exp _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result42)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result42)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result43=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result43)
        | (`Functor (_a0,_a1,_a2),`Functor (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`Sig _a0,`Sig _b0) -> self#sig_item _a0 _b0
        | (`SigEnd,`SigEnd) -> true
        | (`With (_a0,_a1),`With (_b0,_b1)) ->
            (self#module_type _a0 _b0) && (self#with_constr _a1 _b1)
        | (`ModuleTypeOf _a0,`ModuleTypeOf _b0) -> self#module_exp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result43)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result44=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class _a0,`Class _b0) -> self#class_type _a0 _b0
        | (`ClassType _a0,`ClassType _b0) -> self#class_type _a0 _b0
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#sig_item _a0 _b0) && (self#sig_item _a1 _b1)
        | (`DirectiveSimple _a0,`DirectiveSimple _b0) -> self#alident _a0 _b0
        | (`Directive (_a0,_a1),`Directive (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#exp _a1 _b1)
        | (`Exception _a0,`Exception _b0) -> self#of_ctyp _a0 _b0
        | (`External (_a0,_a1,_a2),`External (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#strings _a2 _b2)
        | (`Include _a0,`Include _b0) -> self#module_type _a0 _b0
        | (`Module (_a0,_a1),`Module (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | (`RecModule _a0,`RecModule _b0) -> self#module_binding _a0 _b0
        | (`ModuleType (_a0,_a1),`ModuleType (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | (`ModuleTypeEnd _a0,`ModuleTypeEnd _b0) -> self#auident _a0 _b0
        | (`Open _a0,`Open _b0) -> self#ident _a0 _b0
        | (`Type _a0,`Type _b0) -> self#typedecl _a0 _b0
        | (`Val (_a0,_a1),`Val (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result44)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result45=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TypeEq (_a0,_a1),`TypeEq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`TypeEqPriv (_a0,_a1),`TypeEqPriv (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ModuleEq (_a0,_a1),`ModuleEq (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ident _a1 _b1)
        | (`TypeSubst (_a0,_a1),`TypeSubst (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ModuleSubst (_a0,_a1),`ModuleSubst (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ident _a1 _b1)
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#with_constr _a0 _b0) && (self#with_constr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result45)
        | (_,_) -> false
    method binding : binding -> binding -> 'result46=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#binding _a0 _b0) && (self#binding _a1 _b1)
        | (`Bind (_a0,_a1),`Bind (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#exp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result46)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result47=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#module_binding _a0 _b0) && (self#module_binding _a1 _b1)
        | (`ModuleBind (_a0,_a1,_a2),`ModuleBind (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_exp _a2 _b2)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result47)
        | (_,_) -> false
    method case : case -> case -> 'result48=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Bar (_a0,_a1),`Bar (_b0,_b1)) ->
            (self#case _a0 _b0) && (self#case _a1 _b1)
        | (`Case (_a0,_a1),`Case (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#exp _a1 _b1)
        | (`CaseWhen (_a0,_a1,_a2),`CaseWhen (_b0,_b1,_b2)) ->
            ((self#pat _a0 _b0) && (self#exp _a1 _b1)) && (self#exp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result48)
        | (_,_) -> false
    method module_exp : module_exp -> module_exp -> 'result49=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result49)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#module_exp _a0 _b0) && (self#module_exp _a1 _b1)
        | (`Functor (_a0,_a1,_a2),`Functor (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_exp _a2 _b2)
        | (`Struct _a0,`Struct _b0) -> self#stru _a0 _b0
        | (`StructEnd,`StructEnd) -> true
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#module_exp _a0 _b0) && (self#module_type _a1 _b1)
        | (`PackageModule _a0,`PackageModule _b0) -> self#exp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result49)
        | (_,_) -> false
    method stru : stru -> stru -> 'result50=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class _a0,`Class _b0) -> self#class_exp _a0 _b0
        | (`ClassType _a0,`ClassType _b0) -> self#class_type _a0 _b0
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#stru _a0 _b0) && (self#stru _a1 _b1)
        | (`DirectiveSimple _a0,`DirectiveSimple _b0) -> self#alident _a0 _b0
        | (`Directive (_a0,_a1),`Directive (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#exp _a1 _b1)
        | (`Exception _a0,`Exception _b0) -> self#of_ctyp _a0 _b0
        | (`StExp _a0,`StExp _b0) -> self#exp _a0 _b0
        | (`External (_a0,_a1,_a2),`External (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#strings _a2 _b2)
        | (`Include _a0,`Include _b0) -> self#module_exp _a0 _b0
        | (`Module (_a0,_a1),`Module (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_exp _a1 _b1)
        | (`RecModule _a0,`RecModule _b0) -> self#module_binding _a0 _b0
        | (`ModuleType (_a0,_a1),`ModuleType (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | (`Open _a0,`Open _b0) -> self#ident _a0 _b0
        | (`Type _a0,`Type _b0) -> self#typedecl _a0 _b0
        | (`Value (_a0,_a1),`Value (_b0,_b1)) ->
            (self#rec_flag _a0 _b0) && (self#binding _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result50)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result51=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`ClassCon (_a0,_a1,_a2),`ClassCon (_b0,_b1,_b2)) ->
            ((self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#type_parameters _a2 _b2)
        | (`ClassConS (_a0,_a1),`ClassConS (_b0,_b1)) ->
            (self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)
        | (`CtFun (_a0,_a1),`CtFun (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#class_type _a1 _b1)
        | (`ObjTy (_a0,_a1),`ObjTy (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#class_sig_item _a1 _b1)
        | (`ObjTyEnd _a0,`ObjTyEnd _b0) -> self#ctyp _a0 _b0
        | (`Obj _a0,`Obj _b0) -> self#class_sig_item _a0 _b0
        | (`ObjEnd,`ObjEnd) -> true
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#class_type _a0 _b0) && (self#class_type _a1 _b1)
        | (`CtCol (_a0,_a1),`CtCol (_b0,_b1)) ->
            (self#class_type _a0 _b0) && (self#class_type _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#class_type _a0 _b0) && (self#class_type _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result51)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result52=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#class_sig_item _a0 _b0) && (self#class_sig_item _a1 _b1)
        | (`SigInherit _a0,`SigInherit _b0) -> self#class_type _a0 _b0
        | (`Method (_a0,_a1,_a2),`Method (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`CgVal (_a0,_a1,_a2,_a3),`CgVal (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#mutable_flag _a1 _b1)) &&
               (self#virtual_flag _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`CgVir (_a0,_a1,_a2),`CgVir (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result52)
        | (_,_) -> false
    method class_exp : class_exp -> class_exp -> 'result53=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`CeApp (_a0,_a1),`CeApp (_b0,_b1)) ->
            (self#class_exp _a0 _b0) && (self#exp _a1 _b1)
        | (`ClassCon (_a0,_a1,_a2),`ClassCon (_b0,_b1,_b2)) ->
            ((self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#type_parameters _a2 _b2)
        | (`ClassConS (_a0,_a1),`ClassConS (_b0,_b1)) ->
            (self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)
        | (`CeFun (_a0,_a1),`CeFun (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#class_exp _a1 _b1)
        | (`LetIn (_a0,_a1,_a2),`LetIn (_b0,_b1,_b2)) ->
            ((self#rec_flag _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#class_exp _a2 _b2)
        | (`Obj _a0,`Obj _b0) -> self#cstru _a0 _b0
        | (`ObjEnd,`ObjEnd) -> true
        | (`ObjPat (_a0,_a1),`ObjPat (_b0,_b1)) ->
            (self#pat _a0 _b0) && (self#cstru _a1 _b1)
        | (`ObjPatEnd _a0,`ObjPatEnd _b0) -> self#pat _a0 _b0
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#class_exp _a0 _b0) && (self#class_type _a1 _b1)
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#class_exp _a0 _b0) && (self#class_exp _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#class_exp _a0 _b0) && (self#class_exp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result53)
        | (_,_) -> false
    method cstru : cstru -> cstru -> 'result54=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#cstru _a0 _b0) && (self#cstru _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Inherit (_a0,_a1),`Inherit (_b0,_b1)) ->
            (self#override_flag _a0 _b0) && (self#class_exp _a1 _b1)
        | (`InheritAs (_a0,_a1,_a2),`InheritAs (_b0,_b1,_b2)) ->
            ((self#override_flag _a0 _b0) && (self#class_exp _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`Initializer _a0,`Initializer _b0) -> self#exp _a0 _b0
        | (`CrMth (_a0,_a1,_a2,_a3,_a4),`CrMth (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
                (self#private_flag _a2 _b2))
               && (self#exp _a3 _b3))
              && (self#ctyp _a4 _b4)
        | (`CrMthS (_a0,_a1,_a2,_a3),`CrMthS (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#exp _a3 _b3)
        | (`CrVal (_a0,_a1,_a2,_a3),`CrVal (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#mutable_flag _a2 _b2))
              && (self#exp _a3 _b3)
        | (`CrVir (_a0,_a1,_a2),`CrVir (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`CrVvr (_a0,_a1,_a2),`CrVvr (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#mutable_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result54)
        | (_,_) -> false
    method ep : ep -> ep -> 'result55=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#vid as _a0),(#vid as _b0)) -> (self#vid _a0 _b0 :>'result55)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Par _a0,`Par _b0) -> self#ep _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result55)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#ep _a0 _b0
        | (`Record _a0,`Record _b0) -> self#rec_bind _a0 _b0
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result55)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result55)
        | (_,_) -> false
    method rec_bind : rec_bind -> rec_bind -> 'result56=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ep _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_bind _a0 _b0) && (self#rec_bind _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result56)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result56)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result57= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'result58= self#unknown
  end

class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> unit= fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> unit=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> unit=
      fun fmt  `Nil  -> Format.fprintf fmt "`Nil"
    method literal : 'fmt -> literal -> unit=
      fun fmt  ->
        function
        | `Chr _a0 -> Format.fprintf fmt "@[<1>(`Chr@ %a)@]" self#string _a0
        | `Int _a0 -> Format.fprintf fmt "@[<1>(`Int@ %a)@]" self#string _a0
        | `Int32 _a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]" self#string _a0
        | `Int64 _a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]" self#string _a0
        | `Flo _a0 -> Format.fprintf fmt "@[<1>(`Flo@ %a)@]" self#string _a0
        | `Nativeint _a0 ->
            Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]" self#string _a0
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
    method rec_flag : 'fmt -> rec_flag -> unit=
      fun fmt  ->
        function
        | `Recursive -> Format.fprintf fmt "`Recursive"
        | `ReNil -> Format.fprintf fmt "`ReNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method direction_flag : 'fmt -> direction_flag -> unit=
      fun fmt  ->
        function
        | `To -> Format.fprintf fmt "`To"
        | `Downto -> Format.fprintf fmt "`Downto"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method mutable_flag : 'fmt -> mutable_flag -> unit=
      fun fmt  ->
        function
        | `Mutable -> Format.fprintf fmt "`Mutable"
        | `MuNil -> Format.fprintf fmt "`MuNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method private_flag : 'fmt -> private_flag -> unit=
      fun fmt  ->
        function
        | `Private -> Format.fprintf fmt "`Private"
        | `PrNil -> Format.fprintf fmt "`PrNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method virtual_flag : 'fmt -> virtual_flag -> unit=
      fun fmt  ->
        function
        | `Virtual -> Format.fprintf fmt "`Virtual"
        | `ViNil -> Format.fprintf fmt "`ViNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method override_flag : 'fmt -> override_flag -> unit=
      fun fmt  ->
        function
        | `Override -> Format.fprintf fmt "`Override"
        | `OvNil -> Format.fprintf fmt "`OvNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method row_var_flag : 'fmt -> row_var_flag -> unit=
      fun fmt  ->
        function
        | `RowVar -> Format.fprintf fmt "`RowVar"
        | `RvNil -> Format.fprintf fmt "`RvNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method position_flag : 'fmt -> position_flag -> unit=
      fun fmt  ->
        function
        | `Positive -> Format.fprintf fmt "`Positive"
        | `Negative -> Format.fprintf fmt "`Negative"
        | `Normal -> Format.fprintf fmt "`Normal"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method strings : 'fmt -> strings -> unit=
      fun fmt  ->
        function
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#strings _a0
              self#strings _a1
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method alident : 'fmt -> alident -> unit=
      fun fmt  ->
        function
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method auident : 'fmt -> auident -> unit=
      fun fmt  ->
        function
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method aident : 'fmt -> aident -> unit=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method astring : 'fmt -> astring -> unit=
      fun fmt  ->
        function
        | `C _a0 -> Format.fprintf fmt "@[<1>(`C@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method uident : 'fmt -> uident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method ident : 'fmt -> ident -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method vid : 'fmt -> vid -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#vid _a0 self#vid
              _a1
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method dupath : 'fmt -> dupath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#dupath _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>unit)
    method dlpath : 'fmt -> dlpath -> unit=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#alident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>unit)
    method any : 'fmt -> any -> unit=
      fun fmt  `Any  -> Format.fprintf fmt "`Any"
    method sid : 'fmt -> sid -> unit=
      fun fmt  (`Id _a0)  ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]" self#ident _a0
    method ctyp : 'fmt -> ctyp -> unit=
      fun fmt  ->
        function
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#ctyp _a0
              self#alident _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `Arrow (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Arrow@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" self#ident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | #sid as _a0 -> (self#sid fmt _a0 :>unit)
        | `TyObj (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a)@]" self#name_ctyp _a0
              self#row_var_flag _a1
        | `TyObjEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyObjEnd@ %a)@]" self#row_var_flag _a0
        | `TyPol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `TyPolEnd _a0 ->
            Format.fprintf fmt "@[<1>(`TyPolEnd@ %a)@]" self#ctyp _a0
        | `TyTypePol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#ctyp _a0
        | `Sta (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sta@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `PolyEq _a0 ->
            Format.fprintf fmt "@[<1>(`PolyEq@ %a)@]" self#row_field _a0
        | `PolySup _a0 ->
            Format.fprintf fmt "@[<1>(`PolySup@ %a)@]" self#row_field _a0
        | `PolyInf _a0 ->
            Format.fprintf fmt "@[<1>(`PolyInf@ %a)@]" self#row_field _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `PolyInfSup (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PolyInfSup@ %a@ %a)@]" self#row_field
              _a0 self#tag_names _a1
        | `Package _a0 ->
            Format.fprintf fmt "@[<1>(`Package@ %a)@]" self#module_type _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_parameters : 'fmt -> type_parameters -> unit=
      fun fmt  ->
        function
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#type_parameters
              _a0 self#type_parameters _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method row_field : 'fmt -> row_field -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#row_field _a0
              self#row_field _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
        | `TyVrnOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a)@]" self#astring _a0
              self#ctyp _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
    method tag_names : 'fmt -> tag_names -> unit=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#tag_names _a0
              self#tag_names _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
    method typedecl : 'fmt -> typedecl -> unit=
      fun fmt  ->
        function
        | `TyDcl (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#opt_decl_params _a1 self#type_info _a2
              self#opt_type_constr _a3
        | `TyAbstr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyAbstr@ %a@ %a@ %a)@]" self#alident
              _a0 self#opt_decl_params _a1 self#opt_type_constr _a2
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#typedecl _a0
              self#typedecl _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_constr : 'fmt -> type_constr -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#type_constr _a0
              self#type_constr _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_type_constr : 'fmt -> opt_type_constr -> unit=
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" self#type_constr _a0
        | `None -> Format.fprintf fmt "`None"
    method decl_param : 'fmt -> decl_param -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Any -> Format.fprintf fmt "`Any"
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method decl_params : 'fmt -> decl_params -> unit=
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Any -> Format.fprintf fmt "`Any"
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#decl_params _a0
              self#decl_params _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method opt_decl_params : 'fmt -> opt_decl_params -> unit=
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" self#decl_params _a0
        | `None -> Format.fprintf fmt "`None"
    method type_info : 'fmt -> type_info -> unit=
      fun fmt  ->
        function
        | `TyMan (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" self#ctyp _a0
              self#private_flag _a1 self#type_repr _a2
        | `TyRepr (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyRepr@ %a@ %a)@]" self#private_flag
              _a0 self#type_repr _a1
        | `TyEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyEq@ %a@ %a)@]" self#private_flag _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method type_repr : 'fmt -> type_repr -> unit=
      fun fmt  ->
        function
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#name_ctyp _a0
        | `Sum _a0 -> Format.fprintf fmt "@[<1>(`Sum@ %a)@]" self#or_ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method name_ctyp : 'fmt -> name_ctyp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#name_ctyp _a0
              self#name_ctyp _a1
        | `TyCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" self#sid _a0
              self#ctyp _a1
        | `TyColMut (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyColMut@ %a@ %a)@]" self#sid _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method or_ctyp : 'fmt -> or_ctyp -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#or_ctyp _a0
              self#or_ctyp _a1
        | `TyCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" self#sid _a0
              self#ctyp _a1
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#sid _a0 self#ctyp
              _a1
        | #sid as _a0 -> (self#sid fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method of_ctyp : 'fmt -> of_ctyp -> unit=
      fun fmt  ->
        function
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#sid _a0 self#ctyp
              _a1
        | #sid as _a0 -> (self#sid fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method pat : 'fmt -> pat -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#pat _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_pat _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#pat _a0
              self#alident _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#pat _a0
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#pat _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#pat _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OptLablExpr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a)@]"
              self#alident _a0 self#pat _a1 self#exp _a2
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#pat _a0 self#pat
              _a1
        | `PaRng (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a)@]" self#pat _a0
              self#pat _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#pat _a0
              self#ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" self#ident _a0
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#pat _a0
        | `ModuleUnpack _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a)@]" self#auident _a0
        | `ModuleConstraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a)@]"
              self#auident _a0 self#ctyp _a1
    method rec_pat : 'fmt -> rec_pat -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#pat _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_pat _a0
              self#rec_pat _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method exp : 'fmt -> exp -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#exp _a0 self#exp
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#exp _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_exp _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | `RecordWith (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a)@]" self#rec_exp
              _a0 self#exp _a1
        | `Field (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Field@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `ArrayDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#exp _a0
        | `Assert _a0 ->
            Format.fprintf fmt "@[<1>(`Assert@ %a)@]" self#exp _a0
        | `Assign (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `For (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#exp _a1 self#exp _a2 self#direction_flag
              _a3 self#exp _a4
        | `Fun _a0 -> Format.fprintf fmt "@[<1>(`Fun@ %a)@]" self#case _a0
        | `IfThenElse (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a)@]" self#exp
              _a0 self#exp _a1 self#exp _a2
        | `IfThen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#exp _a0
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#rec_flag
              _a0 self#binding _a1 self#exp _a2
        | `LetTryInWith (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`LetTryInWith@ %a@ %a@ %a@ %a)@]"
              self#rec_flag _a0 self#binding _a1 self#exp _a2 self#case _a3
        | `LetModule (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a)@]" self#auident
              _a0 self#module_exp _a1 self#exp _a2
        | `Match (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a)@]" self#exp _a0
              self#case _a1
        | `New _a0 -> Format.fprintf fmt "@[<1>(`New@ %a)@]" self#ident _a0
        | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#cstru _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#pat _a0
              self#cstru _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#pat _a0
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OvrInst _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]" self#rec_exp _a0
        | `OvrInstEmpty -> Format.fprintf fmt "`OvrInstEmpty"
        | `Seq _a0 -> Format.fprintf fmt "@[<1>(`Seq@ %a)@]" self#exp _a0
        | `Send (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a)@]" self#exp _a0
              self#alident _a1
        | `StringDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `Try (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a)@]" self#exp _a0 self#case
              _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1
        | `Coercion (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1 self#ctyp _a2
        | `Subtype (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a)@]" self#exp _a0
              self#ctyp _a1
        | `While (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a)@]" self#exp _a0
              self#exp _a1
        | `LetOpen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a)@]" self#ident _a0
              self#exp _a1
        | `LocalTypeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a)@]" self#alident
              _a0 self#exp _a1
        | `Package_exp _a0 ->
            Format.fprintf fmt "@[<1>(`Package_exp@ %a)@]" self#module_exp
              _a0
    method rec_exp : 'fmt -> rec_exp -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_exp _a0
              self#rec_exp _a1
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#exp _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method module_type : 'fmt -> module_type -> unit=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>unit)
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1 self#module_type _a2
        | `Sig _a0 ->
            Format.fprintf fmt "@[<1>(`Sig@ %a)@]" self#sig_item _a0
        | `SigEnd -> Format.fprintf fmt "`SigEnd"
        | `With (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`With@ %a@ %a)@]" self#module_type _a0
              self#with_constr _a1
        | `ModuleTypeOf _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a)@]" self#module_exp
              _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method sig_item : 'fmt -> sig_item -> unit=
      fun fmt  ->
        function
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" self#class_type _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" self#class_type _a0
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#sig_item _a0
              self#sig_item _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" self#alident
              _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" self#of_ctyp _a0
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" self#alident
              _a0 self#ctyp _a1 self#strings _a2
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" self#module_type _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" self#auident _a0
              self#module_type _a1
        | `RecModule _a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" self#module_binding
              _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1
        | `ModuleTypeEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleTypeEnd@ %a)@]" self#auident _a0
        | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" self#ident _a0
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" self#typedecl _a0
        | `Val (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a)@]" self#alident _a0
              self#ctyp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method with_constr : 'fmt -> with_constr -> unit=
      fun fmt  ->
        function
        | `TypeEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `TypeEqPriv (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeEqPriv@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ModuleEq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `TypeSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a)@]" self#ctyp _a0
              self#ctyp _a1
        | `ModuleSubst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#with_constr _a0
              self#with_constr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method binding : 'fmt -> binding -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#binding _a0
              self#binding _a1
        | `Bind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a)@]" self#pat _a0 
              self#exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method module_binding : 'fmt -> module_binding -> unit=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#module_binding
              _a0 self#module_binding _a1
        | `ModuleBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a)@]"
              self#auident _a0 self#module_type _a1 self#module_exp _a2
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method case : 'fmt -> case -> unit=
      fun fmt  ->
        function
        | `Bar (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bar@ %a@ %a)@]" self#case _a0
              self#case _a1
        | `Case (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a)@]" self#pat _a0 
              self#exp _a1
        | `CaseWhen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a)@]" self#pat _a0
              self#exp _a1 self#exp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method module_exp : 'fmt -> module_exp -> unit=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#module_exp _a0
              self#module_exp _a1
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1 self#module_exp _a2
        | `Struct _a0 ->
            Format.fprintf fmt "@[<1>(`Struct@ %a)@]" self#stru _a0
        | `StructEnd -> Format.fprintf fmt "`StructEnd"
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#module_exp
              _a0 self#module_type _a1
        | `PackageModule _a0 ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]" self#exp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method stru : 'fmt -> stru -> unit=
      fun fmt  ->
        function
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" self#class_exp _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" self#class_type _a0
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#stru _a0
              self#stru _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" self#alident
              _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" self#alident _a0
              self#exp _a1
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" self#of_ctyp _a0
        | `StExp _a0 -> Format.fprintf fmt "@[<1>(`StExp@ %a)@]" self#exp _a0
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" self#alident
              _a0 self#ctyp _a1 self#strings _a2
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" self#module_exp _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" self#auident _a0
              self#module_exp _a1
        | `RecModule _a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]" self#module_binding
              _a0
        | `ModuleType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1
        | `Open _a0 -> Format.fprintf fmt "@[<1>(`Open@ %a)@]" self#ident _a0
        | `Type _a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]" self#typedecl _a0
        | `Value (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Value@ %a@ %a)@]" self#rec_flag _a0
              self#binding _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method class_type : 'fmt -> class_type -> unit=
      fun fmt  ->
        function
        | `ClassCon (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1 self#type_parameters _a2
        | `ClassConS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1
        | `CtFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a)@]" self#ctyp _a0
              self#class_type _a1
        | `ObjTy (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjTy@ %a@ %a)@]" self#ctyp _a0
              self#class_sig_item _a1
        | `ObjTyEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjTyEnd@ %a)@]" self#ctyp _a0
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#class_sig_item _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#class_type _a0
              self#class_type _a1
        | `CtCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a)@]" self#class_type _a0
              self#class_type _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#class_type _a0
              self#class_type _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method class_sig_item : 'fmt -> class_sig_item -> unit=
      fun fmt  ->
        function
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#class_sig_item
              _a0 self#class_sig_item _a1
        | `SigInherit _a0 ->
            Format.fprintf fmt "@[<1>(`SigInherit@ %a)@]" self#class_type _a0
        | `Method (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a)@]" self#alident
              _a0 self#private_flag _a1 self#ctyp _a2
        | `CgVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#mutable_flag _a1 self#virtual_flag _a2 self#ctyp _a3
        | `CgVir (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CgVir@ %a@ %a@ %a)@]" self#alident _a0
              self#private_flag _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method class_exp : 'fmt -> class_exp -> unit=
      fun fmt  ->
        function
        | `CeApp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a)@]" self#class_exp _a0
              self#exp _a1
        | `ClassCon (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1 self#type_parameters _a2
        | `ClassConS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1
        | `CeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a)@]" self#pat _a0
              self#class_exp _a1
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#rec_flag
              _a0 self#binding _a1 self#class_exp _a2
        | `Obj _a0 -> Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#cstru _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#pat _a0
              self#cstru _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#pat _a0
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#class_exp
              _a0 self#class_type _a1
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#class_exp _a0
              self#class_exp _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#class_exp _a0
              self#class_exp _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method cstru : 'fmt -> cstru -> unit=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#cstru _a0
              self#cstru _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | `Inherit (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" self#override_flag
              _a0 self#class_exp _a1
        | `InheritAs (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a)@]"
              self#override_flag _a0 self#class_exp _a1 self#alident _a2
        | `Initializer _a0 ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a)@]" self#exp _a0
        | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#override_flag _a1 self#private_flag _a2
              self#exp _a3 self#ctyp _a4
        | `CrMthS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#override_flag _a1 self#private_flag _a2
              self#exp _a3
        | `CrVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#override_flag _a1 self#mutable_flag _a2 self#exp _a3
        | `CrVir (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a)@]" self#alident _a0
              self#private_flag _a1 self#ctyp _a2
        | `CrVvr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a)@]" self#alident _a0
              self#mutable_flag _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method ep : 'fmt -> ep -> unit=
      fun fmt  ->
        function
        | #vid as _a0 -> (self#vid fmt _a0 :>unit)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#ep _a0 self#ep
              _a1
        | `Par _a0 -> Format.fprintf fmt "@[<1>(`Par@ %a)@]" self#ep _a0
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#ep _a0
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_bind _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method rec_bind : 'fmt -> rec_bind -> unit=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#ep _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_bind _a0
              self#rec_bind _a1
        | #any as _a0 -> (self#any fmt _a0 :>unit)
        | #ant as _a0 -> (self#ant fmt _a0 :>unit)
    method fanloc_t : 'fmt -> FanLoc.t -> unit= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> unit= self#unknown
  end

let _ = __MetaExpr__