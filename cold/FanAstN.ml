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
        | (`NativeInt _a0,`NativeInt _b0) -> self#string _a0 _b0
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
    method dupath : dupath -> dupath -> 'result19=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#dupath _a0 _b0) && (self#dupath _a1 _b1)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result19)
        | (_,_) -> false
    method dlpath : dlpath -> dlpath -> 'result20=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#dupath _a0 _b0) && (self#alident _a1 _b1)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result20)
        | (_,_) -> false
    method any : any -> any -> 'result21=
      fun _a0  _b0  -> match (_a0, _b0) with | (`Any,`Any) -> true
    method sid : sid -> sid -> 'result22=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Id _a0,`Id _b0) -> self#ident _a0 _b0
    method ctyp : ctyp -> ctyp -> 'result23=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Alias (_a0,_a1),`Alias (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#alident _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result23)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Arrow (_a0,_a1),`Arrow (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ClassPath _a0,`ClassPath _b0) -> self#ident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#ctyp _a1 _b1)
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result23)
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
        | (`Tup _a0,`Tup _b0) -> self#ctyp _a0 _b0
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result23)
        | (_,_) -> false
    method type_parameters : type_parameters -> type_parameters -> 'result24=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#type_parameters _a0 _b0) && (self#type_parameters _a1 _b1)
        | (`Ctyp _a0,`Ctyp _b0) -> self#ctyp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result24)
        | (_,_) -> false
    method row_field : row_field -> row_field -> 'result25=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result25)
        | (`Or (_a0,_a1),`Or (_b0,_b1)) ->
            (self#row_field _a0 _b0) && (self#row_field _a1 _b1)
        | (`TyVrn _a0,`TyVrn _b0) -> self#astring _a0 _b0
        | (`TyVrnOf (_a0,_a1),`TyVrnOf (_b0,_b1)) ->
            (self#astring _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Ctyp _a0,`Ctyp _b0) -> self#ctyp _a0 _b0
        | (_,_) -> false
    method tag_names : tag_names -> tag_names -> 'result26=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result26)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#tag_names _a0 _b0) && (self#tag_names _a1 _b1)
        | (`TyVrn _a0,`TyVrn _b0) -> self#astring _a0 _b0
        | (_,_) -> false
    method typedecl : typedecl -> typedecl -> 'result27=
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result27)
        | (_,_) -> false
    method type_constr : type_constr -> type_constr -> 'result28=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#type_constr _a0 _b0) && (self#type_constr _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result28)
        | (_,_) -> false
    method opt_type_constr : opt_type_constr -> opt_type_constr -> 'result29=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Constr _a0,`Constr _b0) -> self#type_constr _a0 _b0
        | (`Nil,`Nil) -> true
        | (_,_) -> false
    method decl_param : decl_param -> decl_param -> 'result30=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1),`Quote (_b0,_b1)) ->
            (self#position_flag _a0 _b0) && (self#alident _a1 _b1)
        | (`QuoteAny _a0,`QuoteAny _b0) -> self#position_flag _a0 _b0
        | (`Any,`Any) -> true
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result30)
        | (_,_) -> false
    method decl_params : decl_params -> decl_params -> 'result31=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Quote (_a0,_a1),`Quote (_b0,_b1)) ->
            (self#position_flag _a0 _b0) && (self#alident _a1 _b1)
        | (`QuoteAny _a0,`QuoteAny _b0) -> self#position_flag _a0 _b0
        | (`Any,`Any) -> true
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#decl_params _a0 _b0) && (self#decl_params _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result31)
        | (_,_) -> false
    method opt_decl_params : opt_decl_params -> opt_decl_params -> 'result32=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Some _a0,`Some _b0) -> self#decl_params _a0 _b0
        | (`None,`None) -> true
        | (_,_) -> false
    method type_info : type_info -> type_info -> 'result33=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`TyMan (_a0,_a1,_a2),`TyMan (_b0,_b1,_b2)) ->
            ((self#ctyp _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#type_repr _a2 _b2)
        | (`TyRepr (_a0,_a1),`TyRepr (_b0,_b1)) ->
            (self#private_flag _a0 _b0) && (self#type_repr _a1 _b1)
        | (`TyEq (_a0,_a1),`TyEq (_b0,_b1)) ->
            (self#private_flag _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result33)
        | (_,_) -> false
    method type_repr : type_repr -> type_repr -> 'result34=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record _a0,`Record _b0) -> self#name_ctyp _a0 _b0
        | (`Sum _a0,`Sum _b0) -> self#or_ctyp _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result34)
        | (_,_) -> false
    method name_ctyp : name_ctyp -> name_ctyp -> 'result35=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#name_ctyp _a0 _b0) && (self#name_ctyp _a1 _b1)
        | (`TyCol (_a0,_a1),`TyCol (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | (`TyColMut (_a0,_a1),`TyColMut (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result35)
        | (_,_) -> false
    method or_ctyp : or_ctyp -> or_ctyp -> 'result36=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1),`Or (_b0,_b1)) ->
            (self#or_ctyp _a0 _b0) && (self#or_ctyp _a1 _b1)
        | (`TyCol (_a0,_a1),`TyCol (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Of (_a0,_a1),`Of (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result36)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result36)
        | (_,_) -> false
    method of_ctyp : of_ctyp -> of_ctyp -> 'result37=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1),`Of (_b0,_b1)) ->
            (self#sid _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result37)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result37)
        | (_,_) -> false
    method patt : patt -> patt -> 'result38=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result38)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#patt _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#patt _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#patt _a1 _b1)
        | (`Tup _a0,`Tup _b0) -> self#patt _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result38)
        | (`Record _a0,`Record _b0) -> self#rec_patt _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result38)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result38)
        | (`Alias (_a0,_a1),`Alias (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#alident _a1 _b1)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#patt _a0 _b0
        | (`LabelS _a0,`LabelS _b0) -> self#alident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#patt _a1 _b1)
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#patt _a1 _b1)
        | (`OptLablS _a0,`OptLablS _b0) -> self#alident _a0 _b0
        | (`OptLablExpr (_a0,_a1,_a2),`OptLablExpr (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Or (_a0,_a1),`Or (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#patt _a1 _b1)
        | (`PaRng (_a0,_a1),`PaRng (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#patt _a1 _b1)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#ctyp _a1 _b1)
        | (`ClassPath _a0,`ClassPath _b0) -> self#ident _a0 _b0
        | (`Lazy _a0,`Lazy _b0) -> self#patt _a0 _b0
        | (`ModuleUnpack _a0,`ModuleUnpack _b0) -> self#auident _a0 _b0
        | (`ModuleConstraint (_a0,_a1),`ModuleConstraint (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#ctyp _a1 _b1)
        | (_,_) -> false
    method rec_patt : rec_patt -> rec_patt -> 'result39=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#patt _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_patt _a0 _b0) && (self#rec_patt _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result39)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result39)
        | (_,_) -> false
    method expr : expr -> expr -> 'result40=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result40)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`Tup _a0,`Tup _b0) -> self#expr _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result40)
        | (`Record _a0,`Record _b0) -> self#rec_expr _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result40)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result40)
        | (`RecordWith (_a0,_a1),`RecordWith (_b0,_b1)) ->
            (self#rec_expr _a0 _b0) && (self#expr _a1 _b1)
        | (`Dot (_a0,_a1),`Dot (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`ArrayDot (_a0,_a1),`ArrayDot (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#expr _a0 _b0
        | (`ExAsf,`ExAsf) -> true
        | (`ExAsr _a0,`ExAsr _b0) -> self#expr _a0 _b0
        | (`Assign (_a0,_a1),`Assign (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`For (_a0,_a1,_a2,_a3,_a4),`For (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#alident _a0 _b0) && (self#expr _a1 _b1)) &&
                (self#expr _a2 _b2))
               && (self#direction_flag _a3 _b3))
              && (self#expr _a4 _b4)
        | (`Fun _a0,`Fun _b0) -> self#case _a0 _b0
        | (`IfThenElse (_a0,_a1,_a2),`IfThenElse (_b0,_b1,_b2)) ->
            ((self#expr _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`IfThen (_a0,_a1),`IfThen (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`LabelS _a0,`LabelS _b0) -> self#alident _a0 _b0
        | (`Label (_a0,_a1),`Label (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#expr _a1 _b1)
        | (`Lazy _a0,`Lazy _b0) -> self#expr _a0 _b0
        | (`LetIn (_a0,_a1,_a2),`LetIn (_b0,_b1,_b2)) ->
            ((self#rec_flag _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`LetModule (_a0,_a1,_a2),`LetModule (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Match (_a0,_a1),`Match (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#case _a1 _b1)
        | (`New _a0,`New _b0) -> self#ident _a0 _b0
        | (`Obj _a0,`Obj _b0) -> self#class_str_item _a0 _b0
        | (`ObjEnd,`ObjEnd) -> true
        | (`ObjPat (_a0,_a1),`ObjPat (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#class_str_item _a1 _b1)
        | (`ObjPatEnd _a0,`ObjPatEnd _b0) -> self#patt _a0 _b0
        | (`OptLabl (_a0,_a1),`OptLabl (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#expr _a1 _b1)
        | (`OptLablS _a0,`OptLablS _b0) -> self#alident _a0 _b0
        | (`OvrInst _a0,`OvrInst _b0) -> self#rec_expr _a0 _b0
        | (`OvrInstEmpty,`OvrInstEmpty) -> true
        | (`Seq _a0,`Seq _b0) -> self#expr _a0 _b0
        | (`Send (_a0,_a1),`Send (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#alident _a1 _b1)
        | (`StringDot (_a0,_a1),`StringDot (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`Try (_a0,_a1),`Try (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#case _a1 _b1)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Coercion (_a0,_a1,_a2),`Coercion (_b0,_b1,_b2)) ->
            ((self#expr _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Subtype (_a0,_a1),`Subtype (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#ctyp _a1 _b1)
        | (`While (_a0,_a1),`While (_b0,_b1)) ->
            (self#expr _a0 _b0) && (self#expr _a1 _b1)
        | (`LetOpen (_a0,_a1),`LetOpen (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#expr _a1 _b1)
        | (`LocalTypeFun (_a0,_a1),`LocalTypeFun (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#expr _a1 _b1)
        | (`Package_expr _a0,`Package_expr _b0) -> self#module_expr _a0 _b0
        | (_,_) -> false
    method rec_expr : rec_expr -> rec_expr -> 'result41=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_expr _a0 _b0) && (self#rec_expr _a1 _b1)
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#expr _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result41)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result41)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result42=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result42)
        | (`Functor (_a0,_a1,_a2),`Functor (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_type _a2 _b2)
        | (`Sig _a0,`Sig _b0) -> self#sig_item _a0 _b0
        | (`SigEnd,`SigEnd) -> true
        | (`With (_a0,_a1),`With (_b0,_b1)) ->
            (self#module_type _a0 _b0) && (self#with_constr _a1 _b1)
        | (`ModuleTypeOf _a0,`ModuleTypeOf _b0) -> self#module_expr _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result42)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result43=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class _a0,`Class _b0) -> self#class_type _a0 _b0
        | (`ClassType _a0,`ClassType _b0) -> self#class_type _a0 _b0
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#sig_item _a0 _b0) && (self#sig_item _a1 _b1)
        | (`DirectiveSimple _a0,`DirectiveSimple _b0) -> self#alident _a0 _b0
        | (`Directive (_a0,_a1),`Directive (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#expr _a1 _b1)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result43)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result44=
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result44)
        | (_,_) -> false
    method binding : binding -> binding -> 'result45=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#binding _a0 _b0) && (self#binding _a1 _b1)
        | (`Bind (_a0,_a1),`Bind (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result45)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result46=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#module_binding _a0 _b0) && (self#module_binding _a1 _b1)
        | (`ModuleBind (_a0,_a1,_a2),`ModuleBind (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_expr _a2 _b2)
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result46)
        | (_,_) -> false
    method case : case -> case -> 'result47=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1),`Or (_b0,_b1)) ->
            (self#case _a0 _b0) && (self#case _a1 _b1)
        | (`Case (_a0,_a1),`Case (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#expr _a1 _b1)
        | (`CaseWhen (_a0,_a1,_a2),`CaseWhen (_b0,_b1,_b2)) ->
            ((self#patt _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result47)
        | (_,_) -> false
    method module_expr : module_expr -> module_expr -> 'result48=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result48)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#module_expr _a0 _b0) && (self#module_expr _a1 _b1)
        | (`Functor (_a0,_a1,_a2),`Functor (_b0,_b1,_b2)) ->
            ((self#auident _a0 _b0) && (self#module_type _a1 _b1)) &&
              (self#module_expr _a2 _b2)
        | (`Struct _a0,`Struct _b0) -> self#str_item _a0 _b0
        | (`StructEnd,`StructEnd) -> true
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#module_expr _a0 _b0) && (self#module_type _a1 _b1)
        | (`PackageModule _a0,`PackageModule _b0) -> self#expr _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result48)
        | (_,_) -> false
    method str_item : str_item -> str_item -> 'result49=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Class _a0,`Class _b0) -> self#class_expr _a0 _b0
        | (`ClassType _a0,`ClassType _b0) -> self#class_type _a0 _b0
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#str_item _a0 _b0) && (self#str_item _a1 _b1)
        | (`DirectiveSimple _a0,`DirectiveSimple _b0) -> self#alident _a0 _b0
        | (`Directive (_a0,_a1),`Directive (_b0,_b1)) ->
            (self#alident _a0 _b0) && (self#expr _a1 _b1)
        | (`Exception _a0,`Exception _b0) -> self#of_ctyp _a0 _b0
        | (`StExp _a0,`StExp _b0) -> self#expr _a0 _b0
        | (`External (_a0,_a1,_a2),`External (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#strings _a2 _b2)
        | (`Include _a0,`Include _b0) -> self#module_expr _a0 _b0
        | (`Module (_a0,_a1),`Module (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_expr _a1 _b1)
        | (`RecModule _a0,`RecModule _b0) -> self#module_binding _a0 _b0
        | (`ModuleType (_a0,_a1),`ModuleType (_b0,_b1)) ->
            (self#auident _a0 _b0) && (self#module_type _a1 _b1)
        | (`Open _a0,`Open _b0) -> self#ident _a0 _b0
        | (`Type _a0,`Type _b0) -> self#typedecl _a0 _b0
        | (`Value (_a0,_a1),`Value (_b0,_b1)) ->
            (self#rec_flag _a0 _b0) && (self#binding _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result49)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result50=
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result50)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result51=
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result51)
        | (_,_) -> false
    method class_expr : class_expr -> class_expr -> 'result52=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`CeApp (_a0,_a1),`CeApp (_b0,_b1)) ->
            (self#class_expr _a0 _b0) && (self#expr _a1 _b1)
        | (`ClassCon (_a0,_a1,_a2),`ClassCon (_b0,_b1,_b2)) ->
            ((self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#type_parameters _a2 _b2)
        | (`ClassConS (_a0,_a1),`ClassConS (_b0,_b1)) ->
            (self#virtual_flag _a0 _b0) && (self#ident _a1 _b1)
        | (`CeFun (_a0,_a1),`CeFun (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#class_expr _a1 _b1)
        | (`LetIn (_a0,_a1,_a2),`LetIn (_b0,_b1,_b2)) ->
            ((self#rec_flag _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | (`Obj _a0,`Obj _b0) -> self#class_str_item _a0 _b0
        | (`ObjEnd,`ObjEnd) -> true
        | (`ObjPat (_a0,_a1),`ObjPat (_b0,_b1)) ->
            (self#patt _a0 _b0) && (self#class_str_item _a1 _b1)
        | (`ObjPatEnd _a0,`ObjPatEnd _b0) -> self#patt _a0 _b0
        | (`Constraint (_a0,_a1),`Constraint (_b0,_b1)) ->
            (self#class_expr _a0 _b0) && (self#class_type _a1 _b1)
        | (`And (_a0,_a1),`And (_b0,_b1)) ->
            (self#class_expr _a0 _b0) && (self#class_expr _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#class_expr _a0 _b0) && (self#class_expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result52)
        | (_,_) -> false
    method class_str_item : class_str_item -> class_str_item -> 'result53=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#class_str_item _a0 _b0) && (self#class_str_item _a1 _b1)
        | (`Eq (_a0,_a1),`Eq (_b0,_b1)) ->
            (self#ctyp _a0 _b0) && (self#ctyp _a1 _b1)
        | (`Inherit (_a0,_a1),`Inherit (_b0,_b1)) ->
            (self#override_flag _a0 _b0) && (self#class_expr _a1 _b1)
        | (`InheritAs (_a0,_a1,_a2),`InheritAs (_b0,_b1,_b2)) ->
            ((self#override_flag _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`Initializer _a0,`Initializer _b0) -> self#expr _a0 _b0
        | (`CrMth (_a0,_a1,_a2,_a3,_a4),`CrMth (_b0,_b1,_b2,_b3,_b4)) ->
            ((((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
                (self#private_flag _a2 _b2))
               && (self#expr _a3 _b3))
              && (self#ctyp _a4 _b4)
        | (`CrMthS (_a0,_a1,_a2,_a3),`CrMthS (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#private_flag _a2 _b2))
              && (self#expr _a3 _b3)
        | (`CrVal (_a0,_a1,_a2,_a3),`CrVal (_b0,_b1,_b2,_b3)) ->
            (((self#alident _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#mutable_flag _a2 _b2))
              && (self#expr _a3 _b3)
        | (`CrVir (_a0,_a1,_a2),`CrVir (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#private_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`CrVvr (_a0,_a1,_a2),`CrVvr (_b0,_b1,_b2)) ->
            ((self#alident _a0 _b0) && (self#mutable_flag _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result53)
        | (_,_) -> false
    method ep : ep -> ep -> 'result54=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result54)
        | (`App (_a0,_a1),`App (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Vrn _a0,`Vrn _b0) -> self#string _a0 _b0
        | (`Com (_a0,_a1),`Com (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#ep _a0 _b0) && (self#ep _a1 _b1)
        | (`Tup _a0,`Tup _b0) -> self#ep _a0 _b0
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result54)
        | (`ArrayEmpty,`ArrayEmpty) -> true
        | (`Array _a0,`Array _b0) -> self#ep _a0 _b0
        | (`Record _a0,`Record _b0) -> self#rec_bind _a0 _b0
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result54)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result54)
        | (_,_) -> false
    method rec_bind : rec_bind -> rec_bind -> 'result55=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RecBind (_a0,_a1),`RecBind (_b0,_b1)) ->
            (self#ident _a0 _b0) && (self#ep _a1 _b1)
        | (`Sem (_a0,_a1),`Sem (_b0,_b1)) ->
            (self#rec_bind _a0 _b0) && (self#rec_bind _a1 _b1)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result55)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result55)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result56= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'result57= self#unknown
  end
class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> 'result58=
      fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> 'result59=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> 'result60=
      fun fmt  `Nil  -> Format.fprintf fmt "`Nil"
    method literal : 'fmt -> literal -> 'result61=
      fun fmt  ->
        function
        | `Chr _a0 -> Format.fprintf fmt "@[<1>(`Chr@ %a)@]" self#string _a0
        | `Int _a0 -> Format.fprintf fmt "@[<1>(`Int@ %a)@]" self#string _a0
        | `Int32 _a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]" self#string _a0
        | `Int64 _a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]" self#string _a0
        | `Flo _a0 -> Format.fprintf fmt "@[<1>(`Flo@ %a)@]" self#string _a0
        | `NativeInt _a0 ->
            Format.fprintf fmt "@[<1>(`NativeInt@ %a)@]" self#string _a0
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
    method rec_flag : 'fmt -> rec_flag -> 'result62=
      fun fmt  ->
        function
        | `Recursive -> Format.fprintf fmt "`Recursive"
        | `ReNil -> Format.fprintf fmt "`ReNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result62)
    method direction_flag : 'fmt -> direction_flag -> 'result63=
      fun fmt  ->
        function
        | `To -> Format.fprintf fmt "`To"
        | `Downto -> Format.fprintf fmt "`Downto"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result63)
    method mutable_flag : 'fmt -> mutable_flag -> 'result64=
      fun fmt  ->
        function
        | `Mutable -> Format.fprintf fmt "`Mutable"
        | `MuNil -> Format.fprintf fmt "`MuNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result64)
    method private_flag : 'fmt -> private_flag -> 'result65=
      fun fmt  ->
        function
        | `Private -> Format.fprintf fmt "`Private"
        | `PrNil -> Format.fprintf fmt "`PrNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result65)
    method virtual_flag : 'fmt -> virtual_flag -> 'result66=
      fun fmt  ->
        function
        | `Virtual -> Format.fprintf fmt "`Virtual"
        | `ViNil -> Format.fprintf fmt "`ViNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result66)
    method override_flag : 'fmt -> override_flag -> 'result67=
      fun fmt  ->
        function
        | `Override -> Format.fprintf fmt "`Override"
        | `OvNil -> Format.fprintf fmt "`OvNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result67)
    method row_var_flag : 'fmt -> row_var_flag -> 'result68=
      fun fmt  ->
        function
        | `RowVar -> Format.fprintf fmt "`RowVar"
        | `RvNil -> Format.fprintf fmt "`RvNil"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result68)
    method position_flag : 'fmt -> position_flag -> 'result69=
      fun fmt  ->
        function
        | `Positive -> Format.fprintf fmt "`Positive"
        | `Negative -> Format.fprintf fmt "`Negative"
        | `Normal -> Format.fprintf fmt "`Normal"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result69)
    method strings : 'fmt -> strings -> 'result70=
      fun fmt  ->
        function
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#strings _a0
              self#strings _a1
        | `Str _a0 -> Format.fprintf fmt "@[<1>(`Str@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result70)
    method alident : 'fmt -> alident -> 'result71=
      fun fmt  ->
        function
        | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result71)
    method auident : 'fmt -> auident -> 'result72=
      fun fmt  ->
        function
        | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result72)
    method aident : 'fmt -> aident -> 'result73=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>'result73)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result73)
    method astring : 'fmt -> astring -> 'result74=
      fun fmt  ->
        function
        | `C _a0 -> Format.fprintf fmt "@[<1>(`C@ %a)@]" self#string _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result74)
    method uident : 'fmt -> uident -> 'result75=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#uident _a0
              self#uident _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>'result75)
    method ident : 'fmt -> ident -> 'result76=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#ident _a0
              self#ident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>'result76)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result76)
    method dupath : 'fmt -> dupath -> 'result77=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#dupath _a1
        | #auident as _a0 -> (self#auident fmt _a0 :>'result77)
    method dlpath : 'fmt -> dlpath -> 'result78=
      fun fmt  ->
        function
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#dupath _a0
              self#alident _a1
        | #alident as _a0 -> (self#alident fmt _a0 :>'result78)
    method any : 'fmt -> any -> 'result79=
      fun fmt  `Any  -> Format.fprintf fmt "`Any"
    method sid : 'fmt -> sid -> 'result80=
      fun fmt  (`Id _a0)  ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]" self#ident _a0
    method ctyp : 'fmt -> ctyp -> 'result81=
      fun fmt  ->
        function
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#ctyp _a0
              self#alident _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result81)
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
        | #sid as _a0 -> (self#sid fmt _a0 :>'result81)
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
        | `Tup _a0 -> Format.fprintf fmt "@[<1>(`Tup@ %a)@]" self#ctyp _a0
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result81)
    method type_parameters : 'fmt -> type_parameters -> 'result82=
      fun fmt  ->
        function
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#type_parameters
              _a0 self#type_parameters _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result82)
    method row_field : 'fmt -> row_field -> 'result83=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result83)
        | `Or (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a)@]" self#row_field _a0
              self#row_field _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
        | `TyVrnOf (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnOf@ %a@ %a)@]" self#astring _a0
              self#ctyp _a1
        | `Ctyp _a0 -> Format.fprintf fmt "@[<1>(`Ctyp@ %a)@]" self#ctyp _a0
    method tag_names : 'fmt -> tag_names -> 'result84=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result84)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#tag_names _a0
              self#tag_names _a1
        | `TyVrn _a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]" self#astring _a0
    method typedecl : 'fmt -> typedecl -> 'result85=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result85)
    method type_constr : 'fmt -> type_constr -> 'result86=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#type_constr _a0
              self#type_constr _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result86)
    method opt_type_constr : 'fmt -> opt_type_constr -> 'result87=
      fun fmt  ->
        function
        | `Constr _a0 ->
            Format.fprintf fmt "@[<1>(`Constr@ %a)@]" self#type_constr _a0
        | `Nil -> Format.fprintf fmt "`Nil"
    method decl_param : 'fmt -> decl_param -> 'result88=
      fun fmt  ->
        function
        | `Quote (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a)@]" self#position_flag
              _a0 self#alident _a1
        | `QuoteAny _a0 ->
            Format.fprintf fmt "@[<1>(`QuoteAny@ %a)@]" self#position_flag
              _a0
        | `Any -> Format.fprintf fmt "`Any"
        | #ant as _a0 -> (self#ant fmt _a0 :>'result88)
    method decl_params : 'fmt -> decl_params -> 'result89=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result89)
    method opt_decl_params : 'fmt -> opt_decl_params -> 'result90=
      fun fmt  ->
        function
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" self#decl_params _a0
        | `None -> Format.fprintf fmt "`None"
    method type_info : 'fmt -> type_info -> 'result91=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result91)
    method type_repr : 'fmt -> type_repr -> 'result92=
      fun fmt  ->
        function
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#name_ctyp _a0
        | `Sum _a0 -> Format.fprintf fmt "@[<1>(`Sum@ %a)@]" self#or_ctyp _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result92)
    method name_ctyp : 'fmt -> name_ctyp -> 'result93=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result93)
    method or_ctyp : 'fmt -> or_ctyp -> 'result94=
      fun fmt  ->
        function
        | `Or (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a)@]" self#or_ctyp _a0
              self#or_ctyp _a1
        | `TyCol (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a)@]" self#sid _a0
              self#ctyp _a1
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#sid _a0 self#ctyp
              _a1
        | #sid as _a0 -> (self#sid fmt _a0 :>'result94)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result94)
    method of_ctyp : 'fmt -> of_ctyp -> 'result95=
      fun fmt  ->
        function
        | `Of (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#sid _a0 self#ctyp
              _a1
        | #sid as _a0 -> (self#sid fmt _a0 :>'result95)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result95)
    method patt : 'fmt -> patt -> 'result96=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result96)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#patt _a0
              self#patt _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#patt _a0
              self#patt _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#patt _a0
              self#patt _a1
        | `Tup _a0 -> Format.fprintf fmt "@[<1>(`Tup@ %a)@]" self#patt _a0
        | #any as _a0 -> (self#any fmt _a0 :>'result96)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_patt _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result96)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result96)
        | `Alias (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a)@]" self#patt _a0
              self#alident _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#patt _a0
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#patt _a1
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#patt _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OptLablExpr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLablExpr@ %a@ %a@ %a)@]"
              self#alident _a0 self#patt _a1 self#expr _a2
        | `Or (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a)@]" self#patt _a0 self#patt
              _a1
        | `PaRng (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a)@]" self#patt _a0
              self#patt _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#patt _a0
              self#ctyp _a1
        | `ClassPath _a0 ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a)@]" self#ident _a0
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#patt _a0
        | `ModuleUnpack _a0 ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a)@]" self#auident _a0
        | `ModuleConstraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a)@]"
              self#auident _a0 self#ctyp _a1
    method rec_patt : 'fmt -> rec_patt -> 'result97=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#patt _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_patt _a0
              self#rec_patt _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result97)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result97)
    method expr : 'fmt -> expr -> 'result98=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result98)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `Vrn _a0 -> Format.fprintf fmt "@[<1>(`Vrn@ %a)@]" self#string _a0
        | `Com (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `Tup _a0 -> Format.fprintf fmt "@[<1>(`Tup@ %a)@]" self#expr _a0
        | #any as _a0 -> (self#any fmt _a0 :>'result98)
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_expr _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result98)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result98)
        | `RecordWith (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a)@]" self#rec_expr
              _a0 self#expr _a1
        | `Dot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `ArrayDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#expr _a0
        | `ExAsf -> Format.fprintf fmt "`ExAsf"
        | `ExAsr _a0 ->
            Format.fprintf fmt "@[<1>(`ExAsr@ %a)@]" self#expr _a0
        | `Assign (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Assign@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `For (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#expr _a1 self#expr _a2
              self#direction_flag _a3 self#expr _a4
        | `Fun _a0 -> Format.fprintf fmt "@[<1>(`Fun@ %a)@]" self#case _a0
        | `IfThenElse (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a)@]" self#expr
              _a0 self#expr _a1 self#expr _a2
        | `IfThen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`IfThen@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `LabelS _a0 ->
            Format.fprintf fmt "@[<1>(`LabelS@ %a)@]" self#alident _a0
        | `Label (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a)@]" self#alident _a0
              self#expr _a1
        | `Lazy _a0 -> Format.fprintf fmt "@[<1>(`Lazy@ %a)@]" self#expr _a0
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#rec_flag
              _a0 self#binding _a1 self#expr _a2
        | `LetModule (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a)@]" self#auident
              _a0 self#module_expr _a1 self#expr _a2
        | `Match (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a)@]" self#expr _a0
              self#case _a1
        | `New _a0 -> Format.fprintf fmt "@[<1>(`New@ %a)@]" self#ident _a0
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#class_str_item _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#patt _a0
              self#class_str_item _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#patt _a0
        | `OptLabl (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a)@]" self#alident _a0
              self#expr _a1
        | `OptLablS _a0 ->
            Format.fprintf fmt "@[<1>(`OptLablS@ %a)@]" self#alident _a0
        | `OvrInst _a0 ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]" self#rec_expr _a0
        | `OvrInstEmpty -> Format.fprintf fmt "`OvrInstEmpty"
        | `Seq _a0 -> Format.fprintf fmt "@[<1>(`Seq@ %a)@]" self#expr _a0
        | `Send (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a)@]" self#expr _a0
              self#alident _a1
        | `StringDot (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `Try (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a)@]" self#expr _a0
              self#case _a1
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#expr _a0
              self#ctyp _a1
        | `Coercion (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Coercion@ %a@ %a@ %a)@]" self#expr _a0
              self#ctyp _a1 self#ctyp _a2
        | `Subtype (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Subtype@ %a@ %a)@]" self#expr _a0
              self#ctyp _a1
        | `While (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a)@]" self#expr _a0
              self#expr _a1
        | `LetOpen (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LetOpen@ %a@ %a)@]" self#ident _a0
              self#expr _a1
        | `LocalTypeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a)@]" self#alident
              _a0 self#expr _a1
        | `Package_expr _a0 ->
            Format.fprintf fmt "@[<1>(`Package_expr@ %a)@]" self#module_expr
              _a0
    method rec_expr : 'fmt -> rec_expr -> 'result99=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_expr _a0
              self#rec_expr _a1
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#expr _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result99)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result99)
    method module_type : 'fmt -> module_type -> 'result100=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result100)
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
            Format.fprintf fmt "@[<1>(`ModuleTypeOf@ %a)@]" self#module_expr
              _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result100)
    method sig_item : 'fmt -> sig_item -> 'result101=
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
              self#expr _a1
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result101)
    method with_constr : 'fmt -> with_constr -> 'result102=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result102)
    method binding : 'fmt -> binding -> 'result103=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#binding _a0
              self#binding _a1
        | `Bind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a)@]" self#patt _a0
              self#expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result103)
    method module_binding : 'fmt -> module_binding -> 'result104=
      fun fmt  ->
        function
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#module_binding
              _a0 self#module_binding _a1
        | `ModuleBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a)@]"
              self#auident _a0 self#module_type _a1 self#module_expr _a2
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result104)
    method case : 'fmt -> case -> 'result105=
      fun fmt  ->
        function
        | `Or (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a)@]" self#case _a0 self#case
              _a1
        | `Case (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a)@]" self#patt _a0
              self#expr _a1
        | `CaseWhen (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CaseWhen@ %a@ %a@ %a)@]" self#patt _a0
              self#expr _a1 self#expr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result105)
    method module_expr : 'fmt -> module_expr -> 'result106=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result106)
        | `App (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a)@]" self#module_expr _a0
              self#module_expr _a1
        | `Functor (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a)@]" self#auident
              _a0 self#module_type _a1 self#module_expr _a2
        | `Struct _a0 ->
            Format.fprintf fmt "@[<1>(`Struct@ %a)@]" self#str_item _a0
        | `StructEnd -> Format.fprintf fmt "`StructEnd"
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]"
              self#module_expr _a0 self#module_type _a1
        | `PackageModule _a0 ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]" self#expr _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result106)
    method str_item : 'fmt -> str_item -> 'result107=
      fun fmt  ->
        function
        | `Class _a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]" self#class_expr _a0
        | `ClassType _a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]" self#class_type _a0
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#str_item _a0
              self#str_item _a1
        | `DirectiveSimple _a0 ->
            Format.fprintf fmt "@[<1>(`DirectiveSimple@ %a)@]" self#alident
              _a0
        | `Directive (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a)@]" self#alident _a0
              self#expr _a1
        | `Exception _a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]" self#of_ctyp _a0
        | `StExp _a0 ->
            Format.fprintf fmt "@[<1>(`StExp@ %a)@]" self#expr _a0
        | `External (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a)@]" self#alident
              _a0 self#ctyp _a1 self#strings _a2
        | `Include _a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]" self#module_expr _a0
        | `Module (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a)@]" self#auident _a0
              self#module_expr _a1
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result107)
    method class_type : 'fmt -> class_type -> 'result108=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result108)
    method class_sig_item : 'fmt -> class_sig_item -> 'result109=
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result109)
    method class_expr : 'fmt -> class_expr -> 'result110=
      fun fmt  ->
        function
        | `CeApp (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a)@]" self#class_expr _a0
              self#expr _a1
        | `ClassCon (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ClassCon@ %a@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1 self#type_parameters _a2
        | `ClassConS (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassConS@ %a@ %a)@]"
              self#virtual_flag _a0 self#ident _a1
        | `CeFun (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a)@]" self#patt _a0
              self#class_expr _a1
        | `LetIn (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a)@]" self#rec_flag
              _a0 self#binding _a1 self#class_expr _a2
        | `Obj _a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]" self#class_str_item _a0
        | `ObjEnd -> Format.fprintf fmt "`ObjEnd"
        | `ObjPat (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ObjPat@ %a@ %a)@]" self#patt _a0
              self#class_str_item _a1
        | `ObjPatEnd _a0 ->
            Format.fprintf fmt "@[<1>(`ObjPatEnd@ %a)@]" self#patt _a0
        | `Constraint (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Constraint@ %a@ %a)@]" self#class_expr
              _a0 self#class_type _a1
        | `And (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a)@]" self#class_expr _a0
              self#class_expr _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#class_expr _a0
              self#class_expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result110)
    method class_str_item : 'fmt -> class_str_item -> 'result111=
      fun fmt  ->
        function
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#class_str_item
              _a0 self#class_str_item _a1
        | `Eq (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a)@]" self#ctyp _a0 self#ctyp
              _a1
        | `Inherit (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" self#override_flag
              _a0 self#class_expr _a1
        | `InheritAs (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`InheritAs@ %a@ %a@ %a)@]"
              self#override_flag _a0 self#class_expr _a1 self#alident _a2
        | `Initializer _a0 ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a)@]" self#expr _a0
        | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#override_flag _a1 self#private_flag _a2
              self#expr _a3 self#ctyp _a4
        | `CrMthS (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrMthS@ %a@ %a@ %a@ %a)@]"
              self#alident _a0 self#override_flag _a1 self#private_flag _a2
              self#expr _a3
        | `CrVal (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a)@]" self#alident
              _a0 self#override_flag _a1 self#mutable_flag _a2 self#expr _a3
        | `CrVir (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a)@]" self#alident _a0
              self#private_flag _a1 self#ctyp _a2
        | `CrVvr (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a)@]" self#alident _a0
              self#mutable_flag _a1 self#ctyp _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result111)
    method ep : 'fmt -> ep -> 'result112=
      fun fmt  ->
        function
        | #sid as _a0 -> (self#sid fmt _a0 :>'result112)
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
        | `Tup _a0 -> Format.fprintf fmt "@[<1>(`Tup@ %a)@]" self#ep _a0
        | #any as _a0 -> (self#any fmt _a0 :>'result112)
        | `ArrayEmpty -> Format.fprintf fmt "`ArrayEmpty"
        | `Array _a0 -> Format.fprintf fmt "@[<1>(`Array@ %a)@]" self#ep _a0
        | `Record _a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]" self#rec_bind _a0
        | #literal as _a0 -> (self#literal fmt _a0 :>'result112)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result112)
    method rec_bind : 'fmt -> rec_bind -> 'result113=
      fun fmt  ->
        function
        | `RecBind (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a)@]" self#ident _a0
              self#ep _a1
        | `Sem (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a)@]" self#rec_bind _a0
              self#rec_bind _a1
        | #any as _a0 -> (self#any fmt _a0 :>'result113)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result113)
    method fanloc_t : 'fmt -> FanLoc.t -> 'result114= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> 'result115=
      self#unknown
  end
let meta_ant _loc (`Ant (_a0,_a1)) = `Ant (_a0, _a1)
let meta_nil _loc `Nil = `Vrn (_loc, "Nil")
let meta_literal _loc =
  function
  | `Chr _a0 -> `App (_loc, (`Vrn (_loc, "Chr")), (meta_string _loc _a0))
  | `Int _a0 -> `App (_loc, (`Vrn (_loc, "Int")), (meta_string _loc _a0))
  | `Int32 _a0 -> `App (_loc, (`Vrn (_loc, "Int32")), (meta_string _loc _a0))
  | `Int64 _a0 -> `App (_loc, (`Vrn (_loc, "Int64")), (meta_string _loc _a0))
  | `Flo _a0 -> `App (_loc, (`Vrn (_loc, "Flo")), (meta_string _loc _a0))
  | `NativeInt _a0 ->
      `App (_loc, (`Vrn (_loc, "NativeInt")), (meta_string _loc _a0))
  | `Str _a0 -> `App (_loc, (`Vrn (_loc, "Str")), (meta_string _loc _a0))
let meta_rec_flag _loc =
  function
  | `Recursive -> `Vrn (_loc, "Recursive")
  | `ReNil -> `Vrn (_loc, "ReNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result119)
let meta_direction_flag _loc =
  function
  | `To -> `Vrn (_loc, "To")
  | `Downto -> `Vrn (_loc, "Downto")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result120)
let meta_mutable_flag _loc =
  function
  | `Mutable -> `Vrn (_loc, "Mutable")
  | `MuNil -> `Vrn (_loc, "MuNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result121)
let meta_private_flag _loc =
  function
  | `Private -> `Vrn (_loc, "Private")
  | `PrNil -> `Vrn (_loc, "PrNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result122)
let meta_virtual_flag _loc =
  function
  | `Virtual -> `Vrn (_loc, "Virtual")
  | `ViNil -> `Vrn (_loc, "ViNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result123)
let meta_override_flag _loc =
  function
  | `Override -> `Vrn (_loc, "Override")
  | `OvNil -> `Vrn (_loc, "OvNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result124)
let meta_row_var_flag _loc =
  function
  | `RowVar -> `Vrn (_loc, "RowVar")
  | `RvNil -> `Vrn (_loc, "RvNil")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result125)
let meta_position_flag _loc =
  function
  | `Positive -> `Vrn (_loc, "Positive")
  | `Negative -> `Vrn (_loc, "Negative")
  | `Normal -> `Vrn (_loc, "Normal")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result126)
let rec meta_strings _loc =
  function
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_strings _loc _a0))),
          (meta_strings _loc _a1))
  | `Str _a0 -> `App (_loc, (`Vrn (_loc, "Str")), (meta_string _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result127)
let meta_alident _loc =
  function
  | `Lid _a0 -> `App (_loc, (`Vrn (_loc, "Lid")), (meta_string _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result128)
let meta_auident _loc =
  function
  | `Uid _a0 -> `App (_loc, (`Vrn (_loc, "Uid")), (meta_string _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result129)
let meta_aident _loc =
  function
  | #alident as _a0 -> (meta_alident _loc _a0 :>'result130)
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result130)
let meta_astring _loc =
  function
  | `C _a0 -> `App (_loc, (`Vrn (_loc, "C")), (meta_string _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result131)
let rec meta_uident _loc =
  function
  | `Dot (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_uident _loc _a0))),
          (meta_uident _loc _a1))
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_uident _loc _a0))),
          (meta_uident _loc _a1))
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result132)
let rec meta_ident _loc =
  function
  | `Dot (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_ident _loc _a0))),
          (meta_ident _loc _a1))
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_ident _loc _a0))),
          (meta_ident _loc _a1))
  | #alident as _a0 -> (meta_alident _loc _a0 :>'result133)
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result133)
let rec meta_dupath _loc =
  function
  | `Dot (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_dupath _loc _a0))),
          (meta_dupath _loc _a1))
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result134)
let meta_dlpath _loc =
  function
  | `Dot (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_dupath _loc _a0))),
          (meta_alident _loc _a1))
  | #alident as _a0 -> (meta_alident _loc _a0 :>'result135)
let meta_any _loc `Any = `Vrn (_loc, "Any")
let meta_sid _loc (`Id _a0) =
  `App (_loc, (`Vrn (_loc, "Id")), (meta_ident _loc _a0))
let rec meta_ctyp _loc =
  function
  | `Alias (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Alias")), (meta_ctyp _loc _a0))),
          (meta_alident _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result168)
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Arrow (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Arrow")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `ClassPath _a0 ->
      `App (_loc, (`Vrn (_loc, "ClassPath")), (meta_ident _loc _a0))
  | `Label (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Label")), (meta_alident _loc _a0))),
          (meta_ctyp _loc _a1))
  | `OptLabl (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "OptLabl")), (meta_alident _loc _a0))),
          (meta_ctyp _loc _a1))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result168)
  | `TyObj (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TyObj")), (meta_name_ctyp _loc _a0))),
          (meta_row_var_flag _loc _a1))
  | `TyObjEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "TyObjEnd")), (meta_row_var_flag _loc _a0))
  | `TyPol (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyPol")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `TyPolEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "TyPolEnd")), (meta_ctyp _loc _a0))
  | `TyTypePol (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TyTypePol")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Quote (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Quote")), (meta_position_flag _loc _a0))),
          (meta_alident _loc _a1))
  | `QuoteAny _a0 ->
      `App (_loc, (`Vrn (_loc, "QuoteAny")), (meta_position_flag _loc _a0))
  | `Tup _a0 -> `App (_loc, (`Vrn (_loc, "Tup")), (meta_ctyp _loc _a0))
  | `Sta (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sta")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `PolyEq _a0 ->
      `App (_loc, (`Vrn (_loc, "PolyEq")), (meta_row_field _loc _a0))
  | `PolySup _a0 ->
      `App (_loc, (`Vrn (_loc, "PolySup")), (meta_row_field _loc _a0))
  | `PolyInf _a0 ->
      `App (_loc, (`Vrn (_loc, "PolyInf")), (meta_row_field _loc _a0))
  | `Com (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `PolyInfSup (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "PolyInfSup")), (meta_row_field _loc _a0))),
          (meta_tag_names _loc _a1))
  | `Package _a0 ->
      `App (_loc, (`Vrn (_loc, "Package")), (meta_module_type _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result168)
and meta_type_parameters _loc =
  function
  | `Com (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Com")), (meta_type_parameters _loc _a0))),
          (meta_type_parameters _loc _a1))
  | `Ctyp _a0 -> `App (_loc, (`Vrn (_loc, "Ctyp")), (meta_ctyp _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result167)
and meta_row_field _loc =
  function
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result166)
  | `Or (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_row_field _loc _a0))),
          (meta_row_field _loc _a1))
  | `TyVrn _a0 ->
      `App (_loc, (`Vrn (_loc, "TyVrn")), (meta_astring _loc _a0))
  | `TyVrnOf (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TyVrnOf")), (meta_astring _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Ctyp _a0 -> `App (_loc, (`Vrn (_loc, "Ctyp")), (meta_ctyp _loc _a0))
and meta_tag_names _loc =
  function
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result165)
  | `App (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "App")), (meta_tag_names _loc _a0))),
          (meta_tag_names _loc _a1))
  | `TyVrn _a0 ->
      `App (_loc, (`Vrn (_loc, "TyVrn")), (meta_astring _loc _a0))
and meta_typedecl _loc =
  function
  | `TyDcl (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "TyDcl")),
                         (meta_alident _loc _a0))),
                    (meta_opt_decl_params _loc _a1))),
               (meta_type_info _loc _a2))), (meta_opt_type_constr _loc _a3))
  | `TyAbstr (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "TyAbstr")), (meta_alident _loc _a0))),
               (meta_opt_decl_params _loc _a1))),
          (meta_opt_type_constr _loc _a2))
  | `And (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_typedecl _loc _a0))),
          (meta_typedecl _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result164)
and meta_type_constr _loc =
  function
  | `And (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "And")), (meta_type_constr _loc _a0))),
          (meta_type_constr _loc _a1))
  | `Eq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result163)
and meta_opt_type_constr _loc =
  function
  | `Constr _a0 ->
      `App (_loc, (`Vrn (_loc, "Constr")), (meta_type_constr _loc _a0))
  | `Nil -> `Vrn (_loc, "Nil")
and meta_decl_param _loc =
  function
  | `Quote (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Quote")), (meta_position_flag _loc _a0))),
          (meta_alident _loc _a1))
  | `QuoteAny _a0 ->
      `App (_loc, (`Vrn (_loc, "QuoteAny")), (meta_position_flag _loc _a0))
  | `Any -> `Vrn (_loc, "Any")
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result161)
and meta_decl_params _loc =
  function
  | `Quote (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Quote")), (meta_position_flag _loc _a0))),
          (meta_alident _loc _a1))
  | `QuoteAny _a0 ->
      `App (_loc, (`Vrn (_loc, "QuoteAny")), (meta_position_flag _loc _a0))
  | `Any -> `Vrn (_loc, "Any")
  | `Com (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Com")), (meta_decl_params _loc _a0))),
          (meta_decl_params _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result160)
and meta_opt_decl_params _loc =
  function
  | `Some _a0 ->
      `App (_loc, (`Vrn (_loc, "Some")), (meta_decl_params _loc _a0))
  | `None -> `Vrn (_loc, "None")
and meta_type_info _loc =
  function
  | `TyMan (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyMan")), (meta_ctyp _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_type_repr _loc _a2))
  | `TyRepr (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TyRepr")), (meta_private_flag _loc _a0))),
          (meta_type_repr _loc _a1))
  | `TyEq (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TyEq")), (meta_private_flag _loc _a0))),
          (meta_ctyp _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result158)
and meta_type_repr _loc =
  function
  | `Record _a0 ->
      `App (_loc, (`Vrn (_loc, "Record")), (meta_name_ctyp _loc _a0))
  | `Sum _a0 -> `App (_loc, (`Vrn (_loc, "Sum")), (meta_or_ctyp _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result157)
and meta_name_ctyp _loc =
  function
  | `Sem (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Sem")), (meta_name_ctyp _loc _a0))),
          (meta_name_ctyp _loc _a1))
  | `TyCol (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_sid _loc _a0))),
          (meta_ctyp _loc _a1))
  | `TyColMut (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyColMut")), (meta_sid _loc _a0))),
          (meta_ctyp _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result156)
and meta_or_ctyp _loc =
  function
  | `Or (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_or_ctyp _loc _a0))),
          (meta_or_ctyp _loc _a1))
  | `TyCol (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_sid _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Of (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (meta_sid _loc _a0))),
          (meta_ctyp _loc _a1))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result155)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result155)
and meta_of_ctyp _loc =
  function
  | `Of (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (meta_sid _loc _a0))),
          (meta_ctyp _loc _a1))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result154)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result154)
and meta_patt _loc =
  function
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result153)
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_patt _loc _a0))),
          (meta_patt _loc _a1))
  | `Vrn _a0 -> `App (_loc, (`Vrn (_loc, "Vrn")), (meta_string _loc _a0))
  | `Com (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_patt _loc _a0))),
          (meta_patt _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_patt _loc _a0))),
          (meta_patt _loc _a1))
  | `Tup _a0 -> `App (_loc, (`Vrn (_loc, "Tup")), (meta_patt _loc _a0))
  | #any as _a0 -> (meta_any _loc _a0 :>'result153)
  | `Record _a0 ->
      `App (_loc, (`Vrn (_loc, "Record")), (meta_rec_patt _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result153)
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result153)
  | `Alias (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Alias")), (meta_patt _loc _a0))),
          (meta_alident _loc _a1))
  | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
  | `Array _a0 -> `App (_loc, (`Vrn (_loc, "Array")), (meta_patt _loc _a0))
  | `LabelS _a0 ->
      `App (_loc, (`Vrn (_loc, "LabelS")), (meta_alident _loc _a0))
  | `Label (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Label")), (meta_alident _loc _a0))),
          (meta_patt _loc _a1))
  | `OptLabl (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "OptLabl")), (meta_alident _loc _a0))),
          (meta_patt _loc _a1))
  | `OptLablS _a0 ->
      `App (_loc, (`Vrn (_loc, "OptLablS")), (meta_alident _loc _a0))
  | `OptLablExpr (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "OptLablExpr")),
                    (meta_alident _loc _a0))), (meta_patt _loc _a1))),
          (meta_expr _loc _a2))
  | `Or (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_patt _loc _a0))),
          (meta_patt _loc _a1))
  | `PaRng (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "PaRng")), (meta_patt _loc _a0))),
          (meta_patt _loc _a1))
  | `Constraint (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_patt _loc _a0))),
          (meta_ctyp _loc _a1))
  | `ClassPath _a0 ->
      `App (_loc, (`Vrn (_loc, "ClassPath")), (meta_ident _loc _a0))
  | `Lazy _a0 -> `App (_loc, (`Vrn (_loc, "Lazy")), (meta_patt _loc _a0))
  | `ModuleUnpack _a0 ->
      `App (_loc, (`Vrn (_loc, "ModuleUnpack")), (meta_auident _loc _a0))
  | `ModuleConstraint (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "ModuleConstraint")),
               (meta_auident _loc _a0))), (meta_ctyp _loc _a1))
and meta_rec_patt _loc =
  function
  | `RecBind (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_ident _loc _a0))),
          (meta_patt _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_rec_patt _loc _a0))),
          (meta_rec_patt _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result152)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result152)
and meta_expr _loc =
  function
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result151)
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `Vrn _a0 -> `App (_loc, (`Vrn (_loc, "Vrn")), (meta_string _loc _a0))
  | `Com (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `Tup _a0 -> `App (_loc, (`Vrn (_loc, "Tup")), (meta_expr _loc _a0))
  | #any as _a0 -> (meta_any _loc _a0 :>'result151)
  | `Record _a0 ->
      `App (_loc, (`Vrn (_loc, "Record")), (meta_rec_expr _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result151)
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result151)
  | `RecordWith (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecordWith")), (meta_rec_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `Dot (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `ArrayDot (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ArrayDot")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
  | `Array _a0 -> `App (_loc, (`Vrn (_loc, "Array")), (meta_expr _loc _a0))
  | `ExAsf -> `Vrn (_loc, "ExAsf")
  | `ExAsr _a0 -> `App (_loc, (`Vrn (_loc, "ExAsr")), (meta_expr _loc _a0))
  | `Assign (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Assign")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `For (_a0,_a1,_a2,_a3,_a4) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc,
                         (`App
                            (_loc, (`Vrn (_loc, "For")),
                              (meta_alident _loc _a0))),
                         (meta_expr _loc _a1))), (meta_expr _loc _a2))),
               (meta_direction_flag _loc _a3))), (meta_expr _loc _a4))
  | `Fun _a0 -> `App (_loc, (`Vrn (_loc, "Fun")), (meta_case _loc _a0))
  | `IfThenElse (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "IfThenElse")), (meta_expr _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `IfThen (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "IfThen")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `LabelS _a0 ->
      `App (_loc, (`Vrn (_loc, "LabelS")), (meta_alident _loc _a0))
  | `Label (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Label")), (meta_alident _loc _a0))),
          (meta_expr _loc _a1))
  | `Lazy _a0 -> `App (_loc, (`Vrn (_loc, "Lazy")), (meta_expr _loc _a0))
  | `LetIn (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "LetIn")), (meta_rec_flag _loc _a0))),
               (meta_binding _loc _a1))), (meta_expr _loc _a2))
  | `LetModule (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "LetModule")), (meta_auident _loc _a0))),
               (meta_module_expr _loc _a1))), (meta_expr _loc _a2))
  | `Match (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Match")), (meta_expr _loc _a0))),
          (meta_case _loc _a1))
  | `New _a0 -> `App (_loc, (`Vrn (_loc, "New")), (meta_ident _loc _a0))
  | `Obj _a0 ->
      `App (_loc, (`Vrn (_loc, "Obj")), (meta_class_str_item _loc _a0))
  | `ObjEnd -> `Vrn (_loc, "ObjEnd")
  | `ObjPat (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "ObjPat")), (meta_patt _loc _a0))),
          (meta_class_str_item _loc _a1))
  | `ObjPatEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (meta_patt _loc _a0))
  | `OptLabl (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "OptLabl")), (meta_alident _loc _a0))),
          (meta_expr _loc _a1))
  | `OptLablS _a0 ->
      `App (_loc, (`Vrn (_loc, "OptLablS")), (meta_alident _loc _a0))
  | `OvrInst _a0 ->
      `App (_loc, (`Vrn (_loc, "OvrInst")), (meta_rec_expr _loc _a0))
  | `OvrInstEmpty -> `Vrn (_loc, "OvrInstEmpty")
  | `Seq _a0 -> `App (_loc, (`Vrn (_loc, "Seq")), (meta_expr _loc _a0))
  | `Send (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Send")), (meta_expr _loc _a0))),
          (meta_alident _loc _a1))
  | `StringDot (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "StringDot")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `Try (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Try")), (meta_expr _loc _a0))),
          (meta_case _loc _a1))
  | `Constraint (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_expr _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Coercion (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Coercion")), (meta_expr _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Subtype (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Subtype")), (meta_expr _loc _a0))),
          (meta_ctyp _loc _a1))
  | `While (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "While")), (meta_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `LetOpen (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "LetOpen")), (meta_ident _loc _a0))),
          (meta_expr _loc _a1))
  | `LocalTypeFun (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "LocalTypeFun")), (meta_alident _loc _a0))),
          (meta_expr _loc _a1))
  | `Package_expr _a0 ->
      `App (_loc, (`Vrn (_loc, "Package_expr")), (meta_module_expr _loc _a0))
and meta_rec_expr _loc =
  function
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_rec_expr _loc _a0))),
          (meta_rec_expr _loc _a1))
  | `RecBind (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_ident _loc _a0))),
          (meta_expr _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result150)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result150)
and meta_module_type _loc =
  function
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result149)
  | `Functor (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "Functor")), (meta_auident _loc _a0))),
               (meta_module_type _loc _a1))), (meta_module_type _loc _a2))
  | `Sig _a0 -> `App (_loc, (`Vrn (_loc, "Sig")), (meta_sig_item _loc _a0))
  | `SigEnd -> `Vrn (_loc, "SigEnd")
  | `With (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "With")), (meta_module_type _loc _a0))),
          (meta_with_constr _loc _a1))
  | `ModuleTypeOf _a0 ->
      `App (_loc, (`Vrn (_loc, "ModuleTypeOf")), (meta_module_expr _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result149)
and meta_sig_item _loc =
  function
  | `Class _a0 ->
      `App (_loc, (`Vrn (_loc, "Class")), (meta_class_type _loc _a0))
  | `ClassType _a0 ->
      `App (_loc, (`Vrn (_loc, "ClassType")), (meta_class_type _loc _a0))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_sig_item _loc _a0))),
          (meta_sig_item _loc _a1))
  | `DirectiveSimple _a0 ->
      `App (_loc, (`Vrn (_loc, "DirectiveSimple")), (meta_alident _loc _a0))
  | `Directive (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Directive")), (meta_alident _loc _a0))),
          (meta_expr _loc _a1))
  | `Exception _a0 ->
      `App (_loc, (`Vrn (_loc, "Exception")), (meta_of_ctyp _loc _a0))
  | `External (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "External")), (meta_alident _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_strings _loc _a2))
  | `Include _a0 ->
      `App (_loc, (`Vrn (_loc, "Include")), (meta_module_type _loc _a0))
  | `Module (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Module")), (meta_auident _loc _a0))),
          (meta_module_type _loc _a1))
  | `RecModule _a0 ->
      `App (_loc, (`Vrn (_loc, "RecModule")), (meta_module_binding _loc _a0))
  | `ModuleType (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ModuleType")), (meta_auident _loc _a0))),
          (meta_module_type _loc _a1))
  | `ModuleTypeEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "ModuleTypeEnd")), (meta_auident _loc _a0))
  | `Open _a0 -> `App (_loc, (`Vrn (_loc, "Open")), (meta_ident _loc _a0))
  | `Type _a0 -> `App (_loc, (`Vrn (_loc, "Type")), (meta_typedecl _loc _a0))
  | `Val (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Val")), (meta_alident _loc _a0))),
          (meta_ctyp _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result148)
and meta_with_constr _loc =
  function
  | `TypeEq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TypeEq")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `TypeEqPriv (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TypeEqPriv")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `ModuleEq (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ModuleEq")), (meta_ident _loc _a0))),
          (meta_ident _loc _a1))
  | `TypeSubst (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "TypeSubst")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `ModuleSubst (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ModuleSubst")), (meta_ident _loc _a0))),
          (meta_ident _loc _a1))
  | `And (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "And")), (meta_with_constr _loc _a0))),
          (meta_with_constr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result147)
and meta_binding _loc =
  function
  | `And (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_binding _loc _a0))),
          (meta_binding _loc _a1))
  | `Bind (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Bind")), (meta_patt _loc _a0))),
          (meta_expr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result146)
and meta_module_binding _loc =
  function
  | `And (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "And")), (meta_module_binding _loc _a0))),
          (meta_module_binding _loc _a1))
  | `ModuleBind (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "ModuleBind")),
                    (meta_auident _loc _a0))), (meta_module_type _loc _a1))),
          (meta_module_expr _loc _a2))
  | `Constraint (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_auident _loc _a0))),
          (meta_module_type _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result145)
and meta_case _loc =
  function
  | `Or (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_case _loc _a0))),
          (meta_case _loc _a1))
  | `Case (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Case")), (meta_patt _loc _a0))),
          (meta_expr _loc _a1))
  | `CaseWhen (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CaseWhen")), (meta_patt _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result144)
and meta_module_expr _loc =
  function
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result143)
  | `App (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "App")), (meta_module_expr _loc _a0))),
          (meta_module_expr _loc _a1))
  | `Functor (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "Functor")), (meta_auident _loc _a0))),
               (meta_module_type _loc _a1))), (meta_module_expr _loc _a2))
  | `Struct _a0 ->
      `App (_loc, (`Vrn (_loc, "Struct")), (meta_str_item _loc _a0))
  | `StructEnd -> `Vrn (_loc, "StructEnd")
  | `Constraint (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "Constraint")), (meta_module_expr _loc _a0))),
          (meta_module_type _loc _a1))
  | `PackageModule _a0 ->
      `App (_loc, (`Vrn (_loc, "PackageModule")), (meta_expr _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result143)
and meta_str_item _loc =
  function
  | `Class _a0 ->
      `App (_loc, (`Vrn (_loc, "Class")), (meta_class_expr _loc _a0))
  | `ClassType _a0 ->
      `App (_loc, (`Vrn (_loc, "ClassType")), (meta_class_type _loc _a0))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_str_item _loc _a0))),
          (meta_str_item _loc _a1))
  | `DirectiveSimple _a0 ->
      `App (_loc, (`Vrn (_loc, "DirectiveSimple")), (meta_alident _loc _a0))
  | `Directive (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Directive")), (meta_alident _loc _a0))),
          (meta_expr _loc _a1))
  | `Exception _a0 ->
      `App (_loc, (`Vrn (_loc, "Exception")), (meta_of_ctyp _loc _a0))
  | `StExp _a0 -> `App (_loc, (`Vrn (_loc, "StExp")), (meta_expr _loc _a0))
  | `External (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "External")), (meta_alident _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_strings _loc _a2))
  | `Include _a0 ->
      `App (_loc, (`Vrn (_loc, "Include")), (meta_module_expr _loc _a0))
  | `Module (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Module")), (meta_auident _loc _a0))),
          (meta_module_expr _loc _a1))
  | `RecModule _a0 ->
      `App (_loc, (`Vrn (_loc, "RecModule")), (meta_module_binding _loc _a0))
  | `ModuleType (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ModuleType")), (meta_auident _loc _a0))),
          (meta_module_type _loc _a1))
  | `Open _a0 -> `App (_loc, (`Vrn (_loc, "Open")), (meta_ident _loc _a0))
  | `Type _a0 -> `App (_loc, (`Vrn (_loc, "Type")), (meta_typedecl _loc _a0))
  | `Value (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Value")), (meta_rec_flag _loc _a0))),
          (meta_binding _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result142)
and meta_class_type _loc =
  function
  | `ClassCon (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "ClassCon")),
                    (meta_virtual_flag _loc _a0))), (meta_ident _loc _a1))),
          (meta_type_parameters _loc _a2))
  | `ClassConS (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "ClassConS")), (meta_virtual_flag _loc _a0))),
          (meta_ident _loc _a1))
  | `CtFun (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "CtFun")), (meta_ctyp _loc _a0))),
          (meta_class_type _loc _a1))
  | `ObjTy (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "ObjTy")), (meta_ctyp _loc _a0))),
          (meta_class_sig_item _loc _a1))
  | `ObjTyEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "ObjTyEnd")), (meta_ctyp _loc _a0))
  | `Obj _a0 ->
      `App (_loc, (`Vrn (_loc, "Obj")), (meta_class_sig_item _loc _a0))
  | `ObjEnd -> `Vrn (_loc, "ObjEnd")
  | `And (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "And")), (meta_class_type _loc _a0))),
          (meta_class_type _loc _a1))
  | `CtCol (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "CtCol")), (meta_class_type _loc _a0))),
          (meta_class_type _loc _a1))
  | `Eq (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Eq")), (meta_class_type _loc _a0))),
          (meta_class_type _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result141)
and meta_class_sig_item _loc =
  function
  | `Eq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Sem")), (meta_class_sig_item _loc _a0))),
          (meta_class_sig_item _loc _a1))
  | `SigInherit _a0 ->
      `App (_loc, (`Vrn (_loc, "SigInherit")), (meta_class_type _loc _a0))
  | `Method (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Method")), (meta_alident _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_ctyp _loc _a2))
  | `CgVal (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "CgVal")),
                         (meta_alident _loc _a0))),
                    (meta_mutable_flag _loc _a1))),
               (meta_virtual_flag _loc _a2))), (meta_ctyp _loc _a3))
  | `CgVir (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CgVir")), (meta_alident _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_ctyp _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result140)
and meta_class_expr _loc =
  function
  | `CeApp (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "CeApp")), (meta_class_expr _loc _a0))),
          (meta_expr _loc _a1))
  | `ClassCon (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "ClassCon")),
                    (meta_virtual_flag _loc _a0))), (meta_ident _loc _a1))),
          (meta_type_parameters _loc _a2))
  | `ClassConS (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "ClassConS")), (meta_virtual_flag _loc _a0))),
          (meta_ident _loc _a1))
  | `CeFun (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "CeFun")), (meta_patt _loc _a0))),
          (meta_class_expr _loc _a1))
  | `LetIn (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "LetIn")), (meta_rec_flag _loc _a0))),
               (meta_binding _loc _a1))), (meta_class_expr _loc _a2))
  | `Obj _a0 ->
      `App (_loc, (`Vrn (_loc, "Obj")), (meta_class_str_item _loc _a0))
  | `ObjEnd -> `Vrn (_loc, "ObjEnd")
  | `ObjPat (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "ObjPat")), (meta_patt _loc _a0))),
          (meta_class_str_item _loc _a1))
  | `ObjPatEnd _a0 ->
      `App (_loc, (`Vrn (_loc, "ObjPatEnd")), (meta_patt _loc _a0))
  | `Constraint (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "Constraint")), (meta_class_expr _loc _a0))),
          (meta_class_type _loc _a1))
  | `And (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "And")), (meta_class_expr _loc _a0))),
          (meta_class_expr _loc _a1))
  | `Eq (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Eq")), (meta_class_expr _loc _a0))),
          (meta_class_expr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result139)
and meta_class_str_item _loc =
  function
  | `Sem (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Sem")), (meta_class_str_item _loc _a0))),
          (meta_class_str_item _loc _a1))
  | `Eq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_ctyp _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Inherit (_a0,_a1) ->
      `App
        (_loc,
          (`App
             (_loc, (`Vrn (_loc, "Inherit")), (meta_override_flag _loc _a0))),
          (meta_class_expr _loc _a1))
  | `InheritAs (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "InheritAs")),
                    (meta_override_flag _loc _a0))),
               (meta_class_expr _loc _a1))), (meta_alident _loc _a2))
  | `Initializer _a0 ->
      `App (_loc, (`Vrn (_loc, "Initializer")), (meta_expr _loc _a0))
  | `CrMth (_a0,_a1,_a2,_a3,_a4) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc,
                         (`App
                            (_loc, (`Vrn (_loc, "CrMth")),
                              (meta_alident _loc _a0))),
                         (meta_override_flag _loc _a1))),
                    (meta_private_flag _loc _a2))), (meta_expr _loc _a3))),
          (meta_ctyp _loc _a4))
  | `CrMthS (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "CrMthS")),
                         (meta_alident _loc _a0))),
                    (meta_override_flag _loc _a1))),
               (meta_private_flag _loc _a2))), (meta_expr _loc _a3))
  | `CrVal (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "CrVal")),
                         (meta_alident _loc _a0))),
                    (meta_override_flag _loc _a1))),
               (meta_mutable_flag _loc _a2))), (meta_expr _loc _a3))
  | `CrVir (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CrVir")), (meta_alident _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_ctyp _loc _a2))
  | `CrVvr (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CrVvr")), (meta_alident _loc _a0))),
               (meta_mutable_flag _loc _a1))), (meta_ctyp _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result138)
let rec meta_ep _loc =
  function
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result170)
  | `App (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_ep _loc _a0))),
          (meta_ep _loc _a1))
  | `Vrn _a0 -> `App (_loc, (`Vrn (_loc, "Vrn")), (meta_string _loc _a0))
  | `Com (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_ep _loc _a0))),
          (meta_ep _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_ep _loc _a0))),
          (meta_ep _loc _a1))
  | `Tup _a0 -> `App (_loc, (`Vrn (_loc, "Tup")), (meta_ep _loc _a0))
  | #any as _a0 -> (meta_any _loc _a0 :>'result170)
  | `ArrayEmpty -> `Vrn (_loc, "ArrayEmpty")
  | `Array _a0 -> `App (_loc, (`Vrn (_loc, "Array")), (meta_ep _loc _a0))
  | `Record _a0 ->
      `App (_loc, (`Vrn (_loc, "Record")), (meta_rec_bind _loc _a0))
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result170)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result170)
and meta_rec_bind _loc =
  function
  | `RecBind (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_ident _loc _a0))),
          (meta_ep _loc _a1))
  | `Sem (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_rec_bind _loc _a0))),
          (meta_rec_bind _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result169)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result169)