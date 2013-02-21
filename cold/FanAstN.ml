open StdLib
include AstN
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
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
    method ant_nil : ant_nil -> ant_nil -> 'result3=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result3)
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result3)
        | (_,_) -> false
    method literal : literal -> literal -> 'result4=
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
    method rec_flag : rec_flag -> rec_flag -> 'result5=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Recursive _a0,`Recursive _b0) -> self#loc _a0 _b0
        | (`ReNil _a0,`ReNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result5)
        | (_,_) -> false
    method direction_flag : direction_flag -> direction_flag -> 'result6=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`To _a0,`To _b0) -> self#loc _a0 _b0
        | (`Downto _a0,`Downto _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result6)
        | (_,_) -> false
    method mutable_flag : mutable_flag -> mutable_flag -> 'result7=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Mutable _a0,`Mutable _b0) -> self#loc _a0 _b0
        | (`MuNil _a0,`MuNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result7)
        | (_,_) -> false
    method private_flag : private_flag -> private_flag -> 'result8=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Private _a0,`Private _b0) -> self#loc _a0 _b0
        | (`PrNil _a0,`PrNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result8)
        | (_,_) -> false
    method virtual_flag : virtual_flag -> virtual_flag -> 'result9=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Virtual _a0,`Virtual _b0) -> self#loc _a0 _b0
        | (`ViNil _a0,`ViNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result9)
        | (_,_) -> false
    method override_flag : override_flag -> override_flag -> 'result10=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Override _a0,`Override _b0) -> self#loc _a0 _b0
        | (`OvNil _a0,`OvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result10)
        | (_,_) -> false
    method row_var_flag : row_var_flag -> row_var_flag -> 'result11=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`RowVar _a0,`RowVar _b0) -> self#loc _a0 _b0
        | (`RvNil _a0,`RvNil _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result11)
        | (_,_) -> false
    method position_flag : position_flag -> position_flag -> 'result12=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Positive _a0,`Positive _b0) -> self#loc _a0 _b0
        | (`Negative _a0,`Negative _b0) -> self#loc _a0 _b0
        | (`Normal _a0,`Normal _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result12)
        | (_,_) -> false
    method meta_bool : meta_bool -> meta_bool -> 'result13=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`True _a0,`True _b0) -> self#loc _a0 _b0
        | (`False _a0,`False _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result13)
        | (_,_) -> false
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'result14) ->
          'all_a0 meta_option -> 'all_a0 meta_option -> 'result14=
      fun mf_a  _a0  _b0  ->
        match (_a0, _b0) with
        | (`None,`None) -> true
        | (`Some _a0,`Some _b0) -> mf_a self _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result14)
        | (_,_) -> false
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'result15) ->
          'all_a0 meta_list -> 'all_a0 meta_list -> 'result15=
      fun mf_a  _a0  _b0  ->
        match (_a0, _b0) with
        | (`LNil,`LNil) -> true
        | (`LCons (_a0,_a1),`LCons (_b0,_b1)) ->
            (mf_a self _a0 _b0) && (self#meta_list mf_a _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result15)
        | (_,_) -> false
    method alident : alident -> alident -> 'result16=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Lid (_a0,_a1),`Lid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result16)
        | (_,_) -> false
    method auident : auident -> auident -> 'result17=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Uid (_a0,_a1),`Uid (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result17)
        | (_,_) -> false
    method aident : aident -> aident -> 'result18=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result18)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result18)
        | (_,_) -> false
    method astring : astring -> astring -> 'result19=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`C (_a0,_a1),`C (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#string _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result19)
        | (_,_) -> false
    method ident : ident -> ident -> 'result20=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#ident _a2 _b2)
        | ((#alident as _a0),(#alident as _b0)) ->
            (self#alident _a0 _b0 :>'result20)
        | ((#auident as _a0),(#auident as _b0)) ->
            (self#auident _a0 _b0 :>'result20)
        | (_,_) -> false
    method sid : sid -> sid -> 'result21=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Id (_a0,_a1),`Id (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ident _a1 _b1)
    method any : any -> any -> 'result22=
      fun _a0  _b0  ->
        match (_a0, _b0) with | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
    method ctyp : ctyp -> ctyp -> 'result23=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#alident _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result23)
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
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result23)
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
              (self#meta_option (fun self  -> self#alident) _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`Com (_a0,_a1,_a2),`Com (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result23)
        | (_,_) -> false
    method row_field : row_field -> row_field -> 'result24=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result24)
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
    method tag_names : tag_names -> tag_names -> 'result25=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result25)
        | (`App (_a0,_a1,_a2),`App (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#tag_names _a1 _b1)) &&
              (self#tag_names _a2 _b2)
        | (`TyVrn (_a0,_a1),`TyVrn (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#astring _a1 _b1)
        | (_,_) -> false
    method typedecl : typedecl -> typedecl -> 'result26=
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
            (self#ant_nil _a0 _b0 :>'result26)
        | (_,_) -> false
    method type_info : type_info -> type_info -> 'result27=
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
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result27)
        | (_,_) -> false
    method type_repr : type_repr -> type_repr -> 'result28=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)
        | (`Sum (_a0,_a1),`Sum (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ctyp _a1 _b1)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result28)
        | (_,_) -> false
    method name_ctyp : name_ctyp -> name_ctyp -> 'result29=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#name_ctyp _a1 _b1)) &&
              (self#name_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`TyColMut (_a0,_a1,_a2),`TyColMut (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result29)
        | (_,_) -> false
    method or_ctyp : or_ctyp -> or_ctyp -> 'result30=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#or_ctyp _a1 _b1)) &&
              (self#or_ctyp _a2 _b2)
        | (`TyCol (_a0,_a1,_a2),`TyCol (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result30)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result30)
        | (_,_) -> false
    method of_ctyp : of_ctyp -> of_ctyp -> 'result31=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Of (_a0,_a1,_a2),`Of (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sid _a1 _b1)) && (self#ctyp _a2 _b2)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result31)
        | ((#ant_nil as _a0),(#ant_nil as _b0)) ->
            (self#ant_nil _a0 _b0 :>'result31)
        | (_,_) -> false
    method patt : patt -> patt -> 'result32=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result32)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result32)
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
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result32)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_patt _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result32)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result32)
        | (`Alias (_a0,_a1,_a2),`Alias (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#alident _a2 _b2)
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#patt _a1 _b1)
        | (`Label (_a0,_a1,_a2),`Label (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`PaOlbi (_a0,_a1,_a2,_a3),`PaOlbi (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#patt _a2 _b2))
              && (self#meta_option (fun self  -> self#expr) _a3 _b3)
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
        | (`ModuleUnpack (_a0,_a1,_a2),`ModuleUnpack (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#auident _a1 _b1)) &&
              (self#meta_option (fun self  -> self#ctyp) _a2 _b2)
        | (_,_) -> false
    method rec_patt : rec_patt -> rec_patt -> 'result33=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result33)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#patt _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_patt _a1 _b1)) &&
              (self#rec_patt _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result33)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result33)
        | (_,_) -> false
    method expr : expr -> expr -> 'result34=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result34)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result34)
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
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result34)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_expr _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result34)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result34)
        | (`RecordWith (_a0,_a1,_a2),`RecordWith (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Dot (_a0,_a1,_a2),`Dot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`ArrayDot (_a0,_a1,_a2),`ArrayDot (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#expr _a1 _b1)) &&
              (self#expr _a2 _b2)
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
        | (`Obj (_a0,_a1,_a2),`Obj (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#class_str_item _a2 _b2)
        | (`OptLabl (_a0,_a1,_a2),`OptLabl (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`OvrInst (_a0,_a1),`OvrInst (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_expr _a1 _b1)
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
    method rec_expr : rec_expr -> rec_expr -> 'result35=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_expr _a1 _b1)) &&
              (self#rec_expr _a2 _b2)
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result35)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result35)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result36=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result36)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result36)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result36)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result37=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result37)
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#sig_item _a1 _b1)) &&
              (self#sig_item _a2 _b2)
        | (`Directive (_a0,_a1,_a2),`Directive (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`Exception (_a0,_a1),`Exception (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#of_ctyp _a1 _b1)
        | (`External (_a0,_a1,_a2,_a3),`External (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#alident _a1 _b1)) &&
               (self#ctyp _a2 _b2))
              && (self#meta_list (fun self  -> self#string) _a3 _b3)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result37)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result38=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result38)
        | (_,_) -> false
    method binding : binding -> binding -> 'result39=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`And (_a0,_a1,_a2),`And (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#binding _a1 _b1)) &&
              (self#binding _a2 _b2)
        | (`Bind (_a0,_a1,_a2),`Bind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#expr _a2 _b2)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result39)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result40=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result40)
        | (_,_) -> false
    method match_case : match_case -> match_case -> 'result41=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result41)
        | (`Or (_a0,_a1,_a2),`Or (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#match_case _a1 _b1)) &&
              (self#match_case _a2 _b2)
        | (`Case (_a0,_a1,_a2,_a3),`Case (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
               (self#expr _a2 _b2))
              && (self#expr _a3 _b3)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result41)
        | (_,_) -> false
    method module_expr : module_expr -> module_expr -> 'result42=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result42)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result42)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result42)
        | (_,_) -> false
    method str_item : str_item -> str_item -> 'result43=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`Class (_a0,_a1),`Class (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_expr _a1 _b1)
        | (`ClassType (_a0,_a1),`ClassType (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#class_type _a1 _b1)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#str_item _a1 _b1)) &&
              (self#str_item _a2 _b2)
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
              && (self#meta_list (fun self  -> self#string) _a3 _b3)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result43)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result44=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`CtCon (_a0,_a1,_a2,_a3),`CtCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#ctyp _a3 _b3)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result44)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result45=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result45)
        | (_,_) -> false
    method class_expr : class_expr -> class_expr -> 'result46=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`CeApp (_a0,_a1,_a2),`CeApp (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_expr _a1 _b1)) &&
              (self#expr _a2 _b2)
        | (`CeCon (_a0,_a1,_a2,_a3),`CeCon (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#virtual_flag _a1 _b1)) &&
               (self#ident _a2 _b2))
              && (self#ctyp _a3 _b3)
        | (`CeFun (_a0,_a1,_a2),`CeFun (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#patt _a1 _b1)) &&
              (self#class_expr _a2 _b2)
        | (`CeLet (_a0,_a1,_a2,_a3),`CeLet (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#rec_flag _a1 _b1)) &&
               (self#binding _a2 _b2))
              && (self#class_expr _a3 _b3)
        | (`Obj (_a0,_a1,_a2),`Obj (_b0,_b1,_b2)) ->
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result46)
        | (_,_) -> false
    method class_str_item : class_str_item -> class_str_item -> 'result47=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#class_str_item _a1 _b1)) &&
              (self#class_str_item _a2 _b2)
        | (`Eq (_a0,_a1,_a2),`Eq (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ctyp _a1 _b1)) &&
              (self#ctyp _a2 _b2)
        | (`Inherit (_a0,_a1,_a2,_a3),`Inherit (_b0,_b1,_b2,_b3)) ->
            (((self#loc _a0 _b0) && (self#override_flag _a1 _b1)) &&
               (self#class_expr _a2 _b2))
              && (self#meta_option (fun self  -> self#alident) _a3 _b3)
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
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result47)
        | (_,_) -> false
    method ep : ep -> ep -> 'result48=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | ((#nil as _a0),(#nil as _b0)) -> (self#nil _a0 _b0 :>'result48)
        | ((#sid as _a0),(#sid as _b0)) -> (self#sid _a0 _b0 :>'result48)
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
        | ((#any as _a0),(#any as _b0)) -> (self#any _a0 _b0 :>'result48)
        | (`Array (_a0,_a1),`Array (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#ep _a1 _b1)
        | (`Record (_a0,_a1),`Record (_b0,_b1)) ->
            (self#loc _a0 _b0) && (self#rec_bind _a1 _b1)
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result48)
        | ((#literal as _a0),(#literal as _b0)) ->
            (self#literal _a0 _b0 :>'result48)
        | (_,_) -> false
    method rec_bind : rec_bind -> rec_bind -> 'result49=
      fun _a0  _b0  ->
        match (_a0, _b0) with
        | (`Nil _a0,`Nil _b0) -> self#loc _a0 _b0
        | (`RecBind (_a0,_a1,_a2),`RecBind (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#ident _a1 _b1)) && (self#ep _a2 _b2)
        | (`Sem (_a0,_a1,_a2),`Sem (_b0,_b1,_b2)) ->
            ((self#loc _a0 _b0) && (self#rec_bind _a1 _b1)) &&
              (self#rec_bind _a2 _b2)
        | (`Any _a0,`Any _b0) -> self#loc _a0 _b0
        | ((#ant as _a0),(#ant as _b0)) -> (self#ant _a0 _b0 :>'result49)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result50= self#unknown
    method fanutil_anti_cxt :
      FanUtil.anti_cxt -> FanUtil.anti_cxt -> 'result51= self#unknown
  end
class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> 'result52=
      fun fmt  _a0  -> self#fanloc_t fmt _a0
    method ant : 'fmt -> ant -> 'result53=
      fun fmt  (`Ant (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc _a0
          self#fanutil_anti_cxt _a1
    method nil : 'fmt -> nil -> 'result54=
      fun fmt  (`Nil _a0)  ->
        Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
    method ant_nil : 'fmt -> ant_nil -> 'result55=
      fun fmt  ->
        function
        | #ant as _a0 -> (self#ant fmt _a0 :>'result55)
        | #nil as _a0 -> (self#nil fmt _a0 :>'result55)
    method literal : 'fmt -> literal -> 'result56=
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
    method rec_flag : 'fmt -> rec_flag -> 'result57=
      fun fmt  ->
        function
        | `Recursive _a0 ->
            Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" self#loc _a0
        | `ReNil _a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result57)
    method direction_flag : 'fmt -> direction_flag -> 'result58=
      fun fmt  ->
        function
        | `To _a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" self#loc _a0
        | `Downto _a0 ->
            Format.fprintf fmt "@[<1>(`Downto@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result58)
    method mutable_flag : 'fmt -> mutable_flag -> 'result59=
      fun fmt  ->
        function
        | `Mutable _a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" self#loc _a0
        | `MuNil _a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result59)
    method private_flag : 'fmt -> private_flag -> 'result60=
      fun fmt  ->
        function
        | `Private _a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]" self#loc _a0
        | `PrNil _a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result60)
    method virtual_flag : 'fmt -> virtual_flag -> 'result61=
      fun fmt  ->
        function
        | `Virtual _a0 ->
            Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" self#loc _a0
        | `ViNil _a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result61)
    method override_flag : 'fmt -> override_flag -> 'result62=
      fun fmt  ->
        function
        | `Override _a0 ->
            Format.fprintf fmt "@[<1>(`Override@ %a)@]" self#loc _a0
        | `OvNil _a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result62)
    method row_var_flag : 'fmt -> row_var_flag -> 'result63=
      fun fmt  ->
        function
        | `RowVar _a0 ->
            Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" self#loc _a0
        | `RvNil _a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result63)
    method position_flag : 'fmt -> position_flag -> 'result64=
      fun fmt  ->
        function
        | `Positive _a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc _a0
        | `Negative _a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc _a0
        | `Normal _a0 ->
            Format.fprintf fmt "@[<1>(`Normal@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result64)
    method meta_bool : 'fmt -> meta_bool -> 'result65=
      fun fmt  ->
        function
        | `True _a0 -> Format.fprintf fmt "@[<1>(`True@ %a)@]" self#loc _a0
        | `False _a0 -> Format.fprintf fmt "@[<1>(`False@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result65)
    method meta_option :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result66) ->
          'fmt -> 'all_a0 meta_option -> 'result66=
      fun mf_a  fmt  ->
        function
        | `None -> Format.fprintf fmt "`None"
        | `Some _a0 ->
            Format.fprintf fmt "@[<1>(`Some@ %a)@]" (mf_a self) _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result66)
    method meta_list :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result67) ->
          'fmt -> 'all_a0 meta_list -> 'result67=
      fun mf_a  fmt  ->
        function
        | `LNil -> Format.fprintf fmt "`LNil"
        | `LCons (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`LCons@ %a@ %a)@]" (mf_a self) _a0
              (self#meta_list mf_a) _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result67)
    method alident : 'fmt -> alident -> 'result68=
      fun fmt  ->
        function
        | `Lid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result68)
    method auident : 'fmt -> auident -> 'result69=
      fun fmt  ->
        function
        | `Uid (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc _a0
              self#string _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result69)
    method aident : 'fmt -> aident -> 'result70=
      fun fmt  ->
        function
        | #alident as _a0 -> (self#alident fmt _a0 :>'result70)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result70)
    method astring : 'fmt -> astring -> 'result71=
      fun fmt  ->
        function
        | `C (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" self#loc _a0 self#string
              _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result71)
    method ident : 'fmt -> ident -> 'result72=
      fun fmt  ->
        function
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ident _a2
        | #alident as _a0 -> (self#alident fmt _a0 :>'result72)
        | #auident as _a0 -> (self#auident fmt _a0 :>'result72)
    method sid : 'fmt -> sid -> 'result73=
      fun fmt  (`Id (_a0,_a1))  ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc _a0 self#ident _a1
    method any : 'fmt -> any -> 'result74=
      fun fmt  (`Any _a0)  ->
        Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
    method ctyp : 'fmt -> ctyp -> 'result75=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#alident _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result75)
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
        | #sid as _a0 -> (self#sid fmt _a0 :>'result75)
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
              self#position_flag _a1
              (self#meta_option (fun self  -> self#alident)) _a2
        | `TyCol (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | `Com (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result75)
    method row_field : 'fmt -> row_field -> 'result76=
      fun fmt  ->
        function
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result76)
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
    method tag_names : 'fmt -> tag_names -> 'result77=
      fun fmt  ->
        function
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result77)
        | `App (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" self#loc _a0
              self#tag_names _a1 self#tag_names _a2
        | `TyVrn (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc _a0
              self#astring _a1
    method typedecl : 'fmt -> typedecl -> 'result78=
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
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result78)
    method type_info : 'fmt -> type_info -> 'result79=
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
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result79)
    method type_repr : 'fmt -> type_repr -> 'result80=
      fun fmt  ->
        function
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#name_ctyp _a1
        | `Sum (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" self#loc _a0 self#ctyp
              _a1
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result80)
    method name_ctyp : 'fmt -> name_ctyp -> 'result81=
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
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result81)
    method or_ctyp : 'fmt -> or_ctyp -> 'result82=
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
              self#ctyp _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result82)
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result82)
    method of_ctyp : 'fmt -> of_ctyp -> 'result83=
      fun fmt  ->
        function
        | `Of (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc _a0
              self#sid _a1 self#ctyp _a2
        | #sid as _a0 -> (self#sid fmt _a0 :>'result83)
        | #ant_nil as _a0 -> (self#ant_nil fmt _a0 :>'result83)
    method patt : 'fmt -> patt -> 'result84=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result84)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result84)
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
        | #any as _a0 -> (self#any fmt _a0 :>'result84)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_patt _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result84)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result84)
        | `Alias (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#alident _a2
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0
              self#patt _a1
        | `Label (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#patt _a2
        | `PaOlbi (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`PaOlbi@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#patt _a2
              (self#meta_option (fun self  -> self#expr)) _a3
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
        | `ModuleUnpack (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a@ %a)@]" self#loc
              _a0 self#auident _a1
              (self#meta_option (fun self  -> self#ctyp)) _a2
    method rec_patt : 'fmt -> rec_patt -> 'result85=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result85)
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#patt _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_patt _a1 self#rec_patt _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result85)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result85)
    method expr : 'fmt -> expr -> 'result86=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result86)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result86)
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
        | #any as _a0 -> (self#any fmt _a0 :>'result86)
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result86)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result86)
        | `RecordWith (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecordWith@ %a@ %a@ %a)@]" self#loc
              _a0 self#rec_expr _a1 self#expr _a2
        | `Dot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Dot@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
        | `ArrayDot (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`ArrayDot@ %a@ %a@ %a)@]" self#loc _a0
              self#expr _a1 self#expr _a2
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
        | `Obj (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#class_str_item _a2
        | `OptLabl (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#expr _a2
        | `OvrInst (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1
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
    method rec_expr : 'fmt -> rec_expr -> 'result87=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_expr _a1 self#rec_expr _a2
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#expr _a2
        | #any as _a0 -> (self#any fmt _a0 :>'result87)
        | #ant as _a0 -> (self#ant fmt _a0 :>'result87)
    method module_type : 'fmt -> module_type -> 'result88=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result88)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result88)
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result88)
    method sig_item : 'fmt -> sig_item -> 'result89=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result89)
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#sig_item _a1 self#sig_item _a2
        | `Directive (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc _a0
              self#alident _a1 self#expr _a2
        | `Exception (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc _a0
              self#of_ctyp _a1
        | `External (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#alident _a1 self#ctyp _a2
              (self#meta_list (fun self  -> self#string)) _a3
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result89)
    method with_constr : 'fmt -> with_constr -> 'result90=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result90)
    method binding : 'fmt -> binding -> 'result91=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `And (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc _a0
              self#binding _a1 self#binding _a2
        | `Bind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#expr _a2
        | #ant as _a0 -> (self#ant fmt _a0 :>'result91)
    method module_binding : 'fmt -> module_binding -> 'result92=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result92)
    method match_case : 'fmt -> match_case -> 'result93=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result93)
        | `Or (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc _a0
              self#match_case _a1 self#match_case _a2
        | `Case (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#expr _a2 self#expr _a3
        | #ant as _a0 -> (self#ant fmt _a0 :>'result93)
    method module_expr : 'fmt -> module_expr -> 'result94=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result94)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result94)
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result94)
    method str_item : 'fmt -> str_item -> 'result95=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `Class (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1
        | `ClassType (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc _a0
              self#class_type _a1
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#str_item _a1 self#str_item _a2
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
              _a0 self#alident _a1 self#ctyp _a2
              (self#meta_list (fun self  -> self#string)) _a3
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result95)
    method class_type : 'fmt -> class_type -> 'result96=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `CtCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CtCon@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2 self#ctyp _a3
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result96)
    method class_sig_item : 'fmt -> class_sig_item -> 'result97=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result97)
    method class_expr : 'fmt -> class_expr -> 'result98=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `CeApp (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" self#loc _a0
              self#class_expr _a1 self#expr _a2
        | `CeCon (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CeCon@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#virtual_flag _a1 self#ident _a2 self#ctyp _a3
        | `CeFun (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" self#loc _a0
              self#patt _a1 self#class_expr _a2
        | `CeLet (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`CeLet@ %a@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_flag _a1 self#binding _a2 self#class_expr _a3
        | `Obj (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" self#loc _a0
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result98)
    method class_str_item : 'fmt -> class_str_item -> 'result99=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#class_str_item _a1 self#class_str_item _a2
        | `Eq (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc _a0
              self#ctyp _a1 self#ctyp _a2
        | `Inherit (_a0,_a1,_a2,_a3) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a@ %a)@]" self#loc
              _a0 self#override_flag _a1 self#class_expr _a2
              (self#meta_option (fun self  -> self#alident)) _a3
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
        | #ant as _a0 -> (self#ant fmt _a0 :>'result99)
    method ep : 'fmt -> ep -> 'result100=
      fun fmt  ->
        function
        | #nil as _a0 -> (self#nil fmt _a0 :>'result100)
        | #sid as _a0 -> (self#sid fmt _a0 :>'result100)
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
        | #any as _a0 -> (self#any fmt _a0 :>'result100)
        | `Array (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc _a0 
              self#ep _a1
        | `Record (_a0,_a1) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1
        | #ant as _a0 -> (self#ant fmt _a0 :>'result100)
        | #literal as _a0 -> (self#literal fmt _a0 :>'result100)
    method rec_bind : 'fmt -> rec_bind -> 'result101=
      fun fmt  ->
        function
        | `Nil _a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc _a0
        | `RecBind (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc _a0
              self#ident _a1 self#ep _a2
        | `Sem (_a0,_a1,_a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc _a0
              self#rec_bind _a1 self#rec_bind _a2
        | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc _a0
        | #ant as _a0 -> (self#ant fmt _a0 :>'result101)
    method fanloc_t : 'fmt -> FanLoc.t -> 'result102= self#unknown
    method fanutil_anti_cxt : 'fmt -> FanUtil.anti_cxt -> 'result103=
      self#unknown
  end
let meta_ant _loc (`Ant (_a0,_a1)) = `Ant (_a0, _a1)
let meta_nil _loc (`Nil _a0) =
  `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
let meta_ant_nil _loc =
  function
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result106)
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result106)
let meta_literal _loc =
  function
  | `Chr (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Chr")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Int (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Int")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Int32 (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Int32")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Int64 (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Int64")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Flo (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Flo")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `NativeInt (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "NativeInt")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Str (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Str")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
let meta_rec_flag _loc =
  function
  | `Recursive _a0 ->
      `App (_loc, (`Vrn (_loc, "Recursive")), (meta_loc _loc _a0))
  | `ReNil _a0 -> `App (_loc, (`Vrn (_loc, "ReNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result108)
let meta_direction_flag _loc =
  function
  | `To _a0 -> `App (_loc, (`Vrn (_loc, "To")), (meta_loc _loc _a0))
  | `Downto _a0 -> `App (_loc, (`Vrn (_loc, "Downto")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result109)
let meta_mutable_flag _loc =
  function
  | `Mutable _a0 ->
      `App (_loc, (`Vrn (_loc, "Mutable")), (meta_loc _loc _a0))
  | `MuNil _a0 -> `App (_loc, (`Vrn (_loc, "MuNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result110)
let meta_private_flag _loc =
  function
  | `Private _a0 ->
      `App (_loc, (`Vrn (_loc, "Private")), (meta_loc _loc _a0))
  | `PrNil _a0 -> `App (_loc, (`Vrn (_loc, "PrNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result111)
let meta_virtual_flag _loc =
  function
  | `Virtual _a0 ->
      `App (_loc, (`Vrn (_loc, "Virtual")), (meta_loc _loc _a0))
  | `ViNil _a0 -> `App (_loc, (`Vrn (_loc, "ViNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result112)
let meta_override_flag _loc =
  function
  | `Override _a0 ->
      `App (_loc, (`Vrn (_loc, "Override")), (meta_loc _loc _a0))
  | `OvNil _a0 -> `App (_loc, (`Vrn (_loc, "OvNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result113)
let meta_row_var_flag _loc =
  function
  | `RowVar _a0 -> `App (_loc, (`Vrn (_loc, "RowVar")), (meta_loc _loc _a0))
  | `RvNil _a0 -> `App (_loc, (`Vrn (_loc, "RvNil")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result114)
let meta_position_flag _loc =
  function
  | `Positive _a0 ->
      `App (_loc, (`Vrn (_loc, "Positive")), (meta_loc _loc _a0))
  | `Negative _a0 ->
      `App (_loc, (`Vrn (_loc, "Negative")), (meta_loc _loc _a0))
  | `Normal _a0 -> `App (_loc, (`Vrn (_loc, "Normal")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result115)
let meta_meta_bool _loc =
  function
  | `True _a0 -> `App (_loc, (`Vrn (_loc, "True")), (meta_loc _loc _a0))
  | `False _a0 -> `App (_loc, (`Vrn (_loc, "False")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result116)
let meta_meta_option mf_a _loc =
  function
  | `None -> `Vrn (_loc, "None")
  | `Some _a0 -> `App (_loc, (`Vrn (_loc, "Some")), (mf_a _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result117)
let rec meta_meta_list mf_a _loc =
  function
  | `LNil -> `Vrn (_loc, "LNil")
  | `LCons (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "LCons")), (mf_a _loc _a0))),
          (meta_meta_list mf_a _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result118)
let meta_alident _loc =
  function
  | `Lid (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Lid")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result119)
let meta_auident _loc =
  function
  | `Uid (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Uid")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result120)
let meta_aident _loc =
  function
  | #alident as _a0 -> (meta_alident _loc _a0 :>'result121)
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result121)
let meta_astring _loc =
  function
  | `C (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "C")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result122)
let rec meta_ident _loc =
  function
  | `Dot (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_ident _loc _a2))
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_ident _loc _a2))
  | #alident as _a0 -> (meta_alident _loc _a0 :>'result123)
  | #auident as _a0 -> (meta_auident _loc _a0 :>'result123)
let meta_sid _loc (`Id (_a0,_a1)) =
  `App
    (_loc, (`App (_loc, (`Vrn (_loc, "Id")), (meta_loc _loc _a0))),
      (meta_ident _loc _a1))
let meta_any _loc (`Any _a0) =
  `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
let rec meta_ctyp _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `Alias (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_alident _loc _a2))
  | #any as _a0 -> (meta_any _loc _a0 :>'result150)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Arrow (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Arrow")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `ClassPath (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ClassPath")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
  | `Label (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
  | `OptLabl (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result150)
  | `TyObj (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyObj")), (meta_loc _loc _a0))),
               (meta_name_ctyp _loc _a1))), (meta_row_var_flag _loc _a2))
  | `TyPol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyPol")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `TyTypePol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyTypePol")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Quote (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Quote")), (meta_loc _loc _a0))),
               (meta_position_flag _loc _a1))),
          (meta_meta_option meta_alident _loc _a2))
  | `TyCol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
               (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
  | `Com (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Of (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Or (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Tup (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
          (meta_ctyp _loc _a1))
  | `Sta (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sta")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `PolyEq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "PolyEq")), (meta_loc _loc _a0))),
          (meta_row_field _loc _a1))
  | `PolySup (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "PolySup")), (meta_loc _loc _a0))),
          (meta_row_field _loc _a1))
  | `PolyInf (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "PolyInf")), (meta_loc _loc _a0))),
          (meta_row_field _loc _a1))
  | `PolyInfSup (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "PolyInfSup")), (meta_loc _loc _a0))),
               (meta_row_field _loc _a1))), (meta_tag_names _loc _a2))
  | `Package (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Package")), (meta_loc _loc _a0))),
          (meta_module_type _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result150)
and meta_row_field _loc =
  function
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result149)
  | `Or (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
               (meta_row_field _loc _a1))), (meta_row_field _loc _a2))
  | `TyVrn (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyVrn")), (meta_loc _loc _a0))),
          (meta_astring _loc _a1))
  | `TyVrnOf (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyVrnOf")), (meta_loc _loc _a0))),
               (meta_astring _loc _a1))), (meta_ctyp _loc _a2))
  | `Ctyp (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Ctyp")), (meta_loc _loc _a0))),
          (meta_ctyp _loc _a1))
and meta_tag_names _loc =
  function
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result148)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_tag_names _loc _a1))), (meta_tag_names _loc _a2))
  | `TyVrn (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "TyVrn")), (meta_loc _loc _a0))),
          (meta_astring _loc _a1))
and meta_typedecl _loc =
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
                              (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                    (meta_list meta_ctyp _loc _a2))),
               (meta_type_info _loc _a3))),
          (meta_list
             (fun _loc  (_a0,_a1)  ->
                `Tup
                  (_loc,
                    (`Com (_loc, (meta_ctyp _loc _a0), (meta_ctyp _loc _a1)))))
             _loc _a4))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_typedecl _loc _a1))), (meta_typedecl _loc _a2))
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result147)
and meta_type_info _loc =
  function
  | `TyMan (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "TyMan")), (meta_loc _loc _a0))),
                    (meta_ctyp _loc _a1))), (meta_private_flag _loc _a2))),
          (meta_type_repr _loc _a3))
  | `TyRepr (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyRepr")), (meta_loc _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_type_repr _loc _a2))
  | `TyEq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyEq")), (meta_loc _loc _a0))),
               (meta_private_flag _loc _a1))), (meta_ctyp _loc _a2))
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result146)
and meta_type_repr _loc =
  function
  | `Record (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
          (meta_name_ctyp _loc _a1))
  | `Sum (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sum")), (meta_loc _loc _a0))),
          (meta_ctyp _loc _a1))
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result145)
and meta_name_ctyp _loc =
  function
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_name_ctyp _loc _a1))), (meta_name_ctyp _loc _a2))
  | `TyCol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
               (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
  | `TyColMut (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyColMut")), (meta_loc _loc _a0))),
               (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result144)
and meta_or_ctyp _loc =
  function
  | `Or (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
               (meta_or_ctyp _loc _a1))), (meta_or_ctyp _loc _a2))
  | `TyCol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TyCol")), (meta_loc _loc _a0))),
               (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
  | `Of (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result143)
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result143)
and meta_of_ctyp _loc =
  function
  | `Of (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Of")), (meta_loc _loc _a0))),
               (meta_sid _loc _a1))), (meta_ctyp _loc _a2))
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result142)
  | #ant_nil as _a0 -> (meta_ant_nil _loc _a0 :>'result142)
and meta_patt _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result141)
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result141)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_patt _loc _a2))
  | `Vrn (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Com (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_patt _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_patt _loc _a2))
  | `Tup (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
          (meta_patt _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result141)
  | `Record (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
          (meta_rec_patt _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result141)
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result141)
  | `Alias (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Alias")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_alident _loc _a2))
  | `Array (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
          (meta_patt _loc _a1))
  | `Label (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_patt _loc _a2))
  | `PaOlbi (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "PaOlbi")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_patt _loc _a2))),
          (meta_meta_option meta_expr _loc _a3))
  | `Or (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_patt _loc _a2))
  | `PaRng (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "PaRng")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_patt _loc _a2))
  | `Constraint (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_ctyp _loc _a2))
  | `ClassPath (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ClassPath")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
  | `Lazy (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Lazy")), (meta_loc _loc _a0))),
          (meta_patt _loc _a1))
  | `ModuleUnpack (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "ModuleUnpack")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))),
          (meta_meta_option meta_ctyp _loc _a2))
and meta_rec_patt _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result140)
  | `RecBind (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_patt _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_rec_patt _loc _a1))), (meta_rec_patt _loc _a2))
  | #any as _a0 -> (meta_any _loc _a0 :>'result140)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result140)
and meta_expr _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result139)
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result139)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Vrn (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Com (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Tup (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result139)
  | `Record (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
          (meta_rec_expr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result139)
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result139)
  | `RecordWith (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "RecordWith")), (meta_loc _loc _a0))),
               (meta_rec_expr _loc _a1))), (meta_expr _loc _a2))
  | `Dot (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Dot")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `ArrayDot (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "ArrayDot")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Array (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | `ExAsf _a0 -> `App (_loc, (`Vrn (_loc, "ExAsf")), (meta_loc _loc _a0))
  | `ExAsr (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "ExAsr")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | `Assign (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Assign")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
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
                                   (meta_loc _loc _a0))),
                              (meta_alident _loc _a1))),
                         (meta_expr _loc _a2))), (meta_expr _loc _a3))),
               (meta_direction_flag _loc _a4))), (meta_expr _loc _a5))
  | `Fun (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Fun")), (meta_loc _loc _a0))),
          (meta_match_case _loc _a1))
  | `IfThenElse (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "IfThenElse")),
                         (meta_loc _loc _a0))), (meta_expr _loc _a1))),
               (meta_expr _loc _a2))), (meta_expr _loc _a3))
  | `IfThen (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "IfThen")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Label (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Label")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_expr _loc _a2))
  | `Lazy (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Lazy")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | `LetIn (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "LetIn")), (meta_loc _loc _a0))),
                    (meta_rec_flag _loc _a1))), (meta_binding _loc _a2))),
          (meta_expr _loc _a3))
  | `LetModule (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "LetModule")),
                         (meta_loc _loc _a0))), (meta_auident _loc _a1))),
               (meta_module_expr _loc _a2))), (meta_expr _loc _a3))
  | `Match (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Match")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_match_case _loc _a2))
  | `New (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "New")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
  | `Obj (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
  | `OptLabl (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "OptLabl")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_expr _loc _a2))
  | `OvrInst (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "OvrInst")), (meta_loc _loc _a0))),
          (meta_rec_expr _loc _a1))
  | `Seq (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Seq")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | `Send (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Send")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_alident _loc _a2))
  | `StringDot (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "StringDot")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `Try (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Try")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_match_case _loc _a2))
  | `Constraint (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_ctyp _loc _a2))
  | `Coercion (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "Coercion")), (meta_loc _loc _a0))),
                    (meta_expr _loc _a1))), (meta_ctyp _loc _a2))),
          (meta_ctyp _loc _a3))
  | `While (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "While")), (meta_loc _loc _a0))),
               (meta_expr _loc _a1))), (meta_expr _loc _a2))
  | `LetOpen (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "LetOpen")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_expr _loc _a2))
  | `LocalTypeFun (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "LocalTypeFun")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_expr _loc _a2))
  | `Package_expr (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Package_expr")), (meta_loc _loc _a0))),
          (meta_module_expr _loc _a1))
and meta_rec_expr _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_rec_expr _loc _a1))), (meta_rec_expr _loc _a2))
  | `RecBind (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_expr _loc _a2))
  | #any as _a0 -> (meta_any _loc _a0 :>'result138)
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result138)
and meta_module_type _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result137)
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result137)
  | `MtFun (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "MtFun")), (meta_loc _loc _a0))),
                    (meta_auident _loc _a1))), (meta_module_type _loc _a2))),
          (meta_module_type _loc _a3))
  | `Sig (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Sig")), (meta_loc _loc _a0))),
          (meta_sig_item _loc _a1))
  | `With (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "With")), (meta_loc _loc _a0))),
               (meta_module_type _loc _a1))), (meta_with_constr _loc _a2))
  | `ModuleTypeOf (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ModuleTypeOf")), (meta_loc _loc _a0))),
          (meta_module_expr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result137)
and meta_sig_item _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result136)
  | `Class (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Class")), (meta_loc _loc _a0))),
          (meta_class_type _loc _a1))
  | `ClassType (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ClassType")), (meta_loc _loc _a0))),
          (meta_class_type _loc _a1))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_sig_item _loc _a1))), (meta_sig_item _loc _a2))
  | `Directive (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Directive")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_expr _loc _a2))
  | `Exception (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Exception")), (meta_loc _loc _a0))),
          (meta_of_ctyp _loc _a1))
  | `External (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "External")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_ctyp _loc _a2))),
          (meta_meta_list meta_string _loc _a3))
  | `Include (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Include")), (meta_loc _loc _a0))),
          (meta_module_type _loc _a1))
  | `Module (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Module")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))), (meta_module_type _loc _a2))
  | `RecModule (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecModule")), (meta_loc _loc _a0))),
          (meta_module_binding _loc _a1))
  | `ModuleType (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "ModuleType")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))), (meta_module_type _loc _a2))
  | `Open (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Open")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
  | `Type (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Type")), (meta_loc _loc _a0))),
          (meta_typedecl _loc _a1))
  | `Val (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Val")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_ctyp _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result136)
and meta_with_constr _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `TypeEq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TypeEq")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `TypeEqPriv (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TypeEqPriv")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `ModuleEq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "ModuleEq")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_ident _loc _a2))
  | `TypeSubst (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "TypeSubst")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `ModuleSubst (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc, (`Vrn (_loc, "ModuleSubst")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_ident _loc _a2))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_with_constr _loc _a1))), (meta_with_constr _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result135)
and meta_binding _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_binding _loc _a1))), (meta_binding _loc _a2))
  | `Bind (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Bind")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_expr _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result134)
and meta_module_binding _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_module_binding _loc _a1))),
          (meta_module_binding _loc _a2))
  | `ModuleBind (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "ModuleBind")),
                         (meta_loc _loc _a0))), (meta_auident _loc _a1))),
               (meta_module_type _loc _a2))), (meta_module_expr _loc _a3))
  | `Constraint (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))), (meta_module_type _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result133)
and meta_match_case _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result132)
  | `Or (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Or")), (meta_loc _loc _a0))),
               (meta_match_case _loc _a1))), (meta_match_case _loc _a2))
  | `Case (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "Case")), (meta_loc _loc _a0))),
                    (meta_patt _loc _a1))), (meta_expr _loc _a2))),
          (meta_expr _loc _a3))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result132)
and meta_module_expr _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result131)
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result131)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_module_expr _loc _a1))), (meta_module_expr _loc _a2))
  | `Functor (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "Functor")), (meta_loc _loc _a0))),
                    (meta_auident _loc _a1))), (meta_module_type _loc _a2))),
          (meta_module_expr _loc _a3))
  | `Struct (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Struct")), (meta_loc _loc _a0))),
          (meta_str_item _loc _a1))
  | `Constraint (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Constraint")), (meta_loc _loc _a0))),
               (meta_module_expr _loc _a1))), (meta_module_type _loc _a2))
  | `PackageModule (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "PackageModule")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result131)
and meta_str_item _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `Class (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Class")), (meta_loc _loc _a0))),
          (meta_class_expr _loc _a1))
  | `ClassType (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "ClassType")), (meta_loc _loc _a0))),
          (meta_class_type _loc _a1))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_str_item _loc _a1))), (meta_str_item _loc _a2))
  | `Directive (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Directive")), (meta_loc _loc _a0))),
               (meta_alident _loc _a1))), (meta_expr _loc _a2))
  | `Exception (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Exception")), (meta_loc _loc _a0))),
          (meta_of_ctyp _loc _a1))
  | `StExp (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "StExp")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
  | `External (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "External")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_ctyp _loc _a2))),
          (meta_meta_list meta_string _loc _a3))
  | `Include (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Include")), (meta_loc _loc _a0))),
          (meta_module_expr _loc _a1))
  | `Module (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Module")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))), (meta_module_expr _loc _a2))
  | `RecModule (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "RecModule")), (meta_loc _loc _a0))),
          (meta_module_binding _loc _a1))
  | `ModuleType (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "ModuleType")), (meta_loc _loc _a0))),
               (meta_auident _loc _a1))), (meta_module_type _loc _a2))
  | `Open (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Open")), (meta_loc _loc _a0))),
          (meta_ident _loc _a1))
  | `Type (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Type")), (meta_loc _loc _a0))),
          (meta_typedecl _loc _a1))
  | `Value (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "Value")), (meta_loc _loc _a0))),
               (meta_rec_flag _loc _a1))), (meta_binding _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result130)
and meta_class_type _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `CtCon (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CtCon")), (meta_loc _loc _a0))),
                    (meta_virtual_flag _loc _a1))), (meta_ident _loc _a2))),
          (meta_ctyp _loc _a3))
  | `CtFun (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CtFun")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_class_type _loc _a2))
  | `CtSig (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CtSig")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_class_sig_item _loc _a2))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
  | `CtCol (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CtCol")), (meta_loc _loc _a0))),
               (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
  | `CtEq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CtEq")), (meta_loc _loc _a0))),
               (meta_class_type _loc _a1))), (meta_class_type _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result129)
and meta_class_sig_item _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `Eq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_class_sig_item _loc _a1))),
          (meta_class_sig_item _loc _a2))
  | `SigInherit (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "SigInherit")), (meta_loc _loc _a0))),
          (meta_class_type _loc _a1))
  | `Method (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "Method")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_private_flag _loc _a2))),
          (meta_ctyp _loc _a3))
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
                              (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                    (meta_mutable_flag _loc _a2))),
               (meta_virtual_flag _loc _a3))), (meta_ctyp _loc _a4))
  | `CgVir (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CgVir")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_private_flag _loc _a2))),
          (meta_ctyp _loc _a3))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result128)
and meta_class_expr _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `CeApp (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CeApp")), (meta_loc _loc _a0))),
               (meta_class_expr _loc _a1))), (meta_expr _loc _a2))
  | `CeCon (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CeCon")), (meta_loc _loc _a0))),
                    (meta_virtual_flag _loc _a1))), (meta_ident _loc _a2))),
          (meta_ctyp _loc _a3))
  | `CeFun (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CeFun")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_class_expr _loc _a2))
  | `CeLet (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CeLet")), (meta_loc _loc _a0))),
                    (meta_rec_flag _loc _a1))), (meta_binding _loc _a2))),
          (meta_class_expr _loc _a3))
  | `Obj (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Obj")), (meta_loc _loc _a0))),
               (meta_patt _loc _a1))), (meta_class_str_item _loc _a2))
  | `CeTyc (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "CeTyc")), (meta_loc _loc _a0))),
               (meta_class_expr _loc _a1))), (meta_class_type _loc _a2))
  | `And (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "And")), (meta_loc _loc _a0))),
               (meta_class_expr _loc _a1))), (meta_class_expr _loc _a2))
  | `Eq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
               (meta_class_expr _loc _a1))), (meta_class_expr _loc _a2))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result127)
and meta_class_str_item _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_class_str_item _loc _a1))),
          (meta_class_str_item _loc _a2))
  | `Eq (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Eq")), (meta_loc _loc _a0))),
               (meta_ctyp _loc _a1))), (meta_ctyp _loc _a2))
  | `Inherit (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App
                       (_loc, (`Vrn (_loc, "Inherit")), (meta_loc _loc _a0))),
                    (meta_override_flag _loc _a1))),
               (meta_class_expr _loc _a2))),
          (meta_meta_option meta_alident _loc _a3))
  | `Initializer (_a0,_a1) ->
      `App
        (_loc,
          (`App (_loc, (`Vrn (_loc, "Initializer")), (meta_loc _loc _a0))),
          (meta_expr _loc _a1))
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
                                   (meta_loc _loc _a0))),
                              (meta_alident _loc _a1))),
                         (meta_override_flag _loc _a2))),
                    (meta_private_flag _loc _a3))), (meta_expr _loc _a4))),
          (meta_ctyp _loc _a5))
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
                              (meta_loc _loc _a0))), (meta_alident _loc _a1))),
                    (meta_override_flag _loc _a2))),
               (meta_mutable_flag _loc _a3))), (meta_expr _loc _a4))
  | `CrVir (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CrVir")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_private_flag _loc _a2))),
          (meta_ctyp _loc _a3))
  | `CrVvr (_a0,_a1,_a2,_a3) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App
                  (_loc,
                    (`App (_loc, (`Vrn (_loc, "CrVvr")), (meta_loc _loc _a0))),
                    (meta_alident _loc _a1))), (meta_mutable_flag _loc _a2))),
          (meta_ctyp _loc _a3))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result126)
let rec meta_ep _loc =
  function
  | #nil as _a0 -> (meta_nil _loc _a0 :>'result152)
  | #sid as _a0 -> (meta_sid _loc _a0 :>'result152)
  | `App (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "App")), (meta_loc _loc _a0))),
               (meta_ep _loc _a1))), (meta_ep _loc _a2))
  | `Vrn (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Vrn")), (meta_loc _loc _a0))),
          (meta_string _loc _a1))
  | `Com (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (meta_loc _loc _a0))),
               (meta_ep _loc _a1))), (meta_ep _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_ep _loc _a1))), (meta_ep _loc _a2))
  | `Tup (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Tup")), (meta_loc _loc _a0))),
          (meta_ep _loc _a1))
  | #any as _a0 -> (meta_any _loc _a0 :>'result152)
  | `Array (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Array")), (meta_loc _loc _a0))),
          (meta_ep _loc _a1))
  | `Record (_a0,_a1) ->
      `App
        (_loc, (`App (_loc, (`Vrn (_loc, "Record")), (meta_loc _loc _a0))),
          (meta_rec_bind _loc _a1))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result152)
  | #literal as _a0 -> (meta_literal _loc _a0 :>'result152)
and meta_rec_bind _loc =
  function
  | `Nil _a0 -> `App (_loc, (`Vrn (_loc, "Nil")), (meta_loc _loc _a0))
  | `RecBind (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc,
               (`App (_loc, (`Vrn (_loc, "RecBind")), (meta_loc _loc _a0))),
               (meta_ident _loc _a1))), (meta_ep _loc _a2))
  | `Sem (_a0,_a1,_a2) ->
      `App
        (_loc,
          (`App
             (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (meta_loc _loc _a0))),
               (meta_rec_bind _loc _a1))), (meta_rec_bind _loc _a2))
  | `Any _a0 -> `App (_loc, (`Vrn (_loc, "Any")), (meta_loc _loc _a0))
  | #ant as _a0 -> (meta_ant _loc _a0 :>'result151)