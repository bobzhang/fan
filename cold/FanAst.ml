include Ast
module type META_LOC =
  sig
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> patt
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> expr
  end
open FanUtil
open LibUtil
open StdLib
let loc_of_ctyp: ctyp -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_patt: patt -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_expr: expr -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_module_type: module_type -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_module_expr: module_expr -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_sig_item: sig_item -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_str_item: str_item -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_class_type: class_type -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_class_sig_item: class_sig_item -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_class_expr: class_expr -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_class_str_item: class_str_item -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_with_constr: with_constr -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_binding: binding -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_rec_binding: rec_binding -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_module_binding: module_binding -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_match_case: match_case -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let loc_of_ident: ident -> FanLoc.t =
  fun x  -> let open Obj in magic (field (field (repr x) 1) 0)
let safe_string_escaped s =
  if ((String.length s) > 2) && (((s.[0]) = '\\') && ((s.[1]) = '$'))
  then s
  else String.escaped s
let strip_loc_list f lst = List.map f lst
let _ = ()
class eq =
  object (self : 'self_type)
    inherit  eqbase
    method loc : loc -> loc -> 'result= fun a0  a1  -> self#fanloc_t a0 a1
    method ant : ant -> ant -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
    method literal : literal -> literal -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Chr (a0,a1),`Chr (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Int (a0,a1),`Int (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Int32 (a0,a1),`Int32 (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Int64 (a0,a1),`Int64 (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Flo (a0,a1),`Flo (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`NativeInt (a0,a1),`NativeInt (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Str (a0,a1),`Str (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (_,_) -> false
    method rec_flag : rec_flag -> rec_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Recursive a0,`Recursive b0) -> self#loc a0 b0
        | (`ReNil a0,`ReNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method direction_flag : direction_flag -> direction_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`To a0,`To b0) -> self#loc a0 b0
        | (`Downto a0,`Downto b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method mutable_flag : mutable_flag -> mutable_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Mutable a0,`Mutable b0) -> self#loc a0 b0
        | (`MuNil a0,`MuNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method private_flag : private_flag -> private_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Private a0,`Private b0) -> self#loc a0 b0
        | (`PrNil a0,`PrNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method virtual_flag : virtual_flag -> virtual_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Virtual a0,`Virtual b0) -> self#loc a0 b0
        | (`ViNil a0,`ViNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method override_flag : override_flag -> override_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Override a0,`Override b0) -> self#loc a0 b0
        | (`OvNil a0,`OvNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method row_var_flag : row_var_flag -> row_var_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`RowVar a0,`RowVar b0) -> self#loc a0 b0
        | (`RvNil a0,`RvNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method position_flag : position_flag -> position_flag -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Positive a0,`Positive b0) -> self#loc a0 b0
        | (`Negative a0,`Negative b0) -> self#loc a0 b0
        | (`Normal a0,`Normal b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method meta_bool : meta_bool -> meta_bool -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`True a0,`True b0) -> self#loc a0 b0
        | (`False a0,`False b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'result) ->
          'all_a0 meta_option -> 'all_a0 meta_option -> 'result=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`None a0,`None b0) -> self#loc a0 b0
        | (`Some a0,`Some b0) -> mf_a self a0 b0
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (_,_) -> false
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'result) ->
          'all_a0 meta_list -> 'all_a0 meta_list -> 'result=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`LNil a0,`LNil b0) -> self#loc a0 b0
        | (`LCons (a0,a1),`LCons (b0,b1)) ->
            (mf_a self a0 b0) && (self#meta_list mf_a a1 b1)
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (_,_) -> false
    method alident : alident -> alident -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Lid (a0,a1),`Lid (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method auident : auident -> auident -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Uid (a0,a1),`Uid (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method aident : aident -> aident -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | ((#alident as a0),(#alident as b0)) ->
            (self#alident a0 b0 :>'result)
        | ((#auident as a0),(#auident as b0)) ->
            (self#auident a0 b0 :>'result)
        | (_,_) -> false
    method astring : astring -> astring -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`C (a0,a1),`C (b0,b1)) -> (self#loc a0 b0) && (self#string a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method ident : ident -> ident -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`IdAcc (a0,a1,a2),`IdAcc (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#ident a2 b2)
        | (`IdApp (a0,a1,a2),`IdApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#ident a2 b2)
        | ((#alident as a0),(#alident as b0)) ->
            (self#alident a0 b0 :>'result)
        | ((#auident as a0),(#auident as b0)) ->
            (self#auident a0 b0 :>'result)
        | (_,_) -> false
    method ctyp : ctyp -> ctyp -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Any a0,`Any b0) -> self#loc a0 b0
        | (`TyApp (a0,a1,a2),`TyApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyArr (a0,a1,a2),`TyArr (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`ClassPath (a0,a1),`ClassPath (b0,b1)) ->
            (self#loc a0 b0) && (self#ident a1 b1)
        | (`TyLab (a0,a1,a2),`TyLab (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#ctyp a2 b2)
        | (`Id (a0,a1),`Id (b0,b1)) -> (self#loc a0 b0) && (self#ident a1 b1)
        | (`TyMan (a0,a1,a2),`TyMan (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyDcl (a0,a1,a2,a3,a4),`TyDcl (b0,b1,b2,b3,b4)) ->
            ((((self#loc a0 b0) && (self#alident a1 b1)) &&
                (self#list (fun self  -> self#ctyp) a2 b2))
               && (self#ctyp a3 b3))
              &&
              (self#list
                 (fun self  a0  b0  ->
                    match (a0, b0) with
                    | ((a0,a1),(b0,b1)) ->
                        (self#ctyp a0 b0) && (self#ctyp a1 b1)) a4 b4)
        | (`TyObj (a0,a1,a2),`TyObj (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) &&
              (self#row_var_flag a2 b2)
        | (`TyOlb (a0,a1,a2),`TyOlb (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#ctyp a2 b2)
        | (`TyPol (a0,a1,a2),`TyPol (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyTypePol (a0,a1,a2),`TyTypePol (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Quote (a0,a1,a2),`Quote (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#position_flag a1 b1)) &&
              (self#meta_option (fun self  -> self#alident) a2 b2)
        | (`TyRec (a0,a1),`TyRec (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`TyCol (a0,a1,a2),`TyCol (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TySem (a0,a1,a2),`TySem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Com (a0,a1,a2),`Com (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Sum (a0,a1),`Sum (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Of (a0,a1,a2),`Of (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Or (a0,a1,a2),`Or (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Private (a0,a1),`Private (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Mutable (a0,a1),`Mutable (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Tup (a0,a1),`Tup (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Sta (a0,a1,a2),`Sta (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyVrn (a0,a1),`TyVrn (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`TyVrnEq (a0,a1),`TyVrnEq (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`TyVrnSup (a0,a1),`TyVrnSup (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`TyVrnInf (a0,a1),`TyVrnInf (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`TyVrnInfSup (a0,a1,a2),`TyVrnInfSup (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyAmp (a0,a1,a2),`TyAmp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`TyOfAmp (a0,a1,a2),`TyOfAmp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Package (a0,a1),`Package (b0,b1)) ->
            (self#loc a0 b0) && (self#module_type a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method patt : patt -> patt -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) -> (self#loc a0 b0) && (self#ident a1 b1)
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#alident a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (`Any a0,`Any b0) -> self#loc a0 b0
        | (`PaApp (a0,a1,a2),`PaApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#patt a2 b2)
        | (`Array (a0,a1),`Array (b0,b1)) ->
            (self#loc a0 b0) && (self#patt a1 b1)
        | (`PaCom (a0,a1,a2),`PaCom (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#patt a2 b2)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#patt a2 b2)
        | ((#literal as a0),(#literal as b0)) ->
            (self#literal a0 b0 :>'result)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#patt a2 b2)
        | (`PaOlbi (a0,a1,a2,a3),`PaOlbi (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) && (self#patt a2 b2))
              && (self#meta_option (fun self  -> self#expr) a3 b3)
        | (`PaOrp (a0,a1,a2),`PaOrp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#patt a2 b2)
        | (`PaRng (a0,a1,a2),`PaRng (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#patt a2 b2)
        | (`PaRec (a0,a1),`PaRec (b0,b1)) ->
            (self#loc a0 b0) && (self#patt a1 b1)
        | (`PaEq (a0,a1,a2),`PaEq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#patt a2 b2)
        | (`PaTup (a0,a1),`PaTup (b0,b1)) ->
            (self#loc a0 b0) && (self#patt a1 b1)
        | (`PaTyc (a0,a1,a2),`PaTyc (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#ctyp a2 b2)
        | (`PaTyp (a0,a1),`PaTyp (b0,b1)) ->
            (self#loc a0 b0) && (self#ident a1 b1)
        | (`PaVrn (a0,a1),`PaVrn (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            (self#loc a0 b0) && (self#patt a1 b1)
        | (`ModuleUnpack (a0,a1,a2),`ModuleUnpack (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#meta_option (fun self  -> self#ctyp) a2 b2)
        | (_,_) -> false
    method expr : expr -> expr -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) -> (self#loc a0 b0) && (self#ident a1 b1)
        | (`ExAcc (a0,a1,a2),`ExAcc (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (`ExApp (a0,a1,a2),`ExApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`ExAre (a0,a1,a2),`ExAre (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`Array (a0,a1),`Array (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`ExAsf a0,`ExAsf b0) -> self#loc a0 b0
        | (`ExAsr (a0,a1),`ExAsr (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`ExAss (a0,a1,a2),`ExAss (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`For (a0,a1,a2,a3,a4,a5),`For (b0,b1,b2,b3,b4,b5)) ->
            (((((self#loc a0 b0) && (self#alident a1 b1)) &&
                 (self#expr a2 b2))
                && (self#expr a3 b3))
               && (self#direction_flag a4 b4))
              && (self#expr a5 b5)
        | (`Fun (a0,a1),`Fun (b0,b1)) ->
            (self#loc a0 b0) && (self#match_case a1 b1)
        | (`IfThenElse (a0,a1,a2,a3),`IfThenElse (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)) &&
              (self#expr a3 b3)
        | ((#literal as a0),(#literal as b0)) ->
            (self#literal a0 b0 :>'result)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#expr a2 b2)
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`LetIn (a0,a1,a2,a3),`LetIn (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#rec_flag a1 b1)) &&
               (self#binding a2 b2))
              && (self#expr a3 b3)
        | (`LetModule (a0,a1,a2,a3),`LetModule (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#auident a1 b1)) &&
               (self#module_expr a2 b2))
              && (self#expr a3 b3)
        | (`Match (a0,a1,a2),`Match (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) &&
              (self#match_case a2 b2)
        | (`New (a0,a1),`New (b0,b1)) ->
            (self#loc a0 b0) && (self#ident a1 b1)
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) &&
              (self#class_str_item a2 b2)
        | (`OptLabl (a0,a1,a2),`OptLabl (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#expr a2 b2)
        | (`OvrInst (a0,a1),`OvrInst (b0,b1)) ->
            (self#loc a0 b0) && (self#rec_binding a1 b1)
        | (`Record (a0,a1,a2),`Record (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#rec_binding a1 b1)) &&
              (self#expr a2 b2)
        | (`Seq (a0,a1),`Seq (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`Send (a0,a1,a2),`Send (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#alident a2 b2)
        | (`StringDot (a0,a1,a2),`StringDot (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`Try (a0,a1,a2),`Try (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) &&
              (self#match_case a2 b2)
        | (`ExTup (a0,a1),`ExTup (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`ExCom (a0,a1,a2),`ExCom (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`Constraint_exp (a0,a1,a2),`Constraint_exp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#ctyp a2 b2)
        | (`ExCoe (a0,a1,a2,a3),`ExCoe (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#expr a1 b1)) && (self#ctyp a2 b2)) &&
              (self#ctyp a3 b3)
        | (`ExVrn (a0,a1),`ExVrn (b0,b1)) ->
            (self#loc a0 b0) && (self#string a1 b1)
        | (`While (a0,a1,a2),`While (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#expr a1 b1)) && (self#expr a2 b2)
        | (`Let_open (a0,a1,a2),`Let_open (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#expr a2 b2)
        | (`LocalTypeFun (a0,a1,a2),`LocalTypeFun (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#expr a2 b2)
        | (`Package_expr (a0,a1),`Package_expr (b0,b1)) ->
            (self#loc a0 b0) && (self#module_expr a1 b1)
        | (_,_) -> false
    method module_type : module_type -> module_type -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) -> (self#loc a0 b0) && (self#ident a1 b1)
        | (`MtFun (a0,a1,a2,a3),`MtFun (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#auident a1 b1)) &&
               (self#module_type a2 b2))
              && (self#module_type a3 b3)
        | (`Sig (a0,a1),`Sig (b0,b1)) ->
            (self#loc a0 b0) && (self#sig_item a1 b1)
        | (`MtWit (a0,a1,a2),`MtWit (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#module_type a1 b1)) &&
              (self#with_constr a2 b2)
        | (`Of (a0,a1),`Of (b0,b1)) ->
            (self#loc a0 b0) && (self#module_expr a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method sig_item : sig_item -> sig_item -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            (self#loc a0 b0) && (self#class_type a1 b1)
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            (self#loc a0 b0) && (self#class_type a1 b1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#sig_item a1 b1)) &&
              (self#sig_item a2 b2)
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#expr a2 b2)
        | (`Exception (a0,a1),`Exception (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) && (self#ctyp a2 b2))
              && (self#meta_list (fun self  -> self#string) a3 b3)
        | (`Include (a0,a1),`Include (b0,b1)) ->
            (self#loc a0 b0) && (self#module_type a1 b1)
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#module_type a2 b2)
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            (self#loc a0 b0) && (self#module_binding a1 b1)
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#module_type a2 b2)
        | (`Open (a0,a1),`Open (b0,b1)) ->
            (self#loc a0 b0) && (self#ident a1 b1)
        | (`Type (a0,a1),`Type (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Val (a0,a1,a2),`Val (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#ctyp a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method with_constr : with_constr -> with_constr -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`TypeEq (a0,a1,a2),`TypeEq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`ModuleEq (a0,a1,a2),`ModuleEq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#ident a2 b2)
        | (`TypeSubst (a0,a1,a2),`TypeSubst (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`ModuleSubst (a0,a1,a2),`ModuleSubst (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#ident a2 b2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#with_constr a1 b1)) &&
              (self#with_constr a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method binding : binding -> binding -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#binding a1 b1)) &&
              (self#binding a2 b2)
        | (`Bind (a0,a1,a2),`Bind (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) && (self#expr a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method rec_binding : rec_binding -> rec_binding -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#rec_binding a1 b1)) &&
              (self#rec_binding a2 b2)
        | (`RecBind (a0,a1,a2),`RecBind (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ident a1 b1)) && (self#expr a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method module_binding : module_binding -> module_binding -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#module_binding a1 b1)) &&
              (self#module_binding a2 b2)
        | (`ModuleBind (a0,a1,a2,a3),`ModuleBind (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#auident a1 b1)) &&
               (self#module_type a2 b2))
              && (self#module_expr a3 b3)
        | (`ModuleConstraint (a0,a1,a2),`ModuleConstraint (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#module_type a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method match_case : match_case -> match_case -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`McOr (a0,a1,a2),`McOr (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#match_case a1 b1)) &&
              (self#match_case a2 b2)
        | (`Case (a0,a1,a2,a3),`Case (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#patt a1 b1)) && (self#expr a2 b2)) &&
              (self#expr a3 b3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method module_expr : module_expr -> module_expr -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) -> (self#loc a0 b0) && (self#ident a1 b1)
        | (`MeApp (a0,a1,a2),`MeApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#module_expr a1 b1)) &&
              (self#module_expr a2 b2)
        | (`Functor (a0,a1,a2,a3),`Functor (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#auident a1 b1)) &&
               (self#module_type a2 b2))
              && (self#module_expr a3 b3)
        | (`Struct (a0,a1),`Struct (b0,b1)) ->
            (self#loc a0 b0) && (self#str_item a1 b1)
        | (`ModuleExprConstraint (a0,a1,a2),`ModuleExprConstraint (b0,b1,b2))
            ->
            ((self#loc a0 b0) && (self#module_expr a1 b1)) &&
              (self#module_type a2 b2)
        | (`PackageModule (a0,a1),`PackageModule (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method str_item : str_item -> str_item -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            (self#loc a0 b0) && (self#class_expr a1 b1)
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            (self#loc a0 b0) && (self#class_type a1 b1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#str_item a1 b1)) &&
              (self#str_item a2 b2)
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#alident a1 b1)) && (self#expr a2 b2)
        | (`Exception (a0,a1,a2),`Exception (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) &&
              (self#meta_option (fun self  -> self#ident) a2 b2)
        | (`StExp (a0,a1),`StExp (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) && (self#ctyp a2 b2))
              && (self#meta_list (fun self  -> self#string) a3 b3)
        | (`Include (a0,a1),`Include (b0,b1)) ->
            (self#loc a0 b0) && (self#module_expr a1 b1)
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#module_expr a2 b2)
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            (self#loc a0 b0) && (self#module_binding a1 b1)
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#auident a1 b1)) &&
              (self#module_type a2 b2)
        | (`Open (a0,a1),`Open (b0,b1)) ->
            (self#loc a0 b0) && (self#ident a1 b1)
        | (`Type (a0,a1),`Type (b0,b1)) ->
            (self#loc a0 b0) && (self#ctyp a1 b1)
        | (`Value (a0,a1,a2),`Value (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#rec_flag a1 b1)) &&
              (self#binding a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method class_type : class_type -> class_type -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CtCon (a0,a1,a2,a3),`CtCon (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#virtual_flag a1 b1)) &&
               (self#ident a2 b2))
              && (self#ctyp a3 b3)
        | (`CtFun (a0,a1,a2),`CtFun (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) &&
              (self#class_type a2 b2)
        | (`CtSig (a0,a1,a2),`CtSig (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) &&
              (self#class_sig_item a2 b2)
        | (`CtAnd (a0,a1,a2),`CtAnd (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_type a1 b1)) &&
              (self#class_type a2 b2)
        | (`CtCol (a0,a1,a2),`CtCol (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_type a1 b1)) &&
              (self#class_type a2 b2)
        | (`CtEq (a0,a1,a2),`CtEq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_type a1 b1)) &&
              (self#class_type a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method class_sig_item : class_sig_item -> class_sig_item -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_sig_item a1 b1)) &&
              (self#class_sig_item a2 b2)
        | (`Inherit (a0,a1),`Inherit (b0,b1)) ->
            (self#loc a0 b0) && (self#class_type a1 b1)
        | (`Method (a0,a1,a2,a3),`Method (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) &&
               (self#private_flag a2 b2))
              && (self#ctyp a3 b3)
        | (`CgVal (a0,a1,a2,a3,a4),`CgVal (b0,b1,b2,b3,b4)) ->
            ((((self#loc a0 b0) && (self#alident a1 b1)) &&
                (self#mutable_flag a2 b2))
               && (self#virtual_flag a3 b3))
              && (self#ctyp a4 b4)
        | (`CgVir (a0,a1,a2,a3),`CgVir (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) &&
               (self#private_flag a2 b2))
              && (self#ctyp a3 b3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method class_expr : class_expr -> class_expr -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CeApp (a0,a1,a2),`CeApp (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_expr a1 b1)) &&
              (self#expr a2 b2)
        | (`CeCon (a0,a1,a2,a3),`CeCon (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#virtual_flag a1 b1)) &&
               (self#ident a2 b2))
              && (self#ctyp a3 b3)
        | (`CeFun (a0,a1,a2),`CeFun (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) &&
              (self#class_expr a2 b2)
        | (`CeLet (a0,a1,a2,a3),`CeLet (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#rec_flag a1 b1)) &&
               (self#binding a2 b2))
              && (self#class_expr a3 b3)
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#patt a1 b1)) &&
              (self#class_str_item a2 b2)
        | (`CeTyc (a0,a1,a2),`CeTyc (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_expr a1 b1)) &&
              (self#class_type a2 b2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_expr a1 b1)) &&
              (self#class_expr a2 b2)
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_expr a1 b1)) &&
              (self#class_expr a2 b2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method class_str_item : class_str_item -> class_str_item -> 'result=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CrSem (a0,a1,a2),`CrSem (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#class_str_item a1 b1)) &&
              (self#class_str_item a2 b2)
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            ((self#loc a0 b0) && (self#ctyp a1 b1)) && (self#ctyp a2 b2)
        | (`Inherit (a0,a1,a2,a3),`Inherit (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#override_flag a1 b1)) &&
               (self#class_expr a2 b2))
              && (self#meta_option (fun self  -> self#alident) a3 b3)
        | (`Initializer (a0,a1),`Initializer (b0,b1)) ->
            (self#loc a0 b0) && (self#expr a1 b1)
        | (`CrMth (a0,a1,a2,a3,a4,a5),`CrMth (b0,b1,b2,b3,b4,b5)) ->
            (((((self#loc a0 b0) && (self#alident a1 b1)) &&
                 (self#override_flag a2 b2))
                && (self#private_flag a3 b3))
               && (self#expr a4 b4))
              && (self#ctyp a5 b5)
        | (`CrVal (a0,a1,a2,a3,a4),`CrVal (b0,b1,b2,b3,b4)) ->
            ((((self#loc a0 b0) && (self#alident a1 b1)) &&
                (self#override_flag a2 b2))
               && (self#mutable_flag a3 b3))
              && (self#expr a4 b4)
        | (`CrVir (a0,a1,a2,a3),`CrVir (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) &&
               (self#private_flag a2 b2))
              && (self#ctyp a3 b3)
        | (`CrVvr (a0,a1,a2,a3),`CrVvr (b0,b1,b2,b3)) ->
            (((self#loc a0 b0) && (self#alident a1 b1)) &&
               (self#mutable_flag a2 b2))
              && (self#ctyp a3 b3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'result)
        | (_,_) -> false
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'result= self#unknown
  end
class map =
  object (self : 'self_type)
    inherit  mapbase
    method loc : loc -> loc= fun a0  -> self#fanloc_t a0
    method ant : ant -> ant=
      fun (`Ant (a0,a1))  ->
        let a0 = self#loc a0 in let a1 = self#string a1 in `Ant (a0, a1)
    method literal : literal -> literal=
      function
      | `Chr (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Chr (a0, a1)
      | `Int (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Int (a0, a1)
      | `Int32 (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Int32 (a0, a1)
      | `Int64 (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Int64 (a0, a1)
      | `Flo (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Flo (a0, a1)
      | `NativeInt (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#string a1 in `NativeInt (a0, a1)
      | `Str (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Str (a0, a1)
    method rec_flag : rec_flag -> rec_flag=
      function
      | `Recursive a0 -> let a0 = self#loc a0 in `Recursive a0
      | `ReNil a0 -> let a0 = self#loc a0 in `ReNil a0
      | #ant as a0 -> (self#ant a0 :>rec_flag)
    method direction_flag : direction_flag -> direction_flag=
      function
      | `To a0 -> let a0 = self#loc a0 in `To a0
      | `Downto a0 -> let a0 = self#loc a0 in `Downto a0
      | #ant as a0 -> (self#ant a0 :>direction_flag)
    method mutable_flag : mutable_flag -> mutable_flag=
      function
      | `Mutable a0 -> let a0 = self#loc a0 in `Mutable a0
      | `MuNil a0 -> let a0 = self#loc a0 in `MuNil a0
      | #ant as a0 -> (self#ant a0 :>mutable_flag)
    method private_flag : private_flag -> private_flag=
      function
      | `Private a0 -> let a0 = self#loc a0 in `Private a0
      | `PrNil a0 -> let a0 = self#loc a0 in `PrNil a0
      | #ant as a0 -> (self#ant a0 :>private_flag)
    method virtual_flag : virtual_flag -> virtual_flag=
      function
      | `Virtual a0 -> let a0 = self#loc a0 in `Virtual a0
      | `ViNil a0 -> let a0 = self#loc a0 in `ViNil a0
      | #ant as a0 -> (self#ant a0 :>virtual_flag)
    method override_flag : override_flag -> override_flag=
      function
      | `Override a0 -> let a0 = self#loc a0 in `Override a0
      | `OvNil a0 -> let a0 = self#loc a0 in `OvNil a0
      | #ant as a0 -> (self#ant a0 :>override_flag)
    method row_var_flag : row_var_flag -> row_var_flag=
      function
      | `RowVar a0 -> let a0 = self#loc a0 in `RowVar a0
      | `RvNil a0 -> let a0 = self#loc a0 in `RvNil a0
      | #ant as a0 -> (self#ant a0 :>row_var_flag)
    method position_flag : position_flag -> position_flag=
      function
      | `Positive a0 -> let a0 = self#loc a0 in `Positive a0
      | `Negative a0 -> let a0 = self#loc a0 in `Negative a0
      | `Normal a0 -> let a0 = self#loc a0 in `Normal a0
      | #ant as a0 -> (self#ant a0 :>position_flag)
    method meta_bool : meta_bool -> meta_bool=
      function
      | `True a0 -> let a0 = self#loc a0 in `True a0
      | `False a0 -> let a0 = self#loc a0 in `False a0
      | #ant as a0 -> (self#ant a0 :>meta_bool)
    method meta_option :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_option -> 'all_b0 meta_option=
      fun mf_a  ->
        function
        | `None a0 -> let a0 = self#loc a0 in `None a0
        | `Some a0 -> let a0 = mf_a self a0 in `Some a0
        | `Ant (a0,a1) ->
            let a0 = self#loc a0 in let a1 = self#string a1 in `Ant (a0, a1)
    method meta_list :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_list -> 'all_b0 meta_list=
      fun mf_a  ->
        function
        | `LNil a0 -> let a0 = self#loc a0 in `LNil a0
        | `LCons (a0,a1) ->
            let a0 = mf_a self a0 in
            let a1 = self#meta_list mf_a a1 in `LCons (a0, a1)
        | `Ant (a0,a1) ->
            let a0 = self#loc a0 in let a1 = self#string a1 in `Ant (a0, a1)
    method alident : alident -> alident=
      function
      | `Lid (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Lid (a0, a1)
      | #ant as a0 -> (self#ant a0 :>alident)
    method auident : auident -> auident=
      function
      | `Uid (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `Uid (a0, a1)
      | #ant as a0 -> (self#ant a0 :>auident)
    method aident : aident -> aident=
      function
      | #alident as a0 -> (self#alident a0 :>aident)
      | #auident as a0 -> (self#auident a0 :>aident)
    method astring : astring -> astring=
      function
      | `C (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `C (a0, a1)
      | #ant as a0 -> (self#ant a0 :>astring)
    method ident : ident -> ident=
      function
      | `IdAcc (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#ident a2 in `IdAcc (a0, a1, a2)
      | `IdApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#ident a2 in `IdApp (a0, a1, a2)
      | #alident as a0 -> (self#alident a0 :>ident)
      | #auident as a0 -> (self#auident a0 :>ident)
    method ctyp : ctyp -> ctyp=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Alias (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `Alias (a0, a1, a2)
      | `Any a0 -> let a0 = self#loc a0 in `Any a0
      | `TyApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyApp (a0, a1, a2)
      | `TyArr (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyArr (a0, a1, a2)
      | `ClassPath (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in `ClassPath (a0, a1)
      | `TyLab (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#ctyp a2 in `TyLab (a0, a1, a2)
      | `Id (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Id (a0, a1)
      | `TyMan (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyMan (a0, a1, a2)
      | `TyDcl (a0,a1,a2,a3,a4) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#list (fun self  -> self#ctyp) a2 in
          let a3 = self#ctyp a3 in
          let a4 =
            self#list
              (fun self  (a0,a1)  ->
                 let a0 = self#ctyp a0 in let a1 = self#ctyp a1 in (a0, a1))
              a4 in
          `TyDcl (a0, a1, a2, a3, a4)
      | `TyObj (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#row_var_flag a2 in `TyObj (a0, a1, a2)
      | `TyOlb (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#ctyp a2 in `TyOlb (a0, a1, a2)
      | `TyPol (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyPol (a0, a1, a2)
      | `TyTypePol (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyTypePol (a0, a1, a2)
      | `Quote (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#position_flag a1 in
          let a2 = self#meta_option (fun self  -> self#alident) a2 in
          `Quote (a0, a1, a2)
      | `TyRec (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `TyRec (a0, a1)
      | `TyCol (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyCol (a0, a1, a2)
      | `TySem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TySem (a0, a1, a2)
      | `Com (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Com (a0, a1, a2)
      | `Sum (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Sum (a0, a1)
      | `Of (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Of (a0, a1, a2)
      | `And (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `And (a0, a1, a2)
      | `Or (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Or (a0, a1, a2)
      | `Private (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Private (a0, a1)
      | `Mutable (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Mutable (a0, a1)
      | `Tup (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Tup (a0, a1)
      | `Sta (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Sta (a0, a1, a2)
      | `TyVrn (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `TyVrn (a0, a1)
      | `TyVrnEq (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `TyVrnEq (a0, a1)
      | `TyVrnSup (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `TyVrnSup (a0, a1)
      | `TyVrnInf (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `TyVrnInf (a0, a1)
      | `TyVrnInfSup (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyVrnInfSup (a0, a1, a2)
      | `TyAmp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyAmp (a0, a1, a2)
      | `TyOfAmp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TyOfAmp (a0, a1, a2)
      | `Package (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_type a1 in `Package (a0, a1)
      | #ant as a0 -> (self#ant a0 :>ctyp)
    method patt : patt -> patt=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Id (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Id (a0, a1)
      | `Alias (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#alident a2 in `Alias (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>patt)
      | `Any a0 -> let a0 = self#loc a0 in `Any a0
      | `PaApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#patt a2 in `PaApp (a0, a1, a2)
      | `Array (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#patt a1 in `Array (a0, a1)
      | `PaCom (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#patt a2 in `PaCom (a0, a1, a2)
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in let a2 = self#patt a2 in `Sem (a0, a1, a2)
      | #literal as a0 -> (self#literal a0 :>patt)
      | `Label (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#patt a2 in `Label (a0, a1, a2)
      | `PaOlbi (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#patt a2 in
          let a3 = self#meta_option (fun self  -> self#expr) a3 in
          `PaOlbi (a0, a1, a2, a3)
      | `PaOrp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#patt a2 in `PaOrp (a0, a1, a2)
      | `PaRng (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#patt a2 in `PaRng (a0, a1, a2)
      | `PaRec (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#patt a1 in `PaRec (a0, a1)
      | `PaEq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#patt a2 in `PaEq (a0, a1, a2)
      | `PaTup (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#patt a1 in `PaTup (a0, a1)
      | `PaTyc (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#ctyp a2 in `PaTyc (a0, a1, a2)
      | `PaTyp (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `PaTyp (a0, a1)
      | `PaVrn (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `PaVrn (a0, a1)
      | `Lazy (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#patt a1 in `Lazy (a0, a1)
      | `ModuleUnpack (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#meta_option (fun self  -> self#ctyp) a2 in
          `ModuleUnpack (a0, a1, a2)
    method expr : expr -> expr=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Id (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Id (a0, a1)
      | `ExAcc (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `ExAcc (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>expr)
      | `ExApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `ExApp (a0, a1, a2)
      | `ExAre (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `ExAre (a0, a1, a2)
      | `Array (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `Array (a0, a1)
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in let a2 = self#expr a2 in `Sem (a0, a1, a2)
      | `ExAsf a0 -> let a0 = self#loc a0 in `ExAsf a0
      | `ExAsr (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `ExAsr (a0, a1)
      | `ExAss (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `ExAss (a0, a1, a2)
      | `For (a0,a1,a2,a3,a4,a5) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in
          let a3 = self#expr a3 in
          let a4 = self#direction_flag a4 in
          let a5 = self#expr a5 in `For (a0, a1, a2, a3, a4, a5)
      | `Fun (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#match_case a1 in `Fun (a0, a1)
      | `IfThenElse (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in
          let a3 = self#expr a3 in `IfThenElse (a0, a1, a2, a3)
      | #literal as a0 -> (self#literal a0 :>expr)
      | `Label (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in `Label (a0, a1, a2)
      | `Lazy (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `Lazy (a0, a1)
      | `LetIn (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_flag a1 in
          let a2 = self#binding a2 in
          let a3 = self#expr a3 in `LetIn (a0, a1, a2, a3)
      | `LetModule (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_expr a2 in
          let a3 = self#expr a3 in `LetModule (a0, a1, a2, a3)
      | `Match (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#match_case a2 in `Match (a0, a1, a2)
      | `New (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `New (a0, a1)
      | `Obj (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#class_str_item a2 in `Obj (a0, a1, a2)
      | `OptLabl (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in `OptLabl (a0, a1, a2)
      | `OvrInst (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_binding a1 in `OvrInst (a0, a1)
      | `Record (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_binding a1 in
          let a2 = self#expr a2 in `Record (a0, a1, a2)
      | `Seq (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `Seq (a0, a1)
      | `Send (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#alident a2 in `Send (a0, a1, a2)
      | `StringDot (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `StringDot (a0, a1, a2)
      | `Try (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#match_case a2 in `Try (a0, a1, a2)
      | `ExTup (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `ExTup (a0, a1)
      | `ExCom (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `ExCom (a0, a1, a2)
      | `Constraint_exp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#ctyp a2 in `Constraint_exp (a0, a1, a2)
      | `ExCoe (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#ctyp a2 in
          let a3 = self#ctyp a3 in `ExCoe (a0, a1, a2, a3)
      | `ExVrn (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#string a1 in `ExVrn (a0, a1)
      | `While (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in
          let a2 = self#expr a2 in `While (a0, a1, a2)
      | `Let_open (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#expr a2 in `Let_open (a0, a1, a2)
      | `LocalTypeFun (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in `LocalTypeFun (a0, a1, a2)
      | `Package_expr (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_expr a1 in `Package_expr (a0, a1)
    method module_type : module_type -> module_type=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Id (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Id (a0, a1)
      | `MtFun (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in
          let a3 = self#module_type a3 in `MtFun (a0, a1, a2, a3)
      | `Sig (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#sig_item a1 in `Sig (a0, a1)
      | `MtWit (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#module_type a1 in
          let a2 = self#with_constr a2 in `MtWit (a0, a1, a2)
      | `Of (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_expr a1 in `Of (a0, a1)
      | #ant as a0 -> (self#ant a0 :>module_type)
    method sig_item : sig_item -> sig_item=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Class (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in `Class (a0, a1)
      | `ClassType (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in `ClassType (a0, a1)
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#sig_item a1 in
          let a2 = self#sig_item a2 in `Sem (a0, a1, a2)
      | `Directive (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in `Directive (a0, a1, a2)
      | `Exception (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in `Exception (a0, a1)
      | `External (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#ctyp a2 in
          let a3 = self#meta_list (fun self  -> self#string) a3 in
          `External (a0, a1, a2, a3)
      | `Include (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_type a1 in `Include (a0, a1)
      | `Module (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in `Module (a0, a1, a2)
      | `RecModule (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_binding a1 in `RecModule (a0, a1)
      | `ModuleType (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in `ModuleType (a0, a1, a2)
      | `Open (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Open (a0, a1)
      | `Type (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Type (a0, a1)
      | `Val (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#ctyp a2 in `Val (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>sig_item)
    method with_constr : with_constr -> with_constr=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `TypeEq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TypeEq (a0, a1, a2)
      | `ModuleEq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#ident a2 in `ModuleEq (a0, a1, a2)
      | `TypeSubst (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#ctyp a2 in `TypeSubst (a0, a1, a2)
      | `ModuleSubst (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#ident a2 in `ModuleSubst (a0, a1, a2)
      | `And (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#with_constr a1 in
          let a2 = self#with_constr a2 in `And (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>with_constr)
    method binding : binding -> binding=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `And (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#binding a1 in
          let a2 = self#binding a2 in `And (a0, a1, a2)
      | `Bind (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#expr a2 in `Bind (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>binding)
    method rec_binding : rec_binding -> rec_binding=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_binding a1 in
          let a2 = self#rec_binding a2 in `Sem (a0, a1, a2)
      | `RecBind (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ident a1 in
          let a2 = self#expr a2 in `RecBind (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>rec_binding)
    method module_binding : module_binding -> module_binding=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `And (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#module_binding a1 in
          let a2 = self#module_binding a2 in `And (a0, a1, a2)
      | `ModuleBind (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in
          let a3 = self#module_expr a3 in `ModuleBind (a0, a1, a2, a3)
      | `ModuleConstraint (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in `ModuleConstraint (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>module_binding)
    method match_case : match_case -> match_case=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `McOr (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#match_case a1 in
          let a2 = self#match_case a2 in `McOr (a0, a1, a2)
      | `Case (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#expr a2 in
          let a3 = self#expr a3 in `Case (a0, a1, a2, a3)
      | #ant as a0 -> (self#ant a0 :>match_case)
    method module_expr : module_expr -> module_expr=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Id (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Id (a0, a1)
      | `MeApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#module_expr a1 in
          let a2 = self#module_expr a2 in `MeApp (a0, a1, a2)
      | `Functor (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in
          let a3 = self#module_expr a3 in `Functor (a0, a1, a2, a3)
      | `Struct (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#str_item a1 in `Struct (a0, a1)
      | `ModuleExprConstraint (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#module_expr a1 in
          let a2 = self#module_type a2 in `ModuleExprConstraint (a0, a1, a2)
      | `PackageModule (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in `PackageModule (a0, a1)
      | #ant as a0 -> (self#ant a0 :>module_expr)
    method str_item : str_item -> str_item=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Class (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#class_expr a1 in `Class (a0, a1)
      | `ClassType (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in `ClassType (a0, a1)
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#str_item a1 in
          let a2 = self#str_item a2 in `Sem (a0, a1, a2)
      | `Directive (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#expr a2 in `Directive (a0, a1, a2)
      | `Exception (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#meta_option (fun self  -> self#ident) a2 in
          `Exception (a0, a1, a2)
      | `StExp (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#expr a1 in `StExp (a0, a1)
      | `External (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#ctyp a2 in
          let a3 = self#meta_list (fun self  -> self#string) a3 in
          `External (a0, a1, a2, a3)
      | `Include (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_expr a1 in `Include (a0, a1)
      | `Module (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_expr a2 in `Module (a0, a1, a2)
      | `RecModule (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#module_binding a1 in `RecModule (a0, a1)
      | `ModuleType (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#auident a1 in
          let a2 = self#module_type a2 in `ModuleType (a0, a1, a2)
      | `Open (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ident a1 in `Open (a0, a1)
      | `Type (a0,a1) ->
          let a0 = self#loc a0 in let a1 = self#ctyp a1 in `Type (a0, a1)
      | `Value (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_flag a1 in
          let a2 = self#binding a2 in `Value (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>str_item)
    method class_type : class_type -> class_type=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `CtCon (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#virtual_flag a1 in
          let a2 = self#ident a2 in
          let a3 = self#ctyp a3 in `CtCon (a0, a1, a2, a3)
      | `CtFun (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#class_type a2 in `CtFun (a0, a1, a2)
      | `CtSig (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in
          let a2 = self#class_sig_item a2 in `CtSig (a0, a1, a2)
      | `CtAnd (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in
          let a2 = self#class_type a2 in `CtAnd (a0, a1, a2)
      | `CtCol (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in
          let a2 = self#class_type a2 in `CtCol (a0, a1, a2)
      | `CtEq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in
          let a2 = self#class_type a2 in `CtEq (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>class_type)
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `Eq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Eq (a0, a1, a2)
      | `Sem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_sig_item a1 in
          let a2 = self#class_sig_item a2 in `Sem (a0, a1, a2)
      | `Inherit (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#class_type a1 in `Inherit (a0, a1)
      | `Method (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#private_flag a2 in
          let a3 = self#ctyp a3 in `Method (a0, a1, a2, a3)
      | `CgVal (a0,a1,a2,a3,a4) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#mutable_flag a2 in
          let a3 = self#virtual_flag a3 in
          let a4 = self#ctyp a4 in `CgVal (a0, a1, a2, a3, a4)
      | `CgVir (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#private_flag a2 in
          let a3 = self#ctyp a3 in `CgVir (a0, a1, a2, a3)
      | #ant as a0 -> (self#ant a0 :>class_sig_item)
    method class_expr : class_expr -> class_expr=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `CeApp (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_expr a1 in
          let a2 = self#expr a2 in `CeApp (a0, a1, a2)
      | `CeCon (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#virtual_flag a1 in
          let a2 = self#ident a2 in
          let a3 = self#ctyp a3 in `CeCon (a0, a1, a2, a3)
      | `CeFun (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#class_expr a2 in `CeFun (a0, a1, a2)
      | `CeLet (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#rec_flag a1 in
          let a2 = self#binding a2 in
          let a3 = self#class_expr a3 in `CeLet (a0, a1, a2, a3)
      | `Obj (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#patt a1 in
          let a2 = self#class_str_item a2 in `Obj (a0, a1, a2)
      | `CeTyc (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_expr a1 in
          let a2 = self#class_type a2 in `CeTyc (a0, a1, a2)
      | `And (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_expr a1 in
          let a2 = self#class_expr a2 in `And (a0, a1, a2)
      | `Eq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_expr a1 in
          let a2 = self#class_expr a2 in `Eq (a0, a1, a2)
      | #ant as a0 -> (self#ant a0 :>class_expr)
    method class_str_item : class_str_item -> class_str_item=
      function
      | `Nil a0 -> let a0 = self#loc a0 in `Nil a0
      | `CrSem (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#class_str_item a1 in
          let a2 = self#class_str_item a2 in `CrSem (a0, a1, a2)
      | `Eq (a0,a1,a2) ->
          let a0 = self#loc a0 in
          let a1 = self#ctyp a1 in let a2 = self#ctyp a2 in `Eq (a0, a1, a2)
      | `Inherit (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#override_flag a1 in
          let a2 = self#class_expr a2 in
          let a3 = self#meta_option (fun self  -> self#alident) a3 in
          `Inherit (a0, a1, a2, a3)
      | `Initializer (a0,a1) ->
          let a0 = self#loc a0 in
          let a1 = self#expr a1 in `Initializer (a0, a1)
      | `CrMth (a0,a1,a2,a3,a4,a5) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#override_flag a2 in
          let a3 = self#private_flag a3 in
          let a4 = self#expr a4 in
          let a5 = self#ctyp a5 in `CrMth (a0, a1, a2, a3, a4, a5)
      | `CrVal (a0,a1,a2,a3,a4) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#override_flag a2 in
          let a3 = self#mutable_flag a3 in
          let a4 = self#expr a4 in `CrVal (a0, a1, a2, a3, a4)
      | `CrVir (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#private_flag a2 in
          let a3 = self#ctyp a3 in `CrVir (a0, a1, a2, a3)
      | `CrVvr (a0,a1,a2,a3) ->
          let a0 = self#loc a0 in
          let a1 = self#alident a1 in
          let a2 = self#mutable_flag a2 in
          let a3 = self#ctyp a3 in `CrVvr (a0, a1, a2, a3)
      | #ant as a0 -> (self#ant a0 :>class_str_item)
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
  end
class print =
  object (self : 'self_type)
    inherit  printbase
    method loc : 'fmt -> loc -> 'result= fun fmt  a0  -> self#fanloc_t fmt a0
    method ant : 'fmt -> ant -> 'result=
      fun fmt  (`Ant (a0,a1))  ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc a0 self#string a1
    method literal : 'fmt -> literal -> 'result=
      fun fmt  ->
        function
        | `Chr (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Int (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Int32 (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Int64 (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Flo (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" self#loc a0
              self#string a1
        | `NativeInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(`NativeInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Str (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" self#loc a0
              self#string a1
    method rec_flag : 'fmt -> rec_flag -> 'result=
      fun fmt  ->
        function
        | `Recursive a0 ->
            Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" self#loc a0
        | `ReNil a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method direction_flag : 'fmt -> direction_flag -> 'result=
      fun fmt  ->
        function
        | `To a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" self#loc a0
        | `Downto a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method mutable_flag : 'fmt -> mutable_flag -> 'result=
      fun fmt  ->
        function
        | `Mutable a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" self#loc a0
        | `MuNil a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method private_flag : 'fmt -> private_flag -> 'result=
      fun fmt  ->
        function
        | `Private a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]" self#loc a0
        | `PrNil a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method virtual_flag : 'fmt -> virtual_flag -> 'result=
      fun fmt  ->
        function
        | `Virtual a0 ->
            Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" self#loc a0
        | `ViNil a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method override_flag : 'fmt -> override_flag -> 'result=
      fun fmt  ->
        function
        | `Override a0 ->
            Format.fprintf fmt "@[<1>(`Override@ %a)@]" self#loc a0
        | `OvNil a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method row_var_flag : 'fmt -> row_var_flag -> 'result=
      fun fmt  ->
        function
        | `RowVar a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" self#loc a0
        | `RvNil a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method position_flag : 'fmt -> position_flag -> 'result=
      fun fmt  ->
        function
        | `Positive a0 ->
            Format.fprintf fmt "@[<1>(`Positive@ %a)@]" self#loc a0
        | `Negative a0 ->
            Format.fprintf fmt "@[<1>(`Negative@ %a)@]" self#loc a0
        | `Normal a0 -> Format.fprintf fmt "@[<1>(`Normal@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method meta_bool : 'fmt -> meta_bool -> 'result=
      fun fmt  ->
        function
        | `True a0 -> Format.fprintf fmt "@[<1>(`True@ %a)@]" self#loc a0
        | `False a0 -> Format.fprintf fmt "@[<1>(`False@ %a)@]" self#loc a0
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method meta_option :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_option -> 'result=
      fun mf_a  fmt  ->
        function
        | `None a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc a0
        | `Some a0 -> Format.fprintf fmt "@[<1>(`Some@ %a)@]" (mf_a self) a0
        | `Ant (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc a0
              self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_list -> 'result=
      fun mf_a  fmt  ->
        function
        | `LNil a0 -> Format.fprintf fmt "@[<1>(`LNil@ %a)@]" self#loc a0
        | `LCons (a0,a1) ->
            Format.fprintf fmt "@[<1>(`LCons@ %a@ %a)@]" (mf_a self) a0
              (self#meta_list mf_a) a1
        | `Ant (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" self#loc a0
              self#string a1
    method alident : 'fmt -> alident -> 'result=
      fun fmt  ->
        function
        | `Lid (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" self#loc a0
              self#string a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method auident : 'fmt -> auident -> 'result=
      fun fmt  ->
        function
        | `Uid (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" self#loc a0
              self#string a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method aident : 'fmt -> aident -> 'result=
      fun fmt  ->
        function
        | #alident as a0 -> (self#alident fmt a0 :>'result)
        | #auident as a0 -> (self#auident fmt a0 :>'result)
    method astring : 'fmt -> astring -> 'result=
      fun fmt  ->
        function
        | `C (a0,a1) ->
            Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" self#loc a0 self#string
              a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method ident : 'fmt -> ident -> 'result=
      fun fmt  ->
        function
        | `IdAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`IdAcc@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `IdApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`IdApp@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | #alident as a0 -> (self#alident fmt a0 :>'result)
        | #auident as a0 -> (self#auident fmt a0 :>'result)
    method ctyp : 'fmt -> ctyp -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Alias (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc a0
        | `TyApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyApp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyArr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyArr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `ClassPath (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `TyLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyLab@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#ctyp a2
        | `Id (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `TyMan (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyDcl (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc a0 self#alident a1
              (self#list (fun self  -> self#ctyp)) a2 self#ctyp a3
              (self#list
                 (fun self  fmt  (a0,a1)  ->
                    Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#ctyp a0
                      self#ctyp a1)) a4
        | `TyObj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#row_var_flag a2
        | `TyOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyOlb@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#ctyp a2
        | `TyPol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyTypePol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Quote (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" self#loc a0
              self#position_flag a1
              (self#meta_option (fun self  -> self#alident)) a2
        | `TyRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(`TyRec@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TySem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TySem@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Com (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Sum (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `Of (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `And (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Or (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Private (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Private@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `Mutable (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `Tup (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `Sta (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `TyVrnEq (a0,a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnEq@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnSup (a0,a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnSup@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnInf (a0,a1) ->
            Format.fprintf fmt "@[<1>(`TyVrnInf@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnInfSup (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyVrnInfSup@ %a@ %a@ %a)@]" self#loc
              a0 self#ctyp a1 self#ctyp a2
        | `TyAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyOfAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TyOfAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Package (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method patt : 'fmt -> patt -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `Alias (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#alident a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
        | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc a0
        | `PaApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaApp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `Array (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc a0
              self#patt a1
        | `PaCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaCom@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | #literal as a0 -> (self#literal fmt a0 :>'result)
        | `Label (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#patt a2
        | `PaOlbi (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`PaOlbi@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#patt a2
              (self#meta_option (fun self  -> self#expr)) a3
        | `PaOrp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaOrp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaRng (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(`PaRec@ %a@ %a)@]" self#loc a0
              self#patt a1
        | `PaEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#patt a2
        | `PaTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(`PaTup@ %a@ %a)@]" self#loc a0
              self#patt a1
        | `PaTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`PaTyc@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#ctyp a2
        | `PaTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(`PaTyp@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `PaVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(`PaVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `Lazy (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | `ModuleUnpack (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a@ %a)@]" self#loc
              a0 self#auident a1 (self#meta_option (fun self  -> self#ctyp))
              a2
    method expr : 'fmt -> expr -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `ExAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ExAcc@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
        | `ExApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ExApp@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExAre (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ExAre@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `Array (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" self#loc a0
              self#expr a1
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExAsf a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" self#loc a0
        | `ExAsr (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" self#loc a0
              self#expr a1
        | `ExAss (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ExAss@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `For (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#alident a1 self#expr a2 self#expr a3
              self#direction_flag a4 self#expr a5
        | `Fun (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" self#loc a0
              self#match_case a1
        | `IfThenElse (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#expr a1 self#expr a2 self#expr a3
        | #literal as a0 -> (self#literal fmt a0 :>'result)
        | `Label (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#expr a2
        | `Lazy (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `LetIn (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#expr a3
        | `LetModule (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" 
              self#loc a0 self#auident a1 self#module_expr a2 self#expr a3
        | `Match (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | `New (a0,a1) ->
            Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `Obj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | `OptLabl (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#expr a2
        | `OvrInst (a0,a1) ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" self#loc a0
              self#rec_binding a1
        | `Record (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Record@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#expr a2
        | `Seq (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `Send (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#alident a2
        | `StringDot (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `Try (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | `ExTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ExTup@ %a@ %a)@]" self#loc a0
              self#expr a1
        | `ExCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ExCom@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `Constraint_exp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Constraint_exp@ %a@ %a@ %a)@]"
              self#loc a0 self#expr a1 self#ctyp a2
        | `ExCoe (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`ExCoe@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#ctyp a2 self#ctyp a3
        | `ExVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ExVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `While (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `Let_open (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Let_open@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | `LocalTypeFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" self#loc
              a0 self#alident a1 self#expr a2
        | `Package_expr (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Package_expr@ %a@ %a)@]" self#loc a0
              self#module_expr a1
    method module_type : 'fmt -> module_type -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `MtFun (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`MtFun@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#auident a1 self#module_type a2 self#module_type a3
        | `Sig (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" self#loc a0
              self#sig_item a1
        | `MtWit (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`MtWit@ %a@ %a@ %a)@]" self#loc a0
              self#module_type a1 self#with_constr a2
        | `Of (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method sig_item : 'fmt -> sig_item -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Class (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `ClassType (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#sig_item a1 self#sig_item a2
        | `Directive (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#expr a2
        | `Exception (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `External (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#alident a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | `Include (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | `Module (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc a0
              self#auident a1 self#module_type a2
        | `RecModule (a0,a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | `ModuleType (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc a0
              self#auident a1 self#module_type a2
        | `Open (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `Type (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `Val (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#ctyp a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method with_constr : 'fmt -> with_constr -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `TypeEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `ModuleEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `TypeSubst (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `ModuleSubst (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" self#loc
              a0 self#ident a1 self#ident a2
        | `And (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc a0
              self#with_constr a1 self#with_constr a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method binding : 'fmt -> binding -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `And (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc a0
              self#binding a1 self#binding a2
        | `Bind (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method rec_binding : 'fmt -> rec_binding -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#rec_binding a2
        | `RecBind (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method module_binding : 'fmt -> module_binding -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `And (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc a0
              self#module_binding a1 self#module_binding a2
        | `ModuleBind (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#auident a1 self#module_type a2
              self#module_expr a3
        | `ModuleConstraint (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
              self#loc a0 self#auident a1 self#module_type a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method match_case : 'fmt -> match_case -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `McOr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`McOr@ %a@ %a@ %a)@]" self#loc a0
              self#match_case a1 self#match_case a2
        | `Case (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2 self#expr a3
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method module_expr : 'fmt -> module_expr -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `MeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`MeApp@ %a@ %a@ %a)@]" self#loc a0
              self#module_expr a1 self#module_expr a2
        | `Functor (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#auident a1 self#module_type a2 self#module_expr a3
        | `Struct (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" self#loc a0
              self#str_item a1
        | `ModuleExprConstraint (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleExprConstraint@ %a@ %a@ %a)@]"
              self#loc a0 self#module_expr a1 self#module_type a2
        | `PackageModule (a0,a1) ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" self#loc a0
              self#expr a1
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method str_item : 'fmt -> str_item -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Class (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" self#loc a0
              self#class_expr a1
        | `ClassType (a0,a1) ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#str_item a1 self#str_item a2
        | `Directive (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#expr a2
        | `Exception (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Exception@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 (self#meta_option (fun self  -> self#ident)) a2
        | `StExp (a0,a1) ->
            Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" self#loc a0
              self#expr a1
        | `External (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#alident a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | `Include (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | `Module (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" self#loc a0
              self#auident a1 self#module_expr a2
        | `RecModule (a0,a1) ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | `ModuleType (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" self#loc a0
              self#auident a1 self#module_type a2
        | `Open (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `Type (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `Value (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method class_type : 'fmt -> class_type -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `CtCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CtCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | `CtFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_type a2
        | `CtSig (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CtSig@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_sig_item a2
        | `CtAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CtAnd@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | `CtCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | `CtEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CtEq@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method class_sig_item : 'fmt -> class_sig_item -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Eq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Sem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" self#loc a0
              self#class_sig_item a1 self#class_sig_item a2
        | `Inherit (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `Method (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#private_flag a2 self#ctyp a3
        | `CgVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc a0 self#alident a1 self#mutable_flag a2
              self#virtual_flag a3 self#ctyp a4
        | `CgVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CgVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#private_flag a2 self#ctyp a3
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method class_expr : 'fmt -> class_expr -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `CeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#expr a2
        | `CeCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CeCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | `CeFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_expr a2
        | `CeLet (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CeLet@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#class_expr a3
        | `Obj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | `CeTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CeTyc@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_type a2
        | `And (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | `Eq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method class_str_item : 'fmt -> class_str_item -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `CrSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`CrSem@ %a@ %a@ %a)@]" self#loc a0
              self#class_str_item a1 self#class_str_item a2
        | `Eq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `Inherit (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#override_flag a1 self#class_expr a2
              (self#meta_option (fun self  -> self#alident)) a3
        | `Initializer (a0,a1) ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" self#loc a0
              self#expr a1
        | `CrMth (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#alident a1 self#override_flag a2
              self#private_flag a3 self#expr a4 self#ctyp a5
        | `CrVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" 
              self#loc a0 self#alident a1 self#override_flag a2
              self#mutable_flag a3 self#expr a4
        | `CrVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#private_flag a2 self#ctyp a3
        | `CrVvr (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#alident a1 self#mutable_flag a2 self#ctyp a3
        | #ant as a0 -> (self#ant fmt a0 :>'result)
    method fanloc_t : 'fmt -> FanLoc.t -> 'result= self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method loc : loc -> 'self_type= fun a0  -> self#fanloc_t a0
    method ant : ant -> 'self_type=
      fun (`Ant (a0,a1))  -> let self = self#loc a0 in self#string a1
    method literal : literal -> 'self_type=
      function
      | `Chr (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Int (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Int32 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Int64 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Flo (a0,a1) -> let self = self#loc a0 in self#string a1
      | `NativeInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Str (a0,a1) -> let self = self#loc a0 in self#string a1
    method rec_flag : rec_flag -> 'self_type=
      function
      | `Recursive a0 -> self#loc a0
      | `ReNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method direction_flag : direction_flag -> 'self_type=
      function
      | `To a0 -> self#loc a0
      | `Downto a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method mutable_flag : mutable_flag -> 'self_type=
      function
      | `Mutable a0 -> self#loc a0
      | `MuNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method private_flag : private_flag -> 'self_type=
      function
      | `Private a0 -> self#loc a0
      | `PrNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method virtual_flag : virtual_flag -> 'self_type=
      function
      | `Virtual a0 -> self#loc a0
      | `ViNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method override_flag : override_flag -> 'self_type=
      function
      | `Override a0 -> self#loc a0
      | `OvNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method row_var_flag : row_var_flag -> 'self_type=
      function
      | `RowVar a0 -> self#loc a0
      | `RvNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method position_flag : position_flag -> 'self_type=
      function
      | `Positive a0 -> self#loc a0
      | `Negative a0 -> self#loc a0
      | `Normal a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method meta_bool : meta_bool -> 'self_type=
      function
      | `True a0 -> self#loc a0
      | `False a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_option -> 'self_type=
      fun mf_a  ->
        function
        | `None a0 -> self#loc a0
        | `Some a0 -> mf_a self a0
        | `Ant (a0,a1) -> let self = self#loc a0 in self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_list -> 'self_type=
      fun mf_a  ->
        function
        | `LNil a0 -> self#loc a0
        | `LCons (a0,a1) -> let self = mf_a self a0 in self#meta_list mf_a a1
        | `Ant (a0,a1) -> let self = self#loc a0 in self#string a1
    method alident : alident -> 'self_type=
      function
      | `Lid (a0,a1) -> let self = self#loc a0 in self#string a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method auident : auident -> 'self_type=
      function
      | `Uid (a0,a1) -> let self = self#loc a0 in self#string a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method aident : aident -> 'self_type=
      function
      | #alident as a0 -> (self#alident a0 :>'self_type)
      | #auident as a0 -> (self#auident a0 :>'self_type)
    method astring : astring -> 'self_type=
      function
      | `C (a0,a1) -> let self = self#loc a0 in self#string a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method ident : ident -> 'self_type=
      function
      | `IdAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `IdApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | #alident as a0 -> (self#alident a0 :>'self_type)
      | #auident as a0 -> (self#auident a0 :>'self_type)
    method ctyp : ctyp -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Alias (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Any a0 -> self#loc a0
      | `TyApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyArr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `ClassPath (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `TyLab (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#ctyp a2
      | `Id (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `TyMan (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyDcl (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#list (fun self  -> self#ctyp) a2 in
          let self = self#ctyp a3 in
          self#list
            (fun self  (a0,a1)  -> let self = self#ctyp a0 in self#ctyp a1)
            a4
      | `TyObj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#row_var_flag a2
      | `TyOlb (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#ctyp a2
      | `TyPol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyTypePol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Quote (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#position_flag a1 in
          self#meta_option (fun self  -> self#alident) a2
      | `TyRec (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyCol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TySem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Com (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Sum (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Of (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `And (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Or (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Private (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Mutable (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Tup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Sta (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `TyVrnEq (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnSup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnInf (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnInfSup (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyOfAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Package (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method patt : patt -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `Alias (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#alident a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
      | `Any a0 -> self#loc a0
      | `PaApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `Array (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | #literal as a0 -> (self#literal a0 :>'self_type)
      | `Label (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#patt a2
      | `PaOlbi (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#patt a2 in
          self#meta_option (fun self  -> self#expr) a3
      | `PaOrp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaRng (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaRec (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#patt a2
      | `PaTup (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaTyc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#ctyp a2
      | `PaTyp (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `PaVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `Lazy (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `ModuleUnpack (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in
          self#meta_option (fun self  -> self#ctyp) a2
    method expr : expr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `ExAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
      | `ExApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExAre (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `Array (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExAsf a0 -> self#loc a0
      | `ExAsr (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExAss (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `For (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#expr a2 in
          let self = self#expr a3 in
          let self = self#direction_flag a4 in self#expr a5
      | `Fun (a0,a1) -> let self = self#loc a0 in self#match_case a1
      | `IfThenElse (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#expr a2 in self#expr a3
      | #literal as a0 -> (self#literal a0 :>'self_type)
      | `Label (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#expr a2
      | `Lazy (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `LetIn (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in
          let self = self#binding a2 in self#expr a3
      | `LetModule (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#auident a1 in
          let self = self#module_expr a2 in self#expr a3
      | `Match (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | `New (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `Obj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | `OptLabl (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#expr a2
      | `OvrInst (a0,a1) -> let self = self#loc a0 in self#rec_binding a1
      | `Record (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#expr a2
      | `Seq (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `Send (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#alident a2
      | `StringDot (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `Try (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | `ExTup (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `Constraint_exp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#ctyp a2
      | `ExCoe (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#ctyp a2 in self#ctyp a3
      | `ExVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `While (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `Let_open (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | `LocalTypeFun (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#expr a2
      | `Package_expr (a0,a1) ->
          let self = self#loc a0 in self#module_expr a1
    method module_type : module_type -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `MtFun (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#auident a1 in
          let self = self#module_type a2 in self#module_type a3
      | `Sig (a0,a1) -> let self = self#loc a0 in self#sig_item a1
      | `MtWit (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_type a1 in self#with_constr a2
      | `Of (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method sig_item : sig_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Class (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `ClassType (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#sig_item a1 in self#sig_item a2
      | `Directive (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#expr a2
      | `Exception (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `External (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | `Include (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | `Module (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in self#module_type a2
      | `RecModule (a0,a1) ->
          let self = self#loc a0 in self#module_binding a1
      | `ModuleType (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in self#module_type a2
      | `Open (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `Type (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Val (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#ctyp a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method with_constr : with_constr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `TypeEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `ModuleEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `TypeSubst (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `ModuleSubst (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `And (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#with_constr a1 in self#with_constr a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method binding : binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `And (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#binding a1 in self#binding a2
      | `Bind (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#expr a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method rec_binding : rec_binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#rec_binding a2
      | `RecBind (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method module_binding : module_binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `And (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_binding a1 in self#module_binding a2
      | `ModuleBind (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#auident a1 in
          let self = self#module_type a2 in self#module_expr a3
      | `ModuleConstraint (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in self#module_type a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method match_case : match_case -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `McOr (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#match_case a1 in self#match_case a2
      | `Case (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#patt a1 in let self = self#expr a2 in self#expr a3
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method module_expr : module_expr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `MeApp (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_expr a2
      | `Functor (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#auident a1 in
          let self = self#module_type a2 in self#module_expr a3
      | `Struct (a0,a1) -> let self = self#loc a0 in self#str_item a1
      | `ModuleExprConstraint (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_type a2
      | `PackageModule (a0,a1) -> let self = self#loc a0 in self#expr a1
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method str_item : str_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Class (a0,a1) -> let self = self#loc a0 in self#class_expr a1
      | `ClassType (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#str_item a1 in self#str_item a2
      | `Directive (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#alident a1 in self#expr a2
      | `Exception (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in
          self#meta_option (fun self  -> self#ident) a2
      | `StExp (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `External (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | `Include (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | `Module (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in self#module_expr a2
      | `RecModule (a0,a1) ->
          let self = self#loc a0 in self#module_binding a1
      | `ModuleType (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#auident a1 in self#module_type a2
      | `Open (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `Type (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `Value (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in self#binding a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method class_type : class_type -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `CtCon (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#virtual_flag a1 in
          let self = self#ident a2 in self#ctyp a3
      | `CtFun (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#class_type a2
      | `CtSig (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#class_sig_item a2
      | `CtAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | `CtCol (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | `CtEq (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Eq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Sem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_sig_item a1 in self#class_sig_item a2
      | `Inherit (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `Method (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | `CgVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#mutable_flag a2 in
          let self = self#virtual_flag a3 in self#ctyp a4
      | `CgVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method class_expr : class_expr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `CeApp (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#expr a2
      | `CeCon (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#virtual_flag a1 in
          let self = self#ident a2 in self#ctyp a3
      | `CeFun (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_expr a2
      | `CeLet (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in
          let self = self#binding a2 in self#class_expr a3
      | `Obj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | `CeTyc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_type a2
      | `And (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | `Eq (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method class_str_item : class_str_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `CrSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_str_item a1 in self#class_str_item a2
      | `Eq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `Inherit (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#override_flag a1 in
          let self = self#class_expr a2 in
          self#meta_option (fun self  -> self#alident) a3
      | `Initializer (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `CrMth (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#override_flag a2 in
          let self = self#private_flag a3 in
          let self = self#expr a4 in self#ctyp a5
      | `CrVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#override_flag a2 in
          let self = self#mutable_flag a3 in self#expr a4
      | `CrVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | `CrVvr (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#alident a1 in
          let self = self#mutable_flag a2 in self#ctyp a3
      | #ant as a0 -> (self#ant a0 :>'self_type)
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
  end
class fold2 =
  object (self : 'self_type)
    inherit  foldbase2
    method loc : loc -> loc -> 'self_type= fun a0  a1  -> self#fanloc_t a0 a1
    method ant : ant -> ant -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
    method literal : literal -> literal -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Chr (a0,a1),`Chr (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Int (a0,a1),`Int (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Int32 (a0,a1),`Int32 (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Int64 (a0,a1),`Int64 (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Flo (a0,a1),`Flo (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`NativeInt (a0,a1),`NativeInt (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Str (a0,a1),`Str (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_flag : rec_flag -> rec_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Recursive a0,`Recursive b0) -> self#loc a0 b0
        | (`ReNil a0,`ReNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method direction_flag : direction_flag -> direction_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`To a0,`To b0) -> self#loc a0 b0
        | (`Downto a0,`Downto b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method mutable_flag : mutable_flag -> mutable_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Mutable a0,`Mutable b0) -> self#loc a0 b0
        | (`MuNil a0,`MuNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method private_flag : private_flag -> private_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Private a0,`Private b0) -> self#loc a0 b0
        | (`PrNil a0,`PrNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method virtual_flag : virtual_flag -> virtual_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Virtual a0,`Virtual b0) -> self#loc a0 b0
        | (`ViNil a0,`ViNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method override_flag : override_flag -> override_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Override a0,`Override b0) -> self#loc a0 b0
        | (`OvNil a0,`OvNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method row_var_flag : row_var_flag -> row_var_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`RowVar a0,`RowVar b0) -> self#loc a0 b0
        | (`RvNil a0,`RvNil b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method position_flag : position_flag -> position_flag -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Positive a0,`Positive b0) -> self#loc a0 b0
        | (`Negative a0,`Negative b0) -> self#loc a0 b0
        | (`Normal a0,`Normal b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method meta_bool : meta_bool -> meta_bool -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`True a0,`True b0) -> self#loc a0 b0
        | (`False a0,`False b0) -> self#loc a0 b0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_option -> 'all_a0 meta_option -> 'self_type=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`None a0,`None b0) -> self#loc a0 b0
        | (`Some a0,`Some b0) -> mf_a self a0 b0
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (_,_) -> invalid_arg "fold2 failure"
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_list -> 'all_a0 meta_list -> 'self_type=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`LNil a0,`LNil b0) -> self#loc a0 b0
        | (`LCons (a0,a1),`LCons (b0,b1)) ->
            let self = mf_a self a0 b0 in self#meta_list mf_a a1 b1
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (_,_) -> invalid_arg "fold2 failure"
    method alident : alident -> alident -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Lid (a0,a1),`Lid (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method auident : auident -> auident -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Uid (a0,a1),`Uid (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method aident : aident -> aident -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | ((#alident as a0),(#alident as b0)) ->
            (self#alident a0 b0 :>'self_type)
        | ((#auident as a0),(#auident as b0)) ->
            (self#auident a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method astring : astring -> astring -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`C (a0,a1),`C (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method ident : ident -> ident -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`IdAcc (a0,a1,a2),`IdAcc (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#ident a2 b2
        | (`IdApp (a0,a1,a2),`IdApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#ident a2 b2
        | ((#alident as a0),(#alident as b0)) ->
            (self#alident a0 b0 :>'self_type)
        | ((#auident as a0),(#auident as b0)) ->
            (self#auident a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method ctyp : ctyp -> ctyp -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Any a0,`Any b0) -> self#loc a0 b0
        | (`TyApp (a0,a1,a2),`TyApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyArr (a0,a1,a2),`TyArr (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`ClassPath (a0,a1),`ClassPath (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`TyLab (a0,a1,a2),`TyLab (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#ctyp a2 b2
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`TyMan (a0,a1,a2),`TyMan (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyDcl (a0,a1,a2,a3,a4),`TyDcl (b0,b1,b2,b3,b4)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#list (fun self  -> self#ctyp) a2 b2 in
            let self = self#ctyp a3 b3 in
            self#list
              (fun self  a0  b0  ->
                 match (a0, b0) with
                 | ((a0,a1),(b0,b1)) ->
                     let self = self#ctyp a0 b0 in self#ctyp a1 b1) a4 b4
        | (`TyObj (a0,a1,a2),`TyObj (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#row_var_flag a2 b2
        | (`TyOlb (a0,a1,a2),`TyOlb (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#ctyp a2 b2
        | (`TyPol (a0,a1,a2),`TyPol (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyTypePol (a0,a1,a2),`TyTypePol (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Quote (a0,a1,a2),`Quote (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#position_flag a1 b1 in
            self#meta_option (fun self  -> self#alident) a2 b2
        | (`TyRec (a0,a1),`TyRec (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`TyCol (a0,a1,a2),`TyCol (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TySem (a0,a1,a2),`TySem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Com (a0,a1,a2),`Com (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Sum (a0,a1),`Sum (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Of (a0,a1,a2),`Of (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Or (a0,a1,a2),`Or (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Private (a0,a1),`Private (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Mutable (a0,a1),`Mutable (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Tup (a0,a1),`Tup (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Sta (a0,a1,a2),`Sta (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyVrn (a0,a1),`TyVrn (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`TyVrnEq (a0,a1),`TyVrnEq (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`TyVrnSup (a0,a1),`TyVrnSup (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`TyVrnInf (a0,a1),`TyVrnInf (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`TyVrnInfSup (a0,a1,a2),`TyVrnInfSup (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyAmp (a0,a1,a2),`TyAmp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`TyOfAmp (a0,a1,a2),`TyOfAmp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Package (a0,a1),`Package (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_type a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method patt : patt -> patt -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#alident a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (`Any a0,`Any b0) -> self#loc a0 b0
        | (`PaApp (a0,a1,a2),`PaApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#patt a2 b2
        | (`Array (a0,a1),`Array (b0,b1)) ->
            let self = self#loc a0 b0 in self#patt a1 b1
        | (`PaCom (a0,a1,a2),`PaCom (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#patt a2 b2
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#patt a2 b2
        | ((#literal as a0),(#literal as b0)) ->
            (self#literal a0 b0 :>'self_type)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#patt a2 b2
        | (`PaOlbi (a0,a1,a2,a3),`PaOlbi (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#patt a2 b2 in
            self#meta_option (fun self  -> self#expr) a3 b3
        | (`PaOrp (a0,a1,a2),`PaOrp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#patt a2 b2
        | (`PaRng (a0,a1,a2),`PaRng (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#patt a2 b2
        | (`PaRec (a0,a1),`PaRec (b0,b1)) ->
            let self = self#loc a0 b0 in self#patt a1 b1
        | (`PaEq (a0,a1,a2),`PaEq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#patt a2 b2
        | (`PaTup (a0,a1),`PaTup (b0,b1)) ->
            let self = self#loc a0 b0 in self#patt a1 b1
        | (`PaTyc (a0,a1,a2),`PaTyc (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#ctyp a2 b2
        | (`PaTyp (a0,a1),`PaTyp (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`PaVrn (a0,a1),`PaVrn (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            let self = self#loc a0 b0 in self#patt a1 b1
        | (`ModuleUnpack (a0,a1,a2),`ModuleUnpack (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in
            self#meta_option (fun self  -> self#ctyp) a2 b2
        | (_,_) -> invalid_arg "fold2 failure"
    method expr : expr -> expr -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`ExAcc (a0,a1,a2),`ExAcc (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (`ExApp (a0,a1,a2),`ExApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`ExAre (a0,a1,a2),`ExAre (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`Array (a0,a1),`Array (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`ExAsf a0,`ExAsf b0) -> self#loc a0 b0
        | (`ExAsr (a0,a1),`ExAsr (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`ExAss (a0,a1,a2),`ExAss (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`For (a0,a1,a2,a3,a4,a5),`For (b0,b1,b2,b3,b4,b5)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#expr a2 b2 in
            let self = self#expr a3 b3 in
            let self = self#direction_flag a4 b4 in self#expr a5 b5
        | (`Fun (a0,a1),`Fun (b0,b1)) ->
            let self = self#loc a0 b0 in self#match_case a1 b1
        | (`IfThenElse (a0,a1,a2,a3),`IfThenElse (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in
            let self = self#expr a2 b2 in self#expr a3 b3
        | ((#literal as a0),(#literal as b0)) ->
            (self#literal a0 b0 :>'self_type)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#expr a2 b2
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`LetIn (a0,a1,a2,a3),`LetIn (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#rec_flag a1 b1 in
            let self = self#binding a2 b2 in self#expr a3 b3
        | (`LetModule (a0,a1,a2,a3),`LetModule (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in
            let self = self#module_expr a2 b2 in self#expr a3 b3
        | (`Match (a0,a1,a2),`Match (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#match_case a2 b2
        | (`New (a0,a1),`New (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#class_str_item a2 b2
        | (`OptLabl (a0,a1,a2),`OptLabl (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#expr a2 b2
        | (`OvrInst (a0,a1),`OvrInst (b0,b1)) ->
            let self = self#loc a0 b0 in self#rec_binding a1 b1
        | (`Record (a0,a1,a2),`Record (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#rec_binding a1 b1 in self#expr a2 b2
        | (`Seq (a0,a1),`Seq (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`Send (a0,a1,a2),`Send (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#alident a2 b2
        | (`StringDot (a0,a1,a2),`StringDot (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`Try (a0,a1,a2),`Try (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#match_case a2 b2
        | (`ExTup (a0,a1),`ExTup (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`ExCom (a0,a1,a2),`ExCom (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`Constraint_exp (a0,a1,a2),`Constraint_exp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#ctyp a2 b2
        | (`ExCoe (a0,a1,a2,a3),`ExCoe (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in
            let self = self#ctyp a2 b2 in self#ctyp a3 b3
        | (`ExVrn (a0,a1),`ExVrn (b0,b1)) ->
            let self = self#loc a0 b0 in self#string a1 b1
        | (`While (a0,a1,a2),`While (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#expr a1 b1 in self#expr a2 b2
        | (`Let_open (a0,a1,a2),`Let_open (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#expr a2 b2
        | (`LocalTypeFun (a0,a1,a2),`LocalTypeFun (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#expr a2 b2
        | (`Package_expr (a0,a1),`Package_expr (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_expr a1 b1
        | (_,_) -> invalid_arg "fold2 failure"
    method module_type : module_type -> module_type -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`MtFun (a0,a1,a2,a3),`MtFun (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in
            let self = self#module_type a2 b2 in self#module_type a3 b3
        | (`Sig (a0,a1),`Sig (b0,b1)) ->
            let self = self#loc a0 b0 in self#sig_item a1 b1
        | (`MtWit (a0,a1,a2),`MtWit (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#module_type a1 b1 in self#with_constr a2 b2
        | (`Of (a0,a1),`Of (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_expr a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method sig_item : sig_item -> sig_item -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            let self = self#loc a0 b0 in self#class_type a1 b1
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            let self = self#loc a0 b0 in self#class_type a1 b1
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#sig_item a1 b1 in self#sig_item a2 b2
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#expr a2 b2
        | (`Exception (a0,a1),`Exception (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#ctyp a2 b2 in
            self#meta_list (fun self  -> self#string) a3 b3
        | (`Include (a0,a1),`Include (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_type a1 b1
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in self#module_type a2 b2
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_binding a1 b1
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in self#module_type a2 b2
        | (`Open (a0,a1),`Open (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`Type (a0,a1),`Type (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Val (a0,a1,a2),`Val (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#ctyp a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method with_constr : with_constr -> with_constr -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`TypeEq (a0,a1,a2),`TypeEq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`ModuleEq (a0,a1,a2),`ModuleEq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#ident a2 b2
        | (`TypeSubst (a0,a1,a2),`TypeSubst (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`ModuleSubst (a0,a1,a2),`ModuleSubst (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#ident a2 b2
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#with_constr a1 b1 in self#with_constr a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method binding : binding -> binding -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#binding a1 b1 in self#binding a2 b2
        | (`Bind (a0,a1,a2),`Bind (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#expr a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method rec_binding : rec_binding -> rec_binding -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#rec_binding a1 b1 in self#rec_binding a2 b2
        | (`RecBind (a0,a1,a2),`RecBind (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ident a1 b1 in self#expr a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_binding : module_binding -> module_binding -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#module_binding a1 b1 in self#module_binding a2 b2
        | (`ModuleBind (a0,a1,a2,a3),`ModuleBind (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in
            let self = self#module_type a2 b2 in self#module_expr a3 b3
        | (`ModuleConstraint (a0,a1,a2),`ModuleConstraint (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in self#module_type a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method match_case : match_case -> match_case -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`McOr (a0,a1,a2),`McOr (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#match_case a1 b1 in self#match_case a2 b2
        | (`Case (a0,a1,a2,a3),`Case (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in
            let self = self#expr a2 b2 in self#expr a3 b3
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method module_expr : module_expr -> module_expr -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`MeApp (a0,a1,a2),`MeApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#module_expr a1 b1 in self#module_expr a2 b2
        | (`Functor (a0,a1,a2,a3),`Functor (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in
            let self = self#module_type a2 b2 in self#module_expr a3 b3
        | (`Struct (a0,a1),`Struct (b0,b1)) ->
            let self = self#loc a0 b0 in self#str_item a1 b1
        | (`ModuleExprConstraint (a0,a1,a2),`ModuleExprConstraint (b0,b1,b2))
            ->
            let self = self#loc a0 b0 in
            let self = self#module_expr a1 b1 in self#module_type a2 b2
        | (`PackageModule (a0,a1),`PackageModule (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method str_item : str_item -> str_item -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            let self = self#loc a0 b0 in self#class_expr a1 b1
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            let self = self#loc a0 b0 in self#class_type a1 b1
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#str_item a1 b1 in self#str_item a2 b2
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in self#expr a2 b2
        | (`Exception (a0,a1,a2),`Exception (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in
            self#meta_option (fun self  -> self#ident) a2 b2
        | (`StExp (a0,a1),`StExp (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#ctyp a2 b2 in
            self#meta_list (fun self  -> self#string) a3 b3
        | (`Include (a0,a1),`Include (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_expr a1 b1
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in self#module_expr a2 b2
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            let self = self#loc a0 b0 in self#module_binding a1 b1
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#auident a1 b1 in self#module_type a2 b2
        | (`Open (a0,a1),`Open (b0,b1)) ->
            let self = self#loc a0 b0 in self#ident a1 b1
        | (`Type (a0,a1),`Type (b0,b1)) ->
            let self = self#loc a0 b0 in self#ctyp a1 b1
        | (`Value (a0,a1,a2),`Value (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#rec_flag a1 b1 in self#binding a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_type : class_type -> class_type -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CtCon (a0,a1,a2,a3),`CtCon (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#virtual_flag a1 b1 in
            let self = self#ident a2 b2 in self#ctyp a3 b3
        | (`CtFun (a0,a1,a2),`CtFun (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#class_type a2 b2
        | (`CtSig (a0,a1,a2),`CtSig (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#class_sig_item a2 b2
        | (`CtAnd (a0,a1,a2),`CtAnd (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_type a1 b1 in self#class_type a2 b2
        | (`CtCol (a0,a1,a2),`CtCol (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_type a1 b1 in self#class_type a2 b2
        | (`CtEq (a0,a1,a2),`CtEq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_type a1 b1 in self#class_type a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_sig_item : class_sig_item -> class_sig_item -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_sig_item a1 b1 in self#class_sig_item a2 b2
        | (`Inherit (a0,a1),`Inherit (b0,b1)) ->
            let self = self#loc a0 b0 in self#class_type a1 b1
        | (`Method (a0,a1,a2,a3),`Method (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#private_flag a2 b2 in self#ctyp a3 b3
        | (`CgVal (a0,a1,a2,a3,a4),`CgVal (b0,b1,b2,b3,b4)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#mutable_flag a2 b2 in
            let self = self#virtual_flag a3 b3 in self#ctyp a4 b4
        | (`CgVir (a0,a1,a2,a3),`CgVir (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#private_flag a2 b2 in self#ctyp a3 b3
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_expr : class_expr -> class_expr -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CeApp (a0,a1,a2),`CeApp (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_expr a1 b1 in self#expr a2 b2
        | (`CeCon (a0,a1,a2,a3),`CeCon (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#virtual_flag a1 b1 in
            let self = self#ident a2 b2 in self#ctyp a3 b3
        | (`CeFun (a0,a1,a2),`CeFun (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#class_expr a2 b2
        | (`CeLet (a0,a1,a2,a3),`CeLet (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#rec_flag a1 b1 in
            let self = self#binding a2 b2 in self#class_expr a3 b3
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#patt a1 b1 in self#class_str_item a2 b2
        | (`CeTyc (a0,a1,a2),`CeTyc (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_expr a1 b1 in self#class_type a2 b2
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_expr a1 b1 in self#class_expr a2 b2
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_expr a1 b1 in self#class_expr a2 b2
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method class_str_item : class_str_item -> class_str_item -> 'self_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> self#loc a0 b0
        | (`CrSem (a0,a1,a2),`CrSem (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#class_str_item a1 b1 in self#class_str_item a2 b2
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let self = self#loc a0 b0 in
            let self = self#ctyp a1 b1 in self#ctyp a2 b2
        | (`Inherit (a0,a1,a2,a3),`Inherit (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#override_flag a1 b1 in
            let self = self#class_expr a2 b2 in
            self#meta_option (fun self  -> self#alident) a3 b3
        | (`Initializer (a0,a1),`Initializer (b0,b1)) ->
            let self = self#loc a0 b0 in self#expr a1 b1
        | (`CrMth (a0,a1,a2,a3,a4,a5),`CrMth (b0,b1,b2,b3,b4,b5)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#override_flag a2 b2 in
            let self = self#private_flag a3 b3 in
            let self = self#expr a4 b4 in self#ctyp a5 b5
        | (`CrVal (a0,a1,a2,a3,a4),`CrVal (b0,b1,b2,b3,b4)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#override_flag a2 b2 in
            let self = self#mutable_flag a3 b3 in self#expr a4 b4
        | (`CrVir (a0,a1,a2,a3),`CrVir (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#private_flag a2 b2 in self#ctyp a3 b3
        | (`CrVvr (a0,a1,a2,a3),`CrVvr (b0,b1,b2,b3)) ->
            let self = self#loc a0 b0 in
            let self = self#alident a1 b1 in
            let self = self#mutable_flag a2 b2 in self#ctyp a3 b3
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>'self_type)
        | (_,_) -> invalid_arg "fold2 failure"
    method fanloc_t : FanLoc.t -> FanLoc.t -> 'self_type= self#unknown
  end
let pp_print_loc: 'fmt -> loc -> 'result =
  fun fmt  a0  -> FanLoc.pp_print_t fmt a0
let pp_print_ant: 'fmt -> ant -> 'result =
  fun fmt  (`Ant (a0,a1))  ->
    Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc a0
      pp_print_string a1
let pp_print_literal: 'fmt -> literal -> 'result =
  fun fmt  ->
    function
    | `Chr (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Chr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Int (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Int@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Int32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Int32@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Int64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Int64@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Flo (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `NativeInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(`NativeInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Str (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
let pp_print_rec_flag: 'fmt -> rec_flag -> 'result =
  fun fmt  ->
    function
    | `Recursive a0 ->
        Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" pp_print_loc a0
    | `ReNil a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_direction_flag: 'fmt -> direction_flag -> 'result =
  fun fmt  ->
    function
    | `To a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" pp_print_loc a0
    | `Downto a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_mutable_flag: 'fmt -> mutable_flag -> 'result =
  fun fmt  ->
    function
    | `Mutable a0 ->
        Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" pp_print_loc a0
    | `MuNil a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_private_flag: 'fmt -> private_flag -> 'result =
  fun fmt  ->
    function
    | `Private a0 ->
        Format.fprintf fmt "@[<1>(`Private@ %a)@]" pp_print_loc a0
    | `PrNil a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_virtual_flag: 'fmt -> virtual_flag -> 'result =
  fun fmt  ->
    function
    | `Virtual a0 ->
        Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" pp_print_loc a0
    | `ViNil a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_override_flag: 'fmt -> override_flag -> 'result =
  fun fmt  ->
    function
    | `Override a0 ->
        Format.fprintf fmt "@[<1>(`Override@ %a)@]" pp_print_loc a0
    | `OvNil a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_row_var_flag: 'fmt -> row_var_flag -> 'result =
  fun fmt  ->
    function
    | `RowVar a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" pp_print_loc a0
    | `RvNil a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_position_flag: 'fmt -> position_flag -> 'result =
  fun fmt  ->
    function
    | `Positive a0 ->
        Format.fprintf fmt "@[<1>(`Positive@ %a)@]" pp_print_loc a0
    | `Negative a0 ->
        Format.fprintf fmt "@[<1>(`Negative@ %a)@]" pp_print_loc a0
    | `Normal a0 -> Format.fprintf fmt "@[<1>(`Normal@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_meta_bool: 'fmt -> meta_bool -> 'result =
  fun fmt  ->
    function
    | `True a0 -> Format.fprintf fmt "@[<1>(`True@ %a)@]" pp_print_loc a0
    | `False a0 -> Format.fprintf fmt "@[<1>(`False@ %a)@]" pp_print_loc a0
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_meta_option :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_option -> 'result=
  fun mf_a  fmt  ->
    function
    | `None a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc a0
    | `Some a0 -> Format.fprintf fmt "@[<1>(`Some@ %a)@]" mf_a a0
    | `Ant (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
let rec pp_print_meta_list :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_list -> 'result=
  fun mf_a  fmt  ->
    function
    | `LNil a0 -> Format.fprintf fmt "@[<1>(`LNil@ %a)@]" pp_print_loc a0
    | `LCons (a0,a1) ->
        Format.fprintf fmt "@[<1>(`LCons@ %a@ %a)@]" mf_a a0
          (pp_print_meta_list mf_a) a1
    | `Ant (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
let pp_print_alident: 'fmt -> alident -> 'result =
  fun fmt  ->
    function
    | `Lid (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_auident: 'fmt -> auident -> 'result =
  fun fmt  ->
    function
    | `Uid (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Uid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let pp_print_aident: 'fmt -> aident -> 'result =
  fun fmt  ->
    function
    | #alident as a0 -> (pp_print_alident fmt a0 :>'result)
    | #auident as a0 -> (pp_print_auident fmt a0 :>'result)
let pp_print_astring: 'fmt -> astring -> 'result =
  fun fmt  ->
    function
    | `C (a0,a1) ->
        Format.fprintf fmt "@[<1>(`C@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
let rec pp_print_ident: 'fmt -> ident -> 'result =
  fun fmt  ->
    function
    | `IdAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`IdAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `IdApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`IdApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | #alident as a0 -> (pp_print_alident fmt a0 :>'result)
    | #auident as a0 -> (pp_print_auident fmt a0 :>'result)
let rec pp_print_ctyp: 'fmt -> ctyp -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Alias (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc a0
    | `TyApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyArr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyArr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `ClassPath (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ClassPath@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `TyLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_ctyp a2
    | `Id (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `TyMan (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyMan@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyDcl (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(`TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 (pp_print_list pp_print_ctyp) a2
          pp_print_ctyp a3
          (pp_print_list
             (fun fmt  (a0,a1)  ->
                Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_ctyp a0
                  pp_print_ctyp a1)) a4
    | `TyObj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyObj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_row_var_flag a2
    | `TyOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_ctyp a2
    | `TyPol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyPol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyTypePol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyTypePol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Quote (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Quote@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_position_flag a1 (pp_print_meta_option pp_print_alident)
          a2
    | `TyRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(`TyRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TySem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TySem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Com (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Sum (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Sum@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Of (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `And (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Or (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Or@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Private (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Private@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Mutable (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Mutable@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Tup (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Tup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Sta (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sta@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `TyVrnEq (a0,a1) ->
        Format.fprintf fmt "@[<1>(`TyVrnEq@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnSup (a0,a1) ->
        Format.fprintf fmt "@[<1>(`TyVrnSup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnInf (a0,a1) ->
        Format.fprintf fmt "@[<1>(`TyVrnInf@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnInfSup (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyVrnInfSup@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_ctyp a1 pp_print_ctyp a2
    | `TyAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyOfAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TyOfAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Package (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Package@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_patt: 'fmt -> patt -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `Alias (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_alident a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
    | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc a0
    | `PaApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `Array (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | #literal as a0 -> (pp_print_literal fmt a0 :>'result)
    | `Label (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_patt a2
    | `PaOlbi (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`PaOlbi@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_patt a2
          (pp_print_meta_option pp_print_expr) a3
    | `PaOrp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaOrp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaRng (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaRng@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(`PaRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_patt a2
    | `PaTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(`PaTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`PaTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_ctyp a2
    | `PaTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(`PaTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `PaVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(`PaVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `Lazy (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `ModuleUnpack (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleUnpack@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_auident a1 (pp_print_meta_option pp_print_ctyp) a2
and pp_print_expr: 'fmt -> expr -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `ExAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ExAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
    | `ExApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ExApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExAre (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ExAre@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `Array (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Array@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExAsf a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" pp_print_loc a0
    | `ExAsr (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ExAsr@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExAss (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ExAss@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `For (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(`For@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_alident a1 pp_print_expr a2 pp_print_expr
          a3 pp_print_direction_flag a4 pp_print_expr a5
    | `Fun (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Fun@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1
    | `IfThenElse (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`IfThenElse@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_expr a1 pp_print_expr a2 pp_print_expr a3
    | #literal as a0 -> (pp_print_literal fmt a0 :>'result)
    | `Label (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Label@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_expr a2
    | `Lazy (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `LetIn (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_expr a3
    | `LetModule (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`LetModule@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_auident a1 pp_print_module_expr a2 pp_print_expr a3
    | `Match (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Match@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | `New (a0,a1) ->
        Format.fprintf fmt "@[<1>(`New@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `Obj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | `OptLabl (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_expr a2
    | `OvrInst (a0,a1) ->
        Format.fprintf fmt "@[<1>(`OvrInst@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1
    | `Record (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Record@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_expr a2
    | `Seq (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Seq@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `Send (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Send@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_alident a2
    | `StringDot (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`StringDot@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `Try (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Try@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | `ExTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ExTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ExCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `Constraint_exp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Constraint_exp@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_expr a1 pp_print_ctyp a2
    | `ExCoe (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`ExCoe@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_ctyp a2 pp_print_ctyp a3
    | `ExVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ExVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `While (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`While@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `Let_open (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Let_open@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | `LocalTypeFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 pp_print_expr a2
    | `Package_expr (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Package_expr@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
and pp_print_module_type: 'fmt -> module_type -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `MtFun (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`MtFun@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_auident a1 pp_print_module_type a2 pp_print_module_type a3
    | `Sig (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Sig@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1
    | `MtWit (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`MtWit@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1 pp_print_with_constr a2
    | `Of (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Of@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_sig_item: 'fmt -> sig_item -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Class (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `ClassType (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1 pp_print_sig_item a2
    | `Directive (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_expr a2
    | `Exception (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Exception@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `External (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | `Include (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | `Module (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_auident a1 pp_print_module_type a2
    | `RecModule (a0,a1) ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | `ModuleType (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_auident a1 pp_print_module_type a2
    | `Open (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `Type (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Val (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Val@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_ctyp a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_with_constr: 'fmt -> with_constr -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `TypeEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TypeEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `ModuleEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `TypeSubst (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`TypeSubst@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `ModuleSubst (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleSubst@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_ident a1 pp_print_ident a2
    | `And (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_with_constr a1 pp_print_with_constr a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_binding: 'fmt -> binding -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `And (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_binding a1 pp_print_binding a2
    | `Bind (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Bind@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_rec_binding: 'fmt -> rec_binding -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_rec_binding a2
    | `RecBind (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_module_binding: 'fmt -> module_binding -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `And (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1 pp_print_module_binding a2
    | `ModuleBind (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`ModuleBind@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_auident a1 pp_print_module_type a2
          pp_print_module_expr a3
    | `ModuleConstraint (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_auident a1 pp_print_module_type a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_match_case: 'fmt -> match_case -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `McOr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`McOr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1 pp_print_match_case a2
    | `Case (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`Case@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2 pp_print_expr a3
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_module_expr: 'fmt -> module_expr -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Id@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `MeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`MeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1 pp_print_module_expr a2
    | `Functor (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`Functor@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_auident a1 pp_print_module_type a2 pp_print_module_expr
          a3
    | `Struct (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Struct@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1
    | `ModuleExprConstraint (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleExprConstraint@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_module_expr a1 pp_print_module_type a2
    | `PackageModule (a0,a1) ->
        Format.fprintf fmt "@[<1>(`PackageModule@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_str_item: 'fmt -> str_item -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Class (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Class@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1
    | `ClassType (a0,a1) ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1 pp_print_str_item a2
    | `Directive (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Directive@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_expr a2
    | `Exception (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Exception@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 (pp_print_meta_option pp_print_ident) a2
    | `StExp (a0,a1) ->
        Format.fprintf fmt "@[<1>(`StExp@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `External (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`External@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | `Include (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Include@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | `Module (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Module@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_auident a1 pp_print_module_expr a2
    | `RecModule (a0,a1) ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | `ModuleType (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_auident a1 pp_print_module_type a2
    | `Open (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Open@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `Type (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Type@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `Value (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Value@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_class_type: 'fmt -> class_type -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `CtCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CtCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | `CtFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CtFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_type a2
    | `CtSig (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CtSig@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_sig_item a2
    | `CtAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CtAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | `CtCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CtCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | `CtEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CtEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_class_sig_item: 'fmt -> class_sig_item -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Eq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Sem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Sem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_sig_item a1 pp_print_class_sig_item a2
    | `Inherit (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `Method (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`Method@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_private_flag a2 pp_print_ctyp a3
    | `CgVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(`CgVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 pp_print_mutable_flag a2
          pp_print_virtual_flag a3 pp_print_ctyp a4
    | `CgVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CgVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_private_flag a2 pp_print_ctyp a3
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_class_expr: 'fmt -> class_expr -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `CeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_expr a2
    | `CeCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CeCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | `CeFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CeFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_expr a2
    | `CeLet (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CeLet@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_class_expr a3
    | `Obj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Obj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | `CeTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CeTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_type a2
    | `And (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`And@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | `Eq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
and pp_print_class_str_item: 'fmt -> class_str_item -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `CrSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`CrSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_str_item a1 pp_print_class_str_item a2
    | `Eq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(`Eq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `Inherit (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_override_flag a1 pp_print_class_expr a2
          (pp_print_meta_option pp_print_alident) a3
    | `Initializer (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Initializer@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `CrMth (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(`CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_alident a1 pp_print_override_flag a2
          pp_print_private_flag a3 pp_print_expr a4 pp_print_ctyp a5
    | `CrVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(`CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_alident a1 pp_print_override_flag a2
          pp_print_mutable_flag a3 pp_print_expr a4
    | `CrVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CrVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_private_flag a2 pp_print_ctyp a3
    | `CrVvr (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(`CrVvr@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_alident a1 pp_print_mutable_flag a2 pp_print_ctyp a3
    | #ant as a0 -> (pp_print_ant fmt a0 :>'result)
class iter =
  object (self : 'self_type)
    inherit  iterbase
    method loc : loc -> 'result= fun a0  -> self#fanloc_t a0
    method ant : ant -> 'result=
      fun (`Ant (a0,a1))  -> self#loc a0; self#string a1
    method literal : literal -> 'result=
      function
      | `Chr (a0,a1) -> (self#loc a0; self#string a1)
      | `Int (a0,a1) -> (self#loc a0; self#string a1)
      | `Int32 (a0,a1) -> (self#loc a0; self#string a1)
      | `Int64 (a0,a1) -> (self#loc a0; self#string a1)
      | `Flo (a0,a1) -> (self#loc a0; self#string a1)
      | `NativeInt (a0,a1) -> (self#loc a0; self#string a1)
      | `Str (a0,a1) -> (self#loc a0; self#string a1)
    method rec_flag : rec_flag -> 'result=
      function
      | `Recursive a0 -> self#loc a0
      | `ReNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method direction_flag : direction_flag -> 'result=
      function
      | `To a0 -> self#loc a0
      | `Downto a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method mutable_flag : mutable_flag -> 'result=
      function
      | `Mutable a0 -> self#loc a0
      | `MuNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method private_flag : private_flag -> 'result=
      function
      | `Private a0 -> self#loc a0
      | `PrNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method virtual_flag : virtual_flag -> 'result=
      function
      | `Virtual a0 -> self#loc a0
      | `ViNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method override_flag : override_flag -> 'result=
      function
      | `Override a0 -> self#loc a0
      | `OvNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method row_var_flag : row_var_flag -> 'result=
      function
      | `RowVar a0 -> self#loc a0
      | `RvNil a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method position_flag : position_flag -> 'result=
      function
      | `Positive a0 -> self#loc a0
      | `Negative a0 -> self#loc a0
      | `Normal a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method meta_bool : meta_bool -> 'result=
      function
      | `True a0 -> self#loc a0
      | `False a0 -> self#loc a0
      | #ant as a0 -> (self#ant a0 :>'result)
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'result) -> 'all_a0 meta_option -> 'result=
      fun mf_a  ->
        function
        | `None a0 -> self#loc a0
        | `Some a0 -> mf_a self a0
        | `Ant (a0,a1) -> (self#loc a0; self#string a1)
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'result) -> 'all_a0 meta_list -> 'result=
      fun mf_a  ->
        function
        | `LNil a0 -> self#loc a0
        | `LCons (a0,a1) -> (mf_a self a0; self#meta_list mf_a a1)
        | `Ant (a0,a1) -> (self#loc a0; self#string a1)
    method alident : alident -> 'result=
      function
      | `Lid (a0,a1) -> (self#loc a0; self#string a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method auident : auident -> 'result=
      function
      | `Uid (a0,a1) -> (self#loc a0; self#string a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method aident : aident -> 'result=
      function
      | #alident as a0 -> (self#alident a0 :>'result)
      | #auident as a0 -> (self#auident a0 :>'result)
    method astring : astring -> 'result=
      function
      | `C (a0,a1) -> (self#loc a0; self#string a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method ident : ident -> 'result=
      function
      | `IdAcc (a0,a1,a2) -> (self#loc a0; self#ident a1; self#ident a2)
      | `IdApp (a0,a1,a2) -> (self#loc a0; self#ident a1; self#ident a2)
      | #alident as a0 -> (self#alident a0 :>'result)
      | #auident as a0 -> (self#auident a0 :>'result)
    method ctyp : ctyp -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Alias (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Any a0 -> self#loc a0
      | `TyApp (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyArr (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `ClassPath (a0,a1) -> (self#loc a0; self#ident a1)
      | `TyLab (a0,a1,a2) -> (self#loc a0; self#alident a1; self#ctyp a2)
      | `Id (a0,a1) -> (self#loc a0; self#ident a1)
      | `TyMan (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyDcl (a0,a1,a2,a3,a4) ->
          (self#loc a0;
           self#alident a1;
           self#list (fun self  -> self#ctyp) a2;
           self#ctyp a3;
           self#list (fun self  (a0,a1)  -> self#ctyp a0; self#ctyp a1) a4)
      | `TyObj (a0,a1,a2) ->
          (self#loc a0; self#ctyp a1; self#row_var_flag a2)
      | `TyOlb (a0,a1,a2) -> (self#loc a0; self#alident a1; self#ctyp a2)
      | `TyPol (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyTypePol (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Quote (a0,a1,a2) ->
          (self#loc a0;
           self#position_flag a1;
           self#meta_option (fun self  -> self#alident) a2)
      | `TyRec (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `TyCol (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TySem (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Com (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Sum (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Of (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `And (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Or (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Private (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Mutable (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Tup (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Sta (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyVrn (a0,a1) -> (self#loc a0; self#string a1)
      | `TyVrnEq (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `TyVrnSup (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `TyVrnInf (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `TyVrnInfSup (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyAmp (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `TyOfAmp (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Package (a0,a1) -> (self#loc a0; self#module_type a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method patt : patt -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> (self#loc a0; self#ident a1)
      | `Alias (a0,a1,a2) -> (self#loc a0; self#patt a1; self#alident a2)
      | #ant as a0 -> (self#ant a0 :>'result)
      | `Any a0 -> self#loc a0
      | `PaApp (a0,a1,a2) -> (self#loc a0; self#patt a1; self#patt a2)
      | `Array (a0,a1) -> (self#loc a0; self#patt a1)
      | `PaCom (a0,a1,a2) -> (self#loc a0; self#patt a1; self#patt a2)
      | `Sem (a0,a1,a2) -> (self#loc a0; self#patt a1; self#patt a2)
      | #literal as a0 -> (self#literal a0 :>'result)
      | `Label (a0,a1,a2) -> (self#loc a0; self#alident a1; self#patt a2)
      | `PaOlbi (a0,a1,a2,a3) ->
          (self#loc a0;
           self#alident a1;
           self#patt a2;
           self#meta_option (fun self  -> self#expr) a3)
      | `PaOrp (a0,a1,a2) -> (self#loc a0; self#patt a1; self#patt a2)
      | `PaRng (a0,a1,a2) -> (self#loc a0; self#patt a1; self#patt a2)
      | `PaRec (a0,a1) -> (self#loc a0; self#patt a1)
      | `PaEq (a0,a1,a2) -> (self#loc a0; self#ident a1; self#patt a2)
      | `PaTup (a0,a1) -> (self#loc a0; self#patt a1)
      | `PaTyc (a0,a1,a2) -> (self#loc a0; self#patt a1; self#ctyp a2)
      | `PaTyp (a0,a1) -> (self#loc a0; self#ident a1)
      | `PaVrn (a0,a1) -> (self#loc a0; self#string a1)
      | `Lazy (a0,a1) -> (self#loc a0; self#patt a1)
      | `ModuleUnpack (a0,a1,a2) ->
          (self#loc a0;
           self#auident a1;
           self#meta_option (fun self  -> self#ctyp) a2)
    method expr : expr -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> (self#loc a0; self#ident a1)
      | `ExAcc (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | #ant as a0 -> (self#ant a0 :>'result)
      | `ExApp (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `ExAre (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `Array (a0,a1) -> (self#loc a0; self#expr a1)
      | `Sem (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `ExAsf a0 -> self#loc a0
      | `ExAsr (a0,a1) -> (self#loc a0; self#expr a1)
      | `ExAss (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `For (a0,a1,a2,a3,a4,a5) ->
          (self#loc a0;
           self#alident a1;
           self#expr a2;
           self#expr a3;
           self#direction_flag a4;
           self#expr a5)
      | `Fun (a0,a1) -> (self#loc a0; self#match_case a1)
      | `IfThenElse (a0,a1,a2,a3) ->
          (self#loc a0; self#expr a1; self#expr a2; self#expr a3)
      | #literal as a0 -> (self#literal a0 :>'result)
      | `Label (a0,a1,a2) -> (self#loc a0; self#alident a1; self#expr a2)
      | `Lazy (a0,a1) -> (self#loc a0; self#expr a1)
      | `LetIn (a0,a1,a2,a3) ->
          (self#loc a0; self#rec_flag a1; self#binding a2; self#expr a3)
      | `LetModule (a0,a1,a2,a3) ->
          (self#loc a0; self#auident a1; self#module_expr a2; self#expr a3)
      | `Match (a0,a1,a2) -> (self#loc a0; self#expr a1; self#match_case a2)
      | `New (a0,a1) -> (self#loc a0; self#ident a1)
      | `Obj (a0,a1,a2) ->
          (self#loc a0; self#patt a1; self#class_str_item a2)
      | `OptLabl (a0,a1,a2) -> (self#loc a0; self#alident a1; self#expr a2)
      | `OvrInst (a0,a1) -> (self#loc a0; self#rec_binding a1)
      | `Record (a0,a1,a2) ->
          (self#loc a0; self#rec_binding a1; self#expr a2)
      | `Seq (a0,a1) -> (self#loc a0; self#expr a1)
      | `Send (a0,a1,a2) -> (self#loc a0; self#expr a1; self#alident a2)
      | `StringDot (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `Try (a0,a1,a2) -> (self#loc a0; self#expr a1; self#match_case a2)
      | `ExTup (a0,a1) -> (self#loc a0; self#expr a1)
      | `ExCom (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `Constraint_exp (a0,a1,a2) ->
          (self#loc a0; self#expr a1; self#ctyp a2)
      | `ExCoe (a0,a1,a2,a3) ->
          (self#loc a0; self#expr a1; self#ctyp a2; self#ctyp a3)
      | `ExVrn (a0,a1) -> (self#loc a0; self#string a1)
      | `While (a0,a1,a2) -> (self#loc a0; self#expr a1; self#expr a2)
      | `Let_open (a0,a1,a2) -> (self#loc a0; self#ident a1; self#expr a2)
      | `LocalTypeFun (a0,a1,a2) ->
          (self#loc a0; self#alident a1; self#expr a2)
      | `Package_expr (a0,a1) -> (self#loc a0; self#module_expr a1)
    method module_type : module_type -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> (self#loc a0; self#ident a1)
      | `MtFun (a0,a1,a2,a3) ->
          (self#loc a0;
           self#auident a1;
           self#module_type a2;
           self#module_type a3)
      | `Sig (a0,a1) -> (self#loc a0; self#sig_item a1)
      | `MtWit (a0,a1,a2) ->
          (self#loc a0; self#module_type a1; self#with_constr a2)
      | `Of (a0,a1) -> (self#loc a0; self#module_expr a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method sig_item : sig_item -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Class (a0,a1) -> (self#loc a0; self#class_type a1)
      | `ClassType (a0,a1) -> (self#loc a0; self#class_type a1)
      | `Sem (a0,a1,a2) -> (self#loc a0; self#sig_item a1; self#sig_item a2)
      | `Directive (a0,a1,a2) -> (self#loc a0; self#alident a1; self#expr a2)
      | `Exception (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `External (a0,a1,a2,a3) ->
          (self#loc a0;
           self#alident a1;
           self#ctyp a2;
           self#meta_list (fun self  -> self#string) a3)
      | `Include (a0,a1) -> (self#loc a0; self#module_type a1)
      | `Module (a0,a1,a2) ->
          (self#loc a0; self#auident a1; self#module_type a2)
      | `RecModule (a0,a1) -> (self#loc a0; self#module_binding a1)
      | `ModuleType (a0,a1,a2) ->
          (self#loc a0; self#auident a1; self#module_type a2)
      | `Open (a0,a1) -> (self#loc a0; self#ident a1)
      | `Type (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Val (a0,a1,a2) -> (self#loc a0; self#alident a1; self#ctyp a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method with_constr : with_constr -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `TypeEq (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `ModuleEq (a0,a1,a2) -> (self#loc a0; self#ident a1; self#ident a2)
      | `TypeSubst (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `ModuleSubst (a0,a1,a2) ->
          (self#loc a0; self#ident a1; self#ident a2)
      | `And (a0,a1,a2) ->
          (self#loc a0; self#with_constr a1; self#with_constr a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method binding : binding -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `And (a0,a1,a2) -> (self#loc a0; self#binding a1; self#binding a2)
      | `Bind (a0,a1,a2) -> (self#loc a0; self#patt a1; self#expr a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method rec_binding : rec_binding -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Sem (a0,a1,a2) ->
          (self#loc a0; self#rec_binding a1; self#rec_binding a2)
      | `RecBind (a0,a1,a2) -> (self#loc a0; self#ident a1; self#expr a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method module_binding : module_binding -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `And (a0,a1,a2) ->
          (self#loc a0; self#module_binding a1; self#module_binding a2)
      | `ModuleBind (a0,a1,a2,a3) ->
          (self#loc a0;
           self#auident a1;
           self#module_type a2;
           self#module_expr a3)
      | `ModuleConstraint (a0,a1,a2) ->
          (self#loc a0; self#auident a1; self#module_type a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method match_case : match_case -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `McOr (a0,a1,a2) ->
          (self#loc a0; self#match_case a1; self#match_case a2)
      | `Case (a0,a1,a2,a3) ->
          (self#loc a0; self#patt a1; self#expr a2; self#expr a3)
      | #ant as a0 -> (self#ant a0 :>'result)
    method module_expr : module_expr -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Id (a0,a1) -> (self#loc a0; self#ident a1)
      | `MeApp (a0,a1,a2) ->
          (self#loc a0; self#module_expr a1; self#module_expr a2)
      | `Functor (a0,a1,a2,a3) ->
          (self#loc a0;
           self#auident a1;
           self#module_type a2;
           self#module_expr a3)
      | `Struct (a0,a1) -> (self#loc a0; self#str_item a1)
      | `ModuleExprConstraint (a0,a1,a2) ->
          (self#loc a0; self#module_expr a1; self#module_type a2)
      | `PackageModule (a0,a1) -> (self#loc a0; self#expr a1)
      | #ant as a0 -> (self#ant a0 :>'result)
    method str_item : str_item -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Class (a0,a1) -> (self#loc a0; self#class_expr a1)
      | `ClassType (a0,a1) -> (self#loc a0; self#class_type a1)
      | `Sem (a0,a1,a2) -> (self#loc a0; self#str_item a1; self#str_item a2)
      | `Directive (a0,a1,a2) -> (self#loc a0; self#alident a1; self#expr a2)
      | `Exception (a0,a1,a2) ->
          (self#loc a0;
           self#ctyp a1;
           self#meta_option (fun self  -> self#ident) a2)
      | `StExp (a0,a1) -> (self#loc a0; self#expr a1)
      | `External (a0,a1,a2,a3) ->
          (self#loc a0;
           self#alident a1;
           self#ctyp a2;
           self#meta_list (fun self  -> self#string) a3)
      | `Include (a0,a1) -> (self#loc a0; self#module_expr a1)
      | `Module (a0,a1,a2) ->
          (self#loc a0; self#auident a1; self#module_expr a2)
      | `RecModule (a0,a1) -> (self#loc a0; self#module_binding a1)
      | `ModuleType (a0,a1,a2) ->
          (self#loc a0; self#auident a1; self#module_type a2)
      | `Open (a0,a1) -> (self#loc a0; self#ident a1)
      | `Type (a0,a1) -> (self#loc a0; self#ctyp a1)
      | `Value (a0,a1,a2) -> (self#loc a0; self#rec_flag a1; self#binding a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method class_type : class_type -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `CtCon (a0,a1,a2,a3) ->
          (self#loc a0; self#virtual_flag a1; self#ident a2; self#ctyp a3)
      | `CtFun (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#class_type a2)
      | `CtSig (a0,a1,a2) ->
          (self#loc a0; self#ctyp a1; self#class_sig_item a2)
      | `CtAnd (a0,a1,a2) ->
          (self#loc a0; self#class_type a1; self#class_type a2)
      | `CtCol (a0,a1,a2) ->
          (self#loc a0; self#class_type a1; self#class_type a2)
      | `CtEq (a0,a1,a2) ->
          (self#loc a0; self#class_type a1; self#class_type a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method class_sig_item : class_sig_item -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `Eq (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Sem (a0,a1,a2) ->
          (self#loc a0; self#class_sig_item a1; self#class_sig_item a2)
      | `Inherit (a0,a1) -> (self#loc a0; self#class_type a1)
      | `Method (a0,a1,a2,a3) ->
          (self#loc a0; self#alident a1; self#private_flag a2; self#ctyp a3)
      | `CgVal (a0,a1,a2,a3,a4) ->
          (self#loc a0;
           self#alident a1;
           self#mutable_flag a2;
           self#virtual_flag a3;
           self#ctyp a4)
      | `CgVir (a0,a1,a2,a3) ->
          (self#loc a0; self#alident a1; self#private_flag a2; self#ctyp a3)
      | #ant as a0 -> (self#ant a0 :>'result)
    method class_expr : class_expr -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `CeApp (a0,a1,a2) -> (self#loc a0; self#class_expr a1; self#expr a2)
      | `CeCon (a0,a1,a2,a3) ->
          (self#loc a0; self#virtual_flag a1; self#ident a2; self#ctyp a3)
      | `CeFun (a0,a1,a2) -> (self#loc a0; self#patt a1; self#class_expr a2)
      | `CeLet (a0,a1,a2,a3) ->
          (self#loc a0; self#rec_flag a1; self#binding a2; self#class_expr a3)
      | `Obj (a0,a1,a2) ->
          (self#loc a0; self#patt a1; self#class_str_item a2)
      | `CeTyc (a0,a1,a2) ->
          (self#loc a0; self#class_expr a1; self#class_type a2)
      | `And (a0,a1,a2) ->
          (self#loc a0; self#class_expr a1; self#class_expr a2)
      | `Eq (a0,a1,a2) ->
          (self#loc a0; self#class_expr a1; self#class_expr a2)
      | #ant as a0 -> (self#ant a0 :>'result)
    method class_str_item : class_str_item -> 'result=
      function
      | `Nil a0 -> self#loc a0
      | `CrSem (a0,a1,a2) ->
          (self#loc a0; self#class_str_item a1; self#class_str_item a2)
      | `Eq (a0,a1,a2) -> (self#loc a0; self#ctyp a1; self#ctyp a2)
      | `Inherit (a0,a1,a2,a3) ->
          (self#loc a0;
           self#override_flag a1;
           self#class_expr a2;
           self#meta_option (fun self  -> self#alident) a3)
      | `Initializer (a0,a1) -> (self#loc a0; self#expr a1)
      | `CrMth (a0,a1,a2,a3,a4,a5) ->
          (self#loc a0;
           self#alident a1;
           self#override_flag a2;
           self#private_flag a3;
           self#expr a4;
           self#ctyp a5)
      | `CrVal (a0,a1,a2,a3,a4) ->
          (self#loc a0;
           self#alident a1;
           self#override_flag a2;
           self#mutable_flag a3;
           self#expr a4)
      | `CrVir (a0,a1,a2,a3) ->
          (self#loc a0; self#alident a1; self#private_flag a2; self#ctyp a3)
      | `CrVvr (a0,a1,a2,a3) ->
          (self#loc a0; self#alident a1; self#mutable_flag a2; self#ctyp a3)
      | #ant as a0 -> (self#ant a0 :>'result)
    method fanloc_t : FanLoc.t -> 'result= self#unknown
  end
class map2 =
  object (self : 'self_type)
    inherit  mapbase2
    method loc : loc -> loc -> loc= fun a0  a1  -> self#fanloc_t a0 a1
    method ant : ant -> ant -> ant=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Ant (a0, a1)
    method literal : literal -> literal -> literal=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Chr (a0,a1),`Chr (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Chr (a0, a1)
        | (`Int (a0,a1),`Int (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Int (a0, a1)
        | (`Int32 (a0,a1),`Int32 (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Int32 (a0, a1)
        | (`Int64 (a0,a1),`Int64 (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Int64 (a0, a1)
        | (`Flo (a0,a1),`Flo (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Flo (a0, a1)
        | (`NativeInt (a0,a1),`NativeInt (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `NativeInt (a0, a1)
        | (`Str (a0,a1),`Str (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Str (a0, a1)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_flag : rec_flag -> rec_flag -> rec_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Recursive a0,`Recursive b0) ->
            let a0 = self#loc a0 b0 in `Recursive a0
        | (`ReNil a0,`ReNil b0) -> let a0 = self#loc a0 b0 in `ReNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>rec_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method direction_flag :
      direction_flag -> direction_flag -> direction_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`To a0,`To b0) -> let a0 = self#loc a0 b0 in `To a0
        | (`Downto a0,`Downto b0) -> let a0 = self#loc a0 b0 in `Downto a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>direction_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method mutable_flag : mutable_flag -> mutable_flag -> mutable_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Mutable a0,`Mutable b0) -> let a0 = self#loc a0 b0 in `Mutable a0
        | (`MuNil a0,`MuNil b0) -> let a0 = self#loc a0 b0 in `MuNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>mutable_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method private_flag : private_flag -> private_flag -> private_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Private a0,`Private b0) -> let a0 = self#loc a0 b0 in `Private a0
        | (`PrNil a0,`PrNil b0) -> let a0 = self#loc a0 b0 in `PrNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>private_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method virtual_flag : virtual_flag -> virtual_flag -> virtual_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Virtual a0,`Virtual b0) -> let a0 = self#loc a0 b0 in `Virtual a0
        | (`ViNil a0,`ViNil b0) -> let a0 = self#loc a0 b0 in `ViNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>virtual_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method override_flag : override_flag -> override_flag -> override_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Override a0,`Override b0) ->
            let a0 = self#loc a0 b0 in `Override a0
        | (`OvNil a0,`OvNil b0) -> let a0 = self#loc a0 b0 in `OvNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>override_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method row_var_flag : row_var_flag -> row_var_flag -> row_var_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`RowVar a0,`RowVar b0) -> let a0 = self#loc a0 b0 in `RowVar a0
        | (`RvNil a0,`RvNil b0) -> let a0 = self#loc a0 b0 in `RvNil a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>row_var_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method position_flag : position_flag -> position_flag -> position_flag=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Positive a0,`Positive b0) ->
            let a0 = self#loc a0 b0 in `Positive a0
        | (`Negative a0,`Negative b0) ->
            let a0 = self#loc a0 b0 in `Negative a0
        | (`Normal a0,`Normal b0) -> let a0 = self#loc a0 b0 in `Normal a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>position_flag)
        | (_,_) -> invalid_arg "map2 failure"
    method meta_bool : meta_bool -> meta_bool -> meta_bool=
      fun a0  b0  ->
        match (a0, b0) with
        | (`True a0,`True b0) -> let a0 = self#loc a0 b0 in `True a0
        | (`False a0,`False b0) -> let a0 = self#loc a0 b0 in `False a0
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>meta_bool)
        | (_,_) -> invalid_arg "map2 failure"
    method meta_option :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_option -> 'all_a0 meta_option -> 'all_b0 meta_option=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`None a0,`None b0) -> let a0 = self#loc a0 b0 in `None a0
        | (`Some a0,`Some b0) -> let a0 = mf_a self a0 b0 in `Some a0
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Ant (a0, a1)
        | (_,_) -> invalid_arg "map2 failure"
    method meta_list :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_list -> 'all_a0 meta_list -> 'all_b0 meta_list=
      fun mf_a  a0  b0  ->
        match (a0, b0) with
        | (`LNil a0,`LNil b0) -> let a0 = self#loc a0 b0 in `LNil a0
        | (`LCons (a0,a1),`LCons (b0,b1)) ->
            let a0 = mf_a self a0 b0 in
            let a1 = self#meta_list mf_a a1 b1 in `LCons (a0, a1)
        | (`Ant (a0,a1),`Ant (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Ant (a0, a1)
        | (_,_) -> invalid_arg "map2 failure"
    method alident : alident -> alident -> alident=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Lid (a0,a1),`Lid (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Lid (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>alident)
        | (_,_) -> invalid_arg "map2 failure"
    method auident : auident -> auident -> auident=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Uid (a0,a1),`Uid (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `Uid (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>auident)
        | (_,_) -> invalid_arg "map2 failure"
    method aident : aident -> aident -> aident=
      fun a0  b0  ->
        match (a0, b0) with
        | ((#alident as a0),(#alident as b0)) ->
            (self#alident a0 b0 :>aident)
        | ((#auident as a0),(#auident as b0)) ->
            (self#auident a0 b0 :>aident)
        | (_,_) -> invalid_arg "map2 failure"
    method astring : astring -> astring -> astring=
      fun a0  b0  ->
        match (a0, b0) with
        | (`C (a0,a1),`C (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `C (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>astring)
        | (_,_) -> invalid_arg "map2 failure"
    method ident : ident -> ident -> ident=
      fun a0  b0  ->
        match (a0, b0) with
        | (`IdAcc (a0,a1,a2),`IdAcc (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#ident a2 b2 in `IdAcc (a0, a1, a2)
        | (`IdApp (a0,a1,a2),`IdApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#ident a2 b2 in `IdApp (a0, a1, a2)
        | ((#alident as a0),(#alident as b0)) -> (self#alident a0 b0 :>ident)
        | ((#auident as a0),(#auident as b0)) -> (self#auident a0 b0 :>ident)
        | (_,_) -> invalid_arg "map2 failure"
    method ctyp : ctyp -> ctyp -> ctyp=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Alias (a0, a1, a2)
        | (`Any a0,`Any b0) -> let a0 = self#loc a0 b0 in `Any a0
        | (`TyApp (a0,a1,a2),`TyApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyApp (a0, a1, a2)
        | (`TyArr (a0,a1,a2),`TyArr (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyArr (a0, a1, a2)
        | (`ClassPath (a0,a1),`ClassPath (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `ClassPath (a0, a1)
        | (`TyLab (a0,a1,a2),`TyLab (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyLab (a0, a1, a2)
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Id (a0, a1)
        | (`TyMan (a0,a1,a2),`TyMan (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyMan (a0, a1, a2)
        | (`TyDcl (a0,a1,a2,a3,a4),`TyDcl (b0,b1,b2,b3,b4)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#list (fun self  -> self#ctyp) a2 b2 in
            let a3 = self#ctyp a3 b3 in
            let a4 =
              self#list
                (fun self  a0  b0  ->
                   match (a0, b0) with
                   | ((a0,a1),(b0,b1)) ->
                       let a0 = self#ctyp a0 b0 in
                       let a1 = self#ctyp a1 b1 in (a0, a1)) a4 b4 in
            `TyDcl (a0, a1, a2, a3, a4)
        | (`TyObj (a0,a1,a2),`TyObj (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#row_var_flag a2 b2 in `TyObj (a0, a1, a2)
        | (`TyOlb (a0,a1,a2),`TyOlb (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyOlb (a0, a1, a2)
        | (`TyPol (a0,a1,a2),`TyPol (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyPol (a0, a1, a2)
        | (`TyTypePol (a0,a1,a2),`TyTypePol (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyTypePol (a0, a1, a2)
        | (`Quote (a0,a1,a2),`Quote (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#position_flag a1 b1 in
            let a2 = self#meta_option (fun self  -> self#alident) a2 b2 in
            `Quote (a0, a1, a2)
        | (`TyRec (a0,a1),`TyRec (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `TyRec (a0, a1)
        | (`TyCol (a0,a1,a2),`TyCol (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyCol (a0, a1, a2)
        | (`TySem (a0,a1,a2),`TySem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TySem (a0, a1, a2)
        | (`Com (a0,a1,a2),`Com (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Com (a0, a1, a2)
        | (`Sum (a0,a1),`Sum (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Sum (a0, a1)
        | (`Of (a0,a1,a2),`Of (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Of (a0, a1, a2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `And (a0, a1, a2)
        | (`Or (a0,a1,a2),`Or (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Or (a0, a1, a2)
        | (`Private (a0,a1),`Private (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Private (a0, a1)
        | (`Mutable (a0,a1),`Mutable (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Mutable (a0, a1)
        | (`Tup (a0,a1),`Tup (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Tup (a0, a1)
        | (`Sta (a0,a1,a2),`Sta (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Sta (a0, a1, a2)
        | (`TyVrn (a0,a1),`TyVrn (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `TyVrn (a0, a1)
        | (`TyVrnEq (a0,a1),`TyVrnEq (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `TyVrnEq (a0, a1)
        | (`TyVrnSup (a0,a1),`TyVrnSup (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `TyVrnSup (a0, a1)
        | (`TyVrnInf (a0,a1),`TyVrnInf (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `TyVrnInf (a0, a1)
        | (`TyVrnInfSup (a0,a1,a2),`TyVrnInfSup (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyVrnInfSup (a0, a1, a2)
        | (`TyAmp (a0,a1,a2),`TyAmp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyAmp (a0, a1, a2)
        | (`TyOfAmp (a0,a1,a2),`TyOfAmp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TyOfAmp (a0, a1, a2)
        | (`Package (a0,a1),`Package (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_type a1 b1 in `Package (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>ctyp)
        | (_,_) -> invalid_arg "map2 failure"
    method patt : patt -> patt -> patt=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Id (a0, a1)
        | (`Alias (a0,a1,a2),`Alias (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#alident a2 b2 in `Alias (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>patt)
        | (`Any a0,`Any b0) -> let a0 = self#loc a0 b0 in `Any a0
        | (`PaApp (a0,a1,a2),`PaApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#patt a2 b2 in `PaApp (a0, a1, a2)
        | (`Array (a0,a1),`Array (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in `Array (a0, a1)
        | (`PaCom (a0,a1,a2),`PaCom (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#patt a2 b2 in `PaCom (a0, a1, a2)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#patt a2 b2 in `Sem (a0, a1, a2)
        | ((#literal as a0),(#literal as b0)) -> (self#literal a0 b0 :>patt)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#patt a2 b2 in `Label (a0, a1, a2)
        | (`PaOlbi (a0,a1,a2,a3),`PaOlbi (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#patt a2 b2 in
            let a3 = self#meta_option (fun self  -> self#expr) a3 b3 in
            `PaOlbi (a0, a1, a2, a3)
        | (`PaOrp (a0,a1,a2),`PaOrp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#patt a2 b2 in `PaOrp (a0, a1, a2)
        | (`PaRng (a0,a1,a2),`PaRng (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#patt a2 b2 in `PaRng (a0, a1, a2)
        | (`PaRec (a0,a1),`PaRec (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in `PaRec (a0, a1)
        | (`PaEq (a0,a1,a2),`PaEq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#patt a2 b2 in `PaEq (a0, a1, a2)
        | (`PaTup (a0,a1),`PaTup (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in `PaTup (a0, a1)
        | (`PaTyc (a0,a1,a2),`PaTyc (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#ctyp a2 b2 in `PaTyc (a0, a1, a2)
        | (`PaTyp (a0,a1),`PaTyp (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `PaTyp (a0, a1)
        | (`PaVrn (a0,a1),`PaVrn (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `PaVrn (a0, a1)
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in `Lazy (a0, a1)
        | (`ModuleUnpack (a0,a1,a2),`ModuleUnpack (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#meta_option (fun self  -> self#ctyp) a2 b2 in
            `ModuleUnpack (a0, a1, a2)
        | (_,_) -> invalid_arg "map2 failure"
    method expr : expr -> expr -> expr=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Id (a0, a1)
        | (`ExAcc (a0,a1,a2),`ExAcc (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `ExAcc (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>expr)
        | (`ExApp (a0,a1,a2),`ExApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `ExApp (a0, a1, a2)
        | (`ExAre (a0,a1,a2),`ExAre (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `ExAre (a0, a1, a2)
        | (`Array (a0,a1),`Array (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `Array (a0, a1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `Sem (a0, a1, a2)
        | (`ExAsf a0,`ExAsf b0) -> let a0 = self#loc a0 b0 in `ExAsf a0
        | (`ExAsr (a0,a1),`ExAsr (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `ExAsr (a0, a1)
        | (`ExAss (a0,a1,a2),`ExAss (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `ExAss (a0, a1, a2)
        | (`For (a0,a1,a2,a3,a4,a5),`For (b0,b1,b2,b3,b4,b5)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in
            let a3 = self#expr a3 b3 in
            let a4 = self#direction_flag a4 b4 in
            let a5 = self#expr a5 b5 in `For (a0, a1, a2, a3, a4, a5)
        | (`Fun (a0,a1),`Fun (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#match_case a1 b1 in `Fun (a0, a1)
        | (`IfThenElse (a0,a1,a2,a3),`IfThenElse (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in
            let a3 = self#expr a3 b3 in `IfThenElse (a0, a1, a2, a3)
        | ((#literal as a0),(#literal as b0)) -> (self#literal a0 b0 :>expr)
        | (`Label (a0,a1,a2),`Label (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in `Label (a0, a1, a2)
        | (`Lazy (a0,a1),`Lazy (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `Lazy (a0, a1)
        | (`LetIn (a0,a1,a2,a3),`LetIn (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_flag a1 b1 in
            let a2 = self#binding a2 b2 in
            let a3 = self#expr a3 b3 in `LetIn (a0, a1, a2, a3)
        | (`LetModule (a0,a1,a2,a3),`LetModule (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_expr a2 b2 in
            let a3 = self#expr a3 b3 in `LetModule (a0, a1, a2, a3)
        | (`Match (a0,a1,a2),`Match (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#match_case a2 b2 in `Match (a0, a1, a2)
        | (`New (a0,a1),`New (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `New (a0, a1)
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#class_str_item a2 b2 in `Obj (a0, a1, a2)
        | (`OptLabl (a0,a1,a2),`OptLabl (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in `OptLabl (a0, a1, a2)
        | (`OvrInst (a0,a1),`OvrInst (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_binding a1 b1 in `OvrInst (a0, a1)
        | (`Record (a0,a1,a2),`Record (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_binding a1 b1 in
            let a2 = self#expr a2 b2 in `Record (a0, a1, a2)
        | (`Seq (a0,a1),`Seq (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `Seq (a0, a1)
        | (`Send (a0,a1,a2),`Send (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#alident a2 b2 in `Send (a0, a1, a2)
        | (`StringDot (a0,a1,a2),`StringDot (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `StringDot (a0, a1, a2)
        | (`Try (a0,a1,a2),`Try (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#match_case a2 b2 in `Try (a0, a1, a2)
        | (`ExTup (a0,a1),`ExTup (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `ExTup (a0, a1)
        | (`ExCom (a0,a1,a2),`ExCom (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `ExCom (a0, a1, a2)
        | (`Constraint_exp (a0,a1,a2),`Constraint_exp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#ctyp a2 b2 in `Constraint_exp (a0, a1, a2)
        | (`ExCoe (a0,a1,a2,a3),`ExCoe (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#ctyp a2 b2 in
            let a3 = self#ctyp a3 b3 in `ExCoe (a0, a1, a2, a3)
        | (`ExVrn (a0,a1),`ExVrn (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#string a1 b1 in `ExVrn (a0, a1)
        | (`While (a0,a1,a2),`While (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in
            let a2 = self#expr a2 b2 in `While (a0, a1, a2)
        | (`Let_open (a0,a1,a2),`Let_open (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#expr a2 b2 in `Let_open (a0, a1, a2)
        | (`LocalTypeFun (a0,a1,a2),`LocalTypeFun (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in `LocalTypeFun (a0, a1, a2)
        | (`Package_expr (a0,a1),`Package_expr (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_expr a1 b1 in `Package_expr (a0, a1)
        | (_,_) -> invalid_arg "map2 failure"
    method module_type : module_type -> module_type -> module_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Id (a0, a1)
        | (`MtFun (a0,a1,a2,a3),`MtFun (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in
            let a3 = self#module_type a3 b3 in `MtFun (a0, a1, a2, a3)
        | (`Sig (a0,a1),`Sig (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#sig_item a1 b1 in `Sig (a0, a1)
        | (`MtWit (a0,a1,a2),`MtWit (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_type a1 b1 in
            let a2 = self#with_constr a2 b2 in `MtWit (a0, a1, a2)
        | (`Of (a0,a1),`Of (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_expr a1 b1 in `Of (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>module_type)
        | (_,_) -> invalid_arg "map2 failure"
    method sig_item : sig_item -> sig_item -> sig_item=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in `Class (a0, a1)
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in `ClassType (a0, a1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#sig_item a1 b1 in
            let a2 = self#sig_item a2 b2 in `Sem (a0, a1, a2)
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in `Directive (a0, a1, a2)
        | (`Exception (a0,a1),`Exception (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Exception (a0, a1)
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#ctyp a2 b2 in
            let a3 = self#meta_list (fun self  -> self#string) a3 b3 in
            `External (a0, a1, a2, a3)
        | (`Include (a0,a1),`Include (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_type a1 b1 in `Include (a0, a1)
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in `Module (a0, a1, a2)
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_binding a1 b1 in `RecModule (a0, a1)
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in `ModuleType (a0, a1, a2)
        | (`Open (a0,a1),`Open (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Open (a0, a1)
        | (`Type (a0,a1),`Type (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Type (a0, a1)
        | (`Val (a0,a1,a2),`Val (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#ctyp a2 b2 in `Val (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>sig_item)
        | (_,_) -> invalid_arg "map2 failure"
    method with_constr : with_constr -> with_constr -> with_constr=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`TypeEq (a0,a1,a2),`TypeEq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TypeEq (a0, a1, a2)
        | (`ModuleEq (a0,a1,a2),`ModuleEq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#ident a2 b2 in `ModuleEq (a0, a1, a2)
        | (`TypeSubst (a0,a1,a2),`TypeSubst (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `TypeSubst (a0, a1, a2)
        | (`ModuleSubst (a0,a1,a2),`ModuleSubst (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#ident a2 b2 in `ModuleSubst (a0, a1, a2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#with_constr a1 b1 in
            let a2 = self#with_constr a2 b2 in `And (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>with_constr)
        | (_,_) -> invalid_arg "map2 failure"
    method binding : binding -> binding -> binding=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#binding a1 b1 in
            let a2 = self#binding a2 b2 in `And (a0, a1, a2)
        | (`Bind (a0,a1,a2),`Bind (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#expr a2 b2 in `Bind (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>binding)
        | (_,_) -> invalid_arg "map2 failure"
    method rec_binding : rec_binding -> rec_binding -> rec_binding=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_binding a1 b1 in
            let a2 = self#rec_binding a2 b2 in `Sem (a0, a1, a2)
        | (`RecBind (a0,a1,a2),`RecBind (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in
            let a2 = self#expr a2 b2 in `RecBind (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>rec_binding)
        | (_,_) -> invalid_arg "map2 failure"
    method module_binding :
      module_binding -> module_binding -> module_binding=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_binding a1 b1 in
            let a2 = self#module_binding a2 b2 in `And (a0, a1, a2)
        | (`ModuleBind (a0,a1,a2,a3),`ModuleBind (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in
            let a3 = self#module_expr a3 b3 in `ModuleBind (a0, a1, a2, a3)
        | (`ModuleConstraint (a0,a1,a2),`ModuleConstraint (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in `ModuleConstraint (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>module_binding)
        | (_,_) -> invalid_arg "map2 failure"
    method match_case : match_case -> match_case -> match_case=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`McOr (a0,a1,a2),`McOr (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#match_case a1 b1 in
            let a2 = self#match_case a2 b2 in `McOr (a0, a1, a2)
        | (`Case (a0,a1,a2,a3),`Case (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#expr a2 b2 in
            let a3 = self#expr a3 b3 in `Case (a0, a1, a2, a3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>match_case)
        | (_,_) -> invalid_arg "map2 failure"
    method module_expr : module_expr -> module_expr -> module_expr=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Id (a0,a1),`Id (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Id (a0, a1)
        | (`MeApp (a0,a1,a2),`MeApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_expr a1 b1 in
            let a2 = self#module_expr a2 b2 in `MeApp (a0, a1, a2)
        | (`Functor (a0,a1,a2,a3),`Functor (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in
            let a3 = self#module_expr a3 b3 in `Functor (a0, a1, a2, a3)
        | (`Struct (a0,a1),`Struct (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#str_item a1 b1 in `Struct (a0, a1)
        | (`ModuleExprConstraint (a0,a1,a2),`ModuleExprConstraint (b0,b1,b2))
            ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_expr a1 b1 in
            let a2 = self#module_type a2 b2 in
            `ModuleExprConstraint (a0, a1, a2)
        | (`PackageModule (a0,a1),`PackageModule (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `PackageModule (a0, a1)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>module_expr)
        | (_,_) -> invalid_arg "map2 failure"
    method str_item : str_item -> str_item -> str_item=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Class (a0,a1),`Class (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_expr a1 b1 in `Class (a0, a1)
        | (`ClassType (a0,a1),`ClassType (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in `ClassType (a0, a1)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#str_item a1 b1 in
            let a2 = self#str_item a2 b2 in `Sem (a0, a1, a2)
        | (`Directive (a0,a1,a2),`Directive (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#expr a2 b2 in `Directive (a0, a1, a2)
        | (`Exception (a0,a1,a2),`Exception (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#meta_option (fun self  -> self#ident) a2 b2 in
            `Exception (a0, a1, a2)
        | (`StExp (a0,a1),`StExp (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `StExp (a0, a1)
        | (`External (a0,a1,a2,a3),`External (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#ctyp a2 b2 in
            let a3 = self#meta_list (fun self  -> self#string) a3 b3 in
            `External (a0, a1, a2, a3)
        | (`Include (a0,a1),`Include (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_expr a1 b1 in `Include (a0, a1)
        | (`Module (a0,a1,a2),`Module (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_expr a2 b2 in `Module (a0, a1, a2)
        | (`RecModule (a0,a1),`RecModule (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#module_binding a1 b1 in `RecModule (a0, a1)
        | (`ModuleType (a0,a1,a2),`ModuleType (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#auident a1 b1 in
            let a2 = self#module_type a2 b2 in `ModuleType (a0, a1, a2)
        | (`Open (a0,a1),`Open (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ident a1 b1 in `Open (a0, a1)
        | (`Type (a0,a1),`Type (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in `Type (a0, a1)
        | (`Value (a0,a1,a2),`Value (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_flag a1 b1 in
            let a2 = self#binding a2 b2 in `Value (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>str_item)
        | (_,_) -> invalid_arg "map2 failure"
    method class_type : class_type -> class_type -> class_type=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`CtCon (a0,a1,a2,a3),`CtCon (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#virtual_flag a1 b1 in
            let a2 = self#ident a2 b2 in
            let a3 = self#ctyp a3 b3 in `CtCon (a0, a1, a2, a3)
        | (`CtFun (a0,a1,a2),`CtFun (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#class_type a2 b2 in `CtFun (a0, a1, a2)
        | (`CtSig (a0,a1,a2),`CtSig (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#class_sig_item a2 b2 in `CtSig (a0, a1, a2)
        | (`CtAnd (a0,a1,a2),`CtAnd (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in
            let a2 = self#class_type a2 b2 in `CtAnd (a0, a1, a2)
        | (`CtCol (a0,a1,a2),`CtCol (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in
            let a2 = self#class_type a2 b2 in `CtCol (a0, a1, a2)
        | (`CtEq (a0,a1,a2),`CtEq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in
            let a2 = self#class_type a2 b2 in `CtEq (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>class_type)
        | (_,_) -> invalid_arg "map2 failure"
    method class_sig_item :
      class_sig_item -> class_sig_item -> class_sig_item=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Eq (a0, a1, a2)
        | (`Sem (a0,a1,a2),`Sem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_sig_item a1 b1 in
            let a2 = self#class_sig_item a2 b2 in `Sem (a0, a1, a2)
        | (`Inherit (a0,a1),`Inherit (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_type a1 b1 in `Inherit (a0, a1)
        | (`Method (a0,a1,a2,a3),`Method (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#private_flag a2 b2 in
            let a3 = self#ctyp a3 b3 in `Method (a0, a1, a2, a3)
        | (`CgVal (a0,a1,a2,a3,a4),`CgVal (b0,b1,b2,b3,b4)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#mutable_flag a2 b2 in
            let a3 = self#virtual_flag a3 b3 in
            let a4 = self#ctyp a4 b4 in `CgVal (a0, a1, a2, a3, a4)
        | (`CgVir (a0,a1,a2,a3),`CgVir (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#private_flag a2 b2 in
            let a3 = self#ctyp a3 b3 in `CgVir (a0, a1, a2, a3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>class_sig_item)
        | (_,_) -> invalid_arg "map2 failure"
    method class_expr : class_expr -> class_expr -> class_expr=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`CeApp (a0,a1,a2),`CeApp (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_expr a1 b1 in
            let a2 = self#expr a2 b2 in `CeApp (a0, a1, a2)
        | (`CeCon (a0,a1,a2,a3),`CeCon (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#virtual_flag a1 b1 in
            let a2 = self#ident a2 b2 in
            let a3 = self#ctyp a3 b3 in `CeCon (a0, a1, a2, a3)
        | (`CeFun (a0,a1,a2),`CeFun (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#class_expr a2 b2 in `CeFun (a0, a1, a2)
        | (`CeLet (a0,a1,a2,a3),`CeLet (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#rec_flag a1 b1 in
            let a2 = self#binding a2 b2 in
            let a3 = self#class_expr a3 b3 in `CeLet (a0, a1, a2, a3)
        | (`Obj (a0,a1,a2),`Obj (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#patt a1 b1 in
            let a2 = self#class_str_item a2 b2 in `Obj (a0, a1, a2)
        | (`CeTyc (a0,a1,a2),`CeTyc (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_expr a1 b1 in
            let a2 = self#class_type a2 b2 in `CeTyc (a0, a1, a2)
        | (`And (a0,a1,a2),`And (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_expr a1 b1 in
            let a2 = self#class_expr a2 b2 in `And (a0, a1, a2)
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_expr a1 b1 in
            let a2 = self#class_expr a2 b2 in `Eq (a0, a1, a2)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>class_expr)
        | (_,_) -> invalid_arg "map2 failure"
    method class_str_item :
      class_str_item -> class_str_item -> class_str_item=
      fun a0  b0  ->
        match (a0, b0) with
        | (`Nil a0,`Nil b0) -> let a0 = self#loc a0 b0 in `Nil a0
        | (`CrSem (a0,a1,a2),`CrSem (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#class_str_item a1 b1 in
            let a2 = self#class_str_item a2 b2 in `CrSem (a0, a1, a2)
        | (`Eq (a0,a1,a2),`Eq (b0,b1,b2)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#ctyp a1 b1 in
            let a2 = self#ctyp a2 b2 in `Eq (a0, a1, a2)
        | (`Inherit (a0,a1,a2,a3),`Inherit (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#override_flag a1 b1 in
            let a2 = self#class_expr a2 b2 in
            let a3 = self#meta_option (fun self  -> self#alident) a3 b3 in
            `Inherit (a0, a1, a2, a3)
        | (`Initializer (a0,a1),`Initializer (b0,b1)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#expr a1 b1 in `Initializer (a0, a1)
        | (`CrMth (a0,a1,a2,a3,a4,a5),`CrMth (b0,b1,b2,b3,b4,b5)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#override_flag a2 b2 in
            let a3 = self#private_flag a3 b3 in
            let a4 = self#expr a4 b4 in
            let a5 = self#ctyp a5 b5 in `CrMth (a0, a1, a2, a3, a4, a5)
        | (`CrVal (a0,a1,a2,a3,a4),`CrVal (b0,b1,b2,b3,b4)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#override_flag a2 b2 in
            let a3 = self#mutable_flag a3 b3 in
            let a4 = self#expr a4 b4 in `CrVal (a0, a1, a2, a3, a4)
        | (`CrVir (a0,a1,a2,a3),`CrVir (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#private_flag a2 b2 in
            let a3 = self#ctyp a3 b3 in `CrVir (a0, a1, a2, a3)
        | (`CrVvr (a0,a1,a2,a3),`CrVvr (b0,b1,b2,b3)) ->
            let a0 = self#loc a0 b0 in
            let a1 = self#alident a1 b1 in
            let a2 = self#mutable_flag a2 b2 in
            let a3 = self#ctyp a3 b3 in `CrVvr (a0, a1, a2, a3)
        | ((#ant as a0),(#ant as b0)) -> (self#ant a0 b0 :>class_str_item)
        | (_,_) -> invalid_arg "map2 failure"
    method fanloc_t : FanLoc.t -> FanLoc.t -> FanLoc.t= self#unknown
  end
module MExpr =
  struct
    let meta_int _loc i = `Int (_loc, (string_of_int i))
    let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))
    let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))
    let meta_nativeint _loc i = `NativeInt (_loc, (Nativeint.to_string i))
    let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))
    let meta_string _loc i = `Str (_loc, (safe_string_escaped i))
    let meta_char _loc i = `Chr (_loc, (Char.escaped i))
    let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))
    let meta_bool _loc =
      function
      | true  -> `Id (_loc, (`Lid (_loc, "true")))
      | false  -> `Id (_loc, (`Lid (_loc, "false")))
    let meta_ref mf_a _loc i =
      `Record
        (_loc,
          (`RecBind (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))),
          (`Nil _loc))
    let mklist loc =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
            `ExApp
              (_loc, (`ExApp (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
                (loop false el)) in
      loop true
    let mkarray loc arr =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
            `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
      let items = arr |> Array.to_list in loop true items
    let meta_list mf_a _loc ls =
      mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
    let meta_array mf_a _loc ls =
      mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
    let meta_option mf_a _loc =
      function
      | None  -> `Id (_loc, (`Uid (_loc, "None")))
      | Some x ->
          `ExApp (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
    let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
      (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
      invalid_arg "meta_arrow not implemented"
  end
module MPatt =
  struct
    let meta_int _loc i = `Int (_loc, (string_of_int i))
    let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))
    let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))
    let meta_nativeint _loc i = `NativeInt (_loc, (Nativeint.to_string i))
    let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))
    let meta_string _loc i = `Str (_loc, (safe_string_escaped i))
    let meta_char _loc i = `Chr (_loc, (Char.escaped i))
    let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))
    let meta_bool _loc =
      function
      | true  -> `Id (_loc, (`Lid (_loc, "true")))
      | false  -> `Id (_loc, (`Lid (_loc, "false")))
    let meta_ref mf_a _loc i =
      `PaRec
        (_loc,
          (`PaEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))
    let mklist loc =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
            `PaApp
              (_loc, (`PaApp (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
                (loop false el)) in
      loop true
    let mkarray loc arr =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
            `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
      let items = arr |> Array.to_list in loop true items
    let meta_list mf_a _loc ls =
      mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
    let meta_array mf_a _loc ls =
      mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
    let meta_option mf_a _loc =
      function
      | None  -> `Id (_loc, (`Uid (_loc, "None")))
      | Some x ->
          `PaApp (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
    let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
      (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
      invalid_arg "meta_arrow not implemented"
  end
module Make(MetaLoc:META_LOC) =
  struct
    module Expr =
      struct
        open MExpr
        let meta_loc = MetaLoc.meta_loc_expr
        let meta_ant: 'loc -> ant -> 'result =
          fun _loc  (`Ant (a0,a1))  -> `Ant (a0, a1)
        let meta_literal: 'loc -> literal -> 'result =
          fun _loc  ->
            function
            | `Chr (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Chr")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Int")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int32 (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Int32")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int64 (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Int64")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Flo (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Flo")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `NativeInt (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "NativeInt")),
                         (meta_loc _loc a0))), (meta_string _loc a1))
            | `Str (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Str")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
        let meta_rec_flag: 'loc -> rec_flag -> 'result =
          fun _loc  ->
            function
            | `Recursive a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Recursive")), (meta_loc _loc a0))
            | `ReNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "ReNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_direction_flag: 'loc -> direction_flag -> 'result =
          fun _loc  ->
            function
            | `To a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "To")), (meta_loc _loc a0))
            | `Downto a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Downto")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_mutable_flag: 'loc -> mutable_flag -> 'result =
          fun _loc  ->
            function
            | `Mutable a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Mutable")), (meta_loc _loc a0))
            | `MuNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "MuNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_private_flag: 'loc -> private_flag -> 'result =
          fun _loc  ->
            function
            | `Private a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Private")), (meta_loc _loc a0))
            | `PrNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "PrNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_virtual_flag: 'loc -> virtual_flag -> 'result =
          fun _loc  ->
            function
            | `Virtual a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Virtual")), (meta_loc _loc a0))
            | `ViNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "ViNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_override_flag: 'loc -> override_flag -> 'result =
          fun _loc  ->
            function
            | `Override a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Override")), (meta_loc _loc a0))
            | `OvNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "OvNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_row_var_flag: 'loc -> row_var_flag -> 'result =
          fun _loc  ->
            function
            | `RowVar a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "RowVar")), (meta_loc _loc a0))
            | `RvNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "RvNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_position_flag: 'loc -> position_flag -> 'result =
          fun _loc  ->
            function
            | `Positive a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Positive")), (meta_loc _loc a0))
            | `Negative a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Negative")), (meta_loc _loc a0))
            | `Normal a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Normal")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_meta_bool: 'loc -> meta_bool -> 'result =
          fun _loc  ->
            function
            | `True a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "True")), (meta_loc _loc a0))
            | `False a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "False")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_meta_option :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_option -> 'result=
          fun mf_a  _loc  ->
            function
            | `None a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "None")), (meta_loc _loc a0))
            | `Some a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Some")), (mf_a _loc a0))
            | `Ant (a0,a1) -> `Ant (a0, a1)
        let rec meta_meta_list :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_list -> 'result=
          fun mf_a  _loc  ->
            function
            | `LNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "LNil")), (meta_loc _loc a0))
            | `LCons (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "LCons")), (mf_a _loc a0))),
                    (meta_meta_list mf_a _loc a1))
            | `Ant (a0,a1) -> `Ant (a0, a1)
        let meta_alident: 'loc -> alident -> 'result =
          fun _loc  ->
            function
            | `Lid (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Lid")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_auident: 'loc -> auident -> 'result =
          fun _loc  ->
            function
            | `Uid (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Uid")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_aident: 'loc -> aident -> 'result =
          fun _loc  ->
            function
            | #alident as a0 -> (meta_alident _loc a0 :>'result)
            | #auident as a0 -> (meta_auident _loc a0 :>'result)
        let meta_astring: 'loc -> astring -> 'result =
          fun _loc  ->
            function
            | `C (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "C")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let rec meta_ident: 'loc -> ident -> 'result =
          fun _loc  ->
            function
            | `IdAcc (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "IdAcc")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `IdApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "IdApp")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | #alident as a0 -> (meta_alident _loc a0 :>'result)
            | #auident as a0 -> (meta_auident _loc a0 :>'result)
        let rec meta_ctyp: 'loc -> ctyp -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Alias (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Alias")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Any a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Any")), (meta_loc _loc a0))
            | `TyApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyApp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyArr (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyArr")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ClassPath (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ClassPath")),
                         (meta_loc _loc a0))), (meta_ident _loc a1))
            | `TyLab (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyLab")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | `Id (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `TyMan (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyMan")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyDcl (a0,a1,a2,a3,a4) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`ExApp
                                      (_loc, (`ExVrn (_loc, "TyDcl")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_list meta_ctyp _loc a2))),
                         (meta_ctyp _loc a3))),
                    (meta_list
                       (fun _loc  (a0,a1)  ->
                          `ExTup
                            (_loc,
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a0),
                                   (meta_ctyp _loc a1))))) _loc a4))
            | `TyObj (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyObj")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_row_var_flag _loc a2))
            | `TyOlb (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyOlb")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyPol (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyPol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyTypePol (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyTypePol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Quote (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Quote")),
                              (meta_loc _loc a0))),
                         (meta_position_flag _loc a1))),
                    (meta_meta_option meta_alident _loc a2))
            | `TyRec (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "TyRec")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `TyCol (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyCol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TySem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TySem")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Com (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Com")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Sum (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Sum")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Of (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Of")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `And (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "And")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Or (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Or")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Private (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Private")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Mutable (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Mutable")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Tup (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Tup")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Sta (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sta")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyVrn (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "TyVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `TyVrnEq (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "TyVrnEq")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `TyVrnSup (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "TyVrnSup")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `TyVrnInf (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "TyVrnInf")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `TyVrnInfSup (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyVrnInfSup")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyAmp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyAmp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyOfAmp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TyOfAmp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Package (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Package")), (meta_loc _loc a0))),
                    (meta_module_type _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_patt: 'loc -> patt -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Alias (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Alias")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_alident _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
            | `Any a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Any")), (meta_loc _loc a0))
            | `PaApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaApp")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `Array (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Array")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaCom (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaCom")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | #literal as a0 -> (meta_literal _loc a0 :>'result)
            | `Label (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Label")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_patt _loc a2))
            | `PaOlbi (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "PaOlbi")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_patt _loc a2))),
                    (meta_meta_option meta_expr _loc a3))
            | `PaOrp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaOrp")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `PaRng (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaRng")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `PaRec (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "PaRec")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaEq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaEq")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_patt _loc a2))
            | `PaTup (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "PaTup")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaTyc (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "PaTyc")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_ctyp _loc a2))
            | `PaTyp (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "PaTyp")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `PaVrn (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "PaVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Lazy (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Lazy")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `ModuleUnpack (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleUnpack")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_meta_option meta_ctyp _loc a2))
        and meta_expr: 'loc -> expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `ExAcc (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ExAcc")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
            | `ExApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ExApp")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `ExAre (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ExAre")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Array (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Array")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `ExAsf a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "ExAsf")), (meta_loc _loc a0))
            | `ExAsr (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ExAsr")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `ExAss (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ExAss")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `For (a0,a1,a2,a3,a4,a5) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`ExApp
                                      (_loc,
                                        (`ExApp
                                           (_loc, (`ExVrn (_loc, "For")),
                                             (meta_loc _loc a0))),
                                        (meta_alident _loc a1))),
                                   (meta_expr _loc a2))),
                              (meta_expr _loc a3))),
                         (meta_direction_flag _loc a4))),
                    (meta_expr _loc a5))
            | `Fun (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Fun")), (meta_loc _loc a0))),
                    (meta_match_case _loc a1))
            | `IfThenElse (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "IfThenElse")),
                                   (meta_loc _loc a0))), (meta_expr _loc a1))),
                         (meta_expr _loc a2))), (meta_expr _loc a3))
            | #literal as a0 -> (meta_literal _loc a0 :>'result)
            | `Label (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Label")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Lazy (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Lazy")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `LetIn (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "LetIn")),
                                   (meta_loc _loc a0))),
                              (meta_rec_flag _loc a1))),
                         (meta_binding _loc a2))), (meta_expr _loc a3))
            | `LetModule (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "LetModule")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_expr _loc a2))), (meta_expr _loc a3))
            | `Match (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Match")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_match_case _loc a2))
            | `New (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "New")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Obj (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Obj")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_str_item _loc a2))
            | `OptLabl (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "OptLabl")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `OvrInst (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "OvrInst")), (meta_loc _loc a0))),
                    (meta_rec_binding _loc a1))
            | `Record (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Record")),
                              (meta_loc _loc a0))),
                         (meta_rec_binding _loc a1))), (meta_expr _loc a2))
            | `Seq (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Seq")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `Send (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Send")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_alident _loc a2))
            | `StringDot (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "StringDot")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Try (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Try")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_match_case _loc a2))
            | `ExTup (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ExTup")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `ExCom (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ExCom")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Constraint_exp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Constraint_exp")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_ctyp _loc a2))
            | `ExCoe (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "ExCoe")),
                                   (meta_loc _loc a0))), (meta_expr _loc a1))),
                         (meta_ctyp _loc a2))), (meta_ctyp _loc a3))
            | `ExVrn (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ExVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `While (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "While")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Let_open (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Let_open")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_expr _loc a2))
            | `LocalTypeFun (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "LocalTypeFun")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Package_expr (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Package_expr")),
                         (meta_loc _loc a0))), (meta_module_expr _loc a1))
        and meta_module_type: 'loc -> module_type -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `MtFun (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "MtFun")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_type _loc a3))
            | `Sig (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Sig")), (meta_loc _loc a0))),
                    (meta_sig_item _loc a1))
            | `MtWit (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "MtWit")),
                              (meta_loc _loc a0))),
                         (meta_module_type _loc a1))),
                    (meta_with_constr _loc a2))
            | `Of (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Of")), (meta_loc _loc a0))),
                    (meta_module_expr _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_sig_item: 'loc -> sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Class")), (meta_loc _loc a0))),
                    (meta_class_type _loc a1))
            | `ClassType (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ClassType")),
                         (meta_loc _loc a0))), (meta_class_type _loc a1))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_sig_item _loc a1))),
                    (meta_sig_item _loc a2))
            | `Directive (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Directive")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Exception (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Exception")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `External (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "External")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_ctyp _loc a2))),
                    (meta_meta_list meta_string _loc a3))
            | `Include (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Include")), (meta_loc _loc a0))),
                    (meta_module_type _loc a1))
            | `Module (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Module")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `RecModule (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "RecModule")),
                         (meta_loc _loc a0))), (meta_module_binding _loc a1))
            | `ModuleType (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleType")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `Open (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Open")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Type (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Type")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Val (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Val")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_with_constr: 'loc -> with_constr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `TypeEq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TypeEq")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ModuleEq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleEq")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `TypeSubst (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "TypeSubst")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ModuleSubst (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleSubst")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `And (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_with_constr _loc a1))),
                    (meta_with_constr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_binding: 'loc -> binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "And")),
                              (meta_loc _loc a0))), (meta_binding _loc a1))),
                    (meta_binding _loc a2))
            | `Bind (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Bind")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_rec_binding: 'loc -> rec_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))),
                         (meta_rec_binding _loc a1))),
                    (meta_rec_binding _loc a2))
            | `RecBind (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "RecBind")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_module_binding: 'loc -> module_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_module_binding _loc a1))),
                    (meta_module_binding _loc a2))
            | `ModuleBind (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "ModuleBind")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_expr _loc a3))
            | `ModuleConstraint (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleConstraint")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_match_case: 'loc -> match_case -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `McOr (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "McOr")),
                              (meta_loc _loc a0))),
                         (meta_match_case _loc a1))),
                    (meta_match_case _loc a2))
            | `Case (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "Case")),
                                   (meta_loc _loc a0))), (meta_patt _loc a1))),
                         (meta_expr _loc a2))), (meta_expr _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_module_expr: 'loc -> module_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp (_loc, (`ExVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `MeApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "MeApp")),
                              (meta_loc _loc a0))),
                         (meta_module_expr _loc a1))),
                    (meta_module_expr _loc a2))
            | `Functor (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "Functor")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_expr _loc a3))
            | `Struct (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Struct")), (meta_loc _loc a0))),
                    (meta_str_item _loc a1))
            | `ModuleExprConstraint (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleExprConstraint")),
                              (meta_loc _loc a0))),
                         (meta_module_expr _loc a1))),
                    (meta_module_type _loc a2))
            | `PackageModule (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "PackageModule")),
                         (meta_loc _loc a0))), (meta_expr _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_str_item: 'loc -> str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Class")), (meta_loc _loc a0))),
                    (meta_class_expr _loc a1))
            | `ClassType (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "ClassType")),
                         (meta_loc _loc a0))), (meta_class_type _loc a1))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_str_item _loc a1))),
                    (meta_str_item _loc a2))
            | `Directive (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Directive")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Exception (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Exception")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_meta_option meta_ident _loc a2))
            | `StExp (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "StExp")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `External (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "External")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_ctyp _loc a2))),
                    (meta_meta_list meta_string _loc a3))
            | `Include (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Include")), (meta_loc _loc a0))),
                    (meta_module_expr _loc a1))
            | `Module (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Module")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_expr _loc a2))
            | `RecModule (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "RecModule")),
                         (meta_loc _loc a0))), (meta_module_binding _loc a1))
            | `ModuleType (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "ModuleType")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `Open (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Open")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Type (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Type")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Value (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Value")),
                              (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                    (meta_binding _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_type: 'loc -> class_type -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CtCon (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CtCon")),
                                   (meta_loc _loc a0))),
                              (meta_virtual_flag _loc a1))),
                         (meta_ident _loc a2))), (meta_ctyp _loc a3))
            | `CtFun (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CtFun")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_class_type _loc a2))
            | `CtSig (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CtSig")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_class_sig_item _loc a2))
            | `CtAnd (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CtAnd")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | `CtCol (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CtCol")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | `CtEq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CtEq")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Eq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Sem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Sem")),
                              (meta_loc _loc a0))),
                         (meta_class_sig_item _loc a1))),
                    (meta_class_sig_item _loc a2))
            | `Inherit (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Inherit")), (meta_loc _loc a0))),
                    (meta_class_type _loc a1))
            | `Method (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "Method")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | `CgVal (a0,a1,a2,a3,a4) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`ExApp
                                      (_loc, (`ExVrn (_loc, "CgVal")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_mutable_flag _loc a2))),
                         (meta_virtual_flag _loc a3))), (meta_ctyp _loc a4))
            | `CgVir (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CgVir")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_expr: 'loc -> class_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CeApp (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CeApp")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))), (meta_expr _loc a2))
            | `CeCon (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CeCon")),
                                   (meta_loc _loc a0))),
                              (meta_virtual_flag _loc a1))),
                         (meta_ident _loc a2))), (meta_ctyp _loc a3))
            | `CeFun (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CeFun")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_expr _loc a2))
            | `CeLet (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CeLet")),
                                   (meta_loc _loc a0))),
                              (meta_rec_flag _loc a1))),
                         (meta_binding _loc a2))), (meta_class_expr _loc a3))
            | `Obj (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Obj")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_str_item _loc a2))
            | `CeTyc (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CeTyc")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_type _loc a2))
            | `And (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_expr _loc a2))
            | `Eq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_str_item: 'loc -> class_str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CrSem (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "CrSem")),
                              (meta_loc _loc a0))),
                         (meta_class_str_item _loc a1))),
                    (meta_class_str_item _loc a2))
            | `Eq (a0,a1,a2) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc, (`ExVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Inherit (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "Inherit")),
                                   (meta_loc _loc a0))),
                              (meta_override_flag _loc a1))),
                         (meta_class_expr _loc a2))),
                    (meta_meta_option meta_alident _loc a3))
            | `Initializer (a0,a1) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc, (`ExVrn (_loc, "Initializer")),
                         (meta_loc _loc a0))), (meta_expr _loc a1))
            | `CrMth (a0,a1,a2,a3,a4,a5) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`ExApp
                                      (_loc,
                                        (`ExApp
                                           (_loc, (`ExVrn (_loc, "CrMth")),
                                             (meta_loc _loc a0))),
                                        (meta_alident _loc a1))),
                                   (meta_override_flag _loc a2))),
                              (meta_private_flag _loc a3))),
                         (meta_expr _loc a4))), (meta_ctyp _loc a5))
            | `CrVal (a0,a1,a2,a3,a4) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`ExApp
                                      (_loc, (`ExVrn (_loc, "CrVal")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_override_flag _loc a2))),
                         (meta_mutable_flag _loc a3))), (meta_expr _loc a4))
            | `CrVir (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CrVir")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | `CrVvr (a0,a1,a2,a3) ->
                `ExApp
                  (_loc,
                    (`ExApp
                       (_loc,
                         (`ExApp
                            (_loc,
                              (`ExApp
                                 (_loc, (`ExVrn (_loc, "CrVvr")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
      end
    module Patt =
      struct
        open MPatt
        let meta_loc = MetaLoc.meta_loc_patt
        let meta_ant: 'loc -> ant -> 'result =
          fun _loc  (`Ant (a0,a1))  -> `Ant (a0, a1)
        let meta_literal: 'loc -> literal -> 'result =
          fun _loc  ->
            function
            | `Chr (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Chr")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Int")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int32 (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Int32")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Int64 (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Int64")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Flo (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Flo")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `NativeInt (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "NativeInt")),
                         (meta_loc _loc a0))), (meta_string _loc a1))
            | `Str (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Str")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
        let meta_rec_flag: 'loc -> rec_flag -> 'result =
          fun _loc  ->
            function
            | `Recursive a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Recursive")), (meta_loc _loc a0))
            | `ReNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "ReNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_direction_flag: 'loc -> direction_flag -> 'result =
          fun _loc  ->
            function
            | `To a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "To")), (meta_loc _loc a0))
            | `Downto a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Downto")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_mutable_flag: 'loc -> mutable_flag -> 'result =
          fun _loc  ->
            function
            | `Mutable a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Mutable")), (meta_loc _loc a0))
            | `MuNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "MuNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_private_flag: 'loc -> private_flag -> 'result =
          fun _loc  ->
            function
            | `Private a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Private")), (meta_loc _loc a0))
            | `PrNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "PrNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_virtual_flag: 'loc -> virtual_flag -> 'result =
          fun _loc  ->
            function
            | `Virtual a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Virtual")), (meta_loc _loc a0))
            | `ViNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "ViNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_override_flag: 'loc -> override_flag -> 'result =
          fun _loc  ->
            function
            | `Override a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Override")), (meta_loc _loc a0))
            | `OvNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "OvNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_row_var_flag: 'loc -> row_var_flag -> 'result =
          fun _loc  ->
            function
            | `RowVar a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "RowVar")), (meta_loc _loc a0))
            | `RvNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "RvNil")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_position_flag: 'loc -> position_flag -> 'result =
          fun _loc  ->
            function
            | `Positive a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Positive")), (meta_loc _loc a0))
            | `Negative a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Negative")), (meta_loc _loc a0))
            | `Normal a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Normal")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_meta_bool: 'loc -> meta_bool -> 'result =
          fun _loc  ->
            function
            | `True a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "True")), (meta_loc _loc a0))
            | `False a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "False")), (meta_loc _loc a0))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_meta_option :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_option -> 'result=
          fun mf_a  _loc  ->
            function
            | `None a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "None")), (meta_loc _loc a0))
            | `Some a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Some")), (mf_a _loc a0))
            | `Ant (a0,a1) -> `Ant (a0, a1)
        let rec meta_meta_list :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_list -> 'result=
          fun mf_a  _loc  ->
            function
            | `LNil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "LNil")), (meta_loc _loc a0))
            | `LCons (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "LCons")), (mf_a _loc a0))),
                    (meta_meta_list mf_a _loc a1))
            | `Ant (a0,a1) -> `Ant (a0, a1)
        let meta_alident: 'loc -> alident -> 'result =
          fun _loc  ->
            function
            | `Lid (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Lid")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_auident: 'loc -> auident -> 'result =
          fun _loc  ->
            function
            | `Uid (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Uid")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let meta_aident: 'loc -> aident -> 'result =
          fun _loc  ->
            function
            | #alident as a0 -> (meta_alident _loc a0 :>'result)
            | #auident as a0 -> (meta_auident _loc a0 :>'result)
        let meta_astring: 'loc -> astring -> 'result =
          fun _loc  ->
            function
            | `C (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "C")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        let rec meta_ident: 'loc -> ident -> 'result =
          fun _loc  ->
            function
            | `IdAcc (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "IdAcc")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `IdApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "IdApp")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | #alident as a0 -> (meta_alident _loc a0 :>'result)
            | #auident as a0 -> (meta_auident _loc a0 :>'result)
        let rec meta_ctyp: 'loc -> ctyp -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Alias (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Alias")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Any a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Any")), (meta_loc _loc a0))
            | `TyApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyApp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyArr (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyArr")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ClassPath (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ClassPath")),
                         (meta_loc _loc a0))), (meta_ident _loc a1))
            | `TyLab (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyLab")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | `Id (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `TyMan (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyMan")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyDcl (a0,a1,a2,a3,a4) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc,
                                   (`PaApp
                                      (_loc, (`PaVrn (_loc, "TyDcl")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_list meta_ctyp _loc a2))),
                         (meta_ctyp _loc a3))),
                    (meta_list
                       (fun _loc  (a0,a1)  ->
                          `PaTup
                            (_loc,
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a0),
                                   (meta_ctyp _loc a1))))) _loc a4))
            | `TyObj (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyObj")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_row_var_flag _loc a2))
            | `TyOlb (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyOlb")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyPol (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyPol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyTypePol (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyTypePol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Quote (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Quote")),
                              (meta_loc _loc a0))),
                         (meta_position_flag _loc a1))),
                    (meta_meta_option meta_alident _loc a2))
            | `TyRec (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "TyRec")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `TyCol (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyCol")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TySem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TySem")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Com (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Com")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Sum (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Sum")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Of (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Of")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `And (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "And")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Or (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Or")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Private (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Private")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Mutable (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Mutable")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Tup (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Tup")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Sta (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sta")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyVrn (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "TyVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `TyVrnEq (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "TyVrnEq")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `TyVrnSup (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "TyVrnSup")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `TyVrnInf (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "TyVrnInf")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `TyVrnInfSup (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyVrnInfSup")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyAmp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyAmp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `TyOfAmp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TyOfAmp")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `Package (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Package")), (meta_loc _loc a0))),
                    (meta_module_type _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_patt: 'loc -> patt -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Alias (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Alias")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_alident _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
            | `Any a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Any")), (meta_loc _loc a0))
            | `PaApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaApp")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `Array (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Array")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaCom (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaCom")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | #literal as a0 -> (meta_literal _loc a0 :>'result)
            | `Label (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Label")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_patt _loc a2))
            | `PaOlbi (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "PaOlbi")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_patt _loc a2))),
                    (meta_meta_option meta_expr _loc a3))
            | `PaOrp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaOrp")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `PaRng (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaRng")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_patt _loc a2))
            | `PaRec (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "PaRec")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaEq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaEq")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_patt _loc a2))
            | `PaTup (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "PaTup")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `PaTyc (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "PaTyc")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_ctyp _loc a2))
            | `PaTyp (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "PaTyp")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `PaVrn (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "PaVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `Lazy (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Lazy")), (meta_loc _loc a0))),
                    (meta_patt _loc a1))
            | `ModuleUnpack (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleUnpack")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_meta_option meta_ctyp _loc a2))
        and meta_expr: 'loc -> expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `ExAcc (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ExAcc")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
            | `ExApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ExApp")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `ExAre (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ExAre")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Array (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Array")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `ExAsf a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "ExAsf")), (meta_loc _loc a0))
            | `ExAsr (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ExAsr")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `ExAss (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ExAss")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `For (a0,a1,a2,a3,a4,a5) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc,
                                   (`PaApp
                                      (_loc,
                                        (`PaApp
                                           (_loc, (`PaVrn (_loc, "For")),
                                             (meta_loc _loc a0))),
                                        (meta_alident _loc a1))),
                                   (meta_expr _loc a2))),
                              (meta_expr _loc a3))),
                         (meta_direction_flag _loc a4))),
                    (meta_expr _loc a5))
            | `Fun (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Fun")), (meta_loc _loc a0))),
                    (meta_match_case _loc a1))
            | `IfThenElse (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "IfThenElse")),
                                   (meta_loc _loc a0))), (meta_expr _loc a1))),
                         (meta_expr _loc a2))), (meta_expr _loc a3))
            | #literal as a0 -> (meta_literal _loc a0 :>'result)
            | `Label (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Label")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Lazy (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Lazy")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `LetIn (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "LetIn")),
                                   (meta_loc _loc a0))),
                              (meta_rec_flag _loc a1))),
                         (meta_binding _loc a2))), (meta_expr _loc a3))
            | `LetModule (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "LetModule")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_expr _loc a2))), (meta_expr _loc a3))
            | `Match (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Match")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_match_case _loc a2))
            | `New (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "New")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Obj (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Obj")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_str_item _loc a2))
            | `OptLabl (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "OptLabl")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `OvrInst (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "OvrInst")), (meta_loc _loc a0))),
                    (meta_rec_binding _loc a1))
            | `Record (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Record")),
                              (meta_loc _loc a0))),
                         (meta_rec_binding _loc a1))), (meta_expr _loc a2))
            | `Seq (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Seq")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `Send (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Send")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_alident _loc a2))
            | `StringDot (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "StringDot")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Try (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Try")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_match_case _loc a2))
            | `ExTup (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ExTup")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `ExCom (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ExCom")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Constraint_exp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Constraint_exp")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_ctyp _loc a2))
            | `ExCoe (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "ExCoe")),
                                   (meta_loc _loc a0))), (meta_expr _loc a1))),
                         (meta_ctyp _loc a2))), (meta_ctyp _loc a3))
            | `ExVrn (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ExVrn")), (meta_loc _loc a0))),
                    (meta_string _loc a1))
            | `While (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "While")),
                              (meta_loc _loc a0))), (meta_expr _loc a1))),
                    (meta_expr _loc a2))
            | `Let_open (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Let_open")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_expr _loc a2))
            | `LocalTypeFun (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "LocalTypeFun")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Package_expr (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Package_expr")),
                         (meta_loc _loc a0))), (meta_module_expr _loc a1))
        and meta_module_type: 'loc -> module_type -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `MtFun (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "MtFun")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_type _loc a3))
            | `Sig (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Sig")), (meta_loc _loc a0))),
                    (meta_sig_item _loc a1))
            | `MtWit (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "MtWit")),
                              (meta_loc _loc a0))),
                         (meta_module_type _loc a1))),
                    (meta_with_constr _loc a2))
            | `Of (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Of")), (meta_loc _loc a0))),
                    (meta_module_expr _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_sig_item: 'loc -> sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Class")), (meta_loc _loc a0))),
                    (meta_class_type _loc a1))
            | `ClassType (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ClassType")),
                         (meta_loc _loc a0))), (meta_class_type _loc a1))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_sig_item _loc a1))),
                    (meta_sig_item _loc a2))
            | `Directive (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Directive")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Exception (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Exception")),
                         (meta_loc _loc a0))), (meta_ctyp _loc a1))
            | `External (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "External")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_ctyp _loc a2))),
                    (meta_meta_list meta_string _loc a3))
            | `Include (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Include")), (meta_loc _loc a0))),
                    (meta_module_type _loc a1))
            | `Module (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Module")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `RecModule (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "RecModule")),
                         (meta_loc _loc a0))), (meta_module_binding _loc a1))
            | `ModuleType (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleType")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `Open (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Open")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Type (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Type")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Val (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Val")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_ctyp _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_with_constr: 'loc -> with_constr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `TypeEq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TypeEq")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ModuleEq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleEq")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `TypeSubst (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "TypeSubst")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_ctyp _loc a2))
            | `ModuleSubst (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleSubst")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_ident _loc a2))
            | `And (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_with_constr _loc a1))),
                    (meta_with_constr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_binding: 'loc -> binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "And")),
                              (meta_loc _loc a0))), (meta_binding _loc a1))),
                    (meta_binding _loc a2))
            | `Bind (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Bind")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_rec_binding: 'loc -> rec_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))),
                         (meta_rec_binding _loc a1))),
                    (meta_rec_binding _loc a2))
            | `RecBind (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "RecBind")),
                              (meta_loc _loc a0))), (meta_ident _loc a1))),
                    (meta_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_module_binding: 'loc -> module_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_module_binding _loc a1))),
                    (meta_module_binding _loc a2))
            | `ModuleBind (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "ModuleBind")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_expr _loc a3))
            | `ModuleConstraint (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleConstraint")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_match_case: 'loc -> match_case -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `McOr (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "McOr")),
                              (meta_loc _loc a0))),
                         (meta_match_case _loc a1))),
                    (meta_match_case _loc a2))
            | `Case (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "Case")),
                                   (meta_loc _loc a0))), (meta_patt _loc a1))),
                         (meta_expr _loc a2))), (meta_expr _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_module_expr: 'loc -> module_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp (_loc, (`PaVrn (_loc, "Id")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `MeApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "MeApp")),
                              (meta_loc _loc a0))),
                         (meta_module_expr _loc a1))),
                    (meta_module_expr _loc a2))
            | `Functor (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "Functor")),
                                   (meta_loc _loc a0))),
                              (meta_auident _loc a1))),
                         (meta_module_type _loc a2))),
                    (meta_module_expr _loc a3))
            | `Struct (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Struct")), (meta_loc _loc a0))),
                    (meta_str_item _loc a1))
            | `ModuleExprConstraint (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleExprConstraint")),
                              (meta_loc _loc a0))),
                         (meta_module_expr _loc a1))),
                    (meta_module_type _loc a2))
            | `PackageModule (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "PackageModule")),
                         (meta_loc _loc a0))), (meta_expr _loc a1))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_str_item: 'loc -> str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Class")), (meta_loc _loc a0))),
                    (meta_class_expr _loc a1))
            | `ClassType (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "ClassType")),
                         (meta_loc _loc a0))), (meta_class_type _loc a1))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))), (meta_str_item _loc a1))),
                    (meta_str_item _loc a2))
            | `Directive (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Directive")),
                              (meta_loc _loc a0))), (meta_alident _loc a1))),
                    (meta_expr _loc a2))
            | `Exception (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Exception")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_meta_option meta_ident _loc a2))
            | `StExp (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "StExp")), (meta_loc _loc a0))),
                    (meta_expr _loc a1))
            | `External (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "External")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))), (meta_ctyp _loc a2))),
                    (meta_meta_list meta_string _loc a3))
            | `Include (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Include")), (meta_loc _loc a0))),
                    (meta_module_expr _loc a1))
            | `Module (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Module")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_expr _loc a2))
            | `RecModule (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "RecModule")),
                         (meta_loc _loc a0))), (meta_module_binding _loc a1))
            | `ModuleType (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "ModuleType")),
                              (meta_loc _loc a0))), (meta_auident _loc a1))),
                    (meta_module_type _loc a2))
            | `Open (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Open")), (meta_loc _loc a0))),
                    (meta_ident _loc a1))
            | `Type (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Type")), (meta_loc _loc a0))),
                    (meta_ctyp _loc a1))
            | `Value (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Value")),
                              (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                    (meta_binding _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_type: 'loc -> class_type -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CtCon (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CtCon")),
                                   (meta_loc _loc a0))),
                              (meta_virtual_flag _loc a1))),
                         (meta_ident _loc a2))), (meta_ctyp _loc a3))
            | `CtFun (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CtFun")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_class_type _loc a2))
            | `CtSig (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CtSig")),
                              (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                    (meta_class_sig_item _loc a2))
            | `CtAnd (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CtAnd")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | `CtCol (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CtCol")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | `CtEq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CtEq")),
                              (meta_loc _loc a0))),
                         (meta_class_type _loc a1))),
                    (meta_class_type _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Eq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Sem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Sem")),
                              (meta_loc _loc a0))),
                         (meta_class_sig_item _loc a1))),
                    (meta_class_sig_item _loc a2))
            | `Inherit (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Inherit")), (meta_loc _loc a0))),
                    (meta_class_type _loc a1))
            | `Method (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "Method")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | `CgVal (a0,a1,a2,a3,a4) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc,
                                   (`PaApp
                                      (_loc, (`PaVrn (_loc, "CgVal")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_mutable_flag _loc a2))),
                         (meta_virtual_flag _loc a3))), (meta_ctyp _loc a4))
            | `CgVir (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CgVir")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_expr: 'loc -> class_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CeApp (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CeApp")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))), (meta_expr _loc a2))
            | `CeCon (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CeCon")),
                                   (meta_loc _loc a0))),
                              (meta_virtual_flag _loc a1))),
                         (meta_ident _loc a2))), (meta_ctyp _loc a3))
            | `CeFun (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CeFun")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_expr _loc a2))
            | `CeLet (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CeLet")),
                                   (meta_loc _loc a0))),
                              (meta_rec_flag _loc a1))),
                         (meta_binding _loc a2))), (meta_class_expr _loc a3))
            | `Obj (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Obj")),
                              (meta_loc _loc a0))), (meta_patt _loc a1))),
                    (meta_class_str_item _loc a2))
            | `CeTyc (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CeTyc")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_type _loc a2))
            | `And (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "And")),
                              (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_expr _loc a2))
            | `Eq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_class_expr _loc a1))),
                    (meta_class_expr _loc a2))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
        and meta_class_str_item: 'loc -> class_str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `CrSem (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "CrSem")),
                              (meta_loc _loc a0))),
                         (meta_class_str_item _loc a1))),
                    (meta_class_str_item _loc a2))
            | `Eq (a0,a1,a2) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc, (`PaVrn (_loc, "Eq")), (meta_loc _loc a0))),
                         (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
            | `Inherit (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "Inherit")),
                                   (meta_loc _loc a0))),
                              (meta_override_flag _loc a1))),
                         (meta_class_expr _loc a2))),
                    (meta_meta_option meta_alident _loc a3))
            | `Initializer (a0,a1) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc, (`PaVrn (_loc, "Initializer")),
                         (meta_loc _loc a0))), (meta_expr _loc a1))
            | `CrMth (a0,a1,a2,a3,a4,a5) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc,
                                   (`PaApp
                                      (_loc,
                                        (`PaApp
                                           (_loc, (`PaVrn (_loc, "CrMth")),
                                             (meta_loc _loc a0))),
                                        (meta_alident _loc a1))),
                                   (meta_override_flag _loc a2))),
                              (meta_private_flag _loc a3))),
                         (meta_expr _loc a4))), (meta_ctyp _loc a5))
            | `CrVal (a0,a1,a2,a3,a4) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc,
                                   (`PaApp
                                      (_loc, (`PaVrn (_loc, "CrVal")),
                                        (meta_loc _loc a0))),
                                   (meta_alident _loc a1))),
                              (meta_override_flag _loc a2))),
                         (meta_mutable_flag _loc a3))), (meta_expr _loc a4))
            | `CrVir (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CrVir")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
            | `CrVvr (a0,a1,a2,a3) ->
                `PaApp
                  (_loc,
                    (`PaApp
                       (_loc,
                         (`PaApp
                            (_loc,
                              (`PaApp
                                 (_loc, (`PaVrn (_loc, "CrVvr")),
                                   (meta_loc _loc a0))),
                              (meta_alident _loc a1))),
                         (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
            | #ant as a0 -> (meta_ant _loc a0 :>'result)
      end
  end
let rec is_module_longident =
  function
  | `IdAcc (_loc,_,i) -> is_module_longident i
  | `IdApp (_loc,i1,i2) ->
      (is_module_longident i1) && (is_module_longident i2)
  | `Uid (_loc,_) -> true
  | _ -> false
let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =
    function
    | `ExApp (_loc,e1,e2) -> `IdApp (_loc, (self e1), (self e2))
    | `ExAcc (_loc,e1,e2) -> `IdAcc (_loc, (self e1), (self e2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | `ExApp (_loc,_,_) -> error () | t -> self t
let ident_of_ctyp =
  let error () = invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =
    function
    | `TyApp (_loc,t1,t2) -> `IdApp (_loc, (self t1), (self t2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | t -> self t
let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self =
    function
    | `PaApp (_loc,p1,p2) -> `IdApp (_loc, (self p1), (self p2))
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | p -> self p
let rec is_irrefut_patt: patt -> bool =
  function
  | `Id (_loc,`Lid (_,_)) -> true
  | `Id (_loc,`Uid (_,"()")) -> true
  | `Any _loc -> true
  | `Nil _loc -> true
  | `Alias (_loc,x,_) -> is_irrefut_patt x
  | `PaRec (_loc,p) -> is_irrefut_patt p
  | `PaEq (_loc,_,p) -> is_irrefut_patt p
  | `Sem (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaCom (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaOrp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaApp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaTyc (_loc,p,_) -> is_irrefut_patt p
  | `PaTup (_loc,pl) -> is_irrefut_patt pl
  | `PaOlbi (_loc,_,p,_) -> is_irrefut_patt p
  | `Label (_loc,_,`Nil _) -> true
  | `Label (_loc,_,p) -> is_irrefut_patt p
  | `Lazy (_loc,p) -> is_irrefut_patt p
  | `Id (_loc,_) -> false
  | `ModuleUnpack (_loc,_,_) -> true
  | `PaVrn (_loc,_)|`Str (_loc,_)|`PaRng (_loc,_,_)|`Flo (_loc,_)
    |`NativeInt (_loc,_)|`Int64 (_loc,_)|`Int32 (_loc,_)|`Int (_loc,_)
    |`Chr (_loc,_)|`PaTyp (_loc,_)|`Array (_loc,_)|`Ant (_loc,_) -> false
let rec is_constructor =
  function
  | `IdAcc (_loc,_,i) -> is_constructor i
  | `Uid (_loc,_) -> true
  | `Lid (_loc,_)|`IdApp (_loc,_,_) -> false
  | `Ant (_loc,_) -> assert false
let is_patt_constructor =
  function
  | `Id (_loc,i) -> is_constructor i
  | `PaVrn (_loc,_) -> true
  | _ -> false
let rec is_expr_constructor =
  function
  | `Id (_loc,i) -> is_constructor i
  | `ExAcc (_loc,e1,e2) ->
      (is_expr_constructor e1) && (is_expr_constructor e2)
  | `ExVrn (_loc,_) -> true
  | _ -> false
let ghost = FanLoc.ghost
let rec tyOr_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `Or (_loc, t, (tyOr_of_list ts))
let rec tyAnd_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `And (_loc, t, (tyAnd_of_list ts))
let rec tySem_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TySem (_loc, t, (tySem_of_list ts))
let rec tyCom_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `Com (_loc, t, (tyCom_of_list ts))
let rec tyAmp_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TyAmp (_loc, t, (tyAmp_of_list ts))
let rec tySta_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `Sta (_loc, t, (tySta_of_list ts))
let tyApp_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_ctyp x in `TyApp (_loc, x, y)) t ts
let tyVarApp_of_list (_loc,ls) =
  let aux =
    function
    | [] -> `Nil ghost
    | t::[] -> `Quote (_loc, (`Normal _loc), (`Some t))
    | t::ts ->
        List.fold_left
          (fun x  y  ->
             `TyApp (_loc, x, (`Quote (_loc, (`Normal _loc), (`Some y)))))
          (`Quote (_loc, (`Normal _loc), (`Some t))) ts in
  aux ls
let rec stSem_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_str_item t in `Sem (_loc, t, (stSem_of_list ts))
let rec sgSem_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_sig_item t in `Sem (_loc, t, (sgSem_of_list ts))
let rec biAnd_of_list =
  function
  | [] -> `Nil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_binding b in `And (_loc, b, (biAnd_of_list bs))
let rec rbSem_of_list =
  function
  | [] -> `Nil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_rec_binding b in `Sem (_loc, b, (rbSem_of_list bs))
let rec wcAnd_of_list =
  function
  | [] -> `Nil ghost
  | w::[] -> w
  | w::ws ->
      let _loc = loc_of_with_constr w in `And (_loc, w, (wcAnd_of_list ws))
let rec idAcc_of_list =
  function
  | [] -> assert false
  | i::[] -> i
  | i::is ->
      let _loc = loc_of_ident i in `IdAcc (_loc, i, (idAcc_of_list is))
let rec idApp_of_list =
  function
  | [] -> assert false
  | i::[] -> i
  | i::is ->
      let _loc = loc_of_ident i in `IdApp (_loc, i, (idApp_of_list is))
let rec mcOr_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_match_case x in `McOr (_loc, x, (mcOr_of_list xs))
let rec mbAnd_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_binding x in
      `And (_loc, x, (mbAnd_of_list xs))
let rec meApp_of_list =
  function
  | [] -> assert false
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_expr x in `MeApp (_loc, x, (meApp_of_list xs))
let rec ceAnd_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_expr x in `And (_loc, x, (ceAnd_of_list xs))
let rec ctAnd_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_type x in `CtAnd (_loc, x, (ctAnd_of_list xs))
let rec cgSem_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_sig_item x in
      `Sem (_loc, x, (cgSem_of_list xs))
let rec crSem_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_str_item x in
      `CrSem (_loc, x, (crSem_of_list xs))
let rec paSem_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in `Sem (_loc, x, (paSem_of_list xs))
let rec paCom_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in `PaCom (_loc, x, (paCom_of_list xs))
let rec exSem_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in `Sem (_loc, x, (exSem_of_list xs))
let rec exCom_of_list =
  function
  | [] -> `Nil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in `ExCom (_loc, x, (exCom_of_list xs))
let exApp_of_list =
  function
  | [] -> `Nil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_expr x in `ExApp (_loc, x, y)) t ts
let ty_of_stl =
  function
  | (_loc,s,[]) -> `Id (_loc, (`Uid (_loc, s)))
  | (_loc,s,tl) ->
      `Of (_loc, (`Id (_loc, (`Uid (_loc, s)))), (tyAnd_of_list tl))
let ty_of_sbt =
  function
  | (_loc,s,true ,t) ->
      `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), (`Mutable (_loc, t)))
  | (_loc,s,false ,t) -> `TyCol (_loc, (`Id (_loc, (`Lid (_loc, s)))), t)
let bi_of_pe (p,e) = let _loc = loc_of_patt p in `Bind (_loc, p, e)
let sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l)
let record_type_of_list l = tySem_of_list (List.map ty_of_sbt l)
let binding_of_pel l = biAnd_of_list (List.map bi_of_pe l)
let rec pel_of_binding =
  function
  | `And (_loc,b1,b2) -> (pel_of_binding b1) @ (pel_of_binding b2)
  | `Bind (_loc,p,e) -> [(p, e)]
  | _ -> assert false
let rec list_of_binding x acc =
  match x with
  | `And (_loc,b1,b2) -> list_of_binding b1 (list_of_binding b2 acc)
  | t -> t :: acc
let rec list_of_rec_binding x acc =
  match x with
  | `Sem (_loc,b1,b2) -> list_of_rec_binding b1 (list_of_rec_binding b2 acc)
  | t -> t :: acc
let rec list_of_with_constr x acc =
  match x with
  | `And (_loc,w1,w2) -> list_of_with_constr w1 (list_of_with_constr w2 acc)
  | t -> t :: acc
let rec list_of_ctyp x acc =
  match x with
  | `Nil _loc -> acc
  | `TyAmp (_loc,x,y)|`Com (_loc,x,y)|`Sta (_loc,x,y)|`TySem (_loc,x,y)
    |`And (_loc,x,y)|`Or (_loc,x,y) -> list_of_ctyp x (list_of_ctyp y acc)
  | x -> x :: acc
let rec list_of_ctyp_app (x : ctyp) (acc : ctyp list) =
  (match x with
   | `TyApp (_loc,t1,t2) -> list_of_ctyp_app t1 (list_of_ctyp_app t2 acc)
   | `Nil _loc -> acc
   | x -> x :: acc : ctyp list )
let rec list_of_ctyp_com (x : ctyp) (acc : ctyp list) =
  (match x with
   | `Com (_loc,t1,t2) -> list_of_ctyp_com t1 (list_of_ctyp_com t2 acc)
   | `Nil _loc -> acc
   | x -> x :: acc : ctyp list )
let rec list_of_patt x acc =
  match x with
  | `Nil _loc -> acc
  | `PaCom (_loc,x,y)|`Sem (_loc,x,y) -> list_of_patt x (list_of_patt y acc)
  | x -> x :: acc
let rec list_of_expr x acc =
  match x with
  | `Nil _loc -> acc
  | `ExCom (_loc,x,y)|`Sem (_loc,x,y) -> list_of_expr x (list_of_expr y acc)
  | x -> x :: acc
let rec list_of_str_item x acc =
  match x with
  | `Nil _loc -> acc
  | `Sem (_loc,x,y) -> list_of_str_item x (list_of_str_item y acc)
  | x -> x :: acc
let rec list_of_sig_item x acc =
  match x with
  | `Nil _loc -> acc
  | `Sem (_loc,x,y) -> list_of_sig_item x (list_of_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_sig_item x acc =
  match x with
  | `Nil _loc -> acc
  | `Sem (_loc,x,y) ->
      list_of_class_sig_item x (list_of_class_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_str_item x acc =
  match x with
  | `Nil _loc -> acc
  | `CrSem (_loc,x,y) ->
      list_of_class_str_item x (list_of_class_str_item y acc)
  | x -> x :: acc
let rec list_of_class_type x acc =
  match x with
  | `CtAnd (_loc,x,y) -> list_of_class_type x (list_of_class_type y acc)
  | x -> x :: acc
let rec list_of_class_expr x acc =
  match x with
  | `And (_loc,x,y) -> list_of_class_expr x (list_of_class_expr y acc)
  | x -> x :: acc
let rec list_of_module_expr x acc =
  match x with
  | `MeApp (_loc,x,y) -> list_of_module_expr x (list_of_module_expr y acc)
  | x -> x :: acc
let rec list_of_match_case x acc =
  match x with
  | `Nil _loc -> acc
  | `McOr (_loc,x,y) -> list_of_match_case x (list_of_match_case y acc)
  | x -> x :: acc
let rec list_of_ident x acc =
  match x with
  | `IdAcc (_loc,x,y)|`IdApp (_loc,x,y) ->
      list_of_ident x (list_of_ident y acc)
  | x -> x :: acc
let rec list_of_module_binding x acc =
  match x with
  | `And (_loc,x,y) ->
      list_of_module_binding x (list_of_module_binding y acc)
  | x -> x :: acc
let map_expr f =
  object  inherit  map as super method! expr x = f (super#expr x) end
let map_patt f =
  object  inherit  map as super method! patt x = f (super#patt x) end
let map_ctyp f =
  object  inherit  map as super method! ctyp x = f (super#ctyp x) end
let map_str_item f =
  object  inherit  map as super method! str_item x = f (super#str_item x) end
let map_sig_item f =
  object  inherit  map as super method! sig_item x = f (super#sig_item x) end
let map_loc f =
  object  inherit  map as super method! loc x = f (super#loc x) end
class clean_ast =
  object 
    inherit  map as super
    method! with_constr wc =
      match super#with_constr wc with
      | `And (_loc,`Nil _l,wc)|`And (_loc,wc,`Nil _l) -> wc
      | wc -> wc
    method! expr e =
      match super#expr e with
      | `LetIn (_loc,_,`Nil _l,e)|`Record (_loc,`Nil _l,e)
        |`ExCom (_loc,`Nil _l,e)|`ExCom (_loc,e,`Nil _l)
        |`Sem (_loc,`Nil _l,e)|`Sem (_loc,e,`Nil _l) -> e
      | e -> e
    method! patt p =
      match super#patt p with
      | `PaOrp (_loc,`Nil _l,p)|`PaOrp (_loc,p,`Nil _l)
        |`PaCom (_loc,`Nil _l,p)|`PaCom (_loc,p,`Nil _l)
        |`Sem (_loc,`Nil _l,p)|`Sem (_loc,p,`Nil _l) -> p
      | p -> p
    method! match_case mc =
      match super#match_case mc with
      | `McOr (_loc,`Nil _l,mc)|`McOr (_loc,mc,`Nil _l) -> mc
      | mc -> mc
    method! binding bi =
      match super#binding bi with
      | `And (_loc,`Nil _l,bi)|`And (_loc,bi,`Nil _l) -> bi
      | bi -> bi
    method! rec_binding rb =
      match super#rec_binding rb with
      | `Sem (_loc,`Nil _l,bi)|`Sem (_loc,bi,`Nil _l) -> bi
      | bi -> bi
    method! module_binding mb =
      match super#module_binding mb with
      | `And (_loc,`Nil _l,mb)|`And (_loc,mb,`Nil _l) -> mb
      | mb -> mb
    method! ctyp t =
      match super#ctyp t with
      | `TyPol (_loc,`Nil _l,t)|`Alias (_loc,`Nil _l,t)
        |`Alias (_loc,t,`Nil _l)|`TyArr (_loc,t,`Nil _l)
        |`TyArr (_loc,`Nil _l,t)|`Or (_loc,`Nil _l,t)|`Or (_loc,t,`Nil _l)
        |`Of (_loc,t,`Nil _l)|`And (_loc,`Nil _l,t)|`And (_loc,t,`Nil _l)
        |`TySem (_loc,t,`Nil _l)|`TySem (_loc,`Nil _l,t)
        |`Com (_loc,`Nil _l,t)|`Com (_loc,t,`Nil _l)|`TyAmp (_loc,t,`Nil _l)
        |`TyAmp (_loc,`Nil _l,t)|`Sta (_loc,`Nil _l,t)|`Sta (_loc,t,`Nil _l)
          -> t
      | t -> t
    method! sig_item sg =
      match super#sig_item sg with
      | `Sem (_loc,`Nil _l,sg)|`Sem (_loc,sg,`Nil _l) -> sg
      | `Type (_loc,`Nil _l) -> `Nil _loc
      | sg -> sg
    method! str_item st =
      match super#str_item st with
      | `Sem (_loc,`Nil _l,st)|`Sem (_loc,st,`Nil _l) -> st
      | `Type (_loc,`Nil _l) -> `Nil _loc
      | `Value (_loc,_,`Nil _l) -> `Nil _loc
      | st -> st
    method! module_type mt =
      match super#module_type mt with
      | `MtWit (_loc,mt,`Nil _l) -> mt
      | mt -> mt
    method! class_expr ce =
      match super#class_expr ce with
      | `And (_loc,`Nil _l,ce)|`And (_loc,ce,`Nil _l) -> ce
      | ce -> ce
    method! class_type ct =
      match super#class_type ct with
      | `CtAnd (_loc,`Nil _l,ct)|`CtAnd (_loc,ct,`Nil _l) -> ct
      | ct -> ct
    method! class_sig_item csg =
      match super#class_sig_item csg with
      | `Sem (_loc,`Nil _l,csg)|`Sem (_loc,csg,`Nil _l) -> csg
      | csg -> csg
    method! class_str_item cst =
      match super#class_str_item cst with
      | `CrSem (_loc,`Nil _l,cst)|`CrSem (_loc,cst,`Nil _l) -> cst
      | cst -> cst
  end
class reloc _loc = object  inherit  map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  map as super
    method! patt =
      function
      | `Id (_loc,`Lid (_,_)) -> `Any _loc
      | `Alias (_loc,p,_) -> self#patt p
      | p -> super#patt p
  end
let match_pre =
  object (self)
    inherit  map
    method! match_case =
      function
      | `Case (_loc,p,`Nil _,e) ->
          `Case
            (_loc, p, (`Nil _loc),
              (`Fun
                 (_loc,
                   (`Case
                      (_loc, (`Id (_loc, (`Uid (_loc, "()")))), (`Nil _loc),
                        e)))))
      | `Case (_loc,p,e,e1) ->
          `Case
            (_loc, p, e,
              (`Fun
                 (_loc,
                   (`Case
                      (_loc, (`Id (_loc, (`Uid (_loc, "()")))), (`Nil _loc),
                        e1)))))
      | `McOr (_loc,a1,a2) ->
          `McOr (_loc, (self#match_case a1), (self#match_case a2))
      | `Nil _loc -> `Nil _loc
      | `Ant (_loc,x) -> `Ant (_loc, (add_context x "lettry"))
  end
let dump = new print