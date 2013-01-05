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
let _ = ()
class map =
  object (self : 'self_type)
    inherit  mapbase
    method class_str_item : class_str_item -> class_str_item=
      function
      | `CrNil a0 -> `CrNil (self#loc a0)
      | `CrSem a0 ->
          `CrSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_str_item a1),
                   (self#class_str_item a2)))) a0)
      | `CrCtr a0 ->
          `CrCtr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `CrInh a0 ->
          `CrInh
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#override_flag a1),
                   (self#class_expr a2), (self#string a3)))) a0)
      | `CrIni a0 ->
          `CrIni (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `CrMth a0 ->
          `CrMth
            (((fun (a0,a1,a2,a3,a4,a5)  ->
                 ((self#loc a0), (self#string a1), (self#override_flag a2),
                   (self#private_flag a3), (self#expr a4), (self#ctyp a5))))
               a0)
      | `CrVal a0 ->
          `CrVal
            (((fun (a0,a1,a2,a3,a4)  ->
                 ((self#loc a0), (self#string a1), (self#override_flag a2),
                   (self#mutable_flag a3), (self#expr a4)))) a0)
      | `CrVir a0 ->
          `CrVir
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#private_flag a2),
                   (self#ctyp a3)))) a0)
      | `CrVvr a0 ->
          `CrVvr
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#mutable_flag a2),
                   (self#ctyp a3)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method class_expr : class_expr -> class_expr=
      function
      | `CeNil a0 -> `CeNil (self#loc a0)
      | `CeApp a0 ->
          `CeApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#expr a2)))) a0)
      | `CeCon a0 ->
          `CeCon
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
                   (self#ctyp a3)))) a0)
      | `CeFun a0 ->
          `CeFun
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#class_expr a2)))) a0)
      | `CeLet a0 ->
          `CeLet
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#rec_flag a1), (self#binding a2),
                   (self#class_expr a3)))) a0)
      | `CeStr a0 ->
          `CeStr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#class_str_item a2))))
               a0)
      | `CeTyc a0 ->
          `CeTyc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_type a2))))
               a0)
      | `CeAnd a0 ->
          `CeAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_expr a2))))
               a0)
      | `CeEq a0 ->
          `CeEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_expr a2))))
               a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | `CgNil a0 -> `CgNil (self#loc a0)
      | `CgCtr a0 ->
          `CgCtr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `CgSem a0 ->
          `CgSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_sig_item a1),
                   (self#class_sig_item a2)))) a0)
      | `CgInh a0 ->
          `CgInh
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `CgMth a0 ->
          `CgMth
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#private_flag a2),
                   (self#ctyp a3)))) a0)
      | `CgVal a0 ->
          `CgVal
            (((fun (a0,a1,a2,a3,a4)  ->
                 ((self#loc a0), (self#string a1), (self#mutable_flag a2),
                   (self#virtual_flag a3), (self#ctyp a4)))) a0)
      | `CgVir a0 ->
          `CgVir
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#private_flag a2),
                   (self#ctyp a3)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method class_type : class_type -> class_type=
      function
      | `CtNil a0 -> `CtNil (self#loc a0)
      | `CtCon a0 ->
          `CtCon
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
                   (self#ctyp a3)))) a0)
      | `CtFun a0 ->
          `CtFun
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#class_type a2)))) a0)
      | `CtSig a0 ->
          `CtSig
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#class_sig_item a2))))
               a0)
      | `CtAnd a0 ->
          `CtAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_type a1), (self#class_type a2))))
               a0)
      | `CtCol a0 ->
          `CtCol
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_type a1), (self#class_type a2))))
               a0)
      | `CtEq a0 ->
          `CtEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_type a1), (self#class_type a2))))
               a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method str_item : str_item -> str_item=
      function
      | `StNil a0 -> `StNil (self#loc a0)
      | `StCls a0 ->
          `StCls
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_expr a1)))) a0)
      | `StClt a0 ->
          `StClt
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `StSem a0 ->
          `StSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#str_item a1), (self#str_item a2)))) a0)
      | `StDir a0 ->
          `StDir
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `StExc a0 ->
          `StExc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1),
                   (self#meta_option (fun self  -> self#ident) a2)))) a0)
      | `StExp a0 ->
          `StExp (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `StExt a0 ->
          `StExt
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2),
                   (self#meta_list (fun self  -> self#string) a3)))) a0)
      | `StInc a0 ->
          `StInc
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
      | `StMod a0 ->
          `StMod
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_expr a2))))
               a0)
      | `StRecMod a0 ->
          `StRecMod
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_binding a1)))) a0)
      | `StMty a0 ->
          `StMty
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `StOpn a0 ->
          `StOpn (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `StTyp a0 ->
          `StTyp (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `StVal a0 ->
          `StVal
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_flag a1), (self#binding a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_expr : module_expr -> module_expr=
      function
      | `MeNil a0 -> `MeNil (self#loc a0)
      | `MeId a0 ->
          `MeId (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `MeApp a0 ->
          `MeApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_expr a1),
                   (self#module_expr a2)))) a0)
      | `MeFun a0 ->
          `MeFun
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_expr a3)))) a0)
      | `MeStr a0 ->
          `MeStr (((fun (a0,a1)  -> ((self#loc a0), (self#str_item a1)))) a0)
      | `MeTyc a0 ->
          `MeTyc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_expr a1),
                   (self#module_type a2)))) a0)
      | `MePkg a0 ->
          `MePkg (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method match_case : match_case -> match_case=
      function
      | `McNil a0 -> `McNil (self#loc a0)
      | `McOr a0 ->
          `McOr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#match_case a1), (self#match_case a2))))
               a0)
      | `McArr a0 ->
          `McArr
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#patt a1), (self#expr a2),
                   (self#expr a3)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_binding : module_binding -> module_binding=
      function
      | `MbNil a0 -> `MbNil (self#loc a0)
      | `MbAnd a0 ->
          `MbAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_binding a1),
                   (self#module_binding a2)))) a0)
      | `MbColEq a0 ->
          `MbColEq
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_expr a3)))) a0)
      | `MbCol a0 ->
          `MbCol
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method rec_binding : rec_binding -> rec_binding=
      function
      | `RbNil a0 -> `RbNil (self#loc a0)
      | `RbSem a0 ->
          `RbSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_binding a1),
                   (self#rec_binding a2)))) a0)
      | `RbEq a0 ->
          `RbEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#expr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method binding : binding -> binding=
      function
      | `BiNil a0 -> `BiNil (self#loc a0)
      | `BiAnd a0 ->
          `BiAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#binding a1), (self#binding a2)))) a0)
      | `BiEq a0 ->
          `BiEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#expr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method with_constr : with_constr -> with_constr=
      function
      | `WcNil a0 -> `WcNil (self#loc a0)
      | `WcTyp a0 ->
          `WcTyp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `WcMod a0 ->
          `WcMod
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `WcTyS a0 ->
          `WcTyS
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `WcMoS a0 ->
          `WcMoS
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `WcAnd a0 ->
          `WcAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#with_constr a1),
                   (self#with_constr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method sig_item : sig_item -> sig_item=
      function
      | `SgNil a0 -> `SgNil (self#loc a0)
      | `SgCls a0 ->
          `SgCls
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `SgClt a0 ->
          `SgClt
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `SgSem a0 ->
          `SgSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#sig_item a1), (self#sig_item a2)))) a0)
      | `SgDir a0 ->
          `SgDir
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `SgExc a0 ->
          `SgExc (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `SgExt a0 ->
          `SgExt
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2),
                   (self#meta_list (fun self  -> self#string) a3)))) a0)
      | `SgInc a0 ->
          `SgInc
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_type a1)))) a0)
      | `SgMod a0 ->
          `SgMod
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `SgRecMod a0 ->
          `SgRecMod
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_binding a1)))) a0)
      | `SgMty a0 ->
          `SgMty
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `SgOpn a0 ->
          `SgOpn (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `SgTyp a0 ->
          `SgTyp (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `SgVal a0 ->
          `SgVal
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_type : module_type -> module_type=
      function
      | `MtNil a0 -> `MtNil (self#loc a0)
      | `MtId a0 ->
          `MtId (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `MtFun a0 ->
          `MtFun
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_type a3)))) a0)
      | `MtQuo a0 ->
          `MtQuo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `MtSig a0 ->
          `MtSig (((fun (a0,a1)  -> ((self#loc a0), (self#sig_item a1)))) a0)
      | `MtWit a0 ->
          `MtWit
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_type a1),
                   (self#with_constr a2)))) a0)
      | `MtOf a0 ->
          `MtOf
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method expr : expr -> expr=
      function
      | `ExNil a0 -> `ExNil (self#loc a0)
      | `ExId a0 ->
          `ExId (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `ExAcc a0 ->
          `ExAcc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExApp a0 ->
          `ExApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExAre a0 ->
          `ExAre
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExArr a0 ->
          `ExArr (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExSem a0 ->
          `ExSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExAsf a0 -> `ExAsf (self#loc a0)
      | `ExAsr a0 ->
          `ExAsr (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExAss a0 ->
          `ExAss
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExChr a0 ->
          `ExChr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExCoe a0 ->
          `ExCoe
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#expr a1), (self#ctyp a2),
                   (self#ctyp a3)))) a0)
      | `ExFlo a0 ->
          `ExFlo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExFor a0 ->
          `ExFor
            (((fun (a0,a1,a2,a3,a4,a5)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2),
                   (self#expr a3), (self#direction_flag a4), (self#expr a5))))
               a0)
      | `ExFun a0 ->
          `ExFun
            (((fun (a0,a1)  -> ((self#loc a0), (self#match_case a1)))) a0)
      | `ExIfe a0 ->
          `ExIfe
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2),
                   (self#expr a3)))) a0)
      | `ExInt a0 ->
          `ExInt (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExInt32 a0 ->
          `ExInt32 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExInt64 a0 ->
          `ExInt64 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExNativeInt a0 ->
          `ExNativeInt
            (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExLab a0 ->
          `ExLab
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `ExLaz a0 ->
          `ExLaz (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExLet a0 ->
          `ExLet
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#rec_flag a1), (self#binding a2),
                   (self#expr a3)))) a0)
      | `ExLmd a0 ->
          `ExLmd
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_expr a2),
                   (self#expr a3)))) a0)
      | `ExMat a0 ->
          `ExMat
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#match_case a2)))) a0)
      | `ExNew a0 ->
          `ExNew (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `ExObj a0 ->
          `ExObj
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#class_str_item a2))))
               a0)
      | `ExOlb a0 ->
          `ExOlb
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `ExOvr a0 ->
          `ExOvr
            (((fun (a0,a1)  -> ((self#loc a0), (self#rec_binding a1)))) a0)
      | `ExRec a0 ->
          `ExRec
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_binding a1), (self#expr a2)))) a0)
      | `ExSeq a0 ->
          `ExSeq (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExSnd a0 ->
          `ExSnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#string a2)))) a0)
      | `ExSte a0 ->
          `ExSte
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExStr a0 ->
          `ExStr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExTry a0 ->
          `ExTry
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#match_case a2)))) a0)
      | `ExTup a0 ->
          `ExTup (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExCom a0 ->
          `ExCom
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExTyc a0 ->
          `ExTyc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#ctyp a2)))) a0)
      | `ExVrn a0 ->
          `ExVrn (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `ExWhi a0 ->
          `ExWhi
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExOpI a0 ->
          `ExOpI
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#expr a2)))) a0)
      | `ExFUN a0 ->
          `ExFUN
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `ExPkg a0 ->
          `ExPkg
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
    method patt : patt -> patt=
      function
      | `PaNil a0 -> `PaNil (self#loc a0)
      | `PaId a0 ->
          `PaId (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `PaAli a0 ->
          `PaAli
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaAny a0 -> `PaAny (self#loc a0)
      | `PaApp a0 ->
          `PaApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `PaArr a0 ->
          `PaArr (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaCom a0 ->
          `PaCom
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `PaSem a0 ->
          `PaSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `PaChr a0 ->
          `PaChr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaInt a0 ->
          `PaInt (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaInt32 a0 ->
          `PaInt32 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaInt64 a0 ->
          `PaInt64 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaNativeInt a0 ->
          `PaNativeInt
            (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaFlo a0 ->
          `PaFlo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaLab a0 ->
          `PaLab
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#patt a2)))) a0)
      | `PaOlb a0 ->
          `PaOlb
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#patt a2)))) a0)
      | `PaOlbi a0 ->
          `PaOlbi
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#patt a2),
                   (self#expr a3)))) a0)
      | `PaOrp a0 ->
          `PaOrp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `PaRng a0 ->
          `PaRng
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `PaRec a0 ->
          `PaRec (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaEq a0 ->
          `PaEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#patt a2)))) a0)
      | `PaStr a0 ->
          `PaStr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaTup a0 ->
          `PaTup (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaTyc a0 ->
          `PaTyc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#ctyp a2)))) a0)
      | `PaTyp a0 ->
          `PaTyp (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `PaVrn a0 ->
          `PaVrn (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `PaLaz a0 ->
          `PaLaz (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaMod a0 ->
          `PaMod (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method ctyp : ctyp -> ctyp=
      function
      | `TyNil a0 -> `TyNil (self#loc a0)
      | `TyAli a0 ->
          `TyAli
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyAny a0 -> `TyAny (self#loc a0)
      | `TyApp a0 ->
          `TyApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyArr a0 ->
          `TyArr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyCls a0 ->
          `TyCls (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `TyLab a0 ->
          `TyLab
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2)))) a0)
      | `TyId a0 ->
          `TyId (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `TyMan a0 ->
          `TyMan
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyDcl a0 ->
          `TyDcl
            (((fun (a0,a1,a2,a3,a4)  ->
                 ((self#loc a0), (self#string a1),
                   (self#list (fun self  -> self#ctyp) a2), (self#ctyp a3),
                   (self#list
                      (fun self  (a0,a1)  -> ((self#ctyp a0), (self#ctyp a1)))
                      a4)))) a0)
      | `TyObj a0 ->
          `TyObj
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#row_var_flag a2)))) a0)
      | `TyOlb a0 ->
          `TyOlb
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2)))) a0)
      | `TyPol a0 ->
          `TyPol
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyTypePol a0 ->
          `TyTypePol
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyQuo a0 ->
          `TyQuo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `TyQuP a0 ->
          `TyQuP (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `TyQuM a0 ->
          `TyQuM (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `TyAnP a0 -> `TyAnP (self#loc a0)
      | `TyAnM a0 -> `TyAnM (self#loc a0)
      | `TyVrn a0 ->
          `TyVrn (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `TyRec a0 ->
          `TyRec (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyCol a0 ->
          `TyCol
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TySem a0 ->
          `TySem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyCom a0 ->
          `TyCom
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TySum a0 ->
          `TySum (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyOf a0 ->
          `TyOf
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyAnd a0 ->
          `TyAnd
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyOr a0 ->
          `TyOr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyPrv a0 ->
          `TyPrv (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyMut a0 ->
          `TyMut (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyTup a0 ->
          `TyTup (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TySta a0 ->
          `TySta
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyVrnEq a0 ->
          `TyVrnEq (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyVrnSup a0 ->
          `TyVrnSup (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyVrnInf a0 ->
          `TyVrnInf (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `TyVrnInfSup a0 ->
          `TyVrnInfSup
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyAmp a0 ->
          `TyAmp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyOfAmp a0 ->
          `TyOfAmp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyPkg a0 ->
          `TyPkg
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_type a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method ident : ident -> ident=
      function
      | `IdAcc a0 ->
          `IdAcc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `IdApp a0 ->
          `IdApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `Lid a0 ->
          `Lid (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Uid a0 ->
          `Uid (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method meta_list :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_list -> 'all_b0 meta_list=
      fun mf_a  ->
        function
        | `LNil a0 -> `LNil (self#loc a0)
        | `LCons a0 ->
            `LCons
              (((fun (a0,a1)  -> ((mf_a self a0), (self#meta_list mf_a a1))))
                 a0)
        | `Ant a0 ->
            `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method meta_option :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_option -> 'all_b0 meta_option=
      fun mf_a  ->
        function
        | `None a0 -> `None (self#loc a0)
        | `Some a0 -> `Some (mf_a self a0)
        | `Ant a0 ->
            `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method row_var_flag : row_var_flag -> row_var_flag=
      function
      | `RowVar a0 -> `RowVar (self#loc a0)
      | `RvNil a0 -> `RvNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method override_flag : override_flag -> override_flag=
      function
      | `Override a0 -> `Override (self#loc a0)
      | `OvNil a0 -> `OvNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method virtual_flag : virtual_flag -> virtual_flag=
      function
      | `Virtual a0 -> `Virtual (self#loc a0)
      | `ViNil a0 -> `ViNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method private_flag : private_flag -> private_flag=
      function
      | `Private a0 -> `Private (self#loc a0)
      | `PrNil a0 -> `PrNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method mutable_flag : mutable_flag -> mutable_flag=
      function
      | `Mutable a0 -> `Mutable (self#loc a0)
      | `MuNil a0 -> `MuNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method direction_flag : direction_flag -> direction_flag=
      function
      | `To a0 -> `To (self#loc a0)
      | `Downto a0 -> `Downto (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method rec_flag : rec_flag -> rec_flag=
      function
      | `Recursive a0 -> `Recursive (self#loc a0)
      | `ReNil a0 -> `ReNil (self#loc a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method loc : loc -> loc= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
  end
class print =
  object (self : 'self_type)
    inherit  printbase
    method class_str_item : 'fmt -> class_str_item -> 'result=
      fun fmt  ->
        function
        | `CrNil a0 -> Format.fprintf fmt "@[<1>(`CrNil@ %a)@]" self#loc a0
        | `CrSem a0 ->
            Format.fprintf fmt "@[<1>(`CrSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_str_item a1 self#class_str_item a2) a0
        | `CrCtr a0 ->
            Format.fprintf fmt "@[<1>(`CrCtr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `CrInh a0 ->
            Format.fprintf fmt "@[<1>(`CrInh@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#override_flag a1 self#class_expr a2 self#string a3)
              a0
        | `CrIni a0 ->
            Format.fprintf fmt "@[<1>(`CrIni@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `CrMth a0 ->
            Format.fprintf fmt "@[<1>(`CrMth@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1 self#override_flag a2
                   self#private_flag a3 self#expr a4 self#ctyp a5) a0
        | `CrVal a0 ->
            Format.fprintf fmt "@[<1>(`CrVal@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1 self#override_flag a2
                   self#mutable_flag a3 self#expr a4) a0
        | `CrVir a0 ->
            Format.fprintf fmt "@[<1>(`CrVir@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#private_flag a2 self#ctyp a3) a0
        | `CrVvr a0 ->
            Format.fprintf fmt "@[<1>(`CrVvr@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#mutable_flag a2 self#ctyp a3) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method class_expr : 'fmt -> class_expr -> 'result=
      fun fmt  ->
        function
        | `CeNil a0 -> Format.fprintf fmt "@[<1>(`CeNil@ %a)@]" self#loc a0
        | `CeApp a0 ->
            Format.fprintf fmt "@[<1>(`CeApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#expr a2) a0
        | `CeCon a0 ->
            Format.fprintf fmt "@[<1>(`CeCon@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#virtual_flag a1 self#ident a2 self#ctyp a3) a0
        | `CeFun a0 ->
            Format.fprintf fmt "@[<1>(`CeFun@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#class_expr a2) a0
        | `CeLet a0 ->
            Format.fprintf fmt "@[<1>(`CeLet@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_flag a1 self#binding a2 self#class_expr a3) a0
        | `CeStr a0 ->
            Format.fprintf fmt "@[<1>(`CeStr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#class_str_item a2) a0
        | `CeTyc a0 ->
            Format.fprintf fmt "@[<1>(`CeTyc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#class_type a2) a0
        | `CeAnd a0 ->
            Format.fprintf fmt "@[<1>(`CeAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#class_expr a2) a0
        | `CeEq a0 ->
            Format.fprintf fmt "@[<1>(`CeEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#class_expr a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method class_sig_item : 'fmt -> class_sig_item -> 'result=
      fun fmt  ->
        function
        | `CgNil a0 -> Format.fprintf fmt "@[<1>(`CgNil@ %a)@]" self#loc a0
        | `CgCtr a0 ->
            Format.fprintf fmt "@[<1>(`CgCtr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `CgSem a0 ->
            Format.fprintf fmt "@[<1>(`CgSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_sig_item a1 self#class_sig_item a2) a0
        | `CgInh a0 ->
            Format.fprintf fmt "@[<1>(`CgInh@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `CgMth a0 ->
            Format.fprintf fmt "@[<1>(`CgMth@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#private_flag a2 self#ctyp a3) a0
        | `CgVal a0 ->
            Format.fprintf fmt "@[<1>(`CgVal@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1 self#mutable_flag a2
                   self#virtual_flag a3 self#ctyp a4) a0
        | `CgVir a0 ->
            Format.fprintf fmt "@[<1>(`CgVir@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#private_flag a2 self#ctyp a3) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method class_type : 'fmt -> class_type -> 'result=
      fun fmt  ->
        function
        | `CtNil a0 -> Format.fprintf fmt "@[<1>(`CtNil@ %a)@]" self#loc a0
        | `CtCon a0 ->
            Format.fprintf fmt "@[<1>(`CtCon@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#virtual_flag a1 self#ident a2 self#ctyp a3) a0
        | `CtFun a0 ->
            Format.fprintf fmt "@[<1>(`CtFun@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#class_type a2) a0
        | `CtSig a0 ->
            Format.fprintf fmt "@[<1>(`CtSig@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#class_sig_item a2) a0
        | `CtAnd a0 ->
            Format.fprintf fmt "@[<1>(`CtAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_type a1 self#class_type a2) a0
        | `CtCol a0 ->
            Format.fprintf fmt "@[<1>(`CtCol@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_type a1 self#class_type a2) a0
        | `CtEq a0 ->
            Format.fprintf fmt "@[<1>(`CtEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_type a1 self#class_type a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method str_item : 'fmt -> str_item -> 'result=
      fun fmt  ->
        function
        | `StNil a0 -> Format.fprintf fmt "@[<1>(`StNil@ %a)@]" self#loc a0
        | `StCls a0 ->
            Format.fprintf fmt "@[<1>(`StCls@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_expr a1) a0
        | `StClt a0 ->
            Format.fprintf fmt "@[<1>(`StClt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `StSem a0 ->
            Format.fprintf fmt "@[<1>(`StSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#str_item a1 self#str_item a2) a0
        | `StDir a0 ->
            Format.fprintf fmt "@[<1>(`StDir@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `StExc a0 ->
            Format.fprintf fmt "@[<1>(`StExc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 (self#meta_option (fun self  -> self#ident))
                   a2) a0
        | `StExp a0 ->
            Format.fprintf fmt "@[<1>(`StExp@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `StExt a0 ->
            Format.fprintf fmt "@[<1>(`StExt@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2
                   (self#meta_list (fun self  -> self#string)) a3) a0
        | `StInc a0 ->
            Format.fprintf fmt "@[<1>(`StInc@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_expr a1) a0
        | `StMod a0 ->
            Format.fprintf fmt "@[<1>(`StMod@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_expr a2) a0
        | `StRecMod a0 ->
            Format.fprintf fmt "@[<1>(`StRecMod@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_binding a1) a0
        | `StMty a0 ->
            Format.fprintf fmt "@[<1>(`StMty@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `StOpn a0 ->
            Format.fprintf fmt "@[<1>(`StOpn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `StTyp a0 ->
            Format.fprintf fmt "@[<1>(`StTyp@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `StVal a0 ->
            Format.fprintf fmt "@[<1>(`StVal@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_flag a1 self#binding a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method module_expr : 'fmt -> module_expr -> 'result=
      fun fmt  ->
        function
        | `MeNil a0 -> Format.fprintf fmt "@[<1>(`MeNil@ %a)@]" self#loc a0
        | `MeId a0 ->
            Format.fprintf fmt "@[<1>(`MeId@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `MeApp a0 ->
            Format.fprintf fmt "@[<1>(`MeApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_expr a1 self#module_expr a2) a0
        | `MeFun a0 ->
            Format.fprintf fmt "@[<1>(`MeFun@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2 self#module_expr a3) a0
        | `MeStr a0 ->
            Format.fprintf fmt "@[<1>(`MeStr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#str_item a1) a0
        | `MeTyc a0 ->
            Format.fprintf fmt "@[<1>(`MeTyc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_expr a1 self#module_type a2) a0
        | `MePkg a0 ->
            Format.fprintf fmt "@[<1>(`MePkg@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method match_case : 'fmt -> match_case -> 'result=
      fun fmt  ->
        function
        | `McNil a0 -> Format.fprintf fmt "@[<1>(`McNil@ %a)@]" self#loc a0
        | `McOr a0 ->
            Format.fprintf fmt "@[<1>(`McOr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#match_case a1 self#match_case a2) a0
        | `McArr a0 ->
            Format.fprintf fmt "@[<1>(`McArr@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#expr a2 self#expr a3) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method module_binding : 'fmt -> module_binding -> 'result=
      fun fmt  ->
        function
        | `MbNil a0 -> Format.fprintf fmt "@[<1>(`MbNil@ %a)@]" self#loc a0
        | `MbAnd a0 ->
            Format.fprintf fmt "@[<1>(`MbAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_binding a1 self#module_binding a2) a0
        | `MbColEq a0 ->
            Format.fprintf fmt "@[<1>(`MbColEq@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2 self#module_expr a3) a0
        | `MbCol a0 ->
            Format.fprintf fmt "@[<1>(`MbCol@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method rec_binding : 'fmt -> rec_binding -> 'result=
      fun fmt  ->
        function
        | `RbNil a0 -> Format.fprintf fmt "@[<1>(`RbNil@ %a)@]" self#loc a0
        | `RbSem a0 ->
            Format.fprintf fmt "@[<1>(`RbSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_binding a1 self#rec_binding a2) a0
        | `RbEq a0 ->
            Format.fprintf fmt "@[<1>(`RbEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#expr a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method binding : 'fmt -> binding -> 'result=
      fun fmt  ->
        function
        | `BiNil a0 -> Format.fprintf fmt "@[<1>(`BiNil@ %a)@]" self#loc a0
        | `BiAnd a0 ->
            Format.fprintf fmt "@[<1>(`BiAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#binding a1 self#binding a2) a0
        | `BiEq a0 ->
            Format.fprintf fmt "@[<1>(`BiEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#expr a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method with_constr : 'fmt -> with_constr -> 'result=
      fun fmt  ->
        function
        | `WcNil a0 -> Format.fprintf fmt "@[<1>(`WcNil@ %a)@]" self#loc a0
        | `WcTyp a0 ->
            Format.fprintf fmt "@[<1>(`WcTyp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `WcMod a0 ->
            Format.fprintf fmt "@[<1>(`WcMod@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `WcTyS a0 ->
            Format.fprintf fmt "@[<1>(`WcTyS@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `WcMoS a0 ->
            Format.fprintf fmt "@[<1>(`WcMoS@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `WcAnd a0 ->
            Format.fprintf fmt "@[<1>(`WcAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#with_constr a1 self#with_constr a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method sig_item : 'fmt -> sig_item -> 'result=
      fun fmt  ->
        function
        | `SgNil a0 -> Format.fprintf fmt "@[<1>(`SgNil@ %a)@]" self#loc a0
        | `SgCls a0 ->
            Format.fprintf fmt "@[<1>(`SgCls@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `SgClt a0 ->
            Format.fprintf fmt "@[<1>(`SgClt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `SgSem a0 ->
            Format.fprintf fmt "@[<1>(`SgSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#sig_item a1 self#sig_item a2) a0
        | `SgDir a0 ->
            Format.fprintf fmt "@[<1>(`SgDir@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `SgExc a0 ->
            Format.fprintf fmt "@[<1>(`SgExc@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `SgExt a0 ->
            Format.fprintf fmt "@[<1>(`SgExt@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2
                   (self#meta_list (fun self  -> self#string)) a3) a0
        | `SgInc a0 ->
            Format.fprintf fmt "@[<1>(`SgInc@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_type a1) a0
        | `SgMod a0 ->
            Format.fprintf fmt "@[<1>(`SgMod@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `SgRecMod a0 ->
            Format.fprintf fmt "@[<1>(`SgRecMod@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_binding a1) a0
        | `SgMty a0 ->
            Format.fprintf fmt "@[<1>(`SgMty@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `SgOpn a0 ->
            Format.fprintf fmt "@[<1>(`SgOpn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `SgTyp a0 ->
            Format.fprintf fmt "@[<1>(`SgTyp@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `SgVal a0 ->
            Format.fprintf fmt "@[<1>(`SgVal@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method module_type : 'fmt -> module_type -> 'result=
      fun fmt  ->
        function
        | `MtNil a0 -> Format.fprintf fmt "@[<1>(`MtNil@ %a)@]" self#loc a0
        | `MtId a0 ->
            Format.fprintf fmt "@[<1>(`MtId@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `MtFun a0 ->
            Format.fprintf fmt "@[<1>(`MtFun@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2 self#module_type a3) a0
        | `MtQuo a0 ->
            Format.fprintf fmt "@[<1>(`MtQuo@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `MtSig a0 ->
            Format.fprintf fmt "@[<1>(`MtSig@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#sig_item a1) a0
        | `MtWit a0 ->
            Format.fprintf fmt "@[<1>(`MtWit@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_type a1 self#with_constr a2) a0
        | `MtOf a0 ->
            Format.fprintf fmt "@[<1>(`MtOf@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_expr a1) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method expr : 'fmt -> expr -> 'result=
      fun fmt  ->
        function
        | `ExNil a0 -> Format.fprintf fmt "@[<1>(`ExNil@ %a)@]" self#loc a0
        | `ExId a0 ->
            Format.fprintf fmt "@[<1>(`ExId@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `ExAcc a0 ->
            Format.fprintf fmt "@[<1>(`ExAcc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExApp a0 ->
            Format.fprintf fmt "@[<1>(`ExApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExAre a0 ->
            Format.fprintf fmt "@[<1>(`ExAre@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExArr a0 ->
            Format.fprintf fmt "@[<1>(`ExArr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `ExSem a0 ->
            Format.fprintf fmt "@[<1>(`ExSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExAsf a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" self#loc a0
        | `ExAsr a0 ->
            Format.fprintf fmt "@[<1>(`ExAsr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `ExAss a0 ->
            Format.fprintf fmt "@[<1>(`ExAss@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExChr a0 ->
            Format.fprintf fmt "@[<1>(`ExChr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExCoe a0 ->
            Format.fprintf fmt "@[<1>(`ExCoe@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#ctyp a2 self#ctyp a3) a0
        | `ExFlo a0 ->
            Format.fprintf fmt "@[<1>(`ExFlo@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExFor a0 ->
            Format.fprintf fmt "@[<1>(`ExFor@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1 self#expr a2 self#expr a3
                   self#direction_flag a4 self#expr a5) a0
        | `ExFun a0 ->
            Format.fprintf fmt "@[<1>(`ExFun@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#match_case a1) a0
        | `ExIfe a0 ->
            Format.fprintf fmt "@[<1>(`ExIfe@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2 self#expr a3) a0
        | `ExInt a0 ->
            Format.fprintf fmt "@[<1>(`ExInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExInt32 a0 ->
            Format.fprintf fmt "@[<1>(`ExInt32@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExInt64 a0 ->
            Format.fprintf fmt "@[<1>(`ExInt64@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExNativeInt a0 ->
            Format.fprintf fmt "@[<1>(`ExNativeInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExLab a0 ->
            Format.fprintf fmt "@[<1>(`ExLab@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `ExLaz a0 ->
            Format.fprintf fmt "@[<1>(`ExLaz@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `ExLet a0 ->
            Format.fprintf fmt "@[<1>(`ExLet@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_flag a1 self#binding a2 self#expr a3) a0
        | `ExLmd a0 ->
            Format.fprintf fmt "@[<1>(`ExLmd@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_expr a2 self#expr a3) a0
        | `ExMat a0 ->
            Format.fprintf fmt "@[<1>(`ExMat@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#match_case a2) a0
        | `ExNew a0 ->
            Format.fprintf fmt "@[<1>(`ExNew@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `ExObj a0 ->
            Format.fprintf fmt "@[<1>(`ExObj@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#class_str_item a2) a0
        | `ExOlb a0 ->
            Format.fprintf fmt "@[<1>(`ExOlb@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `ExOvr a0 ->
            Format.fprintf fmt "@[<1>(`ExOvr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#rec_binding a1) a0
        | `ExRec a0 ->
            Format.fprintf fmt "@[<1>(`ExRec@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_binding a1 self#expr a2) a0
        | `ExSeq a0 ->
            Format.fprintf fmt "@[<1>(`ExSeq@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `ExSnd a0 ->
            Format.fprintf fmt "@[<1>(`ExSnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#string a2) a0
        | `ExSte a0 ->
            Format.fprintf fmt "@[<1>(`ExSte@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExStr a0 ->
            Format.fprintf fmt "@[<1>(`ExStr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExTry a0 ->
            Format.fprintf fmt "@[<1>(`ExTry@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#match_case a2) a0
        | `ExTup a0 ->
            Format.fprintf fmt "@[<1>(`ExTup@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `ExCom a0 ->
            Format.fprintf fmt "@[<1>(`ExCom@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExTyc a0 ->
            Format.fprintf fmt "@[<1>(`ExTyc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#ctyp a2) a0
        | `ExVrn a0 ->
            Format.fprintf fmt "@[<1>(`ExVrn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `ExWhi a0 ->
            Format.fprintf fmt "@[<1>(`ExWhi@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `ExOpI a0 ->
            Format.fprintf fmt "@[<1>(`ExOpI@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#expr a2) a0
        | `ExFUN a0 ->
            Format.fprintf fmt "@[<1>(`ExFUN@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `ExPkg a0 ->
            Format.fprintf fmt "@[<1>(`ExPkg@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_expr a1) a0
    method patt : 'fmt -> patt -> 'result=
      fun fmt  ->
        function
        | `PaNil a0 -> Format.fprintf fmt "@[<1>(`PaNil@ %a)@]" self#loc a0
        | `PaId a0 ->
            Format.fprintf fmt "@[<1>(`PaId@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `PaAli a0 ->
            Format.fprintf fmt "@[<1>(`PaAli@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaAny a0 -> Format.fprintf fmt "@[<1>(`PaAny@ %a)@]" self#loc a0
        | `PaApp a0 ->
            Format.fprintf fmt "@[<1>(`PaApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `PaArr a0 ->
            Format.fprintf fmt "@[<1>(`PaArr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#patt
                   a1) a0
        | `PaCom a0 ->
            Format.fprintf fmt "@[<1>(`PaCom@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `PaSem a0 ->
            Format.fprintf fmt "@[<1>(`PaSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `PaChr a0 ->
            Format.fprintf fmt "@[<1>(`PaChr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaInt a0 ->
            Format.fprintf fmt "@[<1>(`PaInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaInt32 a0 ->
            Format.fprintf fmt "@[<1>(`PaInt32@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaInt64 a0 ->
            Format.fprintf fmt "@[<1>(`PaInt64@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaNativeInt a0 ->
            Format.fprintf fmt "@[<1>(`PaNativeInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaFlo a0 ->
            Format.fprintf fmt "@[<1>(`PaFlo@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaLab a0 ->
            Format.fprintf fmt "@[<1>(`PaLab@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#patt a2) a0
        | `PaOlb a0 ->
            Format.fprintf fmt "@[<1>(`PaOlb@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#patt a2) a0
        | `PaOlbi a0 ->
            Format.fprintf fmt "@[<1>(`PaOlbi@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#patt a2 self#expr a3) a0
        | `PaOrp a0 ->
            Format.fprintf fmt "@[<1>(`PaOrp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `PaRng a0 ->
            Format.fprintf fmt "@[<1>(`PaRng@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `PaRec a0 ->
            Format.fprintf fmt "@[<1>(`PaRec@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#patt
                   a1) a0
        | `PaEq a0 ->
            Format.fprintf fmt "@[<1>(`PaEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#patt a2) a0
        | `PaStr a0 ->
            Format.fprintf fmt "@[<1>(`PaStr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaTup a0 ->
            Format.fprintf fmt "@[<1>(`PaTup@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#patt
                   a1) a0
        | `PaTyc a0 ->
            Format.fprintf fmt "@[<1>(`PaTyc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#ctyp a2) a0
        | `PaTyp a0 ->
            Format.fprintf fmt "@[<1>(`PaTyp@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `PaVrn a0 ->
            Format.fprintf fmt "@[<1>(`PaVrn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `PaLaz a0 ->
            Format.fprintf fmt "@[<1>(`PaLaz@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#patt
                   a1) a0
        | `PaMod a0 ->
            Format.fprintf fmt "@[<1>(`PaMod@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method ctyp : 'fmt -> ctyp -> 'result=
      fun fmt  ->
        function
        | `TyNil a0 -> Format.fprintf fmt "@[<1>(`TyNil@ %a)@]" self#loc a0
        | `TyAli a0 ->
            Format.fprintf fmt "@[<1>(`TyAli@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyAny a0 -> Format.fprintf fmt "@[<1>(`TyAny@ %a)@]" self#loc a0
        | `TyApp a0 ->
            Format.fprintf fmt "@[<1>(`TyApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyArr a0 ->
            Format.fprintf fmt "@[<1>(`TyArr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyCls a0 ->
            Format.fprintf fmt "@[<1>(`TyCls@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `TyLab a0 ->
            Format.fprintf fmt "@[<1>(`TyLab@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2) a0
        | `TyId a0 ->
            Format.fprintf fmt "@[<1>(`TyId@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `TyMan a0 ->
            Format.fprintf fmt "@[<1>(`TyMan@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyDcl a0 ->
            Format.fprintf fmt "@[<1>(`TyDcl@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1
                   (self#list (fun self  -> self#ctyp)) a2 self#ctyp a3
                   (self#list
                      (fun self  fmt  (a0,a1)  ->
                         Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#ctyp a0
                           self#ctyp a1)) a4) a0
        | `TyObj a0 ->
            Format.fprintf fmt "@[<1>(`TyObj@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#row_var_flag a2) a0
        | `TyOlb a0 ->
            Format.fprintf fmt "@[<1>(`TyOlb@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2) a0
        | `TyPol a0 ->
            Format.fprintf fmt "@[<1>(`TyPol@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyTypePol a0 ->
            Format.fprintf fmt "@[<1>(`TyTypePol@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyQuo a0 ->
            Format.fprintf fmt "@[<1>(`TyQuo@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `TyQuP a0 ->
            Format.fprintf fmt "@[<1>(`TyQuP@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `TyQuM a0 ->
            Format.fprintf fmt "@[<1>(`TyQuM@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `TyAnP a0 -> Format.fprintf fmt "@[<1>(`TyAnP@ %a)@]" self#loc a0
        | `TyAnM a0 -> Format.fprintf fmt "@[<1>(`TyAnM@ %a)@]" self#loc a0
        | `TyVrn a0 ->
            Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `TyRec a0 ->
            Format.fprintf fmt "@[<1>(`TyRec@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyCol a0 ->
            Format.fprintf fmt "@[<1>(`TyCol@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TySem a0 ->
            Format.fprintf fmt "@[<1>(`TySem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyCom a0 ->
            Format.fprintf fmt "@[<1>(`TyCom@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TySum a0 ->
            Format.fprintf fmt "@[<1>(`TySum@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyOf a0 ->
            Format.fprintf fmt "@[<1>(`TyOf@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyAnd a0 ->
            Format.fprintf fmt "@[<1>(`TyAnd@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyOr a0 ->
            Format.fprintf fmt "@[<1>(`TyOr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyPrv a0 ->
            Format.fprintf fmt "@[<1>(`TyPrv@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyMut a0 ->
            Format.fprintf fmt "@[<1>(`TyMut@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyTup a0 ->
            Format.fprintf fmt "@[<1>(`TyTup@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TySta a0 ->
            Format.fprintf fmt "@[<1>(`TySta@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyVrnEq a0 ->
            Format.fprintf fmt "@[<1>(`TyVrnEq@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyVrnSup a0 ->
            Format.fprintf fmt "@[<1>(`TyVrnSup@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyVrnInf a0 ->
            Format.fprintf fmt "@[<1>(`TyVrnInf@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `TyVrnInfSup a0 ->
            Format.fprintf fmt "@[<1>(`TyVrnInfSup@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyAmp a0 ->
            Format.fprintf fmt "@[<1>(`TyAmp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyOfAmp a0 ->
            Format.fprintf fmt "@[<1>(`TyOfAmp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyPkg a0 ->
            Format.fprintf fmt "@[<1>(`TyPkg@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_type a1) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method ident : 'fmt -> ident -> 'result=
      fun fmt  ->
        function
        | `IdAcc a0 ->
            Format.fprintf fmt "@[<1>(`IdAcc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `IdApp a0 ->
            Format.fprintf fmt "@[<1>(`IdApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `Lid a0 ->
            Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Uid a0 ->
            Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method meta_list :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_list -> 'result=
      fun mf_a  fmt  ->
        function
        | `LNil a0 -> Format.fprintf fmt "@[<1>(`LNil@ %a)@]" self#loc a0
        | `LCons a0 ->
            Format.fprintf fmt "@[<1>(`LCons@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" (mf_a self) a0
                   (self#meta_list mf_a) a1) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_option -> 'result=
      fun mf_a  fmt  ->
        function
        | `None a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" self#loc a0
        | `Some a0 -> Format.fprintf fmt "@[<1>(`Some@ %a)@]" (mf_a self) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method row_var_flag : 'fmt -> row_var_flag -> 'result=
      fun fmt  ->
        function
        | `RowVar a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" self#loc a0
        | `RvNil a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method override_flag : 'fmt -> override_flag -> 'result=
      fun fmt  ->
        function
        | `Override a0 ->
            Format.fprintf fmt "@[<1>(`Override@ %a)@]" self#loc a0
        | `OvNil a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method virtual_flag : 'fmt -> virtual_flag -> 'result=
      fun fmt  ->
        function
        | `Virtual a0 ->
            Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" self#loc a0
        | `ViNil a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method private_flag : 'fmt -> private_flag -> 'result=
      fun fmt  ->
        function
        | `Private a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]" self#loc a0
        | `PrNil a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method mutable_flag : 'fmt -> mutable_flag -> 'result=
      fun fmt  ->
        function
        | `Mutable a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" self#loc a0
        | `MuNil a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method direction_flag : 'fmt -> direction_flag -> 'result=
      fun fmt  ->
        function
        | `To a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" self#loc a0
        | `Downto a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method rec_flag : 'fmt -> rec_flag -> 'result=
      fun fmt  ->
        function
        | `Recursive a0 ->
            Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" self#loc a0
        | `ReNil a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" self#loc a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
    method loc : 'fmt -> loc -> 'result= fun fmt  a0  -> self#fanloc_t fmt a0
    method fanloc_t : 'fmt -> FanLoc.t -> 'result= self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method class_str_item : class_str_item -> 'self_type=
      function
      | `CrNil a0 -> self#loc a0
      | `CrSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_str_item a1 in self#class_str_item a2))
            a0
      | `CrCtr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `CrInh a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#override_flag a1 in
              let self = self#class_expr a2 in self#string a3)) a0
      | `CrIni a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `CrMth a0 ->
          ((fun (a0,a1,a2,a3,a4,a5)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#override_flag a2 in
              let self = self#private_flag a3 in
              let self = self#expr a4 in self#ctyp a5)) a0
      | `CrVal a0 ->
          ((fun (a0,a1,a2,a3,a4)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#override_flag a2 in
              let self = self#mutable_flag a3 in self#expr a4)) a0
      | `CrVir a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#private_flag a2 in self#ctyp a3)) a0
      | `CrVvr a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#mutable_flag a2 in self#ctyp a3)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method class_expr : class_expr -> 'self_type=
      function
      | `CeNil a0 -> self#loc a0
      | `CeApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#expr a2)) a0
      | `CeCon a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#virtual_flag a1 in
              let self = self#ident a2 in self#ctyp a3)) a0
      | `CeFun a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#class_expr a2)) a0
      | `CeLet a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#rec_flag a1 in
              let self = self#binding a2 in self#class_expr a3)) a0
      | `CeStr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#class_str_item a2)) a0
      | `CeTyc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_type a2)) a0
      | `CeAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_expr a2)) a0
      | `CeEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | `CgNil a0 -> self#loc a0
      | `CgCtr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `CgSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_sig_item a1 in self#class_sig_item a2))
            a0
      | `CgInh a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `CgMth a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#private_flag a2 in self#ctyp a3)) a0
      | `CgVal a0 ->
          ((fun (a0,a1,a2,a3,a4)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#mutable_flag a2 in
              let self = self#virtual_flag a3 in self#ctyp a4)) a0
      | `CgVir a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#private_flag a2 in self#ctyp a3)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method class_type : class_type -> 'self_type=
      function
      | `CtNil a0 -> self#loc a0
      | `CtCon a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#virtual_flag a1 in
              let self = self#ident a2 in self#ctyp a3)) a0
      | `CtFun a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#class_type a2)) a0
      | `CtSig a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#class_sig_item a2)) a0
      | `CtAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_type a1 in self#class_type a2)) a0
      | `CtCol a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_type a1 in self#class_type a2)) a0
      | `CtEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_type a1 in self#class_type a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method str_item : str_item -> 'self_type=
      function
      | `StNil a0 -> self#loc a0
      | `StCls a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_expr a1)) a0
      | `StClt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `StSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#str_item a1 in self#str_item a2)) a0
      | `StDir a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `StExc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in
              self#meta_option (fun self  -> self#ident) a2)) a0
      | `StExp a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `StExt a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#ctyp a2 in
              self#meta_list (fun self  -> self#string) a3)) a0
      | `StInc a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
      | `StMod a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_expr a2)) a0
      | `StRecMod a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_binding a1))
            a0
      | `StMty a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `StOpn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `StTyp a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `StVal a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_flag a1 in self#binding a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_expr : module_expr -> 'self_type=
      function
      | `MeNil a0 -> self#loc a0
      | `MeId a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `MeApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_expr a1 in self#module_expr a2)) a0
      | `MeFun a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_expr a3)) a0
      | `MeStr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#str_item a1)) a0
      | `MeTyc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_expr a1 in self#module_type a2)) a0
      | `MePkg a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method match_case : match_case -> 'self_type=
      function
      | `McNil a0 -> self#loc a0
      | `McOr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#match_case a1 in self#match_case a2)) a0
      | `McArr a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in
              let self = self#expr a2 in self#expr a3)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_binding : module_binding -> 'self_type=
      function
      | `MbNil a0 -> self#loc a0
      | `MbAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_binding a1 in self#module_binding a2))
            a0
      | `MbColEq a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_expr a3)) a0
      | `MbCol a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method rec_binding : rec_binding -> 'self_type=
      function
      | `RbNil a0 -> self#loc a0
      | `RbSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_binding a1 in self#rec_binding a2)) a0
      | `RbEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method binding : binding -> 'self_type=
      function
      | `BiNil a0 -> self#loc a0
      | `BiAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#binding a1 in self#binding a2)) a0
      | `BiEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method with_constr : with_constr -> 'self_type=
      function
      | `WcNil a0 -> self#loc a0
      | `WcTyp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `WcMod a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `WcTyS a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `WcMoS a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `WcAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#with_constr a1 in self#with_constr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method sig_item : sig_item -> 'self_type=
      function
      | `SgNil a0 -> self#loc a0
      | `SgCls a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `SgClt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `SgSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#sig_item a1 in self#sig_item a2)) a0
      | `SgDir a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `SgExc a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `SgExt a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#ctyp a2 in
              self#meta_list (fun self  -> self#string) a3)) a0
      | `SgInc a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_type a1))
            a0
      | `SgMod a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `SgRecMod a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_binding a1))
            a0
      | `SgMty a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `SgOpn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `SgTyp a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `SgVal a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#ctyp a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_type : module_type -> 'self_type=
      function
      | `MtNil a0 -> self#loc a0
      | `MtId a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `MtFun a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_type a3)) a0
      | `MtQuo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `MtSig a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#sig_item a1)) a0
      | `MtWit a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_type a1 in self#with_constr a2)) a0
      | `MtOf a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method expr : expr -> 'self_type=
      function
      | `ExNil a0 -> self#loc a0
      | `ExId a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `ExAcc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExAre a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExArr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExAsf a0 -> self#loc a0
      | `ExAsr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExAss a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExChr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExCoe a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in
              let self = self#ctyp a2 in self#ctyp a3)) a0
      | `ExFlo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExFor a0 ->
          ((fun (a0,a1,a2,a3,a4,a5)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#expr a2 in
              let self = self#expr a3 in
              let self = self#direction_flag a4 in self#expr a5)) a0
      | `ExFun a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#match_case a1)) a0
      | `ExIfe a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in
              let self = self#expr a2 in self#expr a3)) a0
      | `ExInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExInt32 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExInt64 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExNativeInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExLab a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `ExLaz a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExLet a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#rec_flag a1 in
              let self = self#binding a2 in self#expr a3)) a0
      | `ExLmd a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_expr a2 in self#expr a3)) a0
      | `ExMat a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#match_case a2)) a0
      | `ExNew a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `ExObj a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#class_str_item a2)) a0
      | `ExOlb a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `ExOvr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#rec_binding a1))
            a0
      | `ExRec a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_binding a1 in self#expr a2)) a0
      | `ExSeq a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExSnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#string a2)) a0
      | `ExSte a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExStr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExTry a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#match_case a2)) a0
      | `ExTup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExCom a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExTyc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#ctyp a2)) a0
      | `ExVrn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `ExWhi a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `ExOpI a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#expr a2)) a0
      | `ExFUN a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `ExPkg a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
    method patt : patt -> 'self_type=
      function
      | `PaNil a0 -> self#loc a0
      | `PaId a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `PaAli a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaAny a0 -> self#loc a0
      | `PaApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `PaArr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaCom a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `PaSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `PaChr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaInt32 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaInt64 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaNativeInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaFlo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaLab a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#patt a2)) a0
      | `PaOlb a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#patt a2)) a0
      | `PaOlbi a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#patt a2 in self#expr a3)) a0
      | `PaOrp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `PaRng a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `PaRec a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#patt a2)) a0
      | `PaStr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaTup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaTyc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#ctyp a2)) a0
      | `PaTyp a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `PaVrn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `PaLaz a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaMod a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method ctyp : ctyp -> 'self_type=
      function
      | `TyNil a0 -> self#loc a0
      | `TyAli a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyAny a0 -> self#loc a0
      | `TyApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyArr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyCls a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `TyLab a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#ctyp a2)) a0
      | `TyId a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `TyMan a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyDcl a0 ->
          ((fun (a0,a1,a2,a3,a4)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#list (fun self  -> self#ctyp) a2 in
              let self = self#ctyp a3 in
              self#list
                (fun self  (a0,a1)  ->
                   let self = self#ctyp a0 in self#ctyp a1) a4)) a0
      | `TyObj a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#row_var_flag a2)) a0
      | `TyOlb a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#ctyp a2)) a0
      | `TyPol a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyTypePol a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyQuo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `TyQuP a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `TyQuM a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `TyAnP a0 -> self#loc a0
      | `TyAnM a0 -> self#loc a0
      | `TyVrn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `TyRec a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyCol a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TySem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyCom a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TySum a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyOf a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyAnd a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyOr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyPrv a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyMut a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyTup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TySta a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyVrnEq a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyVrnSup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyVrnInf a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `TyVrnInfSup a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyAmp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyOfAmp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyPkg a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_type a1))
            a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method ident : ident -> 'self_type=
      function
      | `IdAcc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `IdApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `Lid a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Uid a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_list -> 'self_type=
      fun mf_a  ->
        function
        | `LNil a0 -> self#loc a0
        | `LCons a0 ->
            ((fun (a0,a1)  ->
                let self = mf_a self a0 in self#meta_list mf_a a1)) a0
        | `Ant a0 ->
            ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_option -> 'self_type=
      fun mf_a  ->
        function
        | `None a0 -> self#loc a0
        | `Some a0 -> mf_a self a0
        | `Ant a0 ->
            ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method row_var_flag : row_var_flag -> 'self_type=
      function
      | `RowVar a0 -> self#loc a0
      | `RvNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method override_flag : override_flag -> 'self_type=
      function
      | `Override a0 -> self#loc a0
      | `OvNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method virtual_flag : virtual_flag -> 'self_type=
      function
      | `Virtual a0 -> self#loc a0
      | `ViNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method private_flag : private_flag -> 'self_type=
      function
      | `Private a0 -> self#loc a0
      | `PrNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method mutable_flag : mutable_flag -> 'self_type=
      function
      | `Mutable a0 -> self#loc a0
      | `MuNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method direction_flag : direction_flag -> 'self_type=
      function
      | `To a0 -> self#loc a0
      | `Downto a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method rec_flag : rec_flag -> 'self_type=
      function
      | `Recursive a0 -> self#loc a0
      | `ReNil a0 -> self#loc a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method loc : loc -> 'self_type= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
  end
let rec pp_print_class_str_item: 'fmt -> class_str_item -> 'result =
  fun fmt  ->
    function
    | `CrNil a0 -> Format.fprintf fmt "@[<1>(`CrNil@ %a)@]" pp_print_loc a0
    | `CrSem a0 ->
        Format.fprintf fmt "@[<1>(`CrSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_str_item a1 pp_print_class_str_item a2) a0
    | `CrCtr a0 ->
        Format.fprintf fmt "@[<1>(`CrCtr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `CrInh a0 ->
        Format.fprintf fmt "@[<1>(`CrInh@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_override_flag a1 pp_print_class_expr a2
               pp_print_string a3) a0
    | `CrIni a0 ->
        Format.fprintf fmt "@[<1>(`CrIni@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `CrMth a0 ->
        Format.fprintf fmt "@[<1>(`CrMth@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1 pp_print_override_flag a2
               pp_print_private_flag a3 pp_print_expr a4 pp_print_ctyp a5) a0
    | `CrVal a0 ->
        Format.fprintf fmt "@[<1>(`CrVal@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1 pp_print_override_flag a2
               pp_print_mutable_flag a3 pp_print_expr a4) a0
    | `CrVir a0 ->
        Format.fprintf fmt "@[<1>(`CrVir@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3)
          a0
    | `CrVvr a0 ->
        Format.fprintf fmt "@[<1>(`CrVvr@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_mutable_flag a2 pp_print_ctyp a3)
          a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_class_expr: 'fmt -> class_expr -> 'result =
  fun fmt  ->
    function
    | `CeNil a0 -> Format.fprintf fmt "@[<1>(`CeNil@ %a)@]" pp_print_loc a0
    | `CeApp a0 ->
        Format.fprintf fmt "@[<1>(`CeApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_expr a2) a0
    | `CeCon a0 ->
        Format.fprintf fmt "@[<1>(`CeCon@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3)
          a0
    | `CeFun a0 ->
        Format.fprintf fmt "@[<1>(`CeFun@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_class_expr a2) a0
    | `CeLet a0 ->
        Format.fprintf fmt "@[<1>(`CeLet@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_flag a1 pp_print_binding a2 pp_print_class_expr
               a3) a0
    | `CeStr a0 ->
        Format.fprintf fmt "@[<1>(`CeStr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_class_str_item a2) a0
    | `CeTyc a0 ->
        Format.fprintf fmt "@[<1>(`CeTyc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_class_type a2) a0
    | `CeAnd a0 ->
        Format.fprintf fmt "@[<1>(`CeAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_class_expr a2) a0
    | `CeEq a0 ->
        Format.fprintf fmt "@[<1>(`CeEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_class_expr a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_class_sig_item: 'fmt -> class_sig_item -> 'result =
  fun fmt  ->
    function
    | `CgNil a0 -> Format.fprintf fmt "@[<1>(`CgNil@ %a)@]" pp_print_loc a0
    | `CgCtr a0 ->
        Format.fprintf fmt "@[<1>(`CgCtr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `CgSem a0 ->
        Format.fprintf fmt "@[<1>(`CgSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_sig_item a1 pp_print_class_sig_item a2) a0
    | `CgInh a0 ->
        Format.fprintf fmt "@[<1>(`CgInh@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `CgMth a0 ->
        Format.fprintf fmt "@[<1>(`CgMth@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3)
          a0
    | `CgVal a0 ->
        Format.fprintf fmt "@[<1>(`CgVal@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1 pp_print_mutable_flag a2
               pp_print_virtual_flag a3 pp_print_ctyp a4) a0
    | `CgVir a0 ->
        Format.fprintf fmt "@[<1>(`CgVir@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3)
          a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_class_type: 'fmt -> class_type -> 'result =
  fun fmt  ->
    function
    | `CtNil a0 -> Format.fprintf fmt "@[<1>(`CtNil@ %a)@]" pp_print_loc a0
    | `CtCon a0 ->
        Format.fprintf fmt "@[<1>(`CtCon@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3)
          a0
    | `CtFun a0 ->
        Format.fprintf fmt "@[<1>(`CtFun@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_class_type a2) a0
    | `CtSig a0 ->
        Format.fprintf fmt "@[<1>(`CtSig@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_class_sig_item a2) a0
    | `CtAnd a0 ->
        Format.fprintf fmt "@[<1>(`CtAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1 pp_print_class_type a2) a0
    | `CtCol a0 ->
        Format.fprintf fmt "@[<1>(`CtCol@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1 pp_print_class_type a2) a0
    | `CtEq a0 ->
        Format.fprintf fmt "@[<1>(`CtEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1 pp_print_class_type a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_str_item: 'fmt -> str_item -> 'result =
  fun fmt  ->
    function
    | `StNil a0 -> Format.fprintf fmt "@[<1>(`StNil@ %a)@]" pp_print_loc a0
    | `StCls a0 ->
        Format.fprintf fmt "@[<1>(`StCls@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1) a0
    | `StClt a0 ->
        Format.fprintf fmt "@[<1>(`StClt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `StSem a0 ->
        Format.fprintf fmt "@[<1>(`StSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_str_item a1 pp_print_str_item a2) a0
    | `StDir a0 ->
        Format.fprintf fmt "@[<1>(`StDir@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `StExc a0 ->
        Format.fprintf fmt "@[<1>(`StExc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 (pp_print_meta_option pp_print_ident) a2) a0
    | `StExp a0 ->
        Format.fprintf fmt "@[<1>(`StExp@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `StExt a0 ->
        Format.fprintf fmt "@[<1>(`StExt@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2
               (pp_print_meta_list pp_print_string) a3) a0
    | `StInc a0 ->
        Format.fprintf fmt "@[<1>(`StInc@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1) a0
    | `StMod a0 ->
        Format.fprintf fmt "@[<1>(`StMod@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_expr a2) a0
    | `StRecMod a0 ->
        Format.fprintf fmt "@[<1>(`StRecMod@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1) a0
    | `StMty a0 ->
        Format.fprintf fmt "@[<1>(`StMty@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `StOpn a0 ->
        Format.fprintf fmt "@[<1>(`StOpn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `StTyp a0 ->
        Format.fprintf fmt "@[<1>(`StTyp@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `StVal a0 ->
        Format.fprintf fmt "@[<1>(`StVal@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_flag a1 pp_print_binding a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_module_expr: 'fmt -> module_expr -> 'result =
  fun fmt  ->
    function
    | `MeNil a0 -> Format.fprintf fmt "@[<1>(`MeNil@ %a)@]" pp_print_loc a0
    | `MeId a0 ->
        Format.fprintf fmt "@[<1>(`MeId@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `MeApp a0 ->
        Format.fprintf fmt "@[<1>(`MeApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1 pp_print_module_expr a2) a0
    | `MeFun a0 ->
        Format.fprintf fmt "@[<1>(`MeFun@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2
               pp_print_module_expr a3) a0
    | `MeStr a0 ->
        Format.fprintf fmt "@[<1>(`MeStr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_str_item a1) a0
    | `MeTyc a0 ->
        Format.fprintf fmt "@[<1>(`MeTyc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1 pp_print_module_type a2) a0
    | `MePkg a0 ->
        Format.fprintf fmt "@[<1>(`MePkg@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_match_case: 'fmt -> match_case -> 'result =
  fun fmt  ->
    function
    | `McNil a0 -> Format.fprintf fmt "@[<1>(`McNil@ %a)@]" pp_print_loc a0
    | `McOr a0 ->
        Format.fprintf fmt "@[<1>(`McOr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_match_case a1 pp_print_match_case a2) a0
    | `McArr a0 ->
        Format.fprintf fmt "@[<1>(`McArr@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_expr a2 pp_print_expr a3) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_module_binding: 'fmt -> module_binding -> 'result =
  fun fmt  ->
    function
    | `MbNil a0 -> Format.fprintf fmt "@[<1>(`MbNil@ %a)@]" pp_print_loc a0
    | `MbAnd a0 ->
        Format.fprintf fmt "@[<1>(`MbAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1 pp_print_module_binding a2) a0
    | `MbColEq a0 ->
        Format.fprintf fmt "@[<1>(`MbColEq@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2
               pp_print_module_expr a3) a0
    | `MbCol a0 ->
        Format.fprintf fmt "@[<1>(`MbCol@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_rec_binding: 'fmt -> rec_binding -> 'result =
  fun fmt  ->
    function
    | `RbNil a0 -> Format.fprintf fmt "@[<1>(`RbNil@ %a)@]" pp_print_loc a0
    | `RbSem a0 ->
        Format.fprintf fmt "@[<1>(`RbSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1 pp_print_rec_binding a2) a0
    | `RbEq a0 ->
        Format.fprintf fmt "@[<1>(`RbEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_expr a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_binding: 'fmt -> binding -> 'result =
  fun fmt  ->
    function
    | `BiNil a0 -> Format.fprintf fmt "@[<1>(`BiNil@ %a)@]" pp_print_loc a0
    | `BiAnd a0 ->
        Format.fprintf fmt "@[<1>(`BiAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_binding a1 pp_print_binding a2) a0
    | `BiEq a0 ->
        Format.fprintf fmt "@[<1>(`BiEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_expr a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_with_constr: 'fmt -> with_constr -> 'result =
  fun fmt  ->
    function
    | `WcNil a0 -> Format.fprintf fmt "@[<1>(`WcNil@ %a)@]" pp_print_loc a0
    | `WcTyp a0 ->
        Format.fprintf fmt "@[<1>(`WcTyp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `WcMod a0 ->
        Format.fprintf fmt "@[<1>(`WcMod@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `WcTyS a0 ->
        Format.fprintf fmt "@[<1>(`WcTyS@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `WcMoS a0 ->
        Format.fprintf fmt "@[<1>(`WcMoS@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `WcAnd a0 ->
        Format.fprintf fmt "@[<1>(`WcAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_with_constr a1 pp_print_with_constr a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_sig_item: 'fmt -> sig_item -> 'result =
  fun fmt  ->
    function
    | `SgNil a0 -> Format.fprintf fmt "@[<1>(`SgNil@ %a)@]" pp_print_loc a0
    | `SgCls a0 ->
        Format.fprintf fmt "@[<1>(`SgCls@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `SgClt a0 ->
        Format.fprintf fmt "@[<1>(`SgClt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `SgSem a0 ->
        Format.fprintf fmt "@[<1>(`SgSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_sig_item a1 pp_print_sig_item a2) a0
    | `SgDir a0 ->
        Format.fprintf fmt "@[<1>(`SgDir@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `SgExc a0 ->
        Format.fprintf fmt "@[<1>(`SgExc@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `SgExt a0 ->
        Format.fprintf fmt "@[<1>(`SgExt@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2
               (pp_print_meta_list pp_print_string) a3) a0
    | `SgInc a0 ->
        Format.fprintf fmt "@[<1>(`SgInc@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_type a1) a0
    | `SgMod a0 ->
        Format.fprintf fmt "@[<1>(`SgMod@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `SgRecMod a0 ->
        Format.fprintf fmt "@[<1>(`SgRecMod@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1) a0
    | `SgMty a0 ->
        Format.fprintf fmt "@[<1>(`SgMty@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `SgOpn a0 ->
        Format.fprintf fmt "@[<1>(`SgOpn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `SgTyp a0 ->
        Format.fprintf fmt "@[<1>(`SgTyp@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `SgVal a0 ->
        Format.fprintf fmt "@[<1>(`SgVal@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_module_type: 'fmt -> module_type -> 'result =
  fun fmt  ->
    function
    | `MtNil a0 -> Format.fprintf fmt "@[<1>(`MtNil@ %a)@]" pp_print_loc a0
    | `MtId a0 ->
        Format.fprintf fmt "@[<1>(`MtId@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `MtFun a0 ->
        Format.fprintf fmt "@[<1>(`MtFun@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2
               pp_print_module_type a3) a0
    | `MtQuo a0 ->
        Format.fprintf fmt "@[<1>(`MtQuo@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `MtSig a0 ->
        Format.fprintf fmt "@[<1>(`MtSig@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_sig_item a1) a0
    | `MtWit a0 ->
        Format.fprintf fmt "@[<1>(`MtWit@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_type a1 pp_print_with_constr a2) a0
    | `MtOf a0 ->
        Format.fprintf fmt "@[<1>(`MtOf@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_expr: 'fmt -> expr -> 'result =
  fun fmt  ->
    function
    | `ExNil a0 -> Format.fprintf fmt "@[<1>(`ExNil@ %a)@]" pp_print_loc a0
    | `ExId a0 ->
        Format.fprintf fmt "@[<1>(`ExId@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `ExAcc a0 ->
        Format.fprintf fmt "@[<1>(`ExAcc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExApp a0 ->
        Format.fprintf fmt "@[<1>(`ExApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExAre a0 ->
        Format.fprintf fmt "@[<1>(`ExAre@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExArr a0 ->
        Format.fprintf fmt "@[<1>(`ExArr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `ExSem a0 ->
        Format.fprintf fmt "@[<1>(`ExSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExAsf a0 -> Format.fprintf fmt "@[<1>(`ExAsf@ %a)@]" pp_print_loc a0
    | `ExAsr a0 ->
        Format.fprintf fmt "@[<1>(`ExAsr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `ExAss a0 ->
        Format.fprintf fmt "@[<1>(`ExAss@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExChr a0 ->
        Format.fprintf fmt "@[<1>(`ExChr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExCoe a0 ->
        Format.fprintf fmt "@[<1>(`ExCoe@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_ctyp a2 pp_print_ctyp a3) a0
    | `ExFlo a0 ->
        Format.fprintf fmt "@[<1>(`ExFlo@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExFor a0 ->
        Format.fprintf fmt "@[<1>(`ExFor@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1 pp_print_expr a2
               pp_print_expr a3 pp_print_direction_flag a4 pp_print_expr a5)
          a0
    | `ExFun a0 ->
        Format.fprintf fmt "@[<1>(`ExFun@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_match_case a1) a0
    | `ExIfe a0 ->
        Format.fprintf fmt "@[<1>(`ExIfe@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2 pp_print_expr a3) a0
    | `ExInt a0 ->
        Format.fprintf fmt "@[<1>(`ExInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExInt32 a0 ->
        Format.fprintf fmt "@[<1>(`ExInt32@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExInt64 a0 ->
        Format.fprintf fmt "@[<1>(`ExInt64@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExNativeInt a0 ->
        Format.fprintf fmt "@[<1>(`ExNativeInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExLab a0 ->
        Format.fprintf fmt "@[<1>(`ExLab@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `ExLaz a0 ->
        Format.fprintf fmt "@[<1>(`ExLaz@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `ExLet a0 ->
        Format.fprintf fmt "@[<1>(`ExLet@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_flag a1 pp_print_binding a2 pp_print_expr a3) a0
    | `ExLmd a0 ->
        Format.fprintf fmt "@[<1>(`ExLmd@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_expr a2 pp_print_expr a3)
          a0
    | `ExMat a0 ->
        Format.fprintf fmt "@[<1>(`ExMat@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_match_case a2) a0
    | `ExNew a0 ->
        Format.fprintf fmt "@[<1>(`ExNew@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `ExObj a0 ->
        Format.fprintf fmt "@[<1>(`ExObj@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_class_str_item a2) a0
    | `ExOlb a0 ->
        Format.fprintf fmt "@[<1>(`ExOlb@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `ExOvr a0 ->
        Format.fprintf fmt "@[<1>(`ExOvr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1) a0
    | `ExRec a0 ->
        Format.fprintf fmt "@[<1>(`ExRec@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1 pp_print_expr a2) a0
    | `ExSeq a0 ->
        Format.fprintf fmt "@[<1>(`ExSeq@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `ExSnd a0 ->
        Format.fprintf fmt "@[<1>(`ExSnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_string a2) a0
    | `ExSte a0 ->
        Format.fprintf fmt "@[<1>(`ExSte@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExStr a0 ->
        Format.fprintf fmt "@[<1>(`ExStr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExTry a0 ->
        Format.fprintf fmt "@[<1>(`ExTry@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_match_case a2) a0
    | `ExTup a0 ->
        Format.fprintf fmt "@[<1>(`ExTup@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `ExCom a0 ->
        Format.fprintf fmt "@[<1>(`ExCom@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExTyc a0 ->
        Format.fprintf fmt "@[<1>(`ExTyc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_ctyp a2) a0
    | `ExVrn a0 ->
        Format.fprintf fmt "@[<1>(`ExVrn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `ExWhi a0 ->
        Format.fprintf fmt "@[<1>(`ExWhi@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `ExOpI a0 ->
        Format.fprintf fmt "@[<1>(`ExOpI@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_expr a2) a0
    | `ExFUN a0 ->
        Format.fprintf fmt "@[<1>(`ExFUN@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `ExPkg a0 ->
        Format.fprintf fmt "@[<1>(`ExPkg@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1) a0
and pp_print_patt: 'fmt -> patt -> 'result =
  fun fmt  ->
    function
    | `PaNil a0 -> Format.fprintf fmt "@[<1>(`PaNil@ %a)@]" pp_print_loc a0
    | `PaId a0 ->
        Format.fprintf fmt "@[<1>(`PaId@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `PaAli a0 ->
        Format.fprintf fmt "@[<1>(`PaAli@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaAny a0 -> Format.fprintf fmt "@[<1>(`PaAny@ %a)@]" pp_print_loc a0
    | `PaApp a0 ->
        Format.fprintf fmt "@[<1>(`PaApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `PaArr a0 ->
        Format.fprintf fmt "@[<1>(`PaArr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1) a0
    | `PaCom a0 ->
        Format.fprintf fmt "@[<1>(`PaCom@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `PaSem a0 ->
        Format.fprintf fmt "@[<1>(`PaSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `PaChr a0 ->
        Format.fprintf fmt "@[<1>(`PaChr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaInt a0 ->
        Format.fprintf fmt "@[<1>(`PaInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaInt32 a0 ->
        Format.fprintf fmt "@[<1>(`PaInt32@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaInt64 a0 ->
        Format.fprintf fmt "@[<1>(`PaInt64@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaNativeInt a0 ->
        Format.fprintf fmt "@[<1>(`PaNativeInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaFlo a0 ->
        Format.fprintf fmt "@[<1>(`PaFlo@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaLab a0 ->
        Format.fprintf fmt "@[<1>(`PaLab@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_patt a2) a0
    | `PaOlb a0 ->
        Format.fprintf fmt "@[<1>(`PaOlb@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_patt a2) a0
    | `PaOlbi a0 ->
        Format.fprintf fmt "@[<1>(`PaOlbi@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_patt a2 pp_print_expr a3) a0
    | `PaOrp a0 ->
        Format.fprintf fmt "@[<1>(`PaOrp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `PaRng a0 ->
        Format.fprintf fmt "@[<1>(`PaRng@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `PaRec a0 ->
        Format.fprintf fmt "@[<1>(`PaRec@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1) a0
    | `PaEq a0 ->
        Format.fprintf fmt "@[<1>(`PaEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_patt a2) a0
    | `PaStr a0 ->
        Format.fprintf fmt "@[<1>(`PaStr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaTup a0 ->
        Format.fprintf fmt "@[<1>(`PaTup@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1) a0
    | `PaTyc a0 ->
        Format.fprintf fmt "@[<1>(`PaTyc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_ctyp a2) a0
    | `PaTyp a0 ->
        Format.fprintf fmt "@[<1>(`PaTyp@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `PaVrn a0 ->
        Format.fprintf fmt "@[<1>(`PaVrn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `PaLaz a0 ->
        Format.fprintf fmt "@[<1>(`PaLaz@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1) a0
    | `PaMod a0 ->
        Format.fprintf fmt "@[<1>(`PaMod@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_ctyp: 'fmt -> ctyp -> 'result =
  fun fmt  ->
    function
    | `TyNil a0 -> Format.fprintf fmt "@[<1>(`TyNil@ %a)@]" pp_print_loc a0
    | `TyAli a0 ->
        Format.fprintf fmt "@[<1>(`TyAli@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyAny a0 -> Format.fprintf fmt "@[<1>(`TyAny@ %a)@]" pp_print_loc a0
    | `TyApp a0 ->
        Format.fprintf fmt "@[<1>(`TyApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyArr a0 ->
        Format.fprintf fmt "@[<1>(`TyArr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyCls a0 ->
        Format.fprintf fmt "@[<1>(`TyCls@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `TyLab a0 ->
        Format.fprintf fmt "@[<1>(`TyLab@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2) a0
    | `TyId a0 ->
        Format.fprintf fmt "@[<1>(`TyId@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `TyMan a0 ->
        Format.fprintf fmt "@[<1>(`TyMan@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyDcl a0 ->
        Format.fprintf fmt "@[<1>(`TyDcl@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1
               (pp_print_list pp_print_ctyp) a2 pp_print_ctyp a3
               (pp_print_list
                  (fun fmt  (a0,a1)  ->
                     Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_ctyp a0
                       pp_print_ctyp a1)) a4) a0
    | `TyObj a0 ->
        Format.fprintf fmt "@[<1>(`TyObj@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_row_var_flag a2) a0
    | `TyOlb a0 ->
        Format.fprintf fmt "@[<1>(`TyOlb@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2) a0
    | `TyPol a0 ->
        Format.fprintf fmt "@[<1>(`TyPol@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyTypePol a0 ->
        Format.fprintf fmt "@[<1>(`TyTypePol@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyQuo a0 ->
        Format.fprintf fmt "@[<1>(`TyQuo@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `TyQuP a0 ->
        Format.fprintf fmt "@[<1>(`TyQuP@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `TyQuM a0 ->
        Format.fprintf fmt "@[<1>(`TyQuM@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `TyAnP a0 -> Format.fprintf fmt "@[<1>(`TyAnP@ %a)@]" pp_print_loc a0
    | `TyAnM a0 -> Format.fprintf fmt "@[<1>(`TyAnM@ %a)@]" pp_print_loc a0
    | `TyVrn a0 ->
        Format.fprintf fmt "@[<1>(`TyVrn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `TyRec a0 ->
        Format.fprintf fmt "@[<1>(`TyRec@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyCol a0 ->
        Format.fprintf fmt "@[<1>(`TyCol@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TySem a0 ->
        Format.fprintf fmt "@[<1>(`TySem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyCom a0 ->
        Format.fprintf fmt "@[<1>(`TyCom@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TySum a0 ->
        Format.fprintf fmt "@[<1>(`TySum@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyOf a0 ->
        Format.fprintf fmt "@[<1>(`TyOf@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyAnd a0 ->
        Format.fprintf fmt "@[<1>(`TyAnd@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyOr a0 ->
        Format.fprintf fmt "@[<1>(`TyOr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyPrv a0 ->
        Format.fprintf fmt "@[<1>(`TyPrv@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyMut a0 ->
        Format.fprintf fmt "@[<1>(`TyMut@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyTup a0 ->
        Format.fprintf fmt "@[<1>(`TyTup@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TySta a0 ->
        Format.fprintf fmt "@[<1>(`TySta@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyVrnEq a0 ->
        Format.fprintf fmt "@[<1>(`TyVrnEq@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyVrnSup a0 ->
        Format.fprintf fmt "@[<1>(`TyVrnSup@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyVrnInf a0 ->
        Format.fprintf fmt "@[<1>(`TyVrnInf@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `TyVrnInfSup a0 ->
        Format.fprintf fmt "@[<1>(`TyVrnInfSup@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyAmp a0 ->
        Format.fprintf fmt "@[<1>(`TyAmp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyOfAmp a0 ->
        Format.fprintf fmt "@[<1>(`TyOfAmp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyPkg a0 ->
        Format.fprintf fmt "@[<1>(`TyPkg@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_type a1) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_ident: 'fmt -> ident -> 'result =
  fun fmt  ->
    function
    | `IdAcc a0 ->
        Format.fprintf fmt "@[<1>(`IdAcc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `IdApp a0 ->
        Format.fprintf fmt "@[<1>(`IdApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `Lid a0 ->
        Format.fprintf fmt "@[<1>(`Lid@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Uid a0 ->
        Format.fprintf fmt "@[<1>(`Uid@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_meta_list :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_list -> 'result=
  fun mf_a  fmt  ->
    function
    | `LNil a0 -> Format.fprintf fmt "@[<1>(`LNil@ %a)@]" pp_print_loc a0
    | `LCons a0 ->
        Format.fprintf fmt "@[<1>(`LCons@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" mf_a a0
               (pp_print_meta_list mf_a) a1) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_meta_option :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_option -> 'result=
  fun mf_a  fmt  ->
    function
    | `None a0 -> Format.fprintf fmt "@[<1>(`None@ %a)@]" pp_print_loc a0
    | `Some a0 -> Format.fprintf fmt "@[<1>(`Some@ %a)@]" mf_a a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_row_var_flag: 'fmt -> row_var_flag -> 'result =
  fun fmt  ->
    function
    | `RowVar a0 -> Format.fprintf fmt "@[<1>(`RowVar@ %a)@]" pp_print_loc a0
    | `RvNil a0 -> Format.fprintf fmt "@[<1>(`RvNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_override_flag: 'fmt -> override_flag -> 'result =
  fun fmt  ->
    function
    | `Override a0 ->
        Format.fprintf fmt "@[<1>(`Override@ %a)@]" pp_print_loc a0
    | `OvNil a0 -> Format.fprintf fmt "@[<1>(`OvNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_virtual_flag: 'fmt -> virtual_flag -> 'result =
  fun fmt  ->
    function
    | `Virtual a0 ->
        Format.fprintf fmt "@[<1>(`Virtual@ %a)@]" pp_print_loc a0
    | `ViNil a0 -> Format.fprintf fmt "@[<1>(`ViNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_private_flag: 'fmt -> private_flag -> 'result =
  fun fmt  ->
    function
    | `Private a0 ->
        Format.fprintf fmt "@[<1>(`Private@ %a)@]" pp_print_loc a0
    | `PrNil a0 -> Format.fprintf fmt "@[<1>(`PrNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_mutable_flag: 'fmt -> mutable_flag -> 'result =
  fun fmt  ->
    function
    | `Mutable a0 ->
        Format.fprintf fmt "@[<1>(`Mutable@ %a)@]" pp_print_loc a0
    | `MuNil a0 -> Format.fprintf fmt "@[<1>(`MuNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_direction_flag: 'fmt -> direction_flag -> 'result =
  fun fmt  ->
    function
    | `To a0 -> Format.fprintf fmt "@[<1>(`To@ %a)@]" pp_print_loc a0
    | `Downto a0 -> Format.fprintf fmt "@[<1>(`Downto@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_rec_flag: 'fmt -> rec_flag -> 'result =
  fun fmt  ->
    function
    | `Recursive a0 ->
        Format.fprintf fmt "@[<1>(`Recursive@ %a)@]" pp_print_loc a0
    | `ReNil a0 -> Format.fprintf fmt "@[<1>(`ReNil@ %a)@]" pp_print_loc a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
and pp_print_loc: 'fmt -> loc -> 'result =
  fun fmt  a0  -> FanLoc.pp_print_t fmt a0
module MExpr = struct
  let meta_int _loc i = `ExInt (_loc, (string_of_int i))
  let meta_int32 _loc i = `ExInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = `ExInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = `ExNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = `ExFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = `ExStr (_loc, (safe_string_escaped i))
  let meta_char _loc i = `ExChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = `ExId (_loc, (`Uid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> `ExId (_loc, (`Lid (_loc, "true")))
    | false  -> `ExId (_loc, (`Lid (_loc, "false")))
  let meta_ref mf_a _loc i =
    `ExRec
      (_loc,
        (`RbEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))),
        (`ExNil _loc))
  let mklist loc =
    let rec loop top =
      function
      | [] -> `ExId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
          `ExApp
            (_loc, (`ExApp (_loc, (`ExId (_loc, (`Uid (_loc, "::")))), e1)),
              (loop false el)) in
    loop true
  let mkarray loc arr =
    let rec loop top =
      function
      | [] -> `ExId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
          `ExArr (_loc, (`ExSem (_loc, e1, (loop false el)))) in
    let items = arr |> Array.to_list in loop true items
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> `ExId (_loc, (`Uid (_loc, "None")))
    | Some x ->
        `ExApp (_loc, (`ExId (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end
module MPatt = struct
  let meta_int _loc i = `PaInt (_loc, (string_of_int i))
  let meta_int32 _loc i = `PaInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = `PaInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = `PaNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = `PaFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = `PaStr (_loc, (safe_string_escaped i))
  let meta_char _loc i = `PaChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = `PaId (_loc, (`Uid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> `PaId (_loc, (`Lid (_loc, "true")))
    | false  -> `PaId (_loc, (`Lid (_loc, "false")))
  let meta_ref mf_a _loc i =
    `PaRec
      (_loc,
        (`PaEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))
  let mklist loc =
    let rec loop top =
      function
      | [] -> `PaId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
          `PaApp
            (_loc, (`PaApp (_loc, (`PaId (_loc, (`Uid (_loc, "::")))), e1)),
              (loop false el)) in
    loop true
  let mkarray loc arr =
    let rec loop top =
      function
      | [] -> `PaId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
          `PaArr (_loc, (`PaSem (_loc, e1, (loop false el)))) in
    let items = arr |> Array.to_list in loop true items
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> `PaId (_loc, (`Uid (_loc, "None")))
    | Some x ->
        `PaApp (_loc, (`PaId (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end
module Make(MetaLoc:META_LOC) =
  struct
  module Expr = struct
    open MExpr let meta_loc = MetaLoc.meta_loc_expr
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | `CrNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CrNil")), (meta_loc _loc a0))
        | `CrSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_str_item _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `CrCtr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrCtr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `CrInh a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrInh")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_override_flag _loc a1),
                                   (`ExCom
                                      (_loc, (meta_class_expr _loc a2),
                                        (meta_string _loc a3)))))))))) _loc
                   a0))
        | `CrIni a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrIni")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `CrMth a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrMth")),
                (((fun _loc  (a0,a1,a2,a3,a4,a5)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_override_flag _loc a2),
                                        (`ExCom
                                           (_loc,
                                             (meta_private_flag _loc a3),
                                             (`ExCom
                                                (_loc, (meta_expr _loc a4),
                                                  (meta_ctyp _loc a5))))))))))))))
                   _loc a0))
        | `CrVal a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrVal")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_override_flag _loc a2),
                                        (`ExCom
                                           (_loc,
                                             (meta_mutable_flag _loc a3),
                                             (meta_expr _loc a4))))))))))))
                   _loc a0))
        | `CrVir a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrVir")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CrVvr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CrVvr")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_mutable_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | `CeNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CeNil")), (meta_loc _loc a0))
        | `CeApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `CeCon a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeCon")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_virtual_flag _loc a1),
                                   (`ExCom
                                      (_loc, (meta_ident _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CeFun a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeFun")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `CeLet a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeLet")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (`ExCom
                                      (_loc, (meta_binding _loc a2),
                                        (meta_class_expr _loc a3))))))))))
                   _loc a0))
        | `CeStr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeStr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `CeTyc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CeAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `CeEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CeEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | `CgNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CgNil")), (meta_loc _loc a0))
        | `CgCtr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgCtr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `CgSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_sig_item _loc a1),
                                   (meta_class_sig_item _loc a2)))))))) _loc
                   a0))
        | `CgInh a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgInh")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `CgMth a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgMth")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CgVal a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgVal")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_mutable_flag _loc a2),
                                        (`ExCom
                                           (_loc,
                                             (meta_virtual_flag _loc a3),
                                             (meta_ctyp _loc a4))))))))))))
                   _loc a0))
        | `CgVir a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CgVir")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | `CtNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CtNil")), (meta_loc _loc a0))
        | `CtCon a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtCon")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_virtual_flag _loc a1),
                                   (`ExCom
                                      (_loc, (meta_ident _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CtFun a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtFun")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtSig a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtSig")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_class_sig_item _loc a2)))))))) _loc
                   a0))
        | `CtAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtCol a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "CtEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | `StNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "StNil")), (meta_loc _loc a0))
        | `StCls a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StCls")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_expr _loc a1)))))) _loc a0))
        | `StClt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StClt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `StSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_str_item _loc a1),
                                   (meta_str_item _loc a2)))))))) _loc a0))
        | `StDir a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StDir")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `StExc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StExc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_meta_option meta_ident _loc a2))))))))
                   _loc a0))
        | `StExp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StExp")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `StExt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StExt")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_meta_list meta_string _loc a3))))))))))
                   _loc a0))
        | `StInc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StInc")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
        | `StMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_expr _loc a2)))))))) _loc a0))
        | `StRecMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StRecMod")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_binding _loc a1)))))) _loc a0))
        | `StMty a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StMty")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `StOpn a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StOpn")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `StTyp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StTyp")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `StVal a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "StVal")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (meta_binding _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | `MeNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MeNil")), (meta_loc _loc a0))
        | `MeId a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MeId")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `MeApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MeApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_module_expr _loc a1),
                                   (meta_module_expr _loc a2)))))))) _loc a0))
        | `MeFun a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MeFun")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_expr _loc a3))))))))))
                   _loc a0))
        | `MeStr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MeStr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_str_item _loc a1)))))) _loc a0))
        | `MeTyc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MeTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_module_expr _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `MePkg a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MePkg")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | `McNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "McNil")), (meta_loc _loc a0))
        | `McOr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "McOr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_match_case _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `McArr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "McArr")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (`ExCom
                                      (_loc, (meta_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | `MbNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MbNil")), (meta_loc _loc a0))
        | `MbAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MbAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_module_binding _loc a1),
                                   (meta_module_binding _loc a2)))))))) _loc
                   a0))
        | `MbColEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MbColEq")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_expr _loc a3))))))))))
                   _loc a0))
        | `MbCol a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MbCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | `RbNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "RbNil")), (meta_loc _loc a0))
        | `RbSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "RbSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_rec_binding _loc a1),
                                   (meta_rec_binding _loc a2)))))))) _loc a0))
        | `RbEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "RbEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | `BiNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "BiNil")), (meta_loc _loc a0))
        | `BiAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "BiAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_binding _loc a1),
                                   (meta_binding _loc a2)))))))) _loc a0))
        | `BiEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "BiEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | `WcNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "WcNil")), (meta_loc _loc a0))
        | `WcTyp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "WcTyp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `WcMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "WcMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `WcTyS a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "WcTyS")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `WcMoS a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "WcMoS")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `WcAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "WcAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_with_constr _loc a1),
                                   (meta_with_constr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | `SgNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "SgNil")), (meta_loc _loc a0))
        | `SgCls a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgCls")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `SgClt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgClt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `SgSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_sig_item _loc a1),
                                   (meta_sig_item _loc a2)))))))) _loc a0))
        | `SgDir a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgDir")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `SgExc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgExc")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `SgExt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgExt")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_meta_list meta_string _loc a3))))))))))
                   _loc a0))
        | `SgInc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgInc")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_type _loc a1)))))) _loc a0))
        | `SgMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `SgRecMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgRecMod")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_binding _loc a1)))))) _loc a0))
        | `SgMty a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgMty")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `SgOpn a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgOpn")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `SgTyp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgTyp")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `SgVal a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "SgVal")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | `MtNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MtNil")), (meta_loc _loc a0))
        | `MtId a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtId")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `MtFun a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtFun")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_type _loc a3))))))))))
                   _loc a0))
        | `MtQuo a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtQuo")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `MtSig a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtSig")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_sig_item _loc a1)))))) _loc a0))
        | `MtWit a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtWit")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_module_type _loc a1),
                                   (meta_with_constr _loc a2)))))))) _loc a0))
        | `MtOf a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "MtOf")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | `ExNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ExNil")), (meta_loc _loc a0))
        | `ExId a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExId")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `ExAcc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExAcc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
        | `ExApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExAre a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExAre")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExArr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExArr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExAsf a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ExAsf")), (meta_loc _loc a0))
        | `ExAsr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExAsr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExAss a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExAss")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExChr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExChr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExCoe a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExCoe")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (`ExCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `ExFlo a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExFlo")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExFor a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExFor")),
                (((fun _loc  (a0,a1,a2,a3,a4,a5)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_expr _loc a2),
                                        (`ExCom
                                           (_loc, (meta_expr _loc a3),
                                             (`ExCom
                                                (_loc,
                                                  (meta_direction_flag _loc
                                                     a4),
                                                  (meta_expr _loc a5))))))))))))))
                   _loc a0))
        | `ExFun a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExFun")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_match_case _loc a1)))))) _loc a0))
        | `ExIfe a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExIfe")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (`ExCom
                                      (_loc, (meta_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExInt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExInt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExInt32 a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExInt32")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExInt64 a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExInt64")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExNativeInt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExNativeInt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExLab a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExLaz a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExLaz")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExLet a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExLet")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (`ExCom
                                      (_loc, (meta_binding _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExLmd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExLmd")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_module_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExMat a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExMat")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `ExNew a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExNew")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `ExObj a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExObj")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `ExOlb a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExOvr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExOvr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_rec_binding _loc a1)))))) _loc a0))
        | `ExRec a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExRec")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_rec_binding _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExSeq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExSeq")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExSnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExSnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_string _loc a2)))))))) _loc a0))
        | `ExSte a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExSte")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExStr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExStr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExTry a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExTry")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `ExTup a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExTup")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExCom a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExTyc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `ExVrn a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExVrn")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExWhi a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExWhi")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExOpI a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExOpI")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExFUN a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExFUN")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExPkg a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "ExPkg")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | `PaNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "PaNil")), (meta_loc _loc a0))
        | `PaId a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaId")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `PaAli a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaAli")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
        | `PaAny a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "PaAny")), (meta_loc _loc a0))
        | `PaApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaArr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaArr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaCom a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaSem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaChr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaChr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaInt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt32 a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaInt32")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt64 a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaInt64")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaNativeInt a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaNativeInt")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaFlo a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaFlo")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaLab a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaOlb a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaOlbi a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaOlbi")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_patt _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `PaOrp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaOrp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaRng a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaRng")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaRec a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaRec")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaStr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaStr")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaTup a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaTup")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaTyc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `PaTyp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaTyp")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `PaVrn a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaVrn")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaLaz a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaLaz")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaMod a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "PaMod")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | `TyNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyNil")), (meta_loc _loc a0))
        | `TyAli a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyAli")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAny a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAny")), (meta_loc _loc a0))
        | `TyApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyArr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyArr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyCls a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyCls")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `TyLab a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyId a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyId")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `TyMan a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyMan")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyDcl a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyDcl")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (`ExCom
                                      (_loc, (meta_list meta_ctyp _loc a2),
                                        (`ExCom
                                           (_loc, (meta_ctyp _loc a3),
                                             (meta_list
                                                (fun _loc  (a0,a1)  ->
                                                   `ExTup
                                                     (_loc,
                                                       (`ExCom
                                                          (_loc,
                                                            (meta_ctyp _loc
                                                               a0),
                                                            (meta_ctyp _loc
                                                               a1))))) _loc
                                                a4)))))))))))) _loc a0))
        | `TyObj a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyObj")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_row_var_flag _loc a2)))))))) _loc a0))
        | `TyOlb a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPol a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyPol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyTypePol a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyTypePol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyQuo a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyQuo")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyQuP a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyQuP")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyQuM a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyQuM")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyAnP a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAnP")), (meta_loc _loc a0))
        | `TyAnM a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAnM")), (meta_loc _loc a0))
        | `TyVrn a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyVrn")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyRec a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyRec")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyCol a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TySem a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TySem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyCom a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TySum a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TySum")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyOf a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyOf")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAnd a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyOr a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyOr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPrv a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyPrv")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyMut a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyMut")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyTup a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyTup")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TySta a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TySta")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyVrnEq a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyVrnEq")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnSup a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyVrnSup")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnInf a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyVrnInf")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnInfSup a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyVrnInfSup")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAmp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyAmp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyOfAmp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyOfAmp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPkg a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "TyPkg")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_type _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | `IdAcc a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "IdAcc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `IdApp a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "IdApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0),
                              (`ExCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `Lid a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "Lid")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `Uid a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "Uid")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | `LNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "LNil")), (meta_loc _loc a0))
        | `LCons a0 ->
            `ExApp
              (_loc, (`ExVrn (_loc, "LCons")),
                (((fun _loc  (a0,a1)  ->
                     `ExTup
                       (_loc,
                         (`ExCom
                            (_loc, (mf_a _loc a0),
                              (meta_meta_list mf_a _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | `None a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "None")), (meta_loc _loc a0))
        | `Some a0 -> `ExApp (_loc, (`ExVrn (_loc, "Some")), (mf_a _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | `RowVar a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "RowVar")), (meta_loc _loc a0))
        | `RvNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "RvNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | `Override a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Override")), (meta_loc _loc a0))
        | `OvNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "OvNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | `Virtual a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Virtual")), (meta_loc _loc a0))
        | `ViNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ViNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | `Private a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Private")), (meta_loc _loc a0))
        | `PrNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "PrNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | `Mutable a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Mutable")), (meta_loc _loc a0))
        | `MuNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MuNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | `To a0 -> `ExApp (_loc, (`ExVrn (_loc, "To")), (meta_loc _loc a0))
        | `Downto a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Downto")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | `Recursive a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "Recursive")), (meta_loc _loc a0))
        | `ReNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ReNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    end
  module Patt = struct
    open MPatt let meta_loc = MetaLoc.meta_loc_patt
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | `CrNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CrNil")), (meta_loc _loc a0))
        | `CrSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_str_item _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `CrCtr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrCtr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `CrInh a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrInh")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_override_flag _loc a1),
                                   (`PaCom
                                      (_loc, (meta_class_expr _loc a2),
                                        (meta_string _loc a3)))))))))) _loc
                   a0))
        | `CrIni a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrIni")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `CrMth a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrMth")),
                (((fun _loc  (a0,a1,a2,a3,a4,a5)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_override_flag _loc a2),
                                        (`PaCom
                                           (_loc,
                                             (meta_private_flag _loc a3),
                                             (`PaCom
                                                (_loc, (meta_expr _loc a4),
                                                  (meta_ctyp _loc a5))))))))))))))
                   _loc a0))
        | `CrVal a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrVal")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_override_flag _loc a2),
                                        (`PaCom
                                           (_loc,
                                             (meta_mutable_flag _loc a3),
                                             (meta_expr _loc a4))))))))))))
                   _loc a0))
        | `CrVir a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrVir")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CrVvr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CrVvr")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_mutable_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | `CeNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CeNil")), (meta_loc _loc a0))
        | `CeApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `CeCon a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeCon")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_virtual_flag _loc a1),
                                   (`PaCom
                                      (_loc, (meta_ident _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CeFun a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeFun")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `CeLet a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeLet")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (`PaCom
                                      (_loc, (meta_binding _loc a2),
                                        (meta_class_expr _loc a3))))))))))
                   _loc a0))
        | `CeStr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeStr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `CeTyc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CeAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `CeEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CeEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_expr _loc a1),
                                   (meta_class_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | `CgNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CgNil")), (meta_loc _loc a0))
        | `CgCtr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgCtr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `CgSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_sig_item _loc a1),
                                   (meta_class_sig_item _loc a2)))))))) _loc
                   a0))
        | `CgInh a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgInh")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `CgMth a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgMth")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CgVal a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgVal")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_mutable_flag _loc a2),
                                        (`PaCom
                                           (_loc,
                                             (meta_virtual_flag _loc a3),
                                             (meta_ctyp _loc a4))))))))))))
                   _loc a0))
        | `CgVir a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CgVir")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_private_flag _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | `CtNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CtNil")), (meta_loc _loc a0))
        | `CtCon a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtCon")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_virtual_flag _loc a1),
                                   (`PaCom
                                      (_loc, (meta_ident _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `CtFun a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtFun")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtSig a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtSig")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_class_sig_item _loc a2)))))))) _loc
                   a0))
        | `CtAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtCol a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `CtEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "CtEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_class_type _loc a1),
                                   (meta_class_type _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | `StNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "StNil")), (meta_loc _loc a0))
        | `StCls a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StCls")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_expr _loc a1)))))) _loc a0))
        | `StClt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StClt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `StSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_str_item _loc a1),
                                   (meta_str_item _loc a2)))))))) _loc a0))
        | `StDir a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StDir")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `StExc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StExc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_meta_option meta_ident _loc a2))))))))
                   _loc a0))
        | `StExp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StExp")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `StExt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StExt")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_meta_list meta_string _loc a3))))))))))
                   _loc a0))
        | `StInc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StInc")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
        | `StMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_expr _loc a2)))))))) _loc a0))
        | `StRecMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StRecMod")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_binding _loc a1)))))) _loc a0))
        | `StMty a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StMty")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `StOpn a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StOpn")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `StTyp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StTyp")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `StVal a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "StVal")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (meta_binding _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | `MeNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MeNil")), (meta_loc _loc a0))
        | `MeId a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MeId")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `MeApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MeApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_module_expr _loc a1),
                                   (meta_module_expr _loc a2)))))))) _loc a0))
        | `MeFun a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MeFun")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_expr _loc a3))))))))))
                   _loc a0))
        | `MeStr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MeStr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_str_item _loc a1)))))) _loc a0))
        | `MeTyc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MeTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_module_expr _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `MePkg a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MePkg")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | `McNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "McNil")), (meta_loc _loc a0))
        | `McOr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "McOr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_match_case _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `McArr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "McArr")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (`PaCom
                                      (_loc, (meta_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | `MbNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MbNil")), (meta_loc _loc a0))
        | `MbAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MbAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_module_binding _loc a1),
                                   (meta_module_binding _loc a2)))))))) _loc
                   a0))
        | `MbColEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MbColEq")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_expr _loc a3))))))))))
                   _loc a0))
        | `MbCol a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MbCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | `RbNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "RbNil")), (meta_loc _loc a0))
        | `RbSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "RbSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_rec_binding _loc a1),
                                   (meta_rec_binding _loc a2)))))))) _loc a0))
        | `RbEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "RbEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | `BiNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "BiNil")), (meta_loc _loc a0))
        | `BiAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "BiAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_binding _loc a1),
                                   (meta_binding _loc a2)))))))) _loc a0))
        | `BiEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "BiEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | `WcNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "WcNil")), (meta_loc _loc a0))
        | `WcTyp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "WcTyp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `WcMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "WcMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `WcTyS a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "WcTyS")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `WcMoS a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "WcMoS")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `WcAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "WcAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_with_constr _loc a1),
                                   (meta_with_constr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | `SgNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "SgNil")), (meta_loc _loc a0))
        | `SgCls a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgCls")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `SgClt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgClt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_class_type _loc a1)))))) _loc a0))
        | `SgSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_sig_item _loc a1),
                                   (meta_sig_item _loc a2)))))))) _loc a0))
        | `SgDir a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgDir")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `SgExc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgExc")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `SgExt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgExt")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_meta_list meta_string _loc a3))))))))))
                   _loc a0))
        | `SgInc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgInc")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_type _loc a1)))))) _loc a0))
        | `SgMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgMod")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `SgRecMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgRecMod")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_binding _loc a1)))))) _loc a0))
        | `SgMty a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgMty")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_module_type _loc a2)))))))) _loc a0))
        | `SgOpn a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgOpn")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `SgTyp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgTyp")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `SgVal a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "SgVal")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | `MtNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MtNil")), (meta_loc _loc a0))
        | `MtId a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtId")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `MtFun a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtFun")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_module_type _loc a2),
                                        (meta_module_type _loc a3))))))))))
                   _loc a0))
        | `MtQuo a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtQuo")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `MtSig a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtSig")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_sig_item _loc a1)))))) _loc a0))
        | `MtWit a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtWit")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_module_type _loc a1),
                                   (meta_with_constr _loc a2)))))))) _loc a0))
        | `MtOf a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "MtOf")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | `ExNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ExNil")), (meta_loc _loc a0))
        | `ExId a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExId")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `ExAcc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExAcc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
        | `ExApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExAre a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExAre")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExArr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExArr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExAsf a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ExAsf")), (meta_loc _loc a0))
        | `ExAsr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExAsr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExAss a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExAss")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExChr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExChr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExCoe a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExCoe")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (`PaCom
                                      (_loc, (meta_ctyp _loc a2),
                                        (meta_ctyp _loc a3)))))))))) _loc a0))
        | `ExFlo a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExFlo")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExFor a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExFor")),
                (((fun _loc  (a0,a1,a2,a3,a4,a5)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_expr _loc a2),
                                        (`PaCom
                                           (_loc, (meta_expr _loc a3),
                                             (`PaCom
                                                (_loc,
                                                  (meta_direction_flag _loc
                                                     a4),
                                                  (meta_expr _loc a5))))))))))))))
                   _loc a0))
        | `ExFun a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExFun")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_match_case _loc a1)))))) _loc a0))
        | `ExIfe a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExIfe")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (`PaCom
                                      (_loc, (meta_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExInt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExInt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExInt32 a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExInt32")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExInt64 a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExInt64")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExNativeInt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExNativeInt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExLab a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExLaz a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExLaz")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExLet a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExLet")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_rec_flag _loc a1),
                                   (`PaCom
                                      (_loc, (meta_binding _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExLmd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExLmd")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_module_expr _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `ExMat a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExMat")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `ExNew a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExNew")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `ExObj a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExObj")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_class_str_item _loc a2)))))))) _loc
                   a0))
        | `ExOlb a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExOvr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExOvr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_rec_binding _loc a1)))))) _loc a0))
        | `ExRec a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExRec")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_rec_binding _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExSeq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExSeq")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExSnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExSnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_string _loc a2)))))))) _loc a0))
        | `ExSte a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExSte")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExStr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExStr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExTry a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExTry")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_match_case _loc a2)))))))) _loc a0))
        | `ExTup a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExTup")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_expr _loc a1))))))
                   _loc a0))
        | `ExCom a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExTyc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `ExVrn a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExVrn")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `ExWhi a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExWhi")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_expr _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExOpI a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExOpI")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExFUN a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExFUN")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_expr _loc a2)))))))) _loc a0))
        | `ExPkg a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "ExPkg")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_expr _loc a1)))))) _loc a0))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | `PaNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "PaNil")), (meta_loc _loc a0))
        | `PaId a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaId")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `PaAli a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaAli")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `Ant a0 -> `Ant a0
        | `PaAny a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "PaAny")), (meta_loc _loc a0))
        | `PaApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaArr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaArr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaCom a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaSem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaSem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaChr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaChr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaInt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt32 a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaInt32")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaInt64 a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaInt64")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaNativeInt a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaNativeInt")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaFlo a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaFlo")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaLab a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaOlb a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaOlbi a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaOlbi")),
                (((fun _loc  (a0,a1,a2,a3)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_patt _loc a2),
                                        (meta_expr _loc a3)))))))))) _loc a0))
        | `PaOrp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaOrp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaRng a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaRng")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaRec a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaRec")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaEq")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_patt _loc a2)))))))) _loc a0))
        | `PaStr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaStr")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaTup a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaTup")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaTyc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaTyc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_patt _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `PaTyp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaTyp")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `PaVrn a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaVrn")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `PaLaz a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaLaz")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_patt _loc a1))))))
                   _loc a0))
        | `PaMod a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "PaMod")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | `TyNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyNil")), (meta_loc _loc a0))
        | `TyAli a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyAli")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAny a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAny")), (meta_loc _loc a0))
        | `TyApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyArr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyArr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyCls a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyCls")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `TyLab a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyLab")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyId a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyId")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ident _loc a1))))))
                   _loc a0))
        | `TyMan a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyMan")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyDcl a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyDcl")),
                (((fun _loc  (a0,a1,a2,a3,a4)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (`PaCom
                                      (_loc, (meta_list meta_ctyp _loc a2),
                                        (`PaCom
                                           (_loc, (meta_ctyp _loc a3),
                                             (meta_list
                                                (fun _loc  (a0,a1)  ->
                                                   `PaTup
                                                     (_loc,
                                                       (`PaCom
                                                          (_loc,
                                                            (meta_ctyp _loc
                                                               a0),
                                                            (meta_ctyp _loc
                                                               a1))))) _loc
                                                a4)))))))))))) _loc a0))
        | `TyObj a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyObj")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_row_var_flag _loc a2)))))))) _loc a0))
        | `TyOlb a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyOlb")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_string _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPol a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyPol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyTypePol a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyTypePol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyQuo a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyQuo")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyQuP a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyQuP")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyQuM a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyQuM")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyAnP a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAnP")), (meta_loc _loc a0))
        | `TyAnM a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAnM")), (meta_loc _loc a0))
        | `TyVrn a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyVrn")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `TyRec a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyRec")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyCol a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyCol")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TySem a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TySem")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyCom a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyCom")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TySum a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TySum")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyOf a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyOf")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAnd a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyAnd")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyOr a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyOr")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPrv a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyPrv")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyMut a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyMut")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyTup a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyTup")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TySta a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TySta")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyVrnEq a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyVrnEq")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnSup a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyVrnSup")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnInf a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyVrnInf")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_ctyp _loc a1))))))
                   _loc a0))
        | `TyVrnInfSup a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyVrnInfSup")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyAmp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyAmp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyOfAmp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyOfAmp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ctyp _loc a1),
                                   (meta_ctyp _loc a2)))))))) _loc a0))
        | `TyPkg a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "TyPkg")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (meta_module_type _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | `IdAcc a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "IdAcc")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `IdApp a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "IdApp")),
                (((fun _loc  (a0,a1,a2)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0),
                              (`PaCom
                                 (_loc, (meta_ident _loc a1),
                                   (meta_ident _loc a2)))))))) _loc a0))
        | `Lid a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "Lid")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `Uid a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "Uid")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (meta_loc _loc a0), (meta_string _loc a1))))))
                   _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | `LNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "LNil")), (meta_loc _loc a0))
        | `LCons a0 ->
            `PaApp
              (_loc, (`PaVrn (_loc, "LCons")),
                (((fun _loc  (a0,a1)  ->
                     `PaTup
                       (_loc,
                         (`PaCom
                            (_loc, (mf_a _loc a0),
                              (meta_meta_list mf_a _loc a1)))))) _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | `None a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "None")), (meta_loc _loc a0))
        | `Some a0 -> `PaApp (_loc, (`PaVrn (_loc, "Some")), (mf_a _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | `RowVar a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "RowVar")), (meta_loc _loc a0))
        | `RvNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "RvNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | `Override a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Override")), (meta_loc _loc a0))
        | `OvNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "OvNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | `Virtual a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Virtual")), (meta_loc _loc a0))
        | `ViNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ViNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | `Private a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Private")), (meta_loc _loc a0))
        | `PrNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "PrNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | `Mutable a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Mutable")), (meta_loc _loc a0))
        | `MuNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MuNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | `To a0 -> `PaApp (_loc, (`PaVrn (_loc, "To")), (meta_loc _loc a0))
        | `Downto a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Downto")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | `Recursive a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "Recursive")), (meta_loc _loc a0))
        | `ReNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ReNil")), (meta_loc _loc a0))
        | `Ant a0 -> `Ant a0
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
    | `ExId (_loc,`Lid (_,_)) -> error ()
    | `ExId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function
  | `ExId (_loc,i) -> i
  | `ExApp (_loc,_,_) -> error ()
  | t -> self t
let ident_of_ctyp =
  let error () = invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =
    function
    | `TyApp (_loc,t1,t2) -> `IdApp (_loc, (self t1), (self t2))
    | `TyId (_loc,`Lid (_,_)) -> error ()
    | `TyId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `TyId (_loc,i) -> i | t -> self t
let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self =
    function
    | `PaApp (_loc,p1,p2) -> `IdApp (_loc, (self p1), (self p2))
    | `PaId (_loc,`Lid (_,_)) -> error ()
    | `PaId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `PaId (_loc,i) -> i | p -> self p
let rec is_irrefut_patt =
  function
  | `PaId (_loc,`Lid (_,_)) -> true
  | `PaId (_loc,`Uid (_,"()")) -> true
  | `PaAny _loc -> true
  | `PaNil _loc -> true
  | `PaAli (_loc,x,y) -> (is_irrefut_patt x) && (is_irrefut_patt y)
  | `PaRec (_loc,p) -> is_irrefut_patt p
  | `PaEq (_loc,_,p) -> is_irrefut_patt p
  | `PaSem (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaCom (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaOrp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaApp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaTyc (_loc,p,_) -> is_irrefut_patt p
  | `PaTup (_loc,pl) -> is_irrefut_patt pl
  | `PaOlb (_loc,_,`PaNil _) -> true
  | `PaOlb (_loc,_,_) -> true
  | `PaOlbi (_loc,_,_,_) -> true
  | `PaLab (_loc,_,`PaNil _) -> true
  | `PaLab (_loc,_,p) -> is_irrefut_patt p
  | `PaLaz (_loc,p) -> is_irrefut_patt p
  | `PaId (_loc,_) -> false
  | `PaMod (_loc,_) -> true
  | `PaVrn (_loc,_)|`PaStr (_loc,_)|`PaRng (_loc,_,_)|`PaFlo (_loc,_)|
      `PaNativeInt (_loc,_)|`PaInt64 (_loc,_)|`PaInt32 (_loc,_)|`PaInt
                                                                  (_loc,_)|
      `PaChr (_loc,_)|`PaTyp (_loc,_)|`PaArr (_loc,_)|`Ant (_loc,_) -> false
let rec is_constructor =
  function
  | `IdAcc (_loc,_,i) -> is_constructor i
  | `Uid (_loc,_) -> true
  | `Lid (_loc,_)|`IdApp (_loc,_,_) -> false
  | `Ant (_loc,_) -> assert false
let is_patt_constructor =
  function
  | `PaId (_loc,i) -> is_constructor i
  | `PaVrn (_loc,_) -> true
  | _ -> false
let rec is_expr_constructor =
  function
  | `ExId (_loc,i) -> is_constructor i
  | `ExAcc (_loc,e1,e2) ->
      (is_expr_constructor e1) && (is_expr_constructor e2)
  | `ExVrn (_loc,_) -> true
  | _ -> false
let ghost = FanLoc.ghost
let rec tyOr_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TyOr (_loc, t, (tyOr_of_list ts))
let rec tyAnd_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TyAnd (_loc, t, (tyAnd_of_list ts))
let rec tySem_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TySem (_loc, t, (tySem_of_list ts))
let rec tyCom_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TyCom (_loc, t, (tyCom_of_list ts))
let rec tyAmp_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TyAmp (_loc, t, (tyAmp_of_list ts))
let rec tySta_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in `TySta (_loc, t, (tySta_of_list ts))
let tyApp_of_list =
  function
  | [] -> `TyNil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_ctyp x in `TyApp (_loc, x, y)) t ts
let tyVarApp_of_list (_loc,ls) =
  let aux =
    function
    | [] -> `TyNil ghost
    | t::[] -> `TyQuo (_loc, t)
    | t::ts ->
        List.fold_left (fun x  y  -> `TyApp (_loc, x, (`TyQuo (_loc, y))))
          (`TyQuo (_loc, t)) ts in
  aux ls
let rec stSem_of_list =
  function
  | [] -> `StNil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_str_item t in `StSem (_loc, t, (stSem_of_list ts))
let rec sgSem_of_list =
  function
  | [] -> `SgNil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_sig_item t in `SgSem (_loc, t, (sgSem_of_list ts))
let rec biAnd_of_list =
  function
  | [] -> `BiNil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_binding b in `BiAnd (_loc, b, (biAnd_of_list bs))
let rec rbSem_of_list =
  function
  | [] -> `RbNil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_rec_binding b in `RbSem (_loc, b, (rbSem_of_list bs))
let rec wcAnd_of_list =
  function
  | [] -> `WcNil ghost
  | w::[] -> w
  | w::ws ->
      let _loc = loc_of_with_constr w in `WcAnd (_loc, w, (wcAnd_of_list ws))
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
  | [] -> `McNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_match_case x in `McOr (_loc, x, (mcOr_of_list xs))
let rec mbAnd_of_list =
  function
  | [] -> `MbNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_binding x in
      `MbAnd (_loc, x, (mbAnd_of_list xs))
let rec meApp_of_list =
  function
  | [] -> assert false
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_expr x in `MeApp (_loc, x, (meApp_of_list xs))
let rec ceAnd_of_list =
  function
  | [] -> `CeNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_expr x in `CeAnd (_loc, x, (ceAnd_of_list xs))
let rec ctAnd_of_list =
  function
  | [] -> `CtNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_type x in `CtAnd (_loc, x, (ctAnd_of_list xs))
let rec cgSem_of_list =
  function
  | [] -> `CgNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_sig_item x in
      `CgSem (_loc, x, (cgSem_of_list xs))
let rec crSem_of_list =
  function
  | [] -> `CrNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_str_item x in
      `CrSem (_loc, x, (crSem_of_list xs))
let rec paSem_of_list =
  function
  | [] -> `PaNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in `PaSem (_loc, x, (paSem_of_list xs))
let rec paCom_of_list =
  function
  | [] -> `PaNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in `PaCom (_loc, x, (paCom_of_list xs))
let rec exSem_of_list =
  function
  | [] -> `ExNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in `ExSem (_loc, x, (exSem_of_list xs))
let rec exCom_of_list =
  function
  | [] -> `ExNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in `ExCom (_loc, x, (exCom_of_list xs))
let exApp_of_list =
  function
  | [] -> `ExNil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_expr x in `ExApp (_loc, x, y)) t ts
let ty_of_stl =
  function
  | (_loc,s,[]) -> `TyId (_loc, (`Uid (_loc, s)))
  | (_loc,s,tl) ->
      `TyOf (_loc, (`TyId (_loc, (`Uid (_loc, s)))), (tyAnd_of_list tl))
let ty_of_sbt =
  function
  | (_loc,s,true ,t) ->
      `TyCol (_loc, (`TyId (_loc, (`Lid (_loc, s)))), (`TyMut (_loc, t)))
  | (_loc,s,false ,t) -> `TyCol (_loc, (`TyId (_loc, (`Lid (_loc, s)))), t)
let bi_of_pe (p,e) = let _loc = loc_of_patt p in `BiEq (_loc, p, e)
let sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l)
let record_type_of_list l = tySem_of_list (List.map ty_of_sbt l)
let binding_of_pel l = biAnd_of_list (List.map bi_of_pe l)
let rec pel_of_binding =
  function
  | `BiAnd (_loc,b1,b2) -> (pel_of_binding b1) @ (pel_of_binding b2)
  | `BiEq (_loc,p,e) -> [(p, e)]
  | _ -> assert false
let rec list_of_binding x acc =
  match x with
  | `BiAnd (_loc,b1,b2) -> list_of_binding b1 (list_of_binding b2 acc)
  | t -> t :: acc
let rec list_of_rec_binding x acc =
  match x with
  | `RbSem (_loc,b1,b2) ->
      list_of_rec_binding b1 (list_of_rec_binding b2 acc)
  | t -> t :: acc
let rec list_of_with_constr x acc =
  match x with
  | `WcAnd (_loc,w1,w2) ->
      list_of_with_constr w1 (list_of_with_constr w2 acc)
  | t -> t :: acc
let rec list_of_ctyp x acc =
  match x with
  | `TyNil _loc -> acc
  | `TyAmp (_loc,x,y)|`TyCom (_loc,x,y)|`TySta (_loc,x,y)|`TySem (_loc,x,y)|
      `TyAnd (_loc,x,y)|`TyOr (_loc,x,y) ->
      list_of_ctyp x (list_of_ctyp y acc)
  | x -> x :: acc
let rec list_of_patt x acc =
  match x with
  | `PaNil _loc -> acc
  | `PaCom (_loc,x,y)|`PaSem (_loc,x,y) ->
      list_of_patt x (list_of_patt y acc)
  | x -> x :: acc
let rec list_of_expr x acc =
  match x with
  | `ExNil _loc -> acc
  | `ExCom (_loc,x,y)|`ExSem (_loc,x,y) ->
      list_of_expr x (list_of_expr y acc)
  | x -> x :: acc
let rec list_of_str_item x acc =
  match x with
  | `StNil _loc -> acc
  | `StSem (_loc,x,y) -> list_of_str_item x (list_of_str_item y acc)
  | x -> x :: acc
let rec list_of_sig_item x acc =
  match x with
  | `SgNil _loc -> acc
  | `SgSem (_loc,x,y) -> list_of_sig_item x (list_of_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_sig_item x acc =
  match x with
  | `CgNil _loc -> acc
  | `CgSem (_loc,x,y) ->
      list_of_class_sig_item x (list_of_class_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_str_item x acc =
  match x with
  | `CrNil _loc -> acc
  | `CrSem (_loc,x,y) ->
      list_of_class_str_item x (list_of_class_str_item y acc)
  | x -> x :: acc
let rec list_of_class_type x acc =
  match x with
  | `CtAnd (_loc,x,y) -> list_of_class_type x (list_of_class_type y acc)
  | x -> x :: acc
let rec list_of_class_expr x acc =
  match x with
  | `CeAnd (_loc,x,y) -> list_of_class_expr x (list_of_class_expr y acc)
  | x -> x :: acc
let rec list_of_module_expr x acc =
  match x with
  | `MeApp (_loc,x,y) -> list_of_module_expr x (list_of_module_expr y acc)
  | x -> x :: acc
let rec list_of_match_case x acc =
  match x with
  | `McNil _loc -> acc
  | `McOr (_loc,x,y) -> list_of_match_case x (list_of_match_case y acc)
  | x -> x :: acc
let rec list_of_ident x acc =
  match x with
  | `IdAcc (_loc,x,y)|`IdApp (_loc,x,y) ->
      list_of_ident x (list_of_ident y acc)
  | x -> x :: acc
let rec list_of_module_binding x acc =
  match x with
  | `MbAnd (_loc,x,y) ->
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
      | `WcAnd (_loc,`WcNil _l,wc)|`WcAnd (_loc,wc,`WcNil _l) -> wc
      | wc -> wc
    method! expr e =
      match super#expr e with
      | `ExLet (_loc,_,`BiNil _l,e)|`ExRec (_loc,`RbNil _l,e)|`ExCom
                                                                (_loc,
                                                                 `ExNil _l,e)|
          `ExCom (_loc,e,`ExNil _l)|`ExSem (_loc,`ExNil _l,e)|`ExSem
                                                                (_loc,e,
                                                                 `ExNil _l)
          -> e
      | e -> e
    method! patt p =
      match super#patt p with
      | `PaAli (_loc,p,`PaNil _l)|`PaOrp (_loc,`PaNil _l,p)|`PaOrp
                                                              (_loc,p,
                                                               `PaNil _l)|
          `PaCom (_loc,`PaNil _l,p)|`PaCom (_loc,p,`PaNil _l)|`PaSem
                                                                (_loc,
                                                                 `PaNil _l,p)|
          `PaSem (_loc,p,`PaNil _l) -> p
      | p -> p
    method! match_case mc =
      match super#match_case mc with
      | `McOr (_loc,`McNil _l,mc)|`McOr (_loc,mc,`McNil _l) -> mc
      | mc -> mc
    method! binding bi =
      match super#binding bi with
      | `BiAnd (_loc,`BiNil _l,bi)|`BiAnd (_loc,bi,`BiNil _l) -> bi
      | bi -> bi
    method! rec_binding rb =
      match super#rec_binding rb with
      | `RbSem (_loc,`RbNil _l,bi)|`RbSem (_loc,bi,`RbNil _l) -> bi
      | bi -> bi
    method! module_binding mb =
      match super#module_binding mb with
      | `MbAnd (_loc,`MbNil _l,mb)|`MbAnd (_loc,mb,`MbNil _l) -> mb
      | mb -> mb
    method! ctyp t =
      match super#ctyp t with
      | `TyPol (_loc,`TyNil _l,t)|`TyAli (_loc,`TyNil _l,t)|`TyAli
                                                              (_loc,t,
                                                               `TyNil _l)|
          `TyArr (_loc,t,`TyNil _l)|`TyArr (_loc,`TyNil _l,t)|`TyOr
                                                                (_loc,
                                                                 `TyNil _l,t)|
          `TyOr (_loc,t,`TyNil _l)|`TyOf (_loc,t,`TyNil _l)|`TyAnd
                                                              (_loc,`TyNil _l,t)|
          `TyAnd (_loc,t,`TyNil _l)|`TySem (_loc,t,`TyNil _l)|`TySem
                                                                (_loc,
                                                                 `TyNil _l,t)|
          `TyCom (_loc,`TyNil _l,t)|`TyCom (_loc,t,`TyNil _l)|`TyAmp
                                                                (_loc,t,
                                                                 `TyNil _l)|
          `TyAmp (_loc,`TyNil _l,t)|`TySta (_loc,`TyNil _l,t)|`TySta
                                                                (_loc,t,
                                                                 `TyNil _l)
          -> t
      | t -> t
    method! sig_item sg =
      match super#sig_item sg with
      | `SgSem (_loc,`SgNil _l,sg)|`SgSem (_loc,sg,`SgNil _l) -> sg
      | `SgTyp (_loc,`TyNil _l) -> `SgNil _loc
      | sg -> sg
    method! str_item st =
      match super#str_item st with
      | `StSem (_loc,`StNil _l,st)|`StSem (_loc,st,`StNil _l) -> st
      | `StTyp (_loc,`TyNil _l) -> `StNil _loc
      | `StVal (_loc,_,`BiNil _l) -> `StNil _loc
      | st -> st
    method! module_type mt =
      match super#module_type mt with
      | `MtWit (_loc,mt,`WcNil _l) -> mt
      | mt -> mt
    method! class_expr ce =
      match super#class_expr ce with
      | `CeAnd (_loc,`CeNil _l,ce)|`CeAnd (_loc,ce,`CeNil _l) -> ce
      | ce -> ce
    method! class_type ct =
      match super#class_type ct with
      | `CtAnd (_loc,`CtNil _l,ct)|`CtAnd (_loc,ct,`CtNil _l) -> ct
      | ct -> ct
    method! class_sig_item csg =
      match super#class_sig_item csg with
      | `CgSem (_loc,`CgNil _l,csg)|`CgSem (_loc,csg,`CgNil _l) -> csg
      | csg -> csg
    method! class_str_item cst =
      match super#class_str_item cst with
      | `CrSem (_loc,`CrNil _l,cst)|`CrSem (_loc,cst,`CrNil _l) -> cst
      | cst -> cst
  end
class reloc _loc = object  inherit  map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  map as super
    method! patt =
      function
      | `PaId (_loc,`Lid (_,_)) -> `PaAny _loc
      | `PaAli (_loc,p,_) -> self#patt p
      | p -> super#patt p
  end
let match_pre =
  object (self)
    inherit  map
    method! match_case =
      function
      | `McArr (_loc,p,`ExNil _,e) ->
          `McArr
            (_loc, p, (`ExNil _loc),
              (`ExFun
                 (_loc,
                   (`McArr
                      (_loc, (`PaId (_loc, (`Uid (_loc, "()")))),
                        (`ExNil _loc), e)))))
      | `McArr (_loc,p,e,e1) ->
          `McArr
            (_loc, p, e,
              (`ExFun
                 (_loc,
                   (`McArr
                      (_loc, (`PaId (_loc, (`Uid (_loc, "()")))),
                        (`ExNil _loc), e1)))))
      | `McOr (_loc,a1,a2) ->
          `McOr (_loc, (self#match_case a1), (self#match_case a2))
      | `McNil _loc -> `McNil _loc
      | `Ant (_loc,x) -> `Ant (_loc, (add_context x "lettry"))
  end
