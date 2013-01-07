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
      | `Nil a0 -> `Nil (self#loc a0)
      | `CrSem a0 ->
          `CrSem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_str_item a1),
                   (self#class_str_item a2)))) a0)
      | `Eq a0 ->
          `Eq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `Inherit a0 ->
          `Inherit
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#override_flag a1),
                   (self#class_expr a2), (self#string a3)))) a0)
      | `Initializer a0 ->
          `Initializer
            (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
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
      | `Nil a0 -> `Nil (self#loc a0)
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
      | `Obj a0 ->
          `Obj
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#class_str_item a2))))
               a0)
      | `CeTyc a0 ->
          `CeTyc
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_type a2))))
               a0)
      | `And a0 ->
          `And
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_expr a2))))
               a0)
      | `Eq a0 ->
          `Eq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_expr a1), (self#class_expr a2))))
               a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Eq a0 ->
          `Eq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#class_sig_item a1),
                   (self#class_sig_item a2)))) a0)
      | `Inherit a0 ->
          `Inherit
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `Method a0 ->
          `Method
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
      | `Nil a0 -> `Nil (self#loc a0)
      | `Class a0 ->
          `Class
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_expr a1)))) a0)
      | `ClassType a0 ->
          `ClassType
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#str_item a1), (self#str_item a2)))) a0)
      | `Directive a0 ->
          `Directive
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `Exception a0 ->
          `Exception
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1),
                   (self#meta_option (fun self  -> self#ident) a2)))) a0)
      | `StExp a0 ->
          `StExp (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `External a0 ->
          `External
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2),
                   (self#meta_list (fun self  -> self#string) a3)))) a0)
      | `Include a0 ->
          `Include
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
      | `Module a0 ->
          `Module
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_expr a2))))
               a0)
      | `RecModule a0 ->
          `RecModule
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_binding a1)))) a0)
      | `ModuleType a0 ->
          `ModuleType
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `Open a0 ->
          `Open (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `Type a0 ->
          `Type (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Value a0 ->
          `Value
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_flag a1), (self#binding a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_expr : module_expr -> module_expr=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Id a0 ->
          `Id (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `MeApp a0 ->
          `MeApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_expr a1),
                   (self#module_expr a2)))) a0)
      | `Functor a0 ->
          `Functor
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_expr a3)))) a0)
      | `Struct a0 ->
          `Struct
            (((fun (a0,a1)  -> ((self#loc a0), (self#str_item a1)))) a0)
      | `ModuleExprConstraint a0 ->
          `ModuleExprConstraint
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_expr a1),
                   (self#module_type a2)))) a0)
      | `PackageModule a0 ->
          `PackageModule
            (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method match_case : match_case -> match_case=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `McOr a0 ->
          `McOr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#match_case a1), (self#match_case a2))))
               a0)
      | `Case a0 ->
          `Case
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#patt a1), (self#expr a2),
                   (self#expr a3)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_binding : module_binding -> module_binding=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `And a0 ->
          `And
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_binding a1),
                   (self#module_binding a2)))) a0)
      | `ModuleBind a0 ->
          `ModuleBind
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_expr a3)))) a0)
      | `ModuleConstraint a0 ->
          `ModuleConstraint
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method rec_binding : rec_binding -> rec_binding=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_binding a1),
                   (self#rec_binding a2)))) a0)
      | `RecBind a0 ->
          `RecBind
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#expr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method binding : binding -> binding=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `And a0 ->
          `And
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#binding a1), (self#binding a2)))) a0)
      | `Bind a0 ->
          `Bind
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#expr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method with_constr : with_constr -> with_constr=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `TypeEq a0 ->
          `TypeEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `ModuleEq a0 ->
          `ModuleEq
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `TypeSubst a0 ->
          `TypeSubst
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `ModuleSubst a0 ->
          `ModuleSubst
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#ident a2)))) a0)
      | `And a0 ->
          `And
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#with_constr a1),
                   (self#with_constr a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method sig_item : sig_item -> sig_item=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Class a0 ->
          `Class
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `ClassType a0 ->
          `ClassType
            (((fun (a0,a1)  -> ((self#loc a0), (self#class_type a1)))) a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#sig_item a1), (self#sig_item a2)))) a0)
      | `Directive a0 ->
          `Directive
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `Exception a0 ->
          `Exception (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `External a0 ->
          `External
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2),
                   (self#meta_list (fun self  -> self#string) a3)))) a0)
      | `Include a0 ->
          `Include
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_type a1)))) a0)
      | `Module a0 ->
          `Module
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `RecModule a0 ->
          `RecModule
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_binding a1)))) a0)
      | `ModuleType a0 ->
          `ModuleType
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2))))
               a0)
      | `Open a0 ->
          `Open (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `Type a0 ->
          `Type (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Value a0 ->
          `Value
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#ctyp a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method module_type : module_type -> module_type=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Id a0 ->
          `Id (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `MtFun a0 ->
          `MtFun
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_type a2),
                   (self#module_type a3)))) a0)
      | `MtQuo a0 ->
          `MtQuo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Sig a0 ->
          `Sig (((fun (a0,a1)  -> ((self#loc a0), (self#sig_item a1)))) a0)
      | `MtWit a0 ->
          `MtWit
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#module_type a1),
                   (self#with_constr a2)))) a0)
      | `Of a0 ->
          `Of (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method expr : expr -> expr=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Id a0 ->
          `Id (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
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
      | `Array a0 ->
          `Array (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExAsf a0 -> `ExAsf (self#loc a0)
      | `ExAsr a0 ->
          `ExAsr (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExAss a0 ->
          `ExAss
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `ExCoe a0 ->
          `ExCoe
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#expr a1), (self#ctyp a2),
                   (self#ctyp a3)))) a0)
      | `Flo a0 ->
          `Flo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Chr a0 ->
          `Chr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `For a0 ->
          `For
            (((fun (a0,a1,a2,a3,a4,a5)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2),
                   (self#expr a3), (self#direction_flag a4), (self#expr a5))))
               a0)
      | `Fun a0 ->
          `Fun (((fun (a0,a1)  -> ((self#loc a0), (self#match_case a1)))) a0)
      | `ExIfe a0 ->
          `ExIfe
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2),
                   (self#expr a3)))) a0)
      | `Int a0 ->
          `Int (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Int32 a0 ->
          `Int32 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Int64 a0 ->
          `Int64 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `NativeInt a0 ->
          `NativeInt
            (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Label a0 ->
          `Label
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `Lazy a0 ->
          `Lazy (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `LetIn a0 ->
          `LetIn
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#rec_flag a1), (self#binding a2),
                   (self#expr a3)))) a0)
      | `LetModule a0 ->
          `LetModule
            (((fun (a0,a1,a2,a3)  ->
                 ((self#loc a0), (self#string a1), (self#module_expr a2),
                   (self#expr a3)))) a0)
      | `Match a0 ->
          `Match
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#match_case a2)))) a0)
      | `New a0 ->
          `New (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `Obj a0 ->
          `Obj
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#class_str_item a2))))
               a0)
      | `OptLabl a0 ->
          `OptLabl
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `OvrInst a0 ->
          `OvrInst
            (((fun (a0,a1)  -> ((self#loc a0), (self#rec_binding a1)))) a0)
      | `Record a0 ->
          `Record
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#rec_binding a1), (self#expr a2)))) a0)
      | `Sequence a0 ->
          `Sequence (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `Send a0 ->
          `Send
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#string a2)))) a0)
      | `StringDot a0 ->
          `StringDot
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `Str a0 ->
          `Str (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Try a0 ->
          `Try
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#match_case a2)))) a0)
      | `ExTup a0 ->
          `ExTup (((fun (a0,a1)  -> ((self#loc a0), (self#expr a1)))) a0)
      | `ExCom a0 ->
          `ExCom
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `Constraint_exp a0 ->
          `Constraint_exp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#ctyp a2)))) a0)
      | `ExVrn a0 ->
          `ExVrn (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `While a0 ->
          `While
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#expr a1), (self#expr a2)))) a0)
      | `Let_open a0 ->
          `Let_open
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ident a1), (self#expr a2)))) a0)
      | `LocalTypeFun a0 ->
          `LocalTypeFun
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#string a1), (self#expr a2)))) a0)
      | `Package_expr a0 ->
          `Package_expr
            (((fun (a0,a1)  -> ((self#loc a0), (self#module_expr a1)))) a0)
    method patt : patt -> patt=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Id a0 ->
          `Id (((fun (a0,a1)  -> ((self#loc a0), (self#ident a1)))) a0)
      | `Alias a0 ->
          `Alias
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `Ant a0 ->
          `Ant (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Any a0 -> `Any (self#loc a0)
      | `PaApp a0 ->
          `PaApp
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `Array a0 ->
          `Array (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaCom a0 ->
          `PaCom
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `Sem a0 ->
          `Sem
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#patt a1), (self#patt a2)))) a0)
      | `Chr a0 ->
          `Chr (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Int a0 ->
          `Int (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Int32 a0 ->
          `Int32 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Int64 a0 ->
          `Int64 (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `NativeInt a0 ->
          `NativeInt
            (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
      | `Flo a0 ->
          `Flo (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
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
      | `Str a0 ->
          `Str (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
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
      | `Lazy a0 ->
          `Lazy (((fun (a0,a1)  -> ((self#loc a0), (self#patt a1)))) a0)
      | `PaMod a0 ->
          `PaMod (((fun (a0,a1)  -> ((self#loc a0), (self#string a1)))) a0)
    method ctyp : ctyp -> ctyp=
      function
      | `Nil a0 -> `Nil (self#loc a0)
      | `Alias a0 ->
          `Alias
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `Any a0 -> `Any (self#loc a0)
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
      | `Com a0 ->
          `Com
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `Sum a0 ->
          `Sum (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Of a0 ->
          `Of
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `And a0 ->
          `And
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `TyOr a0 ->
          `TyOr
            (((fun (a0,a1,a2)  ->
                 ((self#loc a0), (self#ctyp a1), (self#ctyp a2)))) a0)
      | `Private a0 ->
          `Private (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Mutable a0 ->
          `Mutable (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Tup a0 ->
          `Tup (((fun (a0,a1)  -> ((self#loc a0), (self#ctyp a1)))) a0)
      | `Sta a0 ->
          `Sta
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
      | `Package a0 ->
          `Package
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `CrSem a0 ->
            Format.fprintf fmt "@[<1>(`CrSem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_str_item a1 self#class_str_item a2) a0
        | `Eq a0 ->
            Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `Inherit a0 ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#override_flag a1 self#class_expr a2 self#string a3)
              a0
        | `Initializer a0 ->
            Format.fprintf fmt "@[<1>(`Initializer@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
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
        | `Obj a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#class_str_item a2) a0
        | `CeTyc a0 ->
            Format.fprintf fmt "@[<1>(`CeTyc@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#class_type a2) a0
        | `And a0 ->
            Format.fprintf fmt "@[<1>(`And@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_expr a1 self#class_expr a2) a0
        | `Eq a0 ->
            Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Eq a0 ->
            Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#class_sig_item a1 self#class_sig_item a2) a0
        | `Inherit a0 ->
            Format.fprintf fmt "@[<1>(`Inherit@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `Method a0 ->
            Format.fprintf fmt "@[<1>(`Method@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Class a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_expr a1) a0
        | `ClassType a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#str_item a1 self#str_item a2) a0
        | `Directive a0 ->
            Format.fprintf fmt "@[<1>(`Directive@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `Exception a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 (self#meta_option (fun self  -> self#ident))
                   a2) a0
        | `StExp a0 ->
            Format.fprintf fmt "@[<1>(`StExp@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `External a0 ->
            Format.fprintf fmt "@[<1>(`External@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2
                   (self#meta_list (fun self  -> self#string)) a3) a0
        | `Include a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_expr a1) a0
        | `Module a0 ->
            Format.fprintf fmt "@[<1>(`Module@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_expr a2) a0
        | `RecModule a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_binding a1) a0
        | `ModuleType a0 ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `Open a0 ->
            Format.fprintf fmt "@[<1>(`Open@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `Type a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Value a0 ->
            Format.fprintf fmt "@[<1>(`Value@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id a0 ->
            Format.fprintf fmt "@[<1>(`Id@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `MeApp a0 ->
            Format.fprintf fmt "@[<1>(`MeApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_expr a1 self#module_expr a2) a0
        | `Functor a0 ->
            Format.fprintf fmt "@[<1>(`Functor@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2 self#module_expr a3) a0
        | `Struct a0 ->
            Format.fprintf fmt "@[<1>(`Struct@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#str_item a1) a0
        | `ModuleExprConstraint a0 ->
            Format.fprintf fmt "@[<1>(`ModuleExprConstraint@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_expr a1 self#module_type a2) a0
        | `PackageModule a0 ->
            Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `McOr a0 ->
            Format.fprintf fmt "@[<1>(`McOr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#match_case a1 self#match_case a2) a0
        | `Case a0 ->
            Format.fprintf fmt "@[<1>(`Case@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `And a0 ->
            Format.fprintf fmt "@[<1>(`And@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_binding a1 self#module_binding a2) a0
        | `ModuleBind a0 ->
            Format.fprintf fmt "@[<1>(`ModuleBind@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2 self#module_expr a3) a0
        | `ModuleConstraint a0 ->
            Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_binding a1 self#rec_binding a2) a0
        | `RecBind a0 ->
            Format.fprintf fmt "@[<1>(`RecBind@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `And a0 ->
            Format.fprintf fmt "@[<1>(`And@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#binding a1 self#binding a2) a0
        | `Bind a0 ->
            Format.fprintf fmt "@[<1>(`Bind@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `TypeEq a0 ->
            Format.fprintf fmt "@[<1>(`TypeEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `ModuleEq a0 ->
            Format.fprintf fmt "@[<1>(`ModuleEq@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `TypeSubst a0 ->
            Format.fprintf fmt "@[<1>(`TypeSubst@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `ModuleSubst a0 ->
            Format.fprintf fmt "@[<1>(`ModuleSubst@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#ident a2) a0
        | `And a0 ->
            Format.fprintf fmt "@[<1>(`And@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Class a0 ->
            Format.fprintf fmt "@[<1>(`Class@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `ClassType a0 ->
            Format.fprintf fmt "@[<1>(`ClassType@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#class_type a1) a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#sig_item a1 self#sig_item a2) a0
        | `Directive a0 ->
            Format.fprintf fmt "@[<1>(`Directive@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `Exception a0 ->
            Format.fprintf fmt "@[<1>(`Exception@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `External a0 ->
            Format.fprintf fmt "@[<1>(`External@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#ctyp a2
                   (self#meta_list (fun self  -> self#string)) a3) a0
        | `Include a0 ->
            Format.fprintf fmt "@[<1>(`Include@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_type a1) a0
        | `Module a0 ->
            Format.fprintf fmt "@[<1>(`Module@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `RecModule a0 ->
            Format.fprintf fmt "@[<1>(`RecModule@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_binding a1) a0
        | `ModuleType a0 ->
            Format.fprintf fmt "@[<1>(`ModuleType@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_type a2) a0
        | `Open a0 ->
            Format.fprintf fmt "@[<1>(`Open@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `Type a0 ->
            Format.fprintf fmt "@[<1>(`Type@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Value a0 ->
            Format.fprintf fmt "@[<1>(`Value@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id a0 ->
            Format.fprintf fmt "@[<1>(`Id@ %a)@]"
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
        | `Sig a0 ->
            Format.fprintf fmt "@[<1>(`Sig@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#sig_item a1) a0
        | `MtWit a0 ->
            Format.fprintf fmt "@[<1>(`MtWit@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#module_type a1 self#with_constr a2) a0
        | `Of a0 ->
            Format.fprintf fmt "@[<1>(`Of@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id a0 ->
            Format.fprintf fmt "@[<1>(`Id@ %a)@]"
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
        | `Array a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
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
        | `ExCoe a0 ->
            Format.fprintf fmt "@[<1>(`ExCoe@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#ctyp a2 self#ctyp a3) a0
        | `Flo a0 ->
            Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Chr a0 ->
            Format.fprintf fmt "@[<1>(`Chr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `For a0 ->
            Format.fprintf fmt "@[<1>(`For@ %a)@]"
              (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
                   self#loc a0 self#string a1 self#expr a2 self#expr a3
                   self#direction_flag a4 self#expr a5) a0
        | `Fun a0 ->
            Format.fprintf fmt "@[<1>(`Fun@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#match_case a1) a0
        | `ExIfe a0 ->
            Format.fprintf fmt "@[<1>(`ExIfe@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2 self#expr a3) a0
        | `Int a0 ->
            Format.fprintf fmt "@[<1>(`Int@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Int32 a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Int64 a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `NativeInt a0 ->
            Format.fprintf fmt "@[<1>(`NativeInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Label a0 ->
            Format.fprintf fmt "@[<1>(`Label@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `Lazy a0 ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `LetIn a0 ->
            Format.fprintf fmt "@[<1>(`LetIn@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_flag a1 self#binding a2 self#expr a3) a0
        | `LetModule a0 ->
            Format.fprintf fmt "@[<1>(`LetModule@ %a)@]"
              (fun fmt  (a0,a1,a2,a3)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#module_expr a2 self#expr a3) a0
        | `Match a0 ->
            Format.fprintf fmt "@[<1>(`Match@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#match_case a2) a0
        | `New a0 ->
            Format.fprintf fmt "@[<1>(`New@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `Obj a0 ->
            Format.fprintf fmt "@[<1>(`Obj@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#class_str_item a2) a0
        | `OptLabl a0 ->
            Format.fprintf fmt "@[<1>(`OptLabl@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `OvrInst a0 ->
            Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#rec_binding a1) a0
        | `Record a0 ->
            Format.fprintf fmt "@[<1>(`Record@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#rec_binding a1 self#expr a2) a0
        | `Sequence a0 ->
            Format.fprintf fmt "@[<1>(`Sequence@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#expr
                   a1) a0
        | `Send a0 ->
            Format.fprintf fmt "@[<1>(`Send@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#string a2) a0
        | `StringDot a0 ->
            Format.fprintf fmt "@[<1>(`StringDot@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `Str a0 ->
            Format.fprintf fmt "@[<1>(`Str@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Try a0 ->
            Format.fprintf fmt "@[<1>(`Try@ %a)@]"
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
        | `Constraint_exp a0 ->
            Format.fprintf fmt "@[<1>(`Constraint_exp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#ctyp a2) a0
        | `ExVrn a0 ->
            Format.fprintf fmt "@[<1>(`ExVrn@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `While a0 ->
            Format.fprintf fmt "@[<1>(`While@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#expr a1 self#expr a2) a0
        | `Let_open a0 ->
            Format.fprintf fmt "@[<1>(`Let_open@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ident a1 self#expr a2) a0
        | `LocalTypeFun a0 ->
            Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#string a1 self#expr a2) a0
        | `Package_expr a0 ->
            Format.fprintf fmt "@[<1>(`Package_expr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#module_expr a1) a0
    method patt : 'fmt -> patt -> 'result=
      fun fmt  ->
        function
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Id a0 ->
            Format.fprintf fmt "@[<1>(`Id@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ident
                   a1) a0
        | `Alias a0 ->
            Format.fprintf fmt "@[<1>(`Alias@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `Ant a0 ->
            Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc a0
        | `PaApp a0 ->
            Format.fprintf fmt "@[<1>(`PaApp@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `Array a0 ->
            Format.fprintf fmt "@[<1>(`Array@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#patt
                   a1) a0
        | `PaCom a0 ->
            Format.fprintf fmt "@[<1>(`PaCom@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `Sem a0 ->
            Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#patt a1 self#patt a2) a0
        | `Chr a0 ->
            Format.fprintf fmt "@[<1>(`Chr@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Int a0 ->
            Format.fprintf fmt "@[<1>(`Int@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Int32 a0 ->
            Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Int64 a0 ->
            Format.fprintf fmt "@[<1>(`Int64@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `NativeInt a0 ->
            Format.fprintf fmt "@[<1>(`NativeInt@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0
                   self#string a1) a0
        | `Flo a0 ->
            Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
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
        | `Str a0 ->
            Format.fprintf fmt "@[<1>(`Str@ %a)@]"
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
        | `Lazy a0 ->
            Format.fprintf fmt "@[<1>(`Lazy@ %a)@]"
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
        | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" self#loc a0
        | `Alias a0 ->
            Format.fprintf fmt "@[<1>(`Alias@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" self#loc a0
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
        | `Com a0 ->
            Format.fprintf fmt "@[<1>(`Com@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `Sum a0 ->
            Format.fprintf fmt "@[<1>(`Sum@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Of a0 ->
            Format.fprintf fmt "@[<1>(`Of@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `And a0 ->
            Format.fprintf fmt "@[<1>(`And@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `TyOr a0 ->
            Format.fprintf fmt "@[<1>(`TyOr@ %a)@]"
              (fun fmt  (a0,a1,a2)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" self#loc a0
                   self#ctyp a1 self#ctyp a2) a0
        | `Private a0 ->
            Format.fprintf fmt "@[<1>(`Private@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Mutable a0 ->
            Format.fprintf fmt "@[<1>(`Mutable@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Tup a0 ->
            Format.fprintf fmt "@[<1>(`Tup@ %a)@]"
              (fun fmt  (a0,a1)  ->
                 Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#loc a0 self#ctyp
                   a1) a0
        | `Sta a0 ->
            Format.fprintf fmt "@[<1>(`Sta@ %a)@]"
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
        | `Package a0 ->
            Format.fprintf fmt "@[<1>(`Package@ %a)@]"
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
      | `Nil a0 -> self#loc a0
      | `CrSem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_str_item a1 in self#class_str_item a2))
            a0
      | `Eq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `Inherit a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#override_flag a1 in
              let self = self#class_expr a2 in self#string a3)) a0
      | `Initializer a0 ->
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
      | `Nil a0 -> self#loc a0
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
      | `Obj a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#class_str_item a2)) a0
      | `CeTyc a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_type a2)) a0
      | `And a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_expr a2)) a0
      | `Eq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_expr a1 in self#class_expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Eq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `Sem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#class_sig_item a1 in self#class_sig_item a2))
            a0
      | `Inherit a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `Method a0 ->
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
      | `Nil a0 -> self#loc a0
      | `Class a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_expr a1)) a0
      | `ClassType a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `Sem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#str_item a1 in self#str_item a2)) a0
      | `Directive a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `Exception a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in
              self#meta_option (fun self  -> self#ident) a2)) a0
      | `StExp a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `External a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#ctyp a2 in
              self#meta_list (fun self  -> self#string) a3)) a0
      | `Include a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
      | `Module a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_expr a2)) a0
      | `RecModule a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_binding a1))
            a0
      | `ModuleType a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `Open a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `Type a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Value a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_flag a1 in self#binding a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_expr : module_expr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `MeApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_expr a1 in self#module_expr a2)) a0
      | `Functor a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_expr a3)) a0
      | `Struct a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#str_item a1)) a0
      | `ModuleExprConstraint a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_expr a1 in self#module_type a2)) a0
      | `PackageModule a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method match_case : match_case -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `McOr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#match_case a1 in self#match_case a2)) a0
      | `Case a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in
              let self = self#expr a2 in self#expr a3)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_binding : module_binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `And a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_binding a1 in self#module_binding a2))
            a0
      | `ModuleBind a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_expr a3)) a0
      | `ModuleConstraint a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method rec_binding : rec_binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Sem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_binding a1 in self#rec_binding a2)) a0
      | `RecBind a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method binding : binding -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `And a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#binding a1 in self#binding a2)) a0
      | `Bind a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#expr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method with_constr : with_constr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `TypeEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `ModuleEq a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `TypeSubst a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `ModuleSubst a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#ident a2)) a0
      | `And a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#with_constr a1 in self#with_constr a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method sig_item : sig_item -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Class a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `ClassType a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#class_type a1)) a0
      | `Sem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#sig_item a1 in self#sig_item a2)) a0
      | `Directive a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `Exception a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `External a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#ctyp a2 in
              self#meta_list (fun self  -> self#string) a3)) a0
      | `Include a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_type a1))
            a0
      | `Module a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `RecModule a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_binding a1))
            a0
      | `ModuleType a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#module_type a2)) a0
      | `Open a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `Type a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Value a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#ctyp a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method module_type : module_type -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `MtFun a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_type a2 in self#module_type a3)) a0
      | `MtQuo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Sig a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#sig_item a1)) a0
      | `MtWit a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#module_type a1 in self#with_constr a2)) a0
      | `Of a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method expr : expr -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id a0 ->
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
      | `Array a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `Sem a0 ->
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
      | `ExCoe a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in
              let self = self#ctyp a2 in self#ctyp a3)) a0
      | `Flo a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Chr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `For a0 ->
          ((fun (a0,a1,a2,a3,a4,a5)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#expr a2 in
              let self = self#expr a3 in
              let self = self#direction_flag a4 in self#expr a5)) a0
      | `Fun a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#match_case a1)) a0
      | `ExIfe a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in
              let self = self#expr a2 in self#expr a3)) a0
      | `Int a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Int32 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Int64 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `NativeInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Label a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `Lazy a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `LetIn a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#rec_flag a1 in
              let self = self#binding a2 in self#expr a3)) a0
      | `LetModule a0 ->
          ((fun (a0,a1,a2,a3)  ->
              let self = self#loc a0 in
              let self = self#string a1 in
              let self = self#module_expr a2 in self#expr a3)) a0
      | `Match a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#match_case a2)) a0
      | `New a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `Obj a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#class_str_item a2)) a0
      | `OptLabl a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `OvrInst a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#rec_binding a1))
            a0
      | `Record a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#rec_binding a1 in self#expr a2)) a0
      | `Sequence a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `Send a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#string a2)) a0
      | `StringDot a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `Str a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Try a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#match_case a2)) a0
      | `ExTup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#expr a1)) a0
      | `ExCom a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `Constraint_exp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#ctyp a2)) a0
      | `ExVrn a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `While a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#expr a1 in self#expr a2)) a0
      | `Let_open a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ident a1 in self#expr a2)) a0
      | `LocalTypeFun a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#string a1 in self#expr a2)) a0
      | `Package_expr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#module_expr a1))
            a0
    method patt : patt -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Id a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ident a1)) a0
      | `Alias a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `Ant a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Any a0 -> self#loc a0
      | `PaApp a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `Array a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaCom a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `Sem a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#patt a1 in self#patt a2)) a0
      | `Chr a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Int a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Int32 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Int64 a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `NativeInt a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
      | `Flo a0 ->
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
      | `Str a0 ->
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
      | `Lazy a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#patt a1)) a0
      | `PaMod a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#string a1)) a0
    method ctyp : ctyp -> 'self_type=
      function
      | `Nil a0 -> self#loc a0
      | `Alias a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `Any a0 -> self#loc a0
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
      | `Com a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `Sum a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Of a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `And a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `TyOr a0 ->
          ((fun (a0,a1,a2)  ->
              let self = self#loc a0 in
              let self = self#ctyp a1 in self#ctyp a2)) a0
      | `Private a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Mutable a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Tup a0 ->
          ((fun (a0,a1)  -> let self = self#loc a0 in self#ctyp a1)) a0
      | `Sta a0 ->
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
      | `Package a0 ->
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `CrSem a0 ->
        Format.fprintf fmt "@[<1>(`CrSem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_str_item a1 pp_print_class_str_item a2) a0
    | `Eq a0 ->
        Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `Inherit a0 ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_override_flag a1 pp_print_class_expr a2
               pp_print_string a3) a0
    | `Initializer a0 ->
        Format.fprintf fmt "@[<1>(`Initializer@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
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
    | `Obj a0 ->
        Format.fprintf fmt "@[<1>(`Obj@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_class_str_item a2) a0
    | `CeTyc a0 ->
        Format.fprintf fmt "@[<1>(`CeTyc@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_class_type a2) a0
    | `And a0 ->
        Format.fprintf fmt "@[<1>(`And@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1 pp_print_class_expr a2) a0
    | `Eq a0 ->
        Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Eq a0 ->
        Format.fprintf fmt "@[<1>(`Eq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_class_sig_item a1 pp_print_class_sig_item a2) a0
    | `Inherit a0 ->
        Format.fprintf fmt "@[<1>(`Inherit@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `Method a0 ->
        Format.fprintf fmt "@[<1>(`Method@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Class a0 ->
        Format.fprintf fmt "@[<1>(`Class@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_expr a1) a0
    | `ClassType a0 ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_str_item a1 pp_print_str_item a2) a0
    | `Directive a0 ->
        Format.fprintf fmt "@[<1>(`Directive@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `Exception a0 ->
        Format.fprintf fmt "@[<1>(`Exception@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 (pp_print_meta_option pp_print_ident) a2) a0
    | `StExp a0 ->
        Format.fprintf fmt "@[<1>(`StExp@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `External a0 ->
        Format.fprintf fmt "@[<1>(`External@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2
               (pp_print_meta_list pp_print_string) a3) a0
    | `Include a0 ->
        Format.fprintf fmt "@[<1>(`Include@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1) a0
    | `Module a0 ->
        Format.fprintf fmt "@[<1>(`Module@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_expr a2) a0
    | `RecModule a0 ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1) a0
    | `ModuleType a0 ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `Open a0 ->
        Format.fprintf fmt "@[<1>(`Open@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `Type a0 ->
        Format.fprintf fmt "@[<1>(`Type@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Value a0 ->
        Format.fprintf fmt "@[<1>(`Value@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id a0 ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `MeApp a0 ->
        Format.fprintf fmt "@[<1>(`MeApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1 pp_print_module_expr a2) a0
    | `Functor a0 ->
        Format.fprintf fmt "@[<1>(`Functor@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2
               pp_print_module_expr a3) a0
    | `Struct a0 ->
        Format.fprintf fmt "@[<1>(`Struct@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_str_item a1) a0
    | `ModuleExprConstraint a0 ->
        Format.fprintf fmt "@[<1>(`ModuleExprConstraint@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1 pp_print_module_type a2) a0
    | `PackageModule a0 ->
        Format.fprintf fmt "@[<1>(`PackageModule@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `McOr a0 ->
        Format.fprintf fmt "@[<1>(`McOr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_match_case a1 pp_print_match_case a2) a0
    | `Case a0 ->
        Format.fprintf fmt "@[<1>(`Case@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `And a0 ->
        Format.fprintf fmt "@[<1>(`And@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1 pp_print_module_binding a2) a0
    | `ModuleBind a0 ->
        Format.fprintf fmt "@[<1>(`ModuleBind@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2
               pp_print_module_expr a3) a0
    | `ModuleConstraint a0 ->
        Format.fprintf fmt "@[<1>(`ModuleConstraint@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1 pp_print_rec_binding a2) a0
    | `RecBind a0 ->
        Format.fprintf fmt "@[<1>(`RecBind@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `And a0 ->
        Format.fprintf fmt "@[<1>(`And@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_binding a1 pp_print_binding a2) a0
    | `Bind a0 ->
        Format.fprintf fmt "@[<1>(`Bind@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `TypeEq a0 ->
        Format.fprintf fmt "@[<1>(`TypeEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `ModuleEq a0 ->
        Format.fprintf fmt "@[<1>(`ModuleEq@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `TypeSubst a0 ->
        Format.fprintf fmt "@[<1>(`TypeSubst@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `ModuleSubst a0 ->
        Format.fprintf fmt "@[<1>(`ModuleSubst@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_ident a2) a0
    | `And a0 ->
        Format.fprintf fmt "@[<1>(`And@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Class a0 ->
        Format.fprintf fmt "@[<1>(`Class@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `ClassType a0 ->
        Format.fprintf fmt "@[<1>(`ClassType@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_class_type a1) a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_sig_item a1 pp_print_sig_item a2) a0
    | `Directive a0 ->
        Format.fprintf fmt "@[<1>(`Directive@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `Exception a0 ->
        Format.fprintf fmt "@[<1>(`Exception@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `External a0 ->
        Format.fprintf fmt "@[<1>(`External@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_ctyp a2
               (pp_print_meta_list pp_print_string) a3) a0
    | `Include a0 ->
        Format.fprintf fmt "@[<1>(`Include@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_type a1) a0
    | `Module a0 ->
        Format.fprintf fmt "@[<1>(`Module@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `RecModule a0 ->
        Format.fprintf fmt "@[<1>(`RecModule@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_binding a1) a0
    | `ModuleType a0 ->
        Format.fprintf fmt "@[<1>(`ModuleType@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_type a2) a0
    | `Open a0 ->
        Format.fprintf fmt "@[<1>(`Open@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `Type a0 ->
        Format.fprintf fmt "@[<1>(`Type@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Value a0 ->
        Format.fprintf fmt "@[<1>(`Value@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id a0 ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]"
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
    | `Sig a0 ->
        Format.fprintf fmt "@[<1>(`Sig@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_sig_item a1) a0
    | `MtWit a0 ->
        Format.fprintf fmt "@[<1>(`MtWit@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_module_type a1 pp_print_with_constr a2) a0
    | `Of a0 ->
        Format.fprintf fmt "@[<1>(`Of@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id a0 ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]"
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
    | `Array a0 ->
        Format.fprintf fmt "@[<1>(`Array@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
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
    | `ExCoe a0 ->
        Format.fprintf fmt "@[<1>(`ExCoe@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_ctyp a2 pp_print_ctyp a3) a0
    | `Flo a0 ->
        Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Chr a0 ->
        Format.fprintf fmt "@[<1>(`Chr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `For a0 ->
        Format.fprintf fmt "@[<1>(`For@ %a)@]"
          (fun fmt  (a0,a1,a2,a3,a4,a5)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a,@,%a,@,%a)@]"
               pp_print_loc a0 pp_print_string a1 pp_print_expr a2
               pp_print_expr a3 pp_print_direction_flag a4 pp_print_expr a5)
          a0
    | `Fun a0 ->
        Format.fprintf fmt "@[<1>(`Fun@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_match_case a1) a0
    | `ExIfe a0 ->
        Format.fprintf fmt "@[<1>(`ExIfe@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2 pp_print_expr a3) a0
    | `Int a0 ->
        Format.fprintf fmt "@[<1>(`Int@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Int32 a0 ->
        Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Int64 a0 ->
        Format.fprintf fmt "@[<1>(`Int64@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `NativeInt a0 ->
        Format.fprintf fmt "@[<1>(`NativeInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Label a0 ->
        Format.fprintf fmt "@[<1>(`Label@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `Lazy a0 ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `LetIn a0 ->
        Format.fprintf fmt "@[<1>(`LetIn@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_flag a1 pp_print_binding a2 pp_print_expr a3) a0
    | `LetModule a0 ->
        Format.fprintf fmt "@[<1>(`LetModule@ %a)@]"
          (fun fmt  (a0,a1,a2,a3)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_module_expr a2 pp_print_expr a3)
          a0
    | `Match a0 ->
        Format.fprintf fmt "@[<1>(`Match@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_match_case a2) a0
    | `New a0 ->
        Format.fprintf fmt "@[<1>(`New@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `Obj a0 ->
        Format.fprintf fmt "@[<1>(`Obj@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_class_str_item a2) a0
    | `OptLabl a0 ->
        Format.fprintf fmt "@[<1>(`OptLabl@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `OvrInst a0 ->
        Format.fprintf fmt "@[<1>(`OvrInst@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1) a0
    | `Record a0 ->
        Format.fprintf fmt "@[<1>(`Record@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_rec_binding a1 pp_print_expr a2) a0
    | `Sequence a0 ->
        Format.fprintf fmt "@[<1>(`Sequence@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1) a0
    | `Send a0 ->
        Format.fprintf fmt "@[<1>(`Send@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_string a2) a0
    | `StringDot a0 ->
        Format.fprintf fmt "@[<1>(`StringDot@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `Str a0 ->
        Format.fprintf fmt "@[<1>(`Str@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Try a0 ->
        Format.fprintf fmt "@[<1>(`Try@ %a)@]"
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
    | `Constraint_exp a0 ->
        Format.fprintf fmt "@[<1>(`Constraint_exp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_ctyp a2) a0
    | `ExVrn a0 ->
        Format.fprintf fmt "@[<1>(`ExVrn@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `While a0 ->
        Format.fprintf fmt "@[<1>(`While@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_expr a1 pp_print_expr a2) a0
    | `Let_open a0 ->
        Format.fprintf fmt "@[<1>(`Let_open@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1 pp_print_expr a2) a0
    | `LocalTypeFun a0 ->
        Format.fprintf fmt "@[<1>(`LocalTypeFun@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1 pp_print_expr a2) a0
    | `Package_expr a0 ->
        Format.fprintf fmt "@[<1>(`Package_expr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_module_expr a1) a0
and pp_print_patt: 'fmt -> patt -> 'result =
  fun fmt  ->
    function
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Id a0 ->
        Format.fprintf fmt "@[<1>(`Id@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ident a1) a0
    | `Alias a0 ->
        Format.fprintf fmt "@[<1>(`Alias@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `Ant a0 ->
        Format.fprintf fmt "@[<1>(`Ant@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc a0
    | `PaApp a0 ->
        Format.fprintf fmt "@[<1>(`PaApp@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `Array a0 ->
        Format.fprintf fmt "@[<1>(`Array@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1) a0
    | `PaCom a0 ->
        Format.fprintf fmt "@[<1>(`PaCom@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `Sem a0 ->
        Format.fprintf fmt "@[<1>(`Sem@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_patt a1 pp_print_patt a2) a0
    | `Chr a0 ->
        Format.fprintf fmt "@[<1>(`Chr@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Int a0 ->
        Format.fprintf fmt "@[<1>(`Int@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Int32 a0 ->
        Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Int64 a0 ->
        Format.fprintf fmt "@[<1>(`Int64@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `NativeInt a0 ->
        Format.fprintf fmt "@[<1>(`NativeInt@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_string a1) a0
    | `Flo a0 ->
        Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
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
    | `Str a0 ->
        Format.fprintf fmt "@[<1>(`Str@ %a)@]"
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
    | `Lazy a0 ->
        Format.fprintf fmt "@[<1>(`Lazy@ %a)@]"
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
    | `Nil a0 -> Format.fprintf fmt "@[<1>(`Nil@ %a)@]" pp_print_loc a0
    | `Alias a0 ->
        Format.fprintf fmt "@[<1>(`Alias@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `Any a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc a0
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
    | `Com a0 ->
        Format.fprintf fmt "@[<1>(`Com@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `Sum a0 ->
        Format.fprintf fmt "@[<1>(`Sum@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Of a0 ->
        Format.fprintf fmt "@[<1>(`Of@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `And a0 ->
        Format.fprintf fmt "@[<1>(`And@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `TyOr a0 ->
        Format.fprintf fmt "@[<1>(`TyOr@ %a)@]"
          (fun fmt  (a0,a1,a2)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1 pp_print_ctyp a2) a0
    | `Private a0 ->
        Format.fprintf fmt "@[<1>(`Private@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Mutable a0 ->
        Format.fprintf fmt "@[<1>(`Mutable@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Tup a0 ->
        Format.fprintf fmt "@[<1>(`Tup@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_loc a0
               pp_print_ctyp a1) a0
    | `Sta a0 ->
        Format.fprintf fmt "@[<1>(`Sta@ %a)@]"
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
    | `Package a0 ->
        Format.fprintf fmt "@[<1>(`Package@ %a)@]"
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
        let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
            | `Eq a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Inherit a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Inherit")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_override_flag _loc a1),
                                       (`ExCom
                                          (_loc, (meta_class_expr _loc a2),
                                            (meta_string _loc a3))))))))))
                       _loc a0))
            | `Initializer a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Initializer")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
                                          (_loc,
                                            (meta_override_flag _loc a2),
                                            (`ExCom
                                               (_loc,
                                                 (meta_private_flag _loc a3),
                                                 (`ExCom
                                                    (_loc,
                                                      (meta_expr _loc a4),
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
                                          (_loc,
                                            (meta_override_flag _loc a2),
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_class_expr: 'loc -> class_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
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
            | `Obj a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Obj")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
            | `And a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_class_expr _loc a1),
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
            | `Eq a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_class_expr _loc a1),
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Eq a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_class_sig_item _loc a1),
                                       (meta_class_sig_item _loc a2))))))))
                       _loc a0))
            | `Inherit a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Inherit")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Method a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Method")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (`ExCom
                                          (_loc, (meta_private_flag _loc a2),
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_sig_item _loc a2))))))))
                       _loc a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_str_item: 'loc -> str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Class")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_expr _loc a1)))))) _loc a0))
            | `ClassType a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ClassType")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_str_item _loc a1),
                                       (meta_str_item _loc a2)))))))) _loc a0))
            | `Directive a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Directive")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Exception a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Exception")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `External a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "External")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (`ExCom
                                          (_loc, (meta_ctyp _loc a2),
                                            (meta_meta_list meta_string _loc
                                               a3)))))))))) _loc a0))
            | `Include a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Include")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_expr _loc a1)))))) _loc a0))
            | `Module a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Module")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_expr _loc a2)))))))) _loc
                       a0))
            | `RecModule a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "RecModule")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_binding _loc a1)))))) _loc a0))
            | `ModuleType a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleType")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Open a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Open")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Type a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Type")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Value a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Value")),
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
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                       (meta_module_expr _loc a2)))))))) _loc
                       a0))
            | `Functor a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Functor")),
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
            | `Struct a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Struct")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_str_item _loc a1)))))) _loc a0))
            | `ModuleExprConstraint a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleExprConstraint")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_module_expr _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `PackageModule a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "PackageModule")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_match_case: 'loc -> match_case -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `Case a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Case")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_patt _loc a1),
                                       (`ExCom
                                          (_loc, (meta_expr _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_module_binding: 'loc -> module_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_module_binding _loc a1),
                                       (meta_module_binding _loc a2))))))))
                       _loc a0))
            | `ModuleBind a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleBind")),
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
            | `ModuleConstraint a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleConstraint")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_rec_binding: 'loc -> rec_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_rec_binding _loc a1),
                                       (meta_rec_binding _loc a2)))))))) _loc
                       a0))
            | `RecBind a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "RecBind")),
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
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_binding _loc a1),
                                       (meta_binding _loc a2)))))))) _loc a0))
            | `Bind a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Bind")),
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
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `TypeEq a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TypeEq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `ModuleEq a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleEq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_ident _loc a2)))))))) _loc a0))
            | `TypeSubst a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TypeSubst")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `ModuleSubst a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleSubst")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_ident _loc a2)))))))) _loc a0))
            | `And a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_with_constr _loc a1),
                                       (meta_with_constr _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_sig_item: 'loc -> sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Class")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `ClassType a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ClassType")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_sig_item _loc a1),
                                       (meta_sig_item _loc a2)))))))) _loc a0))
            | `Directive a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Directive")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Exception a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Exception")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `External a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "External")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (`ExCom
                                          (_loc, (meta_ctyp _loc a2),
                                            (meta_meta_list meta_string _loc
                                               a3)))))))))) _loc a0))
            | `Include a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Include")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_type _loc a1)))))) _loc a0))
            | `Module a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Module")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `RecModule a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "RecModule")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_binding _loc a1)))))) _loc a0))
            | `ModuleType a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ModuleType")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Open a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Open")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Type a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Type")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Value a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Value")),
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
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Sig a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sig")),
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
                                       (meta_with_constr _loc a2)))))))) _loc
                       a0))
            | `Of a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Of")),
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
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
            | `Array a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Array")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
            | `Flo a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Flo")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Chr a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Chr")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `For a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "For")),
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
                                                      (meta_direction_flag
                                                         _loc a4),
                                                      (meta_expr _loc a5))))))))))))))
                       _loc a0))
            | `Fun a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Fun")),
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
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Int a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int32 a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int32")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int64 a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int64")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `NativeInt a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "NativeInt")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Label a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Label")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Lazy a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Lazy")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `LetIn a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "LetIn")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_rec_flag _loc a1),
                                       (`ExCom
                                          (_loc, (meta_binding _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `LetModule a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "LetModule")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (`ExCom
                                          (_loc, (meta_module_expr _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Match a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Match")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `New a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "New")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Obj a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Obj")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
            | `OptLabl a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "OptLabl")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `OvrInst a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "OvrInst")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_rec_binding _loc a1)))))) _loc a0))
            | `Record a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Record")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_rec_binding _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Sequence a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sequence")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Send a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Send")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_string _loc a2)))))))) _loc a0))
            | `StringDot a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "StringDot")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Str a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Str")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Try a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Try")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `ExTup a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "ExTup")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
            | `Constraint_exp a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Constraint_exp")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `While a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "While")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Let_open a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Let_open")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `LocalTypeFun a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "LocalTypeFun")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Package_expr a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Package_expr")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_expr _loc a1)))))) _loc a0))
        and meta_patt: 'loc -> patt -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Alias a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Alias")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_patt _loc a2)))))))) _loc a0))
            | `Ant a0 -> `Ant a0
            | `Any a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Any")), (meta_loc _loc a0))
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
            | `Array a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Array")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
            | `Sem a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_patt _loc a2)))))))) _loc a0))
            | `Chr a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Chr")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int32 a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int32")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int64 a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Int64")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `NativeInt a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "NativeInt")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Flo a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Flo")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
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
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
            | `Str a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Str")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `PaTup a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "PaTup")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `PaVrn a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "PaVrn")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Lazy a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Lazy")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
            | `PaMod a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "PaMod")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
        and meta_ctyp: 'loc -> ctyp -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Alias a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Alias")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Any a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Any")), (meta_loc _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                          (_loc,
                                            (meta_list meta_ctyp _loc a2),
                                            (`ExCom
                                               (_loc, (meta_ctyp _loc a3),
                                                 (meta_list
                                                    (fun _loc  (a0,a1)  ->
                                                       `ExTup
                                                         (_loc,
                                                           (`ExCom
                                                              (_loc,
                                                                (meta_ctyp
                                                                   _loc a0),
                                                                (meta_ctyp
                                                                   _loc a1)))))
                                                    _loc a4)))))))))))) _loc
                       a0))
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
                                       (meta_row_var_flag _loc a2))))))))
                       _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyQuP a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TyQuP")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyQuM a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TyQuM")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyRec a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TyRec")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
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
            | `Com a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Com")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Sum a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sum")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Of a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Of")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (`ExCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `And a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "And")),
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
            | `Private a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Private")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Mutable a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Mutable")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Tup a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Tup")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Sta a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Sta")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `TyVrnSup a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TyVrnSup")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `TyVrnInf a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "TyVrnInf")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
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
            | `Package a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Package")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Uid a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Uid")),
                    (((fun _loc  (a0,a1)  ->
                         `ExTup
                           (_loc,
                             (`ExCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_meta_list :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_list -> 'result=
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
            | `Some a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Some")), (mf_a _loc a0))
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
                `ExApp
                  (_loc, (`ExVrn (_loc, "Override")), (meta_loc _loc a0))
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
            | `To a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "To")), (meta_loc _loc a0))
            | `Downto a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "Downto")), (meta_loc _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_rec_flag: 'loc -> rec_flag -> 'result =
          fun _loc  ->
            function
            | `Recursive a0 ->
                `ExApp
                  (_loc, (`ExVrn (_loc, "Recursive")), (meta_loc _loc a0))
            | `ReNil a0 ->
                `ExApp (_loc, (`ExVrn (_loc, "ReNil")), (meta_loc _loc a0))
            | `Ant a0 -> `Ant a0
      end
    module Patt =
      struct
        open MPatt
        let meta_loc = MetaLoc.meta_loc_patt
        let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
            | `Eq a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Inherit a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Inherit")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_override_flag _loc a1),
                                       (`PaCom
                                          (_loc, (meta_class_expr _loc a2),
                                            (meta_string _loc a3))))))))))
                       _loc a0))
            | `Initializer a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Initializer")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
                                          (_loc,
                                            (meta_override_flag _loc a2),
                                            (`PaCom
                                               (_loc,
                                                 (meta_private_flag _loc a3),
                                                 (`PaCom
                                                    (_loc,
                                                      (meta_expr _loc a4),
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
                                          (_loc,
                                            (meta_override_flag _loc a2),
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_class_expr: 'loc -> class_expr -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
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
            | `Obj a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Obj")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
            | `And a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_class_expr _loc a1),
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
            | `Eq a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_class_expr _loc a1),
                                       (meta_class_expr _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Eq a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Eq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_class_sig_item _loc a1),
                                       (meta_class_sig_item _loc a2))))))))
                       _loc a0))
            | `Inherit a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Inherit")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Method a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Method")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (`PaCom
                                          (_loc, (meta_private_flag _loc a2),
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_sig_item _loc a2))))))))
                       _loc a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
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
                                       (meta_class_type _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_str_item: 'loc -> str_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Class")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_expr _loc a1)))))) _loc a0))
            | `ClassType a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ClassType")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_str_item _loc a1),
                                       (meta_str_item _loc a2)))))))) _loc a0))
            | `Directive a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Directive")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Exception a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Exception")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `External a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "External")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (`PaCom
                                          (_loc, (meta_ctyp _loc a2),
                                            (meta_meta_list meta_string _loc
                                               a3)))))))))) _loc a0))
            | `Include a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Include")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_expr _loc a1)))))) _loc a0))
            | `Module a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Module")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_expr _loc a2)))))))) _loc
                       a0))
            | `RecModule a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "RecModule")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_binding _loc a1)))))) _loc a0))
            | `ModuleType a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleType")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Open a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Open")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Type a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Type")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Value a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Value")),
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
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                       (meta_module_expr _loc a2)))))))) _loc
                       a0))
            | `Functor a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Functor")),
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
            | `Struct a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Struct")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_str_item _loc a1)))))) _loc a0))
            | `ModuleExprConstraint a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleExprConstraint")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_module_expr _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `PackageModule a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "PackageModule")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_match_case: 'loc -> match_case -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
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
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `Case a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Case")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_patt _loc a1),
                                       (`PaCom
                                          (_loc, (meta_expr _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_module_binding: 'loc -> module_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_module_binding _loc a1),
                                       (meta_module_binding _loc a2))))))))
                       _loc a0))
            | `ModuleBind a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleBind")),
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
            | `ModuleConstraint a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleConstraint")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_rec_binding: 'loc -> rec_binding -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_rec_binding _loc a1),
                                       (meta_rec_binding _loc a2)))))))) _loc
                       a0))
            | `RecBind a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "RecBind")),
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
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `And a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_binding _loc a1),
                                       (meta_binding _loc a2)))))))) _loc a0))
            | `Bind a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Bind")),
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
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `TypeEq a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TypeEq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `ModuleEq a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleEq")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_ident _loc a2)))))))) _loc a0))
            | `TypeSubst a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TypeSubst")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `ModuleSubst a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleSubst")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_ident _loc a2)))))))) _loc a0))
            | `And a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "And")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_with_constr _loc a1),
                                       (meta_with_constr _loc a2)))))))) _loc
                       a0))
            | `Ant a0 -> `Ant a0
        and meta_sig_item: 'loc -> sig_item -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Class a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Class")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `ClassType a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ClassType")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_class_type _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_sig_item _loc a1),
                                       (meta_sig_item _loc a2)))))))) _loc a0))
            | `Directive a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Directive")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Exception a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Exception")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `External a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "External")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (`PaCom
                                          (_loc, (meta_ctyp _loc a2),
                                            (meta_meta_list meta_string _loc
                                               a3)))))))))) _loc a0))
            | `Include a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Include")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_type _loc a1)))))) _loc a0))
            | `Module a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Module")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `RecModule a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "RecModule")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_binding _loc a1)))))) _loc a0))
            | `ModuleType a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ModuleType")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_module_type _loc a2)))))))) _loc
                       a0))
            | `Open a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Open")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Type a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Type")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Value a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Value")),
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
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Sig a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sig")),
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
                                       (meta_with_constr _loc a2)))))))) _loc
                       a0))
            | `Of a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Of")),
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
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
            | `Array a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Array")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
                                            (meta_ctyp _loc a3)))))))))) _loc
                       a0))
            | `Flo a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Flo")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Chr a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Chr")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `For a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "For")),
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
                                                      (meta_direction_flag
                                                         _loc a4),
                                                      (meta_expr _loc a5))))))))))))))
                       _loc a0))
            | `Fun a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Fun")),
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
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Int a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int32 a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int32")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int64 a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int64")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `NativeInt a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "NativeInt")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Label a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Label")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Lazy a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Lazy")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `LetIn a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "LetIn")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_rec_flag _loc a1),
                                       (`PaCom
                                          (_loc, (meta_binding _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `LetModule a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "LetModule")),
                    (((fun _loc  (a0,a1,a2,a3)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (`PaCom
                                          (_loc, (meta_module_expr _loc a2),
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
            | `Match a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Match")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `New a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "New")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Obj a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Obj")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_class_str_item _loc a2))))))))
                       _loc a0))
            | `OptLabl a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "OptLabl")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `OvrInst a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "OvrInst")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_rec_binding _loc a1)))))) _loc a0))
            | `Record a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Record")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_rec_binding _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Sequence a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sequence")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
            | `Send a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Send")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_string _loc a2)))))))) _loc a0))
            | `StringDot a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "StringDot")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Str a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Str")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Try a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Try")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_match_case _loc a2)))))))) _loc
                       a0))
            | `ExTup a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "ExTup")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_expr _loc a1)))))) _loc a0))
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
            | `Constraint_exp a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Constraint_exp")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `While a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "While")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_expr _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Let_open a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Let_open")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ident _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `LocalTypeFun a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "LocalTypeFun")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_string _loc a1),
                                       (meta_expr _loc a2)))))))) _loc a0))
            | `Package_expr a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Package_expr")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_module_expr _loc a1)))))) _loc a0))
        and meta_patt: 'loc -> patt -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Id a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Id")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `Alias a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Alias")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_patt _loc a2)))))))) _loc a0))
            | `Ant a0 -> `Ant a0
            | `Any a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Any")), (meta_loc _loc a0))
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
            | `Array a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Array")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
            | `Sem a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sem")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_patt _loc a1),
                                       (meta_patt _loc a2)))))))) _loc a0))
            | `Chr a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Chr")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int32 a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int32")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Int64 a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Int64")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `NativeInt a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "NativeInt")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Flo a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Flo")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
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
                                            (meta_expr _loc a3)))))))))) _loc
                       a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
            | `Str a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Str")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `PaTup a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "PaTup")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
            | `PaVrn a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "PaVrn")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Lazy a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Lazy")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_patt _loc a1)))))) _loc a0))
            | `PaMod a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "PaMod")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
        and meta_ctyp: 'loc -> ctyp -> 'result =
          fun _loc  ->
            function
            | `Nil a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Nil")), (meta_loc _loc a0))
            | `Alias a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Alias")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Any a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Any")), (meta_loc _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ident _loc a1)))))) _loc a0))
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
                                          (_loc,
                                            (meta_list meta_ctyp _loc a2),
                                            (`PaCom
                                               (_loc, (meta_ctyp _loc a3),
                                                 (meta_list
                                                    (fun _loc  (a0,a1)  ->
                                                       `PaTup
                                                         (_loc,
                                                           (`PaCom
                                                              (_loc,
                                                                (meta_ctyp
                                                                   _loc a0),
                                                                (meta_ctyp
                                                                   _loc a1)))))
                                                    _loc a4)))))))))))) _loc
                       a0))
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
                                       (meta_row_var_flag _loc a2))))))))
                       _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyQuP a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TyQuP")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyQuM a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TyQuM")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `TyRec a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TyRec")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
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
            | `Com a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Com")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `Sum a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sum")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Of a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Of")),
                    (((fun _loc  (a0,a1,a2)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (`PaCom
                                     (_loc, (meta_ctyp _loc a1),
                                       (meta_ctyp _loc a2)))))))) _loc a0))
            | `And a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "And")),
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
            | `Private a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Private")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Mutable a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Mutable")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Tup a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Tup")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `Sta a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Sta")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `TyVrnSup a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TyVrnSup")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
            | `TyVrnInf a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "TyVrnInf")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_ctyp _loc a1)))))) _loc a0))
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
            | `Package a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Package")),
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
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Uid a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Uid")),
                    (((fun _loc  (a0,a1)  ->
                         `PaTup
                           (_loc,
                             (`PaCom
                                (_loc, (meta_loc _loc a0),
                                  (meta_string _loc a1)))))) _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_meta_list :
          'all_a0 .
            ('loc -> 'all_a0 -> 'result) ->
              'loc -> 'all_a0 meta_list -> 'result=
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
            | `Some a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Some")), (mf_a _loc a0))
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
                `PaApp
                  (_loc, (`PaVrn (_loc, "Override")), (meta_loc _loc a0))
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
            | `To a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "To")), (meta_loc _loc a0))
            | `Downto a0 ->
                `PaApp (_loc, (`PaVrn (_loc, "Downto")), (meta_loc _loc a0))
            | `Ant a0 -> `Ant a0
        and meta_rec_flag: 'loc -> rec_flag -> 'result =
          fun _loc  ->
            function
            | `Recursive a0 ->
                `PaApp
                  (_loc, (`PaVrn (_loc, "Recursive")), (meta_loc _loc a0))
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
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | `ExApp (_loc,_,_) -> error () | t -> self t
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
    | `Id (_loc,`Lid (_,_)) -> error ()
    | `Id (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | `Id (_loc,i) -> i | p -> self p
let rec is_irrefut_patt =
  function
  | `Id (_loc,`Lid (_,_)) -> true
  | `Id (_loc,`Uid (_,"()")) -> true
  | `Any _loc -> true
  | `Nil _loc -> true
  | `Alias (_loc,x,y) -> (is_irrefut_patt x) && (is_irrefut_patt y)
  | `PaRec (_loc,p) -> is_irrefut_patt p
  | `PaEq (_loc,_,p) -> is_irrefut_patt p
  | `Sem (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaCom (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaOrp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaApp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | `PaTyc (_loc,p,_) -> is_irrefut_patt p
  | `PaTup (_loc,pl) -> is_irrefut_patt pl
  | `PaOlb (_loc,_,`Nil _) -> true
  | `PaOlb (_loc,_,_) -> true
  | `PaOlbi (_loc,_,_,_) -> true
  | `PaLab (_loc,_,`Nil _) -> true
  | `PaLab (_loc,_,p) -> is_irrefut_patt p
  | `Lazy (_loc,p) -> is_irrefut_patt p
  | `Id (_loc,_) -> false
  | `PaMod (_loc,_) -> true
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
  | t::ts -> let _loc = loc_of_ctyp t in `TyOr (_loc, t, (tyOr_of_list ts))
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
    | t::[] -> `TyQuo (_loc, t)
    | t::ts ->
        List.fold_left (fun x  y  -> `TyApp (_loc, x, (`TyQuo (_loc, y))))
          (`TyQuo (_loc, t)) ts in
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
  | [] -> `CtNil ghost
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
  | (_loc,s,[]) -> `TyId (_loc, (`Uid (_loc, s)))
  | (_loc,s,tl) ->
      `Of (_loc, (`TyId (_loc, (`Uid (_loc, s)))), (tyAnd_of_list tl))
let ty_of_sbt =
  function
  | (_loc,s,true ,t) ->
      `TyCol (_loc, (`TyId (_loc, (`Lid (_loc, s)))), (`Mutable (_loc, t)))
  | (_loc,s,false ,t) -> `TyCol (_loc, (`TyId (_loc, (`Lid (_loc, s)))), t)
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
    |`And (_loc,x,y)|`TyOr (_loc,x,y) -> list_of_ctyp x (list_of_ctyp y acc)
  | x -> x :: acc
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
      | `Alias (_loc,p,`Nil _l)|`PaOrp (_loc,`Nil _l,p)
        |`PaOrp (_loc,p,`Nil _l)|`PaCom (_loc,`Nil _l,p)
        |`PaCom (_loc,p,`Nil _l)|`Sem (_loc,`Nil _l,p)|`Sem (_loc,p,`Nil _l)
          -> p
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
        |`TyArr (_loc,`Nil _l,t)|`TyOr (_loc,`Nil _l,t)
        |`TyOr (_loc,t,`Nil _l)|`Of (_loc,t,`Nil _l)|`And (_loc,`Nil _l,t)
        |`And (_loc,t,`Nil _l)|`TySem (_loc,t,`Nil _l)
        |`TySem (_loc,`Nil _l,t)|`Com (_loc,`Nil _l,t)|`Com (_loc,t,`Nil _l)
        |`TyAmp (_loc,t,`Nil _l)|`TyAmp (_loc,`Nil _l,t)
        |`Sta (_loc,`Nil _l,t)|`Sta (_loc,t,`Nil _l) -> t
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
      | `CtAnd (_loc,`CtNil _l,ct)|`CtAnd (_loc,ct,`CtNil _l) -> ct
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