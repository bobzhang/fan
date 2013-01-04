open Parsetree
open Asttypes
open Longident
open LibUtil
module Ast = FanAst
class printer =
  object (self : 'self)
    method longident _loc i =
      match i with
      | Lident s -> IdLid (_loc, s)
      | Ldot (y,s) ->
          IdAcc (_loc, (self#longident _loc y), (IdLid (_loc, s)))
      | Lapply (a,b) ->
          IdApp (_loc, (self#longident _loc a), (self#longident _loc b))
    method longident_loc i = self#longident i.loc i.txt
    method gen_cases _loc (lst : (pattern* expression) list) =
      List.map
        (fun (p,e)  ->
           match e.pexp_desc with
           | Pexp_when (e1,e2) ->
               McArr (_loc, (self#pattern p), (self#expr e1), (self#expr e2))
           | _ ->
               McArr
                 (_loc, (self#pattern p), (ExNil _loc),
                   (self#expr (e : expression )))) lst
    method constant_expr _loc i =
      match i with
      | Const_int32 i -> ExInt32 (_loc, (Int32.to_string i))
      | Const_int i -> ExInt (_loc, (string_of_int i))
      | Const_int64 i -> ExInt64 (_loc, (Int64.to_string i))
      | Const_float i -> ExFlo (_loc, i)
      | Const_nativeint i -> ExNativeInt (_loc, (Nativeint.to_string i))
      | Const_char i -> ExChr (_loc, (Char.escaped i))
      | Const_string i -> ExStr (_loc, (Ast.safe_string_escaped i))
    method constant_patt _loc i =
      match i with
      | Const_int32 i -> PaInt32 (_loc, (Int32.to_string i))
      | Const_int i -> PaInt (_loc, (string_of_int i))
      | Const_int64 i -> PaInt64 (_loc, (Int64.to_string i))
      | Const_float i -> PaFlo (_loc, i)
      | Const_nativeint i -> PaNativeInt (_loc, (Nativeint.to_string i))
      | Const_char i -> PaChr (_loc, (Char.escaped i))
      | Const_string i -> PaStr (_loc, (Ast.safe_string_escaped i))
    method mutable_flag =
      function | Immutable  -> MuNil | Mutable  -> MuMutable
    method virtual_flag =
      function | Concrete  -> ViNil | Virtual  -> ViVirtual
    method rec_flag =
      function | Nonrecursive  -> ReNil | Recursive |Default  -> ReRecursive
    method direction_flag = function | Upto  -> DiTo | Downto  -> DiDownto
    method private_flag = function | Public  -> PrNil | Private  -> PrPrivate
    method core_type { ptyp_desc = ty; ptyp_loc = _loc } =
      match ty with
      | Ptyp_any  -> TyAny _loc
      | Ptyp_var s -> TyId (_loc, (IdLid (_loc, s)))
      | Ptyp_arrow (label,t1,t2) ->
          (match label with
           | "" -> TyArr (_loc, (self#core_type t1), (self#core_type t2))
           | s ->
               if (s.[0]) = '?'
               then
                 TyArr
                   (_loc, (TyOlb (_loc, label, (self#core_type t1))),
                     (self#core_type t2))
               else
                 TyArr
                   (_loc, (TyLab (_loc, label, (self#core_type t1))),
                     (self#core_type t2)))
      | Ptyp_tuple (x::xs) ->
          TyTup
            (_loc,
              (TySta
                 (_loc, (self#core_type x),
                   (Ast.tySta_of_list (List.map self#core_type xs)))))
      | Ptyp_tuple [] -> assert false
      | Ptyp_alias (ty,s) ->
          TyAli (_loc, (self#core_type ty), (TyId (_loc, (IdLid (_loc, s)))))
      | Ptyp_variant (rfs,closed,labels) ->
          let ls =
            List.map
              (function
               | Rinherit t -> self#core_type t
               | Rtag (label,_b,[]) -> TyVrn (_loc, label)
               | Rtag (label,_b,ls) ->
                   TyOfAmp
                     (_loc, (TyVrn (_loc, label)),
                       (Ast.tyAmp_of_list (List.map self#core_type ls)))) rfs in
          (match (closed, labels) with
           | (true ,None ) -> TyVrnEq (_loc, (Ast.tyOr_of_list ls))
           | (true ,Some x) ->
               let u =
                 (List.map (fun x  -> TyVrn (_loc, x)) x) |>
                   FanAst.tyApp_of_list in
               TyVrnInfSup (_loc, (Ast.tyOr_of_list ls), u)
           | (false ,_) -> TyVrnSup (_loc, (Ast.tyOr_of_list ls)))
      | Ptyp_constr (lid_loc,ts) ->
          FanAst.tyApp_of_list ((TyId (_loc, (self#longident_loc lid_loc)))
            :: (List.map self#core_type ts))
      | Ptyp_object cfs ->
          let row_var = ref false in
          let res =
            List.fold_left
              (fun acc  { pfield_loc = _loc; pfield_desc = cf }  ->
                 match cf with
                 | Pfield (lab,ty) ->
                     let t =
                       TyCol
                         (_loc, (TyId (_loc, (IdLid (_loc, lab)))),
                           (self#core_type ty)) in
                     TySem (_loc, acc, t)
                 | Pfield_var  -> acc) (TyNil _loc) cfs in
          if row_var.contents
          then TyObj (_loc, res, RvRowVar)
          else TyObj (_loc, res, RvNil)
      | Ptyp_class (lid_loc,cts,_lows) ->
          Ast.tyApp_of_list ((TyCls (_loc, (self#longident_loc lid_loc))) ::
            (List.map self#core_type cts))
      | Ptyp_poly (ls,ty) ->
          TyPol
            (_loc, (Ast.tyVarApp_of_list (_loc, ls)), (self#core_type ty))
      | Ptyp_package (lid,ls) ->
          let with_constrs =
            List.map
              (fun (lid,ty)  ->
                 WcTyp
                   (_loc, (TyId (_loc, (self#longident_loc lid))),
                     (self#core_type ty))) ls in
          TyPkg
            (_loc,
              (MtWit
                 (_loc, (MtId (_loc, (self#longident_loc lid))),
                   (Ast.wcAnd_of_list with_constrs))))
    method pattern { ppat_desc = x; ppat_loc = _loc } =
      match x with
      | Ppat_any  -> PaAny _loc
      | Ppat_var { txt;_} -> PaId (_loc, (IdLid (_loc, txt)))
      | Ppat_alias (p,{ txt;_}) ->
          PaAli (_loc, (self#pattern p), (PaId (_loc, (IdLid (_loc, txt)))))
      | Ppat_constant c -> self#constant_patt _loc c
      | Ppat_tuple [] -> assert false
      | Ppat_tuple (x::xs) ->
          PaTup
            (_loc,
              (PaCom
                 (_loc, (self#pattern x),
                   (Ast.paCom_of_list (List.map self#pattern xs)))))
      | Ppat_construct (lid_loc,opt,_b) ->
          (match opt with
           | None  -> PaId (_loc, (self#longident_loc lid_loc))
           | Some x ->
               PaApp
                 (_loc, (PaId (_loc, (self#longident_loc lid_loc))),
                   (self#pattern x)))
      | Ppat_variant (label,opt) ->
          (match opt with
           | Some o -> PaApp (_loc, (PaVrn (_loc, label)), (self#pattern o))
           | None  -> PaVrn (_loc, label))
      | Ppat_record (lst,closed) ->
          let ls =
            List.map
              (fun (lid_loc,p)  ->
                 PaEq (_loc, (self#longident_loc lid_loc), (self#pattern p)))
              lst in
          (match closed with
           | Closed  -> PaRec (_loc, (Ast.paSem_of_list ls))
           | Open  ->
               PaRec
                 (_loc, (PaSem (_loc, (Ast.paSem_of_list ls), (PaAny _loc)))))
      | Ppat_array ls ->
          let ls = List.map self#pattern ls in
          PaArr (_loc, (Ast.paSem_of_list ls))
      | Ppat_or (p1,p2) -> PaOrp (_loc, (self#pattern p1), (self#pattern p2))
      | Ppat_constraint (p1,ty) ->
          PaTyc (_loc, (self#pattern p1), (self#core_type ty))
      | Ppat_type lid_loc -> PaTyp (_loc, (self#longident_loc lid_loc))
      | Ppat_lazy p -> PaLaz (_loc, (self#pattern p))
      | Ppat_unpack { txt;_} -> PaMod (_loc, txt)
    method expr { pexp_desc = x; pexp_loc = _loc } =
      match x with
      | Pexp_ident lid_loc -> ExId (_loc, (self#longident_loc lid_loc))
      | Pexp_constant c -> self#constant_expr _loc c
      | Pexp_let (recf,lst,e) ->
          let recf = self#rec_flag recf in
          let bindings =
            List.map
              (fun (p,e)  -> BiEq (_loc, (self#pattern p), (self#expr e)))
              lst in
          ExLet (_loc, recf, (Ast.biAnd_of_list bindings), (self#expr e))
      | Pexp_function (label,eo,lst) ->
          (match label with
           | "" ->
               let cases = self#gen_cases _loc lst in
               ExFun (_loc, (Ast.mcOr_of_list cases))
           | _ ->
               (match lst with
                | (p,e)::[] ->
                    if (label.[0]) = '?'
                    then
                      (match eo with
                       | Some o ->
                           ExFun
                             (_loc,
                               (McArr
                                  (_loc,
                                    (PaOlbi
                                       (_loc, label, (self#pattern p),
                                         (self#expr o))), (ExNil _loc),
                                    (self#expr e))))
                       | None  ->
                           ExFun
                             (_loc,
                               (McArr
                                  (_loc, (PaOlb (_loc, label, (PaNil _loc))),
                                    (ExNil _loc), (self#expr e)))))
                    else
                      ExFun
                        (_loc,
                          (McArr
                             (_loc, (PaLab (_loc, label, (PaNil _loc))),
                               (ExNil _loc), (self#expr e))))
                | _ -> assert false))
      | Pexp_apply (e,lst) ->
          let args =
            List.map
              (fun (label,e)  ->
                 let v = self#expr e in
                 if label = "" then v else ExLab (_loc, label, v)) lst in
          FanAst.exApp_of_list ((self#expr e) :: args)
      | Pexp_match (e,lst) ->
          let cases = self#gen_cases _loc lst in
          ExMat (_loc, (self#expr e), (Ast.mcOr_of_list cases))
      | Pexp_try (e,lst) ->
          let cases = self#gen_cases _loc lst in
          ExTry (_loc, (self#expr e), (Ast.mcOr_of_list cases))
      | Pexp_tuple [] -> assert false
      | Pexp_tuple (x::xs) ->
          ExTup
            (_loc,
              (ExCom
                 (_loc, (self#expr x),
                   (Ast.exCom_of_list (List.map self#expr xs)))))
      | Pexp_construct (lid_loc,eo,_) ->
          (match eo with
           | None  -> ExId (_loc, (self#longident_loc lid_loc))
           | Some v ->
               ExApp
                 (_loc, (ExId (_loc, (self#longident_loc lid_loc))),
                   (self#expr v)))
      | Pexp_variant (label,eo) ->
          (match eo with
           | Some e -> ExApp (_loc, (ExVrn (_loc, label)), (self#expr e))
           | None  -> ExVrn (_loc, label))
      | Pexp_record (lst,eo) ->
          let bindings =
            List.map
              (fun (lid,e)  ->
                 RbEq (_loc, (self#longident_loc lid), (self#expr e))) lst in
          (match eo with
           | Some e ->
               ExRec (_loc, (Ast.rbSem_of_list bindings), (self#expr e))
           | None  ->
               ExRec (_loc, (Ast.rbSem_of_list bindings), (ExNil _loc)))
      | Pexp_field (e,lid_loc) ->
          ExAcc
            (_loc, (self#expr e),
              (ExId (_loc, (self#longident_loc lid_loc))))
      | Pexp_setfield (e1,lid_loc,e2) ->
          ExAss
            (_loc,
              (ExAcc
                 (_loc, (self#expr e1),
                   (ExId (_loc, (self#longident_loc lid_loc))))),
              (self#expr e2))
      | Pexp_array lst ->
          ExArr (_loc, (Ast.exSem_of_list (List.map self#expr lst)))
      | Pexp_ifthenelse (e1,e2,e3) ->
          (match e3 with
           | Some e3 ->
               ExIfe (_loc, (self#expr e1), (self#expr e2), (self#expr e3))
           | None  ->
               ExIfe
                 (_loc, (self#expr e1), (self#expr e2),
                   (ExId (_loc, (IdUid (_loc, "()"))))))
      | Pexp_sequence (e1,e2) -> ExApp (_loc, (self#expr e1), (self#expr e2))
      | Pexp_while (e1,e2) -> ExWhi (_loc, (self#expr e1), (self#expr e2))
      | Pexp_for ({ txt;_},e1,e2,df,e3) ->
          ExFor
            (_loc, txt, (self#expr e1), (self#expr e2),
              (self#direction_flag df), (self#expr e3))
      | Pexp_constraint (e1,ot1,ot2) ->
          (match (ot1, ot2) with
           | (None ,None ) -> self#expr e1
           | (Some t1,Some t2) ->
               ExCoe
                 (_loc, (self#expr e1), (self#core_type t1),
                   (self#core_type t2))
           | (Some t1,None ) ->
               ExTyc (_loc, (self#expr e1), (self#core_type t1))
           | (None ,Some t2) ->
               ExCoe
                 (_loc, (self#expr e1), (TyNil _loc), (self#core_type t2)))
      | Pexp_when _ -> assert false
      | Pexp_send (e,txt) -> ExSnd (_loc, (self#expr e), txt)
      | Pexp_new lid_loc -> ExNew (_loc, (self#longident_loc lid_loc))
      | Pexp_setinstvar ({ txt;_},e) ->
          ExAss (_loc, (ExId (_loc, (IdLid (_loc, txt)))), (self#expr e))
      | Pexp_override lst ->
          let lst =
            List.map
              (fun ({ txt;_},e)  ->
                 RbEq (_loc, (IdLid (_loc, txt)), (self#expr e))) lst in
          ExOvr (_loc, (Ast.rbSem_of_list lst))
      | Pexp_letmodule ({ txt;_},me,e) ->
          ExLmd (_loc, txt, (self#module_expr me), (self#expr e))
      | Pexp_assert e -> ExAsr (_loc, (self#expr e))
      | Pexp_assertfalse  -> ExAsf _loc
      | Pexp_lazy e -> ExLaz (_loc, (self#expr e))
      | Pexp_poly _ -> assert false
      | Pexp_object { pcstr_pat = pat; pcstr_fields = fs } ->
          ExObj (_loc, (self#pattern pat), (self#class_fields fs))
      | Pexp_newtype (str,e) -> ExFUN (_loc, str, (self#expr e))
      | Pexp_pack me -> ExPkg (_loc, (self#module_expr me))
      | Pexp_open (lid_loc,e) ->
          ExOpI (_loc, (self#longident_loc lid_loc), (self#expr e))
    method module_expr { pmod_desc = x; pmod_loc = _loc } =
      (match x with
       | Pmod_ident lid_loc -> MeId (_loc, (self#longident_loc lid_loc))
       | Pmod_structure s -> MeStr (_loc, (self#structure s))
       | Pmod_functor ({ txt;_},mty,me) ->
           MeFun (_loc, txt, (self#module_type mty), (self#module_expr me))
       | Pmod_apply (me1,me2) ->
           MeApp (_loc, (self#module_expr me1), (self#module_expr me2))
       | Pmod_constraint (me,mty) ->
           MeTyc (_loc, (self#module_expr me), (self#module_type mty))
       | Pmod_unpack e -> MePkg (_loc, (self#expr e)) : Ast.module_expr )
    method lhs_type_declaration (params,variance,({ loc;_} as lid_loc)) =
      let u =
        List.map2
          (fun p  v  ->
             match (p, v) with
             | ((false ,false ),Some { txt; loc = _loc }) ->
                 TyQuo (_loc, txt)
             | ((false ,false ),None ) ->
                 let _loc = FanLoc.ghost in TyAny _loc
             | ((true ,false ),Some { txt; loc = _loc }) -> TyQuP (_loc, txt)
             | ((true ,false ),None ) ->
                 let _loc = FanLoc.ghost in Ast.TyAnP _loc
             | ((false ,true ),Some { txt; loc = _loc }) -> TyQuM (_loc, txt)
             | ((false ,true ),None ) ->
                 let _loc = FanLoc.ghost in Ast.TyAnM _loc
             | _ -> assert false) variance params in
      FanAst.tyApp_of_list ((TyId (loc, (self#longident_loc lid_loc))) :: u)
    method with_constraint (({ loc = _loc;_} as lid1),w) =
      match w with
      | Pwith_type
          { ptype_params = ls; ptype_manifest = Some ty; ptype_variance;_} ->
          WcTyp
            (_loc, (self#lhs_type_declaration (ls, ptype_variance, lid1)),
              (self#core_type ty))
      | Pwith_typesubst
          { ptype_params = ls; ptype_manifest = Some ty; ptype_variance;_} ->
          WcTyS
            (_loc, (self#lhs_type_declaration (ls, ptype_variance, lid1)),
              (self#core_type ty))
      | Pwith_type _|Pwith_typesubst _ -> assert false
      | Pwith_module lid2 ->
          WcMod (_loc, (self#longident_loc lid1), (self#longident_loc lid2))
      | Pwith_modsubst lid2 ->
          WcMoS (_loc, (self#longident_loc lid1), (self#longident_loc lid2))
    method module_type { pmty_desc = x; pmty_loc = _loc } =
      (match x with
       | Pmty_ident lid_loc -> MtId (_loc, (self#longident_loc lid_loc))
       | Pmty_signature s -> MtSig (_loc, (self#signature s))
       | Pmty_functor ({ txt;_},mty1,mty2) ->
           MtFun
             (_loc, txt, (self#module_type mty1), (self#module_type mty2))
       | Pmty_with (mt1,lst) ->
           let lst = List.map self#with_constraint lst in
           MtWit (_loc, (self#module_type mt1), (Ast.wcAnd_of_list lst))
       | Pmty_typeof me -> MtOf (_loc, (self#module_expr me)) : Ast.module_type )
    method structure_item { pstr_desc = x; pstr_loc = _loc } =
      (match x with
       | Pstr_eval e -> StExp (_loc, (self#expr e))
       | Pstr_value (rf,lst) ->
           let bindings =
             List.map
               (fun (p,e)  -> BiEq (_loc, (self#pattern p), (self#expr e)))
               lst in
           StVal (_loc, (self#rec_flag rf), (Ast.biAnd_of_list bindings))
       | Pstr_module ({ txt;_},me) ->
           StMod (_loc, txt, (self#module_expr me))
       | Pstr_modtype ({ txt;_},mty) ->
           StMty (_loc, txt, (self#module_type mty))
       | Pstr_open lid -> StOpn (_loc, (self#longident_loc lid))
       | Pstr_include me -> StInc (_loc, (self#module_expr me))
       | Pstr_class_type _|Pstr_class _|Pstr_recmodule _|Pstr_exn_rebind
           _|Pstr_exception _|Pstr_primitive _|Pstr_type _ -> assert false : 
      Ast.str_item )
    method structure (ls : structure) = (assert false : Ast.str_item )
    method signature (ls : signature) = (assert false : Ast.sig_item )
    method signature_item { psig_desc = x; psig_loc = _loc } =
      (raise Not_found : Ast.sig_item )
    method class_fields (ls : class_field list) =
      (assert false : Ast.class_str_item )
    method class_field { pcf_desc = x; pcf_loc = _loc } =
      (assert false : Ast.class_str_item )
    method class_expr { pcl_desc = x; pcl_loc = _loc } =
      (assert false : Ast.class_expr )
    method class_type ({ pci_expr;_} : class_type class_infos) =
      (assert false : Ast.class_type )
  end