open Parsetree
open Asttypes
open Longident
open LibUtil
module Ast = Camlp4Ast
class printer =
  object (self : 'self)
    method longident _loc i =
      match i with
      | Lident s -> Ast.IdLid (_loc, s)
      | Ldot (y,s) ->
          Ast.IdAcc (_loc, (self#longident _loc y), (Ast.IdLid (_loc, s)))
      | Lapply (a,b) ->
          Ast.IdApp (_loc, (self#longident _loc a), (self#longident _loc b))
    method longident_loc i = self#longident i.loc i.txt
    method gen_cases _loc (lst : (pattern* expression) list) =
      List.map
        (fun (p,e)  ->
           match e.pexp_desc with
           | Pexp_when (e1,e2) ->
               Ast.McArr
                 (_loc, (self#pattern p), (self#expr e1), (self#expr e2))
           | _ ->
               Ast.McArr
                 (_loc, (self#pattern p), (Ast.ExNil _loc),
                   (self#expr (e : expression )))) lst
    method constant_expr _loc i =
      match i with
      | Const_int32 i -> Ast.ExInt32 (_loc, (Int32.to_string i))
      | Const_int i -> Ast.ExInt (_loc, (string_of_int i))
      | Const_int64 i -> Ast.ExInt64 (_loc, (Int64.to_string i))
      | Const_float i -> Ast.ExFlo (_loc, i)
      | Const_nativeint i -> Ast.ExNativeInt (_loc, (Nativeint.to_string i))
      | Const_char i -> Ast.ExChr (_loc, (Char.escaped i))
      | Const_string i -> Ast.ExStr (_loc, (Ast.safe_string_escaped i))
    method constant_patt _loc i =
      match i with
      | Const_int32 i -> Ast.PaInt32 (_loc, (Int32.to_string i))
      | Const_int i -> Ast.PaInt (_loc, (string_of_int i))
      | Const_int64 i -> Ast.PaInt64 (_loc, (Int64.to_string i))
      | Const_float i -> Ast.PaFlo (_loc, i)
      | Const_nativeint i -> Ast.PaNativeInt (_loc, (Nativeint.to_string i))
      | Const_char i -> Ast.PaChr (_loc, (Char.escaped i))
      | Const_string i -> Ast.PaStr (_loc, (Ast.safe_string_escaped i))
    method mutable_flag =
      function | Immutable  -> Ast.MuNil | Mutable  -> Ast.MuMutable
    method virtual_flag =
      function | Concrete  -> Ast.ViNil | Virtual  -> Ast.ViVirtual
    method rec_flag =
      function
      | Nonrecursive  -> Ast.ReNil
      | Recursive |Default  -> Ast.ReRecursive
    method direction_flag =
      function | Upto  -> Ast.DiTo | Downto  -> Ast.DiDownto
    method private_flag =
      function | Public  -> Ast.PrNil | Private  -> Ast.PrPrivate
    method core_type { ptyp_desc = ty; ptyp_loc = _loc } =
      match ty with
      | Ptyp_any  -> Ast.TyAny _loc
      | Ptyp_var s -> Ast.TyId (_loc, (Ast.IdLid (_loc, s)))
      | Ptyp_arrow (label,t1,t2) ->
          (match label with
           | "" -> Ast.TyArr (_loc, (self#core_type t1), (self#core_type t2))
           | s ->
               if (s.[0]) = '?'
               then
                 Ast.TyArr
                   (_loc, (Ast.TyOlb (_loc, label, (self#core_type t1))),
                     (self#core_type t2))
               else
                 Ast.TyArr
                   (_loc, (Ast.TyLab (_loc, label, (self#core_type t1))),
                     (self#core_type t2)))
      | Ptyp_tuple (x::xs) ->
          Ast.TyTup
            (_loc,
              (Ast.TySta
                 (_loc, (self#core_type x),
                   (Ast.tySta_of_list (List.map self#core_type xs)))))
      | Ptyp_tuple [] -> assert false
      | Ptyp_alias (ty,s) ->
          Ast.TyAli
            (_loc, (self#core_type ty),
              (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))))
      | Ptyp_variant (rfs,closed,labels) ->
          let ls =
            List.map
              (function
               | Rinherit t -> self#core_type t
               | Rtag (label,_b,[]) -> Ast.TyVrn (_loc, label)
               | Rtag (label,_b,ls) ->
                   Ast.TyOfAmp
                     (_loc, (Ast.TyVrn (_loc, label)),
                       (Ast.tyAmp_of_list (List.map self#core_type ls)))) rfs in
          (match (closed, labels) with
           | (true ,None ) -> Ast.TyVrnEq (_loc, (Ast.tyOr_of_list ls))
           | (true ,Some x) ->
               let u =
                 (List.map (fun x  -> Ast.TyVrn (_loc, x)) x) |>
                   Camlp4Ast.tyApp_of_list in
               Ast.TyVrnInfSup (_loc, (Ast.tyOr_of_list ls), u)
           | (false ,_) -> Ast.TyVrnSup (_loc, (Ast.tyOr_of_list ls)))
      | Ptyp_constr (lid_loc,ts) ->
          Camlp4Ast.tyApp_of_list
            ((Ast.TyId (_loc, (self#longident_loc lid_loc))) ::
            (List.map self#core_type ts))
      | Ptyp_object cfs ->
          let row_var = ref false in
          let res =
            List.fold_left
              (fun acc  { pfield_loc = _loc; pfield_desc = cf }  ->
                 match cf with
                 | Pfield (lab,ty) ->
                     let t =
                       Ast.TyCol
                         (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, lab)))),
                           (self#core_type ty)) in
                     Ast.TySem (_loc, acc, t)
                 | Pfield_var  -> acc) (Ast.TyNil _loc) cfs in
          if row_var.contents
          then Ast.TyObj (_loc, res, Ast.RvRowVar)
          else Ast.TyObj (_loc, res, Ast.RvNil)
      | Ptyp_class (lid_loc,cts,_lows) ->
          Ast.tyApp_of_list ((Ast.TyCls (_loc, (self#longident_loc lid_loc)))
            :: (List.map self#core_type cts))
      | Ptyp_poly (ls,ty) ->
          Ast.TyPol
            (_loc, (Ast.tyVarApp_of_list (_loc, ls)), (self#core_type ty))
      | Ptyp_package (lid,ls) ->
          let with_constrs =
            List.map
              (fun (lid,ty)  ->
                 Ast.WcTyp
                   (_loc, (Ast.TyId (_loc, (self#longident_loc lid))),
                     (self#core_type ty))) ls in
          Ast.TyPkg
            (_loc,
              (Ast.MtWit
                 (_loc, (Ast.MtId (_loc, (self#longident_loc lid))),
                   (Ast.wcAnd_of_list with_constrs))))
    method pattern { ppat_desc = x; ppat_loc = _loc } =
      match x with
      | Ppat_any  -> Ast.PaAny _loc
      | Ppat_var { txt;_} -> Ast.PaId (_loc, (Ast.IdLid (_loc, txt)))
      | Ppat_alias (p,{ txt;_}) ->
          Ast.PaAli
            (_loc, (self#pattern p),
              (Ast.PaId (_loc, (Ast.IdLid (_loc, txt)))))
      | Ppat_constant c -> self#constant_patt _loc c
      | Ppat_tuple [] -> assert false
      | Ppat_tuple (x::xs) ->
          Ast.PaTup
            (_loc,
              (Ast.PaCom
                 (_loc, (self#pattern x),
                   (Ast.paCom_of_list (List.map self#pattern xs)))))
      | Ppat_construct (lid_loc,opt,_b) ->
          (match opt with
           | None  -> Ast.PaId (_loc, (self#longident_loc lid_loc))
           | Some x ->
               Ast.PaApp
                 (_loc, (Ast.PaId (_loc, (self#longident_loc lid_loc))),
                   (self#pattern x)))
      | Ppat_variant (label,opt) ->
          (match opt with
           | Some o ->
               Ast.PaApp (_loc, (Ast.PaVrn (_loc, label)), (self#pattern o))
           | None  -> Ast.PaVrn (_loc, label))
      | Ppat_record (lst,closed) ->
          let ls =
            List.map
              (fun (lid_loc,p)  ->
                 Ast.PaEq
                   (_loc, (self#longident_loc lid_loc), (self#pattern p)))
              lst in
          (match closed with
           | Closed  -> Ast.PaRec (_loc, (Ast.paSem_of_list ls))
           | Open  ->
               Ast.PaRec
                 (_loc,
                   (Ast.PaSem
                      (_loc, (Ast.paSem_of_list ls), (Ast.PaAny _loc)))))
      | Ppat_array ls ->
          let ls = List.map self#pattern ls in
          Ast.PaArr (_loc, (Ast.paSem_of_list ls))
      | Ppat_or (p1,p2) ->
          Ast.PaOrp (_loc, (self#pattern p1), (self#pattern p2))
      | Ppat_constraint (p1,ty) ->
          Ast.PaTyc (_loc, (self#pattern p1), (self#core_type ty))
      | Ppat_type lid_loc -> Ast.PaTyp (_loc, (self#longident_loc lid_loc))
      | Ppat_lazy p -> Ast.PaLaz (_loc, (self#pattern p))
      | Ppat_unpack { txt;_} -> Ast.PaMod (_loc, txt)
    method expr { pexp_desc = x; pexp_loc = _loc } =
      match x with
      | Pexp_ident lid_loc -> Ast.ExId (_loc, (self#longident_loc lid_loc))
      | Pexp_constant c -> self#constant_expr _loc c
      | Pexp_let (recf,lst,e) ->
          let recf = self#rec_flag recf in
          let bindings =
            List.map
              (fun (p,e)  -> Ast.BiEq (_loc, (self#pattern p), (self#expr e)))
              lst in
          Ast.ExLet (_loc, recf, (Ast.biAnd_of_list bindings), (self#expr e))
      | Pexp_function (label,eo,lst) ->
          (match label with
           | "" ->
               let cases = self#gen_cases _loc lst in
               Ast.ExFun (_loc, (Ast.mcOr_of_list cases))
           | _ ->
               (match lst with
                | (p,e)::[] ->
                    if (label.[0]) = '?'
                    then
                      (match eo with
                       | Some o ->
                           Ast.ExFun
                             (_loc,
                               (Ast.McArr
                                  (_loc,
                                    (Ast.PaOlbi
                                       (_loc, label, (self#pattern p),
                                         (self#expr o))), (Ast.ExNil _loc),
                                    (self#expr e))))
                       | None  ->
                           Ast.ExFun
                             (_loc,
                               (Ast.McArr
                                  (_loc,
                                    (Ast.PaOlb
                                       (_loc, label, (Ast.PaNil _loc))),
                                    (Ast.ExNil _loc), (self#expr e)))))
                    else
                      Ast.ExFun
                        (_loc,
                          (Ast.McArr
                             (_loc,
                               (Ast.PaLab (_loc, label, (Ast.PaNil _loc))),
                               (Ast.ExNil _loc), (self#expr e))))
                | _ -> assert false))
      | Pexp_apply (e,lst) ->
          let args =
            List.map
              (fun (label,e)  ->
                 let v = self#expr e in
                 if label = "" then v else Ast.ExLab (_loc, label, v)) lst in
          Camlp4Ast.exApp_of_list ((self#expr e) :: args)
      | Pexp_match (e,lst) ->
          let cases = self#gen_cases _loc lst in
          Ast.ExMat (_loc, (self#expr e), (Ast.mcOr_of_list cases))
      | Pexp_try (e,lst) ->
          let cases = self#gen_cases _loc lst in
          Ast.ExTry (_loc, (self#expr e), (Ast.mcOr_of_list cases))
      | Pexp_tuple [] -> assert false
      | Pexp_tuple (x::xs) ->
          Ast.ExTup
            (_loc,
              (Ast.ExCom
                 (_loc, (self#expr x),
                   (Ast.exCom_of_list (List.map self#expr xs)))))
      | Pexp_construct (lid_loc,eo,_) ->
          (match eo with
           | None  -> Ast.ExId (_loc, (self#longident_loc lid_loc))
           | Some v ->
               Ast.ExApp
                 (_loc, (Ast.ExId (_loc, (self#longident_loc lid_loc))),
                   (self#expr v)))
      | Pexp_variant (label,eo) ->
          (match eo with
           | Some e ->
               Ast.ExApp (_loc, (Ast.ExVrn (_loc, label)), (self#expr e))
           | None  -> Ast.ExVrn (_loc, label))
      | Pexp_record (lst,eo) ->
          let bindings =
            List.map
              (fun (lid,e)  ->
                 Ast.RbEq (_loc, (self#longident_loc lid), (self#expr e)))
              lst in
          (match eo with
           | Some e ->
               Ast.ExRec (_loc, (Ast.rbSem_of_list bindings), (self#expr e))
           | None  ->
               Ast.ExRec
                 (_loc, (Ast.rbSem_of_list bindings), (Ast.ExNil _loc)))
      | Pexp_field (e,lid_loc) ->
          Ast.ExAcc
            (_loc, (self#expr e),
              (Ast.ExId (_loc, (self#longident_loc lid_loc))))
      | Pexp_setfield (e1,lid_loc,e2) ->
          Ast.ExAss
            (_loc,
              (Ast.ExAcc
                 (_loc, (self#expr e1),
                   (Ast.ExId (_loc, (self#longident_loc lid_loc))))),
              (self#expr e2))
      | Pexp_array lst ->
          Ast.ExArr (_loc, (Ast.exSem_of_list (List.map self#expr lst)))
      | Pexp_ifthenelse (e1,e2,e3) ->
          (match e3 with
           | Some e3 ->
               Ast.ExIfe
                 (_loc, (self#expr e1), (self#expr e2), (self#expr e3))
           | None  ->
               Ast.ExIfe
                 (_loc, (self#expr e1), (self#expr e2),
                   (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))))))
      | Pexp_sequence (e1,e2) ->
          Ast.ExApp (_loc, (self#expr e1), (self#expr e2))
      | Pexp_while (e1,e2) ->
          Ast.ExWhi (_loc, (self#expr e1), (self#expr e2))
      | Pexp_for ({ txt;_},e1,e2,df,e3) ->
          Ast.ExFor
            (_loc, txt, (self#expr e1), (self#expr e2),
              (self#direction_flag df), (self#expr e3))
      | Pexp_constraint (e1,ot1,ot2) ->
          (match (ot1, ot2) with
           | (None ,None ) -> self#expr e1
           | (Some t1,Some t2) ->
               Ast.ExCoe
                 (_loc, (self#expr e1), (self#core_type t1),
                   (self#core_type t2))
           | (Some t1,None ) ->
               Ast.ExTyc (_loc, (self#expr e1), (self#core_type t1))
           | (None ,Some t2) ->
               Ast.ExCoe
                 (_loc, (self#expr e1), (Ast.TyNil _loc),
                   (self#core_type t2)))
      | Pexp_when _ -> assert false
      | Pexp_send (e,txt) -> Ast.ExSnd (_loc, (self#expr e), txt)
      | Pexp_new lid_loc -> Ast.ExNew (_loc, (self#longident_loc lid_loc))
      | Pexp_setinstvar ({ txt;_},e) ->
          Ast.ExAss
            (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, txt)))), (self#expr e))
      | Pexp_override lst ->
          let lst =
            List.map
              (fun ({ txt;_},e)  ->
                 Ast.RbEq (_loc, (Ast.IdLid (_loc, txt)), (self#expr e))) lst in
          Ast.ExOvr (_loc, (Ast.rbSem_of_list lst))
      | Pexp_letmodule ({ txt;_},me,e) ->
          Ast.ExLmd (_loc, txt, (self#module_expr me), (self#expr e))
      | Pexp_assert e -> Ast.ExAsr (_loc, (self#expr e))
      | Pexp_assertfalse  -> Ast.ExAsf _loc
      | Pexp_lazy e -> Ast.ExLaz (_loc, (self#expr e))
      | Pexp_poly _ -> assert false
      | Pexp_object e -> assert false
      | Pexp_newtype _ -> assert false
      | Pexp_pack me -> assert false
      | Pexp_open (lid_loc,e) ->
          Ast.ExOpI (_loc, (self#longident_loc lid_loc), (self#expr e))
    method module_expr { pmod_desc = x; pmod_loc = _loc } =
      (assert false : Ast.module_expr )
    method structure_item { pstr_desc = x; pstr_loc = _loc } =
      (assert false : Ast.str_item )
    method structure (ls : structure) = (assert false : Ast.str_item )
    method signature (ls : signature) = (assert false : Ast.sig_item )
    method signature_item { psig_desc = x; psig_loc = _loc } =
      (raise Not_found : Ast.sig_item )
  end