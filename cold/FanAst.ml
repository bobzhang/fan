open FanUtil
include Ast
module type META_LOC =
  sig
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> Ast.patt
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> Ast.expr
  end
open StdLib
let _ = ()
class map =
  object (self : 'self_type)
    inherit  mapbase
    method class_str_item : class_str_item -> class_str_item=
      function
      | CrNil a0 -> CrNil (self#loc a0)
      | CrSem (a0,a1,a2) ->
          CrSem
            ((self#loc a0), (self#class_str_item a1),
              (self#class_str_item a2))
      | CrCtr (a0,a1,a2) ->
          CrCtr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | CrInh (a0,a1,a2,a3) ->
          CrInh
            ((self#loc a0), (self#override_flag a1), (self#class_expr a2),
              (self#string a3))
      | CrIni (a0,a1) -> CrIni ((self#loc a0), (self#expr a1))
      | CrMth (a0,a1,a2,a3,a4,a5) ->
          CrMth
            ((self#loc a0), (self#string a1), (self#override_flag a2),
              (self#private_flag a3), (self#expr a4), (self#ctyp a5))
      | CrVal (a0,a1,a2,a3,a4) ->
          CrVal
            ((self#loc a0), (self#string a1), (self#override_flag a2),
              (self#mutable_flag a3), (self#expr a4))
      | CrVir (a0,a1,a2,a3) ->
          CrVir
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | CrVvr (a0,a1,a2,a3) ->
          CrVvr
            ((self#loc a0), (self#string a1), (self#mutable_flag a2),
              (self#ctyp a3))
      | CrAnt (a0,a1) -> CrAnt ((self#loc a0), (self#string a1))
    method class_expr : class_expr -> class_expr=
      function
      | CeNil a0 -> CeNil (self#loc a0)
      | CeApp (a0,a1,a2) ->
          CeApp ((self#loc a0), (self#class_expr a1), (self#expr a2))
      | CeCon (a0,a1,a2,a3) ->
          CeCon
            ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
              (self#ctyp a3))
      | CeFun (a0,a1,a2) ->
          CeFun ((self#loc a0), (self#patt a1), (self#class_expr a2))
      | CeLet (a0,a1,a2,a3) ->
          CeLet
            ((self#loc a0), (self#rec_flag a1), (self#binding a2),
              (self#class_expr a3))
      | CeStr (a0,a1,a2) ->
          CeStr ((self#loc a0), (self#patt a1), (self#class_str_item a2))
      | CeTyc (a0,a1,a2) ->
          CeTyc ((self#loc a0), (self#class_expr a1), (self#class_type a2))
      | CeAnd (a0,a1,a2) ->
          CeAnd ((self#loc a0), (self#class_expr a1), (self#class_expr a2))
      | CeEq (a0,a1,a2) ->
          CeEq ((self#loc a0), (self#class_expr a1), (self#class_expr a2))
      | CeAnt (a0,a1) -> CeAnt ((self#loc a0), (self#string a1))
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | CgNil a0 -> CgNil (self#loc a0)
      | CgCtr (a0,a1,a2) ->
          CgCtr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | CgSem (a0,a1,a2) ->
          CgSem
            ((self#loc a0), (self#class_sig_item a1),
              (self#class_sig_item a2))
      | CgInh (a0,a1) -> CgInh ((self#loc a0), (self#class_type a1))
      | CgMth (a0,a1,a2,a3) ->
          CgMth
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | CgVal (a0,a1,a2,a3,a4) ->
          CgVal
            ((self#loc a0), (self#string a1), (self#mutable_flag a2),
              (self#virtual_flag a3), (self#ctyp a4))
      | CgVir (a0,a1,a2,a3) ->
          CgVir
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | CgAnt (a0,a1) -> CgAnt ((self#loc a0), (self#string a1))
    method class_type : class_type -> class_type=
      function
      | CtNil a0 -> CtNil (self#loc a0)
      | CtCon (a0,a1,a2,a3) ->
          CtCon
            ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
              (self#ctyp a3))
      | CtFun (a0,a1,a2) ->
          CtFun ((self#loc a0), (self#ctyp a1), (self#class_type a2))
      | CtSig (a0,a1,a2) ->
          CtSig ((self#loc a0), (self#ctyp a1), (self#class_sig_item a2))
      | CtAnd (a0,a1,a2) ->
          CtAnd ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | CtCol (a0,a1,a2) ->
          CtCol ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | CtEq (a0,a1,a2) ->
          CtEq ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | CtAnt (a0,a1) -> CtAnt ((self#loc a0), (self#string a1))
    method str_item : str_item -> str_item=
      function
      | StNil a0 -> StNil (self#loc a0)
      | StCls (a0,a1) -> StCls ((self#loc a0), (self#class_expr a1))
      | StClt (a0,a1) -> StClt ((self#loc a0), (self#class_type a1))
      | StSem (a0,a1,a2) ->
          StSem ((self#loc a0), (self#str_item a1), (self#str_item a2))
      | StDir (a0,a1,a2) ->
          StDir ((self#loc a0), (self#string a1), (self#expr a2))
      | StExc (a0,a1,a2) ->
          StExc
            ((self#loc a0), (self#ctyp a1),
              (self#meta_option (fun self  -> self#ident) a2))
      | StExp (a0,a1) -> StExp ((self#loc a0), (self#expr a1))
      | StExt (a0,a1,a2,a3) ->
          StExt
            ((self#loc a0), (self#string a1), (self#ctyp a2),
              (self#meta_list (fun self  -> self#string) a3))
      | StInc (a0,a1) -> StInc ((self#loc a0), (self#module_expr a1))
      | StMod (a0,a1,a2) ->
          StMod ((self#loc a0), (self#string a1), (self#module_expr a2))
      | StRecMod (a0,a1) ->
          StRecMod ((self#loc a0), (self#module_binding a1))
      | StMty (a0,a1,a2) ->
          StMty ((self#loc a0), (self#string a1), (self#module_type a2))
      | StOpn (a0,a1) -> StOpn ((self#loc a0), (self#ident a1))
      | StTyp (a0,a1) -> StTyp ((self#loc a0), (self#ctyp a1))
      | StVal (a0,a1,a2) ->
          StVal ((self#loc a0), (self#rec_flag a1), (self#binding a2))
      | StAnt (a0,a1) -> StAnt ((self#loc a0), (self#string a1))
    method module_expr : module_expr -> module_expr=
      function
      | MeNil a0 -> MeNil (self#loc a0)
      | MeId (a0,a1) -> MeId ((self#loc a0), (self#ident a1))
      | MeApp (a0,a1,a2) ->
          MeApp ((self#loc a0), (self#module_expr a1), (self#module_expr a2))
      | MeFun (a0,a1,a2,a3) ->
          MeFun
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_expr a3))
      | MeStr (a0,a1) -> MeStr ((self#loc a0), (self#str_item a1))
      | MeTyc (a0,a1,a2) ->
          MeTyc ((self#loc a0), (self#module_expr a1), (self#module_type a2))
      | MePkg (a0,a1) -> MePkg ((self#loc a0), (self#expr a1))
      | MeAnt (a0,a1) -> MeAnt ((self#loc a0), (self#string a1))
    method match_case : match_case -> match_case=
      function
      | McNil a0 -> McNil (self#loc a0)
      | McOr (a0,a1,a2) ->
          McOr ((self#loc a0), (self#match_case a1), (self#match_case a2))
      | McArr (a0,a1,a2,a3) ->
          McArr
            ((self#loc a0), (self#patt a1), (self#expr a2), (self#expr a3))
      | McAnt (a0,a1) -> McAnt ((self#loc a0), (self#string a1))
    method module_binding : module_binding -> module_binding=
      function
      | MbNil a0 -> MbNil (self#loc a0)
      | MbAnd (a0,a1,a2) ->
          MbAnd
            ((self#loc a0), (self#module_binding a1),
              (self#module_binding a2))
      | MbColEq (a0,a1,a2,a3) ->
          MbColEq
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_expr a3))
      | MbCol (a0,a1,a2) ->
          MbCol ((self#loc a0), (self#string a1), (self#module_type a2))
      | MbAnt (a0,a1) -> MbAnt ((self#loc a0), (self#string a1))
    method rec_binding : rec_binding -> rec_binding=
      function
      | RbNil a0 -> RbNil (self#loc a0)
      | RbSem (a0,a1,a2) ->
          RbSem ((self#loc a0), (self#rec_binding a1), (self#rec_binding a2))
      | RbEq (a0,a1,a2) ->
          RbEq ((self#loc a0), (self#ident a1), (self#expr a2))
      | RbAnt (a0,a1) -> RbAnt ((self#loc a0), (self#string a1))
    method binding : binding -> binding=
      function
      | BiNil a0 -> BiNil (self#loc a0)
      | BiAnd (a0,a1,a2) ->
          BiAnd ((self#loc a0), (self#binding a1), (self#binding a2))
      | BiEq (a0,a1,a2) ->
          BiEq ((self#loc a0), (self#patt a1), (self#expr a2))
      | BiAnt (a0,a1) -> BiAnt ((self#loc a0), (self#string a1))
    method with_constr : with_constr -> with_constr=
      function
      | WcNil a0 -> WcNil (self#loc a0)
      | WcTyp (a0,a1,a2) ->
          WcTyp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | WcMod (a0,a1,a2) ->
          WcMod ((self#loc a0), (self#ident a1), (self#ident a2))
      | WcTyS (a0,a1,a2) ->
          WcTyS ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | WcMoS (a0,a1,a2) ->
          WcMoS ((self#loc a0), (self#ident a1), (self#ident a2))
      | WcAnd (a0,a1,a2) ->
          WcAnd ((self#loc a0), (self#with_constr a1), (self#with_constr a2))
      | WcAnt (a0,a1) -> WcAnt ((self#loc a0), (self#string a1))
    method sig_item : sig_item -> sig_item=
      function
      | SgNil a0 -> SgNil (self#loc a0)
      | SgCls (a0,a1) -> SgCls ((self#loc a0), (self#class_type a1))
      | SgClt (a0,a1) -> SgClt ((self#loc a0), (self#class_type a1))
      | SgSem (a0,a1,a2) ->
          SgSem ((self#loc a0), (self#sig_item a1), (self#sig_item a2))
      | SgDir (a0,a1,a2) ->
          SgDir ((self#loc a0), (self#string a1), (self#expr a2))
      | SgExc (a0,a1) -> SgExc ((self#loc a0), (self#ctyp a1))
      | SgExt (a0,a1,a2,a3) ->
          SgExt
            ((self#loc a0), (self#string a1), (self#ctyp a2),
              (self#meta_list (fun self  -> self#string) a3))
      | SgInc (a0,a1) -> SgInc ((self#loc a0), (self#module_type a1))
      | SgMod (a0,a1,a2) ->
          SgMod ((self#loc a0), (self#string a1), (self#module_type a2))
      | SgRecMod (a0,a1) ->
          SgRecMod ((self#loc a0), (self#module_binding a1))
      | SgMty (a0,a1,a2) ->
          SgMty ((self#loc a0), (self#string a1), (self#module_type a2))
      | SgOpn (a0,a1) -> SgOpn ((self#loc a0), (self#ident a1))
      | SgTyp (a0,a1) -> SgTyp ((self#loc a0), (self#ctyp a1))
      | SgVal (a0,a1,a2) ->
          SgVal ((self#loc a0), (self#string a1), (self#ctyp a2))
      | SgAnt (a0,a1) -> SgAnt ((self#loc a0), (self#string a1))
    method module_type : module_type -> module_type=
      function
      | MtNil a0 -> MtNil (self#loc a0)
      | MtId (a0,a1) -> MtId ((self#loc a0), (self#ident a1))
      | MtFun (a0,a1,a2,a3) ->
          MtFun
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_type a3))
      | MtQuo (a0,a1) -> MtQuo ((self#loc a0), (self#string a1))
      | MtSig (a0,a1) -> MtSig ((self#loc a0), (self#sig_item a1))
      | MtWit (a0,a1,a2) ->
          MtWit ((self#loc a0), (self#module_type a1), (self#with_constr a2))
      | MtOf (a0,a1) -> MtOf ((self#loc a0), (self#module_expr a1))
      | MtAnt (a0,a1) -> MtAnt ((self#loc a0), (self#string a1))
    method expr : expr -> expr=
      function
      | ExNil a0 -> ExNil (self#loc a0)
      | ExId (a0,a1) -> ExId ((self#loc a0), (self#ident a1))
      | ExAcc (a0,a1,a2) ->
          ExAcc ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExAnt (a0,a1) -> ExAnt ((self#loc a0), (self#string a1))
      | ExApp (a0,a1,a2) ->
          ExApp ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExAre (a0,a1,a2) ->
          ExAre ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExArr (a0,a1) -> ExArr ((self#loc a0), (self#expr a1))
      | ExSem (a0,a1,a2) ->
          ExSem ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExAsf a0 -> ExAsf (self#loc a0)
      | ExAsr (a0,a1) -> ExAsr ((self#loc a0), (self#expr a1))
      | ExAss (a0,a1,a2) ->
          ExAss ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExChr (a0,a1) -> ExChr ((self#loc a0), (self#string a1))
      | ExCoe (a0,a1,a2,a3) ->
          ExCoe
            ((self#loc a0), (self#expr a1), (self#ctyp a2), (self#ctyp a3))
      | ExFlo (a0,a1) -> ExFlo ((self#loc a0), (self#string a1))
      | ExFor (a0,a1,a2,a3,a4,a5) ->
          ExFor
            ((self#loc a0), (self#string a1), (self#expr a2), (self#expr a3),
              (self#direction_flag a4), (self#expr a5))
      | ExFun (a0,a1) -> ExFun ((self#loc a0), (self#match_case a1))
      | ExIfe (a0,a1,a2,a3) ->
          ExIfe
            ((self#loc a0), (self#expr a1), (self#expr a2), (self#expr a3))
      | ExInt (a0,a1) -> ExInt ((self#loc a0), (self#string a1))
      | ExInt32 (a0,a1) -> ExInt32 ((self#loc a0), (self#string a1))
      | ExInt64 (a0,a1) -> ExInt64 ((self#loc a0), (self#string a1))
      | ExNativeInt (a0,a1) -> ExNativeInt ((self#loc a0), (self#string a1))
      | ExLab (a0,a1,a2) ->
          ExLab ((self#loc a0), (self#string a1), (self#expr a2))
      | ExLaz (a0,a1) -> ExLaz ((self#loc a0), (self#expr a1))
      | ExLet (a0,a1,a2,a3) ->
          ExLet
            ((self#loc a0), (self#rec_flag a1), (self#binding a2),
              (self#expr a3))
      | ExLmd (a0,a1,a2,a3) ->
          ExLmd
            ((self#loc a0), (self#string a1), (self#module_expr a2),
              (self#expr a3))
      | ExMat (a0,a1,a2) ->
          ExMat ((self#loc a0), (self#expr a1), (self#match_case a2))
      | ExNew (a0,a1) -> ExNew ((self#loc a0), (self#ident a1))
      | ExObj (a0,a1,a2) ->
          ExObj ((self#loc a0), (self#patt a1), (self#class_str_item a2))
      | ExOlb (a0,a1,a2) ->
          ExOlb ((self#loc a0), (self#string a1), (self#expr a2))
      | ExOvr (a0,a1) -> ExOvr ((self#loc a0), (self#rec_binding a1))
      | ExRec (a0,a1,a2) ->
          ExRec ((self#loc a0), (self#rec_binding a1), (self#expr a2))
      | ExSeq (a0,a1) -> ExSeq ((self#loc a0), (self#expr a1))
      | ExSnd (a0,a1,a2) ->
          ExSnd ((self#loc a0), (self#expr a1), (self#string a2))
      | ExSte (a0,a1,a2) ->
          ExSte ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExStr (a0,a1) -> ExStr ((self#loc a0), (self#string a1))
      | ExTry (a0,a1,a2) ->
          ExTry ((self#loc a0), (self#expr a1), (self#match_case a2))
      | ExTup (a0,a1) -> ExTup ((self#loc a0), (self#expr a1))
      | ExCom (a0,a1,a2) ->
          ExCom ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExTyc (a0,a1,a2) ->
          ExTyc ((self#loc a0), (self#expr a1), (self#ctyp a2))
      | ExVrn (a0,a1) -> ExVrn ((self#loc a0), (self#string a1))
      | ExWhi (a0,a1,a2) ->
          ExWhi ((self#loc a0), (self#expr a1), (self#expr a2))
      | ExOpI (a0,a1,a2) ->
          ExOpI ((self#loc a0), (self#ident a1), (self#expr a2))
      | ExFUN (a0,a1,a2) ->
          ExFUN ((self#loc a0), (self#string a1), (self#expr a2))
      | ExPkg (a0,a1) -> ExPkg ((self#loc a0), (self#module_expr a1))
    method patt : patt -> patt=
      function
      | PaNil a0 -> PaNil (self#loc a0)
      | PaId (a0,a1) -> PaId ((self#loc a0), (self#ident a1))
      | PaAli (a0,a1,a2) ->
          PaAli ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaAnt (a0,a1) -> PaAnt ((self#loc a0), (self#string a1))
      | PaAny a0 -> PaAny (self#loc a0)
      | PaApp (a0,a1,a2) ->
          PaApp ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaArr (a0,a1) -> PaArr ((self#loc a0), (self#patt a1))
      | PaCom (a0,a1,a2) ->
          PaCom ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaSem (a0,a1,a2) ->
          PaSem ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaChr (a0,a1) -> PaChr ((self#loc a0), (self#string a1))
      | PaInt (a0,a1) -> PaInt ((self#loc a0), (self#string a1))
      | PaInt32 (a0,a1) -> PaInt32 ((self#loc a0), (self#string a1))
      | PaInt64 (a0,a1) -> PaInt64 ((self#loc a0), (self#string a1))
      | PaNativeInt (a0,a1) -> PaNativeInt ((self#loc a0), (self#string a1))
      | PaFlo (a0,a1) -> PaFlo ((self#loc a0), (self#string a1))
      | PaLab (a0,a1,a2) ->
          PaLab ((self#loc a0), (self#string a1), (self#patt a2))
      | PaOlb (a0,a1,a2) ->
          PaOlb ((self#loc a0), (self#string a1), (self#patt a2))
      | PaOlbi (a0,a1,a2,a3) ->
          PaOlbi
            ((self#loc a0), (self#string a1), (self#patt a2), (self#expr a3))
      | PaOrp (a0,a1,a2) ->
          PaOrp ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaRng (a0,a1,a2) ->
          PaRng ((self#loc a0), (self#patt a1), (self#patt a2))
      | PaRec (a0,a1) -> PaRec ((self#loc a0), (self#patt a1))
      | PaEq (a0,a1,a2) ->
          PaEq ((self#loc a0), (self#ident a1), (self#patt a2))
      | PaStr (a0,a1) -> PaStr ((self#loc a0), (self#string a1))
      | PaTup (a0,a1) -> PaTup ((self#loc a0), (self#patt a1))
      | PaTyc (a0,a1,a2) ->
          PaTyc ((self#loc a0), (self#patt a1), (self#ctyp a2))
      | PaTyp (a0,a1) -> PaTyp ((self#loc a0), (self#ident a1))
      | PaVrn (a0,a1) -> PaVrn ((self#loc a0), (self#string a1))
      | PaLaz (a0,a1) -> PaLaz ((self#loc a0), (self#patt a1))
      | PaMod (a0,a1) -> PaMod ((self#loc a0), (self#string a1))
    method ctyp : ctyp -> ctyp=
      function
      | TyNil a0 -> TyNil (self#loc a0)
      | TyAli (a0,a1,a2) ->
          TyAli ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyAny a0 -> TyAny (self#loc a0)
      | TyApp (a0,a1,a2) ->
          TyApp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyArr (a0,a1,a2) ->
          TyArr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyCls (a0,a1) -> TyCls ((self#loc a0), (self#ident a1))
      | TyLab (a0,a1,a2) ->
          TyLab ((self#loc a0), (self#string a1), (self#ctyp a2))
      | TyId (a0,a1) -> TyId ((self#loc a0), (self#ident a1))
      | TyMan (a0,a1,a2) ->
          TyMan ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyDcl (a0,a1,a2,a3,a4) ->
          TyDcl
            ((self#loc a0), (self#string a1),
              (self#list (fun self  -> self#ctyp) a2), (self#ctyp a3),
              (self#list
                 (fun self  (a0,a1)  -> ((self#ctyp a0), (self#ctyp a1))) a4))
      | TyObj (a0,a1,a2) ->
          TyObj ((self#loc a0), (self#ctyp a1), (self#row_var_flag a2))
      | TyOlb (a0,a1,a2) ->
          TyOlb ((self#loc a0), (self#string a1), (self#ctyp a2))
      | TyPol (a0,a1,a2) ->
          TyPol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyTypePol (a0,a1,a2) ->
          TyTypePol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyQuo (a0,a1) -> TyQuo ((self#loc a0), (self#string a1))
      | TyQuP (a0,a1) -> TyQuP ((self#loc a0), (self#string a1))
      | TyQuM (a0,a1) -> TyQuM ((self#loc a0), (self#string a1))
      | TyAnP a0 -> TyAnP (self#loc a0)
      | TyAnM a0 -> TyAnM (self#loc a0)
      | TyVrn (a0,a1) -> TyVrn ((self#loc a0), (self#string a1))
      | TyRec (a0,a1) -> TyRec ((self#loc a0), (self#ctyp a1))
      | TyCol (a0,a1,a2) ->
          TyCol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TySem (a0,a1,a2) ->
          TySem ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyCom (a0,a1,a2) ->
          TyCom ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TySum (a0,a1) -> TySum ((self#loc a0), (self#ctyp a1))
      | TyOf (a0,a1,a2) ->
          TyOf ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyAnd (a0,a1,a2) ->
          TyAnd ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyOr (a0,a1,a2) ->
          TyOr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyPrv (a0,a1) -> TyPrv ((self#loc a0), (self#ctyp a1))
      | TyMut (a0,a1) -> TyMut ((self#loc a0), (self#ctyp a1))
      | TyTup (a0,a1) -> TyTup ((self#loc a0), (self#ctyp a1))
      | TySta (a0,a1,a2) ->
          TySta ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyVrnEq (a0,a1) -> TyVrnEq ((self#loc a0), (self#ctyp a1))
      | TyVrnSup (a0,a1) -> TyVrnSup ((self#loc a0), (self#ctyp a1))
      | TyVrnInf (a0,a1) -> TyVrnInf ((self#loc a0), (self#ctyp a1))
      | TyVrnInfSup (a0,a1,a2) ->
          TyVrnInfSup ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyAmp (a0,a1,a2) ->
          TyAmp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyOfAmp (a0,a1,a2) ->
          TyOfAmp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | TyPkg (a0,a1) -> TyPkg ((self#loc a0), (self#module_type a1))
      | TyAnt (a0,a1) -> TyAnt ((self#loc a0), (self#string a1))
    method ident : ident -> ident=
      function
      | IdAcc (a0,a1,a2) ->
          IdAcc ((self#loc a0), (self#ident a1), (self#ident a2))
      | IdApp (a0,a1,a2) ->
          IdApp ((self#loc a0), (self#ident a1), (self#ident a2))
      | IdLid (a0,a1) -> IdLid ((self#loc a0), (self#string a1))
      | IdUid (a0,a1) -> IdUid ((self#loc a0), (self#string a1))
      | IdAnt (a0,a1) -> IdAnt ((self#loc a0), (self#string a1))
    method meta_list :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_list -> 'all_b0 meta_list=
      fun mf_a  ->
        function
        | LNil  -> LNil
        | LCons (a0,a1) -> LCons ((mf_a self a0), (self#meta_list mf_a a1))
        | LAnt a0 -> LAnt (self#string a0)
    method meta_option :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_option -> 'all_b0 meta_option=
      fun mf_a  ->
        function
        | ONone  -> ONone
        | OSome a0 -> OSome (mf_a self a0)
        | OAnt a0 -> OAnt (self#string a0)
    method row_var_flag : row_var_flag -> row_var_flag=
      function
      | RvRowVar  -> RvRowVar
      | RvNil  -> RvNil
      | RvAnt a0 -> RvAnt (self#string a0)
    method override_flag : override_flag -> override_flag=
      function
      | OvOverride  -> OvOverride
      | OvNil  -> OvNil
      | OvAnt a0 -> OvAnt (self#string a0)
    method virtual_flag : virtual_flag -> virtual_flag=
      function
      | ViVirtual  -> ViVirtual
      | ViNil  -> ViNil
      | ViAnt a0 -> ViAnt (self#string a0)
    method private_flag : private_flag -> private_flag=
      function
      | PrPrivate  -> PrPrivate
      | PrNil  -> PrNil
      | PrAnt a0 -> PrAnt (self#string a0)
    method mutable_flag : mutable_flag -> mutable_flag=
      function
      | MuMutable  -> MuMutable
      | MuNil  -> MuNil
      | MuAnt a0 -> MuAnt (self#string a0)
    method direction_flag : direction_flag -> direction_flag=
      function
      | DiTo  -> DiTo
      | DiDownto  -> DiDownto
      | DiAnt a0 -> DiAnt (self#string a0)
    method rec_flag : rec_flag -> rec_flag=
      function
      | ReRecursive  -> ReRecursive
      | ReNil  -> ReNil
      | ReAnt a0 -> ReAnt (self#string a0)
    method meta_bool : meta_bool -> meta_bool=
      function
      | BTrue  -> BTrue
      | BFalse  -> BFalse
      | BAnt a0 -> BAnt (self#string a0)
    method loc : loc -> loc= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method class_str_item : class_str_item -> 'self_type=
      function
      | CrNil a0 -> self#loc a0
      | CrSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_str_item a1 in self#class_str_item a2
      | CrCtr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | CrInh (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#override_flag a1 in
          let self = self#class_expr a2 in self#string a3
      | CrIni (a0,a1) -> let self = self#loc a0 in self#expr a1
      | CrMth (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#override_flag a2 in
          let self = self#private_flag a3 in
          let self = self#expr a4 in self#ctyp a5
      | CrVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#override_flag a2 in
          let self = self#mutable_flag a3 in self#expr a4
      | CrVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | CrVvr (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#mutable_flag a2 in self#ctyp a3
      | CrAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_expr : class_expr -> 'self_type=
      function
      | CeNil a0 -> self#loc a0
      | CeApp (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#expr a2
      | CeCon (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#virtual_flag a1 in
          let self = self#ident a2 in self#ctyp a3
      | CeFun (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_expr a2
      | CeLet (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in
          let self = self#binding a2 in self#class_expr a3
      | CeStr (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | CeTyc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_type a2
      | CeAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | CeEq (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | CeAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | CgNil a0 -> self#loc a0
      | CgCtr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | CgSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_sig_item a1 in self#class_sig_item a2
      | CgInh (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | CgMth (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | CgVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#mutable_flag a2 in
          let self = self#virtual_flag a3 in self#ctyp a4
      | CgVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | CgAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_type : class_type -> 'self_type=
      function
      | CtNil a0 -> self#loc a0
      | CtCon (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#virtual_flag a1 in
          let self = self#ident a2 in self#ctyp a3
      | CtFun (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#class_type a2
      | CtSig (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#class_sig_item a2
      | CtAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | CtCol (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | CtEq (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_type a1 in self#class_type a2
      | CtAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method str_item : str_item -> 'self_type=
      function
      | StNil a0 -> self#loc a0
      | StCls (a0,a1) -> let self = self#loc a0 in self#class_expr a1
      | StClt (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | StSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#str_item a1 in self#str_item a2
      | StDir (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | StExc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in
          self#meta_option (fun self  -> self#ident) a2
      | StExp (a0,a1) -> let self = self#loc a0 in self#expr a1
      | StExt (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | StInc (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | StMod (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_expr a2
      | StRecMod (a0,a1) -> let self = self#loc a0 in self#module_binding a1
      | StMty (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | StOpn (a0,a1) -> let self = self#loc a0 in self#ident a1
      | StTyp (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | StVal (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in self#binding a2
      | StAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_expr : module_expr -> 'self_type=
      function
      | MeNil a0 -> self#loc a0
      | MeId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | MeApp (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_expr a2
      | MeFun (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_expr a3
      | MeStr (a0,a1) -> let self = self#loc a0 in self#str_item a1
      | MeTyc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_type a2
      | MePkg (a0,a1) -> let self = self#loc a0 in self#expr a1
      | MeAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method match_case : match_case -> 'self_type=
      function
      | McNil a0 -> self#loc a0
      | McOr (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#match_case a1 in self#match_case a2
      | McArr (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#patt a1 in let self = self#expr a2 in self#expr a3
      | McAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_binding : module_binding -> 'self_type=
      function
      | MbNil a0 -> self#loc a0
      | MbAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_binding a1 in self#module_binding a2
      | MbColEq (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_expr a3
      | MbCol (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | MbAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method rec_binding : rec_binding -> 'self_type=
      function
      | RbNil a0 -> self#loc a0
      | RbSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#rec_binding a2
      | RbEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | RbAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method binding : binding -> 'self_type=
      function
      | BiNil a0 -> self#loc a0
      | BiAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#binding a1 in self#binding a2
      | BiEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#expr a2
      | BiAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method with_constr : with_constr -> 'self_type=
      function
      | WcNil a0 -> self#loc a0
      | WcTyp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | WcMod (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | WcTyS (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | WcMoS (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | WcAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#with_constr a1 in self#with_constr a2
      | WcAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method sig_item : sig_item -> 'self_type=
      function
      | SgNil a0 -> self#loc a0
      | SgCls (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | SgClt (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | SgSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#sig_item a1 in self#sig_item a2
      | SgDir (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | SgExc (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | SgExt (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | SgInc (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | SgMod (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | SgRecMod (a0,a1) -> let self = self#loc a0 in self#module_binding a1
      | SgMty (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | SgOpn (a0,a1) -> let self = self#loc a0 in self#ident a1
      | SgTyp (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | SgVal (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | SgAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_type : module_type -> 'self_type=
      function
      | MtNil a0 -> self#loc a0
      | MtId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | MtFun (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_type a3
      | MtQuo (a0,a1) -> let self = self#loc a0 in self#string a1
      | MtSig (a0,a1) -> let self = self#loc a0 in self#sig_item a1
      | MtWit (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_type a1 in self#with_constr a2
      | MtOf (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | MtAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method expr : expr -> 'self_type=
      function
      | ExNil a0 -> self#loc a0
      | ExId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | ExAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExAnt (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExAre (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExArr (a0,a1) -> let self = self#loc a0 in self#expr a1
      | ExSem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExAsf a0 -> self#loc a0
      | ExAsr (a0,a1) -> let self = self#loc a0 in self#expr a1
      | ExAss (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExChr (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExCoe (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#ctyp a2 in self#ctyp a3
      | ExFlo (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExFor (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#expr a2 in
          let self = self#expr a3 in
          let self = self#direction_flag a4 in self#expr a5
      | ExFun (a0,a1) -> let self = self#loc a0 in self#match_case a1
      | ExIfe (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#expr a2 in self#expr a3
      | ExInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExInt32 (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExInt64 (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExNativeInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | ExLaz (a0,a1) -> let self = self#loc a0 in self#expr a1
      | ExLet (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in
          let self = self#binding a2 in self#expr a3
      | ExLmd (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_expr a2 in self#expr a3
      | ExMat (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | ExNew (a0,a1) -> let self = self#loc a0 in self#ident a1
      | ExObj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | ExOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | ExOvr (a0,a1) -> let self = self#loc a0 in self#rec_binding a1
      | ExRec (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#expr a2
      | ExSeq (a0,a1) -> let self = self#loc a0 in self#expr a1
      | ExSnd (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#string a2
      | ExSte (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExStr (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExTry (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | ExTup (a0,a1) -> let self = self#loc a0 in self#expr a1
      | ExCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExTyc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#ctyp a2
      | ExVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | ExWhi (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | ExOpI (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | ExFUN (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | ExPkg (a0,a1) -> let self = self#loc a0 in self#module_expr a1
    method patt : patt -> 'self_type=
      function
      | PaNil a0 -> self#loc a0
      | PaId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | PaAli (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaAnt (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaAny a0 -> self#loc a0
      | PaApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaArr (a0,a1) -> let self = self#loc a0 in self#patt a1
      | PaCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaSem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaChr (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaInt32 (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaInt64 (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaNativeInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaFlo (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#patt a2
      | PaOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#patt a2
      | PaOlbi (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#patt a2 in self#expr a3
      | PaOrp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaRng (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | PaRec (a0,a1) -> let self = self#loc a0 in self#patt a1
      | PaEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#patt a2
      | PaStr (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaTup (a0,a1) -> let self = self#loc a0 in self#patt a1
      | PaTyc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#ctyp a2
      | PaTyp (a0,a1) -> let self = self#loc a0 in self#ident a1
      | PaVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | PaLaz (a0,a1) -> let self = self#loc a0 in self#patt a1
      | PaMod (a0,a1) -> let self = self#loc a0 in self#string a1
    method ctyp : ctyp -> 'self_type=
      function
      | TyNil a0 -> self#loc a0
      | TyAli (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyAny a0 -> self#loc a0
      | TyApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyArr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyCls (a0,a1) -> let self = self#loc a0 in self#ident a1
      | TyLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | TyId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | TyMan (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyDcl (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#list (fun self  -> self#ctyp) a2 in
          let self = self#ctyp a3 in
          self#list
            (fun self  (a0,a1)  -> let self = self#ctyp a0 in self#ctyp a1)
            a4
      | TyObj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#row_var_flag a2
      | TyOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | TyPol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyTypePol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyQuo (a0,a1) -> let self = self#loc a0 in self#string a1
      | TyQuP (a0,a1) -> let self = self#loc a0 in self#string a1
      | TyQuM (a0,a1) -> let self = self#loc a0 in self#string a1
      | TyAnP a0 -> self#loc a0
      | TyAnM a0 -> self#loc a0
      | TyVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | TyRec (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyCol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TySem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TySum (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyOf (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyAnd (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyOr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyPrv (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyMut (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyTup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TySta (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyVrnEq (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyVrnSup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyVrnInf (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | TyVrnInfSup (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyOfAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | TyPkg (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | TyAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method ident : ident -> 'self_type=
      function
      | IdAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | IdApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | IdLid (a0,a1) -> let self = self#loc a0 in self#string a1
      | IdUid (a0,a1) -> let self = self#loc a0 in self#string a1
      | IdAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_list -> 'self_type=
      fun mf_a  ->
        function
        | LNil  -> self
        | LCons (a0,a1) -> let self = mf_a self a0 in self#meta_list mf_a a1
        | LAnt a0 -> self#string a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_option -> 'self_type=
      fun mf_a  ->
        function
        | ONone  -> self
        | OSome a0 -> mf_a self a0
        | OAnt a0 -> self#string a0
    method row_var_flag : row_var_flag -> 'self_type=
      function
      | RvRowVar  -> self
      | RvNil  -> self
      | RvAnt a0 -> self#string a0
    method override_flag : override_flag -> 'self_type=
      function
      | OvOverride  -> self
      | OvNil  -> self
      | OvAnt a0 -> self#string a0
    method virtual_flag : virtual_flag -> 'self_type=
      function
      | ViVirtual  -> self
      | ViNil  -> self
      | ViAnt a0 -> self#string a0
    method private_flag : private_flag -> 'self_type=
      function
      | PrPrivate  -> self
      | PrNil  -> self
      | PrAnt a0 -> self#string a0
    method mutable_flag : mutable_flag -> 'self_type=
      function
      | MuMutable  -> self
      | MuNil  -> self
      | MuAnt a0 -> self#string a0
    method direction_flag : direction_flag -> 'self_type=
      function
      | DiTo  -> self
      | DiDownto  -> self
      | DiAnt a0 -> self#string a0
    method rec_flag : rec_flag -> 'self_type=
      function
      | ReRecursive  -> self
      | ReNil  -> self
      | ReAnt a0 -> self#string a0
    method meta_bool : meta_bool -> 'self_type=
      function | BTrue  -> self | BFalse  -> self | BAnt a0 -> self#string a0
    method loc : loc -> 'self_type= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
  end
let rec is_module_longident =
  function
  | IdAcc (_loc,_,i) -> is_module_longident i
  | IdApp (_loc,i1,i2) ->
      (is_module_longident i1) && (is_module_longident i2)
  | IdUid (_loc,_) -> true
  | _ -> false
let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =
    function
    | ExApp (_loc,e1,e2) -> IdApp (_loc, (self e1), (self e2))
    | ExAcc (_loc,e1,e2) -> IdAcc (_loc, (self e1), (self e2))
    | ExId (_loc,IdLid (_,_)) -> error ()
    | ExId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | ExId (_loc,i) -> i | ExApp (_loc,_,_) -> error () | t -> self t
let ident_of_ctyp =
  let error () = invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =
    function
    | TyApp (_loc,t1,t2) -> IdApp (_loc, (self t1), (self t2))
    | TyId (_loc,IdLid (_,_)) -> error ()
    | TyId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | TyId (_loc,i) -> i | t -> self t
let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self =
    function
    | PaApp (_loc,p1,p2) -> IdApp (_loc, (self p1), (self p2))
    | PaId (_loc,IdLid (_,_)) -> error ()
    | PaId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () in
  function | PaId (_loc,i) -> i | p -> self p
let rec is_irrefut_patt =
  function
  | PaId (_loc,IdLid (_,_)) -> true
  | PaId (_loc,IdUid (_,"()")) -> true
  | PaAny _loc -> true
  | PaNil _loc -> true
  | PaAli (_loc,x,y) -> (is_irrefut_patt x) && (is_irrefut_patt y)
  | PaRec (_loc,p) -> is_irrefut_patt p
  | PaEq (_loc,_,p) -> is_irrefut_patt p
  | PaSem (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | PaCom (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | PaOrp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | PaApp (_loc,p1,p2) -> (is_irrefut_patt p1) && (is_irrefut_patt p2)
  | PaTyc (_loc,p,_) -> is_irrefut_patt p
  | PaTup (_loc,pl) -> is_irrefut_patt pl
  | PaOlb (_loc,_,PaNil _) -> true
  | PaOlb (_loc,_,_) -> true
  | PaOlbi (_loc,_,_,_) -> true
  | PaLab (_loc,_,PaNil _) -> true
  | PaLab (_loc,_,p) -> is_irrefut_patt p
  | PaLaz (_loc,p) -> is_irrefut_patt p
  | PaId (_loc,_) -> false
  | PaMod (_loc,_) -> true
  | PaVrn (_loc,_)|PaStr (_loc,_)|PaRng (_loc,_,_)|PaFlo (_loc,_)|PaNativeInt
      (_loc,_)|PaInt64 (_loc,_)|PaInt32 (_loc,_)|PaInt (_loc,_)|PaChr
      (_loc,_)|PaTyp (_loc,_)|PaArr (_loc,_)|PaAnt (_loc,_) -> false
let rec is_constructor =
  function
  | IdAcc (_loc,_,i) -> is_constructor i
  | IdUid (_loc,_) -> true
  | IdLid (_loc,_)|IdApp (_loc,_,_) -> false
  | IdAnt (_loc,_) -> assert false
let is_patt_constructor =
  function
  | PaId (_loc,i) -> is_constructor i
  | PaVrn (_loc,_) -> true
  | _ -> false
let rec is_expr_constructor =
  function
  | ExId (_loc,i) -> is_constructor i
  | ExAcc (_loc,e1,e2) ->
      (is_expr_constructor e1) && (is_expr_constructor e2)
  | ExVrn (_loc,_) -> true
  | _ -> false
let ghost = FanLoc.ghost
let rec tyOr_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TyOr (_loc, t, (tyOr_of_list ts))
let rec tyAnd_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TyAnd (_loc, t, (tyAnd_of_list ts))
let rec tySem_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TySem (_loc, t, (tySem_of_list ts))
let rec tyCom_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TyCom (_loc, t, (tyCom_of_list ts))
let rec tyAmp_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TyAmp (_loc, t, (tyAmp_of_list ts))
let rec tySta_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts -> let _loc = loc_of_ctyp t in TySta (_loc, t, (tySta_of_list ts))
let tyApp_of_list =
  function
  | [] -> TyNil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_ctyp x in TyApp (_loc, x, y)) t ts
let tyVarApp_of_list (_loc,ls) =
  let aux =
    function
    | [] -> TyNil ghost
    | t::[] -> TyQuo (_loc, t)
    | t::ts ->
        List.fold_left (fun x  y  -> TyApp (_loc, x, (TyQuo (_loc, y))))
          (TyQuo (_loc, t)) ts in
  aux ls
let rec stSem_of_list =
  function
  | [] -> StNil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_str_item t in StSem (_loc, t, (stSem_of_list ts))
let rec sgSem_of_list =
  function
  | [] -> SgNil ghost
  | t::[] -> t
  | t::ts ->
      let _loc = loc_of_sig_item t in SgSem (_loc, t, (sgSem_of_list ts))
let rec biAnd_of_list =
  function
  | [] -> BiNil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_binding b in BiAnd (_loc, b, (biAnd_of_list bs))
let rec rbSem_of_list =
  function
  | [] -> RbNil ghost
  | b::[] -> b
  | b::bs ->
      let _loc = loc_of_rec_binding b in RbSem (_loc, b, (rbSem_of_list bs))
let rec wcAnd_of_list =
  function
  | [] -> WcNil ghost
  | w::[] -> w
  | w::ws ->
      let _loc = loc_of_with_constr w in WcAnd (_loc, w, (wcAnd_of_list ws))
let rec idAcc_of_list =
  function
  | [] -> assert false
  | i::[] -> i
  | i::is -> let _loc = loc_of_ident i in IdAcc (_loc, i, (idAcc_of_list is))
let rec idApp_of_list =
  function
  | [] -> assert false
  | i::[] -> i
  | i::is -> let _loc = loc_of_ident i in IdApp (_loc, i, (idApp_of_list is))
let rec mcOr_of_list =
  function
  | [] -> McNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_match_case x in McOr (_loc, x, (mcOr_of_list xs))
let rec mbAnd_of_list =
  function
  | [] -> MbNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_binding x in
      MbAnd (_loc, x, (mbAnd_of_list xs))
let rec meApp_of_list =
  function
  | [] -> assert false
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_module_expr x in MeApp (_loc, x, (meApp_of_list xs))
let rec ceAnd_of_list =
  function
  | [] -> CeNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_expr x in CeAnd (_loc, x, (ceAnd_of_list xs))
let rec ctAnd_of_list =
  function
  | [] -> CtNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_type x in CtAnd (_loc, x, (ctAnd_of_list xs))
let rec cgSem_of_list =
  function
  | [] -> CgNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_sig_item x in
      CgSem (_loc, x, (cgSem_of_list xs))
let rec crSem_of_list =
  function
  | [] -> CrNil ghost
  | x::[] -> x
  | x::xs ->
      let _loc = loc_of_class_str_item x in
      CrSem (_loc, x, (crSem_of_list xs))
let rec paSem_of_list =
  function
  | [] -> PaNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in PaSem (_loc, x, (paSem_of_list xs))
let rec paCom_of_list =
  function
  | [] -> PaNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_patt x in PaCom (_loc, x, (paCom_of_list xs))
let rec exSem_of_list =
  function
  | [] -> ExNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in ExSem (_loc, x, (exSem_of_list xs))
let rec exCom_of_list =
  function
  | [] -> ExNil ghost
  | x::[] -> x
  | x::xs -> let _loc = loc_of_expr x in ExCom (_loc, x, (exCom_of_list xs))
let exApp_of_list =
  function
  | [] -> ExNil ghost
  | t::[] -> t
  | t::ts ->
      List.fold_left
        (fun x  y  -> let _loc = loc_of_expr x in ExApp (_loc, x, y)) t ts
let ty_of_stl =
  function
  | (_loc,s,[]) -> TyId (_loc, (IdUid (_loc, s)))
  | (_loc,s,tl) ->
      TyOf (_loc, (TyId (_loc, (IdUid (_loc, s)))), (tyAnd_of_list tl))
let ty_of_sbt =
  function
  | (_loc,s,true ,t) ->
      TyCol (_loc, (TyId (_loc, (IdLid (_loc, s)))), (TyMut (_loc, t)))
  | (_loc,s,false ,t) -> TyCol (_loc, (TyId (_loc, (IdLid (_loc, s)))), t)
let bi_of_pe (p,e) = let _loc = loc_of_patt p in BiEq (_loc, p, e)
let sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l)
let record_type_of_list l = tySem_of_list (List.map ty_of_sbt l)
let binding_of_pel l = biAnd_of_list (List.map bi_of_pe l)
let rec pel_of_binding =
  function
  | BiAnd (_loc,b1,b2) -> (pel_of_binding b1) @ (pel_of_binding b2)
  | BiEq (_loc,p,e) -> [(p, e)]
  | _ -> assert false
let rec list_of_binding x acc =
  match x with
  | BiAnd (_loc,b1,b2) -> list_of_binding b1 (list_of_binding b2 acc)
  | t -> t :: acc
let rec list_of_rec_binding x acc =
  match x with
  | RbSem (_loc,b1,b2) -> list_of_rec_binding b1 (list_of_rec_binding b2 acc)
  | t -> t :: acc
let rec list_of_with_constr x acc =
  match x with
  | WcAnd (_loc,w1,w2) -> list_of_with_constr w1 (list_of_with_constr w2 acc)
  | t -> t :: acc
let rec list_of_ctyp x acc =
  match x with
  | TyNil _loc -> acc
  | TyAmp (_loc,x,y)|TyCom (_loc,x,y)|TySta (_loc,x,y)|TySem (_loc,x,y)|TyAnd
      (_loc,x,y)|TyOr (_loc,x,y) -> list_of_ctyp x (list_of_ctyp y acc)
  | x -> x :: acc
let rec list_of_patt x acc =
  match x with
  | PaNil _loc -> acc
  | PaCom (_loc,x,y)|PaSem (_loc,x,y) -> list_of_patt x (list_of_patt y acc)
  | x -> x :: acc
let rec list_of_expr x acc =
  match x with
  | ExNil _loc -> acc
  | ExCom (_loc,x,y)|ExSem (_loc,x,y) -> list_of_expr x (list_of_expr y acc)
  | x -> x :: acc
let rec list_of_str_item x acc =
  match x with
  | StNil _loc -> acc
  | StSem (_loc,x,y) -> list_of_str_item x (list_of_str_item y acc)
  | x -> x :: acc
let rec list_of_sig_item x acc =
  match x with
  | SgNil _loc -> acc
  | SgSem (_loc,x,y) -> list_of_sig_item x (list_of_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_sig_item x acc =
  match x with
  | CgNil _loc -> acc
  | CgSem (_loc,x,y) ->
      list_of_class_sig_item x (list_of_class_sig_item y acc)
  | x -> x :: acc
let rec list_of_class_str_item x acc =
  match x with
  | CrNil _loc -> acc
  | CrSem (_loc,x,y) ->
      list_of_class_str_item x (list_of_class_str_item y acc)
  | x -> x :: acc
let rec list_of_class_type x acc =
  match x with
  | CtAnd (_loc,x,y) -> list_of_class_type x (list_of_class_type y acc)
  | x -> x :: acc
let rec list_of_class_expr x acc =
  match x with
  | CeAnd (_loc,x,y) -> list_of_class_expr x (list_of_class_expr y acc)
  | x -> x :: acc
let rec list_of_module_expr x acc =
  match x with
  | MeApp (_loc,x,y) -> list_of_module_expr x (list_of_module_expr y acc)
  | x -> x :: acc
let rec list_of_match_case x acc =
  match x with
  | McNil _loc -> acc
  | McOr (_loc,x,y) -> list_of_match_case x (list_of_match_case y acc)
  | x -> x :: acc
let rec list_of_ident x acc =
  match x with
  | IdAcc (_loc,x,y)|IdApp (_loc,x,y) ->
      list_of_ident x (list_of_ident y acc)
  | x -> x :: acc
let rec list_of_module_binding x acc =
  match x with
  | MbAnd (_loc,x,y) ->
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
      | WcAnd (_loc,WcNil _l,wc)|WcAnd (_loc,wc,WcNil _l) -> wc
      | wc -> wc
    method! expr e =
      match super#expr e with
      | ExLet (_loc,_,BiNil _l,e)|ExRec (_loc,RbNil _l,e)|ExCom
          (_loc,ExNil _l,e)|ExCom (_loc,e,ExNil _l)|ExSem
          (_loc,ExNil _l,e)|ExSem (_loc,e,ExNil _l) -> e
      | e -> e
    method! patt p =
      match super#patt p with
      | PaAli (_loc,p,PaNil _l)|PaOrp (_loc,PaNil _l,p)|PaOrp
          (_loc,p,PaNil _l)|PaCom (_loc,PaNil _l,p)|PaCom
          (_loc,p,PaNil _l)|PaSem (_loc,PaNil _l,p)|PaSem (_loc,p,PaNil _l)
          -> p
      | p -> p
    method! match_case mc =
      match super#match_case mc with
      | McOr (_loc,McNil _l,mc)|McOr (_loc,mc,McNil _l) -> mc
      | mc -> mc
    method! binding bi =
      match super#binding bi with
      | BiAnd (_loc,BiNil _l,bi)|BiAnd (_loc,bi,BiNil _l) -> bi
      | bi -> bi
    method! rec_binding rb =
      match super#rec_binding rb with
      | RbSem (_loc,RbNil _l,bi)|RbSem (_loc,bi,RbNil _l) -> bi
      | bi -> bi
    method! module_binding mb =
      match super#module_binding mb with
      | MbAnd (_loc,MbNil _l,mb)|MbAnd (_loc,mb,MbNil _l) -> mb
      | mb -> mb
    method! ctyp t =
      match super#ctyp t with
      | TyPol (_loc,TyNil _l,t)|TyAli (_loc,TyNil _l,t)|TyAli
          (_loc,t,TyNil _l)|TyArr (_loc,t,TyNil _l)|TyArr
          (_loc,TyNil _l,t)|TyOr (_loc,TyNil _l,t)|TyOr
          (_loc,t,TyNil _l)|TyOf (_loc,t,TyNil _l)|TyAnd
          (_loc,TyNil _l,t)|TyAnd (_loc,t,TyNil _l)|TySem
          (_loc,t,TyNil _l)|TySem (_loc,TyNil _l,t)|TyCom
          (_loc,TyNil _l,t)|TyCom (_loc,t,TyNil _l)|TyAmp
          (_loc,t,TyNil _l)|TyAmp (_loc,TyNil _l,t)|TySta
          (_loc,TyNil _l,t)|TySta (_loc,t,TyNil _l) -> t
      | t -> t
    method! sig_item sg =
      match super#sig_item sg with
      | SgSem (_loc,SgNil _l,sg)|SgSem (_loc,sg,SgNil _l) -> sg
      | SgTyp (_loc,TyNil _l) -> SgNil _loc
      | sg -> sg
    method! str_item st =
      match super#str_item st with
      | StSem (_loc,StNil _l,st)|StSem (_loc,st,StNil _l) -> st
      | StTyp (_loc,TyNil _l) -> StNil _loc
      | StVal (_loc,_,BiNil _l) -> StNil _loc
      | st -> st
    method! module_type mt =
      match super#module_type mt with
      | MtWit (_loc,mt,WcNil _l) -> mt
      | mt -> mt
    method! class_expr ce =
      match super#class_expr ce with
      | CeAnd (_loc,CeNil _l,ce)|CeAnd (_loc,ce,CeNil _l) -> ce
      | ce -> ce
    method! class_type ct =
      match super#class_type ct with
      | CtAnd (_loc,CtNil _l,ct)|CtAnd (_loc,ct,CtNil _l) -> ct
      | ct -> ct
    method! class_sig_item csg =
      match super#class_sig_item csg with
      | CgSem (_loc,CgNil _l,csg)|CgSem (_loc,csg,CgNil _l) -> csg
      | csg -> csg
    method! class_str_item cst =
      match super#class_str_item cst with
      | CrSem (_loc,CrNil _l,cst)|CrSem (_loc,cst,CrNil _l) -> cst
      | cst -> cst
  end
class reloc _loc = object  inherit  map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  map as super
    method! patt =
      function
      | PaId (_loc,IdLid (_,_)) -> PaAny _loc
      | PaAli (_loc,p,_) -> self#patt p
      | p -> super#patt p
  end
let match_pre =
  object (self)
    inherit  map
    method! match_case =
      function
      | McArr (_loc,p,ExNil _,e) ->
          McArr
            (_loc, p, (ExNil _loc),
              (ExFun
                 (_loc,
                   (McArr
                      (_loc, (PaId (_loc, (IdUid (_loc, "()")))),
                        (ExNil _loc), e)))))
      | McArr (_loc,p,e,e1) ->
          McArr
            (_loc, p, e,
              (ExFun
                 (_loc,
                   (McArr
                      (_loc, (PaId (_loc, (IdUid (_loc, "()")))),
                        (ExNil _loc), e1)))))
      | McOr (_loc,a1,a2) ->
          McOr (_loc, (self#match_case a1), (self#match_case a2))
      | McNil _loc -> McNil _loc
      | McAnt (_loc,x) -> McAnt (_loc, (add_context x "lettry"))
  end
module Make(MetaLoc:META_LOC) =
  struct
  module Expr = struct
    open StdMeta.Expr let meta_loc = MetaLoc.meta_loc_expr
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | CrNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "CrNil")))),
                (meta_loc _loc a0))
        | CrSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CrSem")))),
                          (meta_loc _loc a0))),
                     (meta_class_str_item _loc a1))),
                (meta_class_str_item _loc a2))
        | CrCtr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CrCtr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | CrInh (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CrInh")))),
                               (meta_loc _loc a0))),
                          (meta_override_flag _loc a1))),
                     (meta_class_expr _loc a2))), (meta_string _loc a3))
        | CrIni (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "CrIni")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | CrMth (a0,a1,a2,a3,a4,a5) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExApp
                                       (_loc,
                                         (ExId
                                            (_loc, (IdUid (_loc, "CrMth")))),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_override_flag _loc a2))),
                          (meta_private_flag _loc a3))), (meta_expr _loc a4))),
                (meta_ctyp _loc a5))
        | CrVal (a0,a1,a2,a3,a4) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExId (_loc, (IdUid (_loc, "CrVal")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_override_flag _loc a2))),
                     (meta_mutable_flag _loc a3))), (meta_expr _loc a4))
        | CrVir (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CrVir")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CrVvr (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CrVvr")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
        | CrAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | CeNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "CeNil")))),
                (meta_loc _loc a0))
        | CeApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeApp")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_expr _loc a2))
        | CeCon (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CeCon")))),
                               (meta_loc _loc a0))),
                          (meta_virtual_flag _loc a1))),
                     (meta_ident _loc a2))), (meta_ctyp _loc a3))
        | CeFun (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeFun")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_expr _loc a2))
        | CeLet (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CeLet")))),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_class_expr _loc a3))
        | CeStr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeStr")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_str_item _loc a2))
        | CeTyc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeTyc")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_type _loc a2))
        | CeAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeAnd")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_expr _loc a2))
        | CeEq (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CeEq")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_expr _loc a2))
        | CeAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | CgNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "CgNil")))),
                (meta_loc _loc a0))
        | CgCtr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CgCtr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | CgSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CgSem")))),
                          (meta_loc _loc a0))),
                     (meta_class_sig_item _loc a1))),
                (meta_class_sig_item _loc a2))
        | CgInh (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "CgInh")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | CgMth (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CgMth")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CgVal (a0,a1,a2,a3,a4) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExId (_loc, (IdUid (_loc, "CgVal")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_mutable_flag _loc a2))),
                     (meta_virtual_flag _loc a3))), (meta_ctyp _loc a4))
        | CgVir (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CgVir")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CgAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | CtNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "CtNil")))),
                (meta_loc _loc a0))
        | CtCon (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "CtCon")))),
                               (meta_loc _loc a0))),
                          (meta_virtual_flag _loc a1))),
                     (meta_ident _loc a2))), (meta_ctyp _loc a3))
        | CtFun (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CtFun")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_class_type _loc a2))
        | CtSig (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CtSig")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_class_sig_item _loc a2))
        | CtAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CtAnd")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtCol (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CtCol")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtEq (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "CtEq")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | StNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "StNil")))),
                (meta_loc _loc a0))
        | StCls (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StCls")))),
                     (meta_loc _loc a0))), (meta_class_expr _loc a1))
        | StClt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StClt")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | StSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StSem")))),
                          (meta_loc _loc a0))), (meta_str_item _loc a1))),
                (meta_str_item _loc a2))
        | StDir (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StDir")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | StExc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StExc")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_meta_option meta_ident _loc a2))
        | StExp (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StExp")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | StExt (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "StExt")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | StInc (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StInc")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
        | StMod (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StMod")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_expr _loc a2))
        | StRecMod (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StRecMod")))),
                     (meta_loc _loc a0))), (meta_module_binding _loc a1))
        | StMty (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StMty")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | StOpn (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StOpn")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | StTyp (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "StTyp")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | StVal (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "StVal")))),
                          (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                (meta_binding _loc a2))
        | StAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | MeNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "MeNil")))),
                (meta_loc _loc a0))
        | MeId (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MeId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | MeApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "MeApp")))),
                          (meta_loc _loc a0))), (meta_module_expr _loc a1))),
                (meta_module_expr _loc a2))
        | MeFun (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "MeFun")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | MeStr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MeStr")))),
                     (meta_loc _loc a0))), (meta_str_item _loc a1))
        | MeTyc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "MeTyc")))),
                          (meta_loc _loc a0))), (meta_module_expr _loc a1))),
                (meta_module_type _loc a2))
        | MePkg (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MePkg")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | MeAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | McNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "McNil")))),
                (meta_loc _loc a0))
        | McOr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "McOr")))),
                          (meta_loc _loc a0))), (meta_match_case _loc a1))),
                (meta_match_case _loc a2))
        | McArr (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "McArr")))),
                               (meta_loc _loc a0))), (meta_patt _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | McAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | MbNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "MbNil")))),
                (meta_loc _loc a0))
        | MbAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "MbAnd")))),
                          (meta_loc _loc a0))),
                     (meta_module_binding _loc a1))),
                (meta_module_binding _loc a2))
        | MbColEq (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "MbColEq")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | MbCol (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "MbCol")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | MbAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | RbNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "RbNil")))),
                (meta_loc _loc a0))
        | RbSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "RbSem")))),
                          (meta_loc _loc a0))), (meta_rec_binding _loc a1))),
                (meta_rec_binding _loc a2))
        | RbEq (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "RbEq")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_expr _loc a2))
        | RbAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | BiNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "BiNil")))),
                (meta_loc _loc a0))
        | BiAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "BiAnd")))),
                          (meta_loc _loc a0))), (meta_binding _loc a1))),
                (meta_binding _loc a2))
        | BiEq (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "BiEq")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_expr _loc a2))
        | BiAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | WcNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "WcNil")))),
                (meta_loc _loc a0))
        | WcTyp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "WcTyp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | WcMod (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "WcMod")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | WcTyS (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "WcTyS")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | WcMoS (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "WcMoS")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | WcAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "WcAnd")))),
                          (meta_loc _loc a0))), (meta_with_constr _loc a1))),
                (meta_with_constr _loc a2))
        | WcAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | SgNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "SgNil")))),
                (meta_loc _loc a0))
        | SgCls (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgCls")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | SgClt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgClt")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | SgSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "SgSem")))),
                          (meta_loc _loc a0))), (meta_sig_item _loc a1))),
                (meta_sig_item _loc a2))
        | SgDir (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "SgDir")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | SgExc (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgExc")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | SgExt (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "SgExt")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | SgInc (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgInc")))),
                     (meta_loc _loc a0))), (meta_module_type _loc a1))
        | SgMod (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "SgMod")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | SgRecMod (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgRecMod")))),
                     (meta_loc _loc a0))), (meta_module_binding _loc a1))
        | SgMty (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "SgMty")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | SgOpn (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgOpn")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | SgTyp (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "SgTyp")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | SgVal (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "SgVal")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | SgAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | MtNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "MtNil")))),
                (meta_loc _loc a0))
        | MtId (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MtId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | MtFun (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "MtFun")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_type _loc a3))
        | MtQuo (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MtQuo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | MtSig (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MtSig")))),
                     (meta_loc _loc a0))), (meta_sig_item _loc a1))
        | MtWit (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "MtWit")))),
                          (meta_loc _loc a0))), (meta_module_type _loc a1))),
                (meta_with_constr _loc a2))
        | MtOf (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "MtOf")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
        | MtAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | ExNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "ExNil")))),
                (meta_loc _loc a0))
        | ExId (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | ExAcc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExAcc")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAnt (a0,a1) -> ExAnt (a0, a1)
        | ExApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExApp")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAre (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExAre")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExArr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExArr")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExSem")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAsf a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "ExAsf")))),
                (meta_loc _loc a0))
        | ExAsr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExAsr")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExAss (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExAss")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExChr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExChr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExCoe (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "ExCoe")))),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_ctyp _loc a2))), (meta_ctyp _loc a3))
        | ExFlo (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExFlo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExFor (a0,a1,a2,a3,a4,a5) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExApp
                                       (_loc,
                                         (ExId
                                            (_loc, (IdUid (_loc, "ExFor")))),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_expr _loc a2))), (meta_expr _loc a3))),
                     (meta_direction_flag _loc a4))), (meta_expr _loc a5))
        | ExFun (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExFun")))),
                     (meta_loc _loc a0))), (meta_match_case _loc a1))
        | ExIfe (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "ExIfe")))),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | ExInt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExInt32 (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExInt32")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExInt64 (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExInt64")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExNativeInt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExNativeInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExLab (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExLaz (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExLaz")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExLet (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "ExLet")))),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_expr _loc a3))
        | ExLmd (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "ExLmd")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_expr _loc a2))), (meta_expr _loc a3))
        | ExMat (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExMat")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_match_case _loc a2))
        | ExNew (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExNew")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | ExObj (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExObj")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_str_item _loc a2))
        | ExOlb (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExOvr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExOvr")))),
                     (meta_loc _loc a0))), (meta_rec_binding _loc a1))
        | ExRec (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExRec")))),
                          (meta_loc _loc a0))), (meta_rec_binding _loc a1))),
                (meta_expr _loc a2))
        | ExSeq (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExSeq")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExSnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExSnd")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_string _loc a2))
        | ExSte (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExSte")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExStr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExStr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExTry (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExTry")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_match_case _loc a2))
        | ExTup (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExTup")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExCom (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExCom")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExTyc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExTyc")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_ctyp _loc a2))
        | ExVrn (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExWhi (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExWhi")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExOpI (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExOpI")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_expr _loc a2))
        | ExFUN (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "ExFUN")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExPkg (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "ExPkg")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | PaNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "PaNil")))),
                (meta_loc _loc a0))
        | PaId (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | PaAli (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaAli")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaAnt (a0,a1) -> ExAnt (a0, a1)
        | PaAny a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "PaAny")))),
                (meta_loc _loc a0))
        | PaApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaApp")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaArr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaArr")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaCom (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaCom")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaSem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaSem")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaChr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaChr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt32 (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaInt32")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt64 (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaInt64")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaNativeInt (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaNativeInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaFlo (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaFlo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaLab (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_patt _loc a2))
        | PaOlb (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_patt _loc a2))
        | PaOlbi (a0,a1,a2,a3) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (IdUid (_loc, "PaOlbi")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_patt _loc a2))), (meta_expr _loc a3))
        | PaOrp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaOrp")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaRng (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaRng")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaRec (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaRec")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaEq (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaEq")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_patt _loc a2))
        | PaStr (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaStr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaTup (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaTup")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaTyc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "PaTyc")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_ctyp _loc a2))
        | PaTyp (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaTyp")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | PaVrn (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaLaz (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaLaz")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaMod (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "PaMod")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | TyNil a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "TyNil")))),
                (meta_loc _loc a0))
        | TyAli (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyAli")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAny a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "TyAny")))),
                (meta_loc _loc a0))
        | TyApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyApp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyArr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyArr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyCls (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyCls")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | TyLab (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | TyId (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | TyMan (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyMan")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyDcl (a0,a1,a2,a3,a4) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc,
                          (ExApp
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExId (_loc, (IdUid (_loc, "TyDcl")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_list meta_ctyp _loc a2))),
                     (meta_ctyp _loc a3))),
                (meta_list
                   (fun _loc  (a0,a1)  ->
                      Ast.ExTup
                        (_loc,
                          (ExCom
                             (_loc, (meta_ctyp _loc a0), (meta_ctyp _loc a1)))))
                   _loc a4))
        | TyObj (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyObj")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_row_var_flag _loc a2))
        | TyOlb (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | TyPol (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyPol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyTypePol (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyTypePol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyQuo (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyQuo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyQuP (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyQuP")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyQuM (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyQuM")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyAnP a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "TyAnP")))),
                (meta_loc _loc a0))
        | TyAnM a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "TyAnM")))),
                (meta_loc _loc a0))
        | TyVrn (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyRec (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyRec")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyCol (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyCol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TySem (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TySem")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyCom (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyCom")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TySum (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TySum")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyOf (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyOf")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAnd (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyAnd")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyOr (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyOr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyPrv (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyPrv")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyMut (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyMut")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyTup (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyTup")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TySta (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TySta")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyVrnEq (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyVrnEq")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnSup (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyVrnSup")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnInf (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyVrnInf")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnInfSup (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyVrnInfSup")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAmp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyAmp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyOfAmp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "TyOfAmp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyPkg (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "TyPkg")))),
                     (meta_loc _loc a0))), (meta_module_type _loc a1))
        | TyAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | IdAcc (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "IdAcc")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | IdApp (a0,a1,a2) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc,
                     (ExApp
                        (_loc, (ExId (_loc, (IdUid (_loc, "IdApp")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | IdLid (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "IdLid")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | IdUid (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "IdUid")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | IdAnt (a0,a1) -> ExAnt (a0, a1)
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | LNil  -> ExId (_loc, (IdUid (_loc, "LNil")))
        | LCons (a0,a1) ->
            ExApp
              (_loc,
                (ExApp
                   (_loc, (ExId (_loc, (IdUid (_loc, "LCons")))),
                     (mf_a _loc a0))), (meta_meta_list mf_a _loc a1))
        | LAnt a0 -> ExAnt (_loc, a0)
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | ONone  -> ExId (_loc, (IdUid (_loc, "ONone")))
        | OSome a0 ->
            ExApp
              (_loc, (ExId (_loc, (IdUid (_loc, "OSome")))), (mf_a _loc a0))
        | OAnt a0 -> ExAnt (_loc, a0)
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | RvRowVar  -> ExId (_loc, (IdUid (_loc, "RvRowVar")))
        | RvNil  -> ExId (_loc, (IdUid (_loc, "RvNil")))
        | RvAnt a0 -> ExAnt (_loc, a0)
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | OvOverride  -> ExId (_loc, (IdUid (_loc, "OvOverride")))
        | OvNil  -> ExId (_loc, (IdUid (_loc, "OvNil")))
        | OvAnt a0 -> ExAnt (_loc, a0)
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | ViVirtual  -> ExId (_loc, (IdUid (_loc, "ViVirtual")))
        | ViNil  -> ExId (_loc, (IdUid (_loc, "ViNil")))
        | ViAnt a0 -> ExAnt (_loc, a0)
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | PrPrivate  -> ExId (_loc, (IdUid (_loc, "PrPrivate")))
        | PrNil  -> ExId (_loc, (IdUid (_loc, "PrNil")))
        | PrAnt a0 -> ExAnt (_loc, a0)
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | MuMutable  -> ExId (_loc, (IdUid (_loc, "MuMutable")))
        | MuNil  -> ExId (_loc, (IdUid (_loc, "MuNil")))
        | MuAnt a0 -> ExAnt (_loc, a0)
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | DiTo  -> ExId (_loc, (IdUid (_loc, "DiTo")))
        | DiDownto  -> ExId (_loc, (IdUid (_loc, "DiDownto")))
        | DiAnt a0 -> ExAnt (_loc, a0)
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | ReRecursive  -> ExId (_loc, (IdUid (_loc, "ReRecursive")))
        | ReNil  -> ExId (_loc, (IdUid (_loc, "ReNil")))
        | ReAnt a0 -> ExAnt (_loc, a0)
    and meta_meta_bool: 'loc -> meta_bool -> 'result =
      fun _loc  ->
        function
        | BTrue  -> ExId (_loc, (IdUid (_loc, "BTrue")))
        | BFalse  -> ExId (_loc, (IdUid (_loc, "BFalse")))
        | BAnt a0 -> ExAnt (_loc, a0)
    end
  module Patt = struct
    open StdMeta.Patt let meta_loc = MetaLoc.meta_loc_patt
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | CrNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "CrNil")))),
                (meta_loc _loc a0))
        | CrSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CrSem")))),
                          (meta_loc _loc a0))),
                     (meta_class_str_item _loc a1))),
                (meta_class_str_item _loc a2))
        | CrCtr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CrCtr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | CrInh (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CrInh")))),
                               (meta_loc _loc a0))),
                          (meta_override_flag _loc a1))),
                     (meta_class_expr _loc a2))), (meta_string _loc a3))
        | CrIni (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "CrIni")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | CrMth (a0,a1,a2,a3,a4,a5) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc,
                               (PaApp
                                  (_loc,
                                    (PaApp
                                       (_loc,
                                         (PaId
                                            (_loc, (IdUid (_loc, "CrMth")))),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_override_flag _loc a2))),
                          (meta_private_flag _loc a3))), (meta_expr _loc a4))),
                (meta_ctyp _loc a5))
        | CrVal (a0,a1,a2,a3,a4) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc,
                               (PaApp
                                  (_loc,
                                    (PaId (_loc, (IdUid (_loc, "CrVal")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_override_flag _loc a2))),
                     (meta_mutable_flag _loc a3))), (meta_expr _loc a4))
        | CrVir (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CrVir")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CrVvr (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CrVvr")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
        | CrAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | CeNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "CeNil")))),
                (meta_loc _loc a0))
        | CeApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeApp")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_expr _loc a2))
        | CeCon (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CeCon")))),
                               (meta_loc _loc a0))),
                          (meta_virtual_flag _loc a1))),
                     (meta_ident _loc a2))), (meta_ctyp _loc a3))
        | CeFun (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeFun")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_expr _loc a2))
        | CeLet (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CeLet")))),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_class_expr _loc a3))
        | CeStr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeStr")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_str_item _loc a2))
        | CeTyc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeTyc")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_type _loc a2))
        | CeAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeAnd")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_expr _loc a2))
        | CeEq (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CeEq")))),
                          (meta_loc _loc a0))), (meta_class_expr _loc a1))),
                (meta_class_expr _loc a2))
        | CeAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | CgNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "CgNil")))),
                (meta_loc _loc a0))
        | CgCtr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CgCtr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | CgSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CgSem")))),
                          (meta_loc _loc a0))),
                     (meta_class_sig_item _loc a1))),
                (meta_class_sig_item _loc a2))
        | CgInh (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "CgInh")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | CgMth (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CgMth")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CgVal (a0,a1,a2,a3,a4) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc,
                               (PaApp
                                  (_loc,
                                    (PaId (_loc, (IdUid (_loc, "CgVal")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_mutable_flag _loc a2))),
                     (meta_virtual_flag _loc a3))), (meta_ctyp _loc a4))
        | CgVir (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CgVir")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | CgAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | CtNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "CtNil")))),
                (meta_loc _loc a0))
        | CtCon (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "CtCon")))),
                               (meta_loc _loc a0))),
                          (meta_virtual_flag _loc a1))),
                     (meta_ident _loc a2))), (meta_ctyp _loc a3))
        | CtFun (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CtFun")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_class_type _loc a2))
        | CtSig (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CtSig")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_class_sig_item _loc a2))
        | CtAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CtAnd")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtCol (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CtCol")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtEq (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "CtEq")))),
                          (meta_loc _loc a0))), (meta_class_type _loc a1))),
                (meta_class_type _loc a2))
        | CtAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | StNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "StNil")))),
                (meta_loc _loc a0))
        | StCls (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StCls")))),
                     (meta_loc _loc a0))), (meta_class_expr _loc a1))
        | StClt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StClt")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | StSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StSem")))),
                          (meta_loc _loc a0))), (meta_str_item _loc a1))),
                (meta_str_item _loc a2))
        | StDir (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StDir")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | StExc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StExc")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_meta_option meta_ident _loc a2))
        | StExp (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StExp")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | StExt (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "StExt")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | StInc (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StInc")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
        | StMod (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StMod")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_expr _loc a2))
        | StRecMod (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StRecMod")))),
                     (meta_loc _loc a0))), (meta_module_binding _loc a1))
        | StMty (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StMty")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | StOpn (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StOpn")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | StTyp (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "StTyp")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | StVal (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "StVal")))),
                          (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                (meta_binding _loc a2))
        | StAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | MeNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "MeNil")))),
                (meta_loc _loc a0))
        | MeId (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MeId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | MeApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "MeApp")))),
                          (meta_loc _loc a0))), (meta_module_expr _loc a1))),
                (meta_module_expr _loc a2))
        | MeFun (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "MeFun")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | MeStr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MeStr")))),
                     (meta_loc _loc a0))), (meta_str_item _loc a1))
        | MeTyc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "MeTyc")))),
                          (meta_loc _loc a0))), (meta_module_expr _loc a1))),
                (meta_module_type _loc a2))
        | MePkg (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MePkg")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | MeAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | McNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "McNil")))),
                (meta_loc _loc a0))
        | McOr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "McOr")))),
                          (meta_loc _loc a0))), (meta_match_case _loc a1))),
                (meta_match_case _loc a2))
        | McArr (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "McArr")))),
                               (meta_loc _loc a0))), (meta_patt _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | McAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | MbNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "MbNil")))),
                (meta_loc _loc a0))
        | MbAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "MbAnd")))),
                          (meta_loc _loc a0))),
                     (meta_module_binding _loc a1))),
                (meta_module_binding _loc a2))
        | MbColEq (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "MbColEq")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | MbCol (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "MbCol")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | MbAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | RbNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "RbNil")))),
                (meta_loc _loc a0))
        | RbSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "RbSem")))),
                          (meta_loc _loc a0))), (meta_rec_binding _loc a1))),
                (meta_rec_binding _loc a2))
        | RbEq (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "RbEq")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_expr _loc a2))
        | RbAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | BiNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "BiNil")))),
                (meta_loc _loc a0))
        | BiAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "BiAnd")))),
                          (meta_loc _loc a0))), (meta_binding _loc a1))),
                (meta_binding _loc a2))
        | BiEq (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "BiEq")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_expr _loc a2))
        | BiAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | WcNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "WcNil")))),
                (meta_loc _loc a0))
        | WcTyp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "WcTyp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | WcMod (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "WcMod")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | WcTyS (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "WcTyS")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | WcMoS (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "WcMoS")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | WcAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "WcAnd")))),
                          (meta_loc _loc a0))), (meta_with_constr _loc a1))),
                (meta_with_constr _loc a2))
        | WcAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | SgNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "SgNil")))),
                (meta_loc _loc a0))
        | SgCls (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgCls")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | SgClt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgClt")))),
                     (meta_loc _loc a0))), (meta_class_type _loc a1))
        | SgSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "SgSem")))),
                          (meta_loc _loc a0))), (meta_sig_item _loc a1))),
                (meta_sig_item _loc a2))
        | SgDir (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "SgDir")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | SgExc (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgExc")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | SgExt (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "SgExt")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | SgInc (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgInc")))),
                     (meta_loc _loc a0))), (meta_module_type _loc a1))
        | SgMod (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "SgMod")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | SgRecMod (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgRecMod")))),
                     (meta_loc _loc a0))), (meta_module_binding _loc a1))
        | SgMty (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "SgMty")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_module_type _loc a2))
        | SgOpn (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgOpn")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | SgTyp (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "SgTyp")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | SgVal (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "SgVal")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | SgAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | MtNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "MtNil")))),
                (meta_loc _loc a0))
        | MtId (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MtId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | MtFun (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "MtFun")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_type _loc a3))
        | MtQuo (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MtQuo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | MtSig (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MtSig")))),
                     (meta_loc _loc a0))), (meta_sig_item _loc a1))
        | MtWit (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "MtWit")))),
                          (meta_loc _loc a0))), (meta_module_type _loc a1))),
                (meta_with_constr _loc a2))
        | MtOf (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "MtOf")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
        | MtAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | ExNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "ExNil")))),
                (meta_loc _loc a0))
        | ExId (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | ExAcc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExAcc")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAnt (a0,a1) -> PaAnt (a0, a1)
        | ExApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExApp")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAre (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExAre")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExArr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExArr")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExSem")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExAsf a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "ExAsf")))),
                (meta_loc _loc a0))
        | ExAsr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExAsr")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExAss (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExAss")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExChr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExChr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExCoe (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "ExCoe")))),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_ctyp _loc a2))), (meta_ctyp _loc a3))
        | ExFlo (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExFlo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExFor (a0,a1,a2,a3,a4,a5) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc,
                               (PaApp
                                  (_loc,
                                    (PaApp
                                       (_loc,
                                         (PaId
                                            (_loc, (IdUid (_loc, "ExFor")))),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_expr _loc a2))), (meta_expr _loc a3))),
                     (meta_direction_flag _loc a4))), (meta_expr _loc a5))
        | ExFun (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExFun")))),
                     (meta_loc _loc a0))), (meta_match_case _loc a1))
        | ExIfe (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "ExIfe")))),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | ExInt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExInt32 (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExInt32")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExInt64 (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExInt64")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExNativeInt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExNativeInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExLab (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExLaz (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExLaz")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExLet (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "ExLet")))),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_expr _loc a3))
        | ExLmd (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "ExLmd")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_expr _loc a2))), (meta_expr _loc a3))
        | ExMat (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExMat")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_match_case _loc a2))
        | ExNew (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExNew")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | ExObj (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExObj")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_class_str_item _loc a2))
        | ExOlb (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExOvr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExOvr")))),
                     (meta_loc _loc a0))), (meta_rec_binding _loc a1))
        | ExRec (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExRec")))),
                          (meta_loc _loc a0))), (meta_rec_binding _loc a1))),
                (meta_expr _loc a2))
        | ExSeq (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExSeq")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExSnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExSnd")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_string _loc a2))
        | ExSte (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExSte")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExStr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExStr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExTry (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExTry")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_match_case _loc a2))
        | ExTup (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExTup")))),
                     (meta_loc _loc a0))), (meta_expr _loc a1))
        | ExCom (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExCom")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExTyc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExTyc")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_ctyp _loc a2))
        | ExVrn (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | ExWhi (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExWhi")))),
                          (meta_loc _loc a0))), (meta_expr _loc a1))),
                (meta_expr _loc a2))
        | ExOpI (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExOpI")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_expr _loc a2))
        | ExFUN (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "ExFUN")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_expr _loc a2))
        | ExPkg (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "ExPkg")))),
                     (meta_loc _loc a0))), (meta_module_expr _loc a1))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | PaNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "PaNil")))),
                (meta_loc _loc a0))
        | PaId (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | PaAli (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaAli")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaAnt (a0,a1) -> PaAnt (a0, a1)
        | PaAny a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "PaAny")))),
                (meta_loc _loc a0))
        | PaApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaApp")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaArr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaArr")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaCom (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaCom")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaSem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaSem")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaChr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaChr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt32 (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaInt32")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaInt64 (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaInt64")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaNativeInt (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaNativeInt")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaFlo (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaFlo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaLab (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_patt _loc a2))
        | PaOlb (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_patt _loc a2))
        | PaOlbi (a0,a1,a2,a3) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc, (PaId (_loc, (IdUid (_loc, "PaOlbi")))),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_patt _loc a2))), (meta_expr _loc a3))
        | PaOrp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaOrp")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaRng (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaRng")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_patt _loc a2))
        | PaRec (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaRec")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaEq (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaEq")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_patt _loc a2))
        | PaStr (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaStr")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaTup (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaTup")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaTyc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "PaTyc")))),
                          (meta_loc _loc a0))), (meta_patt _loc a1))),
                (meta_ctyp _loc a2))
        | PaTyp (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaTyp")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | PaVrn (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | PaLaz (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaLaz")))),
                     (meta_loc _loc a0))), (meta_patt _loc a1))
        | PaMod (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "PaMod")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | TyNil a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "TyNil")))),
                (meta_loc _loc a0))
        | TyAli (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyAli")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAny a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "TyAny")))),
                (meta_loc _loc a0))
        | TyApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyApp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyArr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyArr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyCls (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyCls")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | TyLab (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyLab")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | TyId (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyId")))),
                     (meta_loc _loc a0))), (meta_ident _loc a1))
        | TyMan (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyMan")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyDcl (a0,a1,a2,a3,a4) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc,
                          (PaApp
                             (_loc,
                               (PaApp
                                  (_loc,
                                    (PaId (_loc, (IdUid (_loc, "TyDcl")))),
                                    (meta_loc _loc a0))),
                               (meta_string _loc a1))),
                          (meta_list meta_ctyp _loc a2))),
                     (meta_ctyp _loc a3))),
                (meta_list
                   (fun _loc  (a0,a1)  ->
                      PaTup
                        (_loc,
                          (PaCom
                             (_loc, (meta_ctyp _loc a0), (meta_ctyp _loc a1)))))
                   _loc a4))
        | TyObj (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyObj")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_row_var_flag _loc a2))
        | TyOlb (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyOlb")))),
                          (meta_loc _loc a0))), (meta_string _loc a1))),
                (meta_ctyp _loc a2))
        | TyPol (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyPol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyTypePol (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyTypePol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyQuo (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyQuo")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyQuP (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyQuP")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyQuM (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyQuM")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyAnP a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "TyAnP")))),
                (meta_loc _loc a0))
        | TyAnM a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "TyAnM")))),
                (meta_loc _loc a0))
        | TyVrn (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyVrn")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | TyRec (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyRec")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyCol (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyCol")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TySem (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TySem")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyCom (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyCom")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TySum (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TySum")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyOf (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyOf")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAnd (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyAnd")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyOr (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyOr")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyPrv (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyPrv")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyMut (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyMut")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyTup (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyTup")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TySta (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TySta")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyVrnEq (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyVrnEq")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnSup (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyVrnSup")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnInf (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyVrnInf")))),
                     (meta_loc _loc a0))), (meta_ctyp _loc a1))
        | TyVrnInfSup (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyVrnInfSup")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyAmp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyAmp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyOfAmp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "TyOfAmp")))),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | TyPkg (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "TyPkg")))),
                     (meta_loc _loc a0))), (meta_module_type _loc a1))
        | TyAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | IdAcc (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "IdAcc")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | IdApp (a0,a1,a2) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc,
                     (PaApp
                        (_loc, (PaId (_loc, (IdUid (_loc, "IdApp")))),
                          (meta_loc _loc a0))), (meta_ident _loc a1))),
                (meta_ident _loc a2))
        | IdLid (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "IdLid")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | IdUid (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "IdUid")))),
                     (meta_loc _loc a0))), (meta_string _loc a1))
        | IdAnt (a0,a1) -> PaAnt (a0, a1)
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | LNil  -> PaId (_loc, (IdUid (_loc, "LNil")))
        | LCons (a0,a1) ->
            PaApp
              (_loc,
                (PaApp
                   (_loc, (PaId (_loc, (IdUid (_loc, "LCons")))),
                     (mf_a _loc a0))), (meta_meta_list mf_a _loc a1))
        | LAnt a0 -> PaAnt (_loc, a0)
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | ONone  -> PaId (_loc, (IdUid (_loc, "ONone")))
        | OSome a0 ->
            PaApp
              (_loc, (PaId (_loc, (IdUid (_loc, "OSome")))), (mf_a _loc a0))
        | OAnt a0 -> PaAnt (_loc, a0)
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | RvRowVar  -> PaId (_loc, (IdUid (_loc, "RvRowVar")))
        | RvNil  -> PaId (_loc, (IdUid (_loc, "RvNil")))
        | RvAnt a0 -> PaAnt (_loc, a0)
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | OvOverride  -> PaId (_loc, (IdUid (_loc, "OvOverride")))
        | OvNil  -> PaId (_loc, (IdUid (_loc, "OvNil")))
        | OvAnt a0 -> PaAnt (_loc, a0)
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | ViVirtual  -> PaId (_loc, (IdUid (_loc, "ViVirtual")))
        | ViNil  -> PaId (_loc, (IdUid (_loc, "ViNil")))
        | ViAnt a0 -> PaAnt (_loc, a0)
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | PrPrivate  -> PaId (_loc, (IdUid (_loc, "PrPrivate")))
        | PrNil  -> PaId (_loc, (IdUid (_loc, "PrNil")))
        | PrAnt a0 -> PaAnt (_loc, a0)
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | MuMutable  -> PaId (_loc, (IdUid (_loc, "MuMutable")))
        | MuNil  -> PaId (_loc, (IdUid (_loc, "MuNil")))
        | MuAnt a0 -> PaAnt (_loc, a0)
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | DiTo  -> PaId (_loc, (IdUid (_loc, "DiTo")))
        | DiDownto  -> PaId (_loc, (IdUid (_loc, "DiDownto")))
        | DiAnt a0 -> PaAnt (_loc, a0)
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | ReRecursive  -> PaId (_loc, (IdUid (_loc, "ReRecursive")))
        | ReNil  -> PaId (_loc, (IdUid (_loc, "ReNil")))
        | ReAnt a0 -> PaAnt (_loc, a0)
    and meta_meta_bool: 'loc -> meta_bool -> 'result =
      fun _loc  ->
        function
        | BTrue  -> PaId (_loc, (IdUid (_loc, "BTrue")))
        | BFalse  -> PaId (_loc, (IdUid (_loc, "BFalse")))
        | BAnt a0 -> PaAnt (_loc, a0)
    end
  end