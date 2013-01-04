module type META_LOC =
  sig
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> PAst.patt
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> PAst.expr
  end
open StdLib
let _ = ()
include PAst
class print =
  object (self : 'self_type)
    inherit  printbase
    method class_str_item : 'fmt -> class_str_item -> 'result=
      fun fmt  ->
        function
        | `CrNil a0 -> Format.fprintf fmt "@[<1>(CrNil@ %a)@]" self#loc a0
        | `CrSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CrSem@ %a@ %a@ %a)@]" self#loc a0
              self#class_str_item a1 self#class_str_item a2
        | `CrCtr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CrCtr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `CrInh (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrInh@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#override_flag a1 self#class_expr a2 self#string a3
        | `CrIni (a0,a1) ->
            Format.fprintf fmt "@[<1>(CrIni@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `CrMth (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#string a1 self#override_flag a2
              self#private_flag a3 self#expr a4 self#ctyp a5
        | `CrVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(CrVal@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 self#override_flag a2 self#mutable_flag a3
              self#expr a4
        | `CrVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | `CrVvr (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrVvr@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#mutable_flag a2 self#ctyp a3
        | `CrAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CrAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_expr : 'fmt -> class_expr -> 'result=
      fun fmt  ->
        function
        | `CeNil a0 -> Format.fprintf fmt "@[<1>(CeNil@ %a)@]" self#loc a0
        | `CeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeApp@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#expr a2
        | `CeCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CeCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | `CeFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeFun@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_expr a2
        | `CeLet (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CeLet@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#class_expr a3
        | `CeStr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeStr@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | `CeTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeTyc@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_type a2
        | `CeAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeAnd@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | `CeEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeEq@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | `CeAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CeAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_sig_item : 'fmt -> class_sig_item -> 'result=
      fun fmt  ->
        function
        | `CgNil a0 -> Format.fprintf fmt "@[<1>(CgNil@ %a)@]" self#loc a0
        | `CgCtr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CgCtr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `CgSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CgSem@ %a@ %a@ %a)@]" self#loc a0
              self#class_sig_item a1 self#class_sig_item a2
        | `CgInh (a0,a1) ->
            Format.fprintf fmt "@[<1>(CgInh@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `CgMth (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CgMth@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | `CgVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(CgVal@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 self#mutable_flag a2 self#virtual_flag a3
              self#ctyp a4
        | `CgVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CgVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | `CgAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CgAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_type : 'fmt -> class_type -> 'result=
      fun fmt  ->
        function
        | `CtNil a0 -> Format.fprintf fmt "@[<1>(CtNil@ %a)@]" self#loc a0
        | `CtCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CtCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | `CtFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtFun@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_type a2
        | `CtSig (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtSig@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_sig_item a2
        | `CtAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtAnd@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | `CtCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtCol@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | `CtEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtEq@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | `CtAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CtAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method str_item : 'fmt -> str_item -> 'result=
      fun fmt  ->
        function
        | `StNil a0 -> Format.fprintf fmt "@[<1>(StNil@ %a)@]" self#loc a0
        | `StCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(StCls@ %a@ %a)@]" self#loc a0
              self#class_expr a1
        | `StClt (a0,a1) ->
            Format.fprintf fmt "@[<1>(StClt@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `StSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StSem@ %a@ %a@ %a)@]" self#loc a0
              self#str_item a1 self#str_item a2
        | `StDir (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StDir@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | `StExc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StExc@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 (self#meta_option (fun self  -> self#ident)) a2
        | `StExp (a0,a1) ->
            Format.fprintf fmt "@[<1>(StExp@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `StExt (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(StExt@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | `StInc (a0,a1) ->
            Format.fprintf fmt "@[<1>(StInc@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | `StMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StMod@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_expr a2
        | `StRecMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(StRecMod@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | `StMty (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StMty@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | `StOpn (a0,a1) ->
            Format.fprintf fmt "@[<1>(StOpn@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `StTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(StTyp@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `StVal (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StVal@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2
        | `StAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(StAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_expr : 'fmt -> module_expr -> 'result=
      fun fmt  ->
        function
        | `MeNil a0 -> Format.fprintf fmt "@[<1>(MeNil@ %a)@]" self#loc a0
        | `MeId (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `MeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MeApp@ %a@ %a@ %a)@]" self#loc a0
              self#module_expr a1 self#module_expr a2
        | `MeFun (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MeFun@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_expr a3
        | `MeStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeStr@ %a@ %a)@]" self#loc a0
              self#str_item a1
        | `MeTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MeTyc@ %a@ %a@ %a)@]" self#loc a0
              self#module_expr a1 self#module_type a2
        | `MePkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(MePkg@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `MeAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method match_case : 'fmt -> match_case -> 'result=
      fun fmt  ->
        function
        | `McNil a0 -> Format.fprintf fmt "@[<1>(McNil@ %a)@]" self#loc a0
        | `McOr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(McOr@ %a@ %a@ %a)@]" self#loc a0
              self#match_case a1 self#match_case a2
        | `McArr (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(McArr@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2 self#expr a3
        | `McAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(McAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_binding : 'fmt -> module_binding -> 'result=
      fun fmt  ->
        function
        | `MbNil a0 -> Format.fprintf fmt "@[<1>(MbNil@ %a)@]" self#loc a0
        | `MbAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MbAnd@ %a@ %a@ %a)@]" self#loc a0
              self#module_binding a1 self#module_binding a2
        | `MbColEq (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MbColEq@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_expr a3
        | `MbCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MbCol@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | `MbAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MbAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method rec_binding : 'fmt -> rec_binding -> 'result=
      fun fmt  ->
        function
        | `RbNil a0 -> Format.fprintf fmt "@[<1>(RbNil@ %a)@]" self#loc a0
        | `RbSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(RbSem@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#rec_binding a2
        | `RbEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(RbEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | `RbAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(RbAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method binding : 'fmt -> binding -> 'result=
      fun fmt  ->
        function
        | `BiNil a0 -> Format.fprintf fmt "@[<1>(BiNil@ %a)@]" self#loc a0
        | `BiAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(BiAnd@ %a@ %a@ %a)@]" self#loc a0
              self#binding a1 self#binding a2
        | `BiEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(BiEq@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2
        | `BiAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(BiAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method with_constr : 'fmt -> with_constr -> 'result=
      fun fmt  ->
        function
        | `WcNil a0 -> Format.fprintf fmt "@[<1>(WcNil@ %a)@]" self#loc a0
        | `WcTyp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcTyp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `WcMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcMod@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `WcTyS (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcTyS@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `WcMoS (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcMoS@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `WcAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcAnd@ %a@ %a@ %a)@]" self#loc a0
              self#with_constr a1 self#with_constr a2
        | `WcAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(WcAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method sig_item : 'fmt -> sig_item -> 'result=
      fun fmt  ->
        function
        | `SgNil a0 -> Format.fprintf fmt "@[<1>(SgNil@ %a)@]" self#loc a0
        | `SgCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgCls@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `SgClt (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgClt@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | `SgSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgSem@ %a@ %a@ %a)@]" self#loc a0
              self#sig_item a1 self#sig_item a2
        | `SgDir (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgDir@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | `SgExc (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgExc@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `SgExt (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(SgExt@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | `SgInc (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgInc@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | `SgMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgMod@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | `SgRecMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgRecMod@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | `SgMty (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgMty@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | `SgOpn (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgOpn@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `SgTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgTyp@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `SgVal (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgVal@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | `SgAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_type : 'fmt -> module_type -> 'result=
      fun fmt  ->
        function
        | `MtNil a0 -> Format.fprintf fmt "@[<1>(MtNil@ %a)@]" self#loc a0
        | `MtId (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `MtFun (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MtFun@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_type a3
        | `MtQuo (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtQuo@ %a@ %a)@]" self#loc a0
              self#string a1
        | `MtSig (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtSig@ %a@ %a)@]" self#loc a0
              self#sig_item a1
        | `MtWit (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MtWit@ %a@ %a@ %a)@]" self#loc a0
              self#module_type a1 self#with_constr a2
        | `MtOf (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtOf@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | `MtAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method expr : 'fmt -> expr -> 'result=
      fun fmt  ->
        function
        | `ExNil a0 -> Format.fprintf fmt "@[<1>(ExNil@ %a)@]" self#loc a0
        | `ExId (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `ExAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAcc@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExAnt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExApp@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExAre (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAre@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExArr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExArr@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `ExSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSem@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExAsf a0 -> Format.fprintf fmt "@[<1>(ExAsf@ %a)@]" self#loc a0
        | `ExAsr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExAsr@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `ExAss (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAss@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExChr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExChr@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExCoe (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExCoe@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#ctyp a2 self#ctyp a3
        | `ExFlo (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExFlo@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExFor (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(ExFor@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#string a1 self#expr a2 self#expr a3
              self#direction_flag a4 self#expr a5
        | `ExFun (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExFun@ %a@ %a)@]" self#loc a0
              self#match_case a1
        | `ExIfe (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExIfe@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2 self#expr a3
        | `ExInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExInt32 (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt32@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExInt64 (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt64@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExNativeInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExNativeInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | `ExLaz (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExLaz@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `ExLet (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExLet@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#expr a3
        | `ExLmd (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExLmd@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_expr a2 self#expr a3
        | `ExMat (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExMat@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | `ExNew (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExNew@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `ExObj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExObj@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | `ExOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | `ExOvr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExOvr@ %a@ %a)@]" self#loc a0
              self#rec_binding a1
        | `ExRec (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExRec@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#expr a2
        | `ExSeq (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExSeq@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `ExSnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSnd@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#string a2
        | `ExSte (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSte@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExStr@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExTry (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExTry@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | `ExTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExTup@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | `ExCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExCom@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExTyc@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#ctyp a2
        | `ExVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `ExWhi (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExWhi@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | `ExOpI (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExOpI@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | `ExFUN (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExFUN@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | `ExPkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExPkg@ %a@ %a)@]" self#loc a0
              self#module_expr a1
    method patt : 'fmt -> patt -> 'result=
      fun fmt  ->
        function
        | `PaNil a0 -> Format.fprintf fmt "@[<1>(PaNil@ %a)@]" self#loc a0
        | `PaId (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `PaAli (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaAli@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaAnt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaAny a0 -> Format.fprintf fmt "@[<1>(PaAny@ %a)@]" self#loc a0
        | `PaApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaApp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaArr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaArr@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | `PaCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaCom@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaSem@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaChr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaChr@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaInt32 (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt32@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaInt64 (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt64@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaNativeInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaNativeInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaFlo (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaFlo@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2
        | `PaOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2
        | `PaOlbi (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(PaOlbi@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2 self#expr a3
        | `PaOrp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaOrp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaRng (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaRng@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | `PaRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaRec@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | `PaEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#patt a2
        | `PaStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaStr@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaTup@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | `PaTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaTyc@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#ctyp a2
        | `PaTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaTyp@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `PaVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `PaLaz (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaLaz@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | `PaMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaMod@ %a@ %a)@]" self#loc a0
              self#string a1
    method ctyp : 'fmt -> ctyp -> 'result=
      fun fmt  ->
        function
        | `TyNil a0 -> Format.fprintf fmt "@[<1>(TyNil@ %a)@]" self#loc a0
        | `TyAli (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAli@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyAny a0 -> Format.fprintf fmt "@[<1>(TyAny@ %a)@]" self#loc a0
        | `TyApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyApp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyArr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyArr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyCls@ %a@ %a)@]" self#loc a0
              self#ident a1
        | `TyLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | `TyId (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | `TyMan (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyMan@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyDcl (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(TyDcl@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 (self#list (fun self  -> self#ctyp)) a2
              self#ctyp a3
              (self#list
                 (fun self  fmt  (a0,a1)  ->
                    Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#ctyp a0
                      self#ctyp a1)) a4
        | `TyObj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyObj@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#row_var_flag a2
        | `TyOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | `TyPol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyPol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyTypePol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyTypePol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyQuo (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuo@ %a@ %a)@]" self#loc a0
              self#string a1
        | `TyQuP (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuP@ %a@ %a)@]" self#loc a0
              self#string a1
        | `TyQuM (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuM@ %a@ %a)@]" self#loc a0
              self#string a1
        | `TyAnP a0 -> Format.fprintf fmt "@[<1>(TyAnP@ %a)@]" self#loc a0
        | `TyAnM a0 -> Format.fprintf fmt "@[<1>(TyAnM@ %a)@]" self#loc a0
        | `TyVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | `TyRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyRec@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `TyCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyCol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TySem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TySem@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyCom@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TySum (a0,a1) ->
            Format.fprintf fmt "@[<1>(TySum@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `TyOf (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOf@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAnd@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyOr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyPrv (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyPrv@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `TyMut (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyMut@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `TyTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyTup@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | `TySta (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TySta@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyVrnEq (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnEq@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnSup (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnSup@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnInf (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnInf@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | `TyVrnInfSup (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyVrnInfSup@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyOfAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOfAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | `TyPkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyPkg@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | `TyAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method ident : 'fmt -> ident -> 'result=
      fun fmt  ->
        function
        | `IdAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(IdAcc@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `IdApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(IdApp@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | `IdLid (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdLid@ %a@ %a)@]" self#loc a0
              self#string a1
        | `IdUid (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdUid@ %a@ %a)@]" self#loc a0
              self#string a1
        | `IdAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_list -> 'result=
      fun mf_a  fmt  ->
        function
        | `LNil -> Format.fprintf fmt "LNil"
        | `LCons (a0,a1) ->
            Format.fprintf fmt "@[<1>(LCons@ %a@ %a)@]" (mf_a self) a0
              (self#meta_list mf_a) a1
        | `LAnt a0 -> Format.fprintf fmt "@[<1>(LAnt@ %a)@]" self#string a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_option -> 'result=
      fun mf_a  fmt  ->
        function
        | `ONone -> Format.fprintf fmt "ONone"
        | `OSome a0 -> Format.fprintf fmt "@[<1>(OSome@ %a)@]" (mf_a self) a0
        | `OAnt a0 -> Format.fprintf fmt "@[<1>(OAnt@ %a)@]" self#string a0
    method row_var_flag : 'fmt -> row_var_flag -> 'result=
      fun fmt  ->
        function
        | `RvRowVar -> Format.fprintf fmt "RvRowVar"
        | `RvNil -> Format.fprintf fmt "RvNil"
        | `RvAnt a0 -> Format.fprintf fmt "@[<1>(RvAnt@ %a)@]" self#string a0
    method override_flag : 'fmt -> override_flag -> 'result=
      fun fmt  ->
        function
        | `OvOverride -> Format.fprintf fmt "OvOverride"
        | `OvNil -> Format.fprintf fmt "OvNil"
        | `OvAnt a0 -> Format.fprintf fmt "@[<1>(OvAnt@ %a)@]" self#string a0
    method virtual_flag : 'fmt -> virtual_flag -> 'result=
      fun fmt  ->
        function
        | `ViVirtual -> Format.fprintf fmt "ViVirtual"
        | `ViNil -> Format.fprintf fmt "ViNil"
        | `ViAnt a0 -> Format.fprintf fmt "@[<1>(ViAnt@ %a)@]" self#string a0
    method private_flag : 'fmt -> private_flag -> 'result=
      fun fmt  ->
        function
        | `PrPrivate -> Format.fprintf fmt "PrPrivate"
        | `PrNil -> Format.fprintf fmt "PrNil"
        | `PrAnt a0 -> Format.fprintf fmt "@[<1>(PrAnt@ %a)@]" self#string a0
    method mutable_flag : 'fmt -> mutable_flag -> 'result=
      fun fmt  ->
        function
        | `MuMutable -> Format.fprintf fmt "MuMutable"
        | `MuNil -> Format.fprintf fmt "MuNil"
        | `MuAnt a0 -> Format.fprintf fmt "@[<1>(MuAnt@ %a)@]" self#string a0
    method direction_flag : 'fmt -> direction_flag -> 'result=
      fun fmt  ->
        function
        | `DiTo -> Format.fprintf fmt "DiTo"
        | `DiDownto -> Format.fprintf fmt "DiDownto"
        | `DiAnt a0 -> Format.fprintf fmt "@[<1>(DiAnt@ %a)@]" self#string a0
    method rec_flag : 'fmt -> rec_flag -> 'result=
      fun fmt  ->
        function
        | `ReRecursive -> Format.fprintf fmt "ReRecursive"
        | `ReNil -> Format.fprintf fmt "ReNil"
        | `ReAnt a0 -> Format.fprintf fmt "@[<1>(ReAnt@ %a)@]" self#string a0
    method meta_bool : 'fmt -> meta_bool -> 'result=
      fun fmt  ->
        function
        | `BTrue -> Format.fprintf fmt "BTrue"
        | `BFalse -> Format.fprintf fmt "BFalse"
        | `BAnt a0 -> Format.fprintf fmt "@[<1>(BAnt@ %a)@]" self#string a0
    method loc : 'fmt -> loc -> 'result= fun fmt  a0  -> self#fanloc_t fmt a0
    method fanloc_t : 'fmt -> FanLoc.t -> 'result= self#unknown
  end
class fold =
  object (self : 'self_type)
    inherit  foldbase
    method class_str_item : class_str_item -> 'self_type=
      function
      | `CrNil a0 -> self#loc a0
      | `CrSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_str_item a1 in self#class_str_item a2
      | `CrCtr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `CrInh (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#override_flag a1 in
          let self = self#class_expr a2 in self#string a3
      | `CrIni (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `CrMth (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#override_flag a2 in
          let self = self#private_flag a3 in
          let self = self#expr a4 in self#ctyp a5
      | `CrVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#override_flag a2 in
          let self = self#mutable_flag a3 in self#expr a4
      | `CrVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | `CrVvr (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#mutable_flag a2 in self#ctyp a3
      | `CrAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_expr : class_expr -> 'self_type=
      function
      | `CeNil a0 -> self#loc a0
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
      | `CeStr (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | `CeTyc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_type a2
      | `CeAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | `CeEq (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_expr a1 in self#class_expr a2
      | `CeAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_sig_item : class_sig_item -> 'self_type=
      function
      | `CgNil a0 -> self#loc a0
      | `CgCtr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `CgSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#class_sig_item a1 in self#class_sig_item a2
      | `CgInh (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `CgMth (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | `CgVal (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#mutable_flag a2 in
          let self = self#virtual_flag a3 in self#ctyp a4
      | `CgVir (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#private_flag a2 in self#ctyp a3
      | `CgAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method class_type : class_type -> 'self_type=
      function
      | `CtNil a0 -> self#loc a0
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
      | `CtAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method str_item : str_item -> 'self_type=
      function
      | `StNil a0 -> self#loc a0
      | `StCls (a0,a1) -> let self = self#loc a0 in self#class_expr a1
      | `StClt (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `StSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#str_item a1 in self#str_item a2
      | `StDir (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | `StExc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in
          self#meta_option (fun self  -> self#ident) a2
      | `StExp (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `StExt (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | `StInc (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | `StMod (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_expr a2
      | `StRecMod (a0,a1) -> let self = self#loc a0 in self#module_binding a1
      | `StMty (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | `StOpn (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `StTyp (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `StVal (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in self#binding a2
      | `StAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_expr : module_expr -> 'self_type=
      function
      | `MeNil a0 -> self#loc a0
      | `MeId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `MeApp (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_expr a2
      | `MeFun (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_expr a3
      | `MeStr (a0,a1) -> let self = self#loc a0 in self#str_item a1
      | `MeTyc (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_expr a1 in self#module_type a2
      | `MePkg (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `MeAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method match_case : match_case -> 'self_type=
      function
      | `McNil a0 -> self#loc a0
      | `McOr (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#match_case a1 in self#match_case a2
      | `McArr (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#patt a1 in let self = self#expr a2 in self#expr a3
      | `McAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_binding : module_binding -> 'self_type=
      function
      | `MbNil a0 -> self#loc a0
      | `MbAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_binding a1 in self#module_binding a2
      | `MbColEq (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_expr a3
      | `MbCol (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | `MbAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method rec_binding : rec_binding -> 'self_type=
      function
      | `RbNil a0 -> self#loc a0
      | `RbSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#rec_binding a2
      | `RbEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | `RbAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method binding : binding -> 'self_type=
      function
      | `BiNil a0 -> self#loc a0
      | `BiAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#binding a1 in self#binding a2
      | `BiEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#expr a2
      | `BiAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method with_constr : with_constr -> 'self_type=
      function
      | `WcNil a0 -> self#loc a0
      | `WcTyp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `WcMod (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `WcTyS (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `WcMoS (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `WcAnd (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#with_constr a1 in self#with_constr a2
      | `WcAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method sig_item : sig_item -> 'self_type=
      function
      | `SgNil a0 -> self#loc a0
      | `SgCls (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `SgClt (a0,a1) -> let self = self#loc a0 in self#class_type a1
      | `SgSem (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#sig_item a1 in self#sig_item a2
      | `SgDir (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | `SgExc (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `SgExt (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#ctyp a2 in
          self#meta_list (fun self  -> self#string) a3
      | `SgInc (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | `SgMod (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | `SgRecMod (a0,a1) -> let self = self#loc a0 in self#module_binding a1
      | `SgMty (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#string a1 in self#module_type a2
      | `SgOpn (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `SgTyp (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `SgVal (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | `SgAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method module_type : module_type -> 'self_type=
      function
      | `MtNil a0 -> self#loc a0
      | `MtId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `MtFun (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_type a2 in self#module_type a3
      | `MtQuo (a0,a1) -> let self = self#loc a0 in self#string a1
      | `MtSig (a0,a1) -> let self = self#loc a0 in self#sig_item a1
      | `MtWit (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#module_type a1 in self#with_constr a2
      | `MtOf (a0,a1) -> let self = self#loc a0 in self#module_expr a1
      | `MtAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method expr : expr -> 'self_type=
      function
      | `ExNil a0 -> self#loc a0
      | `ExId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `ExAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExAnt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExAre (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExArr (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExSem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExAsf a0 -> self#loc a0
      | `ExAsr (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExAss (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExChr (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExCoe (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#ctyp a2 in self#ctyp a3
      | `ExFlo (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExFor (a0,a1,a2,a3,a4,a5) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#expr a2 in
          let self = self#expr a3 in
          let self = self#direction_flag a4 in self#expr a5
      | `ExFun (a0,a1) -> let self = self#loc a0 in self#match_case a1
      | `ExIfe (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#expr a1 in let self = self#expr a2 in self#expr a3
      | `ExInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExInt32 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExInt64 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExNativeInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | `ExLaz (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExLet (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#rec_flag a1 in
          let self = self#binding a2 in self#expr a3
      | `ExLmd (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#module_expr a2 in self#expr a3
      | `ExMat (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | `ExNew (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `ExObj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#patt a1 in self#class_str_item a2
      | `ExOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | `ExOvr (a0,a1) -> let self = self#loc a0 in self#rec_binding a1
      | `ExRec (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#rec_binding a1 in self#expr a2
      | `ExSeq (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExSnd (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#string a2
      | `ExSte (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExStr (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExTry (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#expr a1 in self#match_case a2
      | `ExTup (a0,a1) -> let self = self#loc a0 in self#expr a1
      | `ExCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExTyc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#ctyp a2
      | `ExVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `ExWhi (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#expr a1 in self#expr a2
      | `ExOpI (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#expr a2
      | `ExFUN (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#expr a2
      | `ExPkg (a0,a1) -> let self = self#loc a0 in self#module_expr a1
    method patt : patt -> 'self_type=
      function
      | `PaNil a0 -> self#loc a0
      | `PaId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `PaAli (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaAnt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaAny a0 -> self#loc a0
      | `PaApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaArr (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaSem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaChr (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaInt32 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaInt64 (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaNativeInt (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaFlo (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#patt a2
      | `PaOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#patt a2
      | `PaOlbi (a0,a1,a2,a3) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#patt a2 in self#expr a3
      | `PaOrp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaRng (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#patt a2
      | `PaRec (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaEq (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#patt a2
      | `PaStr (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaTup (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaTyc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#patt a1 in self#ctyp a2
      | `PaTyp (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `PaVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `PaLaz (a0,a1) -> let self = self#loc a0 in self#patt a1
      | `PaMod (a0,a1) -> let self = self#loc a0 in self#string a1
    method ctyp : ctyp -> 'self_type=
      function
      | `TyNil a0 -> self#loc a0
      | `TyAli (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyAny a0 -> self#loc a0
      | `TyApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyArr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyCls (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `TyLab (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | `TyId (a0,a1) -> let self = self#loc a0 in self#ident a1
      | `TyMan (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyDcl (a0,a1,a2,a3,a4) ->
          let self = self#loc a0 in
          let self = self#string a1 in
          let self = self#list (fun self  -> self#ctyp) a2 in
          let self = self#ctyp a3 in
          self#list
            (fun self  (a0,a1)  -> let self = self#ctyp a0 in self#ctyp a1)
            a4
      | `TyObj (a0,a1,a2) ->
          let self = self#loc a0 in
          let self = self#ctyp a1 in self#row_var_flag a2
      | `TyOlb (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#string a1 in self#ctyp a2
      | `TyPol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyTypePol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyQuo (a0,a1) -> let self = self#loc a0 in self#string a1
      | `TyQuP (a0,a1) -> let self = self#loc a0 in self#string a1
      | `TyQuM (a0,a1) -> let self = self#loc a0 in self#string a1
      | `TyAnP a0 -> self#loc a0
      | `TyAnM a0 -> self#loc a0
      | `TyVrn (a0,a1) -> let self = self#loc a0 in self#string a1
      | `TyRec (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyCol (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TySem (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyCom (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TySum (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyOf (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyAnd (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyOr (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyPrv (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyMut (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyTup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TySta (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyVrnEq (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnSup (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnInf (a0,a1) -> let self = self#loc a0 in self#ctyp a1
      | `TyVrnInfSup (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyOfAmp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ctyp a1 in self#ctyp a2
      | `TyPkg (a0,a1) -> let self = self#loc a0 in self#module_type a1
      | `TyAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method ident : ident -> 'self_type=
      function
      | `IdAcc (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `IdApp (a0,a1,a2) ->
          let self = self#loc a0 in let self = self#ident a1 in self#ident a2
      | `IdLid (a0,a1) -> let self = self#loc a0 in self#string a1
      | `IdUid (a0,a1) -> let self = self#loc a0 in self#string a1
      | `IdAnt (a0,a1) -> let self = self#loc a0 in self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_list -> 'self_type=
      fun mf_a  ->
        function
        | `LNil -> self
        | `LCons (a0,a1) -> let self = mf_a self a0 in self#meta_list mf_a a1
        | `LAnt a0 -> self#string a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'all_a0 -> 'self_type) ->
          'all_a0 meta_option -> 'self_type=
      fun mf_a  ->
        function
        | `ONone -> self
        | `OSome a0 -> mf_a self a0
        | `OAnt a0 -> self#string a0
    method row_var_flag : row_var_flag -> 'self_type=
      function
      | `RvRowVar -> self
      | `RvNil -> self
      | `RvAnt a0 -> self#string a0
    method override_flag : override_flag -> 'self_type=
      function
      | `OvOverride -> self
      | `OvNil -> self
      | `OvAnt a0 -> self#string a0
    method virtual_flag : virtual_flag -> 'self_type=
      function
      | `ViVirtual -> self
      | `ViNil -> self
      | `ViAnt a0 -> self#string a0
    method private_flag : private_flag -> 'self_type=
      function
      | `PrPrivate -> self
      | `PrNil -> self
      | `PrAnt a0 -> self#string a0
    method mutable_flag : mutable_flag -> 'self_type=
      function
      | `MuMutable -> self
      | `MuNil -> self
      | `MuAnt a0 -> self#string a0
    method direction_flag : direction_flag -> 'self_type=
      function
      | `DiTo -> self
      | `DiDownto -> self
      | `DiAnt a0 -> self#string a0
    method rec_flag : rec_flag -> 'self_type=
      function
      | `ReRecursive -> self
      | `ReNil -> self
      | `ReAnt a0 -> self#string a0
    method meta_bool : meta_bool -> 'self_type=
      function
      | `BTrue -> self
      | `BFalse -> self
      | `BAnt a0 -> self#string a0
    method loc : loc -> 'self_type= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> 'self_type= self#unknown
  end
class map =
  object (self : 'self_type)
    inherit  mapbase
    method class_str_item : class_str_item -> class_str_item=
      function
      | `CrNil a0 -> `CrNil (self#loc a0)
      | `CrSem (a0,a1,a2) ->
          `CrSem
            ((self#loc a0), (self#class_str_item a1),
              (self#class_str_item a2))
      | `CrCtr (a0,a1,a2) ->
          `CrCtr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `CrInh (a0,a1,a2,a3) ->
          `CrInh
            ((self#loc a0), (self#override_flag a1), (self#class_expr a2),
              (self#string a3))
      | `CrIni (a0,a1) -> `CrIni ((self#loc a0), (self#expr a1))
      | `CrMth (a0,a1,a2,a3,a4,a5) ->
          `CrMth
            ((self#loc a0), (self#string a1), (self#override_flag a2),
              (self#private_flag a3), (self#expr a4), (self#ctyp a5))
      | `CrVal (a0,a1,a2,a3,a4) ->
          `CrVal
            ((self#loc a0), (self#string a1), (self#override_flag a2),
              (self#mutable_flag a3), (self#expr a4))
      | `CrVir (a0,a1,a2,a3) ->
          `CrVir
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | `CrVvr (a0,a1,a2,a3) ->
          `CrVvr
            ((self#loc a0), (self#string a1), (self#mutable_flag a2),
              (self#ctyp a3))
      | `CrAnt (a0,a1) -> `CrAnt ((self#loc a0), (self#string a1))
    method class_expr : class_expr -> class_expr=
      function
      | `CeNil a0 -> `CeNil (self#loc a0)
      | `CeApp (a0,a1,a2) ->
          `CeApp ((self#loc a0), (self#class_expr a1), (self#expr a2))
      | `CeCon (a0,a1,a2,a3) ->
          `CeCon
            ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
              (self#ctyp a3))
      | `CeFun (a0,a1,a2) ->
          `CeFun ((self#loc a0), (self#patt a1), (self#class_expr a2))
      | `CeLet (a0,a1,a2,a3) ->
          `CeLet
            ((self#loc a0), (self#rec_flag a1), (self#binding a2),
              (self#class_expr a3))
      | `CeStr (a0,a1,a2) ->
          `CeStr ((self#loc a0), (self#patt a1), (self#class_str_item a2))
      | `CeTyc (a0,a1,a2) ->
          `CeTyc ((self#loc a0), (self#class_expr a1), (self#class_type a2))
      | `CeAnd (a0,a1,a2) ->
          `CeAnd ((self#loc a0), (self#class_expr a1), (self#class_expr a2))
      | `CeEq (a0,a1,a2) ->
          `CeEq ((self#loc a0), (self#class_expr a1), (self#class_expr a2))
      | `CeAnt (a0,a1) -> `CeAnt ((self#loc a0), (self#string a1))
    method class_sig_item : class_sig_item -> class_sig_item=
      function
      | `CgNil a0 -> `CgNil (self#loc a0)
      | `CgCtr (a0,a1,a2) ->
          `CgCtr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `CgSem (a0,a1,a2) ->
          `CgSem
            ((self#loc a0), (self#class_sig_item a1),
              (self#class_sig_item a2))
      | `CgInh (a0,a1) -> `CgInh ((self#loc a0), (self#class_type a1))
      | `CgMth (a0,a1,a2,a3) ->
          `CgMth
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | `CgVal (a0,a1,a2,a3,a4) ->
          `CgVal
            ((self#loc a0), (self#string a1), (self#mutable_flag a2),
              (self#virtual_flag a3), (self#ctyp a4))
      | `CgVir (a0,a1,a2,a3) ->
          `CgVir
            ((self#loc a0), (self#string a1), (self#private_flag a2),
              (self#ctyp a3))
      | `CgAnt (a0,a1) -> `CgAnt ((self#loc a0), (self#string a1))
    method class_type : class_type -> class_type=
      function
      | `CtNil a0 -> `CtNil (self#loc a0)
      | `CtCon (a0,a1,a2,a3) ->
          `CtCon
            ((self#loc a0), (self#virtual_flag a1), (self#ident a2),
              (self#ctyp a3))
      | `CtFun (a0,a1,a2) ->
          `CtFun ((self#loc a0), (self#ctyp a1), (self#class_type a2))
      | `CtSig (a0,a1,a2) ->
          `CtSig ((self#loc a0), (self#ctyp a1), (self#class_sig_item a2))
      | `CtAnd (a0,a1,a2) ->
          `CtAnd ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | `CtCol (a0,a1,a2) ->
          `CtCol ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | `CtEq (a0,a1,a2) ->
          `CtEq ((self#loc a0), (self#class_type a1), (self#class_type a2))
      | `CtAnt (a0,a1) -> `CtAnt ((self#loc a0), (self#string a1))
    method str_item : str_item -> str_item=
      function
      | `StNil a0 -> `StNil (self#loc a0)
      | `StCls (a0,a1) -> `StCls ((self#loc a0), (self#class_expr a1))
      | `StClt (a0,a1) -> `StClt ((self#loc a0), (self#class_type a1))
      | `StSem (a0,a1,a2) ->
          `StSem ((self#loc a0), (self#str_item a1), (self#str_item a2))
      | `StDir (a0,a1,a2) ->
          `StDir ((self#loc a0), (self#string a1), (self#expr a2))
      | `StExc (a0,a1,a2) ->
          `StExc
            ((self#loc a0), (self#ctyp a1),
              (self#meta_option (fun self  -> self#ident) a2))
      | `StExp (a0,a1) -> `StExp ((self#loc a0), (self#expr a1))
      | `StExt (a0,a1,a2,a3) ->
          `StExt
            ((self#loc a0), (self#string a1), (self#ctyp a2),
              (self#meta_list (fun self  -> self#string) a3))
      | `StInc (a0,a1) -> `StInc ((self#loc a0), (self#module_expr a1))
      | `StMod (a0,a1,a2) ->
          `StMod ((self#loc a0), (self#string a1), (self#module_expr a2))
      | `StRecMod (a0,a1) ->
          `StRecMod ((self#loc a0), (self#module_binding a1))
      | `StMty (a0,a1,a2) ->
          `StMty ((self#loc a0), (self#string a1), (self#module_type a2))
      | `StOpn (a0,a1) -> `StOpn ((self#loc a0), (self#ident a1))
      | `StTyp (a0,a1) -> `StTyp ((self#loc a0), (self#ctyp a1))
      | `StVal (a0,a1,a2) ->
          `StVal ((self#loc a0), (self#rec_flag a1), (self#binding a2))
      | `StAnt (a0,a1) -> `StAnt ((self#loc a0), (self#string a1))
    method module_expr : module_expr -> module_expr=
      function
      | `MeNil a0 -> `MeNil (self#loc a0)
      | `MeId (a0,a1) -> `MeId ((self#loc a0), (self#ident a1))
      | `MeApp (a0,a1,a2) ->
          `MeApp
            ((self#loc a0), (self#module_expr a1), (self#module_expr a2))
      | `MeFun (a0,a1,a2,a3) ->
          `MeFun
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_expr a3))
      | `MeStr (a0,a1) -> `MeStr ((self#loc a0), (self#str_item a1))
      | `MeTyc (a0,a1,a2) ->
          `MeTyc
            ((self#loc a0), (self#module_expr a1), (self#module_type a2))
      | `MePkg (a0,a1) -> `MePkg ((self#loc a0), (self#expr a1))
      | `MeAnt (a0,a1) -> `MeAnt ((self#loc a0), (self#string a1))
    method match_case : match_case -> match_case=
      function
      | `McNil a0 -> `McNil (self#loc a0)
      | `McOr (a0,a1,a2) ->
          `McOr ((self#loc a0), (self#match_case a1), (self#match_case a2))
      | `McArr (a0,a1,a2,a3) ->
          `McArr
            ((self#loc a0), (self#patt a1), (self#expr a2), (self#expr a3))
      | `McAnt (a0,a1) -> `McAnt ((self#loc a0), (self#string a1))
    method module_binding : module_binding -> module_binding=
      function
      | `MbNil a0 -> `MbNil (self#loc a0)
      | `MbAnd (a0,a1,a2) ->
          `MbAnd
            ((self#loc a0), (self#module_binding a1),
              (self#module_binding a2))
      | `MbColEq (a0,a1,a2,a3) ->
          `MbColEq
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_expr a3))
      | `MbCol (a0,a1,a2) ->
          `MbCol ((self#loc a0), (self#string a1), (self#module_type a2))
      | `MbAnt (a0,a1) -> `MbAnt ((self#loc a0), (self#string a1))
    method rec_binding : rec_binding -> rec_binding=
      function
      | `RbNil a0 -> `RbNil (self#loc a0)
      | `RbSem (a0,a1,a2) ->
          `RbSem
            ((self#loc a0), (self#rec_binding a1), (self#rec_binding a2))
      | `RbEq (a0,a1,a2) ->
          `RbEq ((self#loc a0), (self#ident a1), (self#expr a2))
      | `RbAnt (a0,a1) -> `RbAnt ((self#loc a0), (self#string a1))
    method binding : binding -> binding=
      function
      | `BiNil a0 -> `BiNil (self#loc a0)
      | `BiAnd (a0,a1,a2) ->
          `BiAnd ((self#loc a0), (self#binding a1), (self#binding a2))
      | `BiEq (a0,a1,a2) ->
          `BiEq ((self#loc a0), (self#patt a1), (self#expr a2))
      | `BiAnt (a0,a1) -> `BiAnt ((self#loc a0), (self#string a1))
    method with_constr : with_constr -> with_constr=
      function
      | `WcNil a0 -> `WcNil (self#loc a0)
      | `WcTyp (a0,a1,a2) ->
          `WcTyp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `WcMod (a0,a1,a2) ->
          `WcMod ((self#loc a0), (self#ident a1), (self#ident a2))
      | `WcTyS (a0,a1,a2) ->
          `WcTyS ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `WcMoS (a0,a1,a2) ->
          `WcMoS ((self#loc a0), (self#ident a1), (self#ident a2))
      | `WcAnd (a0,a1,a2) ->
          `WcAnd
            ((self#loc a0), (self#with_constr a1), (self#with_constr a2))
      | `WcAnt (a0,a1) -> `WcAnt ((self#loc a0), (self#string a1))
    method sig_item : sig_item -> sig_item=
      function
      | `SgNil a0 -> `SgNil (self#loc a0)
      | `SgCls (a0,a1) -> `SgCls ((self#loc a0), (self#class_type a1))
      | `SgClt (a0,a1) -> `SgClt ((self#loc a0), (self#class_type a1))
      | `SgSem (a0,a1,a2) ->
          `SgSem ((self#loc a0), (self#sig_item a1), (self#sig_item a2))
      | `SgDir (a0,a1,a2) ->
          `SgDir ((self#loc a0), (self#string a1), (self#expr a2))
      | `SgExc (a0,a1) -> `SgExc ((self#loc a0), (self#ctyp a1))
      | `SgExt (a0,a1,a2,a3) ->
          `SgExt
            ((self#loc a0), (self#string a1), (self#ctyp a2),
              (self#meta_list (fun self  -> self#string) a3))
      | `SgInc (a0,a1) -> `SgInc ((self#loc a0), (self#module_type a1))
      | `SgMod (a0,a1,a2) ->
          `SgMod ((self#loc a0), (self#string a1), (self#module_type a2))
      | `SgRecMod (a0,a1) ->
          `SgRecMod ((self#loc a0), (self#module_binding a1))
      | `SgMty (a0,a1,a2) ->
          `SgMty ((self#loc a0), (self#string a1), (self#module_type a2))
      | `SgOpn (a0,a1) -> `SgOpn ((self#loc a0), (self#ident a1))
      | `SgTyp (a0,a1) -> `SgTyp ((self#loc a0), (self#ctyp a1))
      | `SgVal (a0,a1,a2) ->
          `SgVal ((self#loc a0), (self#string a1), (self#ctyp a2))
      | `SgAnt (a0,a1) -> `SgAnt ((self#loc a0), (self#string a1))
    method module_type : module_type -> module_type=
      function
      | `MtNil a0 -> `MtNil (self#loc a0)
      | `MtId (a0,a1) -> `MtId ((self#loc a0), (self#ident a1))
      | `MtFun (a0,a1,a2,a3) ->
          `MtFun
            ((self#loc a0), (self#string a1), (self#module_type a2),
              (self#module_type a3))
      | `MtQuo (a0,a1) -> `MtQuo ((self#loc a0), (self#string a1))
      | `MtSig (a0,a1) -> `MtSig ((self#loc a0), (self#sig_item a1))
      | `MtWit (a0,a1,a2) ->
          `MtWit
            ((self#loc a0), (self#module_type a1), (self#with_constr a2))
      | `MtOf (a0,a1) -> `MtOf ((self#loc a0), (self#module_expr a1))
      | `MtAnt (a0,a1) -> `MtAnt ((self#loc a0), (self#string a1))
    method expr : expr -> expr=
      function
      | `ExNil a0 -> `ExNil (self#loc a0)
      | `ExId (a0,a1) -> `ExId ((self#loc a0), (self#ident a1))
      | `ExAcc (a0,a1,a2) ->
          `ExAcc ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExAnt (a0,a1) -> `ExAnt ((self#loc a0), (self#string a1))
      | `ExApp (a0,a1,a2) ->
          `ExApp ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExAre (a0,a1,a2) ->
          `ExAre ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExArr (a0,a1) -> `ExArr ((self#loc a0), (self#expr a1))
      | `ExSem (a0,a1,a2) ->
          `ExSem ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExAsf a0 -> `ExAsf (self#loc a0)
      | `ExAsr (a0,a1) -> `ExAsr ((self#loc a0), (self#expr a1))
      | `ExAss (a0,a1,a2) ->
          `ExAss ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExChr (a0,a1) -> `ExChr ((self#loc a0), (self#string a1))
      | `ExCoe (a0,a1,a2,a3) ->
          `ExCoe
            ((self#loc a0), (self#expr a1), (self#ctyp a2), (self#ctyp a3))
      | `ExFlo (a0,a1) -> `ExFlo ((self#loc a0), (self#string a1))
      | `ExFor (a0,a1,a2,a3,a4,a5) ->
          `ExFor
            ((self#loc a0), (self#string a1), (self#expr a2), (self#expr a3),
              (self#direction_flag a4), (self#expr a5))
      | `ExFun (a0,a1) -> `ExFun ((self#loc a0), (self#match_case a1))
      | `ExIfe (a0,a1,a2,a3) ->
          `ExIfe
            ((self#loc a0), (self#expr a1), (self#expr a2), (self#expr a3))
      | `ExInt (a0,a1) -> `ExInt ((self#loc a0), (self#string a1))
      | `ExInt32 (a0,a1) -> `ExInt32 ((self#loc a0), (self#string a1))
      | `ExInt64 (a0,a1) -> `ExInt64 ((self#loc a0), (self#string a1))
      | `ExNativeInt (a0,a1) ->
          `ExNativeInt ((self#loc a0), (self#string a1))
      | `ExLab (a0,a1,a2) ->
          `ExLab ((self#loc a0), (self#string a1), (self#expr a2))
      | `ExLaz (a0,a1) -> `ExLaz ((self#loc a0), (self#expr a1))
      | `ExLet (a0,a1,a2,a3) ->
          `ExLet
            ((self#loc a0), (self#rec_flag a1), (self#binding a2),
              (self#expr a3))
      | `ExLmd (a0,a1,a2,a3) ->
          `ExLmd
            ((self#loc a0), (self#string a1), (self#module_expr a2),
              (self#expr a3))
      | `ExMat (a0,a1,a2) ->
          `ExMat ((self#loc a0), (self#expr a1), (self#match_case a2))
      | `ExNew (a0,a1) -> `ExNew ((self#loc a0), (self#ident a1))
      | `ExObj (a0,a1,a2) ->
          `ExObj ((self#loc a0), (self#patt a1), (self#class_str_item a2))
      | `ExOlb (a0,a1,a2) ->
          `ExOlb ((self#loc a0), (self#string a1), (self#expr a2))
      | `ExOvr (a0,a1) -> `ExOvr ((self#loc a0), (self#rec_binding a1))
      | `ExRec (a0,a1,a2) ->
          `ExRec ((self#loc a0), (self#rec_binding a1), (self#expr a2))
      | `ExSeq (a0,a1) -> `ExSeq ((self#loc a0), (self#expr a1))
      | `ExSnd (a0,a1,a2) ->
          `ExSnd ((self#loc a0), (self#expr a1), (self#string a2))
      | `ExSte (a0,a1,a2) ->
          `ExSte ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExStr (a0,a1) -> `ExStr ((self#loc a0), (self#string a1))
      | `ExTry (a0,a1,a2) ->
          `ExTry ((self#loc a0), (self#expr a1), (self#match_case a2))
      | `ExTup (a0,a1) -> `ExTup ((self#loc a0), (self#expr a1))
      | `ExCom (a0,a1,a2) ->
          `ExCom ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExTyc (a0,a1,a2) ->
          `ExTyc ((self#loc a0), (self#expr a1), (self#ctyp a2))
      | `ExVrn (a0,a1) -> `ExVrn ((self#loc a0), (self#string a1))
      | `ExWhi (a0,a1,a2) ->
          `ExWhi ((self#loc a0), (self#expr a1), (self#expr a2))
      | `ExOpI (a0,a1,a2) ->
          `ExOpI ((self#loc a0), (self#ident a1), (self#expr a2))
      | `ExFUN (a0,a1,a2) ->
          `ExFUN ((self#loc a0), (self#string a1), (self#expr a2))
      | `ExPkg (a0,a1) -> `ExPkg ((self#loc a0), (self#module_expr a1))
    method patt : patt -> patt=
      function
      | `PaNil a0 -> `PaNil (self#loc a0)
      | `PaId (a0,a1) -> `PaId ((self#loc a0), (self#ident a1))
      | `PaAli (a0,a1,a2) ->
          `PaAli ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaAnt (a0,a1) -> `PaAnt ((self#loc a0), (self#string a1))
      | `PaAny a0 -> `PaAny (self#loc a0)
      | `PaApp (a0,a1,a2) ->
          `PaApp ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaArr (a0,a1) -> `PaArr ((self#loc a0), (self#patt a1))
      | `PaCom (a0,a1,a2) ->
          `PaCom ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaSem (a0,a1,a2) ->
          `PaSem ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaChr (a0,a1) -> `PaChr ((self#loc a0), (self#string a1))
      | `PaInt (a0,a1) -> `PaInt ((self#loc a0), (self#string a1))
      | `PaInt32 (a0,a1) -> `PaInt32 ((self#loc a0), (self#string a1))
      | `PaInt64 (a0,a1) -> `PaInt64 ((self#loc a0), (self#string a1))
      | `PaNativeInt (a0,a1) ->
          `PaNativeInt ((self#loc a0), (self#string a1))
      | `PaFlo (a0,a1) -> `PaFlo ((self#loc a0), (self#string a1))
      | `PaLab (a0,a1,a2) ->
          `PaLab ((self#loc a0), (self#string a1), (self#patt a2))
      | `PaOlb (a0,a1,a2) ->
          `PaOlb ((self#loc a0), (self#string a1), (self#patt a2))
      | `PaOlbi (a0,a1,a2,a3) ->
          `PaOlbi
            ((self#loc a0), (self#string a1), (self#patt a2), (self#expr a3))
      | `PaOrp (a0,a1,a2) ->
          `PaOrp ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaRng (a0,a1,a2) ->
          `PaRng ((self#loc a0), (self#patt a1), (self#patt a2))
      | `PaRec (a0,a1) -> `PaRec ((self#loc a0), (self#patt a1))
      | `PaEq (a0,a1,a2) ->
          `PaEq ((self#loc a0), (self#ident a1), (self#patt a2))
      | `PaStr (a0,a1) -> `PaStr ((self#loc a0), (self#string a1))
      | `PaTup (a0,a1) -> `PaTup ((self#loc a0), (self#patt a1))
      | `PaTyc (a0,a1,a2) ->
          `PaTyc ((self#loc a0), (self#patt a1), (self#ctyp a2))
      | `PaTyp (a0,a1) -> `PaTyp ((self#loc a0), (self#ident a1))
      | `PaVrn (a0,a1) -> `PaVrn ((self#loc a0), (self#string a1))
      | `PaLaz (a0,a1) -> `PaLaz ((self#loc a0), (self#patt a1))
      | `PaMod (a0,a1) -> `PaMod ((self#loc a0), (self#string a1))
    method ctyp : ctyp -> ctyp=
      function
      | `TyNil a0 -> `TyNil (self#loc a0)
      | `TyAli (a0,a1,a2) ->
          `TyAli ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyAny a0 -> `TyAny (self#loc a0)
      | `TyApp (a0,a1,a2) ->
          `TyApp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyArr (a0,a1,a2) ->
          `TyArr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyCls (a0,a1) -> `TyCls ((self#loc a0), (self#ident a1))
      | `TyLab (a0,a1,a2) ->
          `TyLab ((self#loc a0), (self#string a1), (self#ctyp a2))
      | `TyId (a0,a1) -> `TyId ((self#loc a0), (self#ident a1))
      | `TyMan (a0,a1,a2) ->
          `TyMan ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyDcl (a0,a1,a2,a3,a4) ->
          `TyDcl
            ((self#loc a0), (self#string a1),
              (self#list (fun self  -> self#ctyp) a2), (self#ctyp a3),
              (self#list
                 (fun self  (a0,a1)  -> ((self#ctyp a0), (self#ctyp a1))) a4))
      | `TyObj (a0,a1,a2) ->
          `TyObj ((self#loc a0), (self#ctyp a1), (self#row_var_flag a2))
      | `TyOlb (a0,a1,a2) ->
          `TyOlb ((self#loc a0), (self#string a1), (self#ctyp a2))
      | `TyPol (a0,a1,a2) ->
          `TyPol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyTypePol (a0,a1,a2) ->
          `TyTypePol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyQuo (a0,a1) -> `TyQuo ((self#loc a0), (self#string a1))
      | `TyQuP (a0,a1) -> `TyQuP ((self#loc a0), (self#string a1))
      | `TyQuM (a0,a1) -> `TyQuM ((self#loc a0), (self#string a1))
      | `TyAnP a0 -> `TyAnP (self#loc a0)
      | `TyAnM a0 -> `TyAnM (self#loc a0)
      | `TyVrn (a0,a1) -> `TyVrn ((self#loc a0), (self#string a1))
      | `TyRec (a0,a1) -> `TyRec ((self#loc a0), (self#ctyp a1))
      | `TyCol (a0,a1,a2) ->
          `TyCol ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TySem (a0,a1,a2) ->
          `TySem ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyCom (a0,a1,a2) ->
          `TyCom ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TySum (a0,a1) -> `TySum ((self#loc a0), (self#ctyp a1))
      | `TyOf (a0,a1,a2) ->
          `TyOf ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyAnd (a0,a1,a2) ->
          `TyAnd ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyOr (a0,a1,a2) ->
          `TyOr ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyPrv (a0,a1) -> `TyPrv ((self#loc a0), (self#ctyp a1))
      | `TyMut (a0,a1) -> `TyMut ((self#loc a0), (self#ctyp a1))
      | `TyTup (a0,a1) -> `TyTup ((self#loc a0), (self#ctyp a1))
      | `TySta (a0,a1,a2) ->
          `TySta ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyVrnEq (a0,a1) -> `TyVrnEq ((self#loc a0), (self#ctyp a1))
      | `TyVrnSup (a0,a1) -> `TyVrnSup ((self#loc a0), (self#ctyp a1))
      | `TyVrnInf (a0,a1) -> `TyVrnInf ((self#loc a0), (self#ctyp a1))
      | `TyVrnInfSup (a0,a1,a2) ->
          `TyVrnInfSup ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyAmp (a0,a1,a2) ->
          `TyAmp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyOfAmp (a0,a1,a2) ->
          `TyOfAmp ((self#loc a0), (self#ctyp a1), (self#ctyp a2))
      | `TyPkg (a0,a1) -> `TyPkg ((self#loc a0), (self#module_type a1))
      | `TyAnt (a0,a1) -> `TyAnt ((self#loc a0), (self#string a1))
    method ident : ident -> ident=
      function
      | `IdAcc (a0,a1,a2) ->
          `IdAcc ((self#loc a0), (self#ident a1), (self#ident a2))
      | `IdApp (a0,a1,a2) ->
          `IdApp ((self#loc a0), (self#ident a1), (self#ident a2))
      | `IdLid (a0,a1) -> `IdLid ((self#loc a0), (self#string a1))
      | `IdUid (a0,a1) -> `IdUid ((self#loc a0), (self#string a1))
      | `IdAnt (a0,a1) -> `IdAnt ((self#loc a0), (self#string a1))
    method meta_list :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_list -> 'all_b0 meta_list=
      fun mf_a  ->
        function
        | `LNil -> `LNil
        | `LCons (a0,a1) -> `LCons ((mf_a self a0), (self#meta_list mf_a a1))
        | `LAnt a0 -> `LAnt (self#string a0)
    method meta_option :
      'all_a0 'all_b0 .
        ('self_type -> 'all_a0 -> 'all_b0) ->
          'all_a0 meta_option -> 'all_b0 meta_option=
      fun mf_a  ->
        function
        | `ONone -> `ONone
        | `OSome a0 -> `OSome (mf_a self a0)
        | `OAnt a0 -> `OAnt (self#string a0)
    method row_var_flag : row_var_flag -> row_var_flag=
      function
      | `RvRowVar -> `RvRowVar
      | `RvNil -> `RvNil
      | `RvAnt a0 -> `RvAnt (self#string a0)
    method override_flag : override_flag -> override_flag=
      function
      | `OvOverride -> `OvOverride
      | `OvNil -> `OvNil
      | `OvAnt a0 -> `OvAnt (self#string a0)
    method virtual_flag : virtual_flag -> virtual_flag=
      function
      | `ViVirtual -> `ViVirtual
      | `ViNil -> `ViNil
      | `ViAnt a0 -> `ViAnt (self#string a0)
    method private_flag : private_flag -> private_flag=
      function
      | `PrPrivate -> `PrPrivate
      | `PrNil -> `PrNil
      | `PrAnt a0 -> `PrAnt (self#string a0)
    method mutable_flag : mutable_flag -> mutable_flag=
      function
      | `MuMutable -> `MuMutable
      | `MuNil -> `MuNil
      | `MuAnt a0 -> `MuAnt (self#string a0)
    method direction_flag : direction_flag -> direction_flag=
      function
      | `DiTo -> `DiTo
      | `DiDownto -> `DiDownto
      | `DiAnt a0 -> `DiAnt (self#string a0)
    method rec_flag : rec_flag -> rec_flag=
      function
      | `ReRecursive -> `ReRecursive
      | `ReNil -> `ReNil
      | `ReAnt a0 -> `ReAnt (self#string a0)
    method meta_bool : meta_bool -> meta_bool=
      function
      | `BTrue -> `BTrue
      | `BFalse -> `BFalse
      | `BAnt a0 -> `BAnt (self#string a0)
    method loc : loc -> loc= fun a0  -> self#fanloc_t a0
    method fanloc_t : FanLoc.t -> FanLoc.t= self#unknown
  end
let rec pp_print_class_str_item: 'fmt -> class_str_item -> 'result =
  fun fmt  ->
    function
    | `CrNil a0 -> Format.fprintf fmt "@[<1>(CrNil@ %a)@]" pp_print_loc a0
    | `CrSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CrSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_str_item a1 pp_print_class_str_item a2
    | `CrCtr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CrCtr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `CrInh (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrInh@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_override_flag a1 pp_print_class_expr a2 pp_print_string a3
    | `CrIni (a0,a1) ->
        Format.fprintf fmt "@[<1>(CrIni@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `CrMth (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_string a1 pp_print_override_flag a2
          pp_print_private_flag a3 pp_print_expr a4 pp_print_ctyp a5
    | `CrVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 pp_print_override_flag a2
          pp_print_mutable_flag a3 pp_print_expr a4
    | `CrVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | `CrVvr (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrVvr@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_mutable_flag a2 pp_print_ctyp a3
    | `CrAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CrAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_expr: 'fmt -> class_expr -> 'result =
  fun fmt  ->
    function
    | `CeNil a0 -> Format.fprintf fmt "@[<1>(CeNil@ %a)@]" pp_print_loc a0
    | `CeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_expr a2
    | `CeCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CeCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | `CeFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_expr a2
    | `CeLet (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CeLet@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_class_expr a3
    | `CeStr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeStr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | `CeTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_type a2
    | `CeAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | `CeEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | `CeAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CeAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_sig_item: 'fmt -> class_sig_item -> 'result =
  fun fmt  ->
    function
    | `CgNil a0 -> Format.fprintf fmt "@[<1>(CgNil@ %a)@]" pp_print_loc a0
    | `CgCtr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CgCtr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `CgSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CgSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_sig_item a1 pp_print_class_sig_item a2
    | `CgInh (a0,a1) ->
        Format.fprintf fmt "@[<1>(CgInh@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `CgMth (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CgMth@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | `CgVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(CgVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 pp_print_mutable_flag a2
          pp_print_virtual_flag a3 pp_print_ctyp a4
    | `CgVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CgVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | `CgAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CgAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_type: 'fmt -> class_type -> 'result =
  fun fmt  ->
    function
    | `CtNil a0 -> Format.fprintf fmt "@[<1>(CtNil@ %a)@]" pp_print_loc a0
    | `CtCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CtCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | `CtFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_type a2
    | `CtSig (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtSig@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_sig_item a2
    | `CtAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | `CtCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | `CtEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | `CtAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CtAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_str_item: 'fmt -> str_item -> 'result =
  fun fmt  ->
    function
    | `StNil a0 -> Format.fprintf fmt "@[<1>(StNil@ %a)@]" pp_print_loc a0
    | `StCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(StCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1
    | `StClt (a0,a1) ->
        Format.fprintf fmt "@[<1>(StClt@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `StSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1 pp_print_str_item a2
    | `StDir (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StDir@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | `StExc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StExc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 (pp_print_meta_option pp_print_ident) a2
    | `StExp (a0,a1) ->
        Format.fprintf fmt "@[<1>(StExp@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `StExt (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(StExt@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | `StInc (a0,a1) ->
        Format.fprintf fmt "@[<1>(StInc@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | `StMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_expr a2
    | `StRecMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(StRecMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | `StMty (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StMty@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | `StOpn (a0,a1) ->
        Format.fprintf fmt "@[<1>(StOpn@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `StTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(StTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `StVal (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StVal@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2
    | `StAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(StAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_expr: 'fmt -> module_expr -> 'result =
  fun fmt  ->
    function
    | `MeNil a0 -> Format.fprintf fmt "@[<1>(MeNil@ %a)@]" pp_print_loc a0
    | `MeId (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `MeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1 pp_print_module_expr a2
    | `MeFun (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MeFun@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_expr a3
    | `MeStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1
    | `MeTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MeTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1 pp_print_module_type a2
    | `MePkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(MePkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `MeAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_match_case: 'fmt -> match_case -> 'result =
  fun fmt  ->
    function
    | `McNil a0 -> Format.fprintf fmt "@[<1>(McNil@ %a)@]" pp_print_loc a0
    | `McOr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(McOr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1 pp_print_match_case a2
    | `McArr (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(McArr@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2 pp_print_expr a3
    | `McAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(McAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_binding: 'fmt -> module_binding -> 'result =
  fun fmt  ->
    function
    | `MbNil a0 -> Format.fprintf fmt "@[<1>(MbNil@ %a)@]" pp_print_loc a0
    | `MbAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MbAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1 pp_print_module_binding a2
    | `MbColEq (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MbColEq@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_expr a3
    | `MbCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MbCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | `MbAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MbAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_rec_binding: 'fmt -> rec_binding -> 'result =
  fun fmt  ->
    function
    | `RbNil a0 -> Format.fprintf fmt "@[<1>(RbNil@ %a)@]" pp_print_loc a0
    | `RbSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(RbSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_rec_binding a2
    | `RbEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(RbEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | `RbAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(RbAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_binding: 'fmt -> binding -> 'result =
  fun fmt  ->
    function
    | `BiNil a0 -> Format.fprintf fmt "@[<1>(BiNil@ %a)@]" pp_print_loc a0
    | `BiAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(BiAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_binding a1 pp_print_binding a2
    | `BiEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(BiEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2
    | `BiAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(BiAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_with_constr: 'fmt -> with_constr -> 'result =
  fun fmt  ->
    function
    | `WcNil a0 -> Format.fprintf fmt "@[<1>(WcNil@ %a)@]" pp_print_loc a0
    | `WcTyp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcTyp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `WcMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `WcTyS (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcTyS@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `WcMoS (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcMoS@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `WcAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_with_constr a1 pp_print_with_constr a2
    | `WcAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(WcAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_sig_item: 'fmt -> sig_item -> 'result =
  fun fmt  ->
    function
    | `SgNil a0 -> Format.fprintf fmt "@[<1>(SgNil@ %a)@]" pp_print_loc a0
    | `SgCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `SgClt (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgClt@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | `SgSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1 pp_print_sig_item a2
    | `SgDir (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgDir@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | `SgExc (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgExc@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `SgExt (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(SgExt@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | `SgInc (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgInc@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | `SgMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | `SgRecMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgRecMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | `SgMty (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgMty@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | `SgOpn (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgOpn@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `SgTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `SgVal (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgVal@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | `SgAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_type: 'fmt -> module_type -> 'result =
  fun fmt  ->
    function
    | `MtNil a0 -> Format.fprintf fmt "@[<1>(MtNil@ %a)@]" pp_print_loc a0
    | `MtId (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `MtFun (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MtFun@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_type a3
    | `MtQuo (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtQuo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `MtSig (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtSig@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1
    | `MtWit (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MtWit@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1 pp_print_with_constr a2
    | `MtOf (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtOf@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | `MtAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_expr: 'fmt -> expr -> 'result =
  fun fmt  ->
    function
    | `ExNil a0 -> Format.fprintf fmt "@[<1>(ExNil@ %a)@]" pp_print_loc a0
    | `ExId (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `ExAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExAre (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAre@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExArr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExArr@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExAsf a0 -> Format.fprintf fmt "@[<1>(ExAsf@ %a)@]" pp_print_loc a0
    | `ExAsr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExAsr@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExAss (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAss@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExChr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExChr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExCoe (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExCoe@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_ctyp a2 pp_print_ctyp a3
    | `ExFlo (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExFlo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExFor (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(ExFor@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_string a1 pp_print_expr a2 pp_print_expr
          a3 pp_print_direction_flag a4 pp_print_expr a5
    | `ExFun (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExFun@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1
    | `ExIfe (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExIfe@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2 pp_print_expr a3
    | `ExInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExInt32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt32@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExInt64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt64@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExNativeInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExNativeInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | `ExLaz (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExLaz@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExLet (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExLet@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_expr a3
    | `ExLmd (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExLmd@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_expr a2 pp_print_expr a3
    | `ExMat (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExMat@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | `ExNew (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExNew@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `ExObj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExObj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | `ExOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | `ExOvr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExOvr@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1
    | `ExRec (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExRec@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_expr a2
    | `ExSeq (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExSeq@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExSnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_string a2
    | `ExSte (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSte@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExTry (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExTry@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | `ExTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | `ExCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_ctyp a2
    | `ExVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `ExWhi (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExWhi@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | `ExOpI (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExOpI@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | `ExFUN (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExFUN@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | `ExPkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExPkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
and pp_print_patt: 'fmt -> patt -> 'result =
  fun fmt  ->
    function
    | `PaNil a0 -> Format.fprintf fmt "@[<1>(PaNil@ %a)@]" pp_print_loc a0
    | `PaId (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `PaAli (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaAli@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaAny a0 -> Format.fprintf fmt "@[<1>(PaAny@ %a)@]" pp_print_loc a0
    | `PaApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaArr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaArr@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaChr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaChr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaInt32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt32@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaInt64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt64@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaNativeInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaNativeInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaFlo (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaFlo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2
    | `PaOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2
    | `PaOlbi (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(PaOlbi@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2 pp_print_expr a3
    | `PaOrp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaOrp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaRng (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaRng@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | `PaRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_patt a2
    | `PaStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_ctyp a2
    | `PaTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `PaVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `PaLaz (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaLaz@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | `PaMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_ctyp: 'fmt -> ctyp -> 'result =
  fun fmt  ->
    function
    | `TyNil a0 -> Format.fprintf fmt "@[<1>(TyNil@ %a)@]" pp_print_loc a0
    | `TyAli (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAli@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyAny a0 -> Format.fprintf fmt "@[<1>(TyAny@ %a)@]" pp_print_loc a0
    | `TyApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyArr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyArr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `TyLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | `TyId (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | `TyMan (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyMan@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyDcl (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 (pp_print_list pp_print_ctyp) a2
          pp_print_ctyp a3
          (pp_print_list
             (fun fmt  (a0,a1)  ->
                Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_ctyp a0
                  pp_print_ctyp a1)) a4
    | `TyObj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyObj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_row_var_flag a2
    | `TyOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | `TyPol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyPol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyTypePol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyTypePol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyQuo (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `TyQuP (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuP@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `TyQuM (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuM@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `TyAnP a0 -> Format.fprintf fmt "@[<1>(TyAnP@ %a)@]" pp_print_loc a0
    | `TyAnM a0 -> Format.fprintf fmt "@[<1>(TyAnM@ %a)@]" pp_print_loc a0
    | `TyVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `TyRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TySem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TySem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TySum (a0,a1) ->
        Format.fprintf fmt "@[<1>(TySum@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyOf (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOf@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyOr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyPrv (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyPrv@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyMut (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyMut@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TySta (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TySta@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyVrnEq (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnEq@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnSup (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnSup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnInf (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnInf@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | `TyVrnInfSup (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyVrnInfSup@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyOfAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOfAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | `TyPkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyPkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | `TyAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_ident: 'fmt -> ident -> 'result =
  fun fmt  ->
    function
    | `IdAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(IdAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `IdApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(IdApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | `IdLid (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdLid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `IdUid (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdUid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | `IdAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_meta_list :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_list -> 'result=
  fun mf_a  fmt  ->
    function
    | `LNil -> Format.fprintf fmt "LNil"
    | `LCons (a0,a1) ->
        Format.fprintf fmt "@[<1>(LCons@ %a@ %a)@]" mf_a a0
          (pp_print_meta_list mf_a) a1
    | `LAnt a0 -> Format.fprintf fmt "@[<1>(LAnt@ %a)@]" pp_print_string a0
and pp_print_meta_option :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_option -> 'result=
  fun mf_a  fmt  ->
    function
    | `ONone -> Format.fprintf fmt "ONone"
    | `OSome a0 -> Format.fprintf fmt "@[<1>(OSome@ %a)@]" mf_a a0
    | `OAnt a0 -> Format.fprintf fmt "@[<1>(OAnt@ %a)@]" pp_print_string a0
and pp_print_row_var_flag: 'fmt -> row_var_flag -> 'result =
  fun fmt  ->
    function
    | `RvRowVar -> Format.fprintf fmt "RvRowVar"
    | `RvNil -> Format.fprintf fmt "RvNil"
    | `RvAnt a0 -> Format.fprintf fmt "@[<1>(RvAnt@ %a)@]" pp_print_string a0
and pp_print_override_flag: 'fmt -> override_flag -> 'result =
  fun fmt  ->
    function
    | `OvOverride -> Format.fprintf fmt "OvOverride"
    | `OvNil -> Format.fprintf fmt "OvNil"
    | `OvAnt a0 -> Format.fprintf fmt "@[<1>(OvAnt@ %a)@]" pp_print_string a0
and pp_print_virtual_flag: 'fmt -> virtual_flag -> 'result =
  fun fmt  ->
    function
    | `ViVirtual -> Format.fprintf fmt "ViVirtual"
    | `ViNil -> Format.fprintf fmt "ViNil"
    | `ViAnt a0 -> Format.fprintf fmt "@[<1>(ViAnt@ %a)@]" pp_print_string a0
and pp_print_private_flag: 'fmt -> private_flag -> 'result =
  fun fmt  ->
    function
    | `PrPrivate -> Format.fprintf fmt "PrPrivate"
    | `PrNil -> Format.fprintf fmt "PrNil"
    | `PrAnt a0 -> Format.fprintf fmt "@[<1>(PrAnt@ %a)@]" pp_print_string a0
and pp_print_mutable_flag: 'fmt -> mutable_flag -> 'result =
  fun fmt  ->
    function
    | `MuMutable -> Format.fprintf fmt "MuMutable"
    | `MuNil -> Format.fprintf fmt "MuNil"
    | `MuAnt a0 -> Format.fprintf fmt "@[<1>(MuAnt@ %a)@]" pp_print_string a0
and pp_print_direction_flag: 'fmt -> direction_flag -> 'result =
  fun fmt  ->
    function
    | `DiTo -> Format.fprintf fmt "DiTo"
    | `DiDownto -> Format.fprintf fmt "DiDownto"
    | `DiAnt a0 -> Format.fprintf fmt "@[<1>(DiAnt@ %a)@]" pp_print_string a0
and pp_print_rec_flag: 'fmt -> rec_flag -> 'result =
  fun fmt  ->
    function
    | `ReRecursive -> Format.fprintf fmt "ReRecursive"
    | `ReNil -> Format.fprintf fmt "ReNil"
    | `ReAnt a0 -> Format.fprintf fmt "@[<1>(ReAnt@ %a)@]" pp_print_string a0
and pp_print_meta_bool: 'fmt -> meta_bool -> 'result =
  fun fmt  ->
    function
    | `BTrue -> Format.fprintf fmt "BTrue"
    | `BFalse -> Format.fprintf fmt "BFalse"
    | `BAnt a0 -> Format.fprintf fmt "@[<1>(BAnt@ %a)@]" pp_print_string a0
and pp_print_loc: 'fmt -> loc -> 'result =
  fun fmt  a0  -> FanLoc.pp_print_t fmt a0
module Make(MetaLoc:META_LOC) =
  struct
  module Expr = struct
    open StdMeta.PExpr let meta_loc = MetaLoc.meta_loc_expr
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | `CrNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CrNil")), (meta_loc _loc a0))
        | `CrSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CrSem")), (meta_loc _loc a0))),
                     (meta_class_str_item _loc a1))),
                (meta_class_str_item _loc a2))
        | `CrCtr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CrCtr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `CrInh (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "CrInh")),
                               (meta_loc _loc a0))),
                          (meta_override_flag _loc a1))),
                     (meta_class_expr _loc a2))), (meta_string _loc a3))
        | `CrIni (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "CrIni")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
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
                                    (meta_string _loc a1))),
                               (meta_override_flag _loc a2))),
                          (meta_private_flag _loc a3))), (meta_expr _loc a4))),
                (meta_ctyp _loc a5))
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
                               (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
        | `CrAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | `CeNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CeNil")), (meta_loc _loc a0))
        | `CeApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CeApp")), (meta_loc _loc a0))),
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
                        (_loc, (`ExVrn (_loc, "CeFun")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_expr _loc a2))
        | `CeLet (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "CeLet")),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_class_expr _loc a3))
        | `CeStr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CeStr")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_str_item _loc a2))
        | `CeTyc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CeTyc")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_type _loc a2))
        | `CeAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CeAnd")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_expr _loc a2))
        | `CeEq (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CeEq")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_expr _loc a2))
        | `CeAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | `CgNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CgNil")), (meta_loc _loc a0))
        | `CgCtr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CgCtr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `CgSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CgSem")), (meta_loc _loc a0))),
                     (meta_class_sig_item _loc a1))),
                (meta_class_sig_item _loc a2))
        | `CgInh (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "CgInh")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `CgMth (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "CgMth")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
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
                               (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | `CgAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | `CtNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "CtNil")), (meta_loc _loc a0))
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
                        (_loc, (`ExVrn (_loc, "CtFun")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_class_type _loc a2))
        | `CtSig (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CtSig")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_class_sig_item _loc a2))
        | `CtAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CtAnd")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtCol (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CtCol")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtEq (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "CtEq")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | `StNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "StNil")), (meta_loc _loc a0))
        | `StCls (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StCls")), (meta_loc _loc a0))),
                (meta_class_expr _loc a1))
        | `StClt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StClt")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `StSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StSem")), (meta_loc _loc a0))),
                     (meta_str_item _loc a1))), (meta_str_item _loc a2))
        | `StDir (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StDir")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `StExc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StExc")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))),
                (meta_meta_option meta_ident _loc a2))
        | `StExp (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StExp")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `StExt (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "StExt")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | `StInc (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StInc")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
        | `StMod (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StMod")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_expr _loc a2))
        | `StRecMod (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "StRecMod")), (meta_loc _loc a0))),
                (meta_module_binding _loc a1))
        | `StMty (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StMty")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `StOpn (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StOpn")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `StTyp (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "StTyp")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `StVal (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "StVal")), (meta_loc _loc a0))),
                     (meta_rec_flag _loc a1))), (meta_binding _loc a2))
        | `StAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | `MeNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MeNil")), (meta_loc _loc a0))
        | `MeId (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MeId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `MeApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "MeApp")), (meta_loc _loc a0))),
                     (meta_module_expr _loc a1))),
                (meta_module_expr _loc a2))
        | `MeFun (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "MeFun")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | `MeStr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MeStr")), (meta_loc _loc a0))),
                (meta_str_item _loc a1))
        | `MeTyc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "MeTyc")), (meta_loc _loc a0))),
                     (meta_module_expr _loc a1))),
                (meta_module_type _loc a2))
        | `MePkg (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MePkg")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `MeAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | `McNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "McNil")), (meta_loc _loc a0))
        | `McOr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "McOr")), (meta_loc _loc a0))),
                     (meta_match_case _loc a1))), (meta_match_case _loc a2))
        | `McArr (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "McArr")),
                               (meta_loc _loc a0))), (meta_patt _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | `McAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | `MbNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MbNil")), (meta_loc _loc a0))
        | `MbAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "MbAnd")), (meta_loc _loc a0))),
                     (meta_module_binding _loc a1))),
                (meta_module_binding _loc a2))
        | `MbColEq (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "MbColEq")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | `MbCol (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "MbCol")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `MbAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | `RbNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "RbNil")), (meta_loc _loc a0))
        | `RbSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "RbSem")), (meta_loc _loc a0))),
                     (meta_rec_binding _loc a1))),
                (meta_rec_binding _loc a2))
        | `RbEq (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "RbEq")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_expr _loc a2))
        | `RbAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | `BiNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "BiNil")), (meta_loc _loc a0))
        | `BiAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "BiAnd")), (meta_loc _loc a0))),
                     (meta_binding _loc a1))), (meta_binding _loc a2))
        | `BiEq (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "BiEq")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_expr _loc a2))
        | `BiAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | `WcNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "WcNil")), (meta_loc _loc a0))
        | `WcTyp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "WcTyp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `WcMod (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "WcMod")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `WcTyS (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "WcTyS")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `WcMoS (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "WcMoS")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `WcAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "WcAnd")), (meta_loc _loc a0))),
                     (meta_with_constr _loc a1))),
                (meta_with_constr _loc a2))
        | `WcAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | `SgNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "SgNil")), (meta_loc _loc a0))
        | `SgCls (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgCls")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `SgClt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgClt")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `SgSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "SgSem")), (meta_loc _loc a0))),
                     (meta_sig_item _loc a1))), (meta_sig_item _loc a2))
        | `SgDir (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "SgDir")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `SgExc (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgExc")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `SgExt (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "SgExt")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | `SgInc (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgInc")), (meta_loc _loc a0))),
                (meta_module_type _loc a1))
        | `SgMod (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "SgMod")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `SgRecMod (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "SgRecMod")), (meta_loc _loc a0))),
                (meta_module_binding _loc a1))
        | `SgMty (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "SgMty")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `SgOpn (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgOpn")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `SgTyp (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "SgTyp")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `SgVal (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "SgVal")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `SgAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | `MtNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "MtNil")), (meta_loc _loc a0))
        | `MtId (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MtId")), (meta_loc _loc a0))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_type _loc a3))
        | `MtQuo (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MtQuo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `MtSig (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MtSig")), (meta_loc _loc a0))),
                (meta_sig_item _loc a1))
        | `MtWit (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "MtWit")), (meta_loc _loc a0))),
                     (meta_module_type _loc a1))),
                (meta_with_constr _loc a2))
        | `MtOf (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "MtOf")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
        | `MtAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | `ExNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ExNil")), (meta_loc _loc a0))
        | `ExId (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `ExAcc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExAcc")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAnt (a0,a1) -> `ExAnt (a0, a1)
        | `ExApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExApp")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAre (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExAre")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExArr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExArr")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExSem")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAsf a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "ExAsf")), (meta_loc _loc a0))
        | `ExAsr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExAsr")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExAss (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExAss")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExChr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExChr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
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
        | `ExFlo (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExFlo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExFor (a0,a1,a2,a3,a4,a5) ->
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
                                       (_loc, (`ExVrn (_loc, "ExFor")),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_expr _loc a2))), (meta_expr _loc a3))),
                     (meta_direction_flag _loc a4))), (meta_expr _loc a5))
        | `ExFun (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExFun")), (meta_loc _loc a0))),
                (meta_match_case _loc a1))
        | `ExIfe (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "ExIfe")),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | `ExInt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExInt32 (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "ExInt32")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExInt64 (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "ExInt64")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExNativeInt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "ExNativeInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExLab (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExLaz (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExLaz")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExLet (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "ExLet")),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_expr _loc a3))
        | `ExLmd (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "ExLmd")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_expr _loc a2))), (meta_expr _loc a3))
        | `ExMat (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExMat")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_match_case _loc a2))
        | `ExNew (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExNew")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `ExObj (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExObj")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_str_item _loc a2))
        | `ExOlb (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExOvr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExOvr")), (meta_loc _loc a0))),
                (meta_rec_binding _loc a1))
        | `ExRec (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExRec")), (meta_loc _loc a0))),
                     (meta_rec_binding _loc a1))), (meta_expr _loc a2))
        | `ExSeq (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExSeq")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExSnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExSnd")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_string _loc a2))
        | `ExSte (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExSte")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExStr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExStr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExTry (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExTry")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_match_case _loc a2))
        | `ExTup (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExTup")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExCom (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExCom")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExTyc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExTyc")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_ctyp _loc a2))
        | `ExVrn (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExWhi (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExWhi")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExOpI (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExOpI")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_expr _loc a2))
        | `ExFUN (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "ExFUN")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExPkg (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "ExPkg")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | `PaNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "PaNil")), (meta_loc _loc a0))
        | `PaId (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `PaAli (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaAli")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaAnt (a0,a1) -> `ExAnt (a0, a1)
        | `PaAny a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "PaAny")), (meta_loc _loc a0))
        | `PaApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaApp")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaArr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaArr")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaCom (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaCom")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaSem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaSem")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaChr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaChr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt32 (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "PaInt32")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt64 (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "PaInt64")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaNativeInt (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "PaNativeInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaFlo (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaFlo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaLab (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_patt _loc a2))
        | `PaOlb (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_patt _loc a2))
        | `PaOlbi (a0,a1,a2,a3) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc,
                          (`ExApp
                             (_loc, (`ExVrn (_loc, "PaOlbi")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_patt _loc a2))), (meta_expr _loc a3))
        | `PaOrp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaOrp")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaRng (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaRng")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaRec (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaRec")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaEq (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaEq")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_patt _loc a2))
        | `PaStr (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaStr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaTup (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaTup")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaTyc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "PaTyc")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_ctyp _loc a2))
        | `PaTyp (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaTyp")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `PaVrn (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaLaz (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaLaz")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaMod (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "PaMod")), (meta_loc _loc a0))),
                (meta_string _loc a1))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | `TyNil a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyNil")), (meta_loc _loc a0))
        | `TyAli (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyAli")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyAny a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAny")), (meta_loc _loc a0))
        | `TyApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyApp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyArr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyArr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyCls (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyCls")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `TyLab (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `TyId (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `TyMan (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyMan")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
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
                               (meta_string _loc a1))),
                          (meta_list meta_ctyp _loc a2))),
                     (meta_ctyp _loc a3))),
                (meta_list
                   (fun _loc  (a0,a1)  ->
                      `ExTup
                        (_loc,
                          (`ExCom
                             (_loc, (meta_ctyp _loc a0), (meta_ctyp _loc a1)))))
                   _loc a4))
        | `TyObj (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyObj")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_row_var_flag _loc a2))
        | `TyOlb (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `TyPol (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyPol")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyTypePol (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyTypePol")),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | `TyQuo (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyQuo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyQuP (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyQuP")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyQuM (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyQuM")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyAnP a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAnP")), (meta_loc _loc a0))
        | `TyAnM a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "TyAnM")), (meta_loc _loc a0))
        | `TyVrn (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyRec (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyRec")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyCol (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyCol")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TySem (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TySem")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyCom (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyCom")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TySum (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TySum")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyOf (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyOf")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyAnd (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyAnd")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyOr (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyOr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyPrv (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyPrv")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyMut (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyMut")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyTup (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyTup")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TySta (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TySta")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
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
                   (_loc, (`ExVrn (_loc, "TyVrnSup")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyVrnInf (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc, (`ExVrn (_loc, "TyVrnInf")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
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
                        (_loc, (`ExVrn (_loc, "TyAmp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyOfAmp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "TyOfAmp")),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | `TyPkg (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "TyPkg")), (meta_loc _loc a0))),
                (meta_module_type _loc a1))
        | `TyAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | `IdAcc (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "IdAcc")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `IdApp (a0,a1,a2) ->
            `ExApp
              (_loc,
                (`ExApp
                   (_loc,
                     (`ExApp
                        (_loc, (`ExVrn (_loc, "IdApp")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `IdLid (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "IdLid")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `IdUid (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "IdUid")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `IdAnt (a0,a1) -> `ExAnt (a0, a1)
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | `LNil -> `ExVrn (_loc, "LNil")
        | `LCons (a0,a1) ->
            `ExApp
              (_loc,
                (`ExApp (_loc, (`ExVrn (_loc, "LCons")), (mf_a _loc a0))),
                (meta_meta_list mf_a _loc a1))
        | `LAnt a0 -> `ExAnt (_loc, a0)
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | `ONone -> `ExVrn (_loc, "ONone")
        | `OSome a0 ->
            `ExApp (_loc, (`ExVrn (_loc, "OSome")), (mf_a _loc a0))
        | `OAnt a0 -> `ExAnt (_loc, a0)
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | `RvRowVar -> `ExVrn (_loc, "RvRowVar")
        | `RvNil -> `ExVrn (_loc, "RvNil")
        | `RvAnt a0 -> `ExAnt (_loc, a0)
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | `OvOverride -> `ExVrn (_loc, "OvOverride")
        | `OvNil -> `ExVrn (_loc, "OvNil")
        | `OvAnt a0 -> `ExAnt (_loc, a0)
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | `ViVirtual -> `ExVrn (_loc, "ViVirtual")
        | `ViNil -> `ExVrn (_loc, "ViNil")
        | `ViAnt a0 -> `ExAnt (_loc, a0)
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | `PrPrivate -> `ExVrn (_loc, "PrPrivate")
        | `PrNil -> `ExVrn (_loc, "PrNil")
        | `PrAnt a0 -> `ExAnt (_loc, a0)
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | `MuMutable -> `ExVrn (_loc, "MuMutable")
        | `MuNil -> `ExVrn (_loc, "MuNil")
        | `MuAnt a0 -> `ExAnt (_loc, a0)
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | `DiTo -> `ExVrn (_loc, "DiTo")
        | `DiDownto -> `ExVrn (_loc, "DiDownto")
        | `DiAnt a0 -> `ExAnt (_loc, a0)
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | `ReRecursive -> `ExVrn (_loc, "ReRecursive")
        | `ReNil -> `ExVrn (_loc, "ReNil")
        | `ReAnt a0 -> `ExAnt (_loc, a0)
    and meta_meta_bool: 'loc -> meta_bool -> 'result =
      fun _loc  ->
        function
        | `BTrue -> `ExVrn (_loc, "BTrue")
        | `BFalse -> `ExVrn (_loc, "BFalse")
        | `BAnt a0 -> `ExAnt (_loc, a0)
    end
  module Patt = struct
    open StdMeta.PPatt let meta_loc = MetaLoc.meta_loc_patt
    let rec meta_class_str_item: 'loc -> class_str_item -> 'result =
      fun _loc  ->
        function
        | `CrNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CrNil")), (meta_loc _loc a0))
        | `CrSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CrSem")), (meta_loc _loc a0))),
                     (meta_class_str_item _loc a1))),
                (meta_class_str_item _loc a2))
        | `CrCtr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CrCtr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `CrInh (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "CrInh")),
                               (meta_loc _loc a0))),
                          (meta_override_flag _loc a1))),
                     (meta_class_expr _loc a2))), (meta_string _loc a3))
        | `CrIni (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "CrIni")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
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
                                    (meta_string _loc a1))),
                               (meta_override_flag _loc a2))),
                          (meta_private_flag _loc a3))), (meta_expr _loc a4))),
                (meta_ctyp _loc a5))
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
                               (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_mutable_flag _loc a2))), (meta_ctyp _loc a3))
        | `CrAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_class_expr: 'loc -> class_expr -> 'result =
      fun _loc  ->
        function
        | `CeNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CeNil")), (meta_loc _loc a0))
        | `CeApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CeApp")), (meta_loc _loc a0))),
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
                        (_loc, (`PaVrn (_loc, "CeFun")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_expr _loc a2))
        | `CeLet (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "CeLet")),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_class_expr _loc a3))
        | `CeStr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CeStr")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_str_item _loc a2))
        | `CeTyc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CeTyc")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_type _loc a2))
        | `CeAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CeAnd")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_expr _loc a2))
        | `CeEq (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CeEq")), (meta_loc _loc a0))),
                     (meta_class_expr _loc a1))), (meta_class_expr _loc a2))
        | `CeAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_class_sig_item: 'loc -> class_sig_item -> 'result =
      fun _loc  ->
        function
        | `CgNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CgNil")), (meta_loc _loc a0))
        | `CgCtr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CgCtr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `CgSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CgSem")), (meta_loc _loc a0))),
                     (meta_class_sig_item _loc a1))),
                (meta_class_sig_item _loc a2))
        | `CgInh (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "CgInh")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `CgMth (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "CgMth")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
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
                               (meta_string _loc a1))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_private_flag _loc a2))), (meta_ctyp _loc a3))
        | `CgAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_class_type: 'loc -> class_type -> 'result =
      fun _loc  ->
        function
        | `CtNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "CtNil")), (meta_loc _loc a0))
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
                        (_loc, (`PaVrn (_loc, "CtFun")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_class_type _loc a2))
        | `CtSig (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CtSig")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_class_sig_item _loc a2))
        | `CtAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CtAnd")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtCol (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CtCol")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtEq (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "CtEq")), (meta_loc _loc a0))),
                     (meta_class_type _loc a1))), (meta_class_type _loc a2))
        | `CtAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_str_item: 'loc -> str_item -> 'result =
      fun _loc  ->
        function
        | `StNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "StNil")), (meta_loc _loc a0))
        | `StCls (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StCls")), (meta_loc _loc a0))),
                (meta_class_expr _loc a1))
        | `StClt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StClt")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `StSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StSem")), (meta_loc _loc a0))),
                     (meta_str_item _loc a1))), (meta_str_item _loc a2))
        | `StDir (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StDir")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `StExc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StExc")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))),
                (meta_meta_option meta_ident _loc a2))
        | `StExp (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StExp")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `StExt (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "StExt")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | `StInc (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StInc")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
        | `StMod (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StMod")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_expr _loc a2))
        | `StRecMod (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "StRecMod")), (meta_loc _loc a0))),
                (meta_module_binding _loc a1))
        | `StMty (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StMty")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `StOpn (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StOpn")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `StTyp (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "StTyp")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `StVal (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "StVal")), (meta_loc _loc a0))),
                     (meta_rec_flag _loc a1))), (meta_binding _loc a2))
        | `StAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_module_expr: 'loc -> module_expr -> 'result =
      fun _loc  ->
        function
        | `MeNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MeNil")), (meta_loc _loc a0))
        | `MeId (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MeId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `MeApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "MeApp")), (meta_loc _loc a0))),
                     (meta_module_expr _loc a1))),
                (meta_module_expr _loc a2))
        | `MeFun (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "MeFun")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | `MeStr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MeStr")), (meta_loc _loc a0))),
                (meta_str_item _loc a1))
        | `MeTyc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "MeTyc")), (meta_loc _loc a0))),
                     (meta_module_expr _loc a1))),
                (meta_module_type _loc a2))
        | `MePkg (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MePkg")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `MeAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_match_case: 'loc -> match_case -> 'result =
      fun _loc  ->
        function
        | `McNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "McNil")), (meta_loc _loc a0))
        | `McOr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "McOr")), (meta_loc _loc a0))),
                     (meta_match_case _loc a1))), (meta_match_case _loc a2))
        | `McArr (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "McArr")),
                               (meta_loc _loc a0))), (meta_patt _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | `McAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_module_binding: 'loc -> module_binding -> 'result =
      fun _loc  ->
        function
        | `MbNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MbNil")), (meta_loc _loc a0))
        | `MbAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "MbAnd")), (meta_loc _loc a0))),
                     (meta_module_binding _loc a1))),
                (meta_module_binding _loc a2))
        | `MbColEq (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "MbColEq")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_expr _loc a3))
        | `MbCol (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "MbCol")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `MbAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_rec_binding: 'loc -> rec_binding -> 'result =
      fun _loc  ->
        function
        | `RbNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "RbNil")), (meta_loc _loc a0))
        | `RbSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "RbSem")), (meta_loc _loc a0))),
                     (meta_rec_binding _loc a1))),
                (meta_rec_binding _loc a2))
        | `RbEq (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "RbEq")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_expr _loc a2))
        | `RbAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_binding: 'loc -> binding -> 'result =
      fun _loc  ->
        function
        | `BiNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "BiNil")), (meta_loc _loc a0))
        | `BiAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "BiAnd")), (meta_loc _loc a0))),
                     (meta_binding _loc a1))), (meta_binding _loc a2))
        | `BiEq (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "BiEq")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_expr _loc a2))
        | `BiAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_with_constr: 'loc -> with_constr -> 'result =
      fun _loc  ->
        function
        | `WcNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "WcNil")), (meta_loc _loc a0))
        | `WcTyp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "WcTyp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `WcMod (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "WcMod")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `WcTyS (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "WcTyS")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `WcMoS (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "WcMoS")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `WcAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "WcAnd")), (meta_loc _loc a0))),
                     (meta_with_constr _loc a1))),
                (meta_with_constr _loc a2))
        | `WcAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_sig_item: 'loc -> sig_item -> 'result =
      fun _loc  ->
        function
        | `SgNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "SgNil")), (meta_loc _loc a0))
        | `SgCls (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgCls")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `SgClt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgClt")), (meta_loc _loc a0))),
                (meta_class_type _loc a1))
        | `SgSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "SgSem")), (meta_loc _loc a0))),
                     (meta_sig_item _loc a1))), (meta_sig_item _loc a2))
        | `SgDir (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "SgDir")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `SgExc (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgExc")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `SgExt (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "SgExt")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_ctyp _loc a2))),
                (meta_meta_list meta_string _loc a3))
        | `SgInc (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgInc")), (meta_loc _loc a0))),
                (meta_module_type _loc a1))
        | `SgMod (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "SgMod")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `SgRecMod (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "SgRecMod")), (meta_loc _loc a0))),
                (meta_module_binding _loc a1))
        | `SgMty (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "SgMty")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_module_type _loc a2))
        | `SgOpn (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgOpn")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `SgTyp (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "SgTyp")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `SgVal (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "SgVal")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `SgAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_module_type: 'loc -> module_type -> 'result =
      fun _loc  ->
        function
        | `MtNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "MtNil")), (meta_loc _loc a0))
        | `MtId (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MtId")), (meta_loc _loc a0))),
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
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_type _loc a2))),
                (meta_module_type _loc a3))
        | `MtQuo (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MtQuo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `MtSig (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MtSig")), (meta_loc _loc a0))),
                (meta_sig_item _loc a1))
        | `MtWit (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "MtWit")), (meta_loc _loc a0))),
                     (meta_module_type _loc a1))),
                (meta_with_constr _loc a2))
        | `MtOf (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "MtOf")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
        | `MtAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_expr: 'loc -> expr -> 'result =
      fun _loc  ->
        function
        | `ExNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ExNil")), (meta_loc _loc a0))
        | `ExId (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `ExAcc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExAcc")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAnt (a0,a1) -> `PaAnt (a0, a1)
        | `ExApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExApp")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAre (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExAre")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExArr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExArr")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExSem")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExAsf a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "ExAsf")), (meta_loc _loc a0))
        | `ExAsr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExAsr")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExAss (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExAss")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExChr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExChr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
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
        | `ExFlo (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExFlo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExFor (a0,a1,a2,a3,a4,a5) ->
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
                                       (_loc, (`PaVrn (_loc, "ExFor")),
                                         (meta_loc _loc a0))),
                                    (meta_string _loc a1))),
                               (meta_expr _loc a2))), (meta_expr _loc a3))),
                     (meta_direction_flag _loc a4))), (meta_expr _loc a5))
        | `ExFun (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExFun")), (meta_loc _loc a0))),
                (meta_match_case _loc a1))
        | `ExIfe (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "ExIfe")),
                               (meta_loc _loc a0))), (meta_expr _loc a1))),
                     (meta_expr _loc a2))), (meta_expr _loc a3))
        | `ExInt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExInt32 (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "ExInt32")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExInt64 (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "ExInt64")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExNativeInt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "ExNativeInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExLab (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExLaz (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExLaz")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExLet (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "ExLet")),
                               (meta_loc _loc a0))), (meta_rec_flag _loc a1))),
                     (meta_binding _loc a2))), (meta_expr _loc a3))
        | `ExLmd (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "ExLmd")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_module_expr _loc a2))), (meta_expr _loc a3))
        | `ExMat (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExMat")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_match_case _loc a2))
        | `ExNew (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExNew")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `ExObj (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExObj")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_class_str_item _loc a2))
        | `ExOlb (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExOvr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExOvr")), (meta_loc _loc a0))),
                (meta_rec_binding _loc a1))
        | `ExRec (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExRec")), (meta_loc _loc a0))),
                     (meta_rec_binding _loc a1))), (meta_expr _loc a2))
        | `ExSeq (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExSeq")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExSnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExSnd")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_string _loc a2))
        | `ExSte (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExSte")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExStr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExStr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExTry (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExTry")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_match_case _loc a2))
        | `ExTup (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExTup")), (meta_loc _loc a0))),
                (meta_expr _loc a1))
        | `ExCom (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExCom")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExTyc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExTyc")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_ctyp _loc a2))
        | `ExVrn (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `ExWhi (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExWhi")), (meta_loc _loc a0))),
                     (meta_expr _loc a1))), (meta_expr _loc a2))
        | `ExOpI (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExOpI")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_expr _loc a2))
        | `ExFUN (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "ExFUN")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_expr _loc a2))
        | `ExPkg (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "ExPkg")), (meta_loc _loc a0))),
                (meta_module_expr _loc a1))
    and meta_patt: 'loc -> patt -> 'result =
      fun _loc  ->
        function
        | `PaNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "PaNil")), (meta_loc _loc a0))
        | `PaId (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `PaAli (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaAli")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaAnt (a0,a1) -> `PaAnt (a0, a1)
        | `PaAny a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "PaAny")), (meta_loc _loc a0))
        | `PaApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaApp")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaArr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaArr")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaCom (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaCom")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaSem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaSem")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaChr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaChr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt32 (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "PaInt32")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaInt64 (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "PaInt64")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaNativeInt (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "PaNativeInt")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaFlo (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaFlo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaLab (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_patt _loc a2))
        | `PaOlb (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_patt _loc a2))
        | `PaOlbi (a0,a1,a2,a3) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc,
                          (`PaApp
                             (_loc, (`PaVrn (_loc, "PaOlbi")),
                               (meta_loc _loc a0))), (meta_string _loc a1))),
                     (meta_patt _loc a2))), (meta_expr _loc a3))
        | `PaOrp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaOrp")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaRng (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaRng")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_patt _loc a2))
        | `PaRec (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaRec")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaEq (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaEq")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_patt _loc a2))
        | `PaStr (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaStr")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaTup (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaTup")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaTyc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "PaTyc")), (meta_loc _loc a0))),
                     (meta_patt _loc a1))), (meta_ctyp _loc a2))
        | `PaTyp (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaTyp")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `PaVrn (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `PaLaz (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaLaz")), (meta_loc _loc a0))),
                (meta_patt _loc a1))
        | `PaMod (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "PaMod")), (meta_loc _loc a0))),
                (meta_string _loc a1))
    and meta_ctyp: 'loc -> ctyp -> 'result =
      fun _loc  ->
        function
        | `TyNil a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyNil")), (meta_loc _loc a0))
        | `TyAli (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyAli")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyAny a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAny")), (meta_loc _loc a0))
        | `TyApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyApp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyArr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyArr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyCls (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyCls")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `TyLab (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyLab")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `TyId (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyId")), (meta_loc _loc a0))),
                (meta_ident _loc a1))
        | `TyMan (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyMan")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
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
                               (meta_string _loc a1))),
                          (meta_list meta_ctyp _loc a2))),
                     (meta_ctyp _loc a3))),
                (meta_list
                   (fun _loc  (a0,a1)  ->
                      `PaTup
                        (_loc,
                          (`PaCom
                             (_loc, (meta_ctyp _loc a0), (meta_ctyp _loc a1)))))
                   _loc a4))
        | `TyObj (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyObj")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_row_var_flag _loc a2))
        | `TyOlb (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyOlb")), (meta_loc _loc a0))),
                     (meta_string _loc a1))), (meta_ctyp _loc a2))
        | `TyPol (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyPol")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyTypePol (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyTypePol")),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | `TyQuo (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyQuo")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyQuP (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyQuP")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyQuM (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyQuM")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyAnP a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAnP")), (meta_loc _loc a0))
        | `TyAnM a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "TyAnM")), (meta_loc _loc a0))
        | `TyVrn (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyVrn")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `TyRec (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyRec")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyCol (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyCol")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TySem (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TySem")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyCom (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyCom")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TySum (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TySum")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyOf (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyOf")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyAnd (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyAnd")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyOr (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyOr")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyPrv (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyPrv")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyMut (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyMut")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyTup (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyTup")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TySta (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TySta")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
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
                   (_loc, (`PaVrn (_loc, "TyVrnSup")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
        | `TyVrnInf (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc, (`PaVrn (_loc, "TyVrnInf")), (meta_loc _loc a0))),
                (meta_ctyp _loc a1))
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
                        (_loc, (`PaVrn (_loc, "TyAmp")), (meta_loc _loc a0))),
                     (meta_ctyp _loc a1))), (meta_ctyp _loc a2))
        | `TyOfAmp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "TyOfAmp")),
                          (meta_loc _loc a0))), (meta_ctyp _loc a1))),
                (meta_ctyp _loc a2))
        | `TyPkg (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "TyPkg")), (meta_loc _loc a0))),
                (meta_module_type _loc a1))
        | `TyAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_ident: 'loc -> ident -> 'result =
      fun _loc  ->
        function
        | `IdAcc (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "IdAcc")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `IdApp (a0,a1,a2) ->
            `PaApp
              (_loc,
                (`PaApp
                   (_loc,
                     (`PaApp
                        (_loc, (`PaVrn (_loc, "IdApp")), (meta_loc _loc a0))),
                     (meta_ident _loc a1))), (meta_ident _loc a2))
        | `IdLid (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "IdLid")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `IdUid (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "IdUid")), (meta_loc _loc a0))),
                (meta_string _loc a1))
        | `IdAnt (a0,a1) -> `PaAnt (a0, a1)
    and meta_meta_list :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) -> 'loc -> 'all_a0 meta_list -> 'result=
      fun mf_a  _loc  ->
        function
        | `LNil -> `PaVrn (_loc, "LNil")
        | `LCons (a0,a1) ->
            `PaApp
              (_loc,
                (`PaApp (_loc, (`PaVrn (_loc, "LCons")), (mf_a _loc a0))),
                (meta_meta_list mf_a _loc a1))
        | `LAnt a0 -> `PaAnt (_loc, a0)
    and meta_meta_option :
      'all_a0 .
        ('loc -> 'all_a0 -> 'result) ->
          'loc -> 'all_a0 meta_option -> 'result=
      fun mf_a  _loc  ->
        function
        | `ONone -> `PaVrn (_loc, "ONone")
        | `OSome a0 ->
            `PaApp (_loc, (`PaVrn (_loc, "OSome")), (mf_a _loc a0))
        | `OAnt a0 -> `PaAnt (_loc, a0)
    and meta_row_var_flag: 'loc -> row_var_flag -> 'result =
      fun _loc  ->
        function
        | `RvRowVar -> `PaVrn (_loc, "RvRowVar")
        | `RvNil -> `PaVrn (_loc, "RvNil")
        | `RvAnt a0 -> `PaAnt (_loc, a0)
    and meta_override_flag: 'loc -> override_flag -> 'result =
      fun _loc  ->
        function
        | `OvOverride -> `PaVrn (_loc, "OvOverride")
        | `OvNil -> `PaVrn (_loc, "OvNil")
        | `OvAnt a0 -> `PaAnt (_loc, a0)
    and meta_virtual_flag: 'loc -> virtual_flag -> 'result =
      fun _loc  ->
        function
        | `ViVirtual -> `PaVrn (_loc, "ViVirtual")
        | `ViNil -> `PaVrn (_loc, "ViNil")
        | `ViAnt a0 -> `PaAnt (_loc, a0)
    and meta_private_flag: 'loc -> private_flag -> 'result =
      fun _loc  ->
        function
        | `PrPrivate -> `PaVrn (_loc, "PrPrivate")
        | `PrNil -> `PaVrn (_loc, "PrNil")
        | `PrAnt a0 -> `PaAnt (_loc, a0)
    and meta_mutable_flag: 'loc -> mutable_flag -> 'result =
      fun _loc  ->
        function
        | `MuMutable -> `PaVrn (_loc, "MuMutable")
        | `MuNil -> `PaVrn (_loc, "MuNil")
        | `MuAnt a0 -> `PaAnt (_loc, a0)
    and meta_direction_flag: 'loc -> direction_flag -> 'result =
      fun _loc  ->
        function
        | `DiTo -> `PaVrn (_loc, "DiTo")
        | `DiDownto -> `PaVrn (_loc, "DiDownto")
        | `DiAnt a0 -> `PaAnt (_loc, a0)
    and meta_rec_flag: 'loc -> rec_flag -> 'result =
      fun _loc  ->
        function
        | `ReRecursive -> `PaVrn (_loc, "ReRecursive")
        | `ReNil -> `PaVrn (_loc, "ReNil")
        | `ReAnt a0 -> `PaAnt (_loc, a0)
    and meta_meta_bool: 'loc -> meta_bool -> 'result =
      fun _loc  ->
        function
        | `BTrue -> `PaVrn (_loc, "BTrue")
        | `BFalse -> `PaVrn (_loc, "BFalse")
        | `BAnt a0 -> `PaAnt (_loc, a0)
    end
  end