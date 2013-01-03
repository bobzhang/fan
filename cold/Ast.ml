let _ = ()
open StdLib
type loc = FanLoc.t 
and meta_bool =  
  | BTrue
  | BFalse
  | BAnt of string 
and rec_flag =  
  | ReRecursive
  | ReNil
  | ReAnt of string 
and direction_flag =  
  | DiTo
  | DiDownto
  | DiAnt of string 
and mutable_flag =  
  | MuMutable
  | MuNil
  | MuAnt of string 
and private_flag =  
  | PrPrivate
  | PrNil
  | PrAnt of string 
and virtual_flag =  
  | ViVirtual
  | ViNil
  | ViAnt of string 
and override_flag =  
  | OvOverride
  | OvNil
  | OvAnt of string 
and row_var_flag =  
  | RvRowVar
  | RvNil
  | RvAnt of string 
and 'a meta_option =  
  | ONone
  | OSome of 'a
  | OAnt of string 
and 'a meta_list =  
  | LNil
  | LCons of 'a* 'a meta_list
  | LAnt of string 
and ident =  
  | IdAcc of loc* ident* ident
  | IdApp of loc* ident* ident
  | IdLid of loc* string
  | IdUid of loc* string
  | IdAnt of loc* string 
and ctyp =  
  | TyNil of loc
  | TyAli of loc* ctyp* ctyp
  | TyAny of loc
  | TyApp of loc* ctyp* ctyp
  | TyArr of loc* ctyp* ctyp
  | TyCls of loc* ident
  | TyLab of loc* string* ctyp
  | TyId of loc* ident
  | TyMan of loc* ctyp* ctyp
  | TyDcl of loc* string* ctyp list* ctyp* (ctyp* ctyp) list
  | TyObj of loc* ctyp* row_var_flag
  | TyOlb of loc* string* ctyp
  | TyPol of loc* ctyp* ctyp
  | TyTypePol of loc* ctyp* ctyp
  | TyQuo of loc* string
  | TyQuP of loc* string
  | TyQuM of loc* string
  | TyAnP of loc
  | TyAnM of loc
  | TyVrn of loc* string
  | TyRec of loc* ctyp
  | TyCol of loc* ctyp* ctyp
  | TySem of loc* ctyp* ctyp
  | TyCom of loc* ctyp* ctyp
  | TySum of loc* ctyp
  | TyOf of loc* ctyp* ctyp
  | TyAnd of loc* ctyp* ctyp
  | TyOr of loc* ctyp* ctyp
  | TyPrv of loc* ctyp
  | TyMut of loc* ctyp
  | TyTup of loc* ctyp
  | TySta of loc* ctyp* ctyp
  | TyVrnEq of loc* ctyp
  | TyVrnSup of loc* ctyp
  | TyVrnInf of loc* ctyp
  | TyVrnInfSup of loc* ctyp* ctyp
  | TyAmp of loc* ctyp* ctyp
  | TyOfAmp of loc* ctyp* ctyp
  | TyPkg of loc* module_type
  | TyAnt of loc* string 
and patt =  
  | PaNil of loc
  | PaId of loc* ident
  | PaAli of loc* patt* patt
  | PaAnt of loc* string
  | PaAny of loc
  | PaApp of loc* patt* patt
  | PaArr of loc* patt
  | PaCom of loc* patt* patt
  | PaSem of loc* patt* patt
  | PaChr of loc* string
  | PaInt of loc* string
  | PaInt32 of loc* string
  | PaInt64 of loc* string
  | PaNativeInt of loc* string
  | PaFlo of loc* string
  | PaLab of loc* string* patt
  | PaOlb of loc* string* patt
  | PaOlbi of loc* string* patt* expr
  | PaOrp of loc* patt* patt
  | PaRng of loc* patt* patt
  | PaRec of loc* patt
  | PaEq of loc* ident* patt
  | PaStr of loc* string
  | PaTup of loc* patt
  | PaTyc of loc* patt* ctyp
  | PaTyp of loc* ident
  | PaVrn of loc* string
  | PaLaz of loc* patt
  | PaMod of loc* string 
and expr =  
  | ExNil of loc
  | ExId of loc* ident
  | ExAcc of loc* expr* expr
  | ExAnt of loc* string
  | ExApp of loc* expr* expr
  | ExAre of loc* expr* expr
  | ExArr of loc* expr
  | ExSem of loc* expr* expr
  | ExAsf of loc
  | ExAsr of loc* expr
  | ExAss of loc* expr* expr
  | ExChr of loc* string
  | ExCoe of loc* expr* ctyp* ctyp
  | ExFlo of loc* string
  | ExFor of loc* string* expr* expr* direction_flag* expr
  | ExFun of loc* match_case
  | ExIfe of loc* expr* expr* expr
  | ExInt of loc* string
  | ExInt32 of loc* string
  | ExInt64 of loc* string
  | ExNativeInt of loc* string
  | ExLab of loc* string* expr
  | ExLaz of loc* expr
  | ExLet of loc* rec_flag* binding* expr
  | ExLmd of loc* string* module_expr* expr
  | ExMat of loc* expr* match_case
  | ExNew of loc* ident
  | ExObj of loc* patt* class_str_item
  | ExOlb of loc* string* expr
  | ExOvr of loc* rec_binding
  | ExRec of loc* rec_binding* expr
  | ExSeq of loc* expr
  | ExSnd of loc* expr* string
  | ExSte of loc* expr* expr
  | ExStr of loc* string
  | ExTry of loc* expr* match_case
  | ExTup of loc* expr
  | ExCom of loc* expr* expr
  | ExTyc of loc* expr* ctyp
  | ExVrn of loc* string
  | ExWhi of loc* expr* expr
  | ExOpI of loc* ident* expr
  | ExFUN of loc* string* expr
  | ExPkg of loc* module_expr 
and module_type =  
  | MtNil of loc
  | MtId of loc* ident
  | MtFun of loc* string* module_type* module_type
  | MtQuo of loc* string
  | MtSig of loc* sig_item
  | MtWit of loc* module_type* with_constr
  | MtOf of loc* module_expr
  | MtAnt of loc* string 
and sig_item =  
  | SgNil of loc
  | SgCls of loc* class_type
  | SgClt of loc* class_type
  | SgSem of loc* sig_item* sig_item
  | SgDir of loc* string* expr
  | SgExc of loc* ctyp
  | SgExt of loc* string* ctyp* string meta_list
  | SgInc of loc* module_type
  | SgMod of loc* string* module_type
  | SgRecMod of loc* module_binding
  | SgMty of loc* string* module_type
  | SgOpn of loc* ident
  | SgTyp of loc* ctyp
  | SgVal of loc* string* ctyp
  | SgAnt of loc* string 
and with_constr =  
  | WcNil of loc
  | WcTyp of loc* ctyp* ctyp
  | WcMod of loc* ident* ident
  | WcTyS of loc* ctyp* ctyp
  | WcMoS of loc* ident* ident
  | WcAnd of loc* with_constr* with_constr
  | WcAnt of loc* string 
and binding =  
  | BiNil of loc
  | BiAnd of loc* binding* binding
  | BiEq of loc* patt* expr
  | BiAnt of loc* string 
and rec_binding =  
  | RbNil of loc
  | RbSem of loc* rec_binding* rec_binding
  | RbEq of loc* ident* expr
  | RbAnt of loc* string 
and module_binding =  
  | MbNil of loc
  | MbAnd of loc* module_binding* module_binding
  | MbColEq of loc* string* module_type* module_expr
  | MbCol of loc* string* module_type
  | MbAnt of loc* string 
and match_case =  
  | McNil of loc
  | McOr of loc* match_case* match_case
  | McArr of loc* patt* expr* expr
  | McAnt of loc* string 
and module_expr =  
  | MeNil of loc
  | MeId of loc* ident
  | MeApp of loc* module_expr* module_expr
  | MeFun of loc* string* module_type* module_expr
  | MeStr of loc* str_item
  | MeTyc of loc* module_expr* module_type
  | MePkg of loc* expr
  | MeAnt of loc* string 
and str_item =  
  | StNil of loc
  | StCls of loc* class_expr
  | StClt of loc* class_type
  | StSem of loc* str_item* str_item
  | StDir of loc* string* expr
  | StExc of loc* ctyp* ident meta_option
  | StExp of loc* expr
  | StExt of loc* string* ctyp* string meta_list
  | StInc of loc* module_expr
  | StMod of loc* string* module_expr
  | StRecMod of loc* module_binding
  | StMty of loc* string* module_type
  | StOpn of loc* ident
  | StTyp of loc* ctyp
  | StVal of loc* rec_flag* binding
  | StAnt of loc* string 
and class_type =  
  | CtNil of loc
  | CtCon of loc* virtual_flag* ident* ctyp
  | CtFun of loc* ctyp* class_type
  | CtSig of loc* ctyp* class_sig_item
  | CtAnd of loc* class_type* class_type
  | CtCol of loc* class_type* class_type
  | CtEq of loc* class_type* class_type
  | CtAnt of loc* string 
and class_sig_item =  
  | CgNil of loc
  | CgCtr of loc* ctyp* ctyp
  | CgSem of loc* class_sig_item* class_sig_item
  | CgInh of loc* class_type
  | CgMth of loc* string* private_flag* ctyp
  | CgVal of loc* string* mutable_flag* virtual_flag* ctyp
  | CgVir of loc* string* private_flag* ctyp
  | CgAnt of loc* string 
and class_expr =  
  | CeNil of loc
  | CeApp of loc* class_expr* expr
  | CeCon of loc* virtual_flag* ident* ctyp
  | CeFun of loc* patt* class_expr
  | CeLet of loc* rec_flag* binding* class_expr
  | CeStr of loc* patt* class_str_item
  | CeTyc of loc* class_expr* class_type
  | CeAnd of loc* class_expr* class_expr
  | CeEq of loc* class_expr* class_expr
  | CeAnt of loc* string 
and class_str_item =  
  | CrNil of loc
  | CrSem of loc* class_str_item* class_str_item
  | CrCtr of loc* ctyp* ctyp
  | CrInh of loc* override_flag* class_expr* string
  | CrIni of loc* expr
  | CrMth of loc* string* override_flag* private_flag* expr* ctyp
  | CrVal of loc* string* override_flag* mutable_flag* expr
  | CrVir of loc* string* private_flag* ctyp
  | CrVvr of loc* string* mutable_flag* ctyp
  | CrAnt of loc* string 
class print =
  object (self : 'self_type)
    inherit  printbase
    method class_str_item : 'fmt -> class_str_item -> 'result=
      fun fmt  ->
        function
        | CrNil a0 -> Format.fprintf fmt "@[<1>(CrNil@ %a)@]" self#loc a0
        | CrSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CrSem@ %a@ %a@ %a)@]" self#loc a0
              self#class_str_item a1 self#class_str_item a2
        | CrCtr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CrCtr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | CrInh (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrInh@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#override_flag a1 self#class_expr a2 self#string a3
        | CrIni (a0,a1) ->
            Format.fprintf fmt "@[<1>(CrIni@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | CrMth (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#string a1 self#override_flag a2
              self#private_flag a3 self#expr a4 self#ctyp a5
        | CrVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(CrVal@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 self#override_flag a2 self#mutable_flag a3
              self#expr a4
        | CrVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | CrVvr (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CrVvr@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#mutable_flag a2 self#ctyp a3
        | CrAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CrAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_expr : 'fmt -> class_expr -> 'result=
      fun fmt  ->
        function
        | CeNil a0 -> Format.fprintf fmt "@[<1>(CeNil@ %a)@]" self#loc a0
        | CeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeApp@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#expr a2
        | CeCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CeCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | CeFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeFun@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_expr a2
        | CeLet (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CeLet@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#class_expr a3
        | CeStr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeStr@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | CeTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeTyc@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_type a2
        | CeAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeAnd@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | CeEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CeEq@ %a@ %a@ %a)@]" self#loc a0
              self#class_expr a1 self#class_expr a2
        | CeAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CeAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_sig_item : 'fmt -> class_sig_item -> 'result=
      fun fmt  ->
        function
        | CgNil a0 -> Format.fprintf fmt "@[<1>(CgNil@ %a)@]" self#loc a0
        | CgCtr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CgCtr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | CgSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CgSem@ %a@ %a@ %a)@]" self#loc a0
              self#class_sig_item a1 self#class_sig_item a2
        | CgInh (a0,a1) ->
            Format.fprintf fmt "@[<1>(CgInh@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | CgMth (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CgMth@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | CgVal (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(CgVal@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 self#mutable_flag a2 self#virtual_flag a3
              self#ctyp a4
        | CgVir (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CgVir@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#private_flag a2 self#ctyp a3
        | CgAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CgAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method class_type : 'fmt -> class_type -> 'result=
      fun fmt  ->
        function
        | CtNil a0 -> Format.fprintf fmt "@[<1>(CtNil@ %a)@]" self#loc a0
        | CtCon (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(CtCon@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#virtual_flag a1 self#ident a2 self#ctyp a3
        | CtFun (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtFun@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_type a2
        | CtSig (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtSig@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#class_sig_item a2
        | CtAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtAnd@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | CtCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtCol@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | CtEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(CtEq@ %a@ %a@ %a)@]" self#loc a0
              self#class_type a1 self#class_type a2
        | CtAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(CtAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method str_item : 'fmt -> str_item -> 'result=
      fun fmt  ->
        function
        | StNil a0 -> Format.fprintf fmt "@[<1>(StNil@ %a)@]" self#loc a0
        | StCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(StCls@ %a@ %a)@]" self#loc a0
              self#class_expr a1
        | StClt (a0,a1) ->
            Format.fprintf fmt "@[<1>(StClt@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | StSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StSem@ %a@ %a@ %a)@]" self#loc a0
              self#str_item a1 self#str_item a2
        | StDir (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StDir@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | StExc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StExc@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 (self#meta_option (fun self  -> self#ident)) a2
        | StExp (a0,a1) ->
            Format.fprintf fmt "@[<1>(StExp@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | StExt (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(StExt@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | StInc (a0,a1) ->
            Format.fprintf fmt "@[<1>(StInc@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | StMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StMod@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_expr a2
        | StRecMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(StRecMod@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | StMty (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StMty@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | StOpn (a0,a1) ->
            Format.fprintf fmt "@[<1>(StOpn@ %a@ %a)@]" self#loc a0
              self#ident a1
        | StTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(StTyp@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | StVal (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(StVal@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2
        | StAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(StAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_expr : 'fmt -> module_expr -> 'result=
      fun fmt  ->
        function
        | MeNil a0 -> Format.fprintf fmt "@[<1>(MeNil@ %a)@]" self#loc a0
        | MeId (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | MeApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MeApp@ %a@ %a@ %a)@]" self#loc a0
              self#module_expr a1 self#module_expr a2
        | MeFun (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MeFun@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_expr a3
        | MeStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeStr@ %a@ %a)@]" self#loc a0
              self#str_item a1
        | MeTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MeTyc@ %a@ %a@ %a)@]" self#loc a0
              self#module_expr a1 self#module_type a2
        | MePkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(MePkg@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | MeAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MeAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method match_case : 'fmt -> match_case -> 'result=
      fun fmt  ->
        function
        | McNil a0 -> Format.fprintf fmt "@[<1>(McNil@ %a)@]" self#loc a0
        | McOr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(McOr@ %a@ %a@ %a)@]" self#loc a0
              self#match_case a1 self#match_case a2
        | McArr (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(McArr@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2 self#expr a3
        | McAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(McAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_binding : 'fmt -> module_binding -> 'result=
      fun fmt  ->
        function
        | MbNil a0 -> Format.fprintf fmt "@[<1>(MbNil@ %a)@]" self#loc a0
        | MbAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MbAnd@ %a@ %a@ %a)@]" self#loc a0
              self#module_binding a1 self#module_binding a2
        | MbColEq (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MbColEq@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_expr a3
        | MbCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MbCol@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | MbAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MbAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method rec_binding : 'fmt -> rec_binding -> 'result=
      fun fmt  ->
        function
        | RbNil a0 -> Format.fprintf fmt "@[<1>(RbNil@ %a)@]" self#loc a0
        | RbSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(RbSem@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#rec_binding a2
        | RbEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(RbEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | RbAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(RbAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method binding : 'fmt -> binding -> 'result=
      fun fmt  ->
        function
        | BiNil a0 -> Format.fprintf fmt "@[<1>(BiNil@ %a)@]" self#loc a0
        | BiAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(BiAnd@ %a@ %a@ %a)@]" self#loc a0
              self#binding a1 self#binding a2
        | BiEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(BiEq@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#expr a2
        | BiAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(BiAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method with_constr : 'fmt -> with_constr -> 'result=
      fun fmt  ->
        function
        | WcNil a0 -> Format.fprintf fmt "@[<1>(WcNil@ %a)@]" self#loc a0
        | WcTyp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcTyp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | WcMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcMod@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | WcTyS (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcTyS@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | WcMoS (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcMoS@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | WcAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(WcAnd@ %a@ %a@ %a)@]" self#loc a0
              self#with_constr a1 self#with_constr a2
        | WcAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(WcAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method sig_item : 'fmt -> sig_item -> 'result=
      fun fmt  ->
        function
        | SgNil a0 -> Format.fprintf fmt "@[<1>(SgNil@ %a)@]" self#loc a0
        | SgCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgCls@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | SgClt (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgClt@ %a@ %a)@]" self#loc a0
              self#class_type a1
        | SgSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgSem@ %a@ %a@ %a)@]" self#loc a0
              self#sig_item a1 self#sig_item a2
        | SgDir (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgDir@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | SgExc (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgExc@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | SgExt (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(SgExt@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
              (self#meta_list (fun self  -> self#string)) a3
        | SgInc (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgInc@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | SgMod (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgMod@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | SgRecMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgRecMod@ %a@ %a)@]" self#loc a0
              self#module_binding a1
        | SgMty (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgMty@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2
        | SgOpn (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgOpn@ %a@ %a)@]" self#loc a0
              self#ident a1
        | SgTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgTyp@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | SgVal (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(SgVal@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | SgAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(SgAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method module_type : 'fmt -> module_type -> 'result=
      fun fmt  ->
        function
        | MtNil a0 -> Format.fprintf fmt "@[<1>(MtNil@ %a)@]" self#loc a0
        | MtId (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | MtFun (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(MtFun@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_type a2 self#module_type a3
        | MtQuo (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtQuo@ %a@ %a)@]" self#loc a0
              self#string a1
        | MtSig (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtSig@ %a@ %a)@]" self#loc a0
              self#sig_item a1
        | MtWit (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(MtWit@ %a@ %a@ %a)@]" self#loc a0
              self#module_type a1 self#with_constr a2
        | MtOf (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtOf@ %a@ %a)@]" self#loc a0
              self#module_expr a1
        | MtAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(MtAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method expr : 'fmt -> expr -> 'result=
      fun fmt  ->
        function
        | ExNil a0 -> Format.fprintf fmt "@[<1>(ExNil@ %a)@]" self#loc a0
        | ExId (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | ExAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAcc@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExAnt@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExApp@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExAre (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAre@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExArr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExArr@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | ExSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSem@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExAsf a0 -> Format.fprintf fmt "@[<1>(ExAsf@ %a)@]" self#loc a0
        | ExAsr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExAsr@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | ExAss (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExAss@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExChr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExChr@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExCoe (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExCoe@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#ctyp a2 self#ctyp a3
        | ExFlo (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExFlo@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExFor (a0,a1,a2,a3,a4,a5) ->
            Format.fprintf fmt "@[<1>(ExFor@ %a@ %a@ %a@ %a@ %a@ %a)@]"
              self#loc a0 self#string a1 self#expr a2 self#expr a3
              self#direction_flag a4 self#expr a5
        | ExFun (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExFun@ %a@ %a)@]" self#loc a0
              self#match_case a1
        | ExIfe (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExIfe@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2 self#expr a3
        | ExInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExInt32 (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt32@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExInt64 (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExInt64@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExNativeInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExNativeInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | ExLaz (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExLaz@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | ExLet (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExLet@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#rec_flag a1 self#binding a2 self#expr a3
        | ExLmd (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(ExLmd@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#module_expr a2 self#expr a3
        | ExMat (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExMat@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | ExNew (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExNew@ %a@ %a)@]" self#loc a0
              self#ident a1
        | ExObj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExObj@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#class_str_item a2
        | ExOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | ExOvr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExOvr@ %a@ %a)@]" self#loc a0
              self#rec_binding a1
        | ExRec (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExRec@ %a@ %a@ %a)@]" self#loc a0
              self#rec_binding a1 self#expr a2
        | ExSeq (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExSeq@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | ExSnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSnd@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#string a2
        | ExSte (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExSte@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExStr@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExTry (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExTry@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#match_case a2
        | ExTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExTup@ %a@ %a)@]" self#loc a0 self#expr
              a1
        | ExCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExCom@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExTyc@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#ctyp a2
        | ExVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | ExWhi (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExWhi@ %a@ %a@ %a)@]" self#loc a0
              self#expr a1 self#expr a2
        | ExOpI (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExOpI@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#expr a2
        | ExFUN (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(ExFUN@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#expr a2
        | ExPkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(ExPkg@ %a@ %a)@]" self#loc a0
              self#module_expr a1
    method patt : 'fmt -> patt -> 'result=
      fun fmt  ->
        function
        | PaNil a0 -> Format.fprintf fmt "@[<1>(PaNil@ %a)@]" self#loc a0
        | PaId (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | PaAli (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaAli@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaAnt@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaAny a0 -> Format.fprintf fmt "@[<1>(PaAny@ %a)@]" self#loc a0
        | PaApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaApp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaArr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaArr@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | PaCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaCom@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaSem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaSem@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaChr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaChr@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaInt32 (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt32@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaInt64 (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaInt64@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaNativeInt (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaNativeInt@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaFlo (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaFlo@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2
        | PaOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2
        | PaOlbi (a0,a1,a2,a3) ->
            Format.fprintf fmt "@[<1>(PaOlbi@ %a@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#patt a2 self#expr a3
        | PaOrp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaOrp@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaRng (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaRng@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#patt a2
        | PaRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaRec@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | PaEq (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaEq@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#patt a2
        | PaStr (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaStr@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaTup@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | PaTyc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(PaTyc@ %a@ %a@ %a)@]" self#loc a0
              self#patt a1 self#ctyp a2
        | PaTyp (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaTyp@ %a@ %a)@]" self#loc a0
              self#ident a1
        | PaVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | PaLaz (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaLaz@ %a@ %a)@]" self#loc a0 self#patt
              a1
        | PaMod (a0,a1) ->
            Format.fprintf fmt "@[<1>(PaMod@ %a@ %a)@]" self#loc a0
              self#string a1
    method ctyp : 'fmt -> ctyp -> 'result=
      fun fmt  ->
        function
        | TyNil a0 -> Format.fprintf fmt "@[<1>(TyNil@ %a)@]" self#loc a0
        | TyAli (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAli@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyAny a0 -> Format.fprintf fmt "@[<1>(TyAny@ %a)@]" self#loc a0
        | TyApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyApp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyArr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyArr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyCls (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyCls@ %a@ %a)@]" self#loc a0
              self#ident a1
        | TyLab (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyLab@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | TyId (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyId@ %a@ %a)@]" self#loc a0 self#ident
              a1
        | TyMan (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyMan@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyDcl (a0,a1,a2,a3,a4) ->
            Format.fprintf fmt "@[<1>(TyDcl@ %a@ %a@ %a@ %a@ %a)@]" self#loc
              a0 self#string a1 (self#list (fun self  -> self#ctyp)) a2
              self#ctyp a3
              (self#list
                 (fun self  fmt  (a0,a1)  ->
                    Format.fprintf fmt "@[<1>(%a,@,%a)@]" self#ctyp a0
                      self#ctyp a1)) a4
        | TyObj (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyObj@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#row_var_flag a2
        | TyOlb (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOlb@ %a@ %a@ %a)@]" self#loc a0
              self#string a1 self#ctyp a2
        | TyPol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyPol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyTypePol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyTypePol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyQuo (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuo@ %a@ %a)@]" self#loc a0
              self#string a1
        | TyQuP (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuP@ %a@ %a)@]" self#loc a0
              self#string a1
        | TyQuM (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyQuM@ %a@ %a)@]" self#loc a0
              self#string a1
        | TyAnP a0 -> Format.fprintf fmt "@[<1>(TyAnP@ %a)@]" self#loc a0
        | TyAnM a0 -> Format.fprintf fmt "@[<1>(TyAnM@ %a)@]" self#loc a0
        | TyVrn (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrn@ %a@ %a)@]" self#loc a0
              self#string a1
        | TyRec (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyRec@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | TyCol (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyCol@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TySem (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TySem@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyCom (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyCom@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TySum (a0,a1) ->
            Format.fprintf fmt "@[<1>(TySum@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | TyOf (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOf@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyAnd (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAnd@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyOr (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOr@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyPrv (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyPrv@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | TyMut (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyMut@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | TyTup (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyTup@ %a@ %a)@]" self#loc a0 self#ctyp
              a1
        | TySta (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TySta@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyVrnEq (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnEq@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | TyVrnSup (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnSup@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | TyVrnInf (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyVrnInf@ %a@ %a)@]" self#loc a0
              self#ctyp a1
        | TyVrnInfSup (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyVrnInfSup@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyOfAmp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(TyOfAmp@ %a@ %a@ %a)@]" self#loc a0
              self#ctyp a1 self#ctyp a2
        | TyPkg (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyPkg@ %a@ %a)@]" self#loc a0
              self#module_type a1
        | TyAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(TyAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method ident : 'fmt -> ident -> 'result=
      fun fmt  ->
        function
        | IdAcc (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(IdAcc@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | IdApp (a0,a1,a2) ->
            Format.fprintf fmt "@[<1>(IdApp@ %a@ %a@ %a)@]" self#loc a0
              self#ident a1 self#ident a2
        | IdLid (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdLid@ %a@ %a)@]" self#loc a0
              self#string a1
        | IdUid (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdUid@ %a@ %a)@]" self#loc a0
              self#string a1
        | IdAnt (a0,a1) ->
            Format.fprintf fmt "@[<1>(IdAnt@ %a@ %a)@]" self#loc a0
              self#string a1
    method meta_list :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_list -> 'result=
      fun mf_a  fmt  ->
        function
        | LNil  -> Format.fprintf fmt "LNil"
        | LCons (a0,a1) ->
            Format.fprintf fmt "@[<1>(LCons@ %a@ %a)@]" (mf_a self) a0
              (self#meta_list mf_a) a1
        | LAnt a0 -> Format.fprintf fmt "@[<1>(LAnt@ %a)@]" self#string a0
    method meta_option :
      'all_a0 .
        ('self_type -> 'fmt -> 'all_a0 -> 'result) ->
          'fmt -> 'all_a0 meta_option -> 'result=
      fun mf_a  fmt  ->
        function
        | ONone  -> Format.fprintf fmt "ONone"
        | OSome a0 -> Format.fprintf fmt "@[<1>(OSome@ %a)@]" (mf_a self) a0
        | OAnt a0 -> Format.fprintf fmt "@[<1>(OAnt@ %a)@]" self#string a0
    method row_var_flag : 'fmt -> row_var_flag -> 'result=
      fun fmt  ->
        function
        | RvRowVar  -> Format.fprintf fmt "RvRowVar"
        | RvNil  -> Format.fprintf fmt "RvNil"
        | RvAnt a0 -> Format.fprintf fmt "@[<1>(RvAnt@ %a)@]" self#string a0
    method override_flag : 'fmt -> override_flag -> 'result=
      fun fmt  ->
        function
        | OvOverride  -> Format.fprintf fmt "OvOverride"
        | OvNil  -> Format.fprintf fmt "OvNil"
        | OvAnt a0 -> Format.fprintf fmt "@[<1>(OvAnt@ %a)@]" self#string a0
    method virtual_flag : 'fmt -> virtual_flag -> 'result=
      fun fmt  ->
        function
        | ViVirtual  -> Format.fprintf fmt "ViVirtual"
        | ViNil  -> Format.fprintf fmt "ViNil"
        | ViAnt a0 -> Format.fprintf fmt "@[<1>(ViAnt@ %a)@]" self#string a0
    method private_flag : 'fmt -> private_flag -> 'result=
      fun fmt  ->
        function
        | PrPrivate  -> Format.fprintf fmt "PrPrivate"
        | PrNil  -> Format.fprintf fmt "PrNil"
        | PrAnt a0 -> Format.fprintf fmt "@[<1>(PrAnt@ %a)@]" self#string a0
    method mutable_flag : 'fmt -> mutable_flag -> 'result=
      fun fmt  ->
        function
        | MuMutable  -> Format.fprintf fmt "MuMutable"
        | MuNil  -> Format.fprintf fmt "MuNil"
        | MuAnt a0 -> Format.fprintf fmt "@[<1>(MuAnt@ %a)@]" self#string a0
    method direction_flag : 'fmt -> direction_flag -> 'result=
      fun fmt  ->
        function
        | DiTo  -> Format.fprintf fmt "DiTo"
        | DiDownto  -> Format.fprintf fmt "DiDownto"
        | DiAnt a0 -> Format.fprintf fmt "@[<1>(DiAnt@ %a)@]" self#string a0
    method rec_flag : 'fmt -> rec_flag -> 'result=
      fun fmt  ->
        function
        | ReRecursive  -> Format.fprintf fmt "ReRecursive"
        | ReNil  -> Format.fprintf fmt "ReNil"
        | ReAnt a0 -> Format.fprintf fmt "@[<1>(ReAnt@ %a)@]" self#string a0
    method meta_bool : 'fmt -> meta_bool -> 'result=
      fun fmt  ->
        function
        | BTrue  -> Format.fprintf fmt "BTrue"
        | BFalse  -> Format.fprintf fmt "BFalse"
        | BAnt a0 -> Format.fprintf fmt "@[<1>(BAnt@ %a)@]" self#string a0
    method loc : 'fmt -> loc -> 'result= fun fmt  a0  -> self#fanloc_t fmt a0
    method fanloc_t : 'fmt -> FanLoc.t -> 'result= self#unknown
  end
let rec pp_print_class_str_item: 'fmt -> class_str_item -> 'result =
  fun fmt  ->
    function
    | CrNil a0 -> Format.fprintf fmt "@[<1>(CrNil@ %a)@]" pp_print_loc a0
    | CrSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CrSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_str_item a1 pp_print_class_str_item a2
    | CrCtr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CrCtr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | CrInh (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrInh@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_override_flag a1 pp_print_class_expr a2 pp_print_string a3
    | CrIni (a0,a1) ->
        Format.fprintf fmt "@[<1>(CrIni@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | CrMth (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(CrMth@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_string a1 pp_print_override_flag a2
          pp_print_private_flag a3 pp_print_expr a4 pp_print_ctyp a5
    | CrVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(CrVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 pp_print_override_flag a2
          pp_print_mutable_flag a3 pp_print_expr a4
    | CrVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | CrVvr (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CrVvr@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_mutable_flag a2 pp_print_ctyp a3
    | CrAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CrAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_expr: 'fmt -> class_expr -> 'result =
  fun fmt  ->
    function
    | CeNil a0 -> Format.fprintf fmt "@[<1>(CeNil@ %a)@]" pp_print_loc a0
    | CeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_expr a2
    | CeCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CeCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | CeFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_expr a2
    | CeLet (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CeLet@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_class_expr a3
    | CeStr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeStr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | CeTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_type a2
    | CeAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | CeEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CeEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1 pp_print_class_expr a2
    | CeAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CeAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_sig_item: 'fmt -> class_sig_item -> 'result =
  fun fmt  ->
    function
    | CgNil a0 -> Format.fprintf fmt "@[<1>(CgNil@ %a)@]" pp_print_loc a0
    | CgCtr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CgCtr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | CgSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CgSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_sig_item a1 pp_print_class_sig_item a2
    | CgInh (a0,a1) ->
        Format.fprintf fmt "@[<1>(CgInh@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | CgMth (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CgMth@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | CgVal (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(CgVal@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 pp_print_mutable_flag a2
          pp_print_virtual_flag a3 pp_print_ctyp a4
    | CgVir (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CgVir@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_private_flag a2 pp_print_ctyp a3
    | CgAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CgAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_class_type: 'fmt -> class_type -> 'result =
  fun fmt  ->
    function
    | CtNil a0 -> Format.fprintf fmt "@[<1>(CtNil@ %a)@]" pp_print_loc a0
    | CtCon (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(CtCon@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_virtual_flag a1 pp_print_ident a2 pp_print_ctyp a3
    | CtFun (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtFun@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_type a2
    | CtSig (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtSig@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_class_sig_item a2
    | CtAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | CtCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | CtEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(CtEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1 pp_print_class_type a2
    | CtAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(CtAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_str_item: 'fmt -> str_item -> 'result =
  fun fmt  ->
    function
    | StNil a0 -> Format.fprintf fmt "@[<1>(StNil@ %a)@]" pp_print_loc a0
    | StCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(StCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_expr a1
    | StClt (a0,a1) ->
        Format.fprintf fmt "@[<1>(StClt@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | StSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1 pp_print_str_item a2
    | StDir (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StDir@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | StExc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StExc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 (pp_print_meta_option pp_print_ident) a2
    | StExp (a0,a1) ->
        Format.fprintf fmt "@[<1>(StExp@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | StExt (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(StExt@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | StInc (a0,a1) ->
        Format.fprintf fmt "@[<1>(StInc@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | StMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_expr a2
    | StRecMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(StRecMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | StMty (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StMty@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | StOpn (a0,a1) ->
        Format.fprintf fmt "@[<1>(StOpn@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | StTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(StTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | StVal (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(StVal@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2
    | StAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(StAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_expr: 'fmt -> module_expr -> 'result =
  fun fmt  ->
    function
    | MeNil a0 -> Format.fprintf fmt "@[<1>(MeNil@ %a)@]" pp_print_loc a0
    | MeId (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | MeApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MeApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1 pp_print_module_expr a2
    | MeFun (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MeFun@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_expr a3
    | MeStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_str_item a1
    | MeTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MeTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1 pp_print_module_type a2
    | MePkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(MePkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | MeAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MeAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_match_case: 'fmt -> match_case -> 'result =
  fun fmt  ->
    function
    | McNil a0 -> Format.fprintf fmt "@[<1>(McNil@ %a)@]" pp_print_loc a0
    | McOr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(McOr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1 pp_print_match_case a2
    | McArr (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(McArr@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2 pp_print_expr a3
    | McAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(McAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_binding: 'fmt -> module_binding -> 'result =
  fun fmt  ->
    function
    | MbNil a0 -> Format.fprintf fmt "@[<1>(MbNil@ %a)@]" pp_print_loc a0
    | MbAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MbAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1 pp_print_module_binding a2
    | MbColEq (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MbColEq@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_expr a3
    | MbCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MbCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | MbAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MbAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_rec_binding: 'fmt -> rec_binding -> 'result =
  fun fmt  ->
    function
    | RbNil a0 -> Format.fprintf fmt "@[<1>(RbNil@ %a)@]" pp_print_loc a0
    | RbSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(RbSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_rec_binding a2
    | RbEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(RbEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | RbAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(RbAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_binding: 'fmt -> binding -> 'result =
  fun fmt  ->
    function
    | BiNil a0 -> Format.fprintf fmt "@[<1>(BiNil@ %a)@]" pp_print_loc a0
    | BiAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(BiAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_binding a1 pp_print_binding a2
    | BiEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(BiEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_expr a2
    | BiAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(BiAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_with_constr: 'fmt -> with_constr -> 'result =
  fun fmt  ->
    function
    | WcNil a0 -> Format.fprintf fmt "@[<1>(WcNil@ %a)@]" pp_print_loc a0
    | WcTyp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcTyp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | WcMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | WcTyS (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcTyS@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | WcMoS (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcMoS@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | WcAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(WcAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_with_constr a1 pp_print_with_constr a2
    | WcAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(WcAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_sig_item: 'fmt -> sig_item -> 'result =
  fun fmt  ->
    function
    | SgNil a0 -> Format.fprintf fmt "@[<1>(SgNil@ %a)@]" pp_print_loc a0
    | SgCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | SgClt (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgClt@ %a@ %a)@]" pp_print_loc a0
          pp_print_class_type a1
    | SgSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1 pp_print_sig_item a2
    | SgDir (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgDir@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | SgExc (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgExc@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | SgExt (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(SgExt@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
          (pp_print_meta_list pp_print_string) a3
    | SgInc (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgInc@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | SgMod (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgMod@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | SgRecMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgRecMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_binding a1
    | SgMty (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgMty@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2
    | SgOpn (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgOpn@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | SgTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | SgVal (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(SgVal@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | SgAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(SgAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_module_type: 'fmt -> module_type -> 'result =
  fun fmt  ->
    function
    | MtNil a0 -> Format.fprintf fmt "@[<1>(MtNil@ %a)@]" pp_print_loc a0
    | MtId (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | MtFun (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(MtFun@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_type a2 pp_print_module_type a3
    | MtQuo (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtQuo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | MtSig (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtSig@ %a@ %a)@]" pp_print_loc a0
          pp_print_sig_item a1
    | MtWit (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(MtWit@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1 pp_print_with_constr a2
    | MtOf (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtOf@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
    | MtAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(MtAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_expr: 'fmt -> expr -> 'result =
  fun fmt  ->
    function
    | ExNil a0 -> Format.fprintf fmt "@[<1>(ExNil@ %a)@]" pp_print_loc a0
    | ExId (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | ExAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExAre (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAre@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExArr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExArr@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | ExSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExAsf a0 -> Format.fprintf fmt "@[<1>(ExAsf@ %a)@]" pp_print_loc a0
    | ExAsr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExAsr@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | ExAss (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExAss@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExChr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExChr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExCoe (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExCoe@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_ctyp a2 pp_print_ctyp a3
    | ExFlo (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExFlo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExFor (a0,a1,a2,a3,a4,a5) ->
        Format.fprintf fmt "@[<1>(ExFor@ %a@ %a@ %a@ %a@ %a@ %a)@]"
          pp_print_loc a0 pp_print_string a1 pp_print_expr a2 pp_print_expr
          a3 pp_print_direction_flag a4 pp_print_expr a5
    | ExFun (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExFun@ %a@ %a)@]" pp_print_loc a0
          pp_print_match_case a1
    | ExIfe (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExIfe@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2 pp_print_expr a3
    | ExInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExInt32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt32@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExInt64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExInt64@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExNativeInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExNativeInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | ExLaz (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExLaz@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | ExLet (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExLet@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_flag a1 pp_print_binding a2 pp_print_expr a3
    | ExLmd (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(ExLmd@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_module_expr a2 pp_print_expr a3
    | ExMat (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExMat@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | ExNew (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExNew@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | ExObj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExObj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_class_str_item a2
    | ExOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | ExOvr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExOvr@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1
    | ExRec (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExRec@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_rec_binding a1 pp_print_expr a2
    | ExSeq (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExSeq@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | ExSnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_string a2
    | ExSte (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExSte@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExTry (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExTry@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_match_case a2
    | ExTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1
    | ExCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_ctyp a2
    | ExVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | ExWhi (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExWhi@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_expr a1 pp_print_expr a2
    | ExOpI (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExOpI@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_expr a2
    | ExFUN (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(ExFUN@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_expr a2
    | ExPkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(ExPkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_expr a1
and pp_print_patt: 'fmt -> patt -> 'result =
  fun fmt  ->
    function
    | PaNil a0 -> Format.fprintf fmt "@[<1>(PaNil@ %a)@]" pp_print_loc a0
    | PaId (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | PaAli (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaAli@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaAny a0 -> Format.fprintf fmt "@[<1>(PaAny@ %a)@]" pp_print_loc a0
    | PaApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaArr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaArr@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | PaCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaSem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaSem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaChr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaChr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaInt32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt32@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaInt64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaInt64@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaNativeInt (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaNativeInt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaFlo (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaFlo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2
    | PaOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2
    | PaOlbi (a0,a1,a2,a3) ->
        Format.fprintf fmt "@[<1>(PaOlbi@ %a@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_patt a2 pp_print_expr a3
    | PaOrp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaOrp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaRng (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaRng@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_patt a2
    | PaRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | PaEq (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaEq@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_patt a2
    | PaStr (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaStr@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | PaTyc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(PaTyc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1 pp_print_ctyp a2
    | PaTyp (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaTyp@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | PaVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | PaLaz (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaLaz@ %a@ %a)@]" pp_print_loc a0
          pp_print_patt a1
    | PaMod (a0,a1) ->
        Format.fprintf fmt "@[<1>(PaMod@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_ctyp: 'fmt -> ctyp -> 'result =
  fun fmt  ->
    function
    | TyNil a0 -> Format.fprintf fmt "@[<1>(TyNil@ %a)@]" pp_print_loc a0
    | TyAli (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAli@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyAny a0 -> Format.fprintf fmt "@[<1>(TyAny@ %a)@]" pp_print_loc a0
    | TyApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyArr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyArr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyCls (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyCls@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | TyLab (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyLab@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | TyId (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyId@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1
    | TyMan (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyMan@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyDcl (a0,a1,a2,a3,a4) ->
        Format.fprintf fmt "@[<1>(TyDcl@ %a@ %a@ %a@ %a@ %a)@]" pp_print_loc
          a0 pp_print_string a1 (pp_print_list pp_print_ctyp) a2
          pp_print_ctyp a3
          (pp_print_list
             (fun fmt  (a0,a1)  ->
                Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_ctyp a0
                  pp_print_ctyp a1)) a4
    | TyObj (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyObj@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_row_var_flag a2
    | TyOlb (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOlb@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1 pp_print_ctyp a2
    | TyPol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyPol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyTypePol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyTypePol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyQuo (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuo@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | TyQuP (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuP@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | TyQuM (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyQuM@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | TyAnP a0 -> Format.fprintf fmt "@[<1>(TyAnP@ %a)@]" pp_print_loc a0
    | TyAnM a0 -> Format.fprintf fmt "@[<1>(TyAnM@ %a)@]" pp_print_loc a0
    | TyVrn (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrn@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | TyRec (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyRec@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyCol (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyCol@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TySem (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TySem@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyCom (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyCom@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TySum (a0,a1) ->
        Format.fprintf fmt "@[<1>(TySum@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyOf (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOf@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyAnd (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAnd@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyOr (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOr@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyPrv (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyPrv@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyMut (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyMut@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyTup (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyTup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TySta (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TySta@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyVrnEq (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnEq@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyVrnSup (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnSup@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyVrnInf (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyVrnInf@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1
    | TyVrnInfSup (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyVrnInfSup@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyOfAmp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(TyOfAmp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ctyp a1 pp_print_ctyp a2
    | TyPkg (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyPkg@ %a@ %a)@]" pp_print_loc a0
          pp_print_module_type a1
    | TyAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(TyAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_ident: 'fmt -> ident -> 'result =
  fun fmt  ->
    function
    | IdAcc (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(IdAcc@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | IdApp (a0,a1,a2) ->
        Format.fprintf fmt "@[<1>(IdApp@ %a@ %a@ %a)@]" pp_print_loc a0
          pp_print_ident a1 pp_print_ident a2
    | IdLid (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdLid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | IdUid (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdUid@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
    | IdAnt (a0,a1) ->
        Format.fprintf fmt "@[<1>(IdAnt@ %a@ %a)@]" pp_print_loc a0
          pp_print_string a1
and pp_print_meta_list :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_list -> 'result=
  fun mf_a  fmt  ->
    function
    | LNil  -> Format.fprintf fmt "LNil"
    | LCons (a0,a1) ->
        Format.fprintf fmt "@[<1>(LCons@ %a@ %a)@]" mf_a a0
          (pp_print_meta_list mf_a) a1
    | LAnt a0 -> Format.fprintf fmt "@[<1>(LAnt@ %a)@]" pp_print_string a0
and pp_print_meta_option :
  'all_a0 .
    ('fmt -> 'all_a0 -> 'result) -> 'fmt -> 'all_a0 meta_option -> 'result=
  fun mf_a  fmt  ->
    function
    | ONone  -> Format.fprintf fmt "ONone"
    | OSome a0 -> Format.fprintf fmt "@[<1>(OSome@ %a)@]" mf_a a0
    | OAnt a0 -> Format.fprintf fmt "@[<1>(OAnt@ %a)@]" pp_print_string a0
and pp_print_row_var_flag: 'fmt -> row_var_flag -> 'result =
  fun fmt  ->
    function
    | RvRowVar  -> Format.fprintf fmt "RvRowVar"
    | RvNil  -> Format.fprintf fmt "RvNil"
    | RvAnt a0 -> Format.fprintf fmt "@[<1>(RvAnt@ %a)@]" pp_print_string a0
and pp_print_override_flag: 'fmt -> override_flag -> 'result =
  fun fmt  ->
    function
    | OvOverride  -> Format.fprintf fmt "OvOverride"
    | OvNil  -> Format.fprintf fmt "OvNil"
    | OvAnt a0 -> Format.fprintf fmt "@[<1>(OvAnt@ %a)@]" pp_print_string a0
and pp_print_virtual_flag: 'fmt -> virtual_flag -> 'result =
  fun fmt  ->
    function
    | ViVirtual  -> Format.fprintf fmt "ViVirtual"
    | ViNil  -> Format.fprintf fmt "ViNil"
    | ViAnt a0 -> Format.fprintf fmt "@[<1>(ViAnt@ %a)@]" pp_print_string a0
and pp_print_private_flag: 'fmt -> private_flag -> 'result =
  fun fmt  ->
    function
    | PrPrivate  -> Format.fprintf fmt "PrPrivate"
    | PrNil  -> Format.fprintf fmt "PrNil"
    | PrAnt a0 -> Format.fprintf fmt "@[<1>(PrAnt@ %a)@]" pp_print_string a0
and pp_print_mutable_flag: 'fmt -> mutable_flag -> 'result =
  fun fmt  ->
    function
    | MuMutable  -> Format.fprintf fmt "MuMutable"
    | MuNil  -> Format.fprintf fmt "MuNil"
    | MuAnt a0 -> Format.fprintf fmt "@[<1>(MuAnt@ %a)@]" pp_print_string a0
and pp_print_direction_flag: 'fmt -> direction_flag -> 'result =
  fun fmt  ->
    function
    | DiTo  -> Format.fprintf fmt "DiTo"
    | DiDownto  -> Format.fprintf fmt "DiDownto"
    | DiAnt a0 -> Format.fprintf fmt "@[<1>(DiAnt@ %a)@]" pp_print_string a0
and pp_print_rec_flag: 'fmt -> rec_flag -> 'result =
  fun fmt  ->
    function
    | ReRecursive  -> Format.fprintf fmt "ReRecursive"
    | ReNil  -> Format.fprintf fmt "ReNil"
    | ReAnt a0 -> Format.fprintf fmt "@[<1>(ReAnt@ %a)@]" pp_print_string a0
and pp_print_meta_bool: 'fmt -> meta_bool -> 'result =
  fun fmt  ->
    function
    | BTrue  -> Format.fprintf fmt "BTrue"
    | BFalse  -> Format.fprintf fmt "BFalse"
    | BAnt a0 -> Format.fprintf fmt "@[<1>(BAnt@ %a)@]" pp_print_string a0
and pp_print_loc: 'fmt -> loc -> 'result =
  fun fmt  a0  -> FanLoc.pp_print_t fmt a0
let safe_string_escaped s =
  if ((String.length s) > 2) && (((s.[0]) = '\\') && ((s.[1]) = '$'))
  then s
  else String.escaped s
external loc_of_ctyp : ctyp -> FanLoc.t = "%field0"
external loc_of_patt : patt -> FanLoc.t = "%field0"
external loc_of_expr : expr -> FanLoc.t = "%field0"
external loc_of_module_type : module_type -> FanLoc.t = "%field0"
external loc_of_module_expr : module_expr -> FanLoc.t = "%field0"
external loc_of_sig_item : sig_item -> FanLoc.t = "%field0"
external loc_of_str_item : str_item -> FanLoc.t = "%field0"
external loc_of_class_type : class_type -> FanLoc.t = "%field0"
external loc_of_class_sig_item : class_sig_item -> FanLoc.t = "%field0"
external loc_of_class_expr : class_expr -> FanLoc.t = "%field0"
external loc_of_class_str_item : class_str_item -> FanLoc.t = "%field0"
external loc_of_with_constr : with_constr -> FanLoc.t = "%field0"
external loc_of_binding : binding -> FanLoc.t = "%field0"
external loc_of_rec_binding : rec_binding -> FanLoc.t = "%field0"
external loc_of_module_binding : module_binding -> FanLoc.t = "%field0"
external loc_of_match_case : match_case -> FanLoc.t = "%field0"
external loc_of_ident : ident -> FanLoc.t = "%field0"