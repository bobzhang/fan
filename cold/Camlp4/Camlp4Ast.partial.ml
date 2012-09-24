type loc = FanLoc.t
and meta_bool = BTrue | BFalse | BAnt of string
and rec_flag = ReRecursive | ReNil | ReAnt of string
and direction_flag = DiTo | DiDownto | DiAnt of string
and mutable_flag = MuMutable | MuNil | MuAnt of string
and private_flag = PrPrivate | PrNil | PrAnt of string
and virtual_flag = ViVirtual | ViNil | ViAnt of string
and override_flag =
    OvOverride | OvNil | OvAnt of string
and row_var_flag =
    RvRowVar | RvNil | RvAnt of string
and 'a meta_option =
    ONone | OSome of 'a | OAnt of string
and 'a meta_list =
    LNil
  | LCons of 'a * 'a meta_list
  | LAnt of string
and ident =
    IdAcc of loc * ident * ident
  | IdApp of loc * ident * ident
  | IdLid of loc * string
  | IdUid of loc * string
  | IdAnt of loc * string
and ctyp =
    TyNil of loc
  | TyAli of loc * ctyp * ctyp
  | TyAny of loc
  | TyApp of loc * ctyp * ctyp
  | TyArr of loc * ctyp * ctyp
  | TyCls of loc * ident
  | TyLab of loc * string * ctyp
  | TyId of loc * ident
  | TyMan of loc * ctyp * ctyp
  | TyDcl of loc * string * ctyp list * ctyp *
     (ctyp * ctyp) list
  | TyObj of loc * ctyp * row_var_flag
  | TyOlb of loc * string * ctyp
  | TyPol of loc * ctyp * ctyp
  | TyTypePol of loc * ctyp * ctyp
  | TyQuo of loc * string
  | TyQuP of loc * string
  | TyQuM of loc * string
  | TyAnP of loc
  | TyAnM of loc
  | TyVrn of loc * string
  | TyRec of loc * ctyp
  | TyCol of loc * ctyp * ctyp
  | TySem of loc * ctyp * ctyp
  | TyCom of loc * ctyp * ctyp
  | TySum of loc * ctyp
  | TyOf of loc * ctyp * ctyp
  | TyAnd of loc * ctyp * ctyp
  | TyOr of loc * ctyp * ctyp
  | TyPrv of loc * ctyp
  | TyMut of loc * ctyp
  | TyTup of loc * ctyp
  | TySta of loc * ctyp * ctyp
  | TyVrnEq of loc * ctyp
  | TyVrnSup of loc * ctyp
  | TyVrnInf of loc * ctyp
  | TyVrnInfSup of loc * ctyp * ctyp
  | TyAmp of loc * ctyp * ctyp
  | TyOfAmp of loc * ctyp * ctyp
  | TyPkg of loc * module_type
  | TyAnt of loc * string
and patt =
    PaNil of loc
  | PaId of loc * ident
  | PaAli of loc * patt * patt
  | PaAnt of loc * string
  | PaAny of loc
  | PaApp of loc * patt * patt
  | PaArr of loc * patt
  | PaCom of loc * patt * patt
  | PaSem of loc * patt * patt
  | PaChr of loc * string
  | PaInt of loc * string
  | PaInt32 of loc * string
  | PaInt64 of loc * string
  | PaNativeInt of loc * string
  | PaFlo of loc * string
  | PaLab of loc * string * patt
  | PaOlb of loc * string * patt
  | PaOlbi of loc * string * patt * expr
  | PaOrp of loc * patt * patt
  | PaRng of loc * patt * patt
  | PaRec of loc * patt
  | PaEq of loc * ident * patt
  | PaStr of loc * string
  | PaTup of loc * patt
  | PaTyc of loc * patt * ctyp
  | PaTyp of loc * ident
  | PaVrn of loc * string
  | PaLaz of loc * patt
  | PaMod of loc * string
and expr =
    ExNil of loc
  | ExId of loc * ident
  | ExAcc of loc * expr * expr
  | ExAnt of loc * string
  | ExApp of loc * expr * expr
  | ExAre of loc * expr * expr
  | ExArr of loc * expr
  | ExSem of loc * expr * expr
  | ExAsf of loc
  | ExAsr of loc * expr
  | ExAss of loc * expr * expr
  | ExChr of loc * string
  | ExCoe of loc * expr * ctyp * ctyp
  | ExFlo of loc * string
  | ExFor of loc * string * expr * expr *
     direction_flag * expr
  | ExFun of loc * match_case
  | ExIfe of loc * expr * expr * expr
  | ExInt of loc * string
  | ExInt32 of loc * string
  | ExInt64 of loc * string
  | ExNativeInt of loc * string
  | ExLab of loc * string * expr
  | ExLaz of loc * expr
  | ExLet of loc * rec_flag * binding * expr
  | ExLmd of loc * string * module_expr * expr
  | ExMat of loc * expr * match_case
  | ExNew of loc * ident
  | ExObj of loc * patt * class_str_item
  | ExOlb of loc * string * expr
  | ExOvr of loc * rec_binding
  | ExRec of loc * rec_binding * expr
  | ExSeq of loc * expr
  | ExSnd of loc * expr * string
  | ExSte of loc * expr * expr
  | ExStr of loc * string
  | ExTry of loc * expr * match_case
  | ExTup of loc * expr
  | ExCom of loc * expr * expr
  | ExTyc of loc * expr * ctyp
  | ExVrn of loc * string
  | ExWhi of loc * expr * expr
  | ExOpI of loc * ident * expr
  | ExFUN of loc * string * expr
  | ExPkg of loc * module_expr
and module_type =
    MtNil of loc
  | MtId of loc * ident
  | MtFun of loc * string * module_type * module_type
  | MtQuo of loc * string
  | MtSig of loc * sig_item
  | MtWit of loc * module_type * with_constr
  | MtOf of loc * module_expr
  | MtAnt of loc * string
and sig_item =
    SgNil of loc
  | SgCls of loc * class_type
  | SgClt of loc * class_type
  | SgSem of loc * sig_item * sig_item
  | SgDir of loc * string * expr
  | SgExc of loc * ctyp
  | SgExt of loc * string * ctyp * string meta_list
  | SgInc of loc * module_type
  | SgMod of loc * string * module_type
  | SgRecMod of loc * module_binding
  | SgMty of loc * string * module_type
  | SgOpn of loc * ident
  | SgTyp of loc * ctyp
  | SgVal of loc * string * ctyp
  | SgAnt of loc * string
and with_constr =
    WcNil of loc
  | WcTyp of loc * ctyp * ctyp
  | WcMod of loc * ident * ident
  | WcTyS of loc * ctyp * ctyp
  | WcMoS of loc * ident * ident
  | WcAnd of loc * with_constr * with_constr
  | WcAnt of loc * string
and binding =
    BiNil of loc
  | BiAnd of loc * binding * binding
  | BiEq of loc * patt * expr
  | BiAnt of loc * string
and rec_binding =
    RbNil of loc
  | RbSem of loc * rec_binding * rec_binding
  | RbEq of loc * ident * expr
  | RbAnt of loc * string
and module_binding =
    MbNil of loc
  | MbAnd of loc * module_binding * module_binding
  | MbColEq of loc * string * module_type *
     module_expr
  | MbCol of loc * string * module_type
  | MbAnt of loc * string
and match_case =
    McNil of loc
  | McOr of loc * match_case * match_case
  | McArr of loc * patt * expr * expr
  | McAnt of loc * string
and module_expr =
    MeNil of loc
  | MeId of loc * ident
  | MeApp of loc * module_expr * module_expr
  | MeFun of loc * string * module_type * module_expr
  | MeStr of loc * str_item
  | MeTyc of loc * module_expr * module_type
  | MePkg of loc * expr
  | MeAnt of loc * string
and str_item =
    StNil of loc
  | StCls of loc * class_expr
  | StClt of loc * class_type
  | StSem of loc * str_item * str_item
  | StDir of loc * string * expr
  | StExc of loc * ctyp * ident meta_option
  | StExp of loc * expr
  | StExt of loc * string * ctyp * string meta_list
  | StInc of loc * module_expr
  | StMod of loc * string * module_expr
  | StRecMod of loc * module_binding
  | StMty of loc * string * module_type
  | StOpn of loc * ident
  | StTyp of loc * ctyp
  | StVal of loc * rec_flag * binding
  | StAnt of loc * string
and class_type =
    CtNil of loc
  | CtCon of loc * virtual_flag * ident * ctyp
  | CtFun of loc * ctyp * class_type
  | CtSig of loc * ctyp * class_sig_item
  | CtAnd of loc * class_type * class_type
  | CtCol of loc * class_type * class_type
  | CtEq of loc * class_type * class_type
  | CtAnt of loc * string
and class_sig_item =
    CgNil of loc
  | CgCtr of loc * ctyp * ctyp
  | CgSem of loc * class_sig_item * class_sig_item
  | CgInh of loc * class_type
  | CgMth of loc * string * private_flag * ctyp
  | CgVal of loc * string * mutable_flag *
     virtual_flag * ctyp
  | CgVir of loc * string * private_flag * ctyp
  | CgAnt of loc * string
and class_expr =
    CeNil of loc
  | CeApp of loc * class_expr * expr
  | CeCon of loc * virtual_flag * ident * ctyp
  | CeFun of loc * patt * class_expr
  | CeLet of loc * rec_flag * binding * class_expr
  | CeStr of loc * patt * class_str_item
  | CeTyc of loc * class_expr * class_type
  | CeAnd of loc * class_expr * class_expr
  | CeEq of loc * class_expr * class_expr
  | CeAnt of loc * string
and class_str_item =
    CrNil of loc
  | CrSem of loc * class_str_item * class_str_item
  | CrCtr of loc * ctyp * ctyp
  | CrInh of loc * override_flag * class_expr *
     string
  | CrIni of loc * expr
  | CrMth of loc * string * override_flag *
     private_flag * expr * ctyp
  | CrVal of loc * string * override_flag *
     mutable_flag * expr
  | CrVir of loc * string * private_flag * ctyp
  | CrVvr of loc * string * mutable_flag * ctyp
  | CrAnt of loc * string
