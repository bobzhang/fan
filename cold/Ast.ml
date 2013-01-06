type loc = FanLoc.t 
and rec_flag = [ `Recursive of loc | `ReNil of loc | `Ant of (loc* string)] 
and direction_flag = [ `To of loc | `Downto of loc | `Ant of (loc* string)] 
and mutable_flag = [ `Mutable of loc | `MuNil of loc | `Ant of (loc* string)] 
and private_flag = [ `Private of loc | `PrNil of loc | `Ant of (loc* string)] 
and virtual_flag = [ `Virtual of loc | `ViNil of loc | `Ant of (loc* string)] 
and override_flag =
  [ `Override of loc | `OvNil of loc | `Ant of (loc* string)] 
and row_var_flag = [ `RowVar of loc | `RvNil of loc | `Ant of (loc* string)] 
and 'a meta_option = [ `None of loc | `Some of 'a | `Ant of (loc* string)] 
and 'a meta_list =
  [ `LNil of loc | `LCons of ('a* 'a meta_list) | `Ant of (loc* string)] 
and ident =
  [ `IdAcc of (loc* ident* ident) | `IdApp of (loc* ident* ident)
  | `Lid of (loc* string) | `Uid of (loc* string) | `Ant of (loc* string)] 
and ctyp =
  [ `TyNil of loc | `TyAli of (loc* ctyp* ctyp) | `TyAny of loc
  | `TyApp of (loc* ctyp* ctyp) | `TyArr of (loc* ctyp* ctyp)
  | `TyCls of (loc* ident) | `TyLab of (loc* string* ctyp)
  | `TyId of (loc* ident) | `TyMan of (loc* ctyp* ctyp)
  | `TyDcl of (loc* string* ctyp list* ctyp* (ctyp* ctyp) list)
  | `TyObj of (loc* ctyp* row_var_flag) | `TyOlb of (loc* string* ctyp)
  | `TyPol of (loc* ctyp* ctyp) | `TyTypePol of (loc* ctyp* ctyp)
  | `TyQuo of (loc* string) | `TyQuP of (loc* string)
  | `TyQuM of (loc* string) | `TyAnP of loc | `TyAnM of loc
  | `TyVrn of (loc* string) | `TyRec of (loc* ctyp)
  | `TyCol of (loc* ctyp* ctyp) | `TySem of (loc* ctyp* ctyp)
  | `TyCom of (loc* ctyp* ctyp) | `TySum of (loc* ctyp)
  | `TyOf of (loc* ctyp* ctyp) | `TyAnd of (loc* ctyp* ctyp)
  | `TyOr of (loc* ctyp* ctyp) | `TyPrv of (loc* ctyp)
  | `TyMut of (loc* ctyp) | `TyTup of (loc* ctyp)
  | `TySta of (loc* ctyp* ctyp) | `TyVrnEq of (loc* ctyp)
  | `TyVrnSup of (loc* ctyp) | `TyVrnInf of (loc* ctyp)
  | `TyVrnInfSup of (loc* ctyp* ctyp) | `TyAmp of (loc* ctyp* ctyp)
  | `TyOfAmp of (loc* ctyp* ctyp) | `TyPkg of (loc* module_type)
  | `Ant of (loc* string)] 
and patt =
  [ `PaNil of loc | `PaId of (loc* ident) | `PaAli of (loc* patt* patt)
  | `Ant of (loc* string) | `PaAny of loc | `PaApp of (loc* patt* patt)
  | `PaArr of (loc* patt) | `PaCom of (loc* patt* patt)
  | `PaSem of (loc* patt* patt) | `Chr of (loc* string)
  | `Int of (loc* string) | `Int32 of (loc* string) | `Int64 of (loc* string)
  | `NativeInt of (loc* string) | `Flo of (loc* string)
  | `PaLab of (loc* string* patt) | `PaOlb of (loc* string* patt)
  | `PaOlbi of (loc* string* patt* expr) | `PaOrp of (loc* patt* patt)
  | `PaRng of (loc* patt* patt) | `PaRec of (loc* patt)
  | `PaEq of (loc* ident* patt) | `Str of (loc* string)
  | `PaTup of (loc* patt) | `PaTyc of (loc* patt* ctyp)
  | `PaTyp of (loc* ident) | `PaVrn of (loc* string) | `Lazy of (loc* patt)
  | `PaMod of (loc* string)] 
and expr =
  [ `ExNil of loc | `ExId of (loc* ident) | `ExAcc of (loc* expr* expr)
  | `Ant of (loc* string) | `ExApp of (loc* expr* expr)
  | `ExAre of (loc* expr* expr) | `ExArr of (loc* expr)
  | `ExSem of (loc* expr* expr) | `ExAsf of loc | `ExAsr of (loc* expr)
  | `ExAss of (loc* expr* expr) | `ExCoe of (loc* expr* ctyp* ctyp)
  | `Flo of (loc* string) | `Chr of (loc* string)
  | `For_loop of (loc* string* expr* expr* direction_flag* expr)
  | `Fun of (loc* match_case) | `ExIfe of (loc* expr* expr* expr)
  | `Int of (loc* string) | `Int32 of (loc* string) | `Int64 of (loc* string)
  | `NativeInt of (loc* string) | `Label of (loc* string* expr)
  | `Lazy of (loc* expr) | `Let_in of (loc* rec_flag* binding* expr)
  | `Let_module of (loc* string* module_expr* expr)
  | `Match of (loc* expr* match_case) | `New of (loc* ident)
  | `Obj of (loc* patt* class_str_item)
  | `Optional_label of (loc* string* expr)
  | `Override_instance of (loc* rec_binding)
  | `Record of (loc* rec_binding* expr) | `Sequence of (loc* expr)
  | `Send of (loc* expr* string) | `String_dot of (loc* expr* expr)
  | `Str of (loc* string) | `Try of (loc* expr* match_case)
  | `ExTup of (loc* expr) | `ExCom of (loc* expr* expr)
  | `Constraint_exp of (loc* expr* ctyp) | `ExVrn of (loc* string)
  | `While of (loc* expr* expr) | `Let_open of (loc* ident* expr)
  | `Local_type_fun of (loc* string* expr)
  | `Package_expr of (loc* module_expr)] 
and module_type =
  [ `MtNil of loc | `MtId of (loc* ident)
  | `MtFun of (loc* string* module_type* module_type)
  | `MtQuo of (loc* string) | `MtSig of (loc* sig_item)
  | `MtWit of (loc* module_type* with_constr) | `MtOf of (loc* module_expr)
  | `Ant of (loc* string)] 
and sig_item =
  [ `SgNil of loc | `SgCls of (loc* class_type) | `SgClt of (loc* class_type)
  | `SgSem of (loc* sig_item* sig_item) | `SgDir of (loc* string* expr)
  | `SgExc of (loc* ctyp) | `SgExt of (loc* string* ctyp* string meta_list)
  | `SgInc of (loc* module_type) | `SgMod of (loc* string* module_type)
  | `SgRecMod of (loc* module_binding) | `SgMty of (loc* string* module_type)
  | `SgOpn of (loc* ident) | `SgTyp of (loc* ctyp)
  | `SgVal of (loc* string* ctyp) | `Ant of (loc* string)] 
and with_constr =
  [ `WcNil of loc | `WcTyp of (loc* ctyp* ctyp)
  | `WcMod of (loc* ident* ident) | `WcTyS of (loc* ctyp* ctyp)
  | `WcMoS of (loc* ident* ident) | `WcAnd of (loc* with_constr* with_constr)
  | `Ant of (loc* string)] 
and binding =
  [ `BiNil of loc | `BiAnd of (loc* binding* binding)
  | `BiEq of (loc* patt* expr) | `Ant of (loc* string)] 
and rec_binding =
  [ `RbNil of loc | `RbSem of (loc* rec_binding* rec_binding)
  | `RbEq of (loc* ident* expr) | `Ant of (loc* string)] 
and module_binding =
  [ `MbNil of loc | `MbAnd of (loc* module_binding* module_binding)
  | `MbColEq of (loc* string* module_type* module_expr)
  | `MbCol of (loc* string* module_type) | `Ant of (loc* string)] 
and match_case =
  [ `McNil of loc | `McOr of (loc* match_case* match_case)
  | `McArr of (loc* patt* expr* expr) | `Ant of (loc* string)] 
and module_expr =
  [ `MeNil of loc | `MeId of (loc* ident)
  | `MeApp of (loc* module_expr* module_expr)
  | `MeFun of (loc* string* module_type* module_expr)
  | `MeStr of (loc* str_item) | `MeTyc of (loc* module_expr* module_type)
  | `MePkg of (loc* expr) | `Ant of (loc* string)] 
and str_item =
  [ `StNil of loc | `StCls of (loc* class_expr) | `StClt of (loc* class_type)
  | `StSem of (loc* str_item* str_item) | `StDir of (loc* string* expr)
  | `StExc of (loc* ctyp* ident meta_option) | `StExp of (loc* expr)
  | `StExt of (loc* string* ctyp* string meta_list)
  | `StInc of (loc* module_expr) | `StMod of (loc* string* module_expr)
  | `StRecMod of (loc* module_binding) | `StMty of (loc* string* module_type)
  | `StOpn of (loc* ident) | `StTyp of (loc* ctyp)
  | `StVal of (loc* rec_flag* binding) | `Ant of (loc* string)] 
and class_type =
  [ `CtNil of loc | `CtCon of (loc* virtual_flag* ident* ctyp)
  | `CtFun of (loc* ctyp* class_type) | `CtSig of (loc* ctyp* class_sig_item)
  | `CtAnd of (loc* class_type* class_type)
  | `CtCol of (loc* class_type* class_type)
  | `CtEq of (loc* class_type* class_type) | `Ant of (loc* string)] 
and class_sig_item =
  [ `CgNil of loc | `CgCtr of (loc* ctyp* ctyp)
  | `CgSem of (loc* class_sig_item* class_sig_item)
  | `CgInh of (loc* class_type) | `CgMth of (loc* string* private_flag* ctyp)
  | `CgVal of (loc* string* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (loc* string* private_flag* ctyp) | `Ant of (loc* string)] 
and class_expr =
  [ `CeNil of loc | `CeApp of (loc* class_expr* expr)
  | `CeCon of (loc* virtual_flag* ident* ctyp)
  | `CeFun of (loc* patt* class_expr)
  | `CeLet of (loc* rec_flag* binding* class_expr)
  | `CeStr of (loc* patt* class_str_item)
  | `CeTyc of (loc* class_expr* class_type)
  | `CeAnd of (loc* class_expr* class_expr)
  | `CeEq of (loc* class_expr* class_expr) | `Ant of (loc* string)] 
and class_str_item =
  [ `CrNil of loc | `CrSem of (loc* class_str_item* class_str_item)
  | `CrCtr of (loc* ctyp* ctyp)
  | `CrInh of (loc* override_flag* class_expr* string)
  | `CrIni of (loc* expr)
  | `CrMth of (loc* string* override_flag* private_flag* expr* ctyp)
  | `CrVal of (loc* string* override_flag* mutable_flag* expr)
  | `CrVir of (loc* string* private_flag* ctyp)
  | `CrVvr of (loc* string* mutable_flag* ctyp) | `Ant of (loc* string)] 