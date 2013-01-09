type loc = FanLoc.t 
type literal =
  [ `Chr of (loc* string) | `Int of (loc* string) | `Int32 of (loc* string)
  | `Int64 of (loc* string) | `Flo of (loc* string)
  | `NativeInt of (loc* string) | `Str of (loc* string)] 
type rec_flag = [ `Recursive of loc | `ReNil of loc | `Ant of (loc* string)] 
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
  [ `Nil of loc | `Alias of (loc* ctyp* ctyp) | `Any of loc
  | `TyApp of (loc* ctyp* ctyp) | `TyArr of (loc* ctyp* ctyp)
  | `TyCls of (loc* ident) | `TyLab of (loc* string* ctyp)
  | `TyId of (loc* ident) | `TyMan of (loc* ctyp* ctyp)
  | `TyDcl of (loc* string* ctyp list* ctyp* (ctyp* ctyp) list)
  | `TyObj of (loc* ctyp* row_var_flag) | `TyOlb of (loc* string* ctyp)
  | `TyPol of (loc* ctyp* ctyp) | `TyTypePol of (loc* ctyp* ctyp)
  | `TyQuo of (loc* string) | `TyQuP of (loc* string)
  | `TyQuM of (loc* string) | `TyAnP of loc | `TyAnM of loc
  | `TyRec of (loc* ctyp) | `TyCol of (loc* ctyp* ctyp)
  | `TySem of (loc* ctyp* ctyp) | `Com of (loc* ctyp* ctyp)
  | `Sum of (loc* ctyp) | `Of of (loc* ctyp* ctyp)
  | `And of (loc* ctyp* ctyp) | `TyOr of (loc* ctyp* ctyp)
  | `Private of (loc* ctyp) | `Mutable of (loc* ctyp) | `Tup of (loc* ctyp)
  | `Sta of (loc* ctyp* ctyp) | `TyVrn of (loc* string)
  | `TyVrnEq of (loc* ctyp) | `TyVrnSup of (loc* ctyp)
  | `TyVrnInf of (loc* ctyp) | `TyVrnInfSup of (loc* ctyp* ctyp)
  | `TyAmp of (loc* ctyp* ctyp) | `TyOfAmp of (loc* ctyp* ctyp)
  | `Package of (loc* module_type) | `Ant of (loc* string)] 
and patt =
  [ `Nil of loc | `Id of (loc* ident) | `Alias of (loc* patt* patt)
  | `Ant of (loc* string) | `Any of loc | `PaApp of (loc* patt* patt)
  | `Array of (loc* patt) | `PaCom of (loc* patt* patt)
  | `Sem of (loc* patt* patt) | literal | `PaLab of (loc* string* patt)
  | `PaOlb of (loc* string* patt) | `PaOlbi of (loc* string* patt* expr)
  | `PaOrp of (loc* patt* patt) | `PaRng of (loc* patt* patt)
  | `PaRec of (loc* patt) | `PaEq of (loc* ident* patt)
  | `PaTup of (loc* patt) | `PaTyc of (loc* patt* ctyp)
  | `PaTyp of (loc* ident) | `PaVrn of (loc* string) | `Lazy of (loc* patt)
  | `PaMod of (loc* string)] 
and expr =
  [ `Nil of loc | `Id of (loc* ident) | `ExAcc of (loc* expr* expr)
  | `Ant of (loc* string) | `ExApp of (loc* expr* expr)
  | `ExAre of (loc* expr* expr) | `Array of (loc* expr)
  | `Sem of (loc* expr* expr) | `ExAsf of loc | `ExAsr of (loc* expr)
  | `ExAss of (loc* expr* expr) | `ExCoe of (loc* expr* ctyp* ctyp)
  | `For of (loc* string* expr* expr* direction_flag* expr)
  | `Fun of (loc* match_case) | `ExIfe of (loc* expr* expr* expr) | literal
  | `Label of (loc* string* expr) | `Lazy of (loc* expr)
  | `LetIn of (loc* rec_flag* binding* expr)
  | `LetModule of (loc* string* module_expr* expr)
  | `Match of (loc* expr* match_case) | `New of (loc* ident)
  | `Obj of (loc* patt* class_str_item) | `OptLabl of (loc* string* expr)
  | `OvrInst of (loc* rec_binding) | `Record of (loc* rec_binding* expr)
  | `Sequence of (loc* expr) | `Send of (loc* expr* string)
  | `StringDot of (loc* expr* expr) | `Try of (loc* expr* match_case)
  | `ExTup of (loc* expr) | `ExCom of (loc* expr* expr)
  | `Constraint_exp of (loc* expr* ctyp) | `ExVrn of (loc* string)
  | `While of (loc* expr* expr) | `Let_open of (loc* ident* expr)
  | `LocalTypeFun of (loc* string* expr)
  | `Package_expr of (loc* module_expr)] 
and module_type =
  [ `Nil of loc | `Id of (loc* ident)
  | `MtFun of (loc* string* module_type* module_type)
  | `MtQuo of (loc* string) | `Sig of (loc* sig_item)
  | `MtWit of (loc* module_type* with_constr) | `Of of (loc* module_expr)
  | `Ant of (loc* string)] 
and sig_item =
  [ `Nil of loc | `Class of (loc* class_type)
  | `ClassType of (loc* class_type) | `Sem of (loc* sig_item* sig_item)
  | `Directive of (loc* string* expr) | `Exception of (loc* ctyp)
  | `External of (loc* string* ctyp* string meta_list)
  | `Include of (loc* module_type) | `Module of (loc* string* module_type)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* string* module_type) | `Open of (loc* ident)
  | `Type of (loc* ctyp) | `Value of (loc* string* ctyp)
  | `Ant of (loc* string)] 
and with_constr =
  [ `Nil of loc | `TypeEq of (loc* ctyp* ctyp)
  | `ModuleEq of (loc* ident* ident) | `TypeSubst of (loc* ctyp* ctyp)
  | `ModuleSubst of (loc* ident* ident)
  | `And of (loc* with_constr* with_constr) | `Ant of (loc* string)] 
and binding =
  [ `Nil of loc | `And of (loc* binding* binding)
  | `Bind of (loc* patt* expr) | `Ant of (loc* string)] 
and rec_binding =
  [ `Nil of loc | `Sem of (loc* rec_binding* rec_binding)
  | `RecBind of (loc* ident* expr) | `Ant of (loc* string)] 
and module_binding =
  [ `Nil of loc | `And of (loc* module_binding* module_binding)
  | `ModuleBind of (loc* string* module_type* module_expr)
  | `ModuleConstraint of (loc* string* module_type) | `Ant of (loc* string)] 
and match_case =
  [ `Nil of loc | `McOr of (loc* match_case* match_case)
  | `Case of (loc* patt* expr* expr) | `Ant of (loc* string)] 
and module_expr =
  [ `Nil of loc | `Id of (loc* ident)
  | `MeApp of (loc* module_expr* module_expr)
  | `Functor of (loc* string* module_type* module_expr)
  | `Struct of (loc* str_item)
  | `ModuleExprConstraint of (loc* module_expr* module_type)
  | `PackageModule of (loc* expr) | `Ant of (loc* string)] 
and str_item =
  [ `Nil of loc | `Class of (loc* class_expr)
  | `ClassType of (loc* class_type) | `Sem of (loc* str_item* str_item)
  | `Directive of (loc* string* expr)
  | `Exception of (loc* ctyp* ident meta_option) | `StExp of (loc* expr)
  | `External of (loc* string* ctyp* string meta_list)
  | `Include of (loc* module_expr) | `Module of (loc* string* module_expr)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* string* module_type) | `Open of (loc* ident)
  | `Type of (loc* ctyp) | `Value of (loc* rec_flag* binding)
  | `Ant of (loc* string)] 
and class_type =
  [ `CtNil of loc | `CtCon of (loc* virtual_flag* ident* ctyp)
  | `CtFun of (loc* ctyp* class_type) | `CtSig of (loc* ctyp* class_sig_item)
  | `CtAnd of (loc* class_type* class_type)
  | `CtCol of (loc* class_type* class_type)
  | `CtEq of (loc* class_type* class_type) | `Ant of (loc* string)] 
and class_sig_item =
  [ `Nil of loc | `Eq of (loc* ctyp* ctyp)
  | `Sem of (loc* class_sig_item* class_sig_item)
  | `Inherit of (loc* class_type)
  | `Method of (loc* string* private_flag* ctyp)
  | `CgVal of (loc* string* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (loc* string* private_flag* ctyp) | `Ant of (loc* string)] 
and class_expr =
  [ `Nil of loc | `CeApp of (loc* class_expr* expr)
  | `CeCon of (loc* virtual_flag* ident* ctyp)
  | `CeFun of (loc* patt* class_expr)
  | `CeLet of (loc* rec_flag* binding* class_expr)
  | `Obj of (loc* patt* class_str_item)
  | `CeTyc of (loc* class_expr* class_type)
  | `And of (loc* class_expr* class_expr)
  | `Eq of (loc* class_expr* class_expr) | `Ant of (loc* string)] 
and class_str_item =
  [ `Nil of loc | `CrSem of (loc* class_str_item* class_str_item)
  | `Eq of (loc* ctyp* ctyp)
  | `Inherit of (loc* override_flag* class_expr* string)
  | `Initializer of (loc* expr)
  | `CrMth of (loc* string* override_flag* private_flag* expr* ctyp)
  | `CrVal of (loc* string* override_flag* mutable_flag* expr)
  | `CrVir of (loc* string* private_flag* ctyp)
  | `CrVvr of (loc* string* mutable_flag* ctyp) | `Ant of (loc* string)] 