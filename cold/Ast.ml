type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type literal =
  [ `Chr of (loc* string) | `Int of (loc* string) | `Int32 of (loc* string)
  | `Int64 of (loc* string) | `Flo of (loc* string)
  | `NativeInt of (loc* string) | `Str of (loc* string)] 
type rec_flag = [ `Recursive of loc | `ReNil of loc | ant] 
type direction_flag = [ `To of loc | `Downto of loc | ant] 
type mutable_flag = [ `Mutable of loc | `MuNil of loc | ant] 
type private_flag = [ `Private of loc | `PrNil of loc | ant] 
type virtual_flag = [ `Virtual of loc | `ViNil of loc | ant] 
type override_flag = [ `Override of loc | `OvNil of loc | ant] 
type row_var_flag = [ `RowVar of loc | `RvNil of loc | ant] 
type position_flag =
  [ `Positive of loc | `Negative of loc | `Normal of loc | ant] 
type meta_bool = [ `True of loc | `False of loc | ant] 
type 'a meta_option = [ `None | `Some of 'a | ant] 
type 'a meta_list = [ `LNil | `LCons of ('a* 'a meta_list) | ant] 
type alident = [ `Lid of (loc* string) | ant] 
type auident = [ `Uid of (loc* string) | ant] 
type aident = [ alident | auident] 
type astring = [ `C of (loc* string) | ant] 
type ident =
  [ `Dot of (loc* ident* ident) | `App of (loc* ident* ident) | alident
  | auident] 
type ctyp =
  [ `Nil of loc | `Alias of (loc* ctyp* ctyp) | `Any of loc
  | `App of (loc* ctyp* ctyp) | `Arrow of (loc* ctyp* ctyp)
  | `ClassPath of (loc* ident) | `Label of (loc* alident* ctyp)
  | `TyOlb of (loc* alident* ctyp) | `Id of (loc* ident)
  | `TyMan of (loc* ctyp* ctyp)
  | `TyDcl of (loc* alident* ctyp list* ctyp* (ctyp* ctyp) list)
  | `TyObj of (loc* name_ctyp* row_var_flag) | `TyPol of (loc* ctyp* ctyp)
  | `TyTypePol of (loc* ctyp* ctyp)
  | `Quote of (loc* position_flag* alident meta_option)
  | `Record of (loc* name_ctyp) | `TyCol of (loc* ctyp* ctyp)
  | `Sem of (loc* ctyp* ctyp) | `Com of (loc* ctyp* ctyp)
  | `Sum of (loc* ctyp) | `Of of (loc* ctyp* ctyp)
  | `And of (loc* ctyp* ctyp) | `Or of (loc* ctyp* ctyp)
  | `Priv of (loc* ctyp) | `Mut of (loc* ctyp) | `Tup of (loc* ctyp)
  | `Sta of (loc* ctyp* ctyp) | `TyVrn of (loc* astring)
  | `TyVrnEq of (loc* ctyp) | `TyVrnSup of (loc* ctyp)
  | `TyVrnInf of (loc* ctyp) | `TyVrnInfSup of (loc* ctyp* ctyp)
  | `Amp of (loc* ctyp* ctyp) | `TyOfAmp of (loc* ctyp* ctyp)
  | `Package of (loc* module_type) | ant] 
and name_ctyp =
  [ `Sem of (loc* name_ctyp* name_ctyp) | `TyCol of (loc* ctyp* ctyp)
  | `Nil of loc | ant] 
and patt =
  [ `Nil of loc | `Id of (loc* ident) | `App of (loc* patt* patt)
  | `Vrn of (loc* string) | `Com of (loc* patt* patt)
  | `Sem of (loc* patt* patt) | `Tup of (loc* patt) | `Any of loc
  | `Record of (loc* rec_patt) | ant | literal
  | `Alias of (loc* patt* alident) | `Array of (loc* patt)
  | `Label of (loc* alident* patt)
  | `PaOlbi of (loc* alident* patt* expr meta_option)
  | `Or of (loc* patt* patt) | `PaRng of (loc* patt* patt)
  | `Constraint of (loc* patt* ctyp) | `ClassPath of (loc* ident)
  | `Lazy of (loc* patt) | `ModuleUnpack of (loc* auident* ctyp meta_option)] 
and rec_patt =
  [ `Nil of loc | `RecBind of (loc* ident* patt)
  | `Sem of (loc* rec_patt* rec_patt) | `Any of loc | ant] 
and expr =
  [ `Nil of loc | `Id of (loc* ident) | `App of (loc* expr* expr)
  | `Vrn of (loc* string) | `Com of (loc* expr* expr)
  | `Sem of (loc* expr* expr) | `Tup of (loc* expr) | `Any of loc
  | `Record of (loc* rec_expr) | ant | literal
  | `RecordWith of (loc* rec_expr* expr) | `Dot of (loc* expr* expr)
  | `ArrayDot of (loc* expr* expr) | `Array of (loc* expr) | `ExAsf of loc
  | `ExAsr of (loc* expr) | `Assign of (loc* expr* expr)
  | `For of (loc* alident* expr* expr* direction_flag* expr)
  | `Fun of (loc* match_case) | `IfThenElse of (loc* expr* expr* expr)
  | `IfThen of (loc* expr* expr) | `Label of (loc* alident* expr)
  | `Lazy of (loc* expr) | `LetIn of (loc* rec_flag* binding* expr)
  | `LetModule of (loc* auident* module_expr* expr)
  | `Match of (loc* expr* match_case) | `New of (loc* ident)
  | `Obj of (loc* patt* class_str_item) | `OptLabl of (loc* alident* expr)
  | `OvrInst of (loc* rec_expr) | `Seq of (loc* expr)
  | `Send of (loc* expr* alident) | `StringDot of (loc* expr* expr)
  | `Try of (loc* expr* match_case) | `Constraint of (loc* expr* ctyp)
  | `Coercion of (loc* expr* ctyp* ctyp) | `While of (loc* expr* expr)
  | `LetOpen of (loc* ident* expr) | `LocalTypeFun of (loc* alident* expr)
  | `Package_expr of (loc* module_expr)] 
and rec_expr =
  [ `Nil of loc | `Sem of (loc* rec_expr* rec_expr)
  | `RecBind of (loc* ident* expr) | `Any of loc | ant] 
and module_type =
  [ `Nil of loc | `Id of (loc* ident)
  | `MtFun of (loc* auident* module_type* module_type)
  | `Sig of (loc* sig_item) | `With of (loc* module_type* with_constr)
  | `ModuleTypeOf of (loc* module_expr) | ant] 
and sig_item =
  [ `Nil of loc | `Class of (loc* class_type)
  | `ClassType of (loc* class_type) | `Sem of (loc* sig_item* sig_item)
  | `Directive of (loc* alident* expr) | `Exception of (loc* ctyp)
  | `External of (loc* alident* ctyp* string meta_list)
  | `Include of (loc* module_type) | `Module of (loc* auident* module_type)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* auident* module_type) | `Open of (loc* ident)
  | `Type of (loc* ctyp) | `Val of (loc* alident* ctyp) | ant] 
and with_constr =
  [ `Nil of loc | `TypeEq of (loc* ctyp* ctyp)
  | `ModuleEq of (loc* ident* ident) | `TypeSubst of (loc* ctyp* ctyp)
  | `ModuleSubst of (loc* ident* ident)
  | `And of (loc* with_constr* with_constr) | ant] 
and binding =
  [ `Nil of loc | `And of (loc* binding* binding)
  | `Bind of (loc* patt* expr) | ant] 
and module_binding =
  [ `Nil of loc | `And of (loc* module_binding* module_binding)
  | `ModuleBind of (loc* auident* module_type* module_expr)
  | `Constraint of (loc* auident* module_type) | ant] 
and match_case =
  [ `Nil of loc | `Or of (loc* match_case* match_case)
  | `Case of (loc* patt* expr* expr) | ant] 
and module_expr =
  [ `Nil of loc | `Id of (loc* ident)
  | `App of (loc* module_expr* module_expr)
  | `Functor of (loc* auident* module_type* module_expr)
  | `Struct of (loc* str_item)
  | `Constraint of (loc* module_expr* module_type)
  | `PackageModule of (loc* expr) | ant] 
and str_item =
  [ `Nil of loc | `Class of (loc* class_expr)
  | `ClassType of (loc* class_type) | `Sem of (loc* str_item* str_item)
  | `Directive of (loc* alident* expr) | `Exception of (loc* ctyp)
  | `StExp of (loc* expr)
  | `External of (loc* alident* ctyp* string meta_list)
  | `Include of (loc* module_expr) | `Module of (loc* auident* module_expr)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* auident* module_type) | `Open of (loc* ident)
  | `Type of (loc* ctyp) | `Value of (loc* rec_flag* binding) | ant] 
and class_type =
  [ `Nil of loc | `CtCon of (loc* virtual_flag* ident* ctyp)
  | `CtFun of (loc* ctyp* class_type) | `CtSig of (loc* ctyp* class_sig_item)
  | `And of (loc* class_type* class_type)
  | `CtCol of (loc* class_type* class_type)
  | `CtEq of (loc* class_type* class_type) | ant] 
and class_sig_item =
  [ `Nil of loc | `Eq of (loc* ctyp* ctyp)
  | `Sem of (loc* class_sig_item* class_sig_item)
  | `SigInherit of (loc* class_type)
  | `Method of (loc* alident* private_flag* ctyp)
  | `CgVal of (loc* alident* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (loc* alident* private_flag* ctyp) | ant] 
and class_expr =
  [ `Nil of loc | `CeApp of (loc* class_expr* expr)
  | `CeCon of (loc* virtual_flag* ident* ctyp)
  | `CeFun of (loc* patt* class_expr)
  | `CeLet of (loc* rec_flag* binding* class_expr)
  | `Obj of (loc* patt* class_str_item)
  | `CeTyc of (loc* class_expr* class_type)
  | `And of (loc* class_expr* class_expr)
  | `Eq of (loc* class_expr* class_expr) | ant] 
and class_str_item =
  [ `Nil of loc | `Sem of (loc* class_str_item* class_str_item)
  | `Eq of (loc* ctyp* ctyp)
  | `Inherit of (loc* override_flag* class_expr* alident meta_option)
  | `Initializer of (loc* expr)
  | `CrMth of (loc* alident* override_flag* private_flag* expr* ctyp)
  | `CrVal of (loc* alident* override_flag* mutable_flag* expr)
  | `CrVir of (loc* alident* private_flag* ctyp)
  | `CrVvr of (loc* alident* mutable_flag* ctyp) | ant] 
type ep =
  [ `Nil of loc | `Id of (loc* ident) | `App of (loc* ep* ep)
  | `Vrn of (loc* string) | `Com of (loc* ep* ep) | `Sem of (loc* ep* ep)
  | `Tup of (loc* ep) | `Any of loc | `Array of (loc* ep)
  | `Record of (loc* rec_bind) | ant | literal] 
and rec_bind =
  [ `Nil of loc | `RecBind of (loc* ident* ep)
  | `Sem of (loc* rec_bind* rec_bind) | `Any of loc | ant] 