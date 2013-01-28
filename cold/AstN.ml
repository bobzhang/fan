let _ = ()
type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type literal =
  [ `Chr of string | `Int of string | `Int32 of string | `Int64 of string
  | `Flo of string | `NativeInt of string | `Str of string] 
type rec_flag = [ `Recursive | `ReNil | ant] 
type direction_flag = [ `To | `Downto | ant] 
type mutable_flag = [ `Mutable | `MuNil | ant] 
type private_flag = [ `Private | `PrNil | ant] 
type virtual_flag = [ `Virtual | `ViNil | ant] 
type override_flag = [ `Override | `OvNil | ant] 
type row_var_flag = [ `RowVar | `RvNil | ant] 
type position_flag = [ `Positive | `Negative | `Normal | ant] 
type meta_bool = [ `True | `False | ant] 
type 'a meta_option = [ `None | `Some of 'a | ant] 
type 'a meta_list = [ `LNil | `LCons of ('a* 'a meta_list) | ant] 
type alident = [ `Lid of string | ant] 
type auident = [ `Uid of string | ant] 
type aident = [ alident | auident] 
type astring = [ `C of string | ant] 
type ident =
  [ `Dot of (ident* ident) | `App of (ident* ident) | alident | auident] 
type ctyp =
  [ `Nil | `Alias of (ctyp* ctyp) | `Any | `App of (ctyp* ctyp)
  | `Arrow of (ctyp* ctyp) | `ClassPath of ident | `Label of (alident* ctyp)
  | `Id of ident | `TyMan of (ctyp* ctyp)
  | `TyDcl of (alident* ctyp list* ctyp* (ctyp* ctyp) list)
  | `TyObj of (ctyp* row_var_flag) | `TyOlb of (alident* ctyp)
  | `TyPol of (ctyp* ctyp) | `TyTypePol of (ctyp* ctyp)
  | `Quote of (position_flag* alident meta_option) | `TyRec of ctyp
  | `TyCol of (ctyp* ctyp) | `Sem of (ctyp* ctyp) | `Com of (ctyp* ctyp)
  | `Sum of ctyp | `Of of (ctyp* ctyp) | `And of (ctyp* ctyp)
  | `Or of (ctyp* ctyp) | `Priv of ctyp | `Mut of ctyp | `Tup of ctyp
  | `Sta of (ctyp* ctyp) | `TyVrn of astring | `TyVrnEq of ctyp
  | `TyVrnSup of ctyp | `TyVrnInf of ctyp | `TyVrnInfSup of (ctyp* ctyp)
  | `Amp of (ctyp* ctyp) | `TyOfAmp of (ctyp* ctyp) | `Package of module_type
  | ant] 
and patt =
  [ `Nil | `Id of ident | `Alias of (patt* alident) | ant | `Any
  | `App of (patt* patt) | `Array of patt | `Com of (patt* patt)
  | `Sem of (patt* patt) | literal | `Label of (alident* patt)
  | `PaOlbi of (alident* patt* expr meta_option) | `Or of (patt* patt)
  | `PaRng of (patt* patt) | `PaRec of patt | `PaEq of (ident* patt)
  | `Tup of patt | `Constraint of (patt* ctyp) | `ClassPath of ident
  | `Vrn of string | `Lazy of patt
  | `ModuleUnpack of (auident* ctyp meta_option)] 
and expr =
  [ `Nil | `Id of ident | `Dot of (expr* expr) | ant | `App of (expr* expr)
  | `ArrayDot of (expr* expr) | `Array of expr | `Sem of (expr* expr)
  | `ExAsf | `ExAsr of expr | `Assign of (expr* expr)
  | `For of (alident* expr* expr* direction_flag* expr) | `Fun of match_case
  | `IfThenElse of (expr* expr* expr) | `IfThen of (expr* expr) | literal
  | `Label of (alident* expr) | `Lazy of expr
  | `LetIn of (rec_flag* binding* expr)
  | `LetModule of (auident* module_expr* expr) | `Match of (expr* match_case)
  | `New of ident | `Obj of (patt* class_str_item)
  | `OptLabl of (alident* expr) | `OvrInst of rec_binding
  | `Record of (rec_binding* expr) | `Seq of expr | `Send of (expr* alident)
  | `StringDot of (expr* expr) | `Try of (expr* match_case) | `Tup of expr
  | `Com of (expr* expr) | `Constraint of (expr* ctyp)
  | `Coercion of (expr* ctyp* ctyp) | `Vrn of string | `While of (expr* expr)
  | `LetOpen of (ident* expr) | `LocalTypeFun of (alident* expr)
  | `Package_expr of module_expr] 
and module_type =
  [ `Nil | `Id of ident | `MtFun of (auident* module_type* module_type)
  | `Sig of sig_item | `With of (module_type* with_constr)
  | `ModuleTypeOf of module_expr | ant] 
and sig_item =
  [ `Nil | `Class of class_type | `ClassType of class_type
  | `Sem of (sig_item* sig_item) | `Directive of (alident* expr)
  | `Exception of ctyp | `External of (alident* ctyp* string meta_list)
  | `Include of module_type | `Module of (auident* module_type)
  | `RecModule of module_binding | `ModuleType of (auident* module_type)
  | `Open of ident | `Type of ctyp | `Val of (alident* ctyp) | ant] 
and with_constr =
  [ `Nil | `TypeEq of (ctyp* ctyp) | `ModuleEq of (ident* ident)
  | `TypeSubst of (ctyp* ctyp) | `ModuleSubst of (ident* ident)
  | `And of (with_constr* with_constr) | ant] 
and binding =
  [ `Nil | `And of (binding* binding) | `Bind of (patt* expr) | ant] 
and rec_binding =
  [ `Nil | `Sem of (rec_binding* rec_binding) | `RecBind of (ident* expr)
  | ant] 
and module_binding =
  [ `Nil | `And of (module_binding* module_binding)
  | `ModuleBind of (auident* module_type* module_expr)
  | `Constraint of (auident* module_type) | ant] 
and match_case =
  [ `Nil | `Or of (match_case* match_case) | `Case of (patt* expr* expr)
  | ant] 
and module_expr =
  [ `Nil | `Id of ident | `App of (module_expr* module_expr)
  | `Functor of (auident* module_type* module_expr) | `Struct of str_item
  | `Constraint of (module_expr* module_type) | `PackageModule of expr | 
    ant]
  
and str_item =
  [ `Nil | `Class of class_expr | `ClassType of class_type
  | `Sem of (str_item* str_item) | `Directive of (alident* expr)
  | `Exception of ctyp | `StExp of expr
  | `External of (alident* ctyp* string meta_list) | `Include of module_expr
  | `Module of (auident* module_expr) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `Open of ident | `Type of ctyp
  | `Value of (rec_flag* binding) | ant] 
and class_type =
  [ `Nil | `CtCon of (virtual_flag* ident* ctyp)
  | `CtFun of (ctyp* class_type) | `CtSig of (ctyp* class_sig_item)
  | `And of (class_type* class_type) | `CtCol of (class_type* class_type)
  | `CtEq of (class_type* class_type) | ant] 
and class_sig_item =
  [ `Nil | `Eq of (ctyp* ctyp) | `Sem of (class_sig_item* class_sig_item)
  | `SigInherit of class_type | `Method of (alident* private_flag* ctyp)
  | `CgVal of (alident* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (alident* private_flag* ctyp) | ant] 
and class_expr =
  [ `Nil | `CeApp of (class_expr* expr)
  | `CeCon of (virtual_flag* ident* ctyp) | `CeFun of (patt* class_expr)
  | `CeLet of (rec_flag* binding* class_expr)
  | `Obj of (patt* class_str_item) | `CeTyc of (class_expr* class_type)
  | `And of (class_expr* class_expr) | `Eq of (class_expr* class_expr) | 
    ant]
  
and class_str_item =
  [ `Nil | `Sem of (class_str_item* class_str_item) | `Eq of (ctyp* ctyp)
  | `Inherit of (override_flag* class_expr* alident meta_option)
  | `Initializer of expr
  | `CrMth of (alident* override_flag* private_flag* expr* ctyp)
  | `CrVal of (alident* override_flag* mutable_flag* expr)
  | `CrVir of (alident* private_flag* ctyp)
  | `CrVvr of (alident* mutable_flag* ctyp) | ant] 