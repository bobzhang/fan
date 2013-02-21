let _ = ()
type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type nil = [ `Nil] 
type ant_nil = [ ant | nil] 
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
type sid = [ `Id of ident] 
type any = [ `Any] 
type ctyp =
  [ `Nil | `Alias of (ctyp* alident) | any | `App of (ctyp* ctyp)
  | `Arrow of (ctyp* ctyp) | `ClassPath of ident | `Label of (alident* ctyp)
  | `OptLabl of (alident* ctyp) | sid | `TyObj of (name_ctyp* row_var_flag)
  | `TyPol of (ctyp* ctyp) | `TyTypePol of (ctyp* ctyp)
  | `Quote of (position_flag* alident meta_option) | `TyCol of (sid* ctyp)
  | `Com of (ctyp* ctyp) | `Of of (ctyp* ctyp) | `And of (ctyp* ctyp)
  | `Or of (ctyp* ctyp) | `Tup of ctyp | `Sta of (ctyp* ctyp)
  | `TyVrn of astring | `TyVrnEq of ctyp | `TyVrnSup of ctyp
  | `TyVrnInf of ctyp | `TyVrnInfSup of (ctyp* ctyp) | `Amp of (ctyp* ctyp)
  | `TyOfAmp of (ctyp* ctyp) | `Package of module_type | ant] 
and typedecl =
  [ `TyDcl of (alident* ctyp list* type_info* (ctyp* ctyp) list)
  | `And of (typedecl* typedecl) | ant_nil] 
and type_info =
  [ `TyMan of (ctyp* private_flag* type_repr)
  | `TyRepr of (private_flag* type_repr) | `TyEq of (private_flag* ctyp)
  | ant_nil] 
and type_repr = [ `Record of name_ctyp | `Sum of ctyp | ant_nil] 
and name_ctyp =
  [ `Sem of (name_ctyp* name_ctyp) | `TyCol of (sid* ctyp)
  | `TyColMut of (sid* ctyp) | ant_nil] 
and or_ctyp =
  [ `Or of (or_ctyp* or_ctyp) | `TyCol of (sid* ctyp) | `Of of (ctyp* ctyp)
  | sid | ant_nil] 
and of_ctyp = [ `Of of (sid* ctyp) | sid | ant_nil] 
and patt =
  [ nil | sid | `App of (patt* patt) | `Vrn of string | `Com of (patt* patt)
  | `Sem of (patt* patt) | `Tup of patt | any | `Record of rec_patt | 
    ant
  | literal | `Alias of (patt* alident) | `Array of patt
  | `Label of (alident* patt) | `PaOlbi of (alident* patt* expr meta_option)
  | `Or of (patt* patt) | `PaRng of (patt* patt)
  | `Constraint of (patt* ctyp) | `ClassPath of ident | `Lazy of patt
  | `ModuleUnpack of (auident* ctyp meta_option)] 
and rec_patt =
  [ nil | `RecBind of (ident* patt) | `Sem of (rec_patt* rec_patt) | 
    any
  | ant] 
and expr =
  [ nil | sid | `App of (expr* expr) | `Vrn of string | `Com of (expr* expr)
  | `Sem of (expr* expr) | `Tup of expr | any | `Record of rec_expr | 
    ant
  | literal | `RecordWith of (rec_expr* expr) | `Dot of (expr* expr)
  | `ArrayDot of (expr* expr) | `Array of expr | `ExAsf | `ExAsr of expr
  | `Assign of (expr* expr)
  | `For of (alident* expr* expr* direction_flag* expr) | `Fun of match_case
  | `IfThenElse of (expr* expr* expr) | `IfThen of (expr* expr)
  | `Label of (alident* expr) | `Lazy of expr
  | `LetIn of (rec_flag* binding* expr)
  | `LetModule of (auident* module_expr* expr) | `Match of (expr* match_case)
  | `New of ident | `Obj of (patt* class_str_item)
  | `OptLabl of (alident* expr) | `OvrInst of rec_expr | `Seq of expr
  | `Send of (expr* alident) | `StringDot of (expr* expr)
  | `Try of (expr* match_case) | `Constraint of (expr* ctyp)
  | `Coercion of (expr* ctyp* ctyp) | `While of (expr* expr)
  | `LetOpen of (ident* expr) | `LocalTypeFun of (alident* expr)
  | `Package_expr of module_expr] 
and rec_expr =
  [ `Nil | `Sem of (rec_expr* rec_expr) | `RecBind of (ident* expr) | 
    any
  | ant] 
and module_type =
  [ nil | sid | `MtFun of (auident* module_type* module_type)
  | `Sig of sig_item | `With of (module_type* with_constr)
  | `ModuleTypeOf of module_expr | ant] 
and sig_item =
  [ nil | `Class of class_type | `ClassType of class_type
  | `Sem of (sig_item* sig_item) | `Directive of (alident* expr)
  | `Exception of of_ctyp | `External of (alident* ctyp* string meta_list)
  | `Include of module_type | `Module of (auident* module_type)
  | `RecModule of module_binding | `ModuleType of (auident* module_type)
  | `Open of ident | `Type of typedecl | `Val of (alident* ctyp) | ant] 
and with_constr =
  [ `Nil | `TypeEq of (ctyp* ctyp) | `TypeEqPriv of (ctyp* ctyp)
  | `ModuleEq of (ident* ident) | `TypeSubst of (ctyp* ctyp)
  | `ModuleSubst of (ident* ident) | `And of (with_constr* with_constr)
  | ant] 
and binding =
  [ `Nil | `And of (binding* binding) | `Bind of (patt* expr) | ant] 
and module_binding =
  [ `Nil | `And of (module_binding* module_binding)
  | `ModuleBind of (auident* module_type* module_expr)
  | `Constraint of (auident* module_type) | ant] 
and match_case =
  [ nil | `Or of (match_case* match_case) | `Case of (patt* expr* expr)
  | ant] 
and module_expr =
  [ nil | sid | `App of (module_expr* module_expr)
  | `Functor of (auident* module_type* module_expr) | `Struct of str_item
  | `Constraint of (module_expr* module_type) | `PackageModule of expr | 
    ant]
  
and str_item =
  [ `Nil | `Class of class_expr | `ClassType of class_type
  | `Sem of (str_item* str_item) | `Directive of (alident* expr)
  | `Exception of of_ctyp | `StExp of expr
  | `External of (alident* ctyp* string meta_list) | `Include of module_expr
  | `Module of (auident* module_expr) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `Open of ident
  | `Type of typedecl | `Value of (rec_flag* binding) | ant] 
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
type ep =
  [ nil | sid | `App of (ep* ep) | `Vrn of string | `Com of (ep* ep)
  | `Sem of (ep* ep) | `Tup of ep | any | `Array of ep | `Record of rec_bind
  | ant | literal] 
and rec_bind =
  [ `Nil | `RecBind of (ident* ep) | `Sem of (rec_bind* rec_bind) | `Any
  | ant] 