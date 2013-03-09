type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type nil = [ `Nil of loc] 
type ant_nil = [ ant | nil] 
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
type strings =
  [ `App of (loc* strings* strings) | `Str of (loc* string) | ant] 
type alident = [ `Lid of (loc* string) | ant] 
type auident = [ `Uid of (loc* string) | ant] 
type aident = [ alident | auident] 
type astring = [ `C of (loc* string) | ant] 
type uident =
  [ `Dot of (loc* uident* uident) | `App of (loc* uident* uident) | auident] 
type ident =
  [ `Dot of (loc* ident* ident) | `App of (loc* ident* ident) | alident
  | auident] 
type dupath = [ `Dot of (loc* dupath* dupath) | auident] 
type dlpath = [ `Dot of (loc* dupath* alident) | alident] 
type sid = [ `Id of (loc* ident)] 
type any = [ `Any of loc] 
type ctyp =
  [ nil | `Alias of (loc* ctyp* alident) | any | `App of (loc* ctyp* ctyp)
  | `Arrow of (loc* ctyp* ctyp) | `ClassPath of (loc* ident)
  | `Label of (loc* alident* ctyp) | `OptLabl of (loc* alident* ctyp) | 
    sid
  | `TyObj of (loc* name_ctyp* row_var_flag) | `TyPol of (loc* ctyp* ctyp)
  | `TyTypePol of (loc* ctyp* ctyp) | `Quote of (loc* position_flag* alident)
  | `QuoteAny of (loc* position_flag) | `Tup of (loc* ctyp)
  | `Sta of (loc* ctyp* ctyp) | `PolyEq of (loc* row_field)
  | `PolySup of (loc* row_field) | `PolyInf of (loc* row_field)
  | `PolyInfSup of (loc* row_field* tag_names)
  | `Package of (loc* module_type) | ant] 
and type_parameters =
  [ `Com of (loc* type_parameters* type_parameters) | `Ctyp of (loc* ctyp)
  | ant | nil] 
and row_field =
  [ ant_nil | `Or of (loc* row_field* row_field) | `TyVrn of (loc* astring)
  | `TyVrnOf of (loc* astring* ctyp) | `Ctyp of (loc* ctyp)] 
and tag_names =
  [ ant_nil | `App of (loc* tag_names* tag_names) | `TyVrn of (loc* astring)] 
and typedecl =
  [ `TyDcl of (loc* alident* ctyp list* type_info* (ctyp* ctyp) list)
  | `And of (loc* typedecl* typedecl) | ant_nil] 
and type_info =
  [ `TyMan of (loc* ctyp* private_flag* type_repr)
  | `TyRepr of (loc* private_flag* type_repr)
  | `TyEq of (loc* private_flag* ctyp) | ant | nil] 
and type_repr =
  [ `Record of (loc* name_ctyp) | `Sum of (loc* or_ctyp) | ant | nil] 
and name_ctyp =
  [ `Sem of (loc* name_ctyp* name_ctyp) | `TyCol of (loc* sid* ctyp)
  | `TyColMut of (loc* sid* ctyp) | ant | nil] 
and or_ctyp =
  [ `Or of (loc* or_ctyp* or_ctyp) | `TyCol of (loc* sid* ctyp)
  | `Of of (loc* sid* ctyp) | sid | ant_nil] 
and of_ctyp = [ `Of of (loc* sid* ctyp) | sid | ant | nil] 
and patt =
  [ nil | sid | `App of (loc* patt* patt) | `Vrn of (loc* string)
  | `Com of (loc* patt* patt) | `Sem of (loc* patt* patt)
  | `Tup of (loc* patt) | any | `Record of (loc* rec_patt) | ant | literal
  | `Alias of (loc* patt* alident) | `Array of (loc* patt)
  | `Label of (loc* alident* patt) | `OptLabl of (loc* alident* patt)
  | `OptLablExpr of (loc* alident* patt* expr) | `Or of (loc* patt* patt)
  | `PaRng of (loc* patt* patt) | `Constraint of (loc* patt* ctyp)
  | `ClassPath of (loc* ident) | `Lazy of (loc* patt)
  | `ModuleUnpack of (loc* auident)
  | `ModuleConstraint of (loc* auident* ctyp)] 
and rec_patt =
  [ nil | `RecBind of (loc* ident* patt) | `Sem of (loc* rec_patt* rec_patt)
  | any | ant] 
and expr =
  [ nil | sid | `App of (loc* expr* expr) | `Vrn of (loc* string)
  | `Com of (loc* expr* expr) | `Sem of (loc* expr* expr)
  | `Tup of (loc* expr) | any | `Record of (loc* rec_expr) | ant | literal
  | `RecordWith of (loc* rec_expr* expr) | `Dot of (loc* expr* expr)
  | `ArrayDot of (loc* expr* expr) | `Array of (loc* expr) | `ExAsf of loc
  | `ExAsr of (loc* expr) | `Assign of (loc* expr* expr)
  | `For of (loc* alident* expr* expr* direction_flag* expr)
  | `Fun of (loc* match_case) | `IfThenElse of (loc* expr* expr* expr)
  | `IfThen of (loc* expr* expr) | `Label of (loc* alident* expr)
  | `Lazy of (loc* expr) | `LetIn of (loc* rec_flag* binding* expr)
  | `LetModule of (loc* auident* module_expr* expr)
  | `Match of (loc* expr* match_case) | `New of (loc* ident)
  | `Obj of (loc* class_str_item) | `ObjPat of (loc* patt* class_str_item)
  | `OptLabl of (loc* alident* expr) | `OvrInst of (loc* rec_expr)
  | `Seq of (loc* expr) | `Send of (loc* expr* alident)
  | `StringDot of (loc* expr* expr) | `Try of (loc* expr* match_case)
  | `Constraint of (loc* expr* ctyp) | `Coercion of (loc* expr* ctyp* ctyp)
  | `While of (loc* expr* expr) | `LetOpen of (loc* ident* expr)
  | `LocalTypeFun of (loc* alident* expr)
  | `Package_expr of (loc* module_expr)] 
and rec_expr =
  [ nil | `Sem of (loc* rec_expr* rec_expr) | `RecBind of (loc* ident* expr)
  | any | ant] 
and module_type =
  [ nil | sid | `MtFun of (loc* auident* module_type* module_type)
  | `Sig of (loc* sig_item) | `With of (loc* module_type* with_constr)
  | `ModuleTypeOf of (loc* module_expr) | ant] 
and sig_item =
  [ nil | `Class of (loc* class_type) | `ClassType of (loc* class_type)
  | `Sem of (loc* sig_item* sig_item) | `DirectiveSimple of (loc* alident)
  | `Directive of (loc* alident* expr) | `Exception of (loc* of_ctyp)
  | `External of (loc* alident* ctyp* strings)
  | `Include of (loc* module_type) | `Module of (loc* auident* module_type)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* auident* module_type) | `Open of (loc* ident)
  | `Type of (loc* typedecl) | `Val of (loc* alident* ctyp) | ant] 
and with_constr =
  [ nil | `TypeEq of (loc* ctyp* ctyp) | `TypeEqPriv of (loc* ctyp* ctyp)
  | `ModuleEq of (loc* ident* ident) | `TypeSubst of (loc* ctyp* ctyp)
  | `ModuleSubst of (loc* ident* ident)
  | `And of (loc* with_constr* with_constr) | ant] 
and binding =
  [ nil | `And of (loc* binding* binding) | `Bind of (loc* patt* expr) | ant] 
and module_binding =
  [ nil | `And of (loc* module_binding* module_binding)
  | `ModuleBind of (loc* auident* module_type* module_expr)
  | `Constraint of (loc* auident* module_type) | ant] 
and match_case =
  [ nil | `Or of (loc* match_case* match_case) | `Case of (loc* patt* expr)
  | `CaseWhen of (loc* patt* expr* expr) | ant] 
and module_expr =
  [ nil | sid | `App of (loc* module_expr* module_expr)
  | `Functor of (loc* auident* module_type* module_expr)
  | `Struct of (loc* str_item)
  | `Constraint of (loc* module_expr* module_type)
  | `PackageModule of (loc* expr) | ant] 
and str_item =
  [ nil | `Class of (loc* class_expr) | `ClassType of (loc* class_type)
  | `Sem of (loc* str_item* str_item) | `DirectiveSimple of (loc* alident)
  | `Directive of (loc* alident* expr) | `Exception of (loc* of_ctyp)
  | `StExp of (loc* expr) | `External of (loc* alident* ctyp* strings)
  | `Include of (loc* module_expr) | `Module of (loc* auident* module_expr)
  | `RecModule of (loc* module_binding)
  | `ModuleType of (loc* auident* module_type) | `Open of (loc* ident)
  | `Type of (loc* typedecl) | `Value of (loc* rec_flag* binding) | ant] 
and class_type =
  [ nil | `CtCon of (loc* virtual_flag* ident* type_parameters)
  | `CtFun of (loc* ctyp* class_type) | `CtSig of (loc* ctyp* class_sig_item)
  | `And of (loc* class_type* class_type)
  | `CtCol of (loc* class_type* class_type)
  | `CtEq of (loc* class_type* class_type) | ant] 
and class_sig_item =
  [ nil | `Eq of (loc* ctyp* ctyp)
  | `Sem of (loc* class_sig_item* class_sig_item)
  | `SigInherit of (loc* class_type)
  | `Method of (loc* alident* private_flag* ctyp)
  | `CgVal of (loc* alident* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (loc* alident* private_flag* ctyp) | ant] 
and class_expr =
  [ nil | `CeApp of (loc* class_expr* expr)
  | `CeCon of (loc* virtual_flag* ident* type_parameters)
  | `CeFun of (loc* patt* class_expr)
  | `CeLet of (loc* rec_flag* binding* class_expr)
  | `Obj of (loc* class_str_item) | `ObjPat of (loc* patt* class_str_item)
  | `CeTyc of (loc* class_expr* class_type)
  | `And of (loc* class_expr* class_expr)
  | `Eq of (loc* class_expr* class_expr) | ant] 
and class_str_item =
  [ nil | `Sem of (loc* class_str_item* class_str_item)
  | `Eq of (loc* ctyp* ctyp) | `Inherit of (loc* override_flag* class_expr)
  | `InheritAs of (loc* override_flag* class_expr* alident)
  | `Initializer of (loc* expr)
  | `CrMth of (loc* alident* override_flag* private_flag* expr* ctyp)
  | `CrVal of (loc* alident* override_flag* mutable_flag* expr)
  | `CrVir of (loc* alident* private_flag* ctyp)
  | `CrVvr of (loc* alident* mutable_flag* ctyp) | ant] 
type ep =
  [ nil | sid | `App of (loc* ep* ep) | `Vrn of (loc* string)
  | `Com of (loc* ep* ep) | `Sem of (loc* ep* ep) | `Tup of (loc* ep) | 
    any
  | `Array of (loc* ep) | `Record of (loc* rec_bind) | literal | ant] 
and rec_bind =
  [ nil | `RecBind of (loc* ident* ep) | `Sem of (loc* rec_bind* rec_bind)
  | any | ant] 