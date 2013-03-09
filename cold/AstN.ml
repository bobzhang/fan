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
type strings = [ `App of (strings* strings) | `Str of string | ant] 
type alident = [ `Lid of string | ant] 
type auident = [ `Uid of string | ant] 
type aident = [ alident | auident] 
type astring = [ `C of string | ant] 
type uident =
  [ `Dot of (uident* uident) | `App of (uident* uident) | auident] 
type ident =
  [ `Dot of (ident* ident) | `App of (ident* ident) | alident | auident] 
type dupath = [ `Dot of (dupath* dupath) | auident] 
type dlpath = [ `Dot of (dupath* alident) | alident] 
type sid = [ `Id of ident] 
type any = [ `Any] 
type ctyp =
  [ nil | `Alias of (ctyp* alident) | any | `App of (ctyp* ctyp)
  | `Arrow of (ctyp* ctyp) | `ClassPath of ident | `Label of (alident* ctyp)
  | `OptLabl of (alident* ctyp) | sid | `TyObj of (name_ctyp* row_var_flag)
  | `TyPol of (ctyp* ctyp) | `TyTypePol of (ctyp* ctyp)
  | `Quote of (position_flag* alident) | `QuoteAny of position_flag
  | `Tup of ctyp | `Sta of (ctyp* ctyp) | `PolyEq of row_field
  | `PolySup of row_field | `PolyInf of row_field
  | `PolyInfSup of (row_field* tag_names) | `Package of module_type | 
    ant]
  
and type_parameters =
  [ `Com of (type_parameters* type_parameters) | `Ctyp of ctyp | ant | nil] 
and row_field =
  [ ant_nil | `Or of (row_field* row_field) | `TyVrn of astring
  | `TyVrnOf of (astring* ctyp) | `Ctyp of ctyp] 
and tag_names =
  [ ant_nil | `App of (tag_names* tag_names) | `TyVrn of astring] 
and typedecl =
  [ `TyDcl of (alident* ctyp list* type_info* (ctyp* ctyp) list)
  | `And of (typedecl* typedecl) | ant_nil] 
and type_info =
  [ `TyMan of (ctyp* private_flag* type_repr)
  | `TyRepr of (private_flag* type_repr) | `TyEq of (private_flag* ctyp)
  | ant | nil] 
and type_repr = [ `Record of name_ctyp | `Sum of or_ctyp | ant | nil] 
and name_ctyp =
  [ `Sem of (name_ctyp* name_ctyp) | `TyCol of (sid* ctyp)
  | `TyColMut of (sid* ctyp) | ant | nil] 
and or_ctyp =
  [ `Or of (or_ctyp* or_ctyp) | `TyCol of (sid* ctyp) | `Of of (sid* ctyp)
  | sid | ant_nil] 
and of_ctyp = [ `Of of (sid* ctyp) | sid | ant | nil] 
and patt =
  [ nil | sid | `App of (patt* patt) | `Vrn of string | `Com of (patt* patt)
  | `Sem of (patt* patt) | `Tup of patt | any | `Record of rec_patt | 
    ant
  | literal | `Alias of (patt* alident) | `Array of patt
  | `Label of (alident* patt) | `OptLabl of (alident* patt)
  | `OptLablExpr of (alident* patt* expr) | `Or of (patt* patt)
  | `PaRng of (patt* patt) | `Constraint of (patt* ctyp)
  | `ClassPath of ident | `Lazy of patt | `ModuleUnpack of auident
  | `ModuleConstraint of (auident* ctyp)] 
and rec_patt =
  [ `RecBind of (ident* patt) | `Sem of (rec_patt* rec_patt) | any | ant] 
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
  | `New of ident | `Obj of class_str_item
  | `ObjPat of (patt* class_str_item) | `OptLabl of (alident* expr)
  | `OvrInst of rec_expr | `OvrInstEmpty | `Seq of expr
  | `Send of (expr* alident) | `StringDot of (expr* expr)
  | `Try of (expr* match_case) | `Constraint of (expr* ctyp)
  | `Coercion of (expr* ctyp* ctyp) | `While of (expr* expr)
  | `LetOpen of (ident* expr) | `LocalTypeFun of (alident* expr)
  | `Package_expr of module_expr] 
and rec_expr =
  [ `Sem of (rec_expr* rec_expr) | `RecBind of (ident* expr) | any | ant] 
and module_type =
  [ nil | sid | `MtFun of (auident* module_type* module_type)
  | `Sig of sig_item | `With of (module_type* with_constr)
  | `ModuleTypeOf of module_expr | ant] 
and sig_item =
  [ nil | `Class of class_type | `ClassType of class_type
  | `Sem of (sig_item* sig_item) | `DirectiveSimple of alident
  | `Directive of (alident* expr) | `Exception of of_ctyp
  | `External of (alident* ctyp* strings) | `Include of module_type
  | `Module of (auident* module_type) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `Open of ident
  | `Type of typedecl | `Val of (alident* ctyp) | ant] 
and with_constr =
  [ nil | `TypeEq of (ctyp* ctyp) | `TypeEqPriv of (ctyp* ctyp)
  | `ModuleEq of (ident* ident) | `TypeSubst of (ctyp* ctyp)
  | `ModuleSubst of (ident* ident) | `And of (with_constr* with_constr)
  | ant] 
and binding =
  [ nil | `And of (binding* binding) | `Bind of (patt* expr) | ant] 
and module_binding =
  [ nil | `And of (module_binding* module_binding)
  | `ModuleBind of (auident* module_type* module_expr)
  | `Constraint of (auident* module_type) | ant] 
and match_case =
  [ nil | `Or of (match_case* match_case) | `Case of (patt* expr)
  | `CaseWhen of (patt* expr* expr) | ant] 
and module_expr =
  [ nil | sid | `App of (module_expr* module_expr)
  | `Functor of (auident* module_type* module_expr) | `Struct of str_item
  | `Constraint of (module_expr* module_type) | `PackageModule of expr | 
    ant]
  
and str_item =
  [ nil | `Class of class_expr | `ClassType of class_type
  | `Sem of (str_item* str_item) | `DirectiveSimple of alident
  | `Directive of (alident* expr) | `Exception of of_ctyp | `StExp of expr
  | `External of (alident* ctyp* strings) | `Include of module_expr
  | `Module of (auident* module_expr) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `Open of ident
  | `Type of typedecl | `Value of (rec_flag* binding) | ant] 
and class_type =
  [ nil | `CtCon of (virtual_flag* ident* type_parameters)
  | `CtFun of (ctyp* class_type) | `CtSig of (ctyp* class_sig_item)
  | `And of (class_type* class_type) | `CtCol of (class_type* class_type)
  | `CtEq of (class_type* class_type) | ant] 
and class_sig_item =
  [ nil | `Eq of (ctyp* ctyp) | `Sem of (class_sig_item* class_sig_item)
  | `SigInherit of class_type | `Method of (alident* private_flag* ctyp)
  | `CgVal of (alident* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (alident* private_flag* ctyp) | ant] 
and class_expr =
  [ nil | `CeApp of (class_expr* expr)
  | `CeCon of (virtual_flag* ident* type_parameters)
  | `CeFun of (patt* class_expr) | `CeLet of (rec_flag* binding* class_expr)
  | `Obj of class_str_item | `ObjPat of (patt* class_str_item)
  | `CeTyc of (class_expr* class_type) | `And of (class_expr* class_expr)
  | `Eq of (class_expr* class_expr) | ant] 
and class_str_item =
  [ nil | `Sem of (class_str_item* class_str_item) | `Eq of (ctyp* ctyp)
  | `Inherit of (override_flag* class_expr)
  | `InheritAs of (override_flag* class_expr* alident) | `Initializer of expr
  | `CrMth of (alident* override_flag* private_flag* expr* ctyp)
  | `CrVal of (alident* override_flag* mutable_flag* expr)
  | `CrVir of (alident* private_flag* ctyp)
  | `CrVvr of (alident* mutable_flag* ctyp) | ant] 
type ep =
  [ nil | sid | `App of (ep* ep) | `Vrn of string | `Com of (ep* ep)
  | `Sem of (ep* ep) | `Tup of ep | any | `Array of ep | `Record of rec_bind
  | literal | ant] 
and rec_bind =
  [ `RecBind of (ident* ep) | `Sem of (rec_bind* rec_bind) | any | ant] 