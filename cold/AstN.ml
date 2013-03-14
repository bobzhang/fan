let _ = (); ()
let _ = ()
type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type nil = [ `Nil] 
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
  [ `Alias of (ctyp* alident) | any | `App of (ctyp* ctyp)
  | `Arrow of (ctyp* ctyp) | `ClassPath of ident | `Label of (alident* ctyp)
  | `OptLabl of (alident* ctyp) | sid | `TyObj of (name_ctyp* row_var_flag)
  | `TyObjEnd of row_var_flag | `TyPol of (ctyp* ctyp) | `TyPolEnd of ctyp
  | `TyTypePol of (ctyp* ctyp) | `Quote of (position_flag* alident)
  | `QuoteAny of position_flag | `Tup of ctyp | `Sta of (ctyp* ctyp)
  | `PolyEq of row_field | `PolySup of row_field | `PolyInf of row_field
  | `PolyInfSup of (row_field* tag_names) | `Package of module_type | 
    ant]
  
and type_parameters =
  [ `Com of (type_parameters* type_parameters) | `Ctyp of ctyp | ant] 
and row_field =
  [ ant | `Or of (row_field* row_field) | `TyVrn of astring
  | `TyVrnOf of (astring* ctyp) | `Ctyp of ctyp] 
and tag_names = [ ant | `App of (tag_names* tag_names) | `TyVrn of astring] 
and typedecl =
  [ `TyDcl of (alident* ctyp list* type_info* (ctyp* ctyp) list)
  | `TyAbstr of (alident* ctyp list* (ctyp* ctyp) list)
  | `And of (typedecl* typedecl) | ant] 
and type_info =
  [ `TyMan of (ctyp* private_flag* type_repr)
  | `TyRepr of (private_flag* type_repr) | `TyEq of (private_flag* ctyp)
  | ant] 
and type_repr = [ `Record of name_ctyp | `Sum of or_ctyp | ant] 
and name_ctyp =
  [ `Sem of (name_ctyp* name_ctyp) | `TyCol of (sid* ctyp)
  | `TyColMut of (sid* ctyp) | ant] 
and or_ctyp =
  [ `Or of (or_ctyp* or_ctyp) | `TyCol of (sid* ctyp) | `Of of (sid* ctyp)
  | sid | ant] 
and of_ctyp = [ `Of of (sid* ctyp) | sid | ant] 
and patt =
  [ sid | `App of (patt* patt) | `Vrn of string | `Com of (patt* patt)
  | `Sem of (patt* patt) | `Tup of patt | any | `Record of rec_patt | 
    ant
  | literal | `Alias of (patt* alident) | `ArrayEmpty | `Array of patt
  | `LabelS of alident | `Label of (alident* patt)
  | `OptLabl of (alident* patt) | `OptLablS of alident
  | `OptLablExpr of (alident* patt* expr) | `Or of (patt* patt)
  | `PaRng of (patt* patt) | `Constraint of (patt* ctyp)
  | `ClassPath of ident | `Lazy of patt | `ModuleUnpack of auident
  | `ModuleConstraint of (auident* ctyp)] 
and rec_patt =
  [ `RecBind of (ident* patt) | `Sem of (rec_patt* rec_patt) | any | ant] 
and expr =
  [ sid | `App of (expr* expr) | `Vrn of string | `Com of (expr* expr)
  | `Sem of (expr* expr) | `Tup of expr | any | `Record of rec_expr | 
    ant
  | literal | `RecordWith of (rec_expr* expr) | `Dot of (expr* expr)
  | `ArrayDot of (expr* expr) | `ArrayEmpty | `Array of expr | `ExAsf
  | `ExAsr of expr | `Assign of (expr* expr)
  | `For of (alident* expr* expr* direction_flag* expr) | `Fun of case
  | `IfThenElse of (expr* expr* expr) | `IfThen of (expr* expr)
  | `LabelS of alident | `Label of (alident* expr) | `Lazy of expr
  | `LetIn of (rec_flag* binding* expr)
  | `LetModule of (auident* module_expr* expr) | `Match of (expr* case)
  | `New of ident | `Obj of class_str_item | `ObjEnd
  | `ObjPat of (patt* class_str_item) | `ObjPatEnd of patt
  | `OptLabl of (alident* expr) | `OptLablS of alident | `OvrInst of rec_expr
  | `OvrInstEmpty | `Seq of expr | `Send of (expr* alident)
  | `StringDot of (expr* expr) | `Try of (expr* case)
  | `Constraint of (expr* ctyp) | `Coercion of (expr* ctyp* ctyp)
  | `Subtype of (expr* ctyp) | `While of (expr* expr)
  | `LetOpen of (ident* expr) | `LocalTypeFun of (alident* expr)
  | `Package_expr of module_expr] 
and rec_expr =
  [ `Sem of (rec_expr* rec_expr) | `RecBind of (ident* expr) | any | ant] 
and module_type =
  [ sid | `Functor of (auident* module_type* module_type) | `Sig of sig_item
  | `SigEnd | `With of (module_type* with_constr)
  | `ModuleTypeOf of module_expr | ant] 
and sig_item =
  [ `Class of class_type | `ClassType of class_type
  | `Sem of (sig_item* sig_item) | `DirectiveSimple of alident
  | `Directive of (alident* expr) | `Exception of of_ctyp
  | `External of (alident* ctyp* strings) | `Include of module_type
  | `Module of (auident* module_type) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `ModuleTypeEnd of auident
  | `Open of ident | `Type of typedecl | `Val of (alident* ctyp) | ant] 
and with_constr =
  [ `TypeEq of (ctyp* ctyp) | `TypeEqPriv of (ctyp* ctyp)
  | `ModuleEq of (ident* ident) | `TypeSubst of (ctyp* ctyp)
  | `ModuleSubst of (ident* ident) | `And of (with_constr* with_constr)
  | ant] 
and binding = [ `And of (binding* binding) | `Bind of (patt* expr) | ant] 
and module_binding =
  [ `And of (module_binding* module_binding)
  | `ModuleBind of (auident* module_type* module_expr)
  | `Constraint of (auident* module_type) | ant] 
and case =
  [ `Or of (case* case) | `Case of (patt* expr)
  | `CaseWhen of (patt* expr* expr) | ant] 
and module_expr =
  [ sid | `App of (module_expr* module_expr)
  | `Functor of (auident* module_type* module_expr) | `Struct of str_item
  | `StructEnd | `Constraint of (module_expr* module_type)
  | `PackageModule of expr | ant] 
and str_item =
  [ `Class of class_expr | `ClassType of class_type
  | `Sem of (str_item* str_item) | `DirectiveSimple of alident
  | `Directive of (alident* expr) | `Exception of of_ctyp | `StExp of expr
  | `External of (alident* ctyp* strings) | `Include of module_expr
  | `Module of (auident* module_expr) | `RecModule of module_binding
  | `ModuleType of (auident* module_type) | `Open of ident
  | `Type of typedecl | `Value of (rec_flag* binding) | ant] 
and class_type =
  [ `ClassCon of (virtual_flag* ident* type_parameters)
  | `ClassConS of (virtual_flag* ident) | `CtFun of (ctyp* class_type)
  | `ObjTy of (ctyp* class_sig_item) | `ObjTyEnd of ctyp
  | `Obj of class_sig_item | `ObjEnd | `And of (class_type* class_type)
  | `CtCol of (class_type* class_type) | `Eq of (class_type* class_type)
  | ant] 
and class_sig_item =
  [ `Eq of (ctyp* ctyp) | `Sem of (class_sig_item* class_sig_item)
  | `SigInherit of class_type | `Method of (alident* private_flag* ctyp)
  | `CgVal of (alident* mutable_flag* virtual_flag* ctyp)
  | `CgVir of (alident* private_flag* ctyp) | ant] 
and class_expr =
  [ `CeApp of (class_expr* expr)
  | `ClassCon of (virtual_flag* ident* type_parameters)
  | `ClassConS of (virtual_flag* ident) | `CeFun of (patt* class_expr)
  | `LetIn of (rec_flag* binding* class_expr) | `Obj of class_str_item
  | `ObjEnd | `ObjPat of (patt* class_str_item) | `ObjPatEnd of patt
  | `Constraint of (class_expr* class_type)
  | `And of (class_expr* class_expr) | `Eq of (class_expr* class_expr) | 
    ant]
  
and class_str_item =
  [ `Sem of (class_str_item* class_str_item) | `Eq of (ctyp* ctyp)
  | `Inherit of (override_flag* class_expr)
  | `InheritAs of (override_flag* class_expr* alident) | `Initializer of expr
  | `CrMth of (alident* override_flag* private_flag* expr* ctyp)
  | `CrMthS of (alident* override_flag* private_flag* expr)
  | `CrVal of (alident* override_flag* mutable_flag* expr)
  | `CrVir of (alident* private_flag* ctyp)
  | `CrVvr of (alident* mutable_flag* ctyp) | ant] 
type ep =
  [ sid | `App of (ep* ep) | `Vrn of string | `Com of (ep* ep)
  | `Sem of (ep* ep) | `Tup of ep | any | `ArrayEmpty | `Array of ep
  | `Record of rec_bind | literal | ant] 
and rec_bind =
  [ `RecBind of (ident* ep) | `Sem of (rec_bind* rec_bind) | any | ant] 