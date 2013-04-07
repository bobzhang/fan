type loc = FanLoc.t 
type ant = [ `Ant of (loc * FanUtil.anti_cxt)] 
type nil = [ `Nil of loc] 
type literal =
  [ `Chr of (loc * string) | `Int of (loc * string)
  | `Int32 of (loc * string) | `Int64 of (loc * string)
  | `Flo of (loc * string) | `NativeInt of (loc * string)
  | `Str of (loc * string)] 
type rec_flag = [ `Recursive of loc | `ReNil of loc | ant] 
type direction_flag = [ `To of loc | `Downto of loc | ant] 
type mutable_flag = [ `Mutable of loc | `MuNil of loc | ant] 
type private_flag = [ `Private of loc | `PrNil of loc | ant] 
type virtual_flag = [ `Virtual of loc | `ViNil of loc | ant] 
type override_flag = [ `Override of loc | `OvNil of loc | ant] 
type row_var_flag = [ `RowVar of loc | `RvNil of loc | ant] 
type position_flag =
  [ `Positive of loc | `Negative of loc | `Normal of loc | ant] 
type strings =
  [ `App of (loc * strings * strings) | `Str of (loc * string) | ant] 
type alident = [ `Lid of (loc * string) | ant] 
type auident = [ `Uid of (loc * string) | ant] 
type aident = [ alident | auident] 
type astring = [ `C of (loc * string) | ant] 
type uident =
  [ `Dot of (loc * uident * uident) | `App of (loc * uident * uident)
  | auident] 
type ident =
  [ `Dot of (loc * ident * ident) | `App of (loc * ident * ident) | alident
  | auident] 
type dupath = [ `Dot of (loc * dupath * dupath) | auident] 
type dlpath = [ `Dot of (loc * dupath * alident) | alident] 
type any = [ `Any of loc] 
type sid = [ `Id of (loc * ident)] 
type ctyp =
  [ `Alias of (loc * ctyp * alident) | any | `App of (loc * ctyp * ctyp)
  | `Arrow of (loc * ctyp * ctyp) | `ClassPath of (loc * ident)
  | `Label of (loc * alident * ctyp) | `OptLabl of (loc * alident * ctyp)
  | sid | `TyObj of (loc * name_ctyp * row_var_flag)
  | `TyObjEnd of (loc * row_var_flag) | `TyPol of (loc * ctyp * ctyp)
  | `TyPolEnd of (loc * ctyp) | `TyTypePol of (loc * ctyp * ctyp)
  | `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag) | `Par of (loc * ctyp)
  | `Sta of (loc * ctyp * ctyp) | `PolyEq of (loc * row_field)
  | `PolySup of (loc * row_field) | `PolyInf of (loc * row_field)
  | `Com of (loc * ctyp * ctyp)
  | `PolyInfSup of (loc * row_field * tag_names)
  | `Package of (loc * module_type) | ant] 
and type_parameters =
  [ `Com of (loc * type_parameters * type_parameters) | `Ctyp of (loc * ctyp)
  | ant] 
and row_field =
  [ ant | `Bar of (loc * row_field * row_field) | `TyVrn of (loc * astring)
  | `TyVrnOf of (loc * astring * ctyp) | `Ctyp of (loc * ctyp)] 
and tag_names =
  [ ant | `App of (loc * tag_names * tag_names) | `TyVrn of (loc * astring)] 
and typedecl =
  [ `TyDcl of (loc * alident * opt_decl_params * type_info * opt_type_constr)
  | `TyAbstr of (loc * alident * opt_decl_params * opt_type_constr)
  | `And of (loc * typedecl * typedecl) | ant] 
and type_constr =
  [ `And of (loc * type_constr * type_constr) | `Eq of (loc * ctyp * ctyp)
  | ant] 
and opt_type_constr = [ `Some of (loc * type_constr) | `None of loc] 
and decl_param =
  [ `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag) | `Any of loc | ant] 
and decl_params =
  [ `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag) | `Any of loc
  | `Com of (loc * decl_params * decl_params) | ant] 
and opt_decl_params = [ `Some of (loc * decl_params) | `None of loc] 
and type_info =
  [ `TyMan of (loc * ctyp * private_flag * type_repr)
  | `TyRepr of (loc * private_flag * type_repr)
  | `TyEq of (loc * private_flag * ctyp) | ant] 
and type_repr =
  [ `Record of (loc * name_ctyp) | `Sum of (loc * or_ctyp) | ant] 
and name_ctyp =
  [ `Sem of (loc * name_ctyp * name_ctyp) | `TyCol of (loc * sid * ctyp)
  | `TyColMut of (loc * sid * ctyp) | ant] 
and or_ctyp =
  [ `Bar of (loc * or_ctyp * or_ctyp) | `TyCol of (loc * sid * ctyp)
  | `Of of (loc * sid * ctyp) | sid | ant] 
and of_ctyp = [ `Of of (loc * sid * ctyp) | sid | ant] 
and pat =
  [ sid | `App of (loc * pat * pat) | `Vrn of (loc * string)
  | `Com of (loc * pat * pat) | `Sem of (loc * pat * pat)
  | `Par of (loc * pat) | any | `Record of (loc * rec_pat) | ant | literal
  | `Alias of (loc * pat * alident) | `ArrayEmpty of loc
  | `Array of (loc * pat) | `LabelS of (loc * alident)
  | `Label of (loc * alident * pat) | `OptLabl of (loc * alident * pat)
  | `OptLablS of (loc * alident)
  | `OptLablExpr of (loc * alident * pat * exp) | `Bar of (loc * pat * pat)
  | `PaRng of (loc * pat * pat) | `Constraint of (loc * pat * ctyp)
  | `ClassPath of (loc * ident) | `Lazy of (loc * pat)
  | `ModuleUnpack of (loc * auident)
  | `ModuleConstraint of (loc * auident * ctyp)] 
and rec_pat =
  [ `RecBind of (loc * ident * pat) | `Sem of (loc * rec_pat * rec_pat) | 
    any
  | ant] 
and exp =
  [ sid | `App of (loc * exp * exp) | `Vrn of (loc * string)
  | `Com of (loc * exp * exp) | `Sem of (loc * exp * exp)
  | `Par of (loc * exp) | any | `Record of (loc * rec_exp) | ant | literal
  | `RecordWith of (loc * rec_exp * exp) | `Dot of (loc * exp * exp)
  | `ArrayDot of (loc * exp * exp) | `ArrayEmpty of loc
  | `Array of (loc * exp) | `ExAsf of loc | `ExAsr of (loc * exp)
  | `Assign of (loc * exp * exp)
  | `For of (loc * alident * exp * exp * direction_flag * exp)
  | `Fun of (loc * case) | `IfThenElse of (loc * exp * exp * exp)
  | `IfThen of (loc * exp * exp) | `LabelS of (loc * alident)
  | `Label of (loc * alident * exp) | `Lazy of (loc * exp)
  | `LetIn of (loc * rec_flag * binding * exp)
  | `LetModule of (loc * auident * module_exp * exp)
  | `Match of (loc * exp * case) | `New of (loc * ident)
  | `Obj of (loc * cstru) | `ObjEnd of loc | `ObjPat of (loc * pat * cstru)
  | `ObjPatEnd of (loc * pat) | `OptLabl of (loc * alident * exp)
  | `OptLablS of (loc * alident) | `OvrInst of (loc * rec_exp)
  | `OvrInstEmpty of loc | `Seq of (loc * exp)
  | `Send of (loc * exp * alident) | `StringDot of (loc * exp * exp)
  | `Try of (loc * exp * case) | `Constraint of (loc * exp * ctyp)
  | `Coercion of (loc * exp * ctyp * ctyp) | `Subtype of (loc * exp * ctyp)
  | `While of (loc * exp * exp) | `LetOpen of (loc * ident * exp)
  | `LocalTypeFun of (loc * alident * exp)
  | `Package_exp of (loc * module_exp)] 
and rec_exp =
  [ `Sem of (loc * rec_exp * rec_exp) | `RecBind of (loc * ident * exp) | 
    any
  | ant] 
and module_type =
  [ sid | `Functor of (loc * auident * module_type * module_type)
  | `Sig of (loc * sig_item) | `SigEnd of loc
  | `With of (loc * module_type * with_constr)
  | `ModuleTypeOf of (loc * module_exp) | ant] 
and sig_item =
  [ `Class of (loc * class_type) | `ClassType of (loc * class_type)
  | `Sem of (loc * sig_item * sig_item) | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * exp) | `Exception of (loc * of_ctyp)
  | `External of (loc * alident * ctyp * strings)
  | `Include of (loc * module_type)
  | `Module of (loc * auident * module_type)
  | `RecModule of (loc * module_binding)
  | `ModuleType of (loc * auident * module_type)
  | `ModuleTypeEnd of (loc * auident) | `Open of (loc * ident)
  | `Type of (loc * typedecl) | `Val of (loc * alident * ctyp) | ant] 
and with_constr =
  [ `TypeEq of (loc * ctyp * ctyp) | `TypeEqPriv of (loc * ctyp * ctyp)
  | `ModuleEq of (loc * ident * ident) | `TypeSubst of (loc * ctyp * ctyp)
  | `ModuleSubst of (loc * ident * ident)
  | `And of (loc * with_constr * with_constr) | ant] 
and binding =
  [ `And of (loc * binding * binding) | `Bind of (loc * pat * exp) | ant] 
and module_binding =
  [ `And of (loc * module_binding * module_binding)
  | `ModuleBind of (loc * auident * module_type * module_exp)
  | `Constraint of (loc * auident * module_type) | ant] 
and case =
  [ `Bar of (loc * case * case) | `Case of (loc * pat * exp)
  | `CaseWhen of (loc * pat * exp * exp) | ant] 
and module_exp =
  [ sid | `App of (loc * module_exp * module_exp)
  | `Functor of (loc * auident * module_type * module_exp)
  | `Struct of (loc * stru) | `StructEnd of loc
  | `Constraint of (loc * module_exp * module_type)
  | `PackageModule of (loc * exp) | ant] 
and stru =
  [ `Class of (loc * class_exp) | `ClassType of (loc * class_type)
  | `Sem of (loc * stru * stru) | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * exp) | `Exception of (loc * of_ctyp)
  | `StExp of (loc * exp) | `External of (loc * alident * ctyp * strings)
  | `Include of (loc * module_exp) | `Module of (loc * auident * module_exp)
  | `RecModule of (loc * module_binding)
  | `ModuleType of (loc * auident * module_type) | `Open of (loc * ident)
  | `Type of (loc * typedecl) | `Value of (loc * rec_flag * binding) | 
    ant]
  
and class_type =
  [ `ClassCon of (loc * virtual_flag * ident * type_parameters)
  | `ClassConS of (loc * virtual_flag * ident)
  | `CtFun of (loc * ctyp * class_type)
  | `ObjTy of (loc * ctyp * class_sig_item) | `ObjTyEnd of (loc * ctyp)
  | `Obj of (loc * class_sig_item) | `ObjEnd of loc
  | `And of (loc * class_type * class_type)
  | `CtCol of (loc * class_type * class_type)
  | `Eq of (loc * class_type * class_type) | ant] 
and class_sig_item =
  [ `Eq of (loc * ctyp * ctyp)
  | `Sem of (loc * class_sig_item * class_sig_item)
  | `SigInherit of (loc * class_type)
  | `Method of (loc * alident * private_flag * ctyp)
  | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)
  | `CgVir of (loc * alident * private_flag * ctyp) | ant] 
and class_exp =
  [ `CeApp of (loc * class_exp * exp)
  | `ClassCon of (loc * virtual_flag * ident * type_parameters)
  | `ClassConS of (loc * virtual_flag * ident)
  | `CeFun of (loc * pat * class_exp)
  | `LetIn of (loc * rec_flag * binding * class_exp) | `Obj of (loc * cstru)
  | `ObjEnd of loc | `ObjPat of (loc * pat * cstru)
  | `ObjPatEnd of (loc * pat) | `Constraint of (loc * class_exp * class_type)
  | `And of (loc * class_exp * class_exp)
  | `Eq of (loc * class_exp * class_exp) | ant] 
and cstru =
  [ `Sem of (loc * cstru * cstru) | `Eq of (loc * ctyp * ctyp)
  | `Inherit of (loc * override_flag * class_exp)
  | `InheritAs of (loc * override_flag * class_exp * alident)
  | `Initializer of (loc * exp)
  | `CrMth of (loc * alident * override_flag * private_flag * exp * ctyp)
  | `CrMthS of (loc * alident * override_flag * private_flag * exp)
  | `CrVal of (loc * alident * override_flag * mutable_flag * exp)
  | `CrVir of (loc * alident * private_flag * ctyp)
  | `CrVvr of (loc * alident * mutable_flag * ctyp) | ant] 
type ep =
  [ sid | `App of (loc * ep * ep) | `Vrn of (loc * string)
  | `Com of (loc * ep * ep) | `Sem of (loc * ep * ep) | `Par of (loc * ep)
  | any | `ArrayEmpty of loc | `Array of (loc * ep)
  | `Record of (loc * rec_bind) | literal | ant] 
and rec_bind =
  [ `RecBind of (loc * ident * ep) | `Sem of (loc * rec_bind * rec_bind)
  | any | ant] 