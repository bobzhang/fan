type loc = FanLoc.t 
type ant = [ `Ant of (loc * FanUtil.anti_cxt)] 
type nil = [ `Nil of loc] 
type literal =
  [ `Chr of (loc * string) | `Int of (loc * string)
  | `Int32 of (loc * string) | `Int64 of (loc * string)
  | `Flo of (loc * string) | `Nativeint of (loc * string)
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
  [ `Dot of (loc * ident * ident) | `Apply of (loc * ident * ident) | 
    alident
  | auident] 
type ident' =
  [ `Dot of (loc * ident * ident) | `Apply of (loc * ident * ident)
  | `Lid of (loc * string) | `Uid of (loc * string)] 
type vid =
  [ `Dot of (loc * vid * vid) | `Lid of (loc * string)
  | `Uid of (loc * string) | ant] 
type vid' =
  [ `Dot of (loc * vid * vid) | `Lid of (loc * string)
  | `Uid of (loc * string)] 
type dupath = [ `Dot of (loc * dupath * dupath) | auident] 
type dlpath = [ `Dot of (loc * dupath * alident) | alident] 
type any = [ `Any of loc] 
type ctyp =
  [ `Alias of (loc * ctyp * alident) | any | `App of (loc * ctyp * ctyp)
  | `Arrow of (loc * ctyp * ctyp) | `ClassPath of (loc * ident)
  | `Label of (loc * alident * ctyp) | `OptLabl of (loc * alident * ctyp)
  | ident' | `TyObj of (loc * name_ctyp * row_var_flag)
  | `TyObjEnd of (loc * row_var_flag) | `TyPol of (loc * ctyp * ctyp)
  | `TyPolEnd of (loc * ctyp) | `TyTypePol of (loc * ctyp * ctyp)
  | `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag) | `Par of (loc * ctyp)
  | `Sta of (loc * ctyp * ctyp) | `PolyEq of (loc * row_field)
  | `PolySup of (loc * row_field) | `PolyInf of (loc * row_field)
  | `Com of (loc * ctyp * ctyp)
  | `PolyInfSup of (loc * row_field * tag_names) | `Package of (loc * mtyp)
  | ant] 
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
  [ `Sem of (loc * name_ctyp * name_ctyp) | `TyCol of (loc * alident * ctyp)
  | `TyColMut of (loc * alident * ctyp) | ant] 
and or_ctyp =
  [ `Bar of (loc * or_ctyp * or_ctyp) | `TyCol of (loc * auident * ctyp)
  | `Of of (loc * auident * ctyp) | auident] 
and of_ctyp = [ `Of of (loc * vid * ctyp) | vid' | ant] 
and pat =
  [ vid | `App of (loc * pat * pat) | `Vrn of (loc * string)
  | `Com of (loc * pat * pat) | `Sem of (loc * pat * pat)
  | `Par of (loc * pat) | any | `Record of (loc * rec_pat) | literal
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
  [ vid | `App of (loc * exp * exp) | `Vrn of (loc * string)
  | `Com of (loc * exp * exp) | `Sem of (loc * exp * exp)
  | `Par of (loc * exp) | any | `Record of (loc * rec_exp) | literal
  | `RecordWith of (loc * rec_exp * exp) | `Field of (loc * exp * exp)
  | `ArrayDot of (loc * exp * exp) | `ArrayEmpty of loc
  | `Array of (loc * exp) | `Assert of (loc * exp)
  | `Assign of (loc * exp * exp)
  | `For of (loc * alident * exp * exp * direction_flag * exp)
  | `Fun of (loc * case) | `IfThenElse of (loc * exp * exp * exp)
  | `IfThen of (loc * exp * exp) | `LabelS of (loc * alident)
  | `Label of (loc * alident * exp) | `Lazy of (loc * exp)
  | `LetIn of (loc * rec_flag * binding * exp)
  | `LetTryInWith of (loc * rec_flag * binding * exp * case)
  | `LetModule of (loc * auident * mexp * exp) | `Match of (loc * exp * case)
  | `New of (loc * ident) | `Obj of (loc * cstru) | `ObjEnd of loc
  | `ObjPat of (loc * pat * cstru) | `ObjPatEnd of (loc * pat)
  | `OptLabl of (loc * alident * exp) | `OptLablS of (loc * alident)
  | `OvrInst of (loc * rec_exp) | `OvrInstEmpty of loc | `Seq of (loc * exp)
  | `Send of (loc * exp * alident) | `StringDot of (loc * exp * exp)
  | `Try of (loc * exp * case) | `Constraint of (loc * exp * ctyp)
  | `Coercion of (loc * exp * ctyp * ctyp) | `Subtype of (loc * exp * ctyp)
  | `While of (loc * exp * exp) | `LetOpen of (loc * ident * exp)
  | `LocalTypeFun of (loc * alident * exp) | `Package_exp of (loc * mexp)] 
and rec_exp =
  [ `Sem of (loc * rec_exp * rec_exp) | `RecBind of (loc * ident * exp) | 
    any
  | ant] 
and mtyp =
  [ ident' | `Sig of (loc * sigi) | `SigEnd of loc
  | `Functor of (loc * auident * mtyp * mtyp)
  | `With of (loc * mtyp * constr) | `ModuleTypeOf of (loc * mexp) | 
    ant]
  
and sigi =
  [ `Val of (loc * alident * ctyp)
  | `External of (loc * alident * ctyp * strings) | `Type of (loc * typedecl)
  | `Exception of (loc * of_ctyp) | `Class of (loc * cltyp)
  | `ClassType of (loc * cltyp) | `Module of (loc * auident * mtyp)
  | `ModuleTypeEnd of (loc * auident) | `ModuleType of (loc * auident * mtyp)
  | `Sem of (loc * sigi * sigi) | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * exp) | `Open of (loc * ident)
  | `Include of (loc * mtyp) | `RecModule of (loc * mbind) | ant] 
and mbind =
  [ `And of (loc * mbind * mbind)
  | `ModuleBind of (loc * auident * mtyp * mexp)
  | `Constraint of (loc * auident * mtyp) | ant] 
and constr =
  [ `TypeEq of (loc * ctyp * ctyp) | `ModuleEq of (loc * ident * ident)
  | `TypeEqPriv of (loc * ctyp * ctyp) | `TypeSubst of (loc * ctyp * ctyp)
  | `ModuleSubst of (loc * ident * ident) | `And of (loc * constr * constr)
  | ant] 
and binding =
  [ `And of (loc * binding * binding) | `Bind of (loc * pat * exp) | ant] 
and case =
  [ `Bar of (loc * case * case) | `Case of (loc * pat * exp)
  | `CaseWhen of (loc * pat * exp * exp) | ant] 
and mexp =
  [ vid' | `App of (loc * mexp * mexp)
  | `Functor of (loc * auident * mtyp * mexp) | `Struct of (loc * stru)
  | `StructEnd of loc | `Constraint of (loc * mexp * mtyp)
  | `PackageModule of (loc * exp) | ant] 
and stru =
  [ `Class of (loc * cldecl) | `ClassType of (loc * cltyp)
  | `Sem of (loc * stru * stru) | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * exp) | `Exception of (loc * of_ctyp)
  | `StExp of (loc * exp) | `External of (loc * alident * ctyp * strings)
  | `Include of (loc * mexp) | `Module of (loc * auident * mexp)
  | `RecModule of (loc * mbind) | `ModuleType of (loc * auident * mtyp)
  | `Open of (loc * ident) | `Type of (loc * typedecl)
  | `Value of (loc * rec_flag * binding) | ant] 
and cltdecl = [ `And of (loc * cltdecl * cltdecl) | ant] 
and cltyp =
  [ `ClassCon of (loc * virtual_flag * ident * type_parameters)
  | `ClassConS of (loc * virtual_flag * ident)
  | `CtFun of (loc * ctyp * cltyp) | `ObjTy of (loc * ctyp * clsigi)
  | `ObjTyEnd of (loc * ctyp) | `Obj of (loc * clsigi) | `ObjEnd of loc
  | `And of (loc * cltyp * cltyp) | `CtCol of (loc * cltyp * cltyp)
  | `Eq of (loc * cltyp * cltyp) | ant] 
and clsigi =
  [ `Sem of (loc * clsigi * clsigi) | `SigInherit of (loc * cltyp)
  | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)
  | `Method of (loc * alident * private_flag * ctyp)
  | `VirMeth of (loc * alident * private_flag * ctyp)
  | `Eq of (loc * ctyp * ctyp) | ant] 
and cldecl =
  [ `ClDecl of (loc * virtual_flag * ident * type_parameters * clexp)
  | `ClDeclS of (loc * virtual_flag * ident * clexp)
  | `And of (loc * cldecl * cldecl) | ant] 
and clexp =
  [ `CeApp of (loc * clexp * exp) | vid'
  | `ClApply of (loc * vid * type_parameters) | `CeFun of (loc * pat * clexp)
  | `LetIn of (loc * rec_flag * binding * clexp) | `Obj of (loc * cstru)
  | `ObjEnd of loc | `ObjPat of (loc * pat * cstru)
  | `ObjPatEnd of (loc * pat) | `Constraint of (loc * clexp * cltyp) | 
    ant]
  
and cstru =
  [ `Sem of (loc * cstru * cstru) | `Eq of (loc * ctyp * ctyp)
  | `Inherit of (loc * override_flag * clexp)
  | `InheritAs of (loc * override_flag * clexp * alident)
  | `Initializer of (loc * exp)
  | `CrMth of (loc * alident * override_flag * private_flag * exp * ctyp)
  | `CrMthS of (loc * alident * override_flag * private_flag * exp)
  | `CrVal of (loc * alident * override_flag * mutable_flag * exp)
  | `VirMeth of (loc * alident * private_flag * ctyp)
  | `CrVvr of (loc * alident * mutable_flag * ctyp) | ant] 
type ep =
  [ vid | `App of (loc * ep * ep) | `Vrn of (loc * string)
  | `Com of (loc * ep * ep) | `Sem of (loc * ep * ep) | `Par of (loc * ep)
  | any | `ArrayEmpty of loc | `Array of (loc * ep)
  | `Record of (loc * rec_bind) | literal] 
and rec_bind =
  [ `RecBind of (loc * ident * ep) | `Sem of (loc * rec_bind * rec_bind)
  | any | ant] 