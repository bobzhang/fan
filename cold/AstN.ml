let _ = (); ()

let _ = ()

type loc = FanLoc.t 

type ant = [ `Ant of (loc * FanUtil.anti_cxt)] 

type nil = [ `Nil] 

type literal =
  [ `Chr of string | `Int of string | `Int32 of string | `Int64 of string
  | `Flo of string | `Nativeint of string | `Str of string] 

type rec_flag = [ `Recursive | `ReNil | ant] 

type direction_flag = [ `To | `Downto | ant] 

type mutable_flag = [ `Mutable | `MuNil | ant] 

type private_flag = [ `Private | `PrNil | ant] 

type virtual_flag = [ `Virtual | `ViNil | ant] 

type override_flag = [ `Override | `OvNil | ant] 

type row_var_flag = [ `RowVar | `RvNil | ant] 

type position_flag = [ `Positive | `Negative | `Normal | ant] 

type strings = [ `App of (strings * strings) | `Str of string | ant] 

type alident = [ `Lid of string | ant] 

type auident = [ `Uid of string | ant] 

type aident = [ alident | auident] 

type astring = [ `C of string | ant] 

type uident =
  [ `Dot of (uident * uident) | `App of (uident * uident) | auident] 

type ident =
  [ `Dot of (ident * ident) | `App of (ident * ident) | alident | auident] 

type ident' =
  [ `Dot of (ident * ident) | `App of (ident * ident) | `Lid of string
  | `Uid of string] 

type vid = [ `Dot of (vid * vid) | `Lid of string | `Uid of string | ant] 

type vid' = [ `Dot of (vid * vid) | `Lid of string | `Uid of string] 

type dupath = [ `Dot of (dupath * dupath) | auident] 

type dlpath = [ `Dot of (dupath * alident) | alident] 

type any = [ `Any] 

type sid = [ `Id of ident] 

type ctyp =
  [ `Alias of (ctyp * alident) | any | `App of (ctyp * ctyp)
  | `Arrow of (ctyp * ctyp) | `ClassPath of ident
  | `Label of (alident * ctyp) | `OptLabl of (alident * ctyp) | sid
  | `TyObj of (name_ctyp * row_var_flag) | `TyObjEnd of row_var_flag
  | `TyPol of (ctyp * ctyp) | `TyPolEnd of ctyp | `TyTypePol of (ctyp * ctyp)
  | `Quote of (position_flag * alident) | `QuoteAny of position_flag
  | `Par of ctyp | `Sta of (ctyp * ctyp) | `PolyEq of row_field
  | `PolySup of row_field | `PolyInf of row_field | `Com of (ctyp * ctyp)
  | `PolyInfSup of (row_field * tag_names) | `Package of module_type | 
    ant]
  
and type_parameters =
  [ `Com of (type_parameters * type_parameters) | `Ctyp of ctyp | ant] 
and row_field =
  [ ant | `Bar of (row_field * row_field) | `TyVrn of astring
  | `TyVrnOf of (astring * ctyp) | `Ctyp of ctyp] 
and tag_names = [ ant | `App of (tag_names * tag_names) | `TyVrn of astring] 
and typedecl =
  [ `TyDcl of (alident * opt_decl_params * type_info * opt_type_constr)
  | `TyAbstr of (alident * opt_decl_params * opt_type_constr)
  | `And of (typedecl * typedecl) | ant] 
and type_constr =
  [ `And of (type_constr * type_constr) | `Eq of (ctyp * ctyp) | ant] 
and opt_type_constr = [ `Some of type_constr | `None] 
and decl_param =
  [ `Quote of (position_flag * alident) | `QuoteAny of position_flag | 
    `Any
  | ant] 
and decl_params =
  [ `Quote of (position_flag * alident) | `QuoteAny of position_flag | 
    `Any
  | `Com of (decl_params * decl_params) | ant] 
and opt_decl_params = [ `Some of decl_params | `None] 
and type_info =
  [ `TyMan of (ctyp * private_flag * type_repr)
  | `TyRepr of (private_flag * type_repr) | `TyEq of (private_flag * ctyp)
  | ant] 
and type_repr = [ `Record of name_ctyp | `Sum of or_ctyp | ant] 
and name_ctyp =
  [ `Sem of (name_ctyp * name_ctyp) | `TyCol of (alident * ctyp)
  | `TyColMut of (alident * ctyp) | ant] 
and or_ctyp =
  [ `Bar of (or_ctyp * or_ctyp) | `TyCol of (auident * ctyp)
  | `Of of (auident * ctyp) | auident] 
and of_ctyp = [ `Of of (vid * ctyp) | vid' | ant] 
and pat =
  [ vid | `App of (pat * pat) | `Vrn of string | `Com of (pat * pat)
  | `Sem of (pat * pat) | `Par of pat | any | `Record of rec_pat | literal
  | `Alias of (pat * alident) | `ArrayEmpty | `Array of pat
  | `LabelS of alident | `Label of (alident * pat)
  | `OptLabl of (alident * pat) | `OptLablS of alident
  | `OptLablExpr of (alident * pat * exp) | `Bar of (pat * pat)
  | `PaRng of (pat * pat) | `Constraint of (pat * ctyp) | `ClassPath of ident
  | `Lazy of pat | `ModuleUnpack of auident
  | `ModuleConstraint of (auident * ctyp)] 
and rec_pat =
  [ `RecBind of (ident * pat) | `Sem of (rec_pat * rec_pat) | any | ant] 
and exp =
  [ vid | `App of (exp * exp) | `Vrn of string | `Com of (exp * exp)
  | `Sem of (exp * exp) | `Par of exp | any | `Record of rec_exp | literal
  | `RecordWith of (rec_exp * exp) | `Field of (exp * exp)
  | `ArrayDot of (exp * exp) | `ArrayEmpty | `Array of exp | `Assert of exp
  | `Assign of (exp * exp)
  | `For of (alident * exp * exp * direction_flag * exp) | `Fun of case
  | `IfThenElse of (exp * exp * exp) | `IfThen of (exp * exp)
  | `LabelS of alident | `Label of (alident * exp) | `Lazy of exp
  | `LetIn of (rec_flag * binding * exp)
  | `LetTryInWith of (rec_flag * binding * exp * case)
  | `LetModule of (auident * module_exp * exp) | `Match of (exp * case)
  | `New of ident | `Obj of cstru | `ObjEnd | `ObjPat of (pat * cstru)
  | `ObjPatEnd of pat | `OptLabl of (alident * exp) | `OptLablS of alident
  | `OvrInst of rec_exp | `OvrInstEmpty | `Seq of exp
  | `Send of (exp * alident) | `StringDot of (exp * exp)
  | `Try of (exp * case) | `Constraint of (exp * ctyp)
  | `Coercion of (exp * ctyp * ctyp) | `Subtype of (exp * ctyp)
  | `While of (exp * exp) | `LetOpen of (ident * exp)
  | `LocalTypeFun of (alident * exp) | `Package_exp of module_exp] 
and rec_exp =
  [ `Sem of (rec_exp * rec_exp) | `RecBind of (ident * exp) | any | ant] 
and module_type =
  [ ident' | `Functor of (auident * module_type * module_type)
  | `Sig of sig_item | `SigEnd | `With of (module_type * with_constr)
  | `ModuleTypeOf of module_exp | ant] 
and sig_item =
  [ `Class of class_type | `ClassType of class_type
  | `Sem of (sig_item * sig_item) | `DirectiveSimple of alident
  | `Directive of (alident * exp) | `Exception of of_ctyp
  | `External of (alident * ctyp * strings) | `Include of module_type
  | `Module of (auident * module_type) | `RecModule of module_binding
  | `ModuleType of (auident * module_type) | `ModuleTypeEnd of auident
  | `Open of ident | `Type of typedecl | `Val of (alident * ctyp) | ant] 
and with_constr =
  [ `TypeEq of (ctyp * ctyp) | `TypeEqPriv of (ctyp * ctyp)
  | `ModuleEq of (ident * ident) | `TypeSubst of (ctyp * ctyp)
  | `ModuleSubst of (ident * ident) | `And of (with_constr * with_constr)
  | ant] 
and binding = [ `And of (binding * binding) | `Bind of (pat * exp) | ant] 
and module_binding =
  [ `And of (module_binding * module_binding)
  | `ModuleBind of (auident * module_type * module_exp)
  | `Constraint of (auident * module_type) | ant] 
and case =
  [ `Bar of (case * case) | `Case of (pat * exp)
  | `CaseWhen of (pat * exp * exp) | ant] 
and module_exp =
  [ vid' | `App of (module_exp * module_exp)
  | `Functor of (auident * module_type * module_exp) | `Struct of stru
  | `StructEnd | `Constraint of (module_exp * module_type)
  | `PackageModule of exp | ant] 
and stru =
  [ `Class of class_exp | `ClassType of class_type | `Sem of (stru * stru)
  | `DirectiveSimple of alident | `Directive of (alident * exp)
  | `Exception of of_ctyp | `StExp of exp
  | `External of (alident * ctyp * strings) | `Include of module_exp
  | `Module of (auident * module_exp) | `RecModule of module_binding
  | `ModuleType of (auident * module_type) | `Open of ident
  | `Type of typedecl | `Value of (rec_flag * binding) | ant] 
and class_type =
  [ `ClassCon of (virtual_flag * ident * type_parameters)
  | `ClassConS of (virtual_flag * ident) | `CtFun of (ctyp * class_type)
  | `ObjTy of (ctyp * class_sig_item) | `ObjTyEnd of ctyp
  | `Obj of class_sig_item | `ObjEnd | `And of (class_type * class_type)
  | `CtCol of (class_type * class_type) | `Eq of (class_type * class_type)
  | ant] 
and class_sig_item =
  [ `Eq of (ctyp * ctyp) | `Sem of (class_sig_item * class_sig_item)
  | `SigInherit of class_type | `Method of (alident * private_flag * ctyp)
  | `CgVal of (alident * mutable_flag * virtual_flag * ctyp)
  | `CgVir of (alident * private_flag * ctyp) | ant] 
and class_exp =
  [ `CeApp of (class_exp * exp)
  | `ClassCon of (virtual_flag * ident * type_parameters)
  | `ClassConS of (virtual_flag * ident) | `CeFun of (pat * class_exp)
  | `LetIn of (rec_flag * binding * class_exp) | `Obj of cstru | `ObjEnd
  | `ObjPat of (pat * cstru) | `ObjPatEnd of pat
  | `Constraint of (class_exp * class_type) | `And of (class_exp * class_exp)
  | `Eq of (class_exp * class_exp) | ant] 
and cstru =
  [ `Sem of (cstru * cstru) | `Eq of (ctyp * ctyp)
  | `Inherit of (override_flag * class_exp)
  | `InheritAs of (override_flag * class_exp * alident) | `Initializer of exp
  | `CrMth of (alident * override_flag * private_flag * exp * ctyp)
  | `CrMthS of (alident * override_flag * private_flag * exp)
  | `CrVal of (alident * override_flag * mutable_flag * exp)
  | `CrVir of (alident * private_flag * ctyp)
  | `CrVvr of (alident * mutable_flag * ctyp) | ant] 

type ep =
  [ vid | `App of (ep * ep) | `Vrn of string | `Com of (ep * ep)
  | `Sem of (ep * ep) | `Par of ep | any | `ArrayEmpty | `Array of ep
  | `Record of rec_bind | literal] 
and rec_bind =
  [ `RecBind of (ident * ep) | `Sem of (rec_bind * rec_bind) | any | ant] 