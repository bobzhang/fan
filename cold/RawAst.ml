type nil = [ `Nil] 

type literal =
  [ `Chr of string | `Int of string | `Int32 of string | `Int64 of string
  | `Flo of string | `Nativeint of string | `Str of string] 

type rec_flag = [ `Recursive | `ReNil] 

type direction_flag = [ `To | `Downto] 

type mutable_flag = [ `Mutable | `MuNil] 

type private_flag = [ `Private | `PrNil] 

type virtual_flag = [ `Virtual | `ViNil] 

type override_flag = [ `Override | `OvNil] 

type row_var_flag = [ `RowVar | `RvNil] 

type position_flag = [ `Positive | `Negative | `Normal] 

type meta_bool = [ `True | `False] 

type 'a meta_option = [ `None | `Some of 'a | ant] 

type 'a meta_list = [ `LNil | `LCons of ('a * 'a meta_list) | ant] 

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

type sid = [ `Id of (loc * ident)] 

type any = [ `Any of loc] 

type ctyp =
  [ nil | `Alias of (loc * ctyp * alident) | any
  | `App of (loc * ctyp * ctyp) | `Arrow of (loc * ctyp * ctyp)
  | `ClassPath of (loc * ident) | `Label of (loc * alident * ctyp)
  | `OptLabl of (loc * alident * ctyp) | sid
  | `TyObj of (loc * name_ctyp * row_var_flag)
  | `TyPol of (loc * ctyp * ctyp) | `TyTypePol of (loc * ctyp * ctyp)
  | `Quote of (loc * position_flag * alident meta_option)
  | `Par of (loc * ctyp) | `Sta of (loc * ctyp * ctyp)
  | `PolyEq of (loc * row_field) | `PolySup of (loc * row_field)
  | `PolyInf of (loc * row_field)
  | `PolyInfSup of (loc * row_field * tag_names) | `Package of (loc * mtyp)
  | ant] 
and type_parameters =
  [ `Com of (loc * type_parameters * type_parameters) | `Ctyp of (loc * ctyp)
  | ant | nil] 
and row_field =
  [ ant_nil | `Bar of (loc * row_field * row_field)
  | `TyVrn of (loc * astring) | `TyVrnOf of (loc * astring * ctyp)
  | `Ctyp of (loc * ctyp)] 
and tag_names =
  [ ant_nil | `App of (loc * tag_names * tag_names)
  | `TyVrn of (loc * astring)] 
and typedecl =
  [ `TyDcl of (loc * alident * ctyp list * type_info * (ctyp * ctyp) list)
  | `And of (loc * typedecl * typedecl) | ant_nil] 
and type_info =
  [ `TyMan of (loc * ctyp * private_flag * type_repr)
  | `TyRepr of (loc * private_flag * type_repr)
  | `TyEq of (loc * private_flag * ctyp) | ant | nil] 
and type_repr =
  [ `Record of (loc * name_ctyp) | `Sum of (loc * or_ctyp) | ant | nil] 
and name_ctyp =
  [ `Sem of (loc * name_ctyp * name_ctyp) | `TyCol of (loc * sid * ctyp)
  | `TyColMut of (loc * sid * ctyp) | ant | nil] 
and or_ctyp =
  [ `Bar of (loc * or_ctyp * or_ctyp) | `TyCol of (loc * sid * ctyp)
  | `Of of (loc * sid * ctyp) | sid | ant_nil] 
and of_ctyp = [ `Of of (loc * sid * ctyp) | sid | ant | nil] 
and pat =
  [ nil | sid | `App of (loc * pat * pat) | `Vrn of (loc * string)
  | `Com of (loc * pat * pat) | `Sem of (loc * pat * pat)
  | `Par of (loc * pat) | any | `Record of (loc * rec_pat) | ant | literal
  | `Alias of (loc * pat * alident) | `Array of (loc * pat)
  | `Label of (loc * alident * pat)
  | `PaOlbi of (loc * alident * pat * exp meta_option)
  | `Bar of (loc * pat * pat) | `PaRng of (loc * pat * pat)
  | `Constraint of (loc * pat * ctyp) | `ClassPath of (loc * ident)
  | `Lazy of (loc * pat)
  | `ModuleUnpack of (loc * auident * ctyp meta_option)] 
and rec_pat =
  [ nil | `RecBind of (loc * ident * pat) | `Sem of (loc * rec_pat * rec_pat)
  | any | ant] 
and exp =
  [ nil | sid | `App of (loc * exp * exp) | `Vrn of (loc * string)
  | `Com of (loc * exp * exp) | `Sem of (loc * exp * exp)
  | `Par of (loc * exp) | any | `Record of (loc * rec_exp) | ant | literal
  | `RecordWith of (loc * rec_exp * exp) | `Dot of (loc * exp * exp)
  | `ArrayDot of (loc * exp * exp) | `Array of (loc * exp)
  | `Assert of (loc * exp) | `Assign of (loc * exp * exp)
  | `For of (loc * alident * exp * exp * direction_flag * exp)
  | `Fun of (loc * case) | `IfThenElse of (loc * exp * exp * exp)
  | `IfThen of (loc * exp * exp) | `Label of (loc * alident * exp)
  | `Lazy of (loc * exp) | `LetIn of (loc * rec_flag * binding * exp)
  | `LetModule of (loc * auident * module_exp * exp)
  | `Match of (loc * exp * case) | `New of (loc * ident)
  | `Obj of (loc * pat * cstru) | `OptLabl of (loc * alident * exp)
  | `OvrInst of (loc * rec_exp) | `Seq of (loc * exp)
  | `Send of (loc * exp * alident) | `StringDot of (loc * exp * exp)
  | `Try of (loc * exp * case) | `Constraint of (loc * exp * ctyp)
  | `Coercion of (loc * exp * ctyp * ctyp) | `While of (loc * exp * exp)
  | `LetOpen of (loc * ident * exp) | `LocalTypeFun of (loc * alident * exp)
  | `Package_exp of (loc * module_exp)] 
and rec_exp =
  [ nil | `Sem of (loc * rec_exp * rec_exp) | `RecBind of (loc * ident * exp)
  | any | ant] 
and mtyp =
  [ nil | sid | `MtFun of (loc * auident * mtyp * mtyp)
  | `Sig of (loc * sig_item) | `With of (loc * mtyp * with_constr)
  | `ModuleTypeOf of (loc * module_exp) | ant] 
and sig_item =
  [ nil | `Class of (loc * class_type) | `ClassType of (loc * class_type)
  | `Sem of (loc * sig_item * sig_item) | `Directive of (loc * alident * exp)
  | `Exception of (loc * of_ctyp)
  | `External of (loc * alident * ctyp * string meta_list)
  | `Include of (loc * mtyp) | `Module of (loc * auident * mtyp)
  | `RecModule of (loc * module_binding)
  | `ModuleType of (loc * auident * mtyp) | `Open of (loc * ident)
  | `Type of (loc * typedecl) | `Val of (loc * alident * ctyp) | ant] 
and with_constr =
  [ nil | `TypeEq of (loc * ctyp * ctyp) | `TypeEqPriv of (loc * ctyp * ctyp)
  | `ModuleEq of (loc * ident * ident) | `TypeSubst of (loc * ctyp * ctyp)
  | `ModuleSubst of (loc * ident * ident)
  | `And of (loc * with_constr * with_constr) | ant] 
and binding =
  [ nil | `And of (loc * binding * binding) | `Bind of (loc * pat * exp)
  | ant] 
and module_binding =
  [ nil | `And of (loc * module_binding * module_binding)
  | `ModuleBind of (loc * auident * mtyp * module_exp)
  | `Constraint of (loc * auident * mtyp) | ant] 
and case =
  [ nil | `Bar of (loc * case * case) | `Case of (loc * pat * exp * exp)
  | ant] 
and module_exp =
  [ nil | sid | `App of (loc * module_exp * module_exp)
  | `Functor of (loc * auident * mtyp * module_exp) | `Struct of (loc * stru)
  | `Constraint of (loc * module_exp * mtyp) | `PackageModule of (loc * exp)
  | ant] 
and stru =
  [ nil | `Class of (loc * class_exp) | `ClassType of (loc * class_type)
  | `Sem of (loc * stru * stru) | `Directive of (loc * alident * exp)
  | `Exception of (loc * of_ctyp) | `StExp of (loc * exp)
  | `External of (loc * alident * ctyp * string meta_list)
  | `Include of (loc * module_exp) | `Module of (loc * auident * module_exp)
  | `RecModule of (loc * module_binding)
  | `ModuleType of (loc * auident * mtyp) | `Open of (loc * ident)
  | `Type of (loc * typedecl) | `Value of (loc * rec_flag * binding) | 
    ant]
  
and class_type =
  [ nil | `CtCon of (loc * virtual_flag * ident * type_parameters)
  | `CtFun of (loc * ctyp * class_type)
  | `CtSig of (loc * ctyp * class_sig_item)
  | `And of (loc * class_type * class_type)
  | `CtCol of (loc * class_type * class_type)
  | `CtEq of (loc * class_type * class_type) | ant] 
and class_sig_item =
  [ nil | `Eq of (loc * ctyp * ctyp)
  | `Sem of (loc * class_sig_item * class_sig_item)
  | `SigInherit of (loc * class_type)
  | `Method of (loc * alident * private_flag * ctyp)
  | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)
  | `CgVir of (loc * alident * private_flag * ctyp) | ant] 
and class_exp =
  [ nil | `CeApp of (loc * class_exp * exp)
  | `CeCon of (loc * virtual_flag * ident * type_parameters)
  | `CeFun of (loc * pat * class_exp)
  | `CeLet of (loc * rec_flag * binding * class_exp)
  | `Obj of (loc * pat * cstru) | `CeTyc of (loc * class_exp * class_type)
  | `And of (loc * class_exp * class_exp)
  | `Eq of (loc * class_exp * class_exp) | ant] 
and cstru =
  [ nil | `Sem of (loc * cstru * cstru) | `Eq of (loc * ctyp * ctyp)
  | `Inherit of (loc * override_flag * class_exp * alident meta_option)
  | `Initializer of (loc * exp)
  | `CrMth of (loc * alident * override_flag * private_flag * exp * ctyp)
  | `CrVal of (loc * alident * override_flag * mutable_flag * exp)
  | `CrVir of (loc * alident * private_flag * ctyp)
  | `CrVvr of (loc * alident * mutable_flag * ctyp) | ant] 

type ep =
  [ nil | sid | `App of (loc * ep * ep) | `Vrn of (loc * string)
  | `Com of (loc * ep * ep) | `Sem of (loc * ep * ep) | `Par of (loc * ep)
  | any | `Array of (loc * ep) | `Record of (loc * rec_bind) | literal | 
    ant]
  
and rec_bind =
  [ nil | `RecBind of (loc * ident * ep)
  | `Sem of (loc * rec_bind * rec_bind) | any | ant] 