let _ = begin (); () end

type loc = FanLoc.t 

type ant = [ `Ant of (loc * FanUtil.anti_cxt)] 

type nil = [ `Nil] 

type literal =
  [ `Chr of string | `Int of string | `Int32 of string | `Int64 of string
  | `Flo of string | `Nativeint of string | `Str of string] 

type flag = [ `Positive | `Negative | ant] 

type position_flag = [ `Positive | `Negative | `Normal | ant] 

type strings = [ `App of (strings * strings) | `Str of string | ant] 

type lident = [ `Lid of string] 

type alident = [ `Lid of string | ant] 

type auident = [ `Uid of string | ant] 

type aident = [ alident | auident] 

type astring = [ `C of string | ant] 

type uident =
  [ `Dot of (uident * uident) | `App of (uident * uident) | auident] 

type ident =
  [ `Dot of (ident * ident) | `Apply of (ident * ident) | alident | auident] 

type ident' =
  [ `Dot of (ident * ident) | `Apply of (ident * ident) | `Lid of string
  | `Uid of string] 

type vid = [ `Dot of (vid * vid) | `Lid of string | `Uid of string | ant] 

type vid' = [ `Dot of (vid * vid) | `Lid of string | `Uid of string] 

type dupath = [ `Dot of (dupath * dupath) | auident] 

type dlpath = [ `Dot of (dupath * alident) | alident] 

type any = [ `Any] 

type ctyp =
  [ `Alias of (ctyp * alident) | any | `App of (ctyp * ctyp)
  | `Arrow of (ctyp * ctyp) | `ClassPath of ident
  | `Label of (alident * ctyp) | `OptLabl of (alident * ctyp) | ident'
  | `TyObj of (name_ctyp * flag) | `TyObjEnd of flag
  | `TyPol of (ctyp * ctyp) | `TyPolEnd of ctyp | `TyTypePol of (ctyp * ctyp)
  | `Quote of (position_flag * alident) | `QuoteAny of position_flag
  | `Par of ctyp | `Sta of (ctyp * ctyp) | `PolyEq of row_field
  | `PolySup of row_field | `PolyInf of row_field | `Com of (ctyp * ctyp)
  | `PolyInfSup of (row_field * tag_names) | `Package of mtyp | ant] 
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
  [ `TyMan of (ctyp * flag * type_repr) | `TyRepr of (flag * type_repr)
  | `TyEq of (flag * ctyp) | ant] 
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
  | `Assign of (exp * exp) | `For of (alident * exp * exp * flag * exp)
  | `Fun of case | `IfThenElse of (exp * exp * exp) | `IfThen of (exp * exp)
  | `LabelS of alident | `Label of (alident * exp) | `Lazy of exp
  | `LetIn of (flag * bind * exp)
  | `LetTryInWith of (flag * bind * exp * case)
  | `LetModule of (auident * mexp * exp) | `Match of (exp * case)
  | `New of ident | `Obj of clfield | `ObjEnd | `ObjPat of (pat * clfield)
  | `ObjPatEnd of pat | `OptLabl of (alident * exp) | `OptLablS of alident
  | `OvrInst of rec_exp | `OvrInstEmpty | `Seq of exp
  | `Send of (exp * alident) | `StringDot of (exp * exp)
  | `Try of (exp * case) | `Constraint of (exp * ctyp)
  | `Coercion of (exp * ctyp * ctyp) | `Subtype of (exp * ctyp)
  | `While of (exp * exp) | `LetOpen of (ident * exp)
  | `LocalTypeFun of (alident * exp) | `Package_exp of mexp] 
and rec_exp =
  [ `Sem of (rec_exp * rec_exp) | `RecBind of (ident * exp) | any | ant] 
and mtyp =
  [ ident' | `Sig of sigi | `SigEnd | `Functor of (auident * mtyp * mtyp)
  | `With of (mtyp * constr) | `ModuleTypeOf of mexp | ant] 
and sigi =
  [ `Val of (alident * ctyp) | `External of (alident * ctyp * strings)
  | `Type of typedecl | `Exception of of_ctyp | `Class of cltdecl
  | `ClassType of cltdecl | `Module of (auident * mtyp)
  | `ModuleTypeEnd of auident | `ModuleType of (auident * mtyp)
  | `Sem of (sigi * sigi) | `DirectiveSimple of alident
  | `Directive of (alident * exp) | `Open of ident | `Include of mtyp
  | `RecModule of mbind | ant] 
and mbind =
  [ `And of (mbind * mbind) | `ModuleBind of (auident * mtyp * mexp)
  | `Constraint of (auident * mtyp) | ant] 
and constr =
  [ `TypeEq of (ctyp * ctyp) | `ModuleEq of (ident * ident)
  | `TypeEqPriv of (ctyp * ctyp) | `TypeSubst of (ctyp * ctyp)
  | `ModuleSubst of (ident * ident) | `And of (constr * constr) | ant] 
and bind = [ `And of (bind * bind) | `Bind of (pat * exp) | ant] 
and case =
  [ `Bar of (case * case) | `Case of (pat * exp)
  | `CaseWhen of (pat * exp * exp) | ant] 
and mexp =
  [ vid' | `App of (mexp * mexp) | `Functor of (auident * mtyp * mexp)
  | `Struct of stru | `StructEnd | `Constraint of (mexp * mtyp)
  | `PackageModule of exp | ant] 
and stru =
  [ `Class of cldecl | `ClassType of cltdecl | `Sem of (stru * stru)
  | `DirectiveSimple of alident | `Directive of (alident * exp)
  | `Exception of of_ctyp | `StExp of exp
  | `External of (alident * ctyp * strings) | `Include of mexp
  | `Module of (auident * mexp) | `RecModule of mbind
  | `ModuleType of (auident * mtyp) | `Open of ident | `Type of typedecl
  | `TypeWith of (typedecl * strings) | `Value of (flag * bind) | ant] 
and cltdecl =
  [ `And of (cltdecl * cltdecl)
  | `CtDecl of (flag * ident * type_parameters * cltyp)
  | `CtDeclS of (flag * ident * cltyp) | ant] 
and cltyp =
  [ vid' | `ClApply of (vid * type_parameters) | `CtFun of (ctyp * cltyp)
  | `ObjTy of (ctyp * clsigi) | `ObjTyEnd of ctyp | `Obj of clsigi | 
    `ObjEnd
  | `And of (cltyp * cltyp) | ant] 
and clsigi =
  [ `Sem of (clsigi * clsigi) | `SigInherit of cltyp
  | `CgVal of (alident * flag * flag * ctyp)
  | `Method of (alident * flag * ctyp) | `VirMeth of (alident * flag * ctyp)
  | `Eq of (ctyp * ctyp) | ant] 
and cldecl =
  [ `ClDecl of (flag * ident * type_parameters * clexp)
  | `ClDeclS of (flag * ident * clexp) | `And of (cldecl * cldecl) | 
    ant]
  
and clexp =
  [ `CeApp of (clexp * exp) | vid' | `ClApply of (vid * type_parameters)
  | `CeFun of (pat * clexp) | `LetIn of (flag * bind * clexp)
  | `Obj of clfield | `ObjEnd | `ObjPat of (pat * clfield)
  | `ObjPatEnd of pat | `Constraint of (clexp * cltyp) | ant] 
and clfield =
  [ `Sem of (clfield * clfield) | `Inherit of (flag * clexp)
  | `InheritAs of (flag * clexp * alident)
  | `CrVal of (alident * flag * flag * exp)
  | `VirVal of (alident * flag * ctyp)
  | `CrMth of (alident * flag * flag * exp * ctyp)
  | `CrMthS of (alident * flag * flag * exp)
  | `VirMeth of (alident * flag * ctyp) | `Eq of (ctyp * ctyp)
  | `Initializer of exp | ant] 

type ep =
  [ vid | `App of (ep * ep) | `Vrn of string | `Com of (ep * ep)
  | `Sem of (ep * ep) | `Par of ep | any | `ArrayEmpty | `Array of ep
  | `Record of rec_bind | literal] 
and rec_bind =
  [ `RecBind of (ident * ep) | `Sem of (rec_bind * rec_bind) | any | ant] 