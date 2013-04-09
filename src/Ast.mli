(* Note: when you modify these types you must increment
   ast magic numbers defined in FanConfig.ml.
   Todo:
   add phantom type to track some type invariants?
 *)
(*

    It provides:
      - Types for all kinds of structure.
      - Map: A base class for map traversals.
      - Map classes and functions for common kinds.

    == Core language ==
    ctyp               :: Representaion of types
    pat               :: The type of patterns
    exp               :: The type of expressions
    case         :: The type of cases for match/function/try constructions
    ident              :: The type of identifiers (including path like Foo(X).Bar.y)
    binding            :: The type of let bindings
    rec_exp        :: The type of record definitions

    == Modules ==
    module_type        :: The type of module types
    sig_item           :: The type of signature items
    stru           :: The type of structure items
    module_exp        :: The type of module expressions
    module_binding     :: The type of recursive module definitions
    with_constr        :: The type of `with' constraints

    == Classes ==
    class_type         :: The type of class types
    class_sig_item     :: The type of class signature items
    class_exp         :: The type of class expressions
    cstru     :: The type of class structure items
 *)


type loc = FanLoc.t;
type ant = [= `Ant of (loc * FanUtil.anti_cxt)];
type nil = [= `Nil of loc];


type literal =
  [= `Chr of (loc * string)
  | `Int of (loc * string)
  | `Int32 of (loc * string)
  | `Int64 of (loc * string)
  | `Flo of (loc * string)
  | `Nativeint of (loc * string)
  | `Str of (loc * string)];   

type rec_flag =
  [= `Recursive of loc 
  | `ReNil of loc 
  | ant];

type direction_flag =
  [= `To of loc
  | `Downto of loc
  | ant ];

type mutable_flag =
  [= `Mutable of loc 
  | `MuNil of loc 
  | ant ];

type private_flag =
  [= `Private of loc 
  | `PrNil of loc 
  | ant ];

type virtual_flag =
  [= `Virtual of loc 
  | `ViNil of loc 
  | ant ];


type override_flag =
  [= `Override of loc 
  | `OvNil of loc 
  | ant ];

type row_var_flag =
  [= `RowVar of loc 
  | `RvNil of loc 
  | ant ];

type position_flag =
  [= `Positive of loc
  | `Negative of loc
  | `Normal of loc
  |ant];


type strings =
  [= `App of (loc * strings * strings)
  | `Str of (loc * string)
  | ant  ]  ;

type alident =
  [= `Lid of (loc * string)
  | ant];

type auident =
  [= `Uid of (loc * string)
  | ant];

type aident =
  [= alident
  | auident ];

type astring =
  [= `C of (loc * string)
  | ant ];

type uident =
  [= `Dot of (loc * uident * uident)
  | `App of (loc * uident * uident)
  | auident];

type ident =
  [= `Dot of (loc * ident * ident)
  | `App of (loc * ident * ident) 
  | alident
  | auident];

type dupath =
  [= `Dot of (loc * dupath * dupath)
  | auident];

type dlpath=
  [= `Dot of (loc * dupath * alident)
  | alident];


type any = [= `Any of loc];
(* type type_quote = *)
(*   [= `Quote of (loc * position_flag * alident) *)
(*   | `QuoteAny of (loc * position_flag) | any | ant ]; *)

type sid = [= `Id of (loc * ident)];


type ctyp =
  [= `Alias of (loc * ctyp * alident)
  | any
  | `App of (loc * ctyp * ctyp) (* t t *) (* list 'a *)
  | `Arrow of (loc * ctyp * ctyp)
  | `ClassPath of (loc * ident) (* #i *) (* #point *)
  | `Label of (loc * alident * ctyp) (* ~s:t *)
  | `OptLabl of (loc * alident * ctyp ) (* ?s:t *)
  | sid
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
  | `TyObj of (loc * name_ctyp * row_var_flag )
  | `TyObjEnd of (loc * row_var_flag)

  | `TyPol of (loc * ctyp * ctyp) (* ! t . t *) (* ! 'a . list 'a -> 'a *)
  | `TyPolEnd of (loc *ctyp) (* !. t *)  
  | `TyTypePol of (loc * ctyp * ctyp) (* type t . t *) (* type a . list a -> a *)

  (*  +'s -'s 's +_ -_ *)      
  | `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag )
  (* | type_quote *)
  | `Par of (loc * ctyp) (* ( t ) *) (* (int * string) *)
  | `Sta of (loc * ctyp * ctyp) (* t * t *)
  | `PolyEq of (loc * row_field)
  | `PolySup of (loc * row_field )
  | `PolyInf of (loc * row_field)
  | `Com of (loc * ctyp * ctyp)
  | `PolyInfSup of (loc * row_field * tag_names)
  | `Package of (loc * module_type) (* (module S) *)
  | ant ]
and type_parameters =
  [= `Com of (loc * type_parameters * type_parameters)
  | `Ctyp of (loc * ctyp)
  | ant]  
and row_field =
  [= ant
  | `Bar of (loc * row_field * row_field )
  | `TyVrn of (loc * astring)
  | `TyVrnOf of (loc * astring * ctyp)
  | `Ctyp of (loc * ctyp)]
and tag_names =
  [= ant
  | `App of (loc * tag_names * tag_names)
  | `TyVrn of (loc * astring )]   
and typedecl =
    (* {:stru| type  ('a, 'b, 'c) t = t |} *)
  [= `TyDcl of (loc * alident * opt_decl_params * type_info * opt_type_constr)
  | `TyAbstr of (loc * alident * opt_decl_params * opt_type_constr ) 
  | `And of (loc * typedecl * typedecl)
  | ant ]
(* original syntax
   {[ type v = u = A of int ]}
   revise syntax
   {[ type v = u = [A of int];]} *)
and type_constr =
  [= `And of (loc * type_constr * type_constr)
  | `Eq of (loc * ctyp * ctyp)
  | ant ]
and opt_type_constr =
 [= `Some of (loc * type_constr) (* changed to some and None later *)
 | `None of loc ]
and decl_param =
  [=  `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag )
  | `Any of loc | ant]
and decl_params =
 [= `Quote of (loc * position_flag * alident)
  | `QuoteAny of (loc * position_flag )
  | `Any of loc 
  | `Com of (loc  * decl_params * decl_params) | ant]
      
and opt_decl_params =
 [= `Some of (loc * decl_params)
 | `None of loc  ]   
and type_info =        (* FIXME be more preicse *)
  [= (* type u = v = [A of int ] *)
   `TyMan of (loc  * ctyp * private_flag  * type_repr)
     (* type u = A.t = {x:int} *)
  | `TyRepr of (loc * private_flag * type_repr)
  | `TyEq of (loc * private_flag * ctyp) (* type u = int *)
  | ant]  
and type_repr =
  [= `Record of (loc * name_ctyp)
  | `Sum of (loc * or_ctyp)
  | ant]
and name_ctyp =
  [= `Sem of (loc * name_ctyp * name_ctyp)
  | `TyCol of (loc * sid * ctyp )
  | `TyColMut of (loc * sid * ctyp)
  | ant]
and or_ctyp =
  [= `Bar of (loc * or_ctyp * or_ctyp )
  | `TyCol of (loc * sid * ctyp)
  | `Of of (loc * sid * ctyp)
  | sid
  | ant]
and of_ctyp =
  [= `Of of (loc * sid * ctyp)
  | sid
  | ant]
and pat =
  [= sid
  | `App of (loc * pat * pat)
  | `Vrn of (loc * string)
  | `Com of (loc * pat * pat)
  | `Sem of (loc * pat * pat)
  | `Par of (loc * pat )
  | any
  | `Record of (loc * rec_pat)
  | ant
  | literal
  | `Alias of (loc * pat * alident)
  | `ArrayEmpty of loc 
  | `Array of (loc * pat) (* [| p |] *)
  | `LabelS of (loc * alident) (* ~s *)
  | `Label of (loc * alident * pat) (* ~s or ~s:(p) *)
    (* ?s or ?s:(p)   *)
  | `OptLabl of (loc * alident * pat)
  | `OptLablS of (loc * alident)
    (* ?s:(p = e) or ?(p = e) *)
  | `OptLablExpr of (loc * alident * pat * exp)
  | `Bar of (loc * pat * pat) (* p | p *)
  | `PaRng (* `Range  *)of (loc * pat * pat) (* p .. p *)
  | `Constraint of (loc * pat * ctyp) (* (p : t) *)
  | `ClassPath of (loc * ident) (* #i *)
  | `Lazy of (loc * pat) (* lazy p *)
  | `ModuleUnpack of (loc * auident)
  | `ModuleConstraint of (loc * auident * ctyp) ]
and rec_pat =
  [= `RecBind of (loc * ident * pat)
  | `Sem of (loc  * rec_pat * rec_pat)
  | any
  | ant]  
and exp =
  [=  sid
  | `App of (loc * exp * exp)
  | `Vrn of (loc * string)
  | `Com of (loc * exp * exp)
  | `Sem of (loc * exp * exp)
  | `Par of (loc * exp)
  | any
  | `Record of (loc * rec_exp)
  | ant 
  | literal
      (* { (e) with rb }  *)
  | `RecordWith of (loc * rec_exp  * exp) (* FIXME give more restrict for the e *)         
  (* | `Dot of (loc * exp * exp) (\* e.e *\) *)
  | `Field of (loc * exp * exp)
  | `ArrayDot of (loc * exp * exp) (* e.(e) *)
  | `ArrayEmpty of loc 
  | `Array of (loc * exp) (* [| e |] *)
  (* | `ExAsf of loc (\* assert false *\) *)
  | `Assert of (loc * exp) (* assert e *)
  | `Assign of (loc * exp * exp) (* e := e *)
        (* for s = e to/downto e do { e } *)
  | `For of (loc * alident * exp * exp * direction_flag * exp)
  | `Fun of (loc * case) (* fun [ mc ] *)
  | `IfThenElse of (loc * exp * exp * exp) (* if e then e else e *)
  | `IfThen of (loc * exp * exp) (* if e then e *)
  | `LabelS of (loc * alident) (* ~s *)
  | `Label of (loc * alident * exp) (* ~s or ~s:e *)
  | `Lazy of (loc * exp) (* lazy e *)
        (* let b in e or let rec b in e *)
  | `LetIn of (loc * rec_flag * binding * exp)
  | `LetTryInWith of (loc * rec_flag * binding * exp * case)        
        (* let module s = me in e *)
  | `LetModule of (loc * auident * module_exp * exp)
        (* match e with [ mc ] *)
  | `Match of (loc * exp * case)
        (* new i *)
  | `New of (loc * ident)
        (* object ((p))? (cst)? end *)

  | `Obj of (loc * cstru)
  | `ObjEnd of loc 
  | `ObjPat of (loc * pat * cstru)
  | `ObjPatEnd of (loc * pat)
        (* ?s or ?s:e *)
  | `OptLabl of (loc *alident * exp)
  | `OptLablS of (loc * alident)
        (* {< rb >} *)
  | `OvrInst of (loc * rec_exp)
  | `OvrInstEmpty of loc
  | `Seq of (loc * exp) (* do { e } *)
  | `Send of (loc * exp * alident) (* e#s *)
  | `StringDot of (loc * exp * exp) (* e.[e] *)
  | `Try of (loc * exp * case) (* try e with [ mc ] *)

  | `Constraint of (loc * exp * ctyp) (*(e : t) *)
  | `Coercion of (loc * exp * ctyp * ctyp) (* or (e : t :> t) *)
  | `Subtype of (loc * exp * ctyp) (* (e :> t) *)
  | `While of (loc * exp * exp)
  | `LetOpen of (loc * ident * exp)
        (* fun (type t) -> e *)
        (* let f x (type t) y z = e *)
  | `LocalTypeFun of (loc *  alident * exp)
        (* (module ME : S) which is represented as (module (ME : S)) *)
  | `Package_exp of (loc * module_exp) ]
and rec_exp =
  [= `Sem of (loc * rec_exp * rec_exp)
  | `RecBind  of (loc * ident * exp)
  | any (* Faked here to be symmertric to rec_pat *)
  | ant ]
and module_type =
  [= sid
  | `Functor of (loc * auident * module_type * module_type)
  | `Sig of (loc * sig_item)
  | `SigEnd of loc 
  | `With of (loc * module_type * with_constr) (* mt with wc *)
  | `ModuleTypeOf of (loc * module_exp) (* module type of m *)
  | ant  ]
and sig_item =
  [= `Class of (loc * class_type)
  | `ClassType of (loc * class_type) (* class type cict *)
  | `Sem of (loc * sig_item * sig_item)
  | `DirectiveSimple of (loc * alident) (* # s or # s e *)
  | `Directive of (loc * alident * exp) (* semantics *)
        (* exception t *)
  | `Exception of (loc * of_ctyp)
        (* external s : t = s ... s *)
  | `External of (loc * alident  * ctyp * (* meta_list  *)strings)
  | `Include of (loc * module_type)
        (* module s : mt *)
  | `Module of (loc * auident * module_type)
        (* module rec mb *)
  | `RecModule of (loc * module_binding)
        (* module type s = mt *)
  | `ModuleType of (loc * auident * module_type)
  | `ModuleTypeEnd of (loc * auident)
  | `Open of (loc * ident)
  | `Type of (loc * typedecl)
  |  `Val of (loc * alident * ctyp)
  | ant  ]
          
and with_constr =
  [= `TypeEq of (loc * ctyp * ctyp)
  | `TypeEqPriv of (loc * ctyp * ctyp)
  | `ModuleEq of (loc * ident * ident)
  | `TypeSubst of (loc * ctyp * ctyp)
  | `ModuleSubst of (loc * ident * ident)
  | `And of (loc * with_constr * with_constr)
  | ant  ]
             
             (*
    let-binding	::=	pattern =  exp  
     value-name  { parameter }  [: typexp] =  exp  
    value-name : type  { typeconstr } .  typexp =  exp
    
   *)           
and binding =
  [=  `And of (loc * binding * binding)
  | `Bind  of (loc * pat * exp)
  | ant  ]
and module_binding =
  [= 
     (* module rec (s : mt) = me and (s : mt) = me *)
   `And of (loc * module_binding * module_binding)
      (* s : mt = me *)
  | `ModuleBind  of (loc *  auident * module_type * module_exp)
  | `Constraint  of (loc * auident * module_type) (* s : mt *)
  | ant ]
and case =
  [= `Bar of (loc * case * case)
  | `Case of (loc * pat * exp)
  | `CaseWhen of (loc * pat * exp * exp)
  | ant  ]
and module_exp =
  [= sid
  | `App of (loc * module_exp * module_exp) (* me me *)
  | `Functor of (loc * auident * module_type * module_exp)
  | `Struct of (loc * stru)
  | `StructEnd of loc 
  | `Constraint of (loc * module_exp * module_type) (* (me : mt) *)
        (* (value e) *)
        (* (value e : S) which is represented as (value (e : S)) *)
  | `PackageModule of (loc * exp)
  | ant  ]
and stru =
  [= `Class of (loc * class_exp) (* class cice *)
  | `ClassType of (loc * class_type) (* class type cict *)
  | `Sem of (loc * stru * stru)
  | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * exp)
        (* exception t or exception t = i *)
        (* | `Exception of ( loc * ctyp * meta_option(\*FIXME*\) ident) *)
  | `Exception of ( loc * of_ctyp)
        (* TODO ExceptionRebind
           http://caml.inria.fr/pub/docs/manual-ocaml/manual016.html
         *)     
  | `StExp of (loc * exp)
  | `External of (loc * alident  * ctyp *  strings)
  | `Include of (loc * module_exp)
  | `Module of (loc * auident * module_exp)
  | `RecModule of (loc * module_binding)
  | `ModuleType of (loc * auident * module_type) (* module type s = mt *)
  | `Open of (loc * ident) (* open i *)
  | `Type of (loc * typedecl) (* type t *)
  | `Value of (loc * rec_flag * binding) (* value (rec)? bi *)
  | ant  ]
and class_type = (* class body type *)         
  [= `ClassCon of
    (loc * virtual_flag * ident *  type_parameters) (* (virtual)? i [ t ] *)
  | `ClassConS of (loc * virtual_flag * ident) (* (virtual)? i *)
  | `CtFun of (loc * ctyp * class_type) (* [t] -> ct *)
  | `ObjTy of (loc * ctyp * class_sig_item) (*object (ty) ..  end*)
  | `ObjTyEnd of (loc * ctyp) (*object (ty) end*)
  | `Obj of (loc * class_sig_item) (* object ... end *)
  | `ObjEnd of (loc) (* object end*)
  | `And of (loc * class_type * class_type)
  | `CtCol of (loc * class_type * class_type) (* ct : ct *)
  | `Eq  of (loc * class_type * class_type) (* ct = ct *)
  | ant ]
and class_sig_item =
  [= `Eq of (loc * ctyp * ctyp)
  | `Sem of (loc * class_sig_item * class_sig_item)
  | `SigInherit of (loc * class_type)
      (* method s : t or method private s : t *)
  | `Method of (loc * alident * private_flag * ctyp)
      (* val (virtual)? (mutable)? s : t *)
  | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)
      (* method virtual (private)? s : t *)
  | `CgVir of (loc *  alident * private_flag * ctyp)
  | ant ]
and class_exp =
  [= `CeApp of (loc * class_exp * exp)   (* ce e *)
  | `ClassCon of (loc * virtual_flag * ident * type_parameters)(* virtual v [t]*)
  | `ClassConS of (loc * virtual_flag * ident) (* virtual v *)
  | `CeFun of (loc * pat * class_exp) (* fun p -> ce *)
  | `LetIn of (loc * rec_flag * binding * class_exp) (* let (rec)? bi in ce *)
  | `Obj of (loc  * cstru) (* object ((p))? (cst)? end *)
  | `ObjEnd of loc (*object end*)
  | `ObjPat of (loc * pat * cstru)(*object (p) .. end*)
  | `ObjPatEnd of (loc * pat) (* object (p) end*)
  | `Constraint of (loc * class_exp * class_type) (* ce : ct *)
  | `And of (loc * class_exp * class_exp)
  | `Eq  of (loc * class_exp * class_exp)
  | ant ]
and cstru =
  [=  `Sem of (loc * cstru * cstru)
  | `Eq of (loc * ctyp * ctyp)
  | `Inherit of (loc * override_flag * class_exp)
  | `InheritAs of (loc * override_flag * class_exp * alident)
  | `Initializer of (loc * exp)
        (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
  | `CrMth of (loc * alident * override_flag * private_flag * exp * ctyp)
  | `CrMthS of (loc * alident * override_flag * private_flag * exp )
        (* value(!)? (mutable)? s = e *)
  | `CrVal of (loc *  alident * override_flag * mutable_flag * exp)
        (* method virtual (private)? s : t *)
  | `CrVir of (loc * alident * private_flag * ctyp)
        (* val virtual (mutable)? s : t *)
  | `CrVvr of (loc * alident * mutable_flag * ctyp)
  | ant  ]; 
(* Any is necessary, since sometimes you want to [meta_loc_pat] to [_]
   Faked here to make a common subtyp of exp pat to be expnessive enough *)
type ep =
  [= sid
  | `App of (loc * ep * ep)
  | `Vrn of (loc * string)
  | `Com of (loc * ep * ep)
  | `Sem of (loc * ep * ep)
  | `Par of (loc * ep)
  | any
  | `ArrayEmpty of loc 
  | `Array of (loc * ep )
  | `Record of (loc * rec_bind)
  | literal
  | ant ]
and rec_bind =
  [=  `RecBind of (loc * ident * ep)
  | `Sem of (loc * rec_bind * rec_bind)
  | any
  | ant];
      
      
(* let _loc = FanLoc.ghost; *)
(* #filter "serialize";; *)
