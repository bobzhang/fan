(* Note: when you modify these types you must increment
   ast magic numbers defined in FanConfig.ml.
   Todo:
   add phantom type to track some type invariants?
 *)
(** Signature for OCaml syntax trees. *) (*

    It provides:
      - Types for all kinds of structure.
      - Map: A base class for map traversals.
      - Map classes and functions for common kinds.

    == Core language ==
    ctyp               :: Representaion of types
    patt               :: The type of patterns
    expr               :: The type of expressions
    match_case         :: The type of cases for match/function/try constructions
    ident              :: The type of identifiers (including path like Foo(X).Bar.y)
    binding            :: The type of let bindings
    rec_expr        :: The type of record definitions

    == Modules ==
    module_type        :: The type of module types
    sig_item           :: The type of signature items
    str_item           :: The type of structure items
    module_expr        :: The type of module expressions
    module_binding     :: The type of recursive module definitions
    with_constr        :: The type of `with' constraints

    == Classes ==
    class_type         :: The type of class types
    class_sig_item     :: The type of class signature items
    class_expr         :: The type of class expressions
    class_str_item     :: The type of class structure items
 *)


type loc = FanLoc.t;
type ant = [= `Ant of (loc * FanUtil.anti_cxt)];
type nil = [= `Nil of loc];
type ant_nil = [= ant|nil];

type literal =
  [= `Chr of (loc * string)
  | `Int of (loc * string)
  | `Int32 of (loc * string)
  | `Int64 of (loc * string)
  | `Flo of (loc * string)
  | `NativeInt of (loc * string)
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

type meta_bool =
  [=`True of loc
  |`False of loc
  | ant];


type strings =
  [= `App of (loc * strings * strings)
  | `Str of (loc * string)
  | ant  ]  ;

(* type 'a mlist= *)
(*   [= `Concat of (loc * 'a mlist * 'a mlist) *)
(*   | 'a *)
(*   | ant]; *)

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


(* type uident = *)
(*  [= `Dot of (loc * uident * uident) *)
(*  | `App of (loc * uident * uident) *)
(*  ];    *)
(* {:ident|A.B.C.d|}
      `Dot
      (`Uid "A"
        `Dot (`Uid "B")
           `Dot("C",`Lid "d"))
    *)
type ident =
  [= `Dot of (loc * ident * ident) (* i . i *)
  | `App of (loc * ident * ident) (* i i *)
  | alident
  | auident];

(* type dopath = *)
(*  [= `Dot of (loc * dopath * dopath) *)
(*  | auident ] ; *)

(* A.B.C *)
type dupath =
  [= `Dot of (loc * dupath * dupath)
  | auident];

type dlpath=
  [= `Dot of (loc * dupath * alident)
  | alident];



type sid = [= `Id of (loc * ident)];
type any = [= `Any of loc];

type ctyp =
  [= nil
  | `Alias of (loc * ctyp * alident)
  | any
  | `App of (loc * ctyp * ctyp) (* t t *) (* list 'a *)
  | `Arrow of (loc * ctyp * ctyp)
  | `ClassPath of (loc * ident) (* #i *) (* #point *)
  | `Label of (loc * alident * ctyp) (* ~s:t *)
        (* ?s:t *)
  | `OptLabl of (loc * alident * ctyp )

  | sid
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
  | `TyObj of (loc * name_ctyp * row_var_flag )
  | `TyPol of (loc * ctyp * ctyp) (* ! t . t *) (* ! 'a . list 'a -> 'a *)
        
  | `TyTypePol of (loc * ctyp * ctyp) (* type t . t *) (* type a . list a -> a *)

  (*  +'s -'s 's +_ -_ *)      


  | `Quote of (loc * position_flag * alident) 
  | `QuoteAny of (loc * position_flag )
  | `Tup of (loc * ctyp) (* ( t ) *) (* (int * string) *)
  | `Sta of (loc * ctyp * ctyp) (* t * t *)
  | `PolyEq of (loc * row_field)
  | `PolySup of (loc * row_field )
  | `PolyInf of (loc * row_field)
  | `PolyInfSup of (loc * row_field * tag_names)
        
  | `Package of (loc * module_type) (* (module S) *)
  | ant ]
and type_parameters =
  [= `Com of (loc * type_parameters * type_parameters)
  | `Ctyp of (loc * ctyp)
  | ant
  | nil]  
and row_field =
  [= ant_nil
  | `Or of (loc * row_field * row_field )
  | `TyVrn of (loc * astring)
  | `TyVrnOf of (loc * astring * ctyp)
  |  `Ctyp of (loc * ctyp)]
and tag_names =
  [= ant_nil
  | `App of (loc * tag_names * tag_names)
  | `TyVrn of (loc * astring )]   
and typedecl =
    (* {:str_item| type  ('a, 'b, 'c) t = t |} *)
  [= `TyDcl of (loc * alident * list ctyp * type_info * list (ctyp * ctyp))
  | `And of (loc * typedecl * typedecl)
  | ant_nil ]
      (* original syntax
         {[ type v = u = A of int ]}
       revise syntax
       {[ type v = u = [A of int];]} 
     *)
and type_info =        (* FIXME be more preicse *)
  [=
   (* type u = v = [A of int ] *)
   `TyMan of (loc  * ctyp * private_flag  * type_repr)
   (* type u = A.t = {x:int} *)
  | `TyRepr of (loc * private_flag * type_repr)

   (* type u = int *)
  | `TyEq of (loc * private_flag * ctyp)
  | ant
  | nil]  
and type_repr =
  [= `Record of (loc * name_ctyp)
  | `Sum of (loc * or_ctyp)
  | ant
  | nil ]   
and name_ctyp =
  [= `Sem of (loc * name_ctyp * name_ctyp)
  | `TyCol of (loc * sid * ctyp )
  | `TyColMut of (loc * sid * ctyp)
  | ant
  | nil ]
and or_ctyp =
  [= `Or of (loc * or_ctyp * or_ctyp )
  | `TyCol of (loc * sid * ctyp)
  | `Of of (loc * (* ctyp *)sid * ctyp)
  | sid
  | ant_nil]
and of_ctyp =
  [= `Of of (loc * sid * ctyp)
  | sid
  | ant
  | nil]
         
and patt =
  [=(*  nil *)
  (* | *) sid
  | `App of (loc * patt * patt)
  | `Vrn of (loc * string)
  | `Com of (loc * patt * patt)
  | `Sem of (loc * patt * patt)
  | `Tup of (loc * patt )
  | any
  | `Record of (loc * rec_patt)
  | ant
  | literal
      
  | `Alias of (loc * patt * alident)  (* (Node x y as n) *)

  | `ArrayEmpty of loc 
  | `Array of (loc * patt) (* [| p |] *)
  | `LabelS of (loc * alident) (* ~s *)
  | `Label of (loc * alident * patt) (* ~s or ~s:(p) *)

    (* ?s or ?s:(p)   *)
  | `OptLabl of (loc * alident * patt)
  | `OptLablS of (loc * alident)
        
    (* ?s:(p = e) or ?(p = e) *)
  | `OptLablExpr of (loc * alident * patt * expr)
        
  | `Or of (loc * patt * patt) (* p | p *)
  | `PaRng (* `Range  *)of (loc * patt * patt) (* p .. p *)
  | `Constraint of (loc * patt * ctyp) (* (p : t) *)
  | `ClassPath of (loc * ident) (* #i *)
  | `Lazy of (loc * patt) (* lazy p *)
    (* (module M ) *)   
  | `ModuleUnpack of (loc * auident)
    (* (module M : ty ) *)      
  | `ModuleConstraint of (loc * auident * ctyp) ]
and rec_patt =
  [= `RecBind of (loc * ident * patt)
  | `Sem of (loc  * rec_patt * rec_patt)
  | any
  | ant]  
and expr =
  [= nil
  | sid
  | `App of (loc * expr * expr)
  | `Vrn of (loc * string)
  | `Com of (loc * expr * expr)
  | `Sem of (loc * expr * expr)
  | `Tup of (loc * expr)
  | any
  | `Record of (loc * rec_expr)
  | ant 
  | literal
      (* { (e) with rb }  *)
  | `RecordWith of (loc * rec_expr  * expr)         
  | `Dot of (loc * expr * expr) (* e.e *)
  | `ArrayDot of (loc * expr * expr) (* e.(e) *)
  | `ArrayEmpty of loc 
  | `Array of (loc * expr) (* [| e |] *)
  | `ExAsf of loc (* assert `False *)
  | `ExAsr of (loc * expr) (* assert e *)
  | `Assign of (loc * expr * expr) (* e := e *)
        (* for s = e to/downto e do { e } *)
  | `For of (loc * alident * expr * expr * direction_flag * expr)
  | `Fun of (loc * match_case) (* fun [ mc ] *)
  | `IfThenElse of (loc * expr * expr * expr) (* if e then e else e *)
  | `IfThen of (loc * expr * expr) (* if e then e *)
  | `LabelS of (loc * alident) (* ~s *)
  | `Label of (loc * alident * expr) (* ~s or ~s:e *)
  | `Lazy of (loc * expr) (* lazy e *)
        (* let b in e or let rec b in e *)
  | `LetIn of (loc * rec_flag * binding * expr)
        (* let module s = me in e *)
  | `LetModule of (loc * auident * module_expr * expr)
        (* match e with [ mc ] *)
  | `Match of (loc * expr * match_case)
        (* new i *)
  | `New of (loc * ident)
        (* object ((p))? (cst)? end *)

  | `Obj of (loc * class_str_item)
  | `ObjPat of (loc * patt * class_str_item)
        (* ?s or ?s:e *)
  | `OptLabl of (loc *alident * expr)
  | `OptLablS of (loc * alident)
        (* {< rb >} *)
  | `OvrInst of (loc * rec_expr)
  | `OvrInstEmpty of loc
        (* do { e } *)
  | `Seq of (loc * expr)
        (* e#s *)
  | `Send of (loc * expr * alident)
        (* e.[e] *)
  | `StringDot of (loc * expr * expr)
        (* try e with [ mc ] *)
  | `Try of (loc * expr * match_case)
        (* (e : t) *)
  | (* `Constraint *) `Constraint of (loc * expr * ctyp)
  | `Coercion of (loc * expr * ctyp * ctyp) (* (e : t) or (e : t :> t) *)          
        (* while e do { e } *)
  | `While of (loc * expr * expr)
        (* let open i in e *)
  | `LetOpen of (loc * ident * expr)
        (* fun (type t) -> e *)
        (* let f x (type t) y z = e *)
  | `LocalTypeFun of (loc *  alident * expr)
        (* (module ME : S) which is represented as (module (ME : S)) *)
  | `Package_expr of (loc * module_expr) ]
and rec_expr =
  [= `Sem of (loc * rec_expr * rec_expr)
  | `RecBind  of (loc * ident * expr)
  | any (* Faked here to be symmertric to rec_patt *)
  | ant (* $s$ *) ]
and module_type =
  [= nil
  | sid
       (* functor (s : mt) -> mt *)
  | `MtFun of (loc * auident * module_type * module_type)
        (* sig sg end *)
  | `Sig of (loc * sig_item)
        (* mt with wc *)
  | `With of (loc * module_type * with_constr)
        (* module type of m *)
  | `ModuleTypeOf of (loc * module_expr)
  | ant  ]
and sig_item =
  [= nil
     (* class cict *)
  | `Class of (loc * class_type)
      (* class type cict *)
  | `ClassType of (loc * class_type)
        (* sg ; sg *)
  | `Sem of (loc * sig_item * sig_item)
        
        (* # s or # s e *)
  | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * expr) (* semantics *)
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
  | `Open of (loc * ident)
  | `Type of (loc * typedecl)
        (* va s : t *)
  |  `Val of (loc * alident * ctyp)
  | ant  ]
          
and with_constr =
  [= nil
     (* type t = t *)
  | `TypeEq of (loc * ctyp * ctyp)
  | `TypeEqPriv of (loc * ctyp * ctyp)
        (* module i = i *)
  | `ModuleEq of (loc * ident * ident)
        (* type t := t *)
  | `TypeSubst of (loc * ctyp * ctyp)
        (* module i := i *)
  | `ModuleSubst of (loc * ident * ident)
  | `And of (loc * with_constr * with_constr)
  | ant  ]
             
             (*
    let-binding	::=	pattern =  expr  
     value-name  { parameter }  [: typexpr] =  expr  
    value-name : type  { typeconstr } .  typexpr =  expr
    
   *)           
and binding =
  [= nil
  | `And of (loc * binding * binding)
        (* p = e *) (* let patt = expr *)
  | `Bind  of (loc * patt * expr)
  | ant  ]
and module_binding =
  [= nil
     (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
  | `And of (loc * module_binding * module_binding)
      (* s : mt = me *)
  | `ModuleBind  of (loc *  auident * module_type * module_expr)
      (* s : mt *)
  | `Constraint  of (loc * auident * module_type)
  | ant ]
and match_case =
  [= nil
  | `Or of (loc * match_case * match_case)
        (* p (when e)? -> e *)
  | `Case of (loc * patt * expr (* * expr *))
  | `CaseWhen of (loc * patt * expr * expr)
     (* | `Caseow of loc and patt and option expr and expr (\* FIXME *\) *)
  | ant  ]
and module_expr =
  [= nil
  | sid
      (* me me *)
  | `App of (loc * module_expr * module_expr)
        (* functor (s : mt) -> me *)
  | `Functor of (loc * auident * module_type * module_expr)
  | `Struct of (loc * str_item)
        (* (me : mt) *)
  | `Constraint of (loc * module_expr * module_type)
        (* (value e) *)
        (* (value e : S) which is represented as (value (e : S)) *)
  | `PackageModule of (loc * expr)
  | ant  ]
and str_item =
  [= nil
     (* class cice *)
  | `Class of (loc * class_expr)
        (* class type cict *)
  | `ClassType of (loc * class_type)
  | `Sem of (loc * str_item * str_item)
                
  | `DirectiveSimple of (loc * alident)
  | `Directive of (loc * alident * expr)

        (* exception t or exception t = i *)
        (* | `Exception of ( loc * ctyp * meta_option(\*FIXME*\) ident) *)
  | `Exception of ( loc * of_ctyp)
        (* TODO ExceptionRebind
           http://caml.inria.fr/pub/docs/manual-ocaml/manual016.html
         *)     
        (* e *)
  | `StExp of (loc * expr)
        (* external s : t = s ... s *)
  | `External of (loc * alident  * ctyp *  strings)
        (* include me *)
  | `Include of (loc * module_expr)
        (* module s = me *)
  | `Module of (loc * auident * module_expr)
        (* module rec mb *)
  | `RecModule of (loc * module_binding)
        (* module type s = mt *)
  | `ModuleType of (loc * auident * module_type)
        (* open i *)
  | `Open of (loc * ident)
        (* type t *)
  | `Type of (loc * (* ctyp *)typedecl)
        (* value (rec)? bi *)
  | `Value of (loc * rec_flag * binding)
  | ant (* $s$ *) ]
and class_type =
  [= nil
     (* (virtual)? i ([ t ])? *)
  | `CtCon of (loc * virtual_flag * ident * (* ctyp *) type_parameters)
        (* [t] -> ct *)
  | `CtFun of (loc * ctyp * class_type)
      (* object ((t))? (csg)? end *)
  | `CtSig of (loc * ctyp * class_sig_item)
        (* ct and ct *)
  | `And of (loc * class_type * class_type)
        (* ct : ct *)
  | `CtCol of (loc * class_type * class_type)
        (* ct = ct *)
  | `CtEq  of (loc * class_type * class_type)
  | ant ]
and class_sig_item =
  [= nil
     (* type t = t *)
  | `Eq of (loc * ctyp * ctyp)
        (* csg ; csg *)
  | `Sem of (loc * class_sig_item * class_sig_item)
        (* inherit ct *)
  | `SigInherit of (loc * class_type)
        (* method s : t or method private s : t *)
  | `Method of (loc * alident * private_flag * ctyp)
        (* val (virtual)? (mutable)? s : t *)
  | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)
        (* method virtual (private)? s : t *)
  | `CgVir of (loc *  alident * private_flag * ctyp)
  | ant ]
and class_expr =
  [= nil
      (* ce e *)
  | `CeApp of (loc * class_expr * expr)
      (* (virtual)? i ([ t ])? *)
  | `CeCon of (loc * virtual_flag * ident * (* ctyp *) type_parameters)
      (* fun p -> ce *)
  | `CeFun of (loc * patt * class_expr)
        (* let (rec)? bi in ce *)
  | `CeLet of (loc * rec_flag * binding * class_expr)
        (* object ((p))? (cst)? end *)
  | `Obj of (loc  * class_str_item)
  | `ObjPat of (loc * patt * class_str_item)
        
        (* ce : ct *)
  | `CeTyc of (loc * class_expr * class_type)
        (* ce and ce *)
  | `And of (loc * class_expr * class_expr)
        (* ce = ce *)
  | `Eq  of (loc * class_expr * class_expr)
  | ant ]
and class_str_item =
  [= nil
  | `Sem of (loc * class_str_item * class_str_item)
        (* type t = t *)
  | `Eq of (loc * ctyp * ctyp)
        (* inherit(!)? ce (as s)? *)
  | `Inherit of (loc * override_flag * class_expr)
  | `InheritAs of (loc * override_flag * class_expr * alident)
      (* initializer e *)
  | `Initializer of (loc * expr)
        (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
  | `CrMth of (loc * alident * override_flag * private_flag * expr * ctyp)
        (* value(!)? (mutable)? s = e *)
  | `CrVal of (loc *  alident * override_flag * mutable_flag * expr)
        (* method virtual (private)? s : t *)
  | `CrVir of (loc * alident * private_flag * ctyp)
        (* val virtual (mutable)? s : t *)
  | `CrVvr of (loc * alident * mutable_flag * ctyp)
  | ant  ]; 




(* Any is necessary, since sometimes you want to [meta_loc_patt] to [_]
   Faked here to make a common subtyp of expr patt to be expressive enough *)
type ep =
  [= (* nil *)
  (* |  *)sid
  | `App of (loc * ep * ep)
  | `Vrn of (loc * string)
  | `Com of (loc * ep * ep)
  | `Sem of (loc * ep * ep)
  | `Tup of (loc * ep)
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
