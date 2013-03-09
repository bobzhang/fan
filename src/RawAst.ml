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


type nil = [= `Nil ];

type literal =
  [= `Chr of string
  | `Int of string
  | `Int32 of string
  | `Int64 of string
  | `Flo of string
  | `NativeInt of  string
  | `Str of string];   

type rec_flag =
  [= `Recursive 
  | `ReNil];

type direction_flag =
  [= `To 
  | `Downto];

type mutable_flag =
  [= `Mutable
  | `MuNil];

type private_flag =
  [= `Private
  | `PrNil];

type virtual_flag =
  [= `Virtual
  | `ViNil];

type override_flag =
  [= `Override
  | `OvNil];

type row_var_flag =
  [= `RowVar
  | `RvNil];

type position_flag =
  [= `Positive 
  | `Negative 
  | `Normal];

type meta_bool =
  [=`True
  |`False];


type strings =
  [= `App of ( strings * strings)
  | `Str of ( string)]  ;

type alident =
  [= `Lid of string];

type auident =
  [= `Uid of string];

type aident =
  [= alident
  | auident ];

type astring =
  [= `C of ( string) ];

type uident =
  [= `Dot of ( uident * uident)
  | `App of ( uident * uident)
  | auident];


(* type uident = *)
(*  [= `Dot of ( uident * uident) *)
(*  | `App of ( uident * uident) *)
(*  ];    *)
(* {:ident|A.B.C.d|}
      `Dot
      (`Uid "A"
        `Dot (`Uid "B")
           `Dot("C",`Lid "d"))
    *)
type ident =
  [= `Dot of ( ident * ident) (* i . i *)
  | `App of ( ident * ident) (* i i *)
  | alident
  | auident];

(* type dopath = *)
(*  [= `Dot of ( dopath * dopath) *)
(*  | auident ] ; *)

(* A.B.C *)
type dupath =
  [= `Dot of ( dupath * dupath)
  | auident];

type dlpath=
  [= `Dot of ( dupath * alident)
  | alident];



type sid = [= `Id of ident];
type any = [= `Any ];

type ctyp =
  [= nil
  | `Alias of ( ctyp * alident)
  | any
  | `App of ( ctyp * ctyp) (* t t *) (* list 'a *)
  | `Arrow of ( ctyp * ctyp)
  | `ClassPath of ( ident) (* #i *) (* #point *)
  | `Label of ( alident * ctyp) (* ~s:t *)
        (* ?s:t *)
  | `OptLabl of ( alident * ctyp )
  | sid
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
  | `TyObj of ( name_ctyp * row_var_flag )
  | `TyPol of ( ctyp * ctyp) (* ! t . t *) (* ! 'a . list 'a -> 'a *)
  | `TyTypePol of ( ctyp * ctyp) (* type t . t *) (* type a . list a -> a *)
  (*  +'s -'s 's +_ -_ *)      
  | `Quote of ( position_flag * alident) 
  | `QuoteAny of ( position_flag )
  | `Tup of ctyp (* ( t ) *) (* (int * string) *)
  | `Sta of ( ctyp * ctyp) (* t * t *)
  | `PolyEq of ( row_field)
  | `PolySup of  row_field 
  | `PolyInf of row_field
  | `PolyInfSup of ( row_field * tag_names)
  | `Package of  module_type (* (module S) *) ]
and type_parameters =
  [= `Com of ( type_parameters * type_parameters)
  | `Ctyp of ( ctyp)
  | nil]  
and row_field =
  [= nil
  | `Or of ( row_field * row_field )
  | `TyVrn of ( astring)
  | `TyVrnOf of ( astring * ctyp)
  |  `Ctyp of ctyp]
and tag_names =
  [= nil
  | `App of ( tag_names * tag_names)
  | `TyVrn of astring]   
and typedecl =
    (* {:str_item| type  ('a, 'b, 'c) t = t |} *)
  [= `TyDcl of ( alident * list ctyp * type_info * list (ctyp * ctyp))
  | `And of ( typedecl * typedecl)
  | nil ]
      (* original syntax
         {[ type v = u = A of int ]}
       revise syntax
       {[ type v = u = [A of int];]} 
     *)
and type_info =        (* FIXME be more preicse *)
  [=
   (* type u = v = [A of int ] *)
   `TyMan of ( ctyp * private_flag  * type_repr)
   (* type u = A.t = {x:int} *)
  | `TyRepr of ( private_flag * type_repr)

   (* type u = int *)
  | `TyEq of ( private_flag * ctyp)
  | nil ]  
and type_repr =
  [= `Record of ( name_ctyp)
  | `Sum of ( or_ctyp)
  | nil ]   
and name_ctyp =
  [= `Sem of ( name_ctyp * name_ctyp)
  | `TyCol of ( sid * ctyp )
  | `TyColMut of ( sid * ctyp)
  | nil ]
and or_ctyp =
  [= `Or of ( or_ctyp * or_ctyp )
  | `TyCol of ( sid * ctyp)
  | `Of of ( (* ctyp *)sid * ctyp)
  | sid
  | nil ]
and of_ctyp =
  [= `Of of ( sid * ctyp)
  | sid
  | nil]
         
and patt =
  [= nil
  | sid
  | `App of ( patt * patt)
  | `Vrn of  string
  | `Com of ( patt * patt)
  | `Sem of ( patt * patt)
  | `Tup of  patt 
  | any
  | `Record of ( rec_patt)
  | literal
  | `Alias of ( patt * alident)  (* (Node x y as n) *)
  | `Array of  patt (* [| p |] *)
  | `Label of ( alident * patt) (* ~s or ~s:(p) *)
    (* ?s or ?s:(p)   *)
  | `OptLabl of ( alident * patt)
    (* ?s:(p = e) or ?(p = e) *)
  | `OptLablExpr of ( alident * patt * expr)
  | `Or of ( patt * patt) (* p | p *)
  | `PaRng (* `Range  *)of ( patt * patt) (* p .. p *)
  | `Constraint of ( patt * ctyp) (* (p : t) *)
  | `ClassPath of  ident (* #i *)
  | `Lazy of  patt (* lazy p *)
    (* (module M ) *)   
  | `ModuleUnpack of auident
    (* (module M : ty ) *)      
  | `ModuleConstraint of ( auident * ctyp) ]
and rec_patt =
  [= nil
  | `RecBind of ( ident * patt)
  | `Sem of ( rec_patt * rec_patt)
  | any]  
and expr =
  [= nil
  | sid
  | `App of ( expr * expr)
  | `Vrn of  string
  | `Com of ( expr * expr)
  | `Sem of ( expr * expr)
  | `Tup of expr
  | any
  | `Record of rec_expr
  | literal
  | `RecordWith of ( rec_expr  * expr) (* { (e) with rb }  *)         
  | `Dot of ( expr * expr) (* e.e *)
  | `ArrayDot of ( expr * expr) (* e.(e) *)
  | `Array of expr (* [| e |] *)
  | `ExAsf  (* assert `False *)
  | `ExAsr of expr (* assert e *)
  | `Assign of (expr * expr) (* e := e *)
  | `For of ( alident * expr * expr * direction_flag * expr)
        (* for s = e to/downto e do { e } *)
  | `Fun of match_case (* fun [ mc ] *)
  | `IfThenElse of ( expr * expr * expr) (* if e then e else e *)
  | `IfThen of ( expr * expr) (* if e then e *)
  | `Label of ( alident * expr) (* ~s or ~s:e *)
  | `Lazy of  expr (* lazy e *)
  | `LetIn of ( rec_flag * binding * expr) (* let b in e or let rec b in e *)
  | `LetModule of ( auident * module_expr * expr) (* let module s = me in e *)
  | `Match of ( expr * match_case) (* match e with [ mc ] *)
  | `New of ident (* new i *)
  | `Obj of ( patt * class_str_item) (* object ((p))? (cst)? end *)
  | `OptLabl of (alident * expr) (* ?s or ?s:e *)
  | `OvrInst of  rec_expr (* {< rb >} *)
  | `Seq of  expr (* do { e } *)
  | `Send of ( expr * alident) (* e#s *)
  | `StringDot of ( expr * expr) (* e.[e] *)
  | `Try of ( expr * match_case) (* try e with [ mc ] *)
  | `Constraint of ( expr * ctyp) (* (e : t) *)
  | `Coercion of ( expr * ctyp * ctyp) (* (e : t) or (e : t :> t) *)          
  | `While of ( expr * expr) (* while e do { e } *)
  | `LetOpen of ( ident * expr) (* let open i in e *)
        (* fun (type t) -> e *)
        (* let f x (type t) y z = e *)
  | `LocalTypeFun of (  alident * expr)
        (* (module ME : S) which is represented as (module (ME : S)) *)
  | `Package_expr of ( module_expr) ]
and rec_expr =
  [= nil
  | `Sem of ( rec_expr * rec_expr) (* rb ; rb *)
  | `RecBind  of ( ident * expr)        (* i = e *)
  | any (* Faked here to be symmertric to rec_patt *)]
and module_type =
  [= nil
  | sid
  | `MtFun of ( auident * module_type * module_type) (* functor (s : mt) -> mt *)
  | `Sig of sig_item (* sig sg end *)
  | `With of ( module_type * with_constr) (* mt with wc *)
  | `ModuleTypeOf of module_expr (* module type of m *)]
and sig_item =
  [= nil
  | `Class of class_type (* class cict *)
  | `ClassType of  class_type (* class type cict *)
  | `Sem of ( sig_item * sig_item) (* sg ; sg *)
  | `Directive of ( alident * expr)(* # s or # s e *)
  | `Exception of of_ctyp (* exception t *)
  | `External of ( alident  * ctyp * strings) (* external s : t = s ... s *)
  | `Include of module_type
  | `Module of ( auident * module_type) (* module s : mt *)
  | `RecModule of module_binding (* module rec mb *)
  | `ModuleType of ( auident * module_type) (* module type s = mt *)
  | `Open of ident
  | `Type of typedecl
  |  `Val of ( alident * ctyp) (* va s : t *)]
          
and with_constr =
  [= nil
  | `TypeEq of ( ctyp * ctyp) (* type t = t *)
  | `TypeEqPriv of ( ctyp * ctyp)
  | `ModuleEq of ( ident * ident) (* module i = i *)
  | `TypeSubst of ( ctyp * ctyp) (* type t := t *)
  | `ModuleSubst of ( ident * ident) (* module i := i *)
  | `And of ( with_constr * with_constr)]
             
(*
  let-binding	::=	pattern =  expr  
   value-name  { parameter }  [: typexpr] =  expr  
  value-name : type  { typeconstr } .  typexpr =  expr
   *)           
and binding =
  [= nil
  | `And of ( binding * binding)
  | `Bind  of ( patt * expr) (* p = e *) (* let patt = expr *)]
and module_binding =
  [= nil
     (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
  | `And of ( module_binding * module_binding)
  | `ModuleBind  of (  auident * module_type * module_expr) (* s : mt = me *)
  | `Constraint  of ( auident * module_type) (* s : mt *)]
and match_case =
  [= nil
  | `Or of ( match_case * match_case)
  | `Case of ( patt * expr * expr) (* p (when e)? -> e *)
     (* | `Caseow of loc and patt and option expr and expr (\* FIXME *\) *)
  ]
and module_expr =
  [= nil
  | sid
  | `App of ( module_expr * module_expr)      (* me me *)
  | `Functor of ( auident * module_type * module_expr) (* functor (s : mt) -> me *)
  | `Struct of ( str_item)
  | `Constraint of ( module_expr * module_type) (* (me : mt) *)
        (* (value e) *)
        (* (value e : S) which is represented as (value (e : S)) *)
  | `PackageModule of ( expr)]
and str_item =
  [= nil
  | `Class of class_expr (* class cice *)
  | `ClassType of ( class_type) (* class type cict *)
  | `Sem of ( str_item * str_item) (* # s or # s e *)
  | `Directive of ( alident * expr)
        (* exception t or exception t = i *)
        (* | `Exception of ( ctyp * meta_option(\*FIXME*\) ident) *)
  | `Exception of ( of_ctyp)
        (* TODO ExceptionRebind
           http://caml.inria.fr/pub/docs/manual-ocaml/manual016.html
         *)     
        (* e *)
  | `StExp of expr
  | `External of ( alident  * ctyp *  strings) (* external s : t = s ... s *)
  | `Include of module_expr
  | `Module of ( auident * module_expr) (* module s = me *)
  | `RecModule of ( module_binding) (* module rec mb *)
  | `ModuleType of ( auident * module_type) (* module type s = mt *)
  | `Open of ( ident)
  | `Type of typedecl (* type t *)
  | `Value of ( rec_flag * binding) (* value (rec)? bi *)]
and class_type =
  [= nil
     (* (virtual)? i ([ t ])? *)
  | `CtCon of ( virtual_flag * ident *  type_parameters)
  | `CtFun of ( ctyp * class_type) (* [t] -> ct *)
      (* object ((t))? (csg)? end *)
  | `CtSig of ( ctyp * class_sig_item)
  | `And of ( class_type * class_type) (* ct and ct *)
  | `CtCol of ( class_type * class_type) (* ct : ct *)
  | `CtEq  of ( class_type * class_type) (* ct = ct *) ]
and class_sig_item =
  [= nil
  | `Eq of ( ctyp * ctyp) (* type t = t *)
  | `Sem of ( class_sig_item * class_sig_item)
  | `SigInherit of class_type
        (* method s : t or method private s : t *)
  | `Method of ( alident * private_flag * ctyp)
        (* val (virtual)? (mutable)? s : t *)
  | `CgVal of ( alident * mutable_flag * virtual_flag * ctyp)
        (* method virtual (private)? s : t *)
  | `CgVir of (  alident * private_flag * ctyp)]
and class_expr =
  [= nil
      (* ce e *)
  | `CeApp of ( class_expr * expr)
      (* (virtual)? i ([ t ])? *)
  | `CeCon of ( virtual_flag * ident * (* ctyp *) type_parameters)
      (* fun p -> ce *)
  | `CeFun of ( patt * class_expr)
        (* let (rec)? bi in ce *)
  | `CeLet of ( rec_flag * binding * class_expr)
        (* object ((p))? (cst)? end *)
  | `Obj of ( patt * class_str_item)
        (* ce : ct *)
  | `CeTyc of ( class_expr * class_type)
        (* ce and ce *)
  | `And of ( class_expr * class_expr)
        (* ce = ce *)
  | `Eq  of ( class_expr * class_expr)]
and class_str_item =
  [= nil
  | `Sem of ( class_str_item * class_str_item)
  | `Eq of ( ctyp * ctyp) (* type t = t *)
        (* inherit(!)? ce (as s)? *)
  | `Inherit of ( override_flag * class_expr)
  | `InheritAs of ( override_flag * class_expr * alident)
  | `Initializer of expr
        (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
  | `CrMth of ( alident * override_flag * private_flag * expr * ctyp)
        (* value(!)? (mutable)? s = e *)
  | `CrVal of (  alident * override_flag * mutable_flag * expr)
        (* method virtual (private)? s : t *)
  | `CrVir of ( alident * private_flag * ctyp)
        (* val virtual (mutable)? s : t *)
  | `CrVvr of ( alident * mutable_flag * ctyp)]; 

(* Any is necessary, since sometimes you want to [meta_loc_patt] to [_]
   Faked here to make a common subtyp of expr patt to be expressive enough *)
type ep =
  [= nil
  | sid
  | `App of ( ep * ep)
  | `Vrn of ( string)
  | `Com of ( ep * ep)
  | `Sem of ( ep * ep)
  | `Tup of ( ep)
  | any
  | `Array of ( ep )
  | `Record of ( rec_bind)
  | literal]
and rec_bind =
  [= nil
  | `RecBind of ( ident * ep)
  | `Sem of ( rec_bind * rec_bind)
  | any];
      
