(* Note: when you modify these types you must increment
   ast magic numbers defined in FanConfig.ml. *)
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
    rec_binding        :: The type of record definitions

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
    type ant = [= `Ant of (loc * string)];
    type literal =
    [= `Chr of (loc * string) (* 'c' *)
    | `Int of (loc * string) (* 42 *)
    | `Int32 of (loc * string)
    | `Int64 of (loc * string)
    | `Flo of (loc * string)
    | `NativeInt of (loc * string)
      (* s *) (* "foo" *)
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

   type meta_option 'a =
    [= `None of loc 
    |  `Some of 'a
    | `Ant of (loc * string) ];
   type meta_list 'a =
    [= `LNil of loc
    | `LCons of ('a * meta_list 'a)
    | `Ant of (loc * string) ]; (* FIXME `Ant no location *)
   type alident =
    [= `Lid of (loc * string)
    | ant];
   type auident =
    [= `Uid of (loc * string)
    | ant];
   type astring =
    [= `C of (loc * string)
    | ant ];
   type ident =
    [= `IdAcc of (loc * ident * ident) (* i . i *)
    | `IdApp of (loc * ident * ident) (* i i *)
    | alident
    | auident];
   type ctyp =
    [= `Nil of loc
    | `Alias of (loc * ctyp * ctyp) (* t as t *) (* list 'a as 'a *)
    | `Any of loc (* _ *)
    | `TyApp of (loc * ctyp * ctyp) (* t t *) (* list 'a *)
    | `TyArr of (loc * ctyp * ctyp) (* t -> t *) (* int -> string *)
    | `TyCls of (loc * ident) (* #i *) (* #point *)
    | `TyLab of (loc * alident * ctyp) (* ~s:t *)
    | `Id  of (loc * ident) (* i *) (* `Lazy.t *)
    | `TyMan of (loc * ctyp * ctyp) (* t == t *) (* type t = [ A | B ] == `Foo.t *)

     (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
    | `TyDcl of (loc * alident * list ctyp * ctyp * list (ctyp * ctyp))
     (* FIXME, the location *)
          
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | `TyObj of (loc * ctyp * row_var_flag)
    | `TyOlb of (loc * alident * ctyp) (* ?s:t *)
    | `TyPol of (loc * ctyp * ctyp) (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | `TyTypePol of (loc * ctyp * ctyp) (* type t . t *) (* type a . list a -> a *)

    (* | `TyQuo of (loc * string) (\* 's *\) *)
    (* | `TyQuP of (loc * string) (\* +'s *\) *)
    (* | `TyQuM of (loc * string) (\* -'s *\) *)
          
    (* | `TyAnP of loc (\* +_ *\) *)
    (* | `TyAnM of loc (\* -_ *\) *)

    (*  +'s -'s 's +_ -_ *)      
    | `Quote of (loc * position_flag * meta_option alident)
          
    | `TyRec of (loc * ctyp) (* { t } *) (* { foo : int ; bar : mutable string } *)
    | `TyCol of (loc * ctyp * ctyp) (* t : t *)
    | `TySem of (loc * ctyp * ctyp) (* t; t *)
    | `Com of (loc * ctyp * ctyp) (* t, t *)
    | `Sum of (loc * ctyp) (* [ t ] *) (* [ A of int * string | B ] *)
    | `Of  of (loc * ctyp * ctyp) (* t of t *) (* A of int *)
    | `And of (loc * ctyp * ctyp) (* t * t *)
    | `Or  of (loc * ctyp * ctyp) (* t | t *)
    | `Private of (loc * ctyp) (* private t *)
    | `Mutable of (loc * ctyp) (* mutable t *)
    | `Tup of (loc * ctyp) (* ( t ) *) (* (int * string) *)
    | `Sta of (loc * ctyp * ctyp) (* t * t *)

    | `TyVrn of (loc * string) (* `s *)          
    | `TyVrnEq of (loc * ctyp) (* [ = t ] *)
    | `TyVrnSup of (loc * ctyp) (* [ > t ] *)
    | `TyVrnInf of (loc * ctyp) (* [ < t ] *)
    | `TyVrnInfSup of (loc * ctyp * ctyp) (* [ < t > t ] *)
          
    | `TyAmp of (loc * ctyp * ctyp) (* t & t *)
    | `TyOfAmp of (loc * ctyp * ctyp) (* t of & t *)
    | `Package of (loc * module_type) (* (module S) *)
    | ant (* $s$ *)]
   and patt =
    [= `Nil of loc
    | `Id  of (loc * ident) (* i *)
    | `Alias of (loc * patt * alident) (* p as p *) (* (Node x y as n) *)
    | ant (* $s$ *)
    | `Any of loc (* _ *)
    | `PaApp of (loc * patt * patt) (* p p *) (* fun x y -> *)
    | `Array of (loc * patt) (* [| p |] *)
    | `PaCom of (loc * patt * patt) (* p, p *)
    | `Sem of (loc * patt * patt) (* p; p *)
    | literal
    | `Label of (loc * alident * patt) (* ~s or ~s:(p) *)
    (* ?s or ?s:(p)  ?s:(p = e) or ?(p = e) *)
    | `PaOlbi of (loc * alident * patt * meta_option expr)
    | `PaOrp of (loc * patt * patt) (* p | p *)
    | `PaRng of (loc * patt * patt) (* p .. p *)
    | `PaRec of (loc * patt) (* { p } *)
    | `PaEq  of (loc * ident * patt) (* i = p *)

    | `PaTup of (loc * patt) (* ( p ) *)
    | `PaTyc of (loc * patt * ctyp) (* (p : t) *)
    | `PaTyp of (loc * ident) (* #i *)
    | `PaVrn of (loc * string) (* `s *)
    | `Lazy of (loc * patt) (* lazy p *)

    (* (module M : ty ) *)      
    | `ModuleUnpack of (loc * (* string *)auident * meta_option ctyp)]
  and expr =
    [= `Nil of loc
    | `Id  of (loc * ident) (* i *)
    | `ExAcc of (loc * expr * expr) (* e.e *)
    | ant (* $s$ *)
    | `ExApp of (loc * expr * expr) (* e e *)
    | `ExAre of (loc * expr * expr) (* e.(e) *)
    | `Array of (loc * expr) (* [| e |] *)
    | `Sem of (loc * expr * expr) (* e; e *)
    | `ExAsf of loc (* assert `False *)
    | `ExAsr of (loc * expr) (* assert e *)
    | `ExAss of (loc * expr * expr) (* e := e *)

      (* for s = e to/downto e do { e } *)
    | `For of (loc * alident * expr * expr * direction_flag * expr)
    | `Fun of (loc * match_case) (* fun [ mc ] *)

    | `IfThenElse of (loc * expr * expr * expr) (* if e then e else e *)
          
    | literal
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
    | `Obj of (loc * patt * class_str_item)
      (* ?s or ?s:e *)
    | `OptLabl of (loc *alident * expr)
      (* {< rb >} *)
    | `OvrInst of (loc * rec_binding)
      (* { rb } or { (e) with rb } *)
    | `Record of (loc * rec_binding * expr)
      (* do { e } *)
    | `Seq of (loc * expr)
      (* e#s *)
    | `Send of (loc * expr * alident)
      (* e.[e] *)
    | `StringDot of (loc * expr * expr)
      (* try e with [ mc ] *)
    | `Try of (loc * expr * match_case)
      (* (e) *)
    | `ExTup of (loc * expr)
      (* e, e *)
    | `ExCom of (loc * expr * expr)
      (* (e : t) *)
    | `Constraint_exp of (loc * expr * ctyp)
    | `ExCoe of (loc * expr * ctyp * ctyp) (* (e : t) or (e : t :> t) *)          
      (* `s *)
    | `ExVrn of (loc * string)
      (* while e do { e } *)
    | `While of (loc * expr * expr)
      (* let open i in e *)
    | `Let_open of (loc * ident * expr)
      (* fun (type t) -> e *)
      (* let f x (type t) y z = e *)
    | `LocalTypeFun of (loc *  alident * expr)
      (* (module ME : S) which is represented as (module (ME : S)) *)
    | `Package_expr of (loc * module_expr) ]
  and module_type =
    [= `Nil of loc
      (* i *) (* A.B.C *)
    | `Id  of (loc * ident)
      (* functor (s : mt) -> mt *)
    | `MtFun of (loc * auident * module_type * module_type)
      (* 's *)
    | `MtQuo of (loc * string)
      (* sig sg end *)
    | `Sig of (loc * sig_item)
      (* mt with wc *)
    | `MtWit of (loc * module_type * with_constr)
      (* module type of m *)
    | `Of of (loc * module_expr)
    | ant (* $s$ *) ]
  and sig_item =
    [= `Nil of loc
      (* class cict *)
    | `Class of (loc * class_type)
      (* class type cict *)
    | `ClassType of (loc * class_type)
      (* sg ; sg *)
    | `Sem of (loc * sig_item * sig_item)
      (* # s or # s e *)
    | `Directive of (loc * alident * expr) (* semantics *)
      (* exception t *)
    | `Exception of (loc * ctyp)
      (* external s : t = s ... s *)
    | `External of (loc * alident  * ctyp * meta_list string)
      (* include mt *)
    | `Include of (loc * module_type)

    (* module s : mt *)
    | `Module of (loc * (* string *)auident * module_type)
          
      (* module rec mb *)
    | `RecModule of (loc * module_binding)

      (* module type s = mt *)
    | `ModuleType of (loc * (* string *)auident * module_type)
          
      (* open i *)
    | `Open of (loc * ident)
      (* type t *)
    | `Type of (loc * ctyp)

      (* va s : t *)
    |  `Val of (loc * alident * ctyp)
          
    | ant (* $s$ *) ]
  and with_constr =
    [= `Nil of loc
      (* type t = t *)
    | `TypeEq of (loc * ctyp * ctyp)
      (* module i = i *)
    | `ModuleEq of (loc * ident * ident)
      (* type t := t *)
    | `TypeSubst of (loc * ctyp * ctyp)
      (* module i := i *)
    | `ModuleSubst of (loc * ident * ident)
      (* wc * wc *)
    | `And of (loc * with_constr * with_constr)
    | ant (* $s$ *) ]
  and binding =
    [= `Nil of loc
      (* bi and bi *) (* let a = 42 and c = 43 *)
    | `And of (loc * binding * binding)
      (* p = e *) (* let patt = expr *)
    | `Bind  of (loc * patt * expr)
    | ant (* $s$ *) ]
  and rec_binding =
    [= `Nil of loc
      (* rb ; rb *)
    | `Sem of (loc * rec_binding * rec_binding)
      (* i = e *)
    | `RecBind  of (loc * ident * expr)
    | ant (* $s$ *) ]
  and module_binding =
    [= `Nil of loc
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | `And of (loc * module_binding * module_binding)
      (* s : mt = me *)
    | `ModuleBind  of (loc * (* string *) auident * module_type * module_expr)

      (* s : mt *)
    | `ModuleConstraint  of (loc * (* string *)auident * module_type)
    | ant (* $s$ *) ]
  and match_case =
    [= `Nil of loc
      (* a | a *)
    | `McOr of (loc * match_case * match_case)
      (* p (when e)? -> e *)
    | `Case of (loc * patt * expr * expr)
    (* | `Caseow of loc and patt and option expr and expr (\* FIXME *\) *)
    | ant (* $s$ *) ]
  and module_expr =
    [= `Nil of loc
      (* i *)
    | `Id  of (loc * ident)
      (* me me *)
    | `MeApp of (loc * module_expr * module_expr)
      (* functor (s : mt) -> me *)
    | `Functor of (loc * (* string *)auident * module_type * module_expr)
      (* struct st end *)
    | `Struct of (loc * str_item)
      (* (me : mt) *)
    | `ModuleExprConstraint of (loc * module_expr * module_type)
      (* (value e) *)
      (* (value e : S) which is represented as (value (e : S)) *)
    | `PackageModule of (loc * expr)
    | ant (* $s$ *) ]
  and str_item =
    [= `Nil of loc
      (* class cice *)
    | `Class of (loc * class_expr)
      (* class type cict *)
    | `ClassType of (loc * class_type)
      (* st ; st *)
    | `Sem of (loc * str_item * str_item)
      (* # s or # s e *)
    | `Directive of (loc * alident * expr)
      (* exception t or exception t = i *)
    | `Exception of ( loc * ctyp * meta_option(*FIXME*) ident)
      (* e *)
    | `StExp of (loc * expr)
      (* external s : t = s ... s *)
    | `External of (loc * alident  * ctyp * meta_list string)
      (* include me *)
    | `Include of (loc * module_expr)

      (* module s = me *)
    | `Module of (loc * auident * module_expr)
          
      (* module rec mb *)
    | `RecModule of (loc * module_binding)

      (* module type s = mt *)
    | `ModuleType of (loc * (* string *)auident * module_type)
      (* open i *)
    | `Open of (loc * ident)
      (* type t *)
    | `Type of (loc * ctyp)
      (* value (rec)? bi *)
    | `Value of (loc * rec_flag * binding)
    | ant (* $s$ *) ]
  and class_type =
    [= `Nil of loc
      (* (virtual)? i ([ t ])? *)
    | `CtCon of (loc * virtual_flag * ident * ctyp)
      (* [t] -> ct *)
    | `CtFun of (loc * ctyp * class_type)
      (* object ((t))? (csg)? end *)
    | `CtSig of (loc * ctyp * class_sig_item)
      (* ct and ct *)
    | `CtAnd of (loc * class_type * class_type)
      (* ct : ct *)
    | `CtCol of (loc * class_type * class_type)
      (* ct = ct *)
    | `CtEq  of (loc * class_type * class_type)
      (* $s$ *)
    | ant ]
  and class_sig_item =
    [= `Nil of loc
      (* type t = t *)
    | `Eq of (loc * ctyp * ctyp)
      (* csg ; csg *)
    | `Sem of (loc * class_sig_item * class_sig_item)
      (* inherit ct *)
    | `Inherit of (loc * class_type)
      (* method s : t or method private s : t *)
    | `Method of (loc * alident * private_flag * ctyp)

    (* val (virtual)? (mutable)? s : t *)
    | `CgVal of (loc * alident * mutable_flag * virtual_flag * ctyp)

     (* method virtual (private)? s : t *)
    | `CgVir of (loc *  alident * private_flag * ctyp)
    | ant (* $s$ *) ]
  and class_expr =
    [= `Nil of loc
      (* ce e *)
    | `CeApp of (loc * class_expr * expr)
      (* (virtual)? i ([ t ])? *)
    | `CeCon of (loc * virtual_flag * ident * ctyp)
      (* fun p -> ce *)
    | `CeFun of (loc * patt * class_expr)
      (* let (rec)? bi in ce *)
    | `CeLet of (loc * rec_flag * binding * class_expr)
      (* object ((p))? (cst)? end *)
    | `Obj of (loc * patt * class_str_item)
      (* ce : ct *)
    | `CeTyc of (loc * class_expr * class_type)
      (* ce and ce *)
    | `And of (loc * class_expr * class_expr)
      (* ce = ce *)
    | `Eq  of (loc * class_expr * class_expr)
      (* $s$ *)
    | ant ]
  and class_str_item =
    [= `Nil of loc
      (* cst ; cst *)
    | `CrSem of (loc * class_str_item * class_str_item)
      (* type t = t *)
    | `Eq of (loc * ctyp * ctyp)
      (* inherit(!)? ce (as s)? *)
    | `Inherit of (loc * override_flag * class_expr * meta_option alident)
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
    | ant (* $s$ *) ]; 


    
(*
  let a (x: [= `External of (loc * [= `Lid of (loc*string)] * ctyp * meta_list string)  ]) =
  (x:> str_item);
  
  
  {:patt| (A x as y)|}
  {:patt| (y as A x)|}
 *)
