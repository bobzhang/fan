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



type loc = FanLoc.t
   and meta_bool =
    [= `BTrue
    | `BFalse
    | `Ant of (loc*string) ]
   and rec_flag =
    [= `ReRecursive
    | `ReNil
    | `Ant of (loc*string) ]
   and direction_flag =
    [= `DiTo
    | `DiDownto
    | `Ant of (loc*string) ]
   and mutable_flag =
    [= `MuMutable
    | `MuNil
    | `Ant of (loc*string) ]
   and private_flag =
    [= `PrPrivate
    | `PrNil
    | `Ant of (loc*string) ]
   and virtual_flag =
    [= `ViVirtual
    | `ViNil
    | `Ant of (loc*string) ]
   and override_flag =
    [= `OvOverride
    | `OvNil
    | `Ant of (loc*string) ]
   and row_var_flag =
    [= `RvRowVar
    | `RvNil
    | `Ant of (loc*string) ]
   and meta_option 'a =
    [= `ONone
    | `OSome of 'a
    | `Ant of (loc*string) ]
   and meta_list 'a =
    [= `LNil
    | `LCons of ('a * meta_list 'a)
    | `Ant of (loc * string) ] (* FIXME `Ant no location *)
   and ident =
    [= `IdAcc of (loc * ident * ident) (* i . i *)
    | `IdApp of (loc * ident * ident) (* i i *)
    | `IdLid of (loc * string) (* foo *)
    | `IdUid of (loc * string) (* `Bar *)
    | `Ant of (loc * string) (* $s$ *) ]
   and ctyp =
    [= `TyNil of loc
    | `TyAli of (loc * ctyp * ctyp) (* t as t *) (* list 'a as 'a *)
    | `TyAny of loc (* _ *)
    | `TyApp of (loc * ctyp * ctyp) (* t t *) (* list 'a *)
    | `TyArr of (loc * ctyp * ctyp) (* t -> t *) (* int -> string *)
    | `TyCls of (loc * ident) (* #i *) (* #point *)
    | `TyLab of (loc * string * ctyp) (* ~s:t *)
    | `TyId  of (loc * ident) (* i *) (* `Lazy.t *)
    | `TyMan of (loc * ctyp * ctyp) (* t == t *) (* type t = [ A | B ] == `Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
    | `TyDcl of (loc * string * list ctyp * ctyp * list (ctyp * ctyp))
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | `TyObj of (loc * ctyp * row_var_flag)
    | `TyOlb of (loc * string * ctyp) (* ?s:t *)
    | `TyPol of (loc * ctyp * ctyp) (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | `TyTypePol of (loc * ctyp * ctyp) (* type t . t *) (* type a . list a -> a *)
    | `TyQuo of (loc * string) (* 's *)
    | `TyQuP of (loc * string) (* +'s *)
    | `TyQuM of (loc * string) (* -'s *)
    | `TyAnP of loc (* +_ *)
    | `TyAnM of loc (* -_ *)
    | `TyVrn of (loc * string) (* `s *)
    | `TyRec of (loc * ctyp) (* { t } *) (* { foo : int ; bar : mutable string } *)
    | `TyCol of (loc * ctyp * ctyp) (* t : t *)
    | `TySem of (loc * ctyp * ctyp) (* t; t *)
    | `TyCom of (loc * ctyp * ctyp) (* t, t *)
    | `TySum of (loc * ctyp) (* [ t ] *) (* [ A of int * string | B ] *)
    | `TyOf  of (loc * ctyp * ctyp) (* t of t *) (* A of int *)
    | `TyAnd of (loc * ctyp * ctyp) (* t * t *)
    | `TyOr  of (loc * ctyp * ctyp) (* t | t *)
    | `TyPrv of (loc * ctyp) (* private t *)
    | `TyMut of (loc * ctyp) (* mutable t *)
    | `TyTup of (loc * ctyp) (* ( t ) *) (* (int * string) *)
    | `TySta of (loc * ctyp * ctyp) (* t * t *)
    | `TyVrnEq of (loc * ctyp) (* [ = t ] *)
    | `TyVrnSup of (loc * ctyp) (* [ > t ] *)
    | `TyVrnInf of (loc * ctyp) (* [ < t ] *)
    | `TyVrnInfSup of (loc * ctyp * ctyp) (* [ < t > t ] *)
    | `TyAmp of (loc * ctyp * ctyp) (* t & t *)
    | `TyOfAmp of (loc * ctyp * ctyp) (* t of & t *)
    | `TyPkg of (loc * module_type) (* (module S) *)
    | `Ant of (loc * string) (* $s$ *)
    ]
   and patt =
    [= `PaNil of loc
    | `PaId  of (loc * ident) (* i *)
    | `PaAli of (loc * patt * patt) (* p as p *) (* (Node x y as n) *)
    | `Ant of (loc * string) (* $s$ *)
    | `PaAny of loc (* _ *)
    | `PaApp of (loc * patt * patt) (* p p *) (* fun x y -> *)
    | `PaArr of (loc * patt) (* [| p |] *)
    | `PaCom of (loc * patt * patt) (* p, p *)
    | `PaSem of (loc * patt * patt) (* p; p *)
    | `PaChr of (loc * string) (* c *) (* 'x' *)
    | `PaInt of (loc * string)
    | `PaInt32 of (loc * string)
    | `PaInt64 of (loc * string)
    | `PaNativeInt of (loc * string)
    | `PaFlo of (loc * string)
    | `PaLab of (loc * string * patt) (* ~s or ~s:(p) *)
    (* ?s or ?s:(p) *)
    | `PaOlb of (loc * string * patt)
    (* ?s:(p = e) or ?(p = e) *)
    | `PaOlbi of (loc * string * patt * expr)
    | `PaOrp of (loc * patt * patt) (* p | p *)
    | `PaRng of (loc * patt * patt) (* p .. p *)
    | `PaRec of (loc * patt) (* { p } *)
    | `PaEq  of (loc * ident * patt) (* i = p *)
    | `PaStr of (loc * string) (* s *)
    | `PaTup of (loc * patt) (* ( p ) *)
    | `PaTyc of (loc * patt * ctyp) (* (p : t) *)
    | `PaTyp of (loc * ident) (* #i *)
    | `PaVrn of (loc * string) (* `s *)
    | `PaLaz of (loc * patt) (* lazy p *)
    | `PaMod of (loc * string) (* (module M) *) ]
  and expr =
    [= `ExNil of loc
    | `ExId  of (loc * ident) (* i *)
    | `ExAcc of (loc * expr * expr) (* e.e *)
    | `Ant of (loc * string) (* $s$ *)
    | `ExApp of (loc * expr * expr) (* e e *)
    | `ExAre of (loc * expr * expr) (* e.(e) *)
    | `ExArr of (loc * expr) (* [| e |] *)
    | `ExSem of (loc * expr * expr) (* e; e *)
    | `ExAsf of loc (* assert `False *)
    | `ExAsr of (loc * expr) (* assert e *)
    | `ExAss of (loc * expr * expr) (* e := e *)
    | `ExChr of (loc * string) (* 'c' *)
    | `ExCoe of (loc * expr * ctyp * ctyp) (* (e : t) or (e : t :> t) *)
    | `ExFlo of (loc * string) (* 3.14 *)
      (* for s = e to/downto e do { e } *)
    | `ExFor of (loc * string * expr * expr * direction_flag * expr)
    | `ExFun of (loc * match_case) (* fun [ mc ] *)
    | `ExIfe of (loc * expr * expr * expr) (* if e then e else e *)
    | `ExInt of (loc * string) (* 42 *)
    | `ExInt32 of (loc * string)
    | `ExInt64 of (loc * string)
    | `ExNativeInt of (loc * string)
    | `ExLab of (loc * string * expr) (* ~s or ~s:e *)
    | `ExLaz of (loc * expr) (* lazy e *)
      (* let b in e or let rec b in e *)
    | `ExLet of (loc * rec_flag * binding * expr)
      (* let module s = me in e *)
    | `ExLmd of (loc * string * module_expr * expr)
      (* match e with [ mc ] *)
    | `ExMat of (loc * expr * match_case)
      (* new i *)
    | `ExNew of (loc * ident)
      (* object ((p))? (cst)? end *)
    | `ExObj of (loc * patt * class_str_item)
      (* ?s or ?s:e *)
    | `ExOlb of (loc * string * expr)
      (* {< rb >} *)
    | `ExOvr of (loc * rec_binding)
      (* { rb } or { (e) with rb } *)
    | `ExRec of (loc * rec_binding * expr)
      (* do { e } *)
    | `ExSeq of (loc * expr)
      (* e#s *)
    | `ExSnd of (loc * expr * string)
      (* e.[e] *)
    | `ExSte of (loc * expr * expr)
      (* s *) (* "foo" *)
    | `ExStr of (loc * string)
      (* try e with [ mc ] *)
    | `ExTry of (loc * expr * match_case)
      (* (e) *)
    | `ExTup of (loc * expr)
      (* e, e *)
    | `ExCom of (loc * expr * expr)
      (* (e : t) *)
    | `ExTyc of (loc * expr * ctyp)
      (* `s *)
    | `ExVrn of (loc * string)
      (* while e do { e } *)
    | `ExWhi of (loc * expr * expr)
      (* let open i in e *)
    | `ExOpI of (loc * ident * expr)
      (* fun (type t) -> e *)
      (* let f x (type t) y z = e *)
    | `ExFUN of (loc * string * expr)
      (* (module ME : S) which is represented as (module (ME : S)) *)
    | `ExPkg of (loc * module_expr) ]
  and module_type =
    [= `MtNil of loc
      (* i *) (* A.B.C *)
    | `MtId  of (loc * ident)
      (* functor (s : mt) -> mt *)
    | `MtFun of (loc * string * module_type * module_type)
      (* 's *)
    | `MtQuo of (loc * string)
      (* sig sg end *)
    | `MtSig of (loc * sig_item)
      (* mt with wc *)
    | `MtWit of (loc * module_type * with_constr)
      (* module type of m *)
    | `MtOf of (loc * module_expr)
    | `Ant of (loc * string) (* $s$ *) ]
  and sig_item =
    [= `SgNil of loc
      (* class cict *)
    | `SgCls of (loc * class_type)
      (* class type cict *)
    | `SgClt of (loc * class_type)
      (* sg ; sg *)
    | `SgSem of (loc * sig_item * sig_item)
      (* # s or # s e *)
    | `SgDir of (loc * string * expr)
      (* exception t *)
    | `SgExc of (loc * ctyp)
      (* external s : t = s ... s *)
    | `SgExt of (loc * string * ctyp * meta_list string)
      (* include mt *)
    | `SgInc of (loc * module_type)
      (* module s : mt *)
    | `SgMod of (loc * string * module_type)
      (* module rec mb *)
    | `SgRecMod of (loc * module_binding)
      (* module type s = mt *)
    | `SgMty of (loc * string * module_type)
      (* open i *)
    | `SgOpn of (loc * ident)
      (* type t *)
    | `SgTyp of (loc * ctyp)
      (* value s : t *)
    | `SgVal of (loc * string * ctyp)
    | `Ant of (loc * string) (* $s$ *) ]
  and with_constr =
    [= `WcNil of loc
      (* type t = t *)
    | `WcTyp of (loc * ctyp * ctyp)
      (* module i = i *)
    | `WcMod of (loc * ident * ident)
      (* type t := t *)
    | `WcTyS of (loc * ctyp * ctyp)
      (* module i := i *)
    | `WcMoS of (loc * ident * ident)
      (* wc * wc *)
    | `WcAnd of (loc * with_constr * with_constr)
    | `Ant of (loc * string) (* $s$ *) ]
  and binding =
    [= `BiNil of loc
      (* bi and bi *) (* let a = 42 and c = 43 *)
    | `BiAnd of (loc * binding * binding)
      (* p = e *) (* let patt = expr *)
    | `BiEq  of (loc * patt * expr)
    | `Ant of (loc * string) (* $s$ *) ]
  and rec_binding =
    [= `RbNil of loc
      (* rb ; rb *)
    | `RbSem of (loc * rec_binding * rec_binding)
      (* i = e *)
    | `RbEq  of (loc * ident * expr)
    | `Ant of (loc * string) (* $s$ *) ]
  and module_binding =
    [= `MbNil of loc
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | `MbAnd of (loc * module_binding * module_binding)
      (* s : mt = me *)
    | `MbColEq  of (loc * string * module_type * module_expr)
      (* s : mt *)
    | `MbCol  of (loc * string * module_type)
    | `Ant of (loc * string) (* $s$ *) ]
  and match_case =
    [= `McNil of loc
      (* a | a *)
    | `McOr of (loc * match_case * match_case)
      (* p (when e)? -> e *)
    | `McArr of (loc * patt * expr * expr)
    (* | `McArrow of loc and patt and option expr and expr (\* FIXME *\) *)
    | `Ant of (loc * string) (* $s$ *) ]
  and module_expr =
    [= `MeNil of loc
      (* i *)
    | `MeId  of (loc * ident)
      (* me me *)
    | `MeApp of (loc * module_expr * module_expr)
      (* functor (s : mt) -> me *)
    | `MeFun of (loc * string * module_type * module_expr)
      (* struct st end *)
    | `MeStr of (loc * str_item)
      (* (me : mt) *)
    | `MeTyc of (loc * module_expr * module_type)
      (* (value e) *)
      (* (value e : S) which is represented as (value (e : S)) *)
    | `MePkg of (loc * expr)
    | `Ant of (loc * string) (* $s$ *) ]
  and str_item =
    [= `StNil of loc
      (* class cice *)
    | `StCls of (loc * class_expr)
      (* class type cict *)
    | `StClt of (loc * class_type)
      (* st ; st *)
    | `StSem of (loc * str_item * str_item)
      (* # s or # s e *)
    | `StDir of (loc * string * expr)
      (* exception t or exception t = i *)
    | `StExc of ( loc * ctyp * meta_option(*FIXME*) ident)
      (* e *)
    | `StExp of (loc * expr)
      (* external s : t = s ... s *)
    | `StExt of (loc * string * ctyp * meta_list string)
      (* include me *)
    | `StInc of (loc * module_expr)
      (* module s = me *)
    | `StMod of (loc * string * module_expr)
      (* module rec mb *)
    | `StRecMod of (loc * module_binding)
      (* module type s = mt *)
    | `StMty of (loc * string * module_type)
      (* open i *)
    | `StOpn of (loc * ident)
      (* type t *)
    | `StTyp of (loc * ctyp)
      (* value (rec)? bi *)
    | `StVal of (loc * rec_flag * binding)
    | `Ant of (loc * string) (* $s$ *) ]
  and class_type =
    [= `CtNil of loc
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
    | `Ant of (loc * string) ]
  and class_sig_item =
    [= `CgNil of loc
      (* type t = t *)
    | `CgCtr of (loc * ctyp * ctyp)
      (* csg ; csg *)
    | `CgSem of (loc * class_sig_item * class_sig_item)
      (* inherit ct *)
    | `CgInh of (loc * class_type)
      (* method s : t or method private s : t *)
    | `CgMth of (loc * string * private_flag * ctyp)
      (* value (virtual)? (mutable)? s : t *)
    | `CgVal of (loc * string * mutable_flag * virtual_flag * ctyp)
      (* method virtual (private)? s : t *)
    | `CgVir of (loc * string * private_flag * ctyp)
    | `Ant of (loc * string) (* $s$ *) ]
  and class_expr =
    [= `CeNil of loc
      (* ce e *)
    | `CeApp of (loc * class_expr * expr)
      (* (virtual)? i ([ t ])? *)
    | `CeCon of (loc * virtual_flag * ident * ctyp)
      (* fun p -> ce *)
    | `CeFun of (loc * patt * class_expr)
      (* let (rec)? bi in ce *)
    | `CeLet of (loc * rec_flag * binding * class_expr)
      (* object ((p))? (cst)? end *)
    | `CeStr of (loc * patt * class_str_item)
      (* ce : ct *)
    | `CeTyc of (loc * class_expr * class_type)
      (* ce and ce *)
    | `CeAnd of (loc * class_expr * class_expr)
      (* ce = ce *)
    | `CeEq  of (loc * class_expr * class_expr)
      (* $s$ *)
    | `Ant of (loc * string) ]
  and class_str_item =
    [= `CrNil of loc
      (* cst ; cst *)
    | `CrSem of (loc * class_str_item * class_str_item)
      (* type t = t *)
    | `CrCtr of (loc * ctyp * ctyp)
      (* inherit(!)? ce (as s)? *)
    | `CrInh of (loc * override_flag * class_expr * string)
      (* initializer e *)
    | `CrIni of (loc * expr)
      (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    | `CrMth of (loc * string * override_flag * private_flag * expr * ctyp)
      (* value(!)? (mutable)? s = e *)
    | `CrVal of (loc * string * override_flag * mutable_flag * expr)
      (* method virtual (private)? s : t *)
    | `CrVir of (loc * string * private_flag * ctyp)
      (* value virtual (mutable)? s : t *)
    | `CrVvr of (loc * string * mutable_flag * ctyp)
    | `Ant of (loc * string) (* $s$ *) ]; 


let loc_of_ctyp : ctyp -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));

let loc_of_patt : patt -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));
  
let loc_of_expr : expr -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));
      

  
let loc_of_module_type : module_type -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_module_expr : module_expr -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));
    
let loc_of_sig_item : sig_item -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));



let loc_of_str_item : str_item -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_class_type : class_type -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_class_sig_item : class_sig_item -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_class_expr : class_expr -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_class_str_item : class_str_item -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_with_constr : with_constr -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_binding : binding -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_rec_binding : rec_binding -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_module_binding : module_binding -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_match_case : match_case -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));


let loc_of_ident : ident -> FanLoc.t =
  fun x -> Obj.(magic ( field (field (repr x) 1) 0));



let safe_string_escaped s =
  if String.length s > 2 && s.[0] = '\\' && s.[1] = '$' then s
  else String.escaped s;
    
