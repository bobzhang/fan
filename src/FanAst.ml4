open FanUtil;
include Ast;
(* Note: when you modify these types you must increment
   ast magic numbers defined in FanConfig.ml. *)
module type META_LOC = sig
      (** The first location is where to put the returned pattern.
          Generally it's _loc to match with {:patt| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> Ast.patt;
      (** The first location is where to put the returned expression.
          Generally it's _loc to match with {:expr| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> Ast.expr;
end;

(** Signature for OCaml syntax trees. *) (*
    This signature is an extension of {!Ast}
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
(* open Ast;   *)
open StdLib;  
{:fans|keep off; <++ "MetaExpr", "MetaPatt", "Map", "Fold";|};



  
{:ocaml|
   type loc = FanLoc.t
   and meta_bool =
    [ `BTrue
    | `BFalse
    | `BAnt of string ]
   and rec_flag =
    [ `ReRecursive
    | `ReNil
    | `ReAnt of string ]
   and direction_flag =
    [ `DiTo
    | `DiDownto
    | `DiAnt of string ]
   and mutable_flag =
    [ `MuMutable
    | `MuNil
    | `MuAnt of string ]
   and private_flag =
    [ `PrPrivate
    | `PrNil
    | `PrAnt of string ]
   and virtual_flag =
    [ `ViVirtual
    | `ViNil
    | `ViAnt of string ]
   and override_flag =
    [ `OvOverride
    | `OvNil
    | `OvAnt of string ]
   and row_var_flag =
    [ `RvRowVar
    | `RvNil
    | `RvAnt of string ]
   and meta_option 'a =
    [ `ONone
    | `OSome of 'a
    | `OAnt of string ]
   and meta_list 'a =
    [ `LNil
    | `LCons of 'a and meta_list 'a
    | `LAnt of string ]
   and ident =
    [ `IdAcc of loc and ident and ident (* i . i *)
    | `IdApp of loc and ident and ident (* i i *)
    | `IdLid of loc and string (* foo *)
    | `IdUid of loc and string (* Bar *)
    | `IdAnt of loc and string (* $s$ *) ]
   and ctyp =
    [ `TyNil of loc
    | `TyAli of loc and ctyp and ctyp (* t as t *) (* list 'a as 'a *)
    | `TyAny of loc (* _ *)
    | `TyApp of loc and ctyp and ctyp (* t t *) (* list 'a *)
    | `TyArr of loc and ctyp and ctyp (* t -> t *) (* int -> string *)
    | `TyCls of loc and ident (* #i *) (* #point *)
    | `TyLab of loc and string and ctyp (* ~s:t *)
    | `TyId  of loc and ident (* i *) (* Lazy.t *)
    | `TyMan of loc and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *)
      (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
      (* first is the name, second is type parameters and third is the core, forth is constraints *)
    | `TyDcl of loc and string and list ctyp and ctyp and list (ctyp * ctyp)
      (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
    | `TyObj of loc and ctyp and row_var_flag
    | `TyOlb of loc and string and ctyp (* ?s:t *)
    | `TyPol of loc and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *)
    | `TyTypePol of loc and ctyp and ctyp (* type t . t *) (* type a . list a -> a *)
    | `TyQuo of loc and string (* 's *)
    | `TyQuP of loc and string (* +'s *)
    | `TyQuM of loc and string (* -'s *)
    | `TyAnP of loc (* +_ *)
    | `TyAnM of loc (* -_ *)
    | `TyVrn of loc and string (* `s *)
    | `TyRec of loc and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *)
    | `TyCol of loc and ctyp and ctyp (* t : t *) (* FIXME be more precise *)
    | `TySem of loc and ctyp and ctyp (* t; t *)
    | `TyCom of loc and ctyp and ctyp (* t, t *)
    | `TySum of loc and ctyp (* [ t ] *) (* [ A of int and string | B ] *)
    | `TyOf  of loc and ctyp and ctyp (* t of t *) (* A of int *)
    | `TyAnd of loc and ctyp and ctyp (* t and t *)
    | `TyOr  of loc and ctyp and ctyp (* t | t *)
    | `TyPrv of loc and ctyp (* private t *)
    | `TyMut of loc and ctyp (* mutable t *)
    | `TyTup of loc and ctyp (* ( t ) *) (* (int * string) *)
    | `TySta of loc and ctyp and ctyp (* t * t *)
    | `TyVrnEq of loc and ctyp (* [ = t ] *)
    | `TyVrnSup of loc and ctyp (* [ > t ] *)
    | `TyVrnInf of loc and ctyp (* [ < t ] *)
    | `TyVrnInfSup of loc and ctyp and ctyp (* [ < t > t ] *)
    | `TyAmp of loc and ctyp and ctyp (* t & t *)
    | `TyOfAmp of loc and ctyp and ctyp (* t of & t *)
    | `TyPkg of loc and module_type (* (module S) *)
    | `TyAnt of loc and string (* $s$ *)
    ]
   and patt =
    [ `PaNil of loc
    | `PaId  of loc and ident (* i *)
    | `PaAli of loc and patt and patt (* p as p *) (* (Node x y as n) *)
    | `PaAnt of loc and string (* $s$ *)
    | `PaAny of loc (* _ *)
    | `PaApp of loc and patt and patt (* p p *) (* fun x y -> *)
    | `PaArr of loc and patt (* [| p |] *)
    | `PaCom of loc and patt and patt (* p, p *)
    | `PaSem of loc and patt and patt (* p; p *)
    | `PaChr of loc and string (* c *) (* 'x' *)
    | `PaInt of loc and string
    | `PaInt32 of loc and string
    | `PaInt64 of loc and string
    | `PaNativeInt of loc and string
    | `PaFlo of loc and string
    | `PaLab of loc and string and patt (* ~s or ~s:(p) *)
    (* ?s or ?s:(p) *)
    | `PaOlb of loc and string and patt
    (* ?s:(p = e) or ?(p = e) *)
    | `PaOlbi of loc and string and patt and expr
    | `PaOrp of loc and patt and patt (* p | p *)
    | `PaRng of loc and patt and patt (* p .. p *)
    | `PaRec of loc and patt (* { p } *)
    | `PaEq  of loc and ident and patt (* i = p *)
    | `PaStr of loc and string (* s *)
    | `PaTup of loc and patt (* ( p ) *)
    | `PaTyc of loc and patt and ctyp (* (p : t) *)
    | `PaTyp of loc and ident (* #i *)
    | `PaVrn of loc and string (* `s *)
    | `PaLaz of loc and patt (* lazy p *)
    | `PaMod of loc and string (* (module M) *) ]
  and expr =
    [ `ExNil of loc
    | `ExId  of loc and ident (* i *)
    | `ExAcc of loc and expr and expr (* e.e *)
    | `ExAnt of loc and string (* $s$ *)
    | `ExApp of loc and expr and expr (* e e *)
    | `ExAre of loc and expr and expr (* e.(e) *)
    | `ExArr of loc and expr (* [| e |] *)
    | `ExSem of loc and expr and expr (* e; e *)
    | `ExAsf of loc (* assert False *)
    | `ExAsr of loc and expr (* assert e *)
    | `ExAss of loc and expr and expr (* e := e *)
    | `ExChr of loc and string (* 'c' *)
    | `ExCoe of loc and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *)
    | ExFlo of loc and string (* 3.14 *)
      (* for s = e to/downto e begin  e  end *)
    | `ExFor of loc and string and expr and expr and direction_flag and expr
    | `ExFun of loc and match_case (* fun [ mc ] *)
    | `ExIfe of loc and expr and expr and expr (* if e then e else e *)
    | `ExInt of loc and string (* 42 *)
    | `ExInt32 of loc and string
    | `ExInt64 of loc and string
    | `ExNativeInt of loc and string
    | `ExLab of loc and string and expr (* ~s or ~s:e *)
    | `ExLaz of loc and expr (* lazy e *)
      (* let b in e or let rec b in e *)
    | `ExLet of loc and rec_flag and binding and expr
      (* let module s = me in e *)
    | `ExLmd of loc and string and module_expr and expr
      (* match e with [ mc ] *)
    | `ExMat of loc and expr and match_case
      (* new i *)
    | `ExNew of loc and ident
      (* object ((p))? (cst)? end *)
    | `ExObj of loc and patt and class_str_item
      (* ?s or ?s:e *)
    | `ExOlb of loc and string and expr
      (* {< rb >} *)
    | `ExOvr of loc and rec_binding
      (* { rb } or { (e) with rb } *)
    | `ExRec of loc and rec_binding and expr
      (* begin  e  end *)
    | `ExSeq of loc and expr
      (* e#s *)
    | `ExSnd of loc and expr and string
      (* e.[e] *)
    | `ExSte of loc and expr and expr
      (* s *) (* "foo" *)
    | `ExStr of loc and string
      (* try e with [ mc ] *)
    | `ExTry of loc and expr and match_case
      (* (e) *)
    | `ExTup of loc and expr
      (* e, e *)
    | `ExCom of loc and expr and expr
      (* (e : t) *)
    | `ExTyc of loc and expr and ctyp
      (* `s *)
    | `ExVrn of loc and string
      (* while e begin  e  end *)
    | `ExWhi of loc and expr and expr
      (* let open i in e *)
    | `ExOpI of loc and ident and expr
      (* fun (type t) -> e *)
      (* let f x (type t) y z = e *)
    | `ExFUN of loc and string and expr
      (* (module ME : S) which is represented as (module (ME : S)) *)
    | `ExPkg of loc and module_expr ]
  and module_type =
    [ `MtNil of loc
      (* i *) (* A.B.C *)
    | `MtId  of loc and ident
      (* functor (s : mt) -> mt *)
    | `MtFun of loc and string and module_type and module_type
      (* 's *)
    | `MtQuo of loc and string
      (* sig sg end *)
    | `MtSig of loc and sig_item
      (* mt with wc *)
    | `MtWit of loc and module_type and with_constr
      (* module type of m *)
    | `MtOf of loc and module_expr
    | `MtAnt of loc and string (* $s$ *) ]
  and sig_item =
    [ `SgNil of loc
      (* class cict *)
    | `SgCls of loc and class_type
      (* class type cict *)
    | `SgClt of loc and class_type
      (* sg ; sg *)
    | `SgSem of loc and sig_item and sig_item
      (* # s or # s e *)
    | `SgDir of loc and string and expr (* the first field does not have location :-( *)
      (* exception t *)
    | `SgExc of loc and ctyp
      (* external s : t = s ... s *)
    | `SgExt of loc and string and ctyp and meta_list string
      (* include mt *)
    | `SgInc of loc and module_type
      (* module s : mt *)
    | `SgMod of loc and string and module_type
      (* module rec mb *)
    | `SgRecMod of loc and module_binding
      (* module type s = mt *)
    | `SgMty of loc and string and module_type
      (* open i *)
    | `SgOpn of loc and ident
      (* type t *)
    | `SgTyp of loc and ctyp
      (* value s : t *)
    | `SgVal of loc and string and ctyp
    | `SgAnt of loc and string (* $s$ *) ]
  and with_constr =
    [ `WcNil of loc
      (* type t = t *)
    | `WcTyp of loc and ctyp and ctyp
      (* module i = i *)
    | `WcMod of loc and ident and ident
      (* type t := t *)
    | `WcTyS of loc and ctyp and ctyp
      (* module i := i *)
    | `WcMoS of loc and ident and ident
      (* wc and wc *)
    | `WcAnd of loc and with_constr and with_constr
    | `WcAnt of loc and string (* $s$ *) ]
  and binding =
    [ `BiNil of loc
      (* bi and bi *) (* let a = 42 and c = 43 *)
    | `BiAnd of loc and binding and binding
      (* p = e *) (* let patt = expr *)
    | `BiEq  of loc and patt and expr
    | `BiAnt of loc and string (* $s$ *) ]
  and rec_binding =
    [ `RbNil of loc
      (* rb ; rb *)
    | `RbSem of loc and rec_binding and rec_binding
      (* i = e *)
    | `RbEq  of loc and ident and expr
    | `RbAnt of loc and string (* $s$ *) ]
  and module_binding =
    [ `MbNil of loc
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | `MbAnd of loc and module_binding and module_binding
      (* s : mt = me *)
    | `MbColEq  of loc and string and module_type and module_expr
      (* s : mt *)
    | `MbCol  of loc and string and module_type
    | `MbAnt of loc and string (* $s$ *) ]
  and match_case =
    [ `McNil of loc
      (* a | a *)
    | `McOr of loc and match_case and match_case
      (* p (when e)? -> e *)
    | `McArr of loc and patt and expr and expr
    (* | McArrow of loc and patt and option expr and expr (\* FIXME *\) *)
    | `McAnt of loc and string (* $s$ *) ]
  and module_expr =
    [ `MeNil of loc
      (* i *)
    | `MeId  of loc and ident
      (* me me *)
    | `MeApp of loc and module_expr and module_expr
      (* functor (s : mt) -> me *)
    | `MeFun of loc and string and module_type and module_expr
      (* struct st end *)
    | `MeStr of loc and str_item
      (* (me : mt) *)
    | `MeTyc of loc and module_expr and module_type
      (* (value e) *)
      (* (value e : S) which is represented as (value (e : S)) *)
    | `MePkg of loc and expr
    | `MeAnt of loc and string (* $s$ *) ]
  and str_item =
    [ `StNil of loc
      (* class cice *)
    | `StCls of loc and class_expr
      (* class type cict *)
    | `StClt of loc and class_type
      (* st ; st *)
    | `StSem of loc and str_item and str_item
      (* # s or # s e *)
    | `StDir of loc and string and expr
      (* exception t or exception t = i *)
    | `StExc of loc and ctyp and meta_option(*FIXME*) ident
      (* e *)
    | `StExp of loc and expr
      (* external s : t = s ... s *)
    | `StExt of loc and string and ctyp and meta_list string
      (* include me *)
    | `StInc of loc and module_expr
      (* module s = me *)
    | `StMod of loc and string and module_expr
      (* module rec mb *)
    | `StRecMod of loc and module_binding
      (* module type s = mt *)
    | `StMty of loc and string and module_type
      (* open i *)
    | `StOpn of loc and ident
      (* type t *)
    | `StTyp of loc and ctyp
      (* value (rec)? bi *)
    | `StVal of loc and rec_flag and binding
    | `StAnt of loc and string (* $s$ *) ]
  and class_type =
    [ `CtNil of loc
      (* (virtual)? i ([ t ])? *)
    | `CtCon of loc and virtual_flag and ident and ctyp
      (* [t] -> ct *)
    | `CtFun of loc and ctyp and class_type
      (* object ((t))? (csg)? end *)
    | `CtSig of loc and ctyp and class_sig_item
      (* ct and ct *)
    | `CtAnd of loc and class_type and class_type
      (* ct : ct *)
    | `CtCol of loc and class_type and class_type
      (* ct = ct *)
    | `CtEq  of loc and class_type and class_type
      (* $s$ *)
    | `CtAnt of loc and string ]
  and class_sig_item =
    [ `CgNil of loc
      (* type t = t *)
    | `CgCtr of loc and ctyp and ctyp
      (* csg ; csg *)
    | `CgSem of loc and class_sig_item and class_sig_item
      (* inherit ct *)
    | `CgInh of loc and class_type
      (* method s : t or method private s : t *)
    | `CgMth of loc and string and private_flag and ctyp
      (* value (virtual)? (mutable)? s : t *)
    | `CgVal of loc and string and mutable_flag and virtual_flag and ctyp
      (* method virtual (private)? s : t *)
    | `CgVir of loc and string and private_flag and ctyp
    | `CgAnt of loc and string (* $s$ *) ]
  and class_expr =
    [ `CeNil of loc
      (* ce e *)
    | `CeApp of loc and class_expr and expr
      (* (virtual)? i ([ t ])? *)
    | `CeCon of loc and virtual_flag and ident and ctyp
      (* fun p -> ce *)
    | `CeFun of loc and patt and class_expr
      (* let (rec)? bi in ce *)
    | `CeLet of loc and rec_flag and binding and class_expr
      (* object ((p))? (cst)? end *)
    | `CeStr of loc and patt and class_str_item
      (* ce : ct *)
    | `CeTyc of loc and class_expr and class_type
      (* ce and ce *)
    | `CeAnd of loc and class_expr and class_expr
      (* ce = ce *)
    | `CeEq  of loc and class_expr and class_expr
      (* $s$ *)
    | `CeAnt of loc and string ]
  and class_str_item =
    [ `CrNil of loc
      (* cst ; cst *)
    | `CrSem of loc and class_str_item and class_str_item
      (* type t = t *)
    | `CrCtr of loc and ctyp and ctyp
      (* inherit(!)? ce (as s)? *)
    | `CrInh of loc and override_flag and class_expr and string
      (* initializer e *)
    | `CrIni of loc and expr
      (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    | `CrMth of loc and string and override_flag and private_flag and expr and ctyp
      (* value(!)? (mutable)? s = e *)
    | `CrVal of loc and string and override_flag and mutable_flag and expr
      (* method virtual (private)? s : t *)
    | `CrVir of loc and string and private_flag and ctyp
      (* value virtual (mutable)? s : t *)
    | `CrVvr of loc and string and mutable_flag and ctyp
    | `CrAnt of loc and string (* $s$ *) ];
  |};



(*
  {[
  is_module_longident {:ident| A.B.t |} ; 
  - : bool = false
  is_module_longident {:ident| A.B |} ; 
  - : bool = true
  ]}
 *)
let rec is_module_longident = fun
  [ {:ident| $_.$i |} -> is_module_longident i
  | {:ident| ($i1 $i2) |} ->
      is_module_longident i1 && is_module_longident i2
  | {:ident| $uid:_ |} -> true
  | _ -> false ];

let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =  fun
    [ {:expr@_loc| $e1 $e2 |} -> {:ident| ( $(self e1) $(self e2)) |}
    | {:expr@_loc| $e1.$e2 |} -> {:ident| $(self e1).$(self e2) |}
    | {:expr| $lid:_ |} -> error ()
    | {:expr| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
  fun
    [ {:expr| $id:i |} -> i
    | {:expr| $_ $_ |} -> error ()
    | t -> self t ];

(*
  {[
  ident_of_ctyp {:ctyp| list int |} ; ;
  Exception: Invalid_argument "ident_of_ctyp: this type is not an identifier".

  ident_of_ctyp {:ctyp| A.B |} ; ;     
  - : ident =
  `IdAcc (, `IdUid (, "A"),
  `IdUid (, "B"))

  {:ctyp| A.B |} ; ;
  - : ctyp =
  `TyId (, `IdAcc (, `IdUid (, "A"), `IdUid (, "B")))

  ident_of_ctyp {:ctyp| (A B).t |} ; ;
  - : ident =
  `IdAcc (, `IdApp (, `IdUid (, "A"), `IdUid (, "B")), `IdLid (, "t"))  ]}
 *)
let ident_of_ctyp =
  let error () =
    invalid_arg "ident_of_ctyp: this type is not an identifier" in
  let rec self =   fun
    [ {:ctyp@_loc| $t1 $t2 |} -> {:ident| ( $(self t1) $(self t2) ) |}
    | {:ctyp| $lid:_ |} -> error ()
    | {:ctyp| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
    fun
    [ {:ctyp| $id:i |} -> i
    | t -> self t ];

let ident_of_patt =
  let error () =
    invalid_arg "ident_of_patt: this pattern is not an identifier" in
  let rec self = fun 
    [ {:patt@_loc| $p1 $p2 |}
      -> {:ident| ( $(self p1) $(self p2) ) |}
    | {:patt| $lid:_ |} -> error ()
    | {:patt| $id:i |} -> if is_module_longident i then i else error ()
    | _ -> error () ] in
    fun
    [ {:patt| $id:i |} -> i
    | p -> self p ];


let rec is_irrefut_patt = with "patt"
    fun
    [ {| $lid:_ |} -> true
    | {| () |} -> true
    | {| _ |} -> true
    | {||} -> true (* why not *)
    | {| ($x as $y) |} -> is_irrefut_patt x && is_irrefut_patt y
    | {| { $p } |} -> is_irrefut_patt p
    | {| $_ = $p |} -> is_irrefut_patt p
    | {| $p1; $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| $p1, $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| $p1 | $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2 (* could be more fine grained *)
    | {| $p1 $p2 |} -> is_irrefut_patt p1 && is_irrefut_patt p2
    | {| ($p : $_) |} -> is_irrefut_patt p
    | {| ($tup:pl) |} -> is_irrefut_patt pl
    | {| ? $_ |} -> true
    | {| ? $_ : ($_ ) |} -> (* is_irrefut_patt p *) true
    | {| ? $_ : ($_ = $_) |} -> (* is_irrefut_patt p *) true
    | {| ~ $_ |} -> true
    | {| ~ $_ : $p |} -> is_irrefut_patt p
    | {| lazy $p |} -> is_irrefut_patt p
    | {| $id:_ |} -> false (* here one need to know the arity of constructors *)
    | {| (module $_) |} -> true
    | {| `$_ |} | {| $str:_ |} | {| $_ .. $_ |} |
      {| $flo:_ |} | {| $nativeint:_ |} | {| $int64:_ |} |
      {| $int32:_ |} | {| $int:_ |} | {| $chr:_ |} |
      {| #$_ |} | {| [| $_ |] |} | {| $anti:_ |} -> false
    ];      
      

let rec is_constructor =  fun
    [ {:ident| $_.$i |} -> is_constructor i
    | {:ident| $uid:_ |} -> true
    | {:ident| $lid:_ |} | {:ident| ($_ $_) |} -> false
    | {:ident| $anti:_ |} -> assert false ];

let is_patt_constructor = fun
    [ {:patt| $id:i |} -> is_constructor i
    | {:patt| `$_ |} -> true
    | _ -> false ];

let rec is_expr_constructor = fun
    [ {:expr| $id:i |} -> is_constructor i
    | {:expr| $e1.$e2 |} -> is_expr_constructor e1 && is_expr_constructor e2
    | {:expr| `$_ |} -> true
    | _ -> false ];

let ghost = FanLoc.ghost ; (* to refine *)
let rec tyOr_of_list = fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t | $(tyOr_of_list ts) |} ];

let rec tyAnd_of_list = fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t and $(tyAnd_of_list ts) |} ];

let rec tySem_of_list = fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t ; $(tySem_of_list ts) |} ];

let rec tyCom_of_list = fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t, $(tyCom_of_list ts) |} ];

let rec tyAmp_of_list =  fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t & $(tyAmp_of_list ts) |} ];

let rec tySta_of_list =  fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_ctyp t in {:ctyp| $t * $(tySta_of_list ts) |} ];

(* LA *)  
let  tyApp_of_list = fun
    [ [] -> {:ctyp@ghost||}
    | [t] -> t
    | [t::ts] ->

        List.fold_left
          (fun x y -> let _loc = loc_of_ctyp  x in {:ctyp| $x $y |}) t ts];

        (* let _loc = loc_of_ctyp t in {:ctyp| $t  $(tyApp_of_list ts) |} ]; *)
(* LA *)
let tyVarApp_of_list (_loc,ls)=
  let  aux = fun 
    [ [] -> {:ctyp@ghost||}
    | [t] -> {:ctyp| '$t |}
    | [t::ts] ->
        List.fold_left (fun x y -> {:ctyp| $x '$y |}) {:ctyp| '$t |} ts ] in
  aux ls;
  
  
let rec stSem_of_list = fun
    [ [] -> {:str_item@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_str_item t in {:str_item| $t ; $(stSem_of_list ts) |} ];
  (* FIXME introduces Nil here *)    
let rec sgSem_of_list = fun
    [ [] -> {:sig_item@ghost||}
    | [t] -> t
    | [t::ts] ->
        let _loc = loc_of_sig_item t in {:sig_item| $t ; $(sgSem_of_list ts) |} ];

let rec biAnd_of_list =  fun
    [ [] -> {:binding@ghost||}
    | [b] -> b
    | [b::bs] ->
        let _loc = loc_of_binding b in {:binding| $b and $(biAnd_of_list bs) |} ];

let rec rbSem_of_list =  fun
    [ [] -> {:rec_binding@ghost||}
    | [b] -> b
    | [b::bs] ->
        let _loc = loc_of_rec_binding b in
        {:rec_binding| $b; $(rbSem_of_list bs) |} ];

let rec wcAnd_of_list = fun
    [ [] -> {:with_constr@ghost||}
    | [w] -> w
    | [w::ws] ->
        let _loc = loc_of_with_constr w in
        {:with_constr| $w and $(wcAnd_of_list ws) |} ];

let rec idAcc_of_list = fun
    [ [] -> assert false
    | [i] -> i
    | [i::is] ->
        let _loc = loc_of_ident i in
        {:ident| $i . $(idAcc_of_list is) |} ];

let rec idApp_of_list =  fun
    [ [] -> assert false
    | [i] -> i
    | [i::is] ->
        let _loc = loc_of_ident i in
        {:ident| ($i $(idApp_of_list is)) |} ];

let rec mcOr_of_list = fun
    [ [] -> {:match_case@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_match_case x in
        {:match_case| $x | $(mcOr_of_list xs) |} ];
  

let rec mbAnd_of_list = fun
    [ [] -> {:module_binding@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_module_binding x in
        {:module_binding| $x and $(mbAnd_of_list xs) |} ];

let rec meApp_of_list = fun
    [ [] -> assert false
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_module_expr x in
        {:module_expr| $x $(meApp_of_list xs) |} ];

let rec ceAnd_of_list =  fun
    [ [] -> {:class_expr@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_expr x in
        {:class_expr| $x and $(ceAnd_of_list xs) |} ];

let rec ctAnd_of_list = fun
    [ [] -> {:class_type@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_type x in
        {:class_type| $x and $(ctAnd_of_list xs) |} ];

let rec cgSem_of_list = fun
    [ [] -> {:class_sig_item@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_sig_item x in
        {:class_sig_item| $x; $(cgSem_of_list xs) |} ];

let rec crSem_of_list = fun
    [ [] -> {:class_str_item@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_class_str_item x in
        {:class_str_item| $x; $(crSem_of_list xs) |} ];

let rec paSem_of_list = fun
    [ [] -> {:patt@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_patt x in
        {:patt| $x; $(paSem_of_list xs) |} ];

let rec paCom_of_list =  fun
    [ [] -> {:patt@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_patt x in
        {:patt| $x, $(paCom_of_list xs) |} ];

let rec exSem_of_list =  fun
    [ [] -> {:expr@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_expr x in
        {:expr| $x; $(exSem_of_list xs) |} ];

let rec exCom_of_list =  fun
    [ [] -> {:expr@ghost||}
    | [x] -> x
    | [x::xs] ->
        let _loc = loc_of_expr x in
        {:expr| $x, $(exCom_of_list xs) |} ];
(* LA *)  
let  exApp_of_list = fun
    [ [] -> {:expr@ghost||}
    | [t] -> t
    | [t::ts] ->
        List.fold_left
          (fun x y -> let _loc = loc_of_expr  x in {:expr| $x $y |}) t ts];

let ty_of_stl = fun
    [ (_loc, s, []) -> {:ctyp| $uid:s |}
    | (_loc, s, tl) -> {:ctyp| $uid:s of $(tyAnd_of_list tl) |} ];

let ty_of_sbt = fun
    [ (_loc, s, true, t) -> {:ctyp| $lid:s : mutable $t |}
    | (_loc, s, false, t) -> {:ctyp| $lid:s : $t |} ];

let bi_of_pe (p, e) = let _loc = loc_of_patt p in {:binding| $p = $e |};
let sum_type_of_list l = tyOr_of_list (List.map ty_of_stl l);
let record_type_of_list l = tySem_of_list (List.map ty_of_sbt l);
let binding_of_pel l = biAnd_of_list (List.map bi_of_pe l);

let rec pel_of_binding =  fun
    [ {:binding| $b1 and $b2 |} -> pel_of_binding b1 @ pel_of_binding b2
    | {:binding| $p = $e |} -> [(p, e)]
    | _ -> assert false ];

let rec list_of_binding x acc =
    match x with
    [ {:binding| $b1 and $b2 |} ->
         list_of_binding b1 (list_of_binding b2 acc)
    | t -> [t :: acc] ];

let rec list_of_rec_binding x acc =  match x with
    [ {:rec_binding| $b1; $b2 |} ->
         list_of_rec_binding b1 (list_of_rec_binding b2 acc)
    | t -> [t :: acc] ];

let rec list_of_with_constr x acc = match x with
  [ {:with_constr| $w1 and $w2 |} ->
    list_of_with_constr w1 (list_of_with_constr w2 acc)
  | t -> [t :: acc] ];

let rec list_of_ctyp x acc =  match x with
  [ {:ctyp||} -> acc
  | {:ctyp| $x & $y |} | {:ctyp| $x, $y |} |
    {:ctyp| $x * $y |} | {:ctyp| $x; $y |} |
    {:ctyp| $x and $y |} | {:ctyp| $x | $y |} ->
        list_of_ctyp x (list_of_ctyp y acc)
  | x -> [x :: acc] ];

let rec list_of_patt x acc = match x with
  [ {:patt||} -> acc
  | {:patt| $x, $y |} | {:patt| $x; $y |} ->
        list_of_patt x (list_of_patt y acc)
  | x -> [x :: acc] ];

let rec list_of_expr x acc =  match x with
  [ {:expr||} -> acc
  | {:expr| $x, $y |} | {:expr| $x; $y |} ->
      list_of_expr x (list_of_expr y acc)
  | x -> [x :: acc] ];

let rec list_of_str_item x acc = match x with
  [ {:str_item||} -> acc
  | {:str_item| $x; $y |} ->
      list_of_str_item x (list_of_str_item y acc)
  | x -> [x :: acc] ];

let rec list_of_sig_item x acc = match x with
  [ {:sig_item||} -> acc
  | {:sig_item| $x; $y |} ->
        list_of_sig_item x (list_of_sig_item y acc)
  | x -> [x :: acc] ];

let rec list_of_class_sig_item x acc =  match x with
  [ {:class_sig_item||} -> acc
  | {:class_sig_item| $x; $y |} ->
      list_of_class_sig_item x (list_of_class_sig_item y acc)
  | x -> [x :: acc] ];

let rec list_of_class_str_item x acc =  match x with
  [ {:class_str_item||} -> acc
  | {:class_str_item| $x; $y |} ->
      list_of_class_str_item x (list_of_class_str_item y acc)
  | x -> [x :: acc] ];

let rec list_of_class_type x acc =  match x with
  [ {:class_type| $x and $y |} ->
    list_of_class_type x (list_of_class_type y acc)
  | x -> [x :: acc] ];

let rec list_of_class_expr x acc = match x with
  [ {:class_expr| $x and $y |} ->
    list_of_class_expr x (list_of_class_expr y acc)
  | x -> [x :: acc] ];

let rec list_of_module_expr x acc = match x with
  [ {:module_expr| $x $y |} ->
    list_of_module_expr x (list_of_module_expr y acc)
  | x -> [x :: acc] ];

let rec list_of_match_case x acc =  match x with
  [ {:match_case||} -> acc
  | {:match_case| $x | $y |} ->
      list_of_match_case x (list_of_match_case y acc)
  | x -> [x :: acc] ];

let rec list_of_ident x acc = match x with
    [ {:ident| $x . $y |} | {:ident| ($x $y) |} ->
      list_of_ident x (list_of_ident y acc)
    | x -> [x :: acc] ];

let rec list_of_module_binding x acc = match x with
  [ {:module_binding| $x and $y |} ->
    list_of_module_binding x (list_of_module_binding y acc)
  | x -> [x :: acc] ];
  
let map_expr f = object
  inherit map as super;
  method! expr x = f (super#expr x);
end;
let map_patt f = object
  inherit map as super;
  method! patt x = f (super#patt x);
end;
let map_ctyp f = object
  inherit map as super;
  method! ctyp x = f (super#ctyp x);
end;
let map_str_item f = object
  inherit map as super;
  method! str_item x = f (super#str_item x);
end;
let map_sig_item f = object
  inherit map as super;
  method! sig_item x = f (super#sig_item x);
end;
let map_loc f = object
  inherit map as super;
  method! loc x = f (super#loc x);
end;


class clean_ast = object
    
  inherit map as super;
  method! with_constr wc =
    with "with_constr"
    match super#with_constr wc with
    [ {| $({@_l||})  and $wc |} |
      {| $wc and $({@_l||} ) |} -> wc
    | wc -> wc ];
  method! expr e =
    with "expr"
    match super#expr e with
    [ {| let $rec:_ $({:binding@_l||}) in $e |} |
      {| { ($e) with $({:rec_binding@_l||})  } |} |
      {| $({@_l||} ), $e |} |
      {| $e, $({@_l||} ) |} |
      {| $({@_l||}); $e |} |
      {| $e; $({@_l||} ) |} -> e
    | e -> e ];
  method! patt p =
    with "patt"
    match super#patt p with
    [ {| ( $p as $({@_l||} ) ) |} |
      {| $({@_l||}) | $p |} |
      {| $p | $({@_l||} ) |} |
      {| $({@_l||} ), $p |} |
      {| $p, $({@_l||} ) |} |
      {| $({@_l||} ); $p |} |
      {| $p; $({@_l||} ) |} -> p
    | p -> p ];
  method! match_case mc =
    with "match_case"
    match super#match_case mc with
    [ {| $({@_l||} ) | $mc |} |
      {| $mc | $({@_l||} ) |} -> mc
    | mc -> mc ];
  method! binding bi =
    with "binding"
    match super#binding bi with
    [ {| $({@_l||} ) and $bi |} |
      {| $bi and $({@_l||} ) |} -> bi
    | bi -> bi ];
  method! rec_binding rb =
    with "rec_binding"
    match super#rec_binding rb with
    [ {| $({@_l||} ) ; $bi |} | {| $bi ; $({@_l||} ) |} -> bi
    | bi -> bi ];

  method! module_binding mb =
    with "module_binding"
    match super#module_binding mb with
    [ {| $({@_l||} ) and $mb |} |
      {| $mb and $({@_l||} ) |} -> mb
    | mb -> mb ];

  method! ctyp t =
    with "ctyp"
    match super#ctyp t with
    [ {| ! $({@_l||} ) . $t |} |
      {| $({@_l||} ) as $t |} |
      {| $t as $({@_l||} ) |} |
      {| $t -> $({@_l||} ) |} |
      {| $({@_l||} ) -> $t |} |
      {| $({@_l||} ) | $t |} |
      {| $t | $({@_l||} ) |} |
      {| $t of $({@_l||} ) |} |
      {| $({@_l||} ) and $t |} |
      {| $t and $({@_l||} ) |} |
      {| $t; $({@_l||} ) |} |
      {| $({@_l||} ); $t |} |
      {| $({@_l||}), $t |} |
      {| $t, $({@_l||} ) |} |
      {| $t & $({@_l||} ) |} |
      {| $({@_l||} ) & $t |} |
      {| $({@_l||} ) * $t |} |
      {| $t * $({@_l||} ) |} -> t
    | t -> t ];

  method! sig_item sg =
    with "sig_item"
    match super#sig_item sg with
    [ {| $({@_l||}); $sg |} | {| $sg; $({@_l||} ) |} -> sg
    | {| type $({:ctyp@_l||} ) |} -> {||}
    | sg -> sg ];

  method! str_item st =
    with "str_item"
    match super#str_item st with
    [ {| $({@_l||} ); $st |} | {| $st; $({@_l||} ) |} -> st
    | {| type $({:ctyp@_l||} ) |} -> {||}
    | {| let $rec:_ $({:binding@_l||} ) |} -> {||}
    | st -> st ];

  method! module_type mt =
    match super#module_type mt with
    [ {:module_type| $mt with $({:with_constr@_l||} ) |} -> mt
    | mt -> mt ];

  method! class_expr ce =
    with "class_expr"
    match super#class_expr ce with
    [ {| $({@_l||} ) and $ce |} | {| $ce and $({@_l||} ) |} -> ce
    | ce -> ce ];

  method! class_type ct =
    with "class_type"
    match super#class_type ct with
    [ {| $({@_l||} ) and $ct |} | {| $ct and $({@_l||} ) |} -> ct
    | ct -> ct ];

  method! class_sig_item csg =
    with "class_sig_item"
    match super#class_sig_item csg with
    [ {| $({@_l||} ); $csg |} | {| $csg; $({@_l||} ) |} -> csg
    | csg -> csg ];

  method! class_str_item cst =
    with "class_str_item"
    match super#class_str_item cst with
    [ {| $({@_l||} ); $cst |} | {| $cst; $({@_l||} ) |} -> cst
    | cst -> cst ];
end;

(* change all the [loc] to [ghost] *)    
class reloc _loc = object
  inherit map ;
  method! loc _ = _loc;
end;

(*
  {[]}
 *)  
let wildcarder = object (self)
  inherit map as super;
  method! patt = fun
  [ {:patt| $lid:_ |} -> {:patt| _ |}
  | {:patt| ($p as $_) |} -> self#patt p
  | p -> super#patt p ];
end;

(* let normalize = object (self) *)
(*   inherit fold as super; *)
(* end; *)

let match_pre = object (self)
  inherit map; (* as super; *)
  method! match_case = with "match_case" fun
   [ {| $pat:p -> $e |} -> {| $pat:p -> fun () -> $e |}
   | {| $pat:p when $e -> $e1 |} -> {| $pat:p when $e -> fun () -> $e1 |}
   | {| $a1 | $a2 |} -> {| $(self#match_case a1) | $(self#match_case a2) |}
   | {| |} -> {| |}
   | {| $anti:x |} -> {| $(anti: add_context x "lettry" ) |} ];
end;
  
module Make(MetaLoc:META_LOC) = struct


  module Expr = struct
    open StdMeta.Expr;
    let meta_loc = MetaLoc.meta_loc_expr;
    __MetaExpr__;
  end;
    
  module Patt =struct
    open StdMeta.Patt;
    let meta_loc = MetaLoc.meta_loc_patt;
    __MetaPatt__;
  end;
  (* module Expr = struct __MetaExpr__; end; *)
end;
