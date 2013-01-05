include Ast;
module type META_LOC = sig
      (** The first location is where to put the returned pattern.
          Generally it's _loc to match with {:patt| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> patt;
      (** The first location is where to put the returned expression.
          Generally it's _loc to match with {:expr| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> expr;
end;
open FanUtil;  
open StdLib;

{:fans|keep off; <++ "MetaExpr", "MetaPatt","Map","Fold","Print","OPrint";|};

{:ocaml|

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

|};




module Make(MetaLoc:META_LOC) = struct
  module Expr (* : sig *)
  (*   val meta_class_str_item: loc -> class_str_item -> PAst.expr; *)
  (* end  *)= struct
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
    


(* +-----------------------------------------------------------------+
   | Other utilities for ast                                         |
   +-----------------------------------------------------------------+ *)
  
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

















