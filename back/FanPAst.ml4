
module type META_LOC = sig
      (** The first location is where to put the returned pattern.
          Generally it's _loc to match with {:patt| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_patt : FanLoc.t -> FanLoc.t -> PAst.patt;
      (** The first location is where to put the returned expression.
          Generally it's _loc to match with {:expr| ... |} quotations.
          The second location is the one to treat. *)
    val meta_loc_expr : FanLoc.t -> FanLoc.t -> PAst.expr;
end;
open FanUtil;  
open StdLib;

{:fans|keep off; <++ "MetaExpr2", "MetaPatt2","FMap","FFold","FPrint","FOPrint";|};

include PAst;
  
{:ocaml|
   type loc = FanLoc.t
   and meta_bool =
    [ `BTrue
    | `BFalse
    | `Ant of string ]
   and rec_flag =
    [ `ReRecursive
    | `ReNil
    | `Ant of string ]
   and direction_flag =
    [ `DiTo
    | `DiDownto
    | `Ant of string ]
   and mutable_flag =
    [ `MuMutable
    | `MuNil
    | `Ant of string ]
   and private_flag =
    [ `PrPrivate
    | `PrNil
    | `Ant of string ]
   and virtual_flag =
    [ `ViVirtual
    | `ViNil
    | `Ant of string ]
   and override_flag =
    [ `OvOverride
    | `OvNil
    | `Ant of string ]
   and row_var_flag =
    [ `RvRowVar
    | `RvNil
    | `Ant of string ]
   and meta_option 'a =
    [ `ONone
    | `OSome of 'a
    | `Ant of string ]
   and meta_list 'a =
    [ `LNil
    | `LCons of 'a and meta_list 'a
    | `Ant of string ]
   and ident =
    [ `IdAcc of loc and ident and ident (* i . i *)
    | `IdApp of loc and ident and ident (* i i *)
    | `IdLid of loc and string (* foo *)
    | `IdUid of loc and string (* Bar *)
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string (* $s$ *)
    ]
   and patt =
    [ `PaNil of loc
    | `PaId  of loc and ident (* i *)
    | `PaAli of loc and patt and patt (* p as p *) (* (Node x y as n) *)
    | `Ant of loc and string (* $s$ *)
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
    | `Ant of loc and string (* $s$ *)
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
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string (* $s$ *) ]
  and binding =
    [ `BiNil of loc
      (* bi and bi *) (* let a = 42 and c = 43 *)
    | `BiAnd of loc and binding and binding
      (* p = e *) (* let patt = expr *)
    | `BiEq  of loc and patt and expr
    | `Ant of loc and string (* $s$ *) ]
  and rec_binding =
    [ `RbNil of loc
      (* rb ; rb *)
    | `RbSem of loc and rec_binding and rec_binding
      (* i = e *)
    | `RbEq  of loc and ident and expr
    | `Ant of loc and string (* $s$ *) ]
  and module_binding =
    [ `MbNil of loc
      (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | `MbAnd of loc and module_binding and module_binding
      (* s : mt = me *)
    | `MbColEq  of loc and string and module_type and module_expr
      (* s : mt *)
    | `MbCol  of loc and string and module_type
    | `Ant of loc and string (* $s$ *) ]
  and match_case =
    [ `McNil of loc
      (* a | a *)
    | `McOr of loc and match_case and match_case
      (* p (when e)? -> e *)
    | `McArr of loc and patt and expr and expr
    (* | McArrow of loc and patt and option expr and expr (\* FIXME *\) *)
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string ]
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
    | `Ant of loc and string (* $s$ *) ]
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
    | `Ant of loc and string ]
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
    | `Ant of loc and string (* $s$ *) ];
  |};



  
module Make(MetaLoc:META_LOC) = struct



  module Expr (* : sig *)
  (*   val meta_class_str_item: loc -> class_str_item -> PAst.expr; *)
  (* end  *)= struct
    open StdMeta.PExpr;
    let meta_loc = MetaLoc.meta_loc_expr;
    __MetaExpr__;
  end;
    
  module Patt =struct
    open StdMeta.PPatt;
    let meta_loc = MetaLoc.meta_loc_patt;
    __MetaPatt__;
  end;
  (* module Expr = struct __MetaExpr__; end; *)
end;

        (* | `CrNil a0 -> *)
        (*     `ExApp *)
        (*       (_loc, (`ExId (_loc, (`IdUid (_loc, "`CrNil")))), *)
        (*         (meta_loc _loc a0)) *)
  














(* to be replaced later *)
let rec list_of_binding x acc =
  match x with
  [ `BiAnd (_loc,b1,b2) -> list_of_binding b1 (list_of_binding b2 acc)
  | t -> [t :: acc]];
let rec list_of_rec_binding x acc =
  match x with
  [ `RbSem (_loc,b1,b2) -> list_of_rec_binding b1 (list_of_rec_binding b2 acc)
  | t -> [t :: acc]];
let rec list_of_with_constr x acc =
  match x with
  [ `WcAnd (_loc,w1,w2) -> list_of_with_constr w1 (list_of_with_constr w2 acc)
  | t -> [t :: acc]];
let rec list_of_ctyp x acc =
  match x with
  [ `TyNil _loc -> acc
  | `TyAmp (_loc,x,y)|`TyCom (_loc,x,y)|`TySta (_loc,x,y)|`TySem (_loc,x,y)|`TyAnd
      (_loc,x,y)|`TyOr (_loc,x,y) -> list_of_ctyp x (list_of_ctyp y acc)
  | x -> [x :: acc]];
let rec list_of_patt x acc =
  match x with
  [ `PaNil _loc -> acc
  | `PaCom (_loc,x,y)|`PaSem (_loc,x,y) -> list_of_patt x (list_of_patt y acc)
  | x -> [x :: acc]];
let rec list_of_expr x acc =
  match x with
  [ `ExNil _loc -> acc
  | `ExCom (_loc,x,y)|`ExSem (_loc,x,y) -> list_of_expr x (list_of_expr y acc)
  | x -> [x :: acc]];
let rec list_of_str_item x acc =
  match x with
  [ `StNil _loc -> acc
  | `StSem (_loc,x,y) -> list_of_str_item x (list_of_str_item y acc)
  | x -> [x :: acc]];
let rec list_of_sig_item x acc =
  match x with
  [ `SgNil _loc -> acc
  | `SgSem (_loc,x,y) -> list_of_sig_item x (list_of_sig_item y acc)
  | x -> [x :: acc]];
let rec list_of_class_sig_item x acc =
  match x with
  [ `CgNil _loc -> acc
  | `CgSem (_loc,x,y) ->
      list_of_class_sig_item x (list_of_class_sig_item y acc)
  | x -> [x :: acc] ];
let rec list_of_class_str_item x acc =
  match x with
  [ `CrNil _loc -> acc
  | `CrSem (_loc,x,y) ->
      list_of_class_str_item x (list_of_class_str_item y acc)
  | x -> [x :: acc] ];
let rec list_of_class_type x acc =
  match x with
  [ `CtAnd (_loc,x,y) -> list_of_class_type x (list_of_class_type y acc)
  | x -> [x :: acc]];
let rec list_of_class_expr x acc =
  match x with
  [ `CeAnd (_loc,x,y) -> list_of_class_expr x (list_of_class_expr y acc)
  | x -> [x :: acc] ];
let rec list_of_module_expr x acc =
  match x with
  [ `MeApp (_loc,x,y) -> list_of_module_expr x (list_of_module_expr y acc)
  | x -> [x :: acc]];
let rec list_of_match_case x acc =
  match x with
  [ `McNil _loc -> acc
  | `McOr (_loc,x,y) -> list_of_match_case x (list_of_match_case y acc)
  | x -> [x :: acc] ]; 
let rec list_of_ident x acc =
  match x with
  [ `IdAcc (_loc,x,y)|`IdApp (_loc,x,y) ->
      list_of_ident x (list_of_ident y acc)
  | x -> [x :: acc] ];
let rec list_of_module_binding x acc =
  match x with
  [ `MbAnd (_loc,x,y) ->
      list_of_module_binding x (list_of_module_binding y acc)
  | x -> [x :: acc] ];
let map_expr f =
  object  inherit  map as super;  method! expr x = f (super#expr x); end;
let map_patt f =
  object  inherit  map as super; method! patt x = f (super#patt x); end;
let map_ctyp f =
  object  inherit  map as super; method! ctyp x = f (super#ctyp x); end;
let map_str_item f =
  object  inherit  map as super; method! str_item x = f (super#str_item x); end;
let map_sig_item f =
  object  inherit  map as super; method! sig_item x = f (super#sig_item x); end;
let map_loc f =
  object  inherit  map as super; method! loc x = f (super#loc x); end;
class clean_ast =
  object 
    inherit  map as super;
    method! with_constr wc =
      match super#with_constr wc with
      [ `WcAnd (_loc,`WcNil _l,wc)|`WcAnd (_loc,wc,`WcNil _l) -> wc
      | wc -> wc];
    method! expr e =
      match super#expr e with
      [ `ExLet (_loc,_,`BiNil _l,e)|`ExRec (_loc,`RbNil _l,e)|`ExCom
          (_loc,`ExNil _l,e)|`ExCom (_loc,e,`ExNil _l)|`ExSem
          (_loc,`ExNil _l,e)|`ExSem (_loc,e,`ExNil _l) -> e
      | e -> e];
    method! patt p =
      match super#patt p with
      [ `PaAli (_loc,p,`PaNil _l)|`PaOrp (_loc,`PaNil _l,p)|`PaOrp
          (_loc,p,`PaNil _l)|`PaCom (_loc,`PaNil _l,p)|`PaCom
          (_loc,p,`PaNil _l)|`PaSem (_loc,`PaNil _l,p)|`PaSem (_loc,p,`PaNil _l)
          -> p
      | p -> p];
    method! match_case mc =
      match super#match_case mc with
      [ `McOr (_loc,`McNil _l,mc)|`McOr (_loc,mc,`McNil _l) -> mc
      | mc -> mc];
    method! binding bi =
      match super#binding bi with
      [ `BiAnd (_loc,`BiNil _l,bi)|`BiAnd (_loc,bi,`BiNil _l) -> bi
      | bi -> bi];
    method! rec_binding rb =
      match super#rec_binding rb with
      [ `RbSem (_loc,`RbNil _l,bi)|`RbSem (_loc,bi,`RbNil _l) -> bi
      | bi -> bi];
    method! module_binding mb =
      match super#module_binding mb with
      [ `MbAnd (_loc,`MbNil _l,mb)|`MbAnd (_loc,mb,`MbNil _l) -> mb
      | mb -> mb];
    method! ctyp t =
      match super#ctyp t with
      [ `TyPol (_loc,`TyNil _l,t)|`TyAli (_loc,`TyNil _l,t)|`TyAli
          (_loc,t,`TyNil _l)|`TyArr (_loc,t,`TyNil _l)|`TyArr
          (_loc,`TyNil _l,t)|`TyOr (_loc,`TyNil _l,t)|`TyOr
          (_loc,t,`TyNil _l)|`TyOf (_loc,t,`TyNil _l)|`TyAnd
          (_loc,`TyNil _l,t)|`TyAnd (_loc,t,`TyNil _l)|`TySem
          (_loc,t,`TyNil _l)|`TySem (_loc,`TyNil _l,t)|`TyCom
          (_loc,`TyNil _l,t)|`TyCom (_loc,t,`TyNil _l)|`TyAmp
          (_loc,t,`TyNil _l)|`TyAmp (_loc,`TyNil _l,t)|`TySta
          (_loc,`TyNil _l,t)|`TySta (_loc,t,`TyNil _l) -> t
      | t -> t];
    method! sig_item sg =
      match super#sig_item sg with
      [ `SgSem (_loc,`SgNil _l,sg)|`SgSem (_loc,sg,`SgNil _l) -> sg
      | `SgTyp (_loc,`TyNil _l) -> `SgNil _loc
      | sg -> sg];
    method! str_item st =
      match super#str_item st with
      [ `StSem (_loc,`StNil _l,st)|`StSem (_loc,st,`StNil _l) -> st
      | `StTyp (_loc,`TyNil _l) -> `StNil _loc
      | `StVal (_loc,_,`BiNil _l) -> `StNil _loc
      | st -> st];
    method! module_type mt =
      match super#module_type mt with
      [ `MtWit (_loc,mt,`WcNil _l) -> mt
      | mt -> mt];
    method! class_expr ce =
      match super#class_expr ce with
      [ `CeAnd (_loc,`CeNil _l,ce)|`CeAnd (_loc,ce,`CeNil _l) -> ce
      | ce -> ce];
    method! class_type ct =
      match super#class_type ct with
      [ `CtAnd (_loc,`CtNil _l,ct)|`CtAnd (_loc,ct,`CtNil _l) -> ct
      | ct -> ct];
    method! class_sig_item csg =
      match super#class_sig_item csg with
      [ `CgSem (_loc,`CgNil _l,csg)|`CgSem (_loc,csg,`CgNil _l) -> csg
      | csg -> csg];
    method! class_str_item cst =
      match super#class_str_item cst with
      [ `CrSem (_loc,`CrNil _l,cst)|`CrSem (_loc,cst,`CrNil _l) -> cst
      | cst -> cst];
  end;
class reloc _loc = object  inherit  map; method! loc _ = _loc; end;
let wildcarder =
  object (self)
    inherit  map as super;
    method! patt =
      function
      [ `PaId (_loc,`IdLid (_,_)) -> `PaAny _loc
      | `PaAli (_loc,p,_) -> self#patt p
      | p -> super#patt p];
  end;
let match_pre =
  object (self)
    inherit  map;
    method! match_case =
      function
      [ `McArr (_loc,p,`ExNil _,e) ->
          `McArr
            (_loc, p, (`ExNil _loc),
              (`ExFun
                 (_loc,
                   (`McArr
                      (_loc, (`PaId (_loc, (`IdUid (_loc, "()")))),
                        (`ExNil _loc), e)))))
      | `McArr (_loc,p,e,e1) ->
          `McArr
            (_loc, p, e,
              (`ExFun
                 (_loc,
                   (`McArr
                      (_loc, (`PaId (_loc, (`IdUid (_loc, "()")))),
                        (`ExNil _loc), e1)))))
      | `McOr (_loc,a1,a2) ->
          `McOr (_loc, (self#match_case a1), (self#match_case a2))
      | `McNil _loc -> `McNil _loc
      | `Ant (_loc,x) -> `Ant (_loc, (add_context x "lettry"))];
  end;


let rec is_module_longident =
  function
  [ `IdAcc (_loc,_,i) -> is_module_longident i
  | `IdApp (_loc,i1,i2) ->
      (is_module_longident i1) && (is_module_longident i2)
  | `IdUid (_loc,_) -> true
  | _ -> false ];
      
let ident_of_expr =
  let error () =
    invalid_arg "ident_of_expr: this expression is not an identifier" in
  let rec self =
    function
    [ `ExApp (_loc,e1,e2) -> `IdApp (_loc, (self e1), (self e2))
    | `ExAcc (_loc,e1,e2) -> `IdAcc (_loc, (self e1), (self e2))
    | `ExId (_loc,`IdLid (_,_)) -> error ()
    | `ExId (_loc,i) -> if is_module_longident i then i else error ()
    | _ -> error () ] in
  function [ `ExId (_loc,i) -> i | `ExApp (_loc,_,_) -> error () | t -> self t ] ;
      
