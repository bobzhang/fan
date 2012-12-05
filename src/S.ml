open Fan.Syntax;
module Ast = Camlp4Ast;
open FanUtil;
let open FanParsers in  begin
   pa_r (module Fan);
   pa_rp (module Fan);
   pa_q (module Fan);
   pa_g (module Fan);
   pa_l (module Fan);
   pa_m (module Fan);
end;
Fan.iter_and_take_callbacks (fun (_,f) -> f ()) ; 
let t e s = Gram.parse_string e FanLoc.string_loc s;  
(* {:extend.create|Gram s v|}; *)
(* with "patt" *)
(*   {:extend|Gram *)
(*   s: *)
(*   ["`"; a_ident{s}  -> {| `$s |} *)
(*   |"`"; a_ident{v}; `ANT (("" | "anti" as n) ,s) *)
(*     -> {| `$v $(anti:mk_anti ~c:"patt" n s)|} *)
(*   |"`"; a_ident{s}; `STR(_,v) -> {| `$s $str:v |} *)
(*   |"`"; a_ident{s}; `LID x  ->  {| `$s $lid:x |} *)
(*   |"`"; a_ident{s}; "("; L1 v SEP ","{v}; ")" -> *)
(*       match v with *)
(*       [ [x] -> {| `$s $x |} *)
(*       | [x::xs] -> {| `$s ($x,$list:xs) |} *)
(*       | _ -> assert false ]  ] *)
(*   v: *)
(*   [ `STR(_,s) -> {| $str:s|} *)
(*   | `LID x   -> (\* {| $(id:{:ident|$lid:x|}) |} *\)  {| $lid:x|}  *)
(*   | S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |} *)
(*   | "("; S{p1} ; "as"; S{p2} ; ")" -> {| ($p1 as $p2) |} *)
(*    ] *)
(*   |}; *)

(* (\* *)
(* t s "`A ((\"x\"|\"y\" as n),s)"; *)
(* t s "`A $x"; *)
(* t s `UID ("First"|"Last" as x ) *)
(* Comparing two ant *)
(* *\) *)
(* Gram.dump Format.std_formatter expr; *)
(* {:delete|Gram ident:[`UID i]|}; *)
(*   (\* {:delete|Gram expr:[TRY module_longident_dot_lparen; S; ")"]|}; *\) *)
(* t expr "A.B.C.D.c"; *)

{:extend.create|Gram a b a_eoi |}  ;
  
{:extend|Gram 
  a:
  [ TRY module_longident_dot_lparen{s} -> s
  | b{s} -> s ]
  b "ident":
  [ 
   (* a_UIDENT{i} -> {| $uid:i |} *)
  (* | *) a_LIDENT{i} -> {| $lid:i |}
  | `UID i -> {|$uid:i|}
  | `UID i; "."; S{j} -> {| $uid:i.$j |} 
  (* | a_UIDENT{i}; "."; S{j} -> {| $uid:i.$j |}  *)]
  (* [ `LID i  -> {| $lid:i|} *)
  (* | `UID s ; "." ; S{j} -> {|$uid:s.$j|}  ] *)
  a_eoi: [a{i} ; `EOI -> i]
|};

(* {:extend.create|Gram c|}  ; *)

(*   with "ident"{:extend|Gram local:d; *)
(*  c:[  d {x}; "(" -> {| $uid:x |}   ] *)
(*                  d:[`UID x -> x ] *)
(* |};   *)

t a_eoi "A.C.U.b"  ;
