open Fan.Syntax;
module Ast = Camlp4Ast;
open FanUtil;
{:extend.create|Gram s v|};
with "patt"
  {:extend|Gram
  s:
  ["`"; a_ident{s}  -> {| `$s |}
  |"`"; a_ident{v}; `ANT (("" | "anti" as n) ,s)
    -> {| `$v $(anti:mk_anti ~c:"patt" n s)|}
  |"`"; a_ident{s}; `STR(_,v) -> {| `$s $str:v |}
  |"`"; a_ident{s}; `LID x  ->  {| `$s $lid:x |}
  |"`"; a_ident{s}; "("; L1 v SEP ","{v}; ")" ->
      match v with
      [ [x] -> {| `$s $x |}
      | [x::xs] -> {| `$s ($x,$list:xs) |}
      | _ -> assert false ]  ]
  v:
  [ `STR(_,s) -> {| $str:s|}
  | `LID x   -> (* {| $(id:{:ident|$lid:x|}) |} *)  {| $lid:x|} 
  | S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |}
  | "("; S{p1} ; "as"; S{p2} ; ")" -> {| ($p1 as $p2) |}
   ]
  |};

(*
t s "`A ((\"x\"|\"y\" as n),s)";
t s "`A $x";
t s `UID ("First"|"Last" as x )
Comparing two ant
*)
