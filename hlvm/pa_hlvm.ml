(* Compilation :
   ocamlc -pp camlp4oof -I +camlp4 dynlink.cma camlp4lib.cma pa_hlvm.ml -c
*)

open Camlp4.PreCast
type loc = Loc.t

module HlvmGram = MakeGram(Lexer)

let rec list _loc = function
  | [] -> <:expr< [] >>
  | hd::tl ->
      <:expr< $hd$ :: $list _loc tl$ >>


let mk = HlvmGram.Entry.mk
let typ_eoi, expr_eoi, str_item_eoi, str_items_eoi =
  mk "typ", mk "expr", mk "str_item", mk "str_items"

let () =
  Camlp4_config.antiquotations := true; 
  let quote _loc str =
    Syntax.Gram.parse_string Syntax.expr_eoi _loc str in

  EXTEND HlvmGram
  GLOBAL: expr_eoi typ_eoi str_item_eoi str_items_eoi;
  expr_eoi: [[ e = expr; `EOI -> e ]];
  typ_eoi: [[ t = typ; `EOI -> t ]];
  str_item_eoi: [[ s = str_item; `EOI -> s ]];
  str_items_eoi: [[ s = str_item; `EOI -> <:expr< [$s$] >>
                  | s = str_item; ";;"; p = str_items_eoi -> <:expr< $s$ :: $p$ >> ]];
  expr:
    [ "top" RIGHTA
        [  "do"; es = LIST1 [ e = expr -> e ] SEP ";" ->
             <:expr< compound $list _loc es$ >>
        |  "if"; p = expr; "then"; t = expr; "else"; f = expr ->
              <:expr< If($p$, $t$, $f$) >>
        | "let"; x = LIDENT; "="; v = expr; "in"; e = SELF ->
            <:expr< Let($str:x$, $v$, $e$) >> ]
      | "<-"
          [ e = SELF; "<-"; e' = SELF ->
              (match e with
                 | <:expr< Get($e1$, $e2$) >> ->
                   <:expr< Set($e1$, $e2$, $e'$) >>
                 | _ -> Loc.raise _loc (Failure "non-assignable left-hand-side")) ]
      | "disjunction" RIGHTA
          [ e1 = expr; "||"; e2 = expr -> <:expr< $e1$ ||: $e2$ >> ]
      | "conjunction" RIGHTA
          [ e1 = expr; "&&"; e2 = expr -> <:expr< $e1$ &&: $e2$ >> ]
      | "cmp" LEFTA
          [ e1 = expr; "<"; e2 = expr -> <:expr< $e1$ <: $e2$ >>
          | e1 = expr; "<="; e2 = expr -> <:expr< $e1$ <=: $e2$ >>
          | e1 = expr; "="; e2 = expr -> <:expr< $e1$ =: $e2$ >>
          | e1 = expr; "<>"; e2 = expr -> <:expr< $e1$ <>: $e2$ >>
          | e1 = expr; ">="; e2 = expr -> <:expr< $e1$ >=: $e2$ >>
          | e1 = expr; ">"; e2 = expr -> <:expr< $e1$ >: $e2$ >> ]
      | "sum" LEFTA
          [ e1 = expr; "+"; e2 = expr -> <:expr< $e1$ +: $e2$ >>
          | e1 = expr; "-"; e2 = expr -> <:expr< $e1$ -: $e2$ >> ]
      | "product" LEFTA
          [ e1 = expr; "*"; e2 = expr -> <:expr< $e1$ *: $e2$ >>
          | e1 = expr; "%"; e2 = expr -> <:expr< $e1$ %: $e2$ >>
          | e1 = expr; "/"; e2 = expr -> <:expr< $e1$ /: $e2$ >> ]
      | "apply"
          [ f = expr; "(" ; args = LIST0 expr SEP "," ; ")" ->
              <:expr< Apply($f$, $list _loc args$) >>
          | "print"; "("; e = SELF; ")" -> <:expr< Print $e$ >>
          | "printf"; "("; fmt = STRING;
            args = [","; args = LIST1 expr SEP "," -> args | -> [] ]; ")" ->
              <:expr< Printf ( $str:fmt$, $list _loc args$ ) >>
          | "length"; "("; e = SELF; ")" -> <:expr< Length $e$ >>
          | "float"; "("; e = SELF; ")" -> <:expr< FloatOfInt $e$ >>
          | "alloc"; "("; e1 = SELF; ","; e2 = SELF; ")" -> <:expr< Alloc($e1$, $e2$) >> ]
      | "." LEFTA
        [ e = SELF; "."; n = INT -> <:expr< GetValue($e$, $int:n$) >>
        | e1 = SELF; "["; e2 = SELF; "]" -> <:expr< Get($e1$, $e2$) >> ]
      | "simple" NONA
          [ n = INT -> <:expr< Int $int:n$ >>
          | x = FLOAT -> <:expr< Float $flo:x$ >>
          | v = LIDENT -> <:expr< Var $str:v$ >>
          | "("; ")" -> <:expr< Unit >>
          | "false" -> <:expr< Bool false >>
          | "true" -> <:expr< Bool true >>
          | "("; e = expr; ")" -> e
          | "{"; es = LIST0 expr SEP ";"; "}" -> <:expr< Struct $list _loc es$ >>
          | `ANTIQUOT("", e) -> quote _loc e
          | `ANTIQUOT("int", e) -> <:expr< Int $quote _loc e$ >>
          | `ANTIQUOT("flo", e) -> <:expr< Float $quote _loc e$ >> ]
    ];

  typ:
    [ "function"
        [ "("; args = LIST1 typ SEP ","; ")"; "->"; res = SELF ->
            <:expr< `Function($list _loc args$, $res$) >> ]
      | "struct"
          [ "{"; fields = LIST0 typ SEP ";"; "}" ->
              <:expr< `Struct $list _loc fields$ >> ]
      | "array"
          [ t = SELF; "array" -> <:expr< `Array $t$ >> ]
      | "simple"
          [ "bool" -> <:expr< `Bool >>
          | "float" -> <:expr< `Float >>
          | "int" -> <:expr< `Int >>
          | "ref" -> <:expr< `Reference >>
          | "unit" -> <:expr< `Unit >>
          | `ANTIQUOT("", t) -> quote _loc t ] ];

  annot: [ [ ":"; t = typ -> t
           | -> <:expr< `Float >> ] ];

  param: [ [ v = LIDENT; t = annot -> <:expr< ($str:v$, $t$) >> ] ];

  param_list:
    [ [ "(" ; args = LIST1 param SEP ","; ")" -> list _loc args ]
      | [ arg = param -> <:expr< [$arg$] >> ] ];

  str_item: [[ s = str_item' -> <:expr< ($s$ : Hlvm.t) >> (* variant coercion *) ]];
  str_item':
    [ [ "let" ; f = LIDENT ; params = param_list; ret = annot;  "=" ; body = expr ->
          <:expr< `Function($str:f$, $params$, $ret$, $body$) >> 
      | "extern"; f = LIDENT; ":"; "("; args = LIST1 typ SEP ","; ")"; "->"; ret = typ ->
          <:expr< `Extern ($str:f$, $list _loc args$, $ret$) >>
      | e = expr  -> <:expr< `Expr $e$ >>
      | `ANTIQUOT("", s) -> quote _loc s ] ];
  END

(** Quotations setup *)
let () =
  Syntax.Quotation.add "type" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> HlvmGram.parse_string typ_eoi loc quote);
  Syntax.Quotation.add "expr" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> HlvmGram.parse_string expr_eoi loc quote);
  Syntax.Quotation.add "phrase" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> HlvmGram.parse_string str_item_eoi loc quote);
  Syntax.Quotation.add "prog" Syntax.Quotation.DynAst.expr_tag
    (fun loc _ quote -> HlvmGram.parse_string str_items_eoi loc quote);
  Syntax.Quotation.default := "expr"
