(* open Ast; *)
open PreCast.Syntax;
open Lib;
open LibUtil;
open FanUtil;
open GramLib;
let help_sequences () = begin
  Printf.eprintf "\
New syntax:\
\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\
\n    while e do e1; e2; ... ; en done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\
\nOld syntax (still supported):\
\n    begin e1; e2; ... ; en end\
\n    while e begin e1; e2; ... ; en end\
\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\
\nVery old (no more supported) syntax:\
\n    do e1; e2; ... ; en-1; return en\
\n    while e do e1; e2; ... ; en; done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\
\n"; flush stderr; exit 1  end;

{:extend.create|Gram pos_exprs|};
let apply () = begin 
  Options.add ("-help_seq", (FanArg.Unit help_sequences), "Print explanations about new sequences and exit.");

    {:extend.clear|Gram
    a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT a_NATIVEINT a_OPTLABEL a_STRING a_UIDENT a_ident
    amp_ctyp and_ctyp match_case match_case0 match_case_quot binding binding_quot rec_binding_quot
    class_declaration class_description class_expr class_expr_quot class_fun_binding class_fun_def
    class_info_for_class_expr class_info_for_class_type class_longident class_longident_and_param
    class_name_and_param class_sig_item class_sig_item_quot class_signature class_str_item class_str_item_quot
    class_structure class_type class_type_declaration class_type_longident class_type_longident_and_param
    class_type_plus class_type_quot comma_ctyp comma_expr comma_ipatt comma_patt comma_type_parameter
    constrain constructor_arg_list constructor_declaration constructor_declarations ctyp ctyp_quot
    cvalue_binding direction_flag dummy eq_expr expr expr_eoi expr_quot field_expr field_expr_list fun_binding
    fun_def ident ident_quot implem interf ipatt ipatt_tcon patt_tcon label label_declaration label_declaration_list
    label_expr_list label_expr label_longident label_patt label_patt_list  let_binding meth_list
    meth_decl module_binding module_binding0 module_binding_quot module_declaration module_expr module_expr_quot
    module_longident module_longident_with_app module_rec_declaration module_type module_type_quot
    more_ctyp name_tags opt_as_lident opt_class_self_patt opt_class_self_type opt_comma_ctyp opt_dot_dot
    opt_eq_ctyp opt_expr opt_meth_list opt_mutable opt_polyt opt_private opt_rec opt_virtual 
    patt patt_as_patt_opt patt_eoi patt_quot   poly_type row_field sem_expr
    sem_expr_for_list sem_patt sem_patt_for_list semi sequence sig_item sig_item_quot sig_items star_ctyp
    str_item str_item_quot str_items top_phrase type_constraint type_declaration type_ident_and_parameters
    type_kind type_longident type_longident_and_parameters type_parameter type_parameters typevars 
    val_longident with_constr with_constr_quot
      lang
  |};  


  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  setup_op_parser prefixop
    (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1);

  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  setup_op_parser infixop2
    (fun x -> (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list_first_char_ok && symbolchar x 1));

  let list = ['@'; '^'] in
  setup_op_parser infixop3
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['+'; '-'] in
  setup_op_parser infixop4
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['*'; '/'; '%'; '\\'] in
  setup_op_parser infixop5
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1);

  setup_op_parser infixop6
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2);


  FanTokenFilter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.setup_parser sem_expr begin
    let symb1 = Gram.parse_origin_tokens expr in
    let symb = parser
      [ [< (`ANT (("list" as n), s), _loc) >] ->
        {:expr| $(anti:mk_anti ~c:"expr;" n s) |}
      | [< a = symb1 >] -> a ] in
    let rec kont al =
      parser
      [ [< (`KEYWORD ";", _); a = symb; 's >] ->
        let _loc =
          FanLoc.merge
            (FanAst.loc_of_expr al) (FanAst.loc_of_expr a) in
        kont {:expr| $al; $a |} s
      | [< >] -> al ] in
    parser [< a = symb; 's >] -> kont a s
  end;

  with "module_expr"
  {:extend|Gram
      module_expr_quot:
      [ module_expr{x} -> x
      | -> {||} ]
      module_binding0:
      { RA
        [ "("; a_UIDENT{m}; ":"; module_type{mt}; ")"; S{mb} ->
            {| functor ( $m : $mt ) -> $mb |}
        | ":"; module_type{mt}; "="; module_expr{me} ->
            {| ( $me : $mt ) |}
        | "="; module_expr{me} -> {| $me |} ] }
      module_expr:
      { "top"
        [ "functor"; "("; a_UIDENT{i}; ":"; module_type{t}; ")"; "->"; S{me} ->
            {| functor ( $i : $t ) -> $me |}
        | "struct"; str_items{st}; "end" ->
            {| struct $st end |} ]
       "apply"
        [ S{me1}; S{me2} -> {| $me1 $me2 |} ]
       "simple"
        [ `ANT ((""|"mexp"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"module_expr" n s) |}
        | `QUOTATION x ->
            AstQuotation.expand _loc x DynAst.module_expr_tag
        | module_longident{i} -> {| $id:i |}
        | "("; S{me}; ":"; module_type{mt}; ")" ->
            {| ( $me : $mt ) |}
        | "("; S{me}; ")" -> {| $me |}
        | "("; "val"; expr{e}; ")" -> {| (val $e) |}  (* first class modules *)
        | "("; "val"; expr{e}; ":"; package_type{p}; ")" ->
            {| (val $e : $p) |} ] } |};

  with "module_binding"
      {:extend|Gram
        module_binding_quot:
        [ S{b1}; "and"; S{b2} ->  {| $b1 and $b2 |}
        | `ANT (("module_binding"|"anti"|"" as n),s) ->  {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | a_UIDENT{m}; ":"; module_type{mt} ->  {| $uid:m : $mt |}
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} ->  {| $uid:m : $mt = $me |}
        | -> {||} ]
        module_binding:
        [ S{b1}; "and"; S{b2} -> {| $b1 and $b2 |}
        | `ANT (("module_binding"|"anti"|"list" |"" as n),s) -> {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.module_binding_tag
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} -> {| $uid:m : $mt = $me |} ]
        module_rec_declaration:
        [ S{m1}; "and"; S{m2} -> {| $m1 and $m2 |}
        | `ANT ((""|"module_binding"|"anti"|"list" as n),s) ->  {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.module_binding_tag
        | a_UIDENT{m}; ":"; module_type{mt} -> {| $uid:m : $mt |} ] |};

  with "with_constr"
      {:extend|Gram
        with_constr_quot:
        [ with_constr{x} -> x  | -> {||} ]
        with_constr: 
        [ S{wc1}; "and"; S{wc2} -> {| $wc1 and $wc2 |}
        | `ANT ((""|"with_constr"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"with_constr" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.with_constr_tag
        | "type"; type_longident_and_parameters{t1}; "="; ctyp{t2} ->           {| type $t1 = $t2 |}
        | "type"; type_longident_and_parameters{t1}; ":="; ctyp{t2} ->         {| type $t1 := $t2 |}
        | "module"; module_longident{i1}; "="; module_longident_with_app{i2} -> {| module $i1 = $i2 |}
        | "module"; module_longident{i1}; ":="; module_longident_with_app{i2} -> {| module $i1 := $i2 |} ] |};

  with "module_type"
    {:extend|Gram
      module_type:
      { "top"
        [ "functor"; "("; a_UIDENT{i}; ":"; S{t}; ")"; "->"; S{mt} ->
            {| functor ( $i : $t ) -> $mt |} ]
        "with"
        [ S{mt}; "with"; with_constr{wc} ->  {| $mt with $wc |} ]
        "apply"
        [ S{mt1}; S{mt2} ->  ModuleType.app0 mt1 mt2 ] (* FIXME *)
        "."
        [ S{mt1}; "."; S{mt2} -> ModuleType.acc0 mt1 mt2 ] (*FIXME*)
        "sig"
        [ "sig"; sig_items{sg}; "end" -> {| sig $sg end |} ]
       "simple"
        [ `ANT ((""|"mtyp"|"anti"|"list" as n),s) ->  {| $(anti:mk_anti ~c:"module_type" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.module_type_tag
        | module_longident_with_app{i} -> {| $id:i |}
        | "'"; a_ident{i} -> {| ' $i |}
        | "("; S{mt}; ")" -> {| $mt |}
        | "module"; "type"; "of"; module_expr{me} -> {| module type of $me |} ] }
      module_declaration:
      { RA
        [ ":"; module_type{mt} -> {| $mt |}
        | "("; a_UIDENT{i}; ":"; module_type{t}; ")"; S{mt} -> {| functor ( $i : $t ) -> $mt |} ] }
      module_type_quot:
      [ module_type{x} -> x | -> {:module_type||} ]  |};

  with "sig_item"
  {:extend|Gram
    sig_item_quot:
    [ "#"; a_LIDENT{n}; opt_expr{dp} -> {| # $n $dp |}
    | sig_item{sg1}; semi; S{sg2} ->
        match sg2 with
        [ {||} -> sg1
        | _ -> {| $sg1; $sg2 |} ]
    | sig_item{sg} -> sg
    | -> {||} ]
    sig_item:
    [ `ANT ((""|"sigi"|"anti"|"list" as n),s) ->  {| $(anti:mk_anti ~c:"sig_item" n s) |}
    | `QUOTATION x -> AstQuotation.expand _loc x DynAst.sig_item_tag
    | "exception"; constructor_declaration{t} ->  {| exception $t |}
    | "external"; a_LIDENT{i}; ":"; ctyp{t}; "="; string_list{sl} -> {| external $i : $t = $sl |}
    | "include"; module_type{mt} -> {| include $mt |}
    | "module"; a_UIDENT{i}; module_declaration{mt} ->  {| module $i : $mt |}
    | "module"; "rec"; module_rec_declaration{mb} ->    {| module rec $mb |}
    | "module"; "type"; a_ident{i}; "="; module_type{mt} ->  {| module type $i = $mt |}
    | "module"; "type"; a_ident{i} ->  {| module type $i |}
    | "open"; module_longident{i} -> {| open $i |}
    | "type"; type_declaration{t} -> {| type $t |}
    | "val"; a_LIDENT{i}; ":"; ctyp{t} ->  {| val $i : $t |}
    | "class"; class_description{cd} ->    {| class $cd |}
    | "class"; "type"; class_type_declaration{ctd} ->  {| class type $ctd |} ]
    (* mli entrance *)    
    interf:
    [ "#"; a_LIDENT{n}; opt_expr{dp};  ";;" -> ([ {| # $n $dp |} ],  Some _loc)
    | sig_item{si}; semi;  S{(sil, stopped)} -> ([si :: sil], stopped)
    | `EOI -> ([], None) ]
    sig_items:
    [ `ANT ((""|"sigi"|"anti"|"list" as n),s) ->  {| $(anti:mk_anti n ~c:"sig_item" s) |}
    | `ANT ((""|"sigi"|"anti"|"list" as n),s); semi; S{sg} ->  {| $(anti:mk_anti n ~c:"sig_item" s); $sg |} 
    | L0 [ sig_item{sg}; semi -> sg ]{l} -> {|$list:l|} ]
 |};

    with "expr"
    {:extend|Gram
      local:  fun_def_patt;
      expr_quot:
      [ expr{e1}; ","; comma_expr{e2} -> {| $e1, $e2 |}
      | expr{e1}; ";"; sem_expr{e2} -> {| $e1; $e2 |}
      | expr{e} -> e
      | -> {||} ]
      cvalue_binding:
      [ "="; expr{e} -> e
      | ":"; "type"; unquoted_typevars{t1}; "." ; ctyp{t2} ; "="; expr{e} -> 
          let u = {:ctyp| ! $t1 . $t2 |} in  {| ($e : $u) |}
      | ":"; poly_type{t}; "="; expr{e} -> {| ($e : $t) |}
      | ":"; poly_type{t}; ":>"; ctyp{t2}; "="; expr{e} ->
          match t with
          [ {:ctyp| ! $_ . $_ |} -> raise (XStream.Error "unexpected polytype here")
          | _ -> {| ($e : $t :> $t2) |} ]
      | ":>"; ctyp{t}; "="; expr{e} -> {| ($e :> $t) |} ]
      fun_binding:
      { RA
          [ "("; "type"; a_LIDENT{i}; ")"; S{e} -> {| fun (type $i) -> $e |}
          | ipatt{p}; S{e} -> {| fun $p -> $e |}
          | cvalue_binding{bi} -> bi  ] }
       lang:
       [ `STR(_,s) ->
        begin let old = !AstQuotation.default;  AstQuotation.default := s; old end ]
       pos_exprs:
       [ L1[dot_lstrings{ls};":";dot_lstrings{rs}
            -> (String.concat "." ls, String.concat "." rs)
            | dot_lstrings{ls} ->
                let x = String.concat "." ls in
                (x,x) ] SEP ";"{xys} -> begin
         let old = !AstQuotation.map;
         AstQuotation.map := SMap.add_list xys old;
           old
       end
       ]
       (* pos_exprs: *)
       (* [ L1 [ `STR(_,x); ":";`STR(_,y) -> (x,y) ] SEP ";"{xys}  -> *)
       (*   begin *)
       (*     let old = !AstQuotation.map; *)
       (*     AstQuotation.map := SMap.add_list xys old; *)
       (*     old *)
       (*   end *)
       (* ] *)
       fun_def_patt:
       ["(";"type";a_LIDENT{i};")" -> fun e -> {|fun (type $i) -> $e |}
       | ipatt{p} -> fun e -> {| fun $p -> $e |}
       | ipatt{p}; "when"; expr{w} -> fun e -> {|fun $p when $w -> $e |} ]
       fun_def:
       {RA
          [ fun_def_patt{f}; "->"; expr{e} ->  f e
          | fun_def_patt{f}; S{e} -> f e] }    
       opt_expr:
       [ expr{e} -> e | -> {||} ]
       expr:
       {
        "top" RA
        [ "let"; opt_rec{r}; binding{bi}; "in"; S{x} ->
            {| let $rec:r $bi in $x |}
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; S{e} ->
            {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; S{e} -> {| let open $id:i in $e |}
        | "let"; "try"; opt_rec{r}; binding{bi}; "in"; S{x}; "with"; match_case{a} ->
            {| let try $rec:r $bi in $x with [ $a ] |}
        | "match"; S{e}; "with"; match_case{a} -> {|match $e with [$a]|}
        | "try"; S{e}; "with"; match_case{a} -> {|try $e with [$a]|}
        | "if"; S{e1}; "then"; S{e2}; "else"; S{e3} -> {| if $e1 then $e2 else $e3 |}
        | "if"; S{e1}; "then"; S{e2} -> {| if $e1 then $e2 |}
             (*FIXME add {|if then|} as a quote *)
        | "do"; sequence{seq}; "done" -> Expr.mksequence ~loc:_loc seq

        | "with"; lang{old}; S{x} -> begin  AstQuotation.default := old; x  end
        | "with";"{"; pos_exprs{old} ;"}"; S{x} -> begin AstQuotation.map := old; x end
        | "for"; a_LIDENT{i}; "="; S{e1}; direction_flag{df}; S{e2}; "do";
            sequence{seq}; "done" -> {| for $i = $e1 $to:df $e2 do $seq done |}
        | "while"; S{e}; "do"; sequence{seq}; "done" ->
            {|while $e do $seq done |}   ]  
       ":=" NA
        [ S{e1}; ":="; S{e2} -> {| $e1 := $e2 |} 
        | S{e1}; "<-"; S{e2} -> (* FIXME should be deleted in original syntax later? *)
            match Expr.bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> {| $e1 <- $e2 |} ] ]
       "||" RA
        [ S{e1}; infixop0{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "&&" RA
        [ S{e1}; infixop1{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "<" LA
        [ S{e1}; infixop2{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "^" RA
        [ S{e1}; infixop3{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "+" LA
        [ S{e1}; infixop4{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "*" LA
        [ S{e1}; "land"; S{e2} -> {| $e1 land $e2 |}
        | S{e1}; "lor"; S{e2} -> {| $e1 lor $e2 |}
        | S{e1}; "lxor"; S{e2} -> {| $e1 lxor $e2 |}
        | S{e1}; "mod"; S{e2} -> {| $e1 mod $e2 |}
        | S{e1}; infixop5{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "**" RA
        [ S{e1}; "asr"; S{e2} -> {| $e1 asr $e2 |}
        | S{e1}; "lsl"; S{e2} -> {| $e1 lsl $e2 |}
        | S{e1}; "lsr"; S{e2} -> {| $e1 lsr $e2 |}
        | S{e1}; infixop6{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "obj" RA
        [
        (* FIXME fun and function duplicated *)      
         "fun"; "[";  L0 match_case0 SEP "|"{a}; "]" -> {| fun [ $list:a ] |}
        | "function"; "[";  L0 match_case0 SEP "|"{a}; "]" -> {| function [ $list:a ] |}
        | "fun"; fun_def{e} -> e
        | "function"; fun_def{e} -> e
        | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" ->
            {| object ($csp) $cst end |} ]
       "unary minus" NA
        [ "-"; S{e} -> Expr.mkumin _loc "-" e
        | "-."; S{e} -> Expr.mkumin _loc "-." e ]
       "apply" LA
        [ S{e1}; S{e2} -> {| $e1 $e2 |}
        | "assert"; S{e} -> Expr.mkassert _loc e
        | "new"; class_longident{i} -> {| new $i |}
        | "lazy"; S{e} -> {| lazy $e |} ]
       "label" NA
        [ "~"; a_LIDENT{i}; ":"; S{e} -> {| ~ $i : $e |}
        | "~"; a_LIDENT{i} -> {| ~ $i |}
        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; S{e} -> {| ~ $i : $e |}
        (* Same remark for ?a:b *)
        | `OPTLABEL i; S{e} -> {| ? $i : $e |}
        | "?"; a_LIDENT{i}; ":"; S{e} -> {| ? $i : $e |}
        | "?"; a_LIDENT{i} -> {| ? $i |} ]
       "." LA
        [ S{e1}; "."; "("; S{e2}; ")" -> {| $e1 .( $e2 ) |}
        | S{e1}; "."; "["; S{e2}; "]" -> {| $e1 .[ $e2 ] |}
        | S{e1}; "."; "{"; comma_expr{e2}; "}" -> Expr.bigarray_get _loc e1 e2
        | S{e1}; "."; S{e2} -> {| $e1 . $e2 |}
        | S{e}; "#"; label{lab} -> {| $e # $lab |} ]
       "~-" NA
        [ "!"; S{e} ->  {| ! $e|}
        | prefixop{f}; S{e} -> {| $f $e |} ]
       "simple"
        [ `QUOTATION x -> AstQuotation.expand _loc x DynAst.expr_tag
        | `ANT (("exp"|""|"anti"|"`bool" |"tup"|"seq"|"int"|"`int"
                |"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"
                |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s) -> {| $(anti:mk_anti ~c:"expr" n s) |}
        | `INT(_,s) -> {|$int:s|}
        | `INT32(_,s) -> {|$int32:s|}
        | `INT64(_,s) -> {|$int64:s|}
        | `FLO(_,s) -> {|$flo:s|}
        | `CHAR(_,s) -> {|$chr:s|}
        | `STR(_,s) -> {|$str:s|}
        | `NATIVEINT(_,s) -> {|$nativeint:s|}
        | TRY module_longident_dot_lparen{i};S{e}; ")" ->
            {| let open $i in $e |}
        (* | TRY val_longident{i} -> {| $id:i |} *)
        | ident{i} -> {|$id:i|} (* FIXME logic was splitted here *)
        | "`"; a_ident{s} -> {| ` $s |}
        | "["; "]" -> {| [] |}
        | "[";sem_expr_for_list{mk_list}; "::"; expr{last}; "]" -> mk_list last
        | "["; sem_expr_for_list{mk_list}; "]" -> mk_list {| [] |}
        | "[|"; "|]" -> {| [| $({||}) |] |}
        | "[|"; sem_expr{el}; "|]" -> {| [| $el |] |}

        | "{"; `LID x ; "with"; label_expr_list{el}; "}" ->
            {| { ($lid:x) with $el }|} (* FIXME add antiquot support *)
        | "{"; label_expr_list{el}; "}" -> {| { $el } |}
        | "{"; "("; S{e}; ")"; "with"; label_expr_list{el}; "}" ->
            {| { ($e) with $el } |}
        | "{<"; ">}" -> {| {<>} |}
        | "{<"; field_expr_list{fel}; ">}" -> {| {< $fel >} |}
        | "("; ")" -> {| () |}
        | "("; S{e}; ":"; ctyp{t}; ")" -> {| ($e : $t) |}
        | "("; S{e}; ","; comma_expr{el}; ")" -> {| ( $e, $el ) |}
        | "("; S{e}; ";"; sequence{seq}; ")" -> Expr.mksequence ~loc:_loc {| $e; $seq |}
        | "("; S{e}; ";"; ")" -> Expr.mksequence ~loc:_loc e
        | "("; S{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" ->
            {| ($e : $t :> $t2 ) |}
        | "("; S{e}; ":>"; ctyp{t}; ")" -> {| ($e :> $t) |}
        | "("; S{e}; ")" -> e
        | "begin"; sequence{seq}; "end" -> Expr.mksequence ~loc:_loc seq
        | "begin"; "end" -> {| () |}
        | "("; "module"; module_expr{me}; ")" ->
            {| (module $me) |}
        | "("; "module"; module_expr{me}; ":"; package_type{pt}; ")" ->
            {| (module $me : $pt) |}  ] }
       sequence: (*FIXME*)
       [ "let"; opt_rec{rf}; binding{bi}; "in"; expr{e}; sequence'{k} ->
         k {| let $rec:rf $bi in $e |}
       | "let"; "try"; opt_rec{r}; binding{bi}; "in"; S{x}; "with"; match_case{a}; sequence'{k}
         ->
          k {| let try $rec:r $bi in $x with [ $a ] |}
       | "let"; opt_rec{rf}; binding{bi}; ";"; S{el} ->
           {| let $rec:rf $bi in $(Expr.mksequence ~loc:_loc el) |}
       | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e}; sequence'{k} ->
           k {| let module $m = $mb in $e |}
       | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; ";"; S{el} ->
           {| let module $m = $mb in $(Expr.mksequence ~loc:_loc el) |}
       | "let"; "open"; module_longident{i}; "in"; S{e} ->
           {| let open $id:i in $e |}
       | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"expr;" n s) |}
       | expr{e}; sequence'{k} -> k e ]
       sequence':
       [ -> fun e -> e
       | ";" -> fun e -> e
       | ";"; sequence{el} -> fun e -> {| $e; $el |} ]       
       infixop1:
       [  [ "&" | "&&" ]{x} -> {| $lid:x |} ]
       infixop0:
       [  [ "or" | "||" ]{x} -> {| $lid:x |} ]
       sem_expr_for_list:
       [ expr{e}; ";"; S{el} -> fun acc -> {| [ $e :: $(el acc) ] |}
       | expr{e}; ";" -> fun acc -> {| [ $e :: $acc ] |}
       | expr{e} -> fun acc -> {| [ $e :: $acc ] |} ]
       comma_expr:
       [ S{e1}; ","; S{e2} -> {| $e1, $e2 |}
       | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"expr," n s) |}
       | expr Level "top"{e} -> e ]
       dummy:
       [ -> () ] |};

  with "binding"
      {:extend|Gram
        binding_quot:
        [ binding{x} -> x | -> {||} ] 
        binding:
        [ `ANT (("binding"|"list" as n),s) ->
          {| $(anti:mk_anti ~c:"binding" n s) |}
        | `ANT ((""|"anti" as n),s); "="; expr{e} ->
            {| $(anti:mk_anti ~c:"patt" n s) = $e |}
        | `ANT ((""|"anti" as n),s) -> {| $(anti:mk_anti ~c:"binding" n s) |}
        | S{b1}; "and"; S{b2} -> {| $b1 and $b2 |}
        | let_binding{b} -> b ] 
        let_binding:
        [ patt{p}; fun_binding{e} -> {| $p = $e |} ] |};

  with "match_case"
    {:extend|Gram
      match_case:
      [ "["; L0 match_case0 SEP "|"{l}; "]" -> {|  $list:l  |} (* FIXME *)
      | patt{p}; "->"; expr{e} -> {| $pat:p -> $e |} ]
      match_case0:
      [ `ANT (("match_case"|"list"| "anti"|"" as n),s) -> {| $(anti:mk_anti ~c:"match_case" n s) |}
      | patt_as_patt_opt{p}; "when"; expr{w};  "->"; expr{e} ->  {| $pat:p when $w -> $e |}
      | patt_as_patt_opt{p}; "->";expr{e} -> {| $pat:p -> $e |} ]
      match_case_quot:
      [ L0 match_case0 SEP "|"{x} -> {| $list:x |}
      | -> {||} ]  |};
  with "rec_binding"
      {:extend|Gram
        rec_binding_quot:
        [ label_expr_list{x} -> x | -> {||} ]
        label_expr:
        [ `ANT (("rec_binding" |""|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"rec_binding" n s) |}
        | label_longident{i}; fun_binding{e} -> {| $id:i = $e |}
        | label_longident{i} -> {| $id:i = $(lid:Ident.to_lid i) |} ]
        field_expr:
        [ `ANT ((""|"bi"|"anti" |"list" as n),s) -> {| $(anti:mk_anti ~c:"rec_binding" n s) |}
        (* | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"rec_binding" n s) |} *)
        | label{l}; "=";  expr Level "top"{e} -> {| $lid:l = $e |} ]
        label_expr_list:
        [ label_expr{b1}; ";"; S{b2} -> {| $b1 ; $b2 |}
        | label_expr{b1}; ";"            -> b1
        | label_expr{b1}                 -> b1  ]
        field_expr_list:
        [ field_expr{b1}; ";"; S{b2} -> {| $b1 ; $b2 |}
        | field_expr{b1}; ";"            -> b1
        | field_expr{b1}                 -> b1  ] |};
  with "patt"
    {:extend|Gram local: patt_constr;

       patt_quot:
       [ patt{x}; ","; comma_patt{y} -> {| $x, $y |}
       | patt{x}; ";"; sem_patt{y} -> {| $x; $y |}
       | patt{x}; "="; patt{y} -> (*FIXME*)
           let i =
             match x with
             [ {@loc| $anti:s |} -> {:ident@loc| $anti:s |}
             | p -> FanAst.ident_of_patt p ] in
           {| $i = $y |}   (* {:patt| x=  y|} *)
       | patt{x} -> x
       | -> {||} ]
       patt_as_patt_opt:
       [ patt{p1}; "as"; patt{p2} -> {| ($p1 as $p2) |}
       | patt{p} -> p ]
       opt_class_self_patt:
       [ "("; patt{p}; ")" -> p
       | "("; patt{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
       | -> {||} ]
       patt_constr:
       [module_longident{i} -> {| $id:i |}
       |"`"; a_ident{s}  -> {| `$s|}
       |`ANT ((""|"pat"|"anti" as n), s) -> {|$(anti:mk_anti ~c:"patt" n s)|} ]
       patt:
       { "|" LA
        [ S{p1}; "|"; S{p2} -> {| $p1 | $p2 |} ]
       ".." NA
        [ S{p1}; ".."; S{p2} -> {| $p1 .. $p2 |} ]
       "apply" LA
        [ patt_constr{p1}; S{p2} ->
          match p2 with
            [ {| ($tup:p) |} ->
              List.fold_left (fun p1 p2 -> {| $p1 $p2 |}) p1
                (FanAst.list_of_patt p [])
            | _ -> {|$p1 $p2 |}  ]
        | patt_constr{p1} -> p1
        | "lazy"; S{p} -> {| lazy $p |}  ]
       "simple"
        [ `ANT ((""|"pat"|"anti"|"tup"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"
                |"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s)
          -> {| $(anti:mk_anti ~c:"patt" n s) |}
        | ident{i} -> {| $id:i |}
        | `INT(_,s) -> {|$int:s|}
        | `INT32(_,s) -> {|$int32:s|}
        | `INT64(_,s) -> {|$int64:s|}
        | `FLO(_,s) -> {|$flo:s|}
        | `CHAR(_,s) -> {|$chr:s|}
        | `STR(_,s) -> {|$str:s|}
        | "-"; `INT(_,s) -> {|$(int:String.neg s)|}
        | "-"; `INT32(_,s) -> {|$(int32:String.neg s)|}
        | "-"; `INT64(_,s) -> {|$(int64:String.neg s)|}
        | "-"; `NATIVEINT(_,s) -> {|$(int64:String.neg s)|}
        | "-"; `FLO(_,s) -> {|$(flo:String.neg s)|}
        | "["; "]" -> {| [] |}
        | "["; sem_patt_for_list{mk_list}; "::"; patt{last}; "]" -> mk_list last
        | "["; sem_patt_for_list{mk_list}; "]" -> mk_list {| [] |}
        | "[|"; "|]" -> {| [| $({||}) |] |}
        | "[|"; sem_patt{pl}; "|]" -> {| [| $pl |] |}
        | "{"; label_patt_list{pl}; "}" -> {| { $pl } |}
        | "("; ")" -> {| () |}
        | "("; "module"; a_UIDENT{m}; ")" -> {| (module $m) |}
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" -> {| ((module $m) : (module $pt)) |}
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as"; S{p2}; ")" -> {| ($p as $p2) |}
        | "("; S{p}; ","; comma_patt{pl}; ")" -> {| ($p, $pl) |}
        | "`"; a_ident{s} -> {| ` $s |}
          (* duplicated may be removed later with [patt Level "apply"] *)
        | "#"; type_longident{i} -> {| # $i |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.patt_tag
        | "_" -> {| _ |}
        | `LABEL i; S{p} -> {| ~ $i : $p |}
        | "~"; `ANT ((""|"lid" as n),i); ":"; S{p} -> {| ~ $(mk_anti n i) : $p |}
        | "~"; `ANT ((""|"lid" as n),i) -> {| ~ $(mk_anti n i) |}
        | "~"; `LID i -> {| ~ $i |}
        | `OPTLABEL i; "("; patt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANT ((""|"lid" as n),i); ":"; "("; patt_tcon{p}; eq_expr{f}; ")" -> f (mk_anti n i) p
        | "?"; `LID i -> {| ? $i |}
        | "?"; `ANT ((""|"lid" as n),i) -> {| ? $(mk_anti n i) |}
        | "?"; "("; ipatt_tcon{p}; ")" ->   {| ? ($p) |}
        | "?"; "("; ipatt_tcon{p}; "="; expr{e}; ")" -> {| ? ($p = $e) |} ] }
       ipatt:
        [ "{"; label_patt_list{pl}; "}" -> {| { $pl } |}
        | `ANT ((""|"pat"|"anti"|"tup" as n),s) -> {| $(anti:mk_anti ~c:"patt" n s) |}
        | "("; ")" -> {| () |}
        | "("; "module"; a_UIDENT{m}; ")" -> {| (module $m) |}
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" -> {| ((module $m) : (module $pt)) |}
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as"; S{p2}; ")" -> {| ($p as $p2) |}
        | "("; S{p}; ","; comma_ipatt{pl}; ")" -> {| ($p, $pl) |}
        | a_LIDENT{s} -> {| $lid:s |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.patt_tag                            
        | "_" -> {| _ |}
        | `LABEL i; S{p} -> {| ~ $i : $p |}
        | "~"; `ANT ((""|"lid" as n),i); ":"; S{p} -> {| ~ $(mk_anti n i) : $p |}
        | "~"; `ANT ((""|"lid" as n),i) -> {| ~ $(mk_anti n i) |}
        | "~"; `LID i -> {| ~ $i |}
        | `OPTLABEL i; "("; patt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANT ((""|"lid" as n),i); ":"; "("; patt_tcon{p}; eq_expr{f}; ")"
          -> f (mk_anti n i) p
        | "?"; `LID i -> {| ? $i |}
        | "?"; `ANT ((""|"lid" as n),i) -> {| ? $(mk_anti n i) |}
        | "?"; "("; ipatt_tcon{p}; ")" ->  {| ? ($p) |}
        | "?"; "("; ipatt_tcon{p}; "="; expr{e}; ")" -> {| ? ($p = $e) |} ]
       sem_patt:
       [`ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt;" n s) |}
       | patt{p1}; ";"; S{p2} -> {| $p1; $p2 |} 
       | patt{p}; ";" -> p
       | patt{p} -> p ] 
       sem_patt_for_list:
       [ patt{p}; ";"; S{pl} -> fun acc -> {| [ $p :: $(pl acc) ] |}
       | patt{p}; ";" -> fun acc -> {| [ $p :: $acc ] |}
       | patt{p} -> fun acc -> {| [ $p :: $acc ] |}  ]
       patt_tcon:
       [ patt{p}; ":"; ctyp{t} -> {| ($p : $t) |}
       | patt{p} -> p ]
       ipatt_tcon:
       [ `ANT((""|"anti" as n),s) -> {| $(anti:mk_anti ~c:"patt" n s ) |}
       | a_LIDENT{i} -> {| $lid:i |}
       | a_LIDENT{i};":"; ctyp{t} -> {| ($lid:i : $t) |}]
       eq_expr:
       [ "="; expr{e} -> fun i p -> {| ? $i : ($p = $e) |}
       | -> fun i p -> {| ? $i : ($p) |} ]
       comma_ipatt:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt," n s) |}
       | ipatt{p} -> p ]
       comma_patt:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt," n s) |}
       | patt{p} -> p ]
       label_patt_list:
       [ label_patt{p1}; ";"; S{p2} -> {| $p1 ; $p2 |}
       | label_patt{p1}; ";"; "_"       -> {| $p1 ; _ |}
       | label_patt{p1}; ";"; "_"; ";"  -> {| $p1 ; _ |}
       | label_patt{p1}; ";"            -> p1
       | label_patt{p1}                 -> p1   ] 
       label_patt:
       [ `ANT ((""|"pat"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"patt" n s) |}
       | `QUOTATION x -> AstQuotation.expand _loc x DynAst.patt_tag
       | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt;" n s) |}
       | label_longident{i}; "="; patt{p} -> {| $i = $p |}
       | label_longident{i} -> {| $i = $(lid:Ident.to_lid i) |} ] |};
    with "ctyp"
    {:extend|Gram
      ctyp_quot:
      [ more_ctyp{x}; ","; comma_ctyp{y} -> {| $x, $y |}
      | more_ctyp{x}; ";"; label_declaration_list{y} -> {| $x; $y |}
      | more_ctyp{x}; "|"; constructor_declarations{y} -> {| $x | $y |}
      | more_ctyp{x}; "of"; constructor_arg_list{y} -> {| $x of $y |}
      | more_ctyp{x}; "of"; constructor_arg_list{y}; "|"; constructor_declarations{z} -> {| $x of $y | $z |}
      | more_ctyp{x}; "of"; "&"; amp_ctyp{y} -> {| $x of & $y |}
      | more_ctyp{x}; "of"; "&"; amp_ctyp{y}; "|"; row_field{z} -> {| $x of & $y | $z |}
      | more_ctyp{x}; ":"; more_ctyp{y} -> {| $x : $y |}
      | more_ctyp{x}; ":"; more_ctyp{y}; ";"; label_declaration_list{z} -> {| $x : $y ; $z |}
      | more_ctyp{x}; "*"; star_ctyp{y} -> {| $x * $y |}
      | more_ctyp{x}; "&"; amp_ctyp{y} -> {| $x & $y |}
      | more_ctyp{x}; "and"; constructor_arg_list{y} -> {| $x and $y |}
      | more_ctyp{x} -> x
      | -> {||}  ]
      more_ctyp:
      [ "mutable"; S{x} -> {| mutable $x |}
      | "`"; a_ident{x} -> {| `$x |}
      | ctyp{x} -> x
      | type_parameter{x} -> x   ]
      type_parameter:
      [ `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | "'"; a_ident{i} -> {| '$lid:i |}
      | "+"; "'"; a_ident{i} -> {| +'$lid:i |}
      | "-"; "'"; a_ident{i} -> {| -'$lid:i |} ]
      type_ident_and_parameters: [ a_LIDENT{i}; L0 optional_type_parameter{tpl} -> (i, tpl) ]
      optional_type_parameter: (* overlapps with type_parameter *)
      [ `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | "'"; a_ident{i} -> {| '$lid:i |}
      | "+"; "'"; a_ident{i} -> {| +'$lid:i |}
      | "-"; "'"; a_ident{i} -> {| -'$lid:i |}
      | "+"; "_" -> `TyAnP _loc   (* FIXME *)
      | "-"; "_" -> `TyAnM _loc  
      | "_" -> {| _ |}  ]
      type_longident_and_parameters:
      [ type_longident{i}; type_parameters{tpl} -> tpl {| $id:i |}
      | `ANT ((""|"anti" as n),s) -> {|$(anti:mk_anti n s ~c:"ctyp")|}] 
      type_parameters:
      [ type_parameter{t1}; S{t2} -> fun acc -> t2 {| $acc $t1 |}
      | type_parameter{t} -> fun acc -> {| $acc $t |}
      | -> fun t -> t  ]
      opt_class_self_type:
      [ "("; ctyp{t}; ")" -> t | -> {||} ]
      type_constraint:
      [ "type" | "constraint" -> () ] 
      meth_list:
      [ meth_decl{m}; ";"; S{(ml, v) }  -> ({| $m; $ml |}, v)
      | meth_decl{m}; ";"; opt_dot_dot{v} -> (m, v)
      | meth_decl{m}; opt_dot_dot{v}      -> (m, v)  ]
      meth_decl:
      [ `ANT ((""|"typ" as n),s)        -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s)          -> {| $(anti:mk_anti ~c:"ctyp;" n s) |}
      | `QUOTATION x                       -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | a_LIDENT{lab}; ":"; poly_type{t} -> {| $lid:lab : $t |} ]
      opt_meth_list:
      [ meth_list{(ml, v) } -> {| < $ml $(..:v) > |}
      | opt_dot_dot{v}     -> {| < $(..:v) > |}  ]
      poly_type: [ ctyp{t} -> t ]
      package_type: [ module_type{p} -> p ] 
      unquoted_typevars:
      [ S{t1}; S{t2} -> {| $t1 $t2 |}
      | `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | a_ident{i} -> {| $lid:i |}   ] 
      row_field:
      [ `ANT ((""|"typ" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s) ->   {| $(anti:mk_anti ~c:"ctyp|" n s) |}
      | S{t1}; "|"; S{t2} -> {| $t1 | $t2 |}
      | "`"; a_ident{i} -> {| `$i |}
      | "`"; a_ident{i}; "of"; "&"; amp_ctyp{t} -> {| `$i of & $t |}
      | "`"; a_ident{i}; "of"; amp_ctyp{t} -> {| `$i of $t |}
      | ctyp{t} -> t ] 
      amp_ctyp:
      [ S{t1}; "&"; S{t2} -> {| $t1 & $t2 |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp&" n s) |}
      | ctyp{t} -> t ]
      name_tags:
      [ `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | S{t1}; S{t2} -> {| $t1 $t2 |}
      | "`"; a_ident{i} -> {| `$i |}  ]
      opt_polyt:
      [ ":"; poly_type{t} -> t  | -> {||} ]
      type_declaration:
      [ `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s) ->          {| $(anti:mk_anti ~c:"ctypand" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | S{t1}; "and"; S{t2} -> {| $t1 and $t2 |}
      |  type_ident_and_parameters{(n, tpl)}; opt_eq_ctyp{tk}; L0 constrain{cl}
        -> `TyDcl _loc n tpl tk cl ]
      constrain:
      [ "constraint"; ctyp{t1}; "="; ctyp{t2} -> (t1, t2) ]
      opt_eq_ctyp:
      [ "="; type_kind{tk} -> tk | -> {||} ] 
      type_kind: [ ctyp{t} -> t ] 
      typevars:
      [ S{t1}; S{t2} -> {| $t1 $t2 |}
      | `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT(("list" as n),s) ->     {| $(anti:mk_anti ~c:"forall" n s)|}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | "'"; a_ident{i} -> {| '$lid:i |} ]
      ctyp:
      { "==" NA (* FIXME should be more restrict *)
        [ S{t1}; "=="; S{t2} -> {| $t1 == $t2 |} ]
       "private" NA
        [ "private"; ctyp Level "alias"{t} -> {| private $t |} ]
       "alias" LA
        [ S{t1}; "as"; S{t2} ->   {| $t1 as $t2 |} ]
       "forall" LA
        [ "!"; typevars{t1}; "."; ctyp{t2} -> {| ! $t1 . $t2 |} ]
       "arrow" RA
        [ S{t1}; "->"; S{t2} ->  {| $t1 -> $t2 |} ]
       "label" NA
        [ "~"; a_LIDENT{i}; ":"; S{t} ->  {| ~ $i : $t |}
        | a_LABEL{i}; S{t}  ->  {| ~ $i : $t |}
        | "?"; a_LIDENT{i}; ":"; S{t} ->  {| ? $i : $t |}
        | a_OPTLABEL{i}; S{t} ->  {| ? $i : $t |} ]
       "apply" LA
        [ S{t1}; S{t2} ->
          let t = {| $t1 $t2 |} in
          try {| $(id:FanAst.ident_of_ctyp t) |}
          with [ Invalid_argument _ -> t ]]
       "." LA
        [ S{t1}; "."; S{t2} ->
            try {| $(id:FanAst.ident_of_ctyp t1).$(id:FanAst.ident_of_ctyp t2) |}
            with [ Invalid_argument s -> raise (XStream.Error s) ] ]
       "simple"
        [ "'"; a_ident{i} -> {| '$i |}
        | "_" -> {| _ |}
        | `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("tup" as n),s) ->  {| ($(tup:{| $(anti:mk_anti ~c:"ctyp" n s) |})) |}
        | `ANT (("id" as n),s) ->   {| $(id:{:ident| $(anti:mk_anti ~c:"ident" n s) |}) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{i} -> {| $lid:i |}
        | a_UIDENT{i} -> {| $uid:i |}
        | "("; S{t}; "*"; star_ctyp{tl}; ")" ->  {| ( $t * $tl ) |}
        | "("; S{t}; ")" -> t
        | "["; "]" -> {| [ ] |}
        | "["; constructor_declarations{t}; "]" -> {| [ $t ] |}
        | "["; "="; row_field{rfl}; "]" ->   {| [ = $rfl ] |} (* polymorphic variant *)
        | "["; ">"; "]" ->   {| [> ]|}
        | "["; ">"; row_field{rfl}; "]" ->    {| [ > $rfl ] |}
        | "["; "<"; row_field{rfl}; "]" ->     {| [ < $rfl ] |}
        | "["; "<"; row_field{rfl}; ">"; name_tags{ntl}; "]" ->   {| [ < $rfl > $ntl ] |}
        | "[<"; row_field{rfl}; "]" ->    {| [ < $rfl ] |}
        | "[<"; row_field{rfl}; ">"; name_tags{ntl}; "]" -> {| [ < $rfl > $ntl ] |}
        | "{"; label_declaration_list{t}; "}" -> {| { $t } |}
        | "#"; class_longident{i} -> {| # $i |}
        | "<"; opt_meth_list{t}; ">" -> t
        | "("; "module"; package_type{p}; ")" -> {| (module $p) |}  ] }
      star_ctyp:
      [ `ANT ((""|"typ" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp*" n s) |}
      | S{t1}; "*"; S{t2} ->   {| $t1 * $t2 |}
      | ctyp{t} -> t  ]
      constructor_declarations:
      [ `ANT ((""|"typ" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s) ->   {| $(anti:mk_anti ~c:"ctyp|" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | S{t1}; "|"; S{t2} ->        {| $t1 | $t2 |}
      | a_UIDENT{s}; "of"; constructor_arg_list{t} ->  {| $uid:s of $t |}
      | a_UIDENT{s}; ":"; ctyp{t} ->
          let (tl, rt) = Ctyp.to_generalized t in
          {| $uid:s : ($(FanAst.tyAnd_of_list tl) -> $rt) |}
      | a_UIDENT{s} ->  {| $uid:s |}  ]
      constructor_declaration:
      [ `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | a_UIDENT{s}; "of"; constructor_arg_list{t} ->   {| $uid:s of $t |}
      | a_UIDENT{s} ->    {| $uid:s |}  ]
      constructor_arg_list:
      [ `ANT (("list" as n),s) ->  {| $(anti:mk_anti ~c:"ctypand" n s) |}
      | S{t1}; "and"; S{t2} -> {| $t1 and $t2 |}
      | ctyp{t} -> t  ]
      label_declaration_list:
      [ label_declaration{t1}; ";"; S{t2} -> {| $t1; $t2 |}
      | label_declaration{t1}; ";"            -> t1
      | label_declaration{t1}                 -> t1  ]
      label_declaration:
      [ `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp;" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.ctyp_tag
      | a_LIDENT{s}; ":"; poly_type{t} ->      {| $lid:s : $t |}
      | a_LIDENT{s}; ":"; "mutable"; poly_type{t} ->  {| $lid:s : mutable $t |}  ]
      class_name_and_param:
      [ a_LIDENT{i}; "["; comma_type_parameter{x}; "]" -> (i, x)
      | a_LIDENT{i} -> (i, {||})  ]
      comma_type_parameter:
      [ S{t1}; ","; S{t2} -> {| $t1, $t2 |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp," n s) |}
      | type_parameter{t} -> t  ]
      opt_comma_ctyp:
      [ "["; comma_ctyp{x}; "]" -> x
      | -> {||}  ]
      comma_ctyp:
      [ S{t1}; ","; S{t2} -> {| $t1, $t2 |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp," n s) |}
      | ctyp{t} -> t  ]  |};
    with "ident"
    {:extend|Gram
      a_ident: [ a_LIDENT{i} -> i |  a_UIDENT{i} -> i ]
      ident_quot:
      { "."
        [ S{i}; "."; S{j} -> {| $i.$j  |} ]
        "simple"
        [ `ANT ((""|"id"|"anti"|"list" |"uid" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
        | `ANT (("lid" as n), s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
        | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s); "."; S{i} -> {| $(anti:mk_anti ~c:"ident" n s).$i |}
        | `LID i -> {| $lid:i |}
        | `UID i -> {| $uid:i |}
        | `UID s ; "." ; S{j} -> {|$uid:s.$j|}
        | "("; S{i};S{j}; ")" -> `IdApp _loc i j  ] }
      ident:
      [ `ANT ((""|"id"|"anti"|"list" |"uid" as n),s) ->
        {| $(anti:mk_anti ~c:"ident" n s) |}
      | `ANT (("lid" as n), s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
      | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s); "."; S{i} ->
          {| $(anti:mk_anti ~c:"ident" n s).$i |}
      | `LID i -> {| $lid:i |}
      | `UID i -> {| $uid:i |}
      | `UID s ; "." ; S{j} -> {|$uid:s.$j|}  ]
      dot_lstrings:
      [ `LID i -> [i]
      | `LID i ; "." ; S {xs} -> [i::xs] ]
      module_longident_dot_lparen:
      [ `ANT ((""|"id"|"anti"|"list"|"uid" as n),s); "."; "(" ->   {| $(anti:mk_anti ~c:"ident" n s) |}

      | `UID i; "."; S{l} -> {|$uid:i.$l|}
      | `UID i; "."; "(" -> {|$uid:i|}
      | `ANT (("uid"|"" as n),s); "."; S{l} -> {| $(anti:mk_anti ~c:"ident" n s).$l|}      
      (* | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |} *)
      (* | a_UIDENT{i}; "."; "(" -> {| $uid:i |} *) ]        
      module_longident:
      [ `ANT ((""|"id"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
      | `UID i; "."; S{l} -> {| $uid:i.$l|}
      | `UID i -> {|$uid:i|}
      | `ANT ((""|"uid" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
      | `ANT((""|"uid" as n), s); "."; S{l} -> {| $(anti:mk_anti ~c:"ident" n s).$l |}
      (* | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |} *)
      (* | a_UIDENT{i} -> {| $uid:i |} *) ]
      module_longident_with_app:
      { "apply"
        [ S{i}; S{j} -> {| ($i $j) |} ]
       "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
       "simple"
        [ `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
        | `UID i -> {|$uid:i|}
        (* | a_UIDENT{i} -> {| $uid:i |} *)
        | "("; S{i}; ")" -> i ] }
      type_longident: (* FIXME *)
      { "apply" (* No parens *)
        [ S{i}; S{j} -> {| ($i $j) |} ]
        "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
        "simple"
        [ `ANT ((""|"id"|"anti"|"list"|"uid"|"lid" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
        | `LID i -> {|$lid:i|}
        | `UID i -> {|$uid:i|}

        (* | a_LIDENT{i} -> {| $lid:i |} *)
        (* | a_UIDENT{i} -> {| $uid:i |} *)
        | "("; S{i}; ")" -> i ] }
      label_longident:
      [ `ANT ((""|"id"|"anti"|"list"|"lid" as n),s) ->  {| $(anti:mk_anti ~c:"ident" n s) |}
      | `LID i -> {|$lid:i|}
      | `UID i; "."; S{l} -> {|$uid:i.$l|}
      | `ANT((""|"uid" as n),s); "."; S{l} -> {|$(anti:mk_anti ~c:"ident" n s).$l|}
      (* | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |} *)
      (* | a_LIDENT{i} -> {| $lid:i |} *) ]
      class_type_longident: [ type_longident{x} -> x ]
      val_longident:[ ident{x} -> x ]
      class_longident:
      [ label_longident{x} -> x ]
      method_opt_override:
      [ "method"; "!" -> {:override_flag| ! |}
      | "method"; `ANT (((""|"override"|"anti") as n),s) ->
          `Ant (_loc,mk_anti ~c:"override_flag" n s)
            (* {:override_flag|$(anti:mk_anti ~c:"override_flag" n s)|} *)
      | "method" -> {:override_flag||}  ] 
      opt_override:
      [ "!" -> {:override_flag| ! |}
      | `ANT ((("!"|"override"|"anti") as n),s) ->
          (* {:override_flag|$(anti:mk_anti ~c:"override_flag" n s) |} *)
          `Ant (_loc,mk_anti ~c:"override_flag" n s)
      | -> {:override_flag||} ]
      
      value_val_opt_override:
      [ "val"; "!" -> {:override_flag| ! |}
      | "val"; `ANT (((""|"override"|"anti") as n),s) ->
          (* {:override_flag|$(anti:mk_anti ~c:"override_flag" n s) |} *)
            `Ant (_loc,mk_anti ~c:"override_flag" n s)
      | "val" -> {:override_flag||}   ] 
      opt_as_lident:  [ "as"; a_LIDENT{i} -> i  | -> ""  ] 
      label:[ a_LIDENT{i} -> i ]
      direction_flag:
      [ "to" -> {:direction_flag| to |}
      | "downto" -> {:direction_flag| downto |}
      | `ANT (("to"|"anti"|"" as n),s) ->
          (* {:direction_flag|$(anti:mk_anti ~c:"direction_flag" n s)|} *)
          `Ant (_loc,mk_anti ~c:"direction_flag" n s)
      ]

      opt_private:
      [ "private" -> {:private_flag| private |}
      | `ANT (("private"|"anti" as n),s) ->
          (* {:private_flag| $(anti:mk_anti ~c:"private_flag" n s)|} *)
          `Ant (_loc,mk_anti ~c:"private_flag" n s)
      | -> {:private_flag||}  ] 
      opt_mutable:
      [ "mutable" -> {:mutable_flag| mutable |}
      | `ANT (("mutable"|"anti" as n),s) ->
          (* {:mutable_flag| $(anti:mk_anti ~c:"mutable_flag" n s) |} *)
          `Ant (_loc,mk_anti ~c:"mutable_flag" n s)
      | -> {:mutable_flag||}  ] 
      opt_virtual:
      [ "virtual" -> {:virtual_flag| virtual |}
      | `ANT (("virtual"|"anti" as n),s) ->
          (* {:virtual_flag|$(anti:(mk_anti ~c:"virtual_flag" n s))|} *)
            (* let _ =  *)`Ant (_loc,mk_anti ~c:"virtual_flag" n s)
      | -> {:virtual_flag||}  ] 
      opt_dot_dot:
      [ ".." -> {:row_var_flag| .. |}
      | `ANT ((".."|"anti" as n),s) ->
          (* {:row_var_flag|$(anti:mk_anti ~c:"row_var_flag" n s) |} *)
          (* let _ =  *)`Ant (_loc,mk_anti ~c:"row_var_flag" n s)
      | -> {:row_var_flag||}  ]
      opt_rec:
      [ "rec" -> {:rec_flag| rec |}
      | `ANT (("rec"|"anti" as n),s) ->
          (* {:rec_flag|$(anti:mk_anti ~c:"rec_flag" n s) |} *)
            `Ant (_loc,mk_anti ~c:"rec_flag" n s)
      | -> {:rec_flag||} ] 
      a_UIDENT:
      [ `ANT ((""|"uid" as n),s) -> mk_anti n s
      | `UID s -> s ]
      a_LIDENT:
      [ `ANT ((""|"lid" as n),s) -> mk_anti n s
      | `LID s -> s ] 
      a_LABEL:
      [ "~"; `ANT (("" as n),s); ":" -> mk_anti n s
      | `LABEL s -> s ] 
      a_OPTLABEL:
      [ "?"; `ANT (("" as n),s); ":" -> mk_anti n s
      | `OPTLABEL s -> s ] 
      string_list:
      [ `ANT ((""|"str_list"),s) -> `Ant (_loc,mk_anti "str_list" s)
      | `STR (_, x); S{xs} -> `LCons x xs
      | `STR (_, x) -> `LCons x (`LNil _loc) ] 
      semi: [ ";" -> () ]
      rec_flag_quot:  [ opt_rec{x} -> x ]
      direction_flag_quot:  [ direction_flag{x} -> x ] 
      mutable_flag_quot: [  opt_mutable{x} -> x ] 
      private_flag_quot: [  opt_private{x} -> x ]
      virtual_flag_quot: [  opt_virtual{x} -> x ] 
      row_var_flag_quot: [  opt_dot_dot{x} -> x ] 
      override_flag_quot:[  opt_override{x} -> x ] 
      patt_eoi:  [ patt{x}; `EOI -> x ] 
      expr_eoi:  [ expr{x}; `EOI -> x ]  |};
  with "str_item"
    {:extend|Gram
    (* ml entrance *)    
      implem:
      [ "#"; a_LIDENT{n}; opt_expr{dp}; ";;" -> ([ {| # $n $dp |} ],  Some _loc)
      | str_item{si}; semi;  S{(sil, stopped)} -> ([si :: sil], stopped)
      | `EOI -> ([], None) ]
      str_items: (* FIXME dump seems to be incorrect *)
      [ `ANT ((""|"stri"|"anti"|"list" as n),s) -> {| $(anti:mk_anti n ~c:"str_item" s) |}
      | `ANT ((""|"stri"|"anti"|"list" as n),s); semi; S{st} -> {| $(anti:mk_anti n ~c:"str_item" s); $st |}
      | L0 [ str_item{st}; semi -> st ]{l} -> {| $list:l |} ]
      top_phrase:
      [ "#"; a_LIDENT{n}; opt_expr{dp}; ";;" -> Some {| # $n $dp |}
      | str_item{st}; semi -> Some st
      | `EOI -> None ]
      str_item_quot:
      [ "#"; a_LIDENT{n}; opt_expr{dp} -> {| # $n $dp |}
      | str_item{st1}; semi; S{st2} ->
          match st2 with
          [ {||} -> st1
          | _ -> {| $st1; $st2 |} ]
      | str_item{st} -> st
      | -> {||} ]
      str_item:
      { "top"
        [ "exception"; constructor_declaration{t} ->
            {| exception $t |}
        | "exception"; constructor_declaration{t}; "="; type_longident{i} ->
            {| exception $t = $i |}
        | "external"; a_LIDENT{i}; ":"; ctyp{t}; "="; string_list{sl} ->
            {| external $i : $t = $sl |}
        | "include"; module_expr{me} -> {| include $me |}
        | "module"; a_UIDENT{i}; module_binding0{mb} ->
            {| module $i = $mb |}
        | "module"; "rec"; module_binding{mb} ->
            {| module rec $mb |}
        | "module"; "type"; a_ident{i}; "="; module_type{mt} ->
            {| module type $i = $mt |}
        | "open"; `LID "lang"; `STR(_,s) -> (* FIXME put in the directive table*)
            begin
              AstQuotation.default:=s;
              {||}
            end
        | "open"; module_longident{i} -> {| open $i |}
        | "type"; type_declaration{td} ->
            {| type $td |}
        | "let"; opt_rec{r}; binding{bi}; "in"; expr{x} ->
              {| let $rec:r $bi in $x |}
        | "let"; opt_rec{r}; binding{bi} ->   match bi with
            [ {:binding| _ = $e |} -> {| $exp:e |}
            | _ -> {| let $rec:r $bi |} ]
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e} ->
              {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; expr{e} -> {| let open $id:i in $e |}
              
        | "let"; "try"; opt_rec{r}; binding{bi}; "in"; expr{x}; "with"; match_case{a} ->
             {| let try $rec:r $bi in $x with [ $a ]|} 
        | "class"; class_declaration{cd} ->  {| class $cd |}
        | "class"; "type"; class_type_declaration{ctd} -> {| class type $ctd |}
        | `ANT ((""|"stri"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"str_item" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.str_item_tag
        | expr{e} -> {| $exp:e |}
              (* this entry makes {| let $rec:r $bi in $x |} parsable *)
        ] }   |};

  with "class_sig_item"
    {:extend|Gram
      class_sig_item_quot:
      [ class_sig_item{x1}; semi; S{x2} ->
        match x2 with
        [ {||} -> x1
        | _ -> {| $x1; $x2 |} ]
      | class_sig_item{x} -> x
      | -> {||} ]
      class_signature:
      [ `ANT ((""|"csg"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_sig_item" n s) |}
      | `ANT ((""|"csg"|"anti"|"list" as n),s); semi; S{csg} ->
          {| $(anti:mk_anti ~c:"class_sig_item" n s); $csg |}
      | L0 [ class_sig_item{csg}; semi -> csg ]{l} -> {| $list:l |}]
      class_sig_item:
      [ `ANT ((""|"csg"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_sig_item" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_sig_item_tag
      | "inherit"; class_type{cs} ->   {| inherit $cs |}
      | "val"; opt_mutable{mf}; opt_virtual{mv}; label{l}; ":"; ctyp{t} ->
          {| val $mutable:mf $virtual:mv $l : $t |}
      | "method"; "virtual"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
          {| method virtual $private:pf $l : $t |}
      | "method"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
          {| method $private:pf $l : $t |}
      | "method"; opt_private{pf}; "virtual"; label{l}; ":"; poly_type{t} ->
          {| method virtual $private:pf $l : $t |}
      | type_constraint; ctyp{t1}; "="; ctyp{t2} -> {| type $t1 = $t2 |} ] |};  
  with "class_str_item"
    {:extend|Gram
      class_structure:
        [ `ANT ((""|"cst"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_str_item" n s) |}
        | `ANT ((""|"cst"|"anti"|"list" as n),s); semi; S{cst} ->
            {| $(anti:mk_anti ~c:"class_str_item" n s); $cst |}
        | L0 [ class_str_item{cst}; semi -> cst ]{l} -> FanAst.crSem_of_list l  ]
      class_str_item:
        [ `ANT ((""|"cst"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"class_str_item" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_str_item_tag
        | "inherit"; opt_override{o}; class_expr{ce}; opt_as_lident{pb} ->
            {| inherit $override:o $ce as $pb |}
        | value_val_opt_override{o}; opt_mutable{mf}; label{lab}; cvalue_binding{e}
          ->
            {| val $override:o $mutable:mf $lab = $e |}
        (* | value_val_opt_override{o}; opt_mutable{mf}; "virtual"; label{l}; ":"; *)
        (*       poly_type{t} -> *)
        (*         match o with *)
        (*         [ {:override_flag@_||} ->{| val virtual $mutable:mf $l : $t |} *)
        (*         | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]   *)

        | value_val_opt_override{o}; "virtual"; opt_mutable{mf}; label{l}; ":";
                poly_type{t} ->
                match o with
                [ {:override_flag@_||} ->{| val virtual $mutable:mf $l : $t |}
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]                    
        | method_opt_override{o}; "virtual"; opt_private{pf}; label{l}; ":";
                poly_type{t} ->
                match o with
                [ {:override_flag@_||} -> {| method virtual $private:pf $l : $t |}
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]  


        | method_opt_override{o}; opt_private{pf}; label{l}; opt_polyt{topt};
                fun_binding{e} ->
            {| method $override:o $private:pf $l : $topt = $e |}
        (* | method_opt_override{o}; opt_private{pf}; "virtual"; label{l}; ":"; *)
        (*      poly_type{t} -> *)
        (*        match o with *)
        (*         [ {:override_flag@_||} -> {| method virtual $private:pf $l : $t |} *)
        (*         | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]   *)

        | type_constraint; ctyp{t1}; "="; ctyp{t2} ->  {| type $t1 = $t2 |}
        | "initializer"; expr{se} -> {| initializer $se |} ]
      class_str_item_quot:
        [ class_str_item{x1}; semi; S{x2} ->
          match x2 with
          [ {||} -> x1
          | _ -> {| $x1; $x2 |} ]
        | class_str_item{x} -> x
        | -> {||} ]
    |};
    
  with "class_expr"
    {:extend|Gram
      class_expr_quot:
      [ S{ce1}; "and"; S{ce2} -> {| $ce1 and $ce2 |}
      | S{ce1}; "="; S{ce2} -> {| $ce1 = $ce2 |}
      | "virtual";   class_name_and_param{(i, ot)} ->  {| virtual $lid:i [ $ot ] |}
      | `ANT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
          let anti = `Ant (_loc,mk_anti ~c:"class_expr" n s) in
          {| $virtual:anti $id:i [ $ot ] |}
      | class_expr{x} -> x
      | -> {||} ]
      class_declaration:
      [ S{c1}; "and"; S{c2} -> {| $c1 and $c2 |}
      | `ANT ((""|"cdcl"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_expr" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_expr_tag
      | class_info_for_class_expr{ci}; class_fun_binding{ce} -> {| $ci = $ce |} ]
      class_fun_binding:
      [ "="; class_expr{ce} -> ce
      | ":"; class_type_plus{ct}; "="; class_expr{ce} -> {| ($ce : $ct) |}
      | ipatt{p}; S{cfb} -> {| fun $p -> $cfb |}  ]
      class_info_for_class_expr:
      [ opt_virtual{mv};  class_name_and_param{(i, ot)} -> {| $virtual:mv $lid:i [ $ot ] |}  ]
      class_fun_def:
      [ ipatt{p}; S{ce} -> {| fun $p -> $ce |}  | "->"; class_expr{ce} -> ce ]
      class_expr:
      { "top"
          [ "fun"; ipatt{p}; class_fun_def{ce} -> {| fun $p -> $ce |}
          | "function"; ipatt{p}; class_fun_def{ce} -> {| fun $p -> $ce |}
          | "let"; opt_rec{rf}; binding{bi}; "in"; S{ce} ->
              {| let $rec:rf $bi in $ce |} ]
        "apply" NA
          [ S{ce}; expr Level "label"{e} -> {| $ce $e |} ]
        "simple"
          [ `ANT ((""|"cexp"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"class_expr" n s) |}
          | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_expr_tag
          | class_longident_and_param{ce} -> ce
          | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" -> {| object ($csp) $cst end |}
          | "("; S{ce}; ":"; class_type{ct}; ")" -> {| ($ce : $ct) |}
          | "("; S{ce}; ")" -> ce ] }
      class_longident_and_param:
      [ class_longident{ci}; "["; comma_ctyp{t}; "]" -> {| $id:ci [ $t ] |}
      | class_longident{ci} -> {| $id:ci |}  ]  |};
  with "class_type"
    {:extend|Gram
      class_description:
      [ S{cd1}; "and"; S{cd2} -> {| $cd1 and $cd2 |}
      | `ANT ((""|"typ"|"anti"|"list" as n),s) ->
          {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_type_tag
      | class_info_for_class_type{ci}; ":"; class_type_plus{ct} -> {| $ci : $ct |}  ]
      class_type_declaration:
      [ S{cd1}; "and"; S{cd2} -> {| $cd1 and $cd2 |}
      | `ANT ((""|"typ"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_type_tag
      | class_info_for_class_type{ci}; "="; class_type{ct} -> {| $ci = $ct |} ]
      class_info_for_class_type:
      [ opt_virtual{mv};  class_name_and_param{(i, ot)} -> {| $virtual:mv $lid:i [ $ot ] |} ]
      class_type_quot:
      [ S{ct1}; "and"; S{ct2} -> {| $ct1 and $ct2 |}
      | S{ct1}; "="; S{ct2} -> {| $ct1 = $ct2 |}
      | S{ct1}; ":"; S{ct2} -> {| $ct1 : $ct2 |}
      | "virtual";  class_name_and_param{(i, ot)} -> {| virtual $lid:i [ $ot ] |}
      | `ANT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
          let anti = `Ant (_loc,mk_anti ~c:"class_type" n s) in
          {| $virtual:anti $id:i [ $ot ] |}
      | class_type_plus{x} -> x
      | -> {||}   ]
      class_type_plus:
      [ "["; ctyp{t}; "]"; "->"; S{ct} -> {| [ $t ] -> $ct |}
      | class_type{ct} -> ct ]
      class_type:
      [ `ANT ((""|"ctyp"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x DynAst.class_type_tag
      | class_type_longident_and_param{ct} -> ct
      | "object"; opt_class_self_type{cst}; class_signature{csg}; "end" ->
          {| object ($cst) $csg end |} ]
      class_type_longident_and_param:
      [ class_type_longident{i}; "["; comma_ctyp{t}; "]" -> {| $id:i [ $t ] |}
      | class_type_longident{i} -> {| $id:i |}   ] |} ;
end;

AstParsers.register_parser ("revise",apply);









