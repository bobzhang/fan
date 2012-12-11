open FanGrammar;
open FanGrammarTools;
open Fan.Syntax;
open LibUtil;
open Lib;
open FanUtil;
module Ast = Camlp4Ast;

FanConfig.antiquotations := true;



{:extend.create|Gram nonterminals nonterminalsclear
  delete_rule_header extend_header  qualuid qualid t_qualid
  (entry_name : Gram.t ([=`name of string | `non] * FanGrammar.name))
  locals entry position assoc name string pattern simple_expr delete_rules
  simple_patt internal_patt|}  ;

{:extend|Gram
  extend_header:
  [ "("; qualid{i}; ":"; t_qualid{t}; ")"
     -> 
       let old=gm() in 
       let () = grammar_module_name := t in
       (Some i,old)
  | qualuid{t}  -> 
      let old = gm() in
      let () = grammar_module_name := t in 
      (None,old)
  | -> (None,gm())]

  nonterminals:
  [ [ "("; qualid{x} ; ":"; t_qualid{t};")" -> `dynamic(x,t) |  qualuid{t} -> `static(t)]{t};
    L0
      [ a_LIDENT{x} -> (x,None,None)
      | "(";a_LIDENT{x};`STR(_,y); ")" ->(x,Some y,None)
      | "(";a_LIDENT{x};`STR(_,y);ctyp{t};  ")" -> (x,Some y,Some t)
      | "(";a_LIDENT{x}; ":"; ctyp{t}; OPT [`STR(_,y) -> y ]{y};  ")" -> (x,y,Some t) ] {ls}
    ->
     with "str_item"
      let mk =
        match t with
        [`static t -> {:expr| $id:t.mk |}
        |`dynamic(x,t) -> {:expr| $id:t.mk_dynamic $id:x |}] in   
      let rest =
        List.map
          (fun
            (x,descr,ty) ->
              match (descr,ty) with
              [(Some d,None) -> {| let $lid:x = $mk $str:d |}
              | (Some d,Some typ) -> {| let $lid:x : $typ = $mk $str:d |}
              |(None,None) -> {| let $lid:x = $mk $str:x  |}
              | (None,Some typ) -> {| let $lid:x : $typ = $mk $str:x  |} ] ) ls in
                {| $list:rest |} ]
  nonterminalsclear:
  [ qualuid{t}; L0 a_LIDENT {ls} ->
    let rest = List.map (fun x -> {:expr| $id:t.clear $lid:x |}) ls in
    {:expr| begin $list:rest end |} ]
  extend_body:
  [ extend_header{(gram,old)};  OPT locals{locals}; L1 entry {el} -> 
    let res = text_of_functorial_extend _loc  gram locals el in 
    let () = grammar_module_name := old in
    res      ]
  delete_rule_header: (*for side effets, parser action *)
  [ qualuid{g} ->
    let old = gm () in
    let () = grammar_module_name := g in
    old  ]

  delete_rule_body:
  [ delete_rule_header{old};  L0 delete_rules {es} ->
       let () = grammar_module_name := old  in 
       {:expr| begin $list:es end|}   ] 
  delete_rules:
  [ name{n} ;":"; "["; L1 [ L0 psymbol SEP ";"{sl} -> sl  ] SEP "|" {sls}; "]" ->
    let rest = List.map (fun sl  ->
      let (e,b) = expr_of_delete_rule _loc n sl in
      {:expr| $(id:gm()).delete_rule $e $b |}) sls in
    {:expr| begin $list:rest end |}   ]

  qualuid:
  [ `UID x; ".";  S{xs} -> {:ident| $uid:x.$xs |}
  | `UID x -> {:ident| $uid:x |} ] 

  qualid:
  [ `UID x; ".";  S{xs} -> {:ident| $uid:x.$xs |}
  | `UID i -> {:ident| $uid:i |}
  | `LID i -> {:ident| $lid:i |} ]

  t_qualid:
  [ `UID x; ".";  S{xs} -> {:ident| $uid:x.$xs |}
  | `UID x; "."; `LID "t" -> {:ident| $uid:x |} ] 

  locals:
  [ `LID "local"; ":"; L1 name{sl}; ";" -> sl ]
  name:[ qualid{il} -> mk_name _loc il ] 

  entry_name:
  [ qualid{il}; OPT[`STR(_,x)->x]{name} -> begin
    (match name with
       [ Some x -> (let old = !AstQuotation.default in (AstQuotation.default:=x;`name old))
     | None -> `non], mk_name _loc il)
       (* (mk_name _loc il,`name) *)
   end]
  entry:
  [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
     -> begin 
       match n with
         [`name old -> AstQuotation.default := old
       | _ -> ()];  
         mk_entry ~name:p ~pos ~levels
     end]
  position:
  [ `UID ("First"|"Last" as x ) ->   {:expr| `$uid:x |}
  | `UID ("Before" | "After" | "Level" as x) ; string{n} ->
    {:expr| `$uid:x  $n |}
  | `UID x -> failwithf "%s is not the right position:(First|Last) or (Before|After|Level)" x]
  level_list:
  [ "{"; L0 level {ll}; "}" -> ll  | level {l} -> [l]]

  level:
  [  OPT [`STR (_, x)  -> x ]{label};  OPT assoc{assoc}; rule_list{rules} ->
     mk_level ~label ~assoc ~rules ]
   (* FIXME a conflict {:extend|Gram e:  "simple" ["-"; a_FLOAT{s} -> () ] |} *)

  assoc:
  [ `UID ("LA"|"RA"|"NA" as x) ->     {:expr| `$uid:x |} 
  | `UID x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ]

  rule_list:
  [ "["; "]" -> [] | "["; L1 rule SEP "|"{rules}; "]" ->  retype_rule_list_without_patterns _loc rules ]

  rule:
  [ L0 psymbol SEP ";"{psl}; OPT ["->"; expr{act}-> act]{action} ->
    mk_rule ~prod:psl ~action ]

  psymbol:
  [ symbol{s} ; OPT ["{"; pattern{p} ; "}" -> p ] {p} ->
    match p with [Some _ -> {(s) with pattern = p } | None -> s]  ] 

  symbol:
  [ `UID ("L0"| "L1" as x); S{s}; OPT [`UID "SEP"; symbol{t} -> t ]{sep } ->
    let () = check_not_tok s in
    let styp = `STapp _loc (`STlid _loc "list") s.styp in
    let text = mk_slist _loc
        (match x with
         ["L0" -> false | "L1" -> true
         | _ -> failwithf "only (L0|L1) allowed here"]) sep s in
       mk_symbol ~text ~styp ~pattern:None
  |`UID "OPT"; S{s}  ->
      let () = check_not_tok s in
      let styp = `STapp _loc (`STlid _loc "option") s.styp in
      let text = `TXopt _loc s.text in
      mk_symbol  ~text ~styp ~pattern:None
  |`UID "TRY"; S{s} ->
      let text = `TXtry _loc s.text in
      mk_symbol  ~text ~styp:(s.styp) ~pattern:None
  | `UID "PEEK"; S{s} ->
      let text = `TXpeek _loc s.text in
      mk_symbol ~text ~styp:(s.styp) ~pattern:None
  | `UID "S" ->
      mk_symbol  ~text:(`TXself _loc)  ~styp:(`STself _loc "S") ~pattern:None
  |`UID "N" ->
      mk_symbol  ~text:(`TXnext _loc)   ~styp:(`STself _loc "N") ~pattern:None
  | "["; L1 rule SEP "|"{rl}; "]" ->
      let rl = retype_rule_list_without_patterns _loc rl in
      let t = new_type_var () in
      mk_symbol  ~text:(`TXrules _loc (mk_srules _loc t rl ""))
        ~styp:(`STquo _loc t) ~pattern:None
  | simple_patt{p} -> 
    let (p,ls) = Expr.filter_patt_with_captured_variables p in
    match ls with
    [ [] -> mk_tok _loc ~pattern:p (`STtok _loc)
    | [(x,y)::ys] ->
        let restrict =
          List.fold_left (fun acc (x,y) -> {:expr| $acc && ( $x = $y ) |} )
            {:expr| $x = $y |} ys  in 
        mk_tok _loc ~restrict ~pattern:p (`STtok _loc) ]
        (* | `UID ("UID"|"LID" as x) ; `ANT ((""),s) -> *)
        (*    let i = AntiquotSyntax.parse_ident _loc s in *)
        (*    let lid = gen_lid () in  *)
        (*    let pattern = {:patt| `$x $lid:lid  |} in *)
        (*    let match_fun = *)
        (*      {:expr| fun [$pat:pattern when $lid:lid = $lid:i -> true | _ -> false ] |} in *)
        (*    let descr = `TXtok _loc match_fun "Normal" (x^) *)
        (*    {text;} *)
        (*    mk_tok _loc ~pattern (`STtok _loc) *)
        
    | `STR (_, s) ->
        mk_symbol  ~text:(`TXkwd _loc s) ~styp:(`STtok _loc) ~pattern:None
    | name{n};  OPT [`UID "Level"; `STR (_, s) -> s ]{lev} ->
        mk_symbol  ~text:(`TXnterm _loc n lev) ~styp:(`STquo _loc n.tvar) ~pattern:None
    | `ANT(("nt"|""),s); OPT [`UID "Level"; `STR (_, s) -> s ]{lev} ->
        let i = (* AntiquotSyntax. *)parse_ident _loc s in
        let n = mk_name _loc i in
        mk_symbol ~text:(`TXnterm _loc n lev) ~styp:(`STquo _loc n.tvar) ~pattern:None
    | "("; S{s}; ")" -> s ]
  

  simple_patt "patt":
   ["`"; a_ident{s}  -> {| `$s |}
   |"`"; a_ident{v}; `ANT (("" | "anti" as n) ,s) -> {| `$v $(anti:mk_anti ~c:"patt" n s)|}
   |"`"; a_ident{s}; `STR(_,v) -> {| `$s $str:v |}
   |"`"; a_ident{s}; `LID x  ->  {| `$s $lid:x |}
   |"`"; a_ident{s}; "_" ->  {| `$s _ |}           
   |"`"; a_ident{s}; "("; L1 internal_patt SEP ","{v}; ")" ->
       match v with
       [ [x] -> {| `$s $x |}
       | [x::xs] -> {| `$s ($x,$list:xs) |}
       | _ -> assert false ]  ]

  internal_patt "patt":
  {
   "as"
     [ S{p1} ; "as"; S{p2} -> {| ($p1 as $p2) |}]  
     "|"
     [S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |} ]
     "simple"
     [ `STR(_,s) -> {| $str:s|}
     | "_" -> {| _ |}
     | `LID x   -> (* {| $(id:{:ident|$lid:x|}) |} *)  {| $lid:x|}
     | "("; S{p}; ")" -> p ] }

  pattern:
  [ `LID i -> {:patt| $lid:i |}
  | "_" -> {:patt| _ |}
  | "("; pattern{p}; ")" -> {:patt| $p |}
  | "("; pattern{p1}; ","; L1 S SEP ","{ps}; ")"-> {:patt| ($p1, $list:ps)|}]
  string:
  [ `STR (_, s) -> {:expr| $str:s |}
  | `ANT ("", s) -> parse_expr _loc s ] (*suport antiquot for string*)


  symbol: 
  [`UID ("FOLD0"|"FOLD1" as x); simple_expr{f}; simple_expr{e}; S{s} ->
     sfold _loc [x] f e s
   |`UID ("FOLD0"|"FOLD1" as x ); simple_expr{f}; simple_expr{e}; S{s};`UID ("SEP" as y); symbol{sep}  ->
       sfold ~sep _loc [x;y] f e s  ]

  simple_expr:
   [ a_LIDENT{i} -> {:expr| $lid:i |}
   | "("; expr{e}; ")" -> e ]  |};

AstQuotation.add_quotation_of_expr ~name:"extend" ~entry:extend_body;
  (* built in extend support *)
AstQuotation.add_quotation_of_expr ~name:"delete" ~entry:delete_rule_body;
  (* built in delete support *)
AstQuotation.add_quotation_of_expr ~name:"extend.clear"
    ~entry:nonterminalsclear;

AstQuotation.add_quotation_of_str_item ~name:"extend.create"
    ~entry:nonterminals;

Options.add ("-meta_action", (FanArg.Set meta_action), "Undocumented"); (* FIXME *)




















