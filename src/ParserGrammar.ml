
open FanGrammar;
open FanGrammarTools;
open PreCast.Syntax;
open LibUtil;
open Lib;
open FanUtil;
open FanAst;

FanConfig.antiquotations := true;



{:create|Gram nonterminals nonterminalsclear
  delete_rule_header extend_header  qualuid qualid t_qualid
  (entry_name : Gram.t ([=`name of FanToken.name | `non] * FanGrammar.name))
  locals entry position assoc name string
  (pattern: Gram.t action_pattern)
  simple_expr delete_rules
  (simple_patt: Gram.t simple_patt)
  internal_patt|}  ;


{:extend|Gram
  (*

    {[
    with str t nonterminals {| U a b c d|} |> Ast2pt.print_str_item f;
    let a = U.mk "a"
    let b = U.mk "b"
    let c = U.mk "c"
    let d = U.mk "d"
    ]}
    It is very simple, may be improved to a depend on a simple engine
    It is used by language [create]
   *)
  nonterminals:
  [ [ "("; qualid{x} ; ":"; t_qualid{t};")" -> `dynamic(x,t)
  |  qualuid{t} -> `static(t)]{t};
    L0
      [ `Lid x  -> (_loc,x,None,None)
  | "("; `Lid x ;`STR(_,y); ")" ->(_loc,x,Some y,None)
  | "(";`Lid x ;`STR(_,y);ctyp{t};  ")" -> (_loc,x,Some y,Some t)
  | "("; `Lid x; ":"; ctyp{t}; OPT [`STR(_,y) -> y ]{y};  ")" -> (_loc,x,y,Some t) ] {ls}
    ->
with str_item
let mk =
  match t with
    [`static t -> {:expr| $id:t.mk |}
  |`dynamic(x,t) -> {:expr| $id:t.mk_dynamic $id:x |}] in   
let rest =
  List.map
    (fun
      (_loc,x,descr,ty) ->
        match (descr,ty) with
          [(Some d,None) ->
            {| let $lid:x = $mk $str:d |}
        | (Some d,Some typ) ->
            {| let $lid:x : $typ = $mk $str:d |}
        |(None,None) ->
            {| let $lid:x = $mk $str:x  |}
        | (None,Some typ) ->
            {| let $lid:x : $typ = $mk $str:x  |} ] ) ls in
            {| $list:rest |} ]
  (* {[
     with str t nonterminalsclear {| U a b c d|} |> Ast2pt.print_expr f;
     U.clear a; U.clear b; U.clear c; U.clear d
     ]}
     It's used by language [clear]
   *)
  nonterminalsclear:
  [ qualuid{t}; L0 [a_lident{x}->x ]{ls} ->
    let rest = List.map (fun x ->
      let _loc = loc_of x in
      {:expr| $id:t.clear $(id:(x:>ident)) |}) ls in
    {:expr| begin $list:rest end |} ]
|};


{:extend|Gram

  (* parse the header, return the current [grammar] and
     previous module name, it has side effect, and can not
     be used alone
     {[
     with str t extend_header {| U.M |};
     - : Ast.ident option * Ast.ident = (None, `Uid (, "Gram"))
     with str t extend_header {| U |};
     - : Ast.ident option * Ast.ident =
     (None, `Dot (, `Uid (, "U"), `Uid (, "M")))
     with str t extend_header {| (g:U.t) |};
     - : Ast.ident option * Ast.ident = (Some (`Lid (, "g")), `Uid (, "U"))
     ]}
     It should be fixed by introducing more advanced grammar features
   *)
  extend_header:
  [ "("; qualid{i}; ":"; t_qualid{t}; ")" -> 
    let old=gm() in 
    let () = grammar_module_name := t in
    (Some i,old)
  | qualuid{t}  ->
      let old = gm() in
      let () = grammar_module_name := t in 
      (None,old)
  | -> (None,gm())]

  (* the main entrance
     return an already converted expression
     {[
     with str t extend_body  {|
     nonterminalsclear:
     [ qualuid{t}; L0 [a_lident{x}->x ]{ls} -> ()] |} |> Ast2pt.print_expr f;

     Gram.extend (nonterminalsclear : 'nonterminalsclear Gram.t )
     (None,
     [(None, None,
     [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ));
     `Slist0
     (Gram.srules nonterminalsclear
     [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
     (Gram.mk_action
     (fun (x : 'a_lident)  (_loc : FanLoc.t)  -> (x : 'e__7 ))))])],
     (Gram.mk_action
     (fun (ls : 'e__7 list)  (t : 'qualuid)  (_loc : FanLoc.t)  ->
     (() : 'nonterminalsclear ))))])])
     ]}

     the function [text_of_functorial_extend] is the driving force
     it has type
     {[ Ast.loc ->
     Ast.ident option ->
     FanGrammar.name list option -> FanGrammar.entry list -> Ast.expr
     ]}
   *) 
  extend_body:
  [ extend_header{(gram,old)};  OPT locals{locals}; L1 entry {el} -> 
    let res = text_of_functorial_extend _loc  gram locals el in 
    let () = grammar_module_name := old in
    res      ]

  (*for side effets, parser action *)
  delete_rule_header:
  [ qualuid{g} -> let old = gm () in let () = grammar_module_name := g in old  ]

  delete_rule_body:
  [ delete_rule_header{old};  L0 delete_rules {es} ->
    let () = grammar_module_name := old  in 
    {:expr| begin $list:es end|}   ] 
  delete_rules:
  [ name{n} ;":"; "["; L1 [ L0 psymbol SEP ";"{sl} -> sl  ] SEP "|" {sls}; "]" ->
    expr_delete_rule _loc n sls ]

  (* parse qualified [X.X] *)
  qualuid:
  [ `Uid x; ".";  S{xs} -> `Dot(_loc,`Uid(_loc,x),xs) | `Uid x -> `Uid(_loc,x) ] 


  (* parse qualified  [X.Y.g]
     {[
     with str t qualid {| A.B.g |};
     - : Ast.ident = `Dot (, `Uid (, "A"), `Dot (, `Uid (, "B"), `Lid (, "g")))
     ]}
   *)
  qualid:
  [ `Uid x ; "."; S{xs} -> `Dot(_loc,`Uid(_loc,x),xs)
  | `Lid i -> `Lid(_loc,i)]

  (* parse qualified path ending with [X.t]
     {[
     with str t t_qualid {| A.U.t |};
     - : Ast.ident = `Dot (, `Uid (, "A"), `Uid (, "U"))
     ]}
   *)
  t_qualid:
  [ `Uid x; ".";  S{xs} -> `Dot(_loc,`Uid(_loc,x),xs)| `Uid x; "."; `Lid "t" -> `Uid(_loc,x) ] 


  (* get local name entry list *)
  
  locals:
  [ `Lid "local"; ":"; L1 name{sl}; ";" -> sl ]

  (* stands for the non-terminal  *)
  name:[ qualid{il} -> mk_name _loc il ] 

  (* parse entry name, accept a quotation name setup (FIXME)*)
  entry_name:
  [ qualid{il}; OPT[`STR(_,x)->x]{name} -> begin
    (match name with
      [ Some x -> (let old = !AstQuotation.default in
      (AstQuotation.default:= FanToken.resolve_name (`Sub [], x);
       `name old))
    | None -> `non], mk_name _loc il)
  end]

  (* return an entry [FanGrammar.entry]
     {[
     with str t entry {| entry:
     [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
     -> begin 
     match n with
     [`name old -> AstQuotation.default := old
     | _ -> ()];  
     mk_entry ~name:p ~pos ~levels
     end] |}

     
     ]}
   *)
  entry:
  [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
    -> begin 
      match n with
      [`name old -> AstQuotation.default := old
      | _ -> ()];  
        mk_entry ~name:p ~pos ~levels
    end]

  (* parse [position] and translate into [expr] node, fixme,
     delay the translation
   *)
  position:
  [ `Uid ("First"|"Last" as x ) ->   {:expr| $vrn:x |}
  | `Uid ("Before" | "After" | "Level" as x) ; string{n} -> {:expr| $vrn:x  $n |}
  | `Uid x -> failwithf "%s is not the right position:(First|Last) or (Before|After|Level)" x]

  level_list:
  [ "{"; L1 level {ll}; "}" -> ll  | level {l} -> [l]] (* FIXME L1 does not work here *)

  level:
  [  OPT [`STR (_, x)  -> x ]{label};  OPT assoc{assoc}; rule_list{rules} ->
    mk_level ~label ~assoc ~rules ]
  (* FIXME a conflict {:extend|Gram e:  "simple" ["-"; a_FLOAT{s} -> () ] |} *)


  (* parse association, and translate into [expr] node. FIXME  *)
  assoc:
  [ `Uid ("LA"|"RA"|"NA" as x) ->     {:expr| $vrn:x |} 
  | `Uid x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ]

  (*
    [retype_rule_list_with_patterns] do a simple transformation around rule list 
   *)
  rule_list:
  [ "["; "]" -> []
  | "["; L1 rule SEP "|"{rules}; "]" ->
    retype_rule_list_without_patterns _loc rules ]

  (* rules: *)
  (* [ rule{x} -> [x] *)
  (* | rule{x}; "|"; S{y} -> [x::y] *)
  (* | `Ant(("rule"|"" as n),s) -> ] *)
  (* return a [rule]
     {[
     with str t rule {|  `Uid ("LA"|"RA"|"NA" as x)   |};
     - : FanGrammar.rule =
     {prod =
     [{text =
     `Stok
     (,
     `Fun
     (,
     `Or
     (,
     `Case
     (,
     `App
     (, `Vrn (, "Uid"),
     `Or
     (, `Or (, `Str (, "LA"), `Str (, "RA")), `Str (, "NA"))),
     `Nil , `Id (, `Lid (, "true"))),
     `Case (, `Any , `Nil , `Id (, `Lid (, "false"))))),
     "Normal", "`Uid (\"LA\"|\"RA\"|\"NA\")");
     styp = `Tok ;
     pattern =
     Some
     (`App
     (, `Vrn (, "Uid"),
     `Alias
     (, `Or (, `Or (, `Str (, "LA"), `Str (, "RA")), `Str (, "NA")),
     `Lid (, "x"))))}];
     action = None}
     ]}  
   *)
  rule:
  [ L0 psymbol SEP ";"{psl}; OPT ["->"; expr{act}-> act]{action} ->
    mk_rule ~prod:psl ~action ]

  (* return symbol with patterns (may override inferred patterns) *)
  psymbol:
  [ symbol{s} ; OPT ["{"; pattern{p} ; "}" -> p ] {p} ->
    match p with [Some _ ->
      {(s) with pattern = (p: option action_pattern :> option patt) } | None -> s]  ] 

  (* return symbol with pattern(inferred) or None  *)
  symbol:
  [ `Uid ("L0"| "L1" as x); S{s}; OPT [`Uid "SEP"; symbol{t} -> t ]{sep } ->
    let () = check_not_tok s in
    (* let styp = `STapp _loc (`STlid _loc "list") s.styp in *)
    let styp = {:ctyp| list  $(s.styp) |} in 
    let text = mk_slist _loc
        (match x with
          ["L0" -> false | "L1" -> true
        | _ -> failwithf "only (L0|L1) allowed here"]) sep s in
    mk_symbol ~text ~styp ~pattern:None
  |`Uid "OPT"; S{s}  ->
    let () = check_not_tok s in
    let styp = {:ctyp| option $(s.styp) |} in 
    let text = `Sopt _loc s.text in
    mk_symbol  ~text ~styp ~pattern:None
  |`Uid "TRY"; S{s} ->
      let text = `Stry _loc s.text in
      mk_symbol  ~text ~styp:(s.styp) ~pattern:None
  | `Uid "PEEK"; S{s} ->
      let text = `Speek _loc s.text in
      mk_symbol ~text ~styp:(s.styp) ~pattern:None
  | `Uid "S" ->
      mk_symbol  ~text:(`Sself _loc)  ~styp:(`Self _loc "S") ~pattern:None
  |`Uid "N" ->
      mk_symbol  ~text:(`Snext _loc)   ~styp:(`Self _loc "N") ~pattern:None
  | "["; L1 rule SEP "|"{rl}; "]" ->
      let rl = retype_rule_list_without_patterns _loc rl in
      let t = new_type_var () in
      mk_symbol  ~text:(`Srules _loc (mk_srules _loc t rl ""))
        ~styp:({:ctyp|'$lid:t |} )
      ~pattern:None
  | simple_patt{p} -> 
      let (p,ls) = Expr.filter_patt_with_captured_variables (p : simple_patt :>patt) in
      match ls with
      [ [] -> mk_tok _loc ~pattern:p (`Tok _loc)
      | [(x,y)::ys] ->
        let restrict =
          List.fold_left (fun acc (x,y) -> {:expr| $acc && ( $x = $y ) |} )
            {:expr| $x = $y |} ys  in 
        mk_tok _loc ~restrict ~pattern:p (`Tok _loc) ]
        (* | `Uid ("Uid"|"Lid" as x) ; `Ant ((""),s) -> *)
        (*    let i = AntiquotSyntax.parse_ident _loc s in *)
        (*    let lid = gen_lid () in  *)
        (*    let pattern = {:patt| `$x $lid:lid  |} in *)
        (*    let match_fun = *)
        (*      {:expr| fun [$pat:pattern when $lid:lid = $lid:i -> true | _ -> false ] |} in *)
        (*    let descr = `Stok _loc match_fun "Normal" (x^) *)
        (*    {text;} *)
        (*    mk_tok _loc ~pattern (`Tok _loc) *)
        
  | `STR (_, s) ->
        mk_symbol  ~text:(`Skeyword _loc s) ~styp:(`Tok _loc) ~pattern:None
  | name{n};  OPT [`Uid "Level"; `STR (_, s) -> s ]{lev} ->
        mk_symbol  ~text:(`Snterm _loc n lev)
          ~styp:({:ctyp|'$(lid:n.tvar)|}) ~pattern:None
  | `Ant(("nt"|""),s); OPT [`Uid "Level"; `STR (_, s) -> s ]{lev} ->
        let i = parse_ident _loc s in
        let n = mk_name _loc i in
        mk_symbol ~text:(`Snterm _loc n lev)
          ~styp:({:ctyp|'$(lid:n.tvar)|}) ~pattern:None
  | "("; S{s}; ")" -> s ]
  

  simple_patt "patt":
  ["`"; luident{s}  ->  {|$vrn:s|}
  |"`"; luident{v}; `Ant (("" | "anti" as n) ,s) ->
    {| $vrn:v $(anti:mk_anti ~c:"patt" n s)|}
  |"`"; luident{s}; `STR(_,v) -> {| $vrn:s $str:v|}
  |"`"; luident{s}; `Lid x  -> {| $vrn:s $lid:x |}
  |"`"; luident{s}; "_" -> {|$vrn:s _|}
  |"`"; luident{s}; "("; L1 internal_patt SEP ","{v}; ")" ->
    match v with
      [ [x] ->  {| $vrn:s $x |}
    | [x::xs] -> {|$vrn:s ($x,$list:xs)|}
    | _ -> assert false ]  ]
  internal_patt "patt":
  {
   "as"
     [S{p1} ; "as";a_lident{s} -> {| ($p1 as $s) |} ]
     "|"
     [S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |} ]
     "simple"
     [ `STR(_,s) -> {| $str:s|}
| "_" -> {| _ |}
| `Lid x   ->  {| $lid:x|}
| "("; S{p}; ")" -> p] }

  pattern:
  [ `Lid i -> {:patt| $lid:i |}
  | "_" -> {:patt| _ |}
  | "("; pattern{p}; ")" -> p
  | "("; pattern{p1}; ","; L1 S SEP ","{ps}; ")"-> {:patt| ($p1, $list:ps)|}]
  string:
  [ `STR (_, s) -> {:expr| $str:s |}| `Ant ("", s) -> parse_expr _loc s ] (*suport antiquot for string*)


  symbol: 
  [`Uid ("FOLD0"|"FOLD1" as x); simple_expr{f}; simple_expr{e}; S{s} ->
    sfold _loc [x] f e s
  |`Uid ("FOLD0"|"FOLD1" as x ); simple_expr{f}; simple_expr{e}; S{s};`Uid ("SEP" as y);
    symbol{sep}  ->
      sfold ~sep _loc [x;y] f e s  ]

  simple_expr:
  [ a_lident{i} -> {:expr| $(id:(i:>ident)) |}
  | "("; expr{e}; ")" -> e ]  |};


let d = `Absolute["Fan";"Lang"];
AstQuotation.of_expr
  ~name:((d,  "extend")) ~entry:extend_body;
AstQuotation.of_expr
    ~name:((d,"delete")) ~entry:delete_rule_body;
AstQuotation.of_expr
    ~name:((d,"clear")) ~entry:nonterminalsclear;
AstQuotation.of_str_item
    ~name:((d,"create")) ~entry:nonterminals;

AstQuotation.add_quotation
    (d,"rule") rule
    ~mexpr:FanGrammar.Expr.meta_rule
    ~mpatt:FanGrammar.Patt.meta_rule
    ~expr_filter:(fun x-> x)
    ~patt_filter:(fun x->x);

AstQuotation.add_quotation
    (d,"entry") entry
    ~mexpr:FanGrammar.Expr.meta_entry
    ~mpatt:FanGrammar.Patt.meta_entry
    ~expr_filter:(fun x-> x)
    ~patt_filter:(fun x->x);

AstQuotation.add_quotation
    (d,"level") level
    ~mexpr:FanGrammar.Expr.meta_level
    ~mpatt:FanGrammar.Patt.meta_level
    ~expr_filter:(fun x-> x)
    ~patt_filter:(fun x->x);

AstQuotation.add_quotation
    (d,"symbol") psymbol
    ~mexpr:FanGrammar.Expr.meta_symbol
    ~mpatt:FanGrammar.Patt.meta_symbol
    ~expr_filter:(fun x -> x)
    ~patt_filter:(fun x-> x);

  





  
(* let _loc = FanLoc.ghost; *)
(* let u : FanGrammar.entry= {:entry| *)
(*   simple_expr: *)
(*   [ a_lident{i} -> {:expr| $(id:(i:>ident)) |} *)
(*   | "("; expr{e}; ")" -> e ] *)
(* |};   *)
(* let u : FanGrammar.rule = {:rule| *)
(*   a_lident{i} -> print_string i *)
(* |};   *)

(* let u : FanGrammar.symbol = {:symbol| *)
(*   "x" *)
(* |}; *)
















