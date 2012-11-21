open LibUtil;
open FanUtil;
open Lib;
open GramLib;

module IdDebugParser = struct
  let name = "Camlp4DebugParser";
  let version = Sys.ocaml_version;
end;

module MakeDebugParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;  
  let debug_mode =
    try
      let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
      let rec loop acc i =
        try
          let pos = String.index_from str i ':' in
          loop (SSet.add (String.sub str i (pos - i)) acc) (pos + 1)
        with
        [ Not_found ->
            SSet.add (String.sub str i (String.length str - i)) acc ] in
      let sections = loop SSet.empty 0 in
      if SSet.mem "*" sections then fun _ -> true
      else fun x -> SSet.mem x sections
    with [ Not_found -> fun _ -> false ];

  let mk_debug_mode _loc = fun [ None -> {:expr| Debug.mode |}
    | Some m -> {:expr| $uid:m.Debug.mode |} ];

  let mk_debug _loc m fmt section args =
    let call = Expr.apply {:expr| Debug.printf $str:section $str:fmt |} args in
      {:expr| if $(mk_debug_mode _loc m) $str:section then $call else () |};
  {:extend| Gram local: start_debug end_or_in ;  
    expr "expr":
     [ start_debug{m}; `LID section; `STR (_, fmt);
       L0 expr Level "."{args}; end_or_in{x} ->
      match (x, debug_mode section) with
      [ (None,   false) -> {| () |}
      | (Some e, false) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> {| let () = $(mk_debug _loc m fmt section args) in $e |} ]  ] 
    end_or_in:
    [ "end" -> None
    | "in"; expr{e} -> Some e  ] 
    start_debug:
    [ `LID "debug" -> None
    | `LID "camlp4_debug" -> Some "Camlp4"  ]  |};


end;

module IdGrammarParser = struct
  let name = "Camlp4GrammarParser";
  let version = Sys.ocaml_version;
end;

  
module MakeGrammarParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  open FanGrammar;
  open FanGrammarTools;
  FanConfig.antiquotations := true;
  {:extend|Gram
      local:
      delete_rule_header extend_header  qualuid qualid t_qualid entry_name
      locals entry position assoc name string pattern simple_expr delete_rules;
    extend_header:
       [ "("; qualid{i}; ":"; t_qualid{t}; ")"
         -> 
           let old=gm() in 
           let () = grammar_module_name := t in
           (Some i,old)
       | qualuid{t}
         -> 
           let old = gm() in
           let () = grammar_module_name := t in 
           (None,old)
       | -> (None,gm())] 
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
     [ delete_rule_header{old};  L0 delete_rules {es}
       ->
         let () = grammar_module_name := old  in 
         {:expr| begin $list:es end|}   ] 
    delete_rules:
     [ name{n} ;":"; "["; L1 [ L0 symbol SEP ";"{sl} -> sl  ] SEP "|" {sls}; "]"
       ->
         let rest = List.map (fun sl  ->
           let (e,b) = expr_of_delete_rule _loc n sl in
           {:expr| $(id:gm()).delete_rule $e $b |}) sls in
         {:expr| begin $list:rest end |}   
        ]
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
         [ Some x -> (let old = !Quotation.default in (Quotation.default:=x;`name old))
         | None -> `non], mk_name _loc il)
       (* (mk_name _loc il,`name) *)
     end]
    entry:
     [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
       -> begin 
         match n with
         [`name old -> Quotation.default := old
         | _ -> ()];  
           mk_entry ~name:p ~pos ~levels
       end]
    position:
      [ `UID ("First"|"Last" as x ) ->   {:expr| `$uid:x |}
      | `UID ("Before" | "After" | "Level" as x) ; string{n} ->
            {:expr| `$uid:x  $n |}     ]
    level_list:
      [ "{"; L0 level (* SEP "|" *) {ll}; "}" -> ll
      | level {l} -> [l]]
    level:
      [  OPT [`STR (_, x)  -> x ]{label};  OPT assoc{assoc}; rule_list{rules} ->
            mk_level ~label ~assoc ~rules ]
    assoc:
      [ `UID ("LA"|"RA"|"NA" as x) ->
         {:expr| `$uid:x |} 
       | `UID x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ]
    rule_list:
      [ "["; "]" -> []
      | "["; L1 rule SEP "|"{rules}; "]" ->  retype_rule_list_without_patterns _loc rules ]
    rule:
      [ L0 psymbol SEP ";"{psl}; OPT ["->"; expr{act}-> act]{action}
        ->
          mk_rule ~prod:psl ~action ]
    psymbol:
      [ symbol{s} ; OPT ["{"; pattern{p} ; "}" -> p ] {p}
        ->
          match p with [Some _ -> {(s) with pattern = p } | None -> s]  ] 
    symbol:
     [ `UID ("L0"| "L1" as x); S{s}; OPT [`UID "SEP"; symbol{t} -> t ]{sep }
       ->
         let () = check_not_tok s in
         let styp = `STapp _loc (`STlid _loc "list") s.styp in
         let text = slist _loc
             (match x with
             ["L0" -> false | "L1" -> true
             | _ -> failwithf "only (L0|L1) allowed here"]) sep s in
         mk_symbol ~text ~styp ~pattern:None
     |`UID "OPT"; S{s}
       ->
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
     | "["; L0 rule SEP "|"{rl}; "]" ->
        let rl = retype_rule_list_without_patterns _loc rl in
        let t = new_type_var () in
        mk_symbol  ~text:(`TXrules _loc (srules _loc t rl ""))
          ~styp:(`STquo _loc t) ~pattern:None
          
     (* | "`"; a_ident{i}; OPT patt{p} -> *)
     (*    let p = match p with *)
     (*      [None -> {:patt| `$i |} *)
     (*      |Some p -> {:patt| `$i $p |} ] in  *)
     | PEEK "`"; patt{p} ->  (*otherwise conflict with name *)
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
         let i = AntiquotSyntax.parse_ident _loc s in
         let n = mk_name _loc i in
         mk_symbol ~text:(`TXnterm _loc n lev) ~styp:(`STquo _loc n.tvar) ~pattern:None
     | "("; S{s}; ")" -> s ]
   pattern:
     [ `LID i -> {:patt| $lid:i |}
     | "_" -> {:patt| _ |}
     | "("; pattern{p}; ")" -> {:patt| $p |}
     | "("; pattern{p1}; ","; L1 S SEP ","{ps}; ")"-> {:patt| ($p1, $list:ps)|}]
   string:
    [ `STR (_, s) -> {:expr| $str:s |}
    | `ANT ("", s) -> AntiquotSyntax.parse_expr _loc s ] (*suport antiquot for string*)

   symbol: 
     [`UID ("FOLD0"|"FOLD1" as x); simple_expr{f}; simple_expr{e}; S{s} ->
            sfold _loc [x] f e s
     |`UID ("FOLD0"|"FOLD1" as x ); simple_expr{f}; simple_expr{e}; S{s};`UID ("SEP" as y); symbol{sep}
       ->
         sfold ~sep _loc [x;y] f e s  ]
   simple_expr:
     [ a_LIDENT{i} -> {:expr| $lid:i |}
     | "("; expr{e}; ")" -> e ]  |};

  Options.add ("-split_ext", (FanArg.Set split_ext),
               "Split EXTEND by functions to turn around a PowerPC problem.");

  Options.add ("-split_gext", (FanArg.Set split_ext),"Old name for the option -split_ext.");

  Options.add ("-meta_action", (FanArg.Set meta_action), "Undocumented"); (* FIXME *)
end;

module IdListComprehension = struct
  let name = "Camlp4ListComprehension";
  let version = Sys.ocaml_version;
end;

module MakeListComprehension (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;

   {:delete|Gram expr: [ "["; sem_expr_for_list; "]"] |};



  let comprehension_or_sem_expr_for_list =
    Gram.mk "comprehension_or_sem_expr_for_list";
   {:extend| Gram
      local: item;
    expr: Level "simple"
     [ "["; comprehension_or_sem_expr_for_list{e}; "]" -> e ]
    comprehension_or_sem_expr_for_list:
     [  expr Level "top"{e}; ";"; sem_expr_for_list{mk} ->
       {:expr| [ $e :: $(mk {:expr| [] |}) ] |}
     | expr Level "top"{e}; ";" -> {:expr| [$e] |}
     | expr Level "top"{e}; "|"; L1 item SEP ";"{l} -> Expr.compr _loc e l
     | expr Level "top"{e} -> {:expr| [$e] |} ]   
    item:
      (* NP: These rules rely on being on this particular order. Which should
             be improved. *)(* FIXME LL *)
      [  TRY [ patt{p}; "<-" -> p]{p} ;  expr Level "top"{e} -> `gen (p, e)
      | expr Level "top"{e} -> `cond e ] |};
  if is_revised ~expr ~sem_expr_for_list then
     {:extend|Gram
       comprehension_or_sem_expr_for_list:
       [  expr Level "top"{e}; ";"; sem_expr_for_list{mk}; "::"; expr{last} ->
            {:expr| [ $e :: $(mk last) ] |}
       | expr Level "top"{e}; "::"; expr{last} ->
           {:expr| [ $e :: $last ] |} ] |}
  else ();

end;
  
module IdMacroParser = struct
  let name = "Camlp4MacroParser";
  let version = Sys.ocaml_version;
end;

(*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
     INCLUDE <string>

  At toplevel (signature item):

     DEFINE <uident>
     IFDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     IFNDEF <uident> THEN <signature_items> [ ELSE <signature_items> ] (END | ENDIF)
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     IFNDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     DEFINE <lident> = <expression> IN <expression>
     __FILE__
     __LOCATION__
     LOCATION_OF <parameter>

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)
     IFNDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)

  As Camlp4 options:

     -D<uident> or -D<uident>=expr   define <uident> with optional let <expr>
     -U<uident>                      undefine it
     -I<dir>                         add <dir> to the search path for INCLUDE'd files

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.

  You can also define a local macro in an expression usigng the DEFINE ... IN form.
  Note that local macros have lowercase names and can not take parameters.

  If a macro is defined to = NOTHING, and then used as an argument to a function,
  this will be equivalent to function taking one less argument. Similarly,
  passing NOTHING as an argument to a macro is equivalent to "erasing" the
  corresponding parameter from the macro body.

  The toplevel statement INCLUDE <string> can be used to include a
  file containing macro definitions and also any other toplevel items.
  The included files are looked up in directories passed in via the -I
  option, falling back to the current directory.

  The expression __FILE__ returns the current compiled file name.
  The expression __LOCATION__ returns the current location of itself.
  If used inside a macro, it returns the location where the macro is
  called.
  The expression (LOCATION_OF parameter) returns the location of the given
  macro parameter. It cannot be used outside a macro definition.

*)



module MakeMacroParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  type item_or_def 'a =
    [ SdStr of 'a
    | SdDef of string and option (list string * Ast.expr)
    | SdUnd of string
    | SdITE of bool and list (item_or_def 'a) and list (item_or_def 'a)
    | SdLazy of Lazy.t 'a ];
  let defined = ref [];
  let is_defined i = List.mem_assoc i !defined;
  let incorrect_number loc l1 l2 =
    FanLoc.raise loc
      (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1)));
  let define eo x = begin 
      match eo with
      [ Some ([], e) ->
        {:extend|Gram
        expr: Level "simple"
          [ `UID $x -> (new Ast.reloc _loc)#expr e ]
        patt: Level "simple"
          [ `UID $x ->
            let p = Expr.substp _loc [] e
            in (new Ast.reloc _loc)#patt p ] |}
      | Some (sl, e) ->
          {:extend| Gram
            expr: Level "apply"
            [ `UID $x; S{param} ->
              let el =  match param with
              [ {:expr| ($tup:e) |} -> Ast.list_of_expr e []
              | e -> [e] ]  in
              if List.length el = List.length sl then
                let env = List.combine sl el in
                (new Expr.subst _loc env)#expr e
              else
                incorrect_number _loc el sl ]
          patt: Level "simple"
            [ `UID $x; S{param} ->
              let pl = match param with
              [ {:patt| ($tup:p) |} -> Ast.list_of_patt p []
              | p -> [p] ] in
              if List.length pl = List.length sl then
                let env = List.combine sl pl in
                let p = Expr.substp _loc env e in
                (new Ast.reloc _loc)#patt p
              else
                incorrect_number _loc pl sl ] |}
      | None -> () ];
      defined := [(x, eo) :: !defined]
    end;

  let undef x =
    try
      begin
        let eo = List.assoc x !defined in
        match eo with
        [ Some ([], _) -> {:delete| Gram expr: [`UID $x ]  patt: [`UID $x ] |}
        | Some (_, _) ->  {:delete| Gram expr: [`UID $x; S ] patt: [`UID $x; S] |}
        | None -> () ];
        defined := list_remove x !defined;
      end
    with
    [ Not_found -> () ];

  let parse_def s =
    match Gram.parse_string expr (FanLoc.mk "<command line>") s with
    [ {:expr| $uid:n |} -> define None n
    | {:expr| $uid:n = $e |} -> define (Some ([],e)) n
    | _ -> invalid_arg s ];

  (* This is a list of directories to search for INCLUDE statements. *)
  let include_dirs = ref [];

  (* Add something to the above, make sure it ends with a slash. *)
  let add_include_dir str =
    if str <> "" then
      let str =
        if String.get str ((String.length str)-1) = '/'
        then str else str ^ "/"
      in include_dirs := !include_dirs @ [str]
    else ();

  let parse_include_file rule =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file ->
      let file =
        try (List.find (dir_ok file) (!include_dirs @ ["./"])) ^ file
        with [ Not_found -> file ]
      in
      let ch = open_in file in
      let st = Stream.of_channel ch in
        Gram.parse rule (FanLoc.mk file) st;

  let rec execute_macro nil cons = fun
    [ SdStr i -> i
    | SdDef (x, eo) -> begin  define eo x; nil  end
    | SdUnd x -> begin  undef x; nil  end
    | SdITE (b, l1, l2) -> execute_macro_list nil cons (if b then l1 else l2)
    | SdLazy l -> Lazy.force l ]

  and execute_macro_list nil cons = fun
  [ [] -> nil
  | [hd::tl] -> (* The evaluation order is important here *)
    let il1 = execute_macro nil cons hd in
    let il2 = execute_macro_list nil cons tl in
    cons il1 il2 ] ;

  (* Stack of conditionals. *)
  let stack = Stack.create () ;

  (* Make an SdITE let by extracting the result of the test from the stack. *)
  let make_SdITE_result st1 st2 =
   let test = Stack.pop stack in
   SdITE test st1 st2 ;

  type branch = [ Then | Else ];

  (* Execute macro only if it belongs to the currently active branch. *)
  let execute_macro_if_active_branch _loc nil cons branch macro_def =
   let test = Stack.top stack in
   let item =
     if (test && branch=Then) || ((not test) && branch=Else) then
      execute_macro nil cons macro_def
     else (* ignore the macro *)
      nil
   in SdStr(item)
   ;

   {:extend|Gram
     local: macro_def macro_def_sig uident_eval_ifdef uident_eval_ifndef
     else_macro_def else_macro_def_sig else_expr smlist_then smlist_else sglist_then
     sglist_else endif opt_macro_value uident ;
    str_item: First
      [ macro_def{x} ->
            execute_macro {:str_item||} (fun a b -> {:str_item| $a; $b |}) x ]
    sig_item: First
      [ macro_def_sig{x} ->
            execute_macro {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) x ]
    macro_def:
      [ "DEFINE"; uident{i}; opt_macro_value{def} -> SdDef i def
      | "UNDEF";  uident{i} -> SdUnd i
      | "IFDEF";  uident_eval_ifdef;  "THEN"; smlist_then{st1}; else_macro_def{st2} ->
          make_SdITE_result st1 st2
      | "IFNDEF"; uident_eval_ifndef; "THEN"; smlist_then{st1}; else_macro_def{st2} ->
          make_SdITE_result st1 st2
      | "INCLUDE"; `STR (_, fname) ->
            SdLazy (lazy (parse_include_file str_items fname)) ]
    macro_def_sig:
      [ "DEFINE"; uident{i} -> SdDef i None
      | "UNDEF";  uident{i} -> SdUnd i
      | "IFDEF";  uident_eval_ifdef;  "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2}
        ->
          make_SdITE_result sg1 sg2
      | "IFNDEF"; uident_eval_ifndef; "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
            make_SdITE_result sg1 sg2
      | "INCLUDE"; `STR (_, fname) ->
            SdLazy (lazy (parse_include_file sig_items fname)) ]
    uident_eval_ifdef:
      [ uident{i} -> Stack.push (is_defined i) stack ]
    uident_eval_ifndef:
      [ uident{i} -> Stack.push (not (is_defined i)) stack ]
    else_macro_def:
      [ "ELSE"; smlist_else{st}; endif -> st
      | endif -> [] ]
    else_macro_def_sig:
      [ "ELSE"; sglist_else{st}; endif -> st
      | endif -> [] ]
    else_expr:
      [ "ELSE"; expr{e}; endif -> e
      | endif -> {:expr| () |} ]
    smlist_then:
      [ L1 [ macro_def{d}; semi ->
        execute_macro_if_active_branch _loc {:str_item||} (fun a b -> {:str_item| $a; $b |}) Then d
      | str_item{si}; semi -> SdStr si ]{sml} -> sml ]
    smlist_else:
      [ L1 [ macro_def{d}; semi ->
        execute_macro_if_active_branch _loc {:str_item||} (fun a b -> {:str_item| $a; $b |}) Else d
      | str_item{si}; semi -> SdStr si ]{sml} -> sml ]
    sglist_then:
      [ L1 [ macro_def_sig{d}; semi ->
        execute_macro_if_active_branch _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Then d
      | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ]   
    sglist_else:
      [ L1 [ macro_def_sig{d}; semi ->
        execute_macro_if_active_branch _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Else d
      | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ]  
    endif:
      [ "END" -> ()
      | "ENDIF" -> () ]
    opt_macro_value:
      [ "("; L1 [ `LID x -> x ] SEP ","{pl}; ")"; "="; expr{e} -> Some (pl, e)
      | "="; expr{e} -> Some ([], e)
      | -> None ]
    expr: Level "top"
      [ "IFDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
        if is_defined i then e1 else e2
      | "IFNDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
          if is_defined i then e2 else e1
      | "DEFINE"; `LID i; "="; expr{def}; "IN"; expr{body} ->
          (new Expr.subst _loc [(i, def)])#expr body ] 
    patt:
      [ "IFDEF"; uident{i}; "THEN"; patt{p1};  "ELSE"; patt{p2}; endif ->
        if is_defined i then p1 else p2
      | "IFNDEF"; uident{i}; "THEN"; patt{p1}; "ELSE"; patt{p2}; endif ->
          if is_defined i then p2 else p1 ]
    uident:
      [ `UID i -> i ]
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)
    expr: Before "simple"
      [ "`";  [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
                | "DEFINE" | "IN" ]{kwd} -> {:expr| `$uid:kwd |}
      | "`"; a_ident{s} -> {:expr| ` $s |} ]
    patt: Before "simple"
      [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ]{kwd} ->
            {:patt| `$uid:kwd |}
      | "`"; a_ident{s} -> {:patt| ` $s |} ] |};


  Options.add ("-D", (FanArg.String parse_def),"<string> Define for IFDEF instruction.");
  Options.add ("-U", (FanArg.String undef), "<string> Undefine for IFDEF instruction.");
  Options.add ("-I", (FanArg.String add_include_dir), "<string> Add a directory to INCLUDE search path.");
end;
module MakeNothing (Syn : Sig.Camlp4Syntax) = struct
 module Ast = Camlp4Ast ;
 (* Remove NOTHING and expanse __FILE__ and __LOCATION__ *)
 Syn.AstFilters.register_str_item_filter (Ast.map_expr Expr.map_expr)#str_item;
end;

module IdRevisedParser = struct
  let name = "Camlp4OCamlRevisedParser";
  let version = Sys.ocaml_version;
end;

module MakeRevisedParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  FanConfig.constructors_arity := false;

  let help_sequences () =
    begin
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
\n";
      flush stderr;
      exit 1
    end
  ;
  Options.add ("-help_seq", (FanArg.Unit help_sequences), "Print explanations about new sequences and exit.");
  Gram.clear a_CHAR;
  Gram.clear a_FLOAT;
  Gram.clear a_INT;
  Gram.clear a_INT32;
  Gram.clear a_INT64;
  Gram.clear a_LABEL;
  Gram.clear a_LIDENT;
  Gram.clear a_NATIVEINT;
  Gram.clear a_OPTLABEL;
  Gram.clear a_STRING;
  Gram.clear a_UIDENT;
  Gram.clear a_ident;
  Gram.clear amp_ctyp;
  Gram.clear and_ctyp;
  Gram.clear match_case;
  Gram.clear match_case0;
  Gram.clear match_case_quot;
  Gram.clear binding;
  Gram.clear binding_quot;
  Gram.clear rec_binding_quot;
  Gram.clear class_declaration;
  Gram.clear class_description;
  Gram.clear class_expr;
  Gram.clear class_expr_quot;
  Gram.clear class_fun_binding;
  Gram.clear class_fun_def;
  Gram.clear class_info_for_class_expr;
  Gram.clear class_info_for_class_type;
  Gram.clear class_longident;
  Gram.clear class_longident_and_param;
  Gram.clear class_name_and_param;
  Gram.clear class_sig_item;
  Gram.clear class_sig_item_quot;
  Gram.clear class_signature;
  Gram.clear class_str_item;
  Gram.clear class_str_item_quot;
  Gram.clear class_structure;
  Gram.clear class_type;
  Gram.clear class_type_declaration;
  Gram.clear class_type_longident;
  Gram.clear class_type_longident_and_param;
  Gram.clear class_type_plus;
  Gram.clear class_type_quot;
  Gram.clear comma_ctyp;
  Gram.clear comma_expr;
  Gram.clear comma_ipatt;
  Gram.clear comma_patt;
  Gram.clear comma_type_parameter;
  Gram.clear constrain;
  Gram.clear constructor_arg_list;
  Gram.clear constructor_declaration;
  Gram.clear constructor_declarations;
  Gram.clear ctyp;
  Gram.clear ctyp_quot;
  Gram.clear cvalue_binding;
  Gram.clear direction_flag;
  Gram.clear dummy;
  Gram.clear eq_expr;
  Gram.clear expr;
  Gram.clear expr_eoi;
  Gram.clear expr_quot;
  Gram.clear field_expr;
  Gram.clear field_expr_list;
  Gram.clear fun_binding;
  Gram.clear fun_def;
  Gram.clear ident;
  Gram.clear ident_quot;
  Gram.clear implem;
  Gram.clear interf;
  Gram.clear ipatt;
  Gram.clear ipatt_tcon;
  Gram.clear label;
  Gram.clear label_declaration;
  Gram.clear label_declaration_list;
  Gram.clear label_expr_list;
  Gram.clear label_expr;
  (* Gram.clear label_ipatt; *)
  (* Gram.clear label_ipatt_list; *)
  Gram.clear label_longident;
  Gram.clear label_patt;
  Gram.clear label_patt_list;
  Gram.clear labeled_ipatt;
  Gram.clear let_binding;
  Gram.clear meth_list;
  Gram.clear meth_decl;
  Gram.clear module_binding;
  Gram.clear module_binding0;
  Gram.clear module_binding_quot;
  Gram.clear module_declaration;
  Gram.clear module_expr;
  Gram.clear module_expr_quot;
  Gram.clear module_longident;
  Gram.clear module_longident_with_app;
  Gram.clear module_rec_declaration;
  Gram.clear module_type;
  Gram.clear module_type_quot;
  Gram.clear more_ctyp;
  Gram.clear name_tags;
  Gram.clear opt_as_lident;
  Gram.clear opt_class_self_patt;
  Gram.clear opt_class_self_type;
  Gram.clear opt_comma_ctyp;
  Gram.clear opt_dot_dot;
  Gram.clear opt_eq_ctyp;
  Gram.clear opt_expr;
  Gram.clear opt_meth_list;
  Gram.clear opt_mutable;
  Gram.clear opt_polyt;
  Gram.clear opt_private;
  Gram.clear opt_rec;
  Gram.clear opt_virtual;
  Gram.clear opt_when_expr;
  Gram.clear patt;
  Gram.clear patt_as_patt_opt;
  Gram.clear patt_eoi;
  Gram.clear patt_quot;
  Gram.clear patt_tcon;
  Gram.clear phrase;
  Gram.clear poly_type;
  Gram.clear row_field;
  Gram.clear sem_expr;
  Gram.clear sem_expr_for_list;
  Gram.clear sem_patt;
  Gram.clear sem_patt_for_list;
  Gram.clear semi;
  Gram.clear sequence;
  Gram.clear sig_item;
  Gram.clear sig_item_quot;
  Gram.clear sig_items;
  Gram.clear star_ctyp;
  Gram.clear str_item;
  Gram.clear str_item_quot;
  Gram.clear str_items;
  Gram.clear top_phrase;
  Gram.clear type_constraint;
  Gram.clear type_declaration;
  Gram.clear type_ident_and_parameters;
  Gram.clear type_kind;
  Gram.clear type_longident;
  Gram.clear type_longident_and_parameters;
  Gram.clear type_parameter;
  Gram.clear type_parameters;
  Gram.clear typevars;
  Gram.clear use_file;
  Gram.clear val_longident;
  Gram.clear with_constr;
  Gram.clear with_constr_quot;


  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  setup_op_parser prefixop
    (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1);

  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  setup_op_parser infixop0
    (fun x -> (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list_first_char_ok && symbolchar x 1));

  let list = ['@'; '^'] in
  setup_op_parser infixop1
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['+'; '-'] in
  setup_op_parser infixop2
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['*'; '/'; '%'; '\\'] in
  setup_op_parser infixop3
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1);

  setup_op_parser infixop4
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2);


  FanToken.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.setup_parser sem_expr begin
    let symb1 = Gram.parse_origin_tokens expr in
    let symb = parser
      [ [< (`ANT (("list" as n), s), ti) >] ->
        let _loc = Gram.token_location ti in
        {:expr| $(anti:mk_anti ~c:"expr;" n s) |}
      | [< a = symb1 >] -> a ] in
    let rec kont al =
      parser
      [ [< (`KEYWORD ";", _); a = symb; 's >] ->
        let _loc =
          FanLoc.merge
            (Ast.loc_of_expr al) (Ast.loc_of_expr a) in
        kont {:expr| $al; $a |} s
      | [< >] -> al ] in
    parser [< a = symb; 's >] -> kont a s
  end;

  (* main grammar extension [Line 826 ~ Line 2123]*)

  {:extend|Gram
    module_expr "module_expr":
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
            Quotation.expand _loc x DynAst.module_expr_tag
        | module_longident{i} -> {| $id:i |}
        | "("; S{me}; ":"; module_type{mt}; ")" ->
            {| ( $me : $mt ) |}
        | "("; S{me}; ")" -> {| $me |}
        | "("; "val"; expr{e}; ")" -> (* val *)
            {| (val $e) |}  (* first class modules *)
        | "("; "val"; expr{e}; ":"; package_type{p}; ")" ->
            {| (val $e : $p) |} ] } |};


    with "str_item"
    {:extend|Gram str_item:
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
              Quotation.default:=s;
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
        | "let"; "open"; module_longident{i}; "in"; expr{e} ->
              {| let open $id:i in $e |}

        | "class"; class_declaration{cd} ->
            {| class $cd |}
        | "class"; "type"; class_type_declaration{ctd} ->
            {| class type $ctd |}
        | `ANT ((""|"stri"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"str_item" n s) |}
            (*
              first, it gives "mk_anti ~c:"str_item" n s" , and then through
              the meta operation, it gets
              (Ast.StAnt (_loc, ( (mk_anti ~c:"str_item" n s) )))
             *)
        | `QUOTATION x -> Quotation.expand _loc x DynAst.str_item_tag
        | expr{e} -> {| $exp:e |}
        (* this entry makes {| let $rec:r $bi in $x |} parsable *)
        ] } |};

    {:extend|Gram
      module_binding0 "module_expr":
      { RA
        [ "("; a_UIDENT{m}; ":"; module_type{mt}; ")"; S{mb} ->
            {| functor ( $m : $mt ) -> $mb |}
        | ":"; module_type{mt}; "="; module_expr{me} ->
            {| ( $me : $mt ) |}
        | "="; module_expr{me} -> {| $me |} ] }
    module_binding "module_binding":
        [ S{b1}; "and"; S{b2} ->
            {| $b1 and $b2 |}
        | `ANT (("module_binding"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `ANT (("" as n),s) ->
            {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `ANT (("" as n),m); ":"; module_type{mt}; "="; module_expr{me} ->
            {| $(mk_anti n m) : $mt = $me |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} ->
            {| $m : $mt = $me |} ] |};
   {:extend|Gram
    module_rec_declaration "module_binding":
      [ S{m1}; "and"; S{m2} -> {| $m1 and $m2 |}
      | `ANT ((""|"module_binding"|"anti"|"list" as n),s) ->
          {| $(anti:mk_anti ~c:"module_binding" n s) |}
      | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
      | a_UIDENT{m}; ":"; module_type{mt} -> {| $m : $mt |} ]
    with_constr "with_constr":
     [ S{wc1}; "and"; S{wc2} -> {| $wc1 and $wc2 |}
     | `ANT ((""|"with_constr"|"anti"|"list" as n),s) ->
         {| $(anti:mk_anti ~c:"with_constr" n s) |}
     | `QUOTATION x -> Quotation.expand _loc x DynAst.with_constr_tag
     | "type"; `ANT ((""|"typ"|"anti" as n),s); "="; ctyp{t} ->
         {| type $(anti:mk_anti ~c:"ctyp" n s) = $t |}
     | "type"; type_longident_and_parameters{t1}; "="; ctyp{t2} ->
         {| type $t1 = $t2 |}
     | "module"; module_longident{i1}; "="; module_longident_with_app{i2} ->
         {| module $i1 = $i2 |}
     | "type"; `ANT ((""|"typ"|"anti" as n),s); ":="; ctyp{t} ->
         {| type $(anti:mk_anti ~c:"ctyp" n s) := $t |}
     | "type"; type_longident_and_parameters{t1}; ":="; ctyp{t2} ->
         {| type $t1 := $t2 |}
     | "module"; module_longident{i1}; ":="; module_longident_with_app{i2} ->
         {| module $i1 := $i2 |} ] |};
    {:extend|Gram
    module_type "module_type":
      { "top"
        [ "functor"; "("; a_UIDENT{i}; ":"; S{t}; ")"; "->"; S{mt} ->
            {| functor ( $i : $t ) -> $mt |} ]
        "with"
        [ S{mt}; "with"; with_constr{wc} ->
            {| $mt with $wc |} ]
        "apply"
        [ S{mt1}; S{mt2}; dummy -> ModuleType.app mt1 mt2 ]
        "."
        [ S{mt1}; "."; S{mt2} -> ModuleType.acc mt1 mt2 ]
        "sig"
        [ "sig"; sig_items{sg}; "end" ->
            {| sig $sg end |} ]
       "simple"
        [ `ANT ((""|"mtyp"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"module_type" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_type_tag
        | module_longident_with_app{i} -> {| $id:i |}
        | "'"; a_ident{i} -> {| ' $i |}
        | "("; S{mt}; ")" -> {| $mt |}
        | "module"; "type"; "of"; module_expr{me} ->
            {| module type of $me |} ] }
    module_declaration "module_type":
      { RA
        [ ":"; module_type{mt} -> {| $mt |}
        | "("; a_UIDENT{i}; ":"; module_type{t}; ")"; S{mt} -> {| functor ( $i : $t ) -> $mt |} ] }  |};

    {:extend|Gram
    sig_item "sig_item":
      [ `ANT ((""|"sigi"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"sig_item" n s) |}
      | `QUOTATION x -> Quotation.expand _loc x DynAst.sig_item_tag
      | "exception"; constructor_declaration{t} ->
          {| exception $t |}
      | "external"; a_LIDENT{i}; ":"; ctyp{t}; "="; string_list{sl} ->
          {| external $i : $t = $sl |}
      | "include"; module_type{mt} -> {| include $mt |}
      | "module"; a_UIDENT{i}; module_declaration{mt} ->
          {| module $i : $mt |}
      | "module"; "rec"; module_rec_declaration{mb} ->
          {| module rec $mb |}
      | "module"; "type"; a_ident{i}; "="; module_type{mt} ->
          {| module type $i = $mt |}
      | "module"; "type"; a_ident{i} ->
          {| module type $i |}
      | "open"; module_longident{i} -> {| open $i |}
      | "type"; type_declaration{t} ->
          {| type $t |}
      | "val"; a_LIDENT{i}; ":"; ctyp{t} ->
          {| val $i : $t |}
      | "class"; class_description{cd} ->
          {| class $cd |}
      | "class"; "type"; class_type_declaration{ctd} ->
          {| class type $ctd |} ] |};


    
    with "expr"
    {:extend|Gram
    lang: [ `STR(_,s) ->
      (let old = !Quotation.default in
      begin Quotation.default := s; old end) ]
    expr:
      { "top" RA
        [ "let"; opt_rec{r}; binding{bi}; "in"; S{x} ->
            {| let $rec:r $bi in $x |}
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; S{e} ->
            {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; S{e} ->
            {| let open $id:i in $e |}
        | "fun"; "[";  L0 match_case0 SEP "|"{a}; "]" ->
            {| fun [ $list:a ] |}
        | "fun"; fun_def{e} -> e
        | "match"; S{e}; "with"; match_case{a} ->
            {| match $(Expr.mksequence' _loc e) with [ $a ] |}
        | "try"; S{e}; "with"; match_case{a} ->
            {| try $(Expr.mksequence' _loc e) with [ $a ] |}
        | "if"; S{e1}; "then"; S{e2}; "else"; S{e3} ->
            {| if $e1 then $e2 else $e3 |}
        | "do"; sequence{seq}; "done" -> Expr.mksequence _loc seq
        | "with"; lang{old}; S{x} -> begin  Quotation.default := old; x  end

        | "for"; a_LIDENT{i}; "="; S{e1}; direction_flag{df}; S{e2}; "do"; sequence{seq}; "done"
          ->
            {| for $i = $e1 $to:df $e2 do $seq done |}
            (* {| for $i = $(Expr.mksequence' _loc e1) $to:df $(Expr.mksequence' _loc e2) do *)
            (*    $seq *)
            (* done |} *)
        | "while"; S{e}; "do"; sequence{seq}; "done" ->
            {|while $e do $seq done |}
            (* {| while $(Expr.mksequence' _loc e) do  $seq done |} *)
        | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" ->
            {| object ($csp) $cst end |} ]
        "where"
        [ S{e}; "where"; opt_rec{rf}; let_binding{lb} ->
            {| let $rec:rf $lb in $e |} ]
       ":=" NA
        [ S{e1}; ":="; S{e2}; dummy ->
              {| $e1 := $e2 |} 
        | S{e1}; "<-"; S{e2}; dummy -> (* FIXME should be deleted in original syntax later? *)
            match Expr.bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> {| $e1 <- $e2 |} ]  
        ]
       "||" RA
        [ S{e1}; infixop6{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "&&" RA
        [ S{e1}; infixop5{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "<" LA
        [ S{e1}; infixop0{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "^" RA
        [ S{e1}; infixop1{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "+" LA
        [ S{e1}; infixop2{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "*" LA
        [ S{e1}; "land"; S{e2} -> {| $e1 land $e2 |}
        | S{e1}; "lor"; S{e2} -> {| $e1 lor $e2 |}
        | S{e1}; "lxor"; S{e2} -> {| $e1 lxor $e2 |}
        | S{e1}; "mod"; S{e2} -> {| $e1 mod $e2 |}
        | S{e1}; infixop3{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "**" RA
        [ S{e1}; "asr"; S{e2} -> {| $e1 asr $e2 |}
        | S{e1}; "lsl"; S{e2} -> {| $e1 lsl $e2 |}
        | S{e1}; "lsr"; S{e2} -> {| $e1 lsr $e2 |}
        | S{e1}; infixop4{op}; S{e2} -> {| $op $e1 $e2 |} ]
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
        [ `QUOTATION x -> Quotation.expand _loc x DynAst.expr_tag
        | `ANT (("exp"|""|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"expr" n s) |}
        | `ANT (("`bool" as n),s) ->
            {| $(id:{:ident| $(anti:mk_anti n s) |}) |}
        | `ANT (("tup" as n),s) ->
            {| $(tup: {| $(anti:mk_anti ~c:"expr" n s) |}) |}
        | `ANT (("seq" as n),s) ->
            {| do $(anti:mk_anti ~c:"expr" n s) done |}
        | a_INT{s} -> {| $int:s |}
        | a_INT32{s} -> {| $int32:s |}
        | a_INT64{s} -> {| $int64:s |}
        | a_NATIVEINT{s} -> {| $nativeint:s |}
        | a_FLOAT{s} -> {| $flo:s |}
        | a_STRING{s} -> {| $str:s |}
        | a_CHAR{s} -> {| $chr:s |}
        | TRY module_longident_dot_lparen{i}; (* sequence *)S{e}; ")" ->
            {| let open $i in $e |}
        | TRY val_longident{i} -> {| $id:i |}
        | "`"; a_ident{s} -> {| ` $s |}
        | "["; "]" -> {| [] |}
        | "[";sem_expr_for_list{mk_list}; "::"; expr{last}; "]" ->
            mk_list last
        | "["; sem_expr_for_list{mk_list}; "]" ->
            mk_list {| [] |}
        | "[|"; "|]" -> {| [| $({||}) |] |}
        | "[|"; sem_expr{el}; "|]" -> {| [| $el |] |}
        | "{"; label_expr_list{el}; "}" -> {| { $el } |}
        | "{"; "("; S{e}; ")"; "with"; label_expr_list{el}; "}" ->
            {| { ($e) with $el } |}
        | "{<"; ">}" -> {| {<>} |}
        | "{<"; field_expr_list{fel}; ">}" -> {| {< $fel >} |}
        | "("; ")" -> {| () |}
        | "("; S{e}; ":"; ctyp{t}; ")" -> {| ($e : $t) |}
        | "("; S{e}; ","; comma_expr{el}; ")" -> {| ( $e, $el ) |}
        | "("; S{e}; ";"; sequence{seq}; ")" -> Expr.mksequence _loc {| $e; $seq |}
        | "("; S{e}; ";"; ")" -> Expr.mksequence _loc e
        | "("; S{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" ->
            {| ($e : $t :> $t2 ) |}
        | "("; S{e}; ":>"; ctyp{t}; ")" -> {| ($e :> $t) |}
        | "("; S{e}; ")" -> e
        | "begin"; sequence{seq}; "end" -> Expr.mksequence _loc seq
        | "begin"; "end" -> {| () |}
        | "("; "module"; module_expr{me}; ")" ->
            {| (module $me) |}
        | "("; "module"; module_expr{me}; ":"; package_type{pt}; ")" ->
            {| (module $me : $pt) |}  ] }
   sequence: (*FIXME*)
      [ "let"; opt_rec{rf}; binding{bi}; "in"; expr{e}; sequence'{k} ->
            k {| let $rec:rf $bi in $e |}
      | "let"; opt_rec{rf}; binding{bi}; ";"; S{el} ->
          {| let $rec:rf $bi in $(Expr.mksequence _loc el) |}
      | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e}; sequence'{k} ->
          k {| let module $m = $mb in $e |}
      | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; ";"; S{el} ->
          {| let module $m = $mb in $(Expr.mksequence _loc el) |}
      | "let"; "open"; module_longident{i}; "in"; S{e} ->
          {| let open $id:i in $e |}
      | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"expr;" n s) |}
      | expr{e}; sequence'{k} -> k e ]
    infixop5:
      [  [ "&" | "&&" ]{x} -> {| $lid:x |} ]
    infixop6:
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
      [ -> () ]
    sequence':
      [ -> fun e -> e
      | ";" -> fun e -> e
      | ";"; sequence{el} -> fun e -> {| $e; $el |} ]
    fun_def:
      [ "("; "type"; a_LIDENT{i}; ")"; fun_def_cont_no_when{e} ->
        {| fun (type $i) -> $e |}
      |  labeled_ipatt{p };  fun_def_cont{(w, e)} ->
            {| fun [ $p when $w -> $e ] |} ] 
    fun_def_cont "expr":
      { RA
        [ "("; "type"; a_LIDENT{i}; ")"; fun_def_cont_no_when{e} ->
          ({||}, {| fun (type $i) -> $e |})
        |  labeled_ipatt{p };  S{(w,e)} ->
            ({||}, {| fun [ $p when $w -> $e ] |})
        | "when"; expr{w}; "->"; expr{e} -> (w, e)
        | "->"; expr{e} -> ({||}, e) ] }
    fun_def_cont_no_when:
      { RA
        [ "("; "type"; a_LIDENT{i}; ")";
          fun_def_cont_no_when{e} -> {| fun (type $i) -> $e |}
        |  labeled_ipatt{ p}; fun_def_cont{(w,e)} ->
            {| fun [ $p when $w -> $e ] |}
        | "->"; expr{e} -> e ] }  |};

  
    {:extend|Gram 
    binding "binding":
     [ `ANT (("binding"|"list" as n),s) ->
       {| $(anti:mk_anti ~c:"binding" n s) |}
     | `ANT ((""|"anti" as n),s); "="; expr{e} ->
         {| $(anti:mk_anti ~c:"patt" n s) = $e |}
     | `ANT ((""|"anti" as n),s) -> {| $(anti:mk_anti ~c:"binding" n s) |}
     | S{b1}; "and"; S{b2} -> {| $b1 and $b2 |}
     | let_binding{b} -> b ] 
    let_binding:
     [ ipatt{p}; fun_binding{e} -> {:binding| $p = $e |} ]
    fun_binding:
     { RA
         [ "("; "type"; a_LIDENT{i}; ")"; S{e} ->
           {:expr| fun (type $i) -> $e |}
         |  labeled_ipatt{p}; S{e} ->
             {:expr| fun $p -> $e |}
         | cvalue_binding{bi} -> bi  ] } |};

    with "match_case"
    {:extend|Gram
     match_case:
     [ "["; L0 match_case0 SEP "|"{l}; "]" -> Ast.mcOr_of_list l
     | ipatt{p}; "->"; expr{e} -> {| $p -> $e |} ]
    match_case0:
     [ `ANT (("match_case"|"list" as n),s) ->
         {| $(anti:mk_anti ~c:"match_case" n s) |}
     | `ANT ((""|"anti" as n),s) ->
         {| $(anti:mk_anti ~c:"match_case" n s) |}
     | `ANT ((""|"anti" as n),s); "->"; expr{e} ->
         {| $(anti:mk_anti ~c:"patt" n s) -> $e |}
     | `ANT ((""|"anti" as n),s); "when"; expr{w}; "->"; expr{e} ->
         {| $(anti:mk_anti ~c:"patt" n s) when $w -> $e |}
     | patt_as_patt_opt{p}; opt_when_expr{w}; "->"; expr{e} ->
         {| $p when $w -> $e |}  ] |};
    {:extend|Gram
    opt_when_expr:
     [ "when"; expr{w} -> w  | -> {:expr||}   ]
    patt_as_patt_opt:
     [ patt{p1}; "as"; patt{p2} -> {:patt| ($p1 as $p2) |}
     | patt{p} -> p ]
    label_expr_list:
      [ label_expr{b1}; ";"; S{b2} -> {:rec_binding| $b1 ; $b2 |}
      | label_expr{b1}; ";"            -> b1
      | label_expr{b1}                 -> b1  ]
    label_expr "rec_binding":
      [ `ANT (("rec_binding" as n),s) ->
          {| $(anti:mk_anti ~c:"rec_binding" n s) |}
      | `ANT ((""|"anti" as n),s) ->
          {| $(anti:mk_anti ~c:"rec_binding" n s) |}
      | `ANT ((""|"anti" as n),s); "="; expr{e} ->
          {| $(anti:mk_anti ~c:"ident" n s) = $e |}
      | `ANT (("list" as n),s) ->
          {| $(anti:mk_anti ~c:"rec_binding" n s) |}
      | label_longident{i}; fun_binding{e} -> {| $i = $e |}
      | label_longident{i} ->
            {| $i = $(lid:Ident.to_lid i) |} ] |};
    with "patt"
    {:extend|Gram local: patt_constr;

    patt_constr:
      [module_longident{i} -> {| $id:i |}
      |"`"; a_ident{s}  -> {| `$s|} ]
    patt:
      { "|" LA
        [ S{p1}; "|"; S{p2} -> {| $p1 | $p2 |} ]
       ".." NA
        [ S{p1}; ".."; S{p2} -> {| $p1 .. $p2 |} ]
       "apply" LA
        [
         (* S{p1}; S{p2} -> {| $p1 $p2 |} *)
          patt_constr{p1}; S{p2} ->
            match p2 with
            [ {| ($tup:p) |} ->
              List.fold_left (fun p1 p2 -> (* {| $p1 $p2 |} *) Ast.PaApp (_loc,p1,p2)) p1
                (Ast.list_of_patt p [])
            | _ -> (* {|$p1 $p2 |} *) Ast.PaApp(_loc,p1,p2) ]
        | patt_constr{p1} -> p1
        | `ANT ((""|"pat"|"anti" as n), s) 
          ->
            {|$(anti:mk_anti ~c:"patt" n s)|} 
        | `ANT ((""|"pat"|"anti" as n1), s1); S{p}
              ->
                let p0 = {|$(anti:mk_anti ~c:"patt" n1 s1)|} in
                {| $p0 $p|}

        | "lazy"; S{p} -> {| lazy $p |}  ]
       "simple"
        [ `ANT ((""|"pat"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"patt" n s) |}
        | `ANT (("tup" as n),s) ->
            {| ($(tup:{| $(anti:mk_anti ~c:"patt" n s) |} )) |}
        | `ANT (("`bool" as n),s) ->
            {| $(id:{:ident| $(anti:mk_anti n s) |}) |}
        | ident{i} -> {| $id:i |}
        | a_INT{s} -> {| $int:s |}
        | a_INT32{s} -> {| $int32:s |}
        | a_INT64{s} -> {| $int64:s |}
        | a_NATIVEINT{s} -> {| $nativeint:s |}
        | a_FLOAT{s} -> {| $flo:s |}
        | a_STRING{s} -> {| $str:s |}
        | a_CHAR{s} -> {| $chr:s |}
        | "-"; a_INT{s} -> {| $(int:neg_string s) |}
        | "-"; a_INT32{s} -> {| $(int32:neg_string s) |}
        | "-"; a_INT64{s} -> {| $(int64:neg_string s) |}
        | "-"; a_NATIVEINT{s} -> {| $(nativeint:neg_string s) |}
        | "-"; a_FLOAT{s} -> {| $(flo:neg_string s) |}
        | "["; "]" -> {| [] |}
        | "["; sem_patt_for_list{mk_list}; "::"; patt{last}; "]" ->
            mk_list last
        | "["; sem_patt_for_list{mk_list}; "]" ->
            mk_list {| [] |}
        | "[|"; "|]" -> {| [| $({||}) |] |}
        | "[|"; sem_patt{pl}; "|]" -> {| [| $pl |] |}
        | "{"; label_patt_list{pl}; "}" -> {| { $pl } |}
        | "("; ")" -> {| () |}
        | "("; "module"; a_UIDENT{m}; ")" -> {| (module $m) |}
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" ->
            {| ((module $m) : (module $pt)) |}
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as"; S{p2}; ")" -> {| ($p as $p2) |}
        | "("; S{p}; ","; comma_patt{pl}; ")" -> {| ($p, $pl) |}
        | "_" -> {| _ |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "`"; a_ident{s} -> {| ` $s |}
        | "#"; type_longident{i} -> {| # $i |}
        | `LABEL i; S{p} -> {| ~ $i : $p |}
        | "~"; `ANT ((""|"lid" as n),i); ":"; S{p} ->
            {| ~ $(mk_anti n i) : $p |}
        | "~"; `ANT ((""|"lid" as n),i) -> {| ~ $(mk_anti n i) |}
        | "~"; `LID i -> {| ~ $i |}
        (* | opt_label{i}; "("; patt_tcon{p}; ")" -> *)
            (* {| ? $i$ : ($p$) |} *)
        | `OPTLABEL i; "("; patt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANT ((""|"lid" as n),i); ":"; "("; patt_tcon{p}; eq_expr{f}; ")" ->
            f (mk_anti n i) p
        | "?"; `LID i -> {| ? $i |}
        | "?"; `ANT ((""|"lid" as n),i) -> {| ? $(mk_anti n i) |}
        | "?"; "("; patt_tcon{p}; ")" ->
            {| ? ($p) |}
        | "?"; "("; patt_tcon{p}; "="; expr{e}; ")" ->
            {| ? ($p = $e) |} ] }
    comma_patt:
        [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
        | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt," n s) |}
        | patt{p} -> p ] 
    sem_patt:
        [ patt{p1}; ";"; S{p2} -> {| $p1; $p2 |}
        | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt;" n s) |}
        | patt{p}; ";" -> p
        | patt{p} -> p ] 
    sem_patt_for_list:
        [ patt{p}; ";"; S{pl} -> fun acc -> {| [ $p :: $(pl acc) ] |}
        | patt{p}; ";" -> fun acc -> {| [ $p :: $acc ] |}
        | patt{p} -> fun acc -> {| [ $p :: $acc ] |}  ] 
    label_patt_list:
        [ label_patt{p1}; ";"; S{p2} -> {| $p1 ; $p2 |}
        | label_patt{p1}; ";"; "_"       -> {| $p1 ; _ |}
        | label_patt{p1}; ";"; "_"; ";"  -> {| $p1 ; _ |}
        | label_patt{p1}; ";"            -> p1
        | label_patt{p1}                 -> p1   ] 
    label_patt:
        [ `ANT ((""|"pat"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"patt" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"patt;" n s) |}
        | label_longident{i}; "="; patt{p} -> {| $i = $p |}
        | label_longident{i} -> {| $i = $(lid:Ident.to_lid i) |} ]
    (* label_ipatt: *)
    (*     [ `ANT ((""|"pat"|"anti" as n),s) -> *)
    (*         {| $(anti:mk_anti ~c:"patt" n s) |} *)
    (*     | `ANT (("list" as n),s) -> *)
    (*         {| $(anti:mk_anti ~c:"patt;" n s) |} *)
    (*     | `QUOTATION x -> *)
    (*         Quotation.expand _loc x DynAst.patt_tag *)
    (*     | label_longident{i}; "="; ipatt{p} -> {| $i = $p |}   ]        *)
    (* label_ipatt_list: *)
    (*    [ label_ipatt{p1}; ";"; S{p2} -> {| $p1 ; $p2 |} *)
    (*    | label_ipatt{p1}; ";"; "_"       -> {| $p1 ; _ |} *)
    (*    | label_ipatt{p1}; ";"; "_"; ";"  -> {| $p1 ; _ |} *)
    (*    | label_ipatt{p1}; ";"            -> p1 *)
    (*    | label_ipatt{p1}                 -> p1   ] *)

    ipatt:
        [ "{"; (* label_ipatt_list{pl} *) label_patt_list{pl}; "}" -> {| { $pl } |}
        | `ANT ((""|"pat"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"patt" n s) |}
        | `ANT (("tup" as n),s) ->
            {| ($(tup:{| $(anti:mk_anti ~c:"patt" n s) |})) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "("; ")" -> {| () |}
        | "("; "module"; a_UIDENT{m}; ")" -> {| (module $m) |}
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" ->
            {| ((module $m) : (module $pt)) |}
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as"; S{p2}; ")" -> {| ($p as $p2) |}
        | "("; S{p}; ","; comma_ipatt{pl}; ")" -> {| ($p, $pl) |}
        | a_LIDENT{s} -> {| $lid:s |}
        | "_" -> {| _ |} 
        | `LABEL i; S{p} -> {| ~ $i : $p |}
        | "~"; `ANT ((""|"lid" as n),i); ":"; S{p} ->
            {| ~ $(mk_anti n i) : $p |}
        | "~"; `ANT ((""|"lid" as n),i) -> {| ~ $(mk_anti n i) |}
        | "~"; `LID i -> {| ~ $i |}
        | `OPTLABEL i; "("; ipatt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANT ((""|"lid" as n),i); ":"; "("; ipatt_tcon{p};
          eq_expr{f}; ")" -> f (mk_anti n i) p
        | "?"; `LID i -> {| ? $i |}
        | "?"; `ANT ((""|"lid" as n),i) -> {| ? $(mk_anti n i) |}
        | "?"; "("; ipatt_tcon{p}; ")" ->
            {| ? ($p) |}
        | "?"; "("; ipatt_tcon{p}; "="; expr{e}; ")" ->
            {| ? ($p = $e) |} ]
    labeled_ipatt:
        [ ipatt{p} -> p ]
    comma_ipatt:
        [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
        | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"patt," n s) |}
        | ipatt{p} -> p ]

 |};
    with "ctyp"
    {:extend|Gram
    type_declaration:
        [ `ANT ((""|"typ"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"ctypand" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | S{t1}; "and"; S{t2} -> {| $t1 and $t2 |}
        |  type_ident_and_parameters{(n, tpl)}; opt_eq_ctyp{tk};
          L0 constrain{cl} -> Ast.TyDcl _loc n tpl tk cl ]
    constrain:
        [ "constraint"; ctyp{t1}; "="; ctyp{t2} -> (t1, t2) ]
    opt_eq_ctyp:
        [ "="; type_kind{tk} -> tk | -> {||} ] 
    type_kind: [ ctyp{t} -> t ] 
    type_ident_and_parameters:
        [ a_LIDENT{i}; L0 optional_type_parameter{tpl} -> (i, tpl) ]
    type_longident_and_parameters:
        [ type_longident{i}; type_parameters{tpl} -> tpl {| $id:i |} ] 
    type_parameters:
        [ type_parameter{t1}; S{t2} ->
            fun acc -> t2 {| $acc $t1 |}
        | type_parameter{t} -> fun acc -> {| $acc $t |}
        | -> fun t -> t  ]
    type_parameter:
        [ `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> {| '$lid:i |}
        | "+"; "'"; a_ident{i} -> {| +'$lid:i |}
        | "-"; "'"; a_ident{i} -> {| -'$lid:i |} ]
    optional_type_parameter:
        [ `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> {| '$lid:i |}
        | "+"; "'"; a_ident{i} -> {| +'$lid:i |}
        | "-"; "'"; a_ident{i} -> {| -'$lid:i |}
        | "+"; "_" -> Ast.TyAnP _loc  
        | "-"; "_" -> Ast.TyAnM _loc  
        | "_" -> {| _ |}  ]
    ctyp :
      { "==" LA
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
          (* let t = Ast.TyApp (_loc, t2, t1) in *)
            let t = {| $t1 $t2 |} in
            try {| $(id:Ast.ident_of_ctyp t) |}
            with [ Invalid_argument _ -> t ]]
       "." LA
        [ S{t1}; "."; S{t2} ->
            try {| $(id:Ast.ident_of_ctyp t1).$(id:Ast.ident_of_ctyp t2) |}
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
       "simple"
        [ "'"; a_ident{i} -> {| '$i |}
        | "_" -> {| _ |}
        | `ANT ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("tup" as n),s) ->  {| ($(tup:{| $(anti:mk_anti ~c:"ctyp" n s) |})) |}
        | `ANT (("id" as n),s) ->   {| $(id:{:ident| $(anti:mk_anti ~c:"ident" n s) |}) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{i} -> {| $lid:i |}
        | a_UIDENT{i} -> {| $uid:i |}
        | "("; S{t}; "*"; star_ctyp{tl}; ")" ->  {| ( $t * $tl ) |}
        | "("; S{t}; ")" -> t
        | "["; "]" -> {| [ ] |}
        | "["; constructor_declarations{t}; "]" -> {| [ $t ] |}
        | "["; "="; row_field{rfl}; "]" ->   {| [ = $rfl ] |}
        | "["; ">"; "]" -> {| [ > $({||}) ] |}
        | "["; ">"; row_field{rfl}; "]" ->    {| [ > $rfl ] |}
        | "["; "<"; row_field{rfl}; "]" ->
            {| [ < $rfl ] |}
        | "["; "<"; row_field{rfl}; ">"; name_tags{ntl}; "]" ->
            {| [ < $rfl > $ntl ] |}
        | "[<"; row_field{rfl}; "]" ->
            {| [ < $rfl ] |}
        | "[<"; row_field{rfl}; ">"; name_tags{ntl}; "]" ->
            {| [ < $rfl > $ntl ] |}
        | "{"; label_declaration_list{t}; "}" -> {| { $t } |}
        | "#"; class_longident{i} -> {| # $i |}
        | "<"; opt_meth_list{t}; ">" -> t
        | "("; "module"; package_type{p}; ")" -> {| (module $p) |}  ] }
    star_ctyp:
        [ `ANT ((""|"typ" as n),s) ->
            {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"ctyp*" n s) |}
        | S{t1}; "*"; S{t2} ->   {| $t1 * $t2 |}
        | ctyp{t} -> t  ]
    constructor_declarations:
        [ `ANT ((""|"typ" as n),s) ->
            {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"ctyp|" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | S{t1}; "|"; S{t2} ->
            {| $t1 | $t2 |}
        | a_UIDENT{s}; "of"; constructor_arg_list{t} ->
            {| $uid:s of $t |}
        | a_UIDENT{s}; ":"; ctyp{t} ->
            let (tl, rt) = Ctyp.to_generalized t in
            {| $uid:s : ($(Ast.tyAnd_of_list tl) -> $rt) |}
        | a_UIDENT{s} ->  {| $uid:s |}  ]
    constructor_declaration:
        [ `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_UIDENT{s}; "of"; constructor_arg_list{t} ->   {| $uid:s of $t |}
        | a_UIDENT{s} ->    {| $uid:s |}  ]
    constructor_arg_list:
        [ `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"ctypand" n s) |}
        | S{t1}; "and"; S{t2} -> {| $t1 and $t2 |}
        | ctyp{t} -> t  ]
    label_declaration_list:
        [ label_declaration{t1}; ";"; S{t2} -> {| $t1; $t2 |}
        | label_declaration{t1}; ";"            -> t1
        | label_declaration{t1}                 -> t1  ]
    label_declaration:
        [ `ANT ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp;" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
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
      | ctyp{t} -> t  ]
    ctyp_quot:
      [ more_ctyp{x}; ","; comma_ctyp{y} -> {| $x, $y |}
      | more_ctyp{x}; ";"; label_declaration_list{y} -> {| $x; $y |}
      | more_ctyp{x}; "|"; constructor_declarations{y} -> {| $x | $y |}
      | more_ctyp{x}; "of"; constructor_arg_list{y} -> {| $x of $y |}
      | more_ctyp{x}; "of"; constructor_arg_list{y}; "|"; constructor_declarations{z} ->
          {| $({| $x of $y |} ) | $z |}
      | more_ctyp{x}; "of"; "&"; amp_ctyp{y} -> {| $x of & $y |}
      | more_ctyp{x}; "of"; "&"; amp_ctyp{y}; "|"; row_field{z} ->    {| $({| $x of & $y |} ) | $z |}
      | more_ctyp{x}; ":"; more_ctyp{y} -> {| $x : $y |}
      | more_ctyp{x}; ":"; more_ctyp{y}; ";"; label_declaration_list{z} ->  {| $({| $x : $y |} ) ; $z |}
      | more_ctyp{x}; "*"; star_ctyp{y} -> {| $x * $y |}
      | more_ctyp{x}; "&"; amp_ctyp{y} -> {| $x & $y |}
      | more_ctyp{x}; "and"; constructor_arg_list{y} -> {| $x and $y |}
      | more_ctyp{x} -> x
      | -> {||}  ] 
    more_ctyp:
      [ "mutable"; S{x} -> {| mutable $x |}
      | "`"; a_ident{x} -> {| `$x |}
      | ctyp{x} -> x
      | type_parameter{x} -> x   ]  |};
    
    with "ident"
    {:extend|Gram
    a_ident: [ a_LIDENT{i} -> i |  a_UIDENT{i} -> i ]
    ident:
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
           (* id it self does not support ANTIQUOT "lid", however [a_UIDENT] supports*)
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{i} -> {| $uid:i |}
        | a_LIDENT{i} -> {| $lid:i |}
        | `ANT ((""|"id"|"anti"|"list" as n),s); "."; S{i} ->
            {| $(anti:mk_anti ~c:"ident" n s).$i |}
        | a_UIDENT{i}; "."; S{j} -> {| $uid:i.$j |} ]
    module_longident_dot_lparen:
        [ `ANT ((""|"id"|"anti"|"list" as n),s); "."; "(" ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |}
        | a_UIDENT{i}; "."; "(" -> {| $uid:i |} ]        
        
    module_longident:
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |}
        | a_UIDENT{i} -> {| $uid:i |} ]
    module_longident_with_app:
      { "apply"
        [ S{i}; S{j} -> {| $i $j |} ]
       "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
       "simple"
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{i} -> {| $uid:i |}
        | "("; S{i}; ")" -> i ] }
 
    type_longident:
      { "apply"
        [ S{i}; S{j} -> {| $i $j |} ]
        "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
        "simple"
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_LIDENT{i} -> {| $lid:i |}
        | a_UIDENT{i} -> {| $uid:i |}
        | "("; S{i}; ")" -> i ] }
    label_longident:
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{m}; "."; S{l} -> {| $uid:m.$l |}
        | a_LIDENT{i} -> {| $lid:i |} ]
    class_type_longident:
      [ type_longident{x} -> x ]
    val_longident:
      [ ident{x} -> x ]
    class_longident:
      [ label_longident{x} -> x ]
    class_declaration:
      [ S{c1}; "and"; S{c2} ->
          {:class_expr| $c1 and $c2 |}
      | `ANT ((""|"cdcl"|"anti"|"list" as n),s) ->
          {:class_expr| $(anti:mk_anti ~c:"class_expr" n s) |}
      | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
      | class_info_for_class_expr{ci}; class_fun_binding{ce} ->
            {:class_expr| $ci = $ce |} ]
    class_fun_binding:
      [ "="; class_expr{ce} -> ce
      | ":"; class_type_plus{ct}; "="; class_expr{ce} ->
          {:class_expr| ($ce : $ct) |}
      | labeled_ipatt{p}; S{cfb} ->
          {:class_expr| fun $p -> $cfb |}  ]
    class_info_for_class_type:
      [ opt_virtual{mv};  class_name_and_param{(i, ot)} ->
        {:class_type| $virtual:mv $lid:i [ $ot ] |} ]
    class_info_for_class_expr:
      [ opt_virtual{mv};  class_name_and_param{(i, ot)} ->
        {:class_expr| $virtual:mv $lid:i [ $ot ] |}  ]
    
    class_fun_def:
      [ labeled_ipatt{p}; S{ce} -> {:class_expr| fun $p -> $ce |}
       | "->"; class_expr{ce} -> ce ]
    class_expr:
      { "top"
        [ "fun"; labeled_ipatt{p}; class_fun_def{ce} ->
            {:class_expr| fun $p -> $ce |}
        | "let"; opt_rec{rf}; binding{bi}; "in"; S{ce} ->
            {:class_expr| let $rec:rf $bi in $ce |} ]
       "apply" NA
        [ S{ce}; expr Level "label"{e} ->
            {:class_expr| $ce $e |} ]
       "simple"
        [ `ANT ((""|"cexp"|"anti" as n),s) ->
            {:class_expr| $(anti:mk_anti ~c:"class_expr" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
        | class_longident_and_param{ce} -> ce
        | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" ->
            {:class_expr| object ($csp) $cst end |}
        | "("; S{ce}; ":"; class_type{ct}; ")" ->
            {:class_expr| ($ce : $ct) |}
        | "("; S{ce}; ")" -> ce ] }
    class_longident_and_param:
        [ class_longident{ci}; "["; comma_ctyp{t}; "]" ->
          {:class_expr| $id:ci [ $t ] |}
        | class_longident{ci} -> {:class_expr| $id:ci |}  ]
    class_structure:
        [ `ANT ((""|"cst"|"anti"|"list" as n),s) ->
            {:class_str_item| $(anti:mk_anti ~c:"class_str_item" n s) |}
        | `ANT ((""|"cst"|"anti"|"list" as n),s); semi; S{cst} ->
            {:class_str_item| $(anti:mk_anti ~c:"class_str_item" n s); $cst |}
        | L0 [ class_str_item{cst}; semi -> cst ]{l} -> Ast.crSem_of_list l  ]
    opt_class_self_patt:
        [ "("; patt{p}; ")" -> p
        | "("; patt{p}; ":"; ctyp{t}; ")" -> {:patt| ($p : $t) |}
        | -> {:patt||} ]
    class_str_item "class_str_item":
        [ `ANT ((""|"cst"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"class_str_item" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_str_item_tag
        | "inherit"; opt_override{o}; class_expr{ce}; opt_as_lident{pb} ->
            {| inherit $override:o $ce as $pb |}
        | value_val_opt_override{o}; opt_mutable{mf}; label{lab}; cvalue_binding{e}
          ->
            {| val $override:o $mutable:mf $lab = $e |}
        | value_val_opt_override{o}; opt_mutable{mf}; "virtual"; label{l}; ":";
              poly_type{t} ->
            if o <> {:override_flag||} then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              {| val virtual $mutable:mf $l : $t |}
        | value_val_opt_override{o}; "virtual"; opt_mutable{mf}; label{l}; ":";
                poly_type{t} ->
            if o <> {:override_flag||} then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              {| val virtual $mutable:mf $l : $t |}
        | method_opt_override{o}; "virtual"; opt_private{pf}; label{l}; ":";
                poly_type{t} ->
            if o <> {:override_flag||} then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              {| method virtual $private:pf $l : $t |}
        | method_opt_override{o}; opt_private{pf}; label{l}; opt_polyt{topt};
                fun_binding{e} ->
            {| method $override:o $private:pf $l : $topt = $e |}
        | method_opt_override{o}; opt_private{pf}; "virtual"; label{l}; ":";
             poly_type{t} ->
            if o <> {:override_flag||} then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              {| method virtual $private:pf $l : $t |}
        | type_constraint; ctyp{t1}; "="; ctyp{t2} ->
            {| type $t1 = $t2 |}
        | "initializer"; expr{se} -> {| initializer $se |} ]
    method_opt_override:
        [ "method"; "!" -> {:override_flag| ! |}
        | "method"; `ANT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | "method" -> {:override_flag||}  ] 
    value_val_opt_override:
        [ "val"; "!" -> {:override_flag| ! |}
        | "val"; `ANT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | "val" -> {:override_flag||}   ] 
    opt_as_lident:
        [ "as"; a_LIDENT{i} -> i  | -> ""  ] 
    opt_polyt:
        [ ":"; poly_type{t} -> t  | -> {:ctyp||} ]
    cvalue_binding "expr":
        [ "="; expr{e} -> e
        | ":"; "type"; unquoted_typevars{t1}; "." ; ctyp{t2} ; "="; expr{e} -> 
         let u = {:ctyp| ! $t1 . $t2 |} in  {| ($e : $u) |}
        | ":"; poly_type{t}; "="; expr{e} -> {| ($e : $t) |}
        | ":"; poly_type{t}; ":>"; ctyp{t2}; "="; expr{e} ->
            match t with
            [ {:ctyp| ! $_ . $_ |} -> raise (Stream.Error "unexpected polytype here")
            | _ -> {| ($e : $t :> $t2) |} ]
        | ":>"; ctyp{t}; "="; expr{e} -> {| ($e :> $t) |} ] 
    label:  [ a_LIDENT{i} -> i ]
    class_type:
        [ `ANT ((""|"ctyp"|"anti" as n),s) ->
            {:class_type| $(anti:mk_anti ~c:"class_type" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | class_type_longident_and_param{ct} -> ct
        | "object"; opt_class_self_type{cst}; class_signature{csg}; "end" ->
            {:class_type| object ($cst) $csg end |} ]
    class_type_longident_and_param:
        [ class_type_longident{i}; "["; comma_ctyp{t}; "]" ->
            {:class_type| $id:i [ $t ] |}
        | class_type_longident{i} -> {:class_type| $id:i |} ] 
    class_type_plus:
        [ "["; ctyp{t}; "]"; "->"; S{ct} -> {:class_type| [ $t ] -> $ct |}
        | class_type{ct} -> ct ]
    opt_class_self_type:
        [ "("; ctyp{t}; ")" -> t
        | -> {:ctyp||} ]
    class_signature:
        [ `ANT ((""|"csg"|"anti"|"list" as n),s) ->
            {:class_sig_item| $(anti:mk_anti ~c:"class_sig_item" n s) |}
        | `ANT ((""|"csg"|"anti"|"list" as n),s); semi; S{csg} ->
            {:class_sig_item| $(anti:mk_anti ~c:"class_sig_item" n s); $csg |}
        | L0 [ class_sig_item{csg}; semi -> csg ]{l} ->
            Ast.cgSem_of_list l  ]
    class_sig_item "class_sig_item":
        [ `ANT ((""|"csg"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"class_sig_item" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_sig_item_tag
        | "inherit"; class_type{cs} ->
            {| inherit $cs |}
        | "val"; opt_mutable{mf}; opt_virtual{mv};
          label{l}; ":"; ctyp{t} ->
            {| val $mutable:mf $virtual:mv $l : $t |}
        | "method"; "virtual"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
            {| method virtual $private:pf $l : $t |}
        | "method"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
            {| method $private:pf $l : $t |}
        | "method"; opt_private{pf}; "virtual"; label{l}; ":"; poly_type{t} ->
            {| method virtual $private:pf $l : $t |}
        | type_constraint; ctyp{t1}; "="; ctyp{t2} ->
            {| type $t1 = $t2 |} ]
    type_constraint:
        [ "type" | "constraint" -> () ] 
    class_description "class_type":
        [ S{cd1}; "and"; S{cd2} ->
            {| $cd1 and $cd2 |}
        | `ANT ((""|"typ"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"class_type" n s) |}
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.class_type_tag
        | class_info_for_class_type{ci}; ":"; class_type_plus{ct} ->
            {| $ci : $ct |}  ]
    class_type_declaration "class_type":
        [ S{cd1}; "and"; S{cd2} ->
          {| $cd1 and $cd2 |}
        | `ANT ((""|"typ"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"class_type" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | class_info_for_class_type{ci}; "="; class_type{ct} ->
            {| $ci = $ct |} ]
    field_expr_list:
        [ field_expr{b1}; ";"; S{b2} -> {:rec_binding| $b1 ; $b2 |}
        | field_expr{b1}; ";"            -> b1
        | field_expr{b1}                 -> b1  ] 
    field_expr "rec_binding":
        [ `ANT ((""|"bi"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"rec_binding" n s) |}
        | `ANT (("list" as n),s) ->
            {| $(anti:mk_anti ~c:"rec_binding" n s) |}
        | label{l}; "=";  expr Level "top"{e} ->
            {| $lid:l = $e |} ]
    meth_list:
        [ meth_decl{m}; ";"; S{(ml, v) }  -> ({:ctyp| $m; $ml |}, v)
        | meth_decl{m}; ";"; opt_dot_dot{v} -> (m, v)
        | meth_decl{m}; opt_dot_dot{v}      -> (m, v)  ]
    meth_decl:
        [ `ANT ((""|"typ" as n),s)        -> {:ctyp| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s)          -> {:ctyp| $(anti:mk_anti ~c:"ctyp;" n s) |}
        | `QUOTATION x                       -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{lab}; ":"; poly_type{t} -> {:ctyp| $lid:lab : $t |} ]
    opt_meth_list:
        [ meth_list{(ml, v) } -> {:ctyp| < $ml $(..:v) > |}
        | opt_dot_dot{v}     -> {:ctyp| < $(..:v) > |}  ]
    poly_type:
        [ ctyp{t} -> t ]
    package_type:
        [ module_type{p} -> p ] 
    typevars:
        [ S{t1}; S{t2} -> {:ctyp| $t1 $t2 |}
        | `ANT ((""|"typ" as n),s) ->
            {:ctyp| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> {:ctyp| '$lid:i |} ]
    unquoted_typevars:
        [ S{t1}; S{t2} -> {:ctyp| $t1 $t2 |}
        | `ANT ((""|"typ" as n),s) ->
            {:ctyp| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_ident{i} -> {:ctyp| $lid:i |}   ] 
    row_field:
        [ `ANT ((""|"typ" as n),s) ->
            {:ctyp| $(anti:mk_anti ~c:"ctyp" n s) |}
        | `ANT (("list" as n),s) ->
            {:ctyp| $(anti:mk_anti ~c:"ctyp|" n s) |}
        | S{t1}; "|"; S{t2} -> {:ctyp| $t1 | $t2 |}
        | "`"; a_ident{i} -> {:ctyp| `$i |}
        | "`"; a_ident{i}; "of"; "&"; amp_ctyp{t} -> {:ctyp| `$i of & $t |}
        | "`"; a_ident{i}; "of"; amp_ctyp{t} -> {:ctyp| `$i of $t |}
        | ctyp{t} -> t ] 
    amp_ctyp:
        [ S{t1}; "&"; S{t2} -> {:ctyp| $t1 & $t2 |}
        | `ANT (("list" as n),s) -> {:ctyp| $(anti:mk_anti ~c:"ctyp&" n s) |}
        | ctyp{t} -> t ]
    name_tags:
        [ `ANT ((""|"typ" as n),s) ->
            {:ctyp| $(anti:mk_anti ~c:"ctyp" n s) |}
        | S{t1}; S{t2} -> {:ctyp| $t1 $t2 |}
        | "`"; a_ident{i} -> {:ctyp| `$i |}  ]
    eq_expr:
        [ "="; expr{e} -> fun i p -> {:patt| ? $i : ($p = $e) |}
        | -> fun i p -> {:patt| ? $i : ($p) |} ]
    patt_tcon:
        [ patt{p}; ":"; ctyp{t} -> {:patt| ($p : $t) |}
        | patt{p} -> p ]
    ipatt_tcon:
        [ ipatt{p}; ":"; ctyp{t} -> {:patt| ($p : $t) |}
        | ipatt{p} -> p ]
    direction_flag:
        [ "to" -> {:direction_flag| to |}
        | "downto" -> {:direction_flag| downto |}
        | `ANT (("to"|"anti" as n),s) -> Ast.DiAnt (mk_anti n s) ]
    opt_private:
        [ "private" -> {:private_flag| private |}
        | `ANT (("private"|"anti" as n),s) -> Ast.PrAnt (mk_anti n s)
        | -> {:private_flag||}  ] 
    opt_mutable:
        [ "mutable" -> {:mutable_flag| mutable |}
        | `ANT (("mutable"|"anti" as n),s) -> Ast.MuAnt (mk_anti n s)
        | -> {:mutable_flag||}  ] 
    opt_virtual:
        [ "virtual" -> {:virtual_flag| virtual |}
        | `ANT (("virtual"|"anti" as n),s) -> Ast.ViAnt (mk_anti n s)
        | -> {:virtual_flag||}  ] 
    opt_dot_dot:
        [ ".." -> {:row_var_flag| .. |}
        | `ANT ((".."|"anti" as n),s) -> Ast.RvAnt (mk_anti n s)
        | -> {:row_var_flag||}  ]
    opt_rec:
        [ "rec" -> {:rec_flag| rec |}
        | `ANT (("rec"|"anti" as n),s) -> Ast.ReAnt (mk_anti n s)
        | -> {:rec_flag||} ] 
    opt_override:
        [ "!" -> {:override_flag| ! |}
        | `ANT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | -> {:override_flag||} ] 
    opt_expr:
        [ expr{e} -> e | -> {:expr||} ] 

    (* mli entrance *)    
    interf:
        [ "#"; a_LIDENT{n}; opt_expr{dp}; (* semi  *) ";;" ->
            ([ {:sig_item| # $n $dp |} ], stopped_at _loc)
          (* Ast.SgDir(_loc,n,dp), stopped is of type FanLoc.t option *)
        | sig_item{si}; semi;  S{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ]
    sig_items:
        [ `ANT ((""|"sigi"|"anti"|"list" as n),s) ->
            {:sig_item| $(anti:mk_anti n ~c:"sig_item" s) |}
        | `ANT ((""|"sigi"|"anti"|"list" as n),s); semi; S{sg} ->
            {:sig_item| $(anti:mk_anti n ~c:"sig_item" s); $sg |} 
        | L0 [ sig_item{sg}; semi -> sg ]{l} -> Ast.sgSem_of_list l  ]
    (* ml entrance *)    
    implem:
        [ "#"; a_LIDENT{n}; opt_expr{dp}; (* semi *)";;" ->
            ([ {:str_item| # $n $dp |} ], stopped_at _loc)
        | str_item{si}; semi;  S{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ]
    str_items:
        [ `ANT ((""|"stri"|"anti"|"list" as n),s) ->
            {:str_item| $(anti:mk_anti n ~c:"str_item" s) |}
        | `ANT ((""|"stri"|"anti"|"list" as n),s); semi; S{st} ->
            {:str_item| $(anti:mk_anti n ~c:"str_item" s); $st |}
        | L0 [ str_item{st}; semi -> st ]{l} -> Ast.stSem_of_list l  ]
    phrase:
        [ "#"; a_LIDENT{n}; opt_expr{dp}; ";;" -> (* directive to be the same as normal syntax*)
            {:str_item| # $n $dp |}
        | str_item{st}; semi -> st  ]         
    top_phrase:
        [ phrase{ph} -> Some ph | `EOI -> None ] 
    use_file:
        [ "#"; a_LIDENT{n}; opt_expr{dp}; semi ->
            ([ {:str_item| # $n $dp |} ], stopped_at _loc)
        | str_item{si}; semi;  S{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ]
    
    a_INT:
        [ `ANT ((""|"int"|"`int" as n),s) -> mk_anti n s
        | `INT (_, s) -> s ] 
    a_INT32:
        [ `ANT ((""|"int32"|"`int32" as n),s) -> mk_anti n s
        | `INT32 (_, s) -> s ] 
    a_INT64:
        [ `ANT ((""|"int64"|"`int64" as n),s) -> mk_anti n s
        | `INT64 (_, s) -> s ]
    a_NATIVEINT:
        [ `ANT ((""|"nativeint"|"`nativeint" as n),s) -> mk_anti n s
        | `NATIVEINT (_, s) -> s ]
    a_FLOAT:
        [ `ANT ((""|"flo"|"`flo" as n),s) -> mk_anti n s
        | `FLOAT (_, s) -> s ]
    a_CHAR:
        [ `ANT ((""|"chr"|"`chr" as n),s) -> mk_anti n s
        | `CHAR (_, s) -> s ] 
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
    a_STRING:
        [ `ANT ((""|"str"|"`str" as n),s) -> mk_anti n s
        | `STR (_, s) -> s ] 
    string_list:
        [ `ANT ((""|"str_list"),s) -> Ast.LAnt (mk_anti "str_list" s)
        | `STR (_, x); string_list{xs} -> Ast.LCons x xs
        | `STR (_, x) -> Ast.LCons x Ast.LNil ] 
    semi:
        [ ";" -> () ]  
    expr_quot:
        [ expr{e1}; ","; comma_expr{e2} -> {:expr| $e1, $e2 |}
        | expr{e1}; ";"; sem_expr{e2} -> {:expr| $e1; $e2 |}
        | expr{e} -> e
        | -> {:expr||} ]
    patt_quot:
        [ patt{x}; ","; comma_patt{y} -> {:patt| $x, $y |}
        | patt{x}; ";"; sem_patt{y} -> {:patt| $x; $y |}
        | patt{x}; "="; patt{y} ->
            let i =
              match x with
              [ {:patt@loc| $anti:s |} -> {@loc| $anti:s |}
              | p -> Ast.ident_of_patt p ]
            in
            {:patt| $i = $y |}
        | patt{x} -> x
        | -> {:patt||} ]

    str_item_quot:
        [ "#"; a_LIDENT{n}; opt_expr{dp} -> {:str_item| # $n $dp |}
        | str_item{st1}; semi; S{st2} ->
            match st2 with
            [ {:str_item||} -> st1
            | _ -> {:str_item| $st1; $st2 |} ]
        | str_item{st} -> st
        | -> {:str_item||} ] 
    sig_item_quot:
        [ "#"; a_LIDENT{n}; opt_expr{dp} -> {:sig_item| # $n $dp |}
        | sig_item{sg1}; semi; S{sg2} ->
            match sg2 with
            [ {:sig_item||} -> sg1
            | _ -> {:sig_item| $sg1; $sg2 |} ]
        | sig_item{sg} -> sg
        | -> {:sig_item||} ] 
    module_type_quot:
        [ module_type{x} -> x | -> {:module_type||} ] 
    module_expr_quot:
        [ module_expr{x} -> x
        | -> {:module_expr||} ] 
    match_case_quot:
        [ L0 match_case0 SEP "|"{x} -> {:match_case| $list:x |}
        | -> {:match_case||} ]
    binding_quot:
        [ binding{x} -> x | -> {:binding||} ] 
    rec_binding_quot:
        [ label_expr_list{x} -> x | -> {:rec_binding||} ] 

    module_binding_quot "module_binding":
        [ S{b1}; "and"; S{b2} ->
            {| $b1 and $b2 |}
        | `ANT (("module_binding"|"anti" as n),s) ->
            {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `ANT (("" as n),s) ->
            {| $(anti:mk_anti ~c:"module_binding" n s) |}
        | `ANT (("" as n),m); ":"; module_type{mt} ->
            {| $(mk_anti n m) : $mt |}
        | `ANT (("" as n),m); ":"; module_type{mt}; "="; module_expr{me} ->
            {| $(mk_anti n m) : $mt = $me |}
        | a_UIDENT{m}; ":"; module_type{mt} ->  {| $m : $mt |}
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} ->  {| $m : $mt = $me |}
        | -> {||} ] 
    ident_quot "ident":
      { "apply"
        [ S{i}; S{j} -> {| $i $j |} ]
        "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
        "simple"
        [ `ANT ((""|"id"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"ident" n s) |}
        | a_UIDENT{i} -> {| $uid:i |}
        | a_LIDENT{i} -> {| $lid:i |}
        | `ANT ((""|"id"|"anti"|"list" as n),s); "."; S{i} ->  {| $(anti:mk_anti ~c:"ident" n s).$i |}
        | "("; S{i}; ")" -> i  ] }
    class_expr_quot "class_expr":
        [ S{ce1}; "and"; S{ce2} -> {| $ce1 and $ce2 |}
        | S{ce1}; "="; S{ce2} -> {| $ce1 = $ce2 |}
        | "virtual";   class_name_and_param{(i, ot)} ->  {| virtual $lid:i [ $ot ] |}
        | `ANT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
            {| $virtual:anti $id:i [ $ot ] |}
        | class_expr{x} -> x
        | -> {||} ]
    class_type_quot "class_type":
        [ S{ct1}; "and"; S{ct2} -> {| $ct1 and $ct2 |}
        | S{ct1}; "="; S{ct2} -> {| $ct1 = $ct2 |}
        | S{ct1}; ":"; S{ct2} -> {| $ct1 : $ct2 |}
        | "virtual";  class_name_and_param{(i, ot)} ->
            {| virtual $lid:i [ $ot ] |}
        | `ANT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
            {| $virtual:anti $id:i [ $ot ] |}
        | class_type_plus{x} -> x
        | -> {||}   ] 
    class_str_item_quot "class_str_item":
        [ class_str_item{x1}; semi; S{x2} ->
          match x2 with
          [ {||} -> x1
          | _ -> {| $x1; $x2 |} ]
        | class_str_item{x} -> x
        | -> {||} ] 
    class_sig_item_quot "class_sig_item":
        [ class_sig_item{x1}; semi; S{x2} ->
          match x2 with
          [ {||} -> x1
          | _ -> {| $x1; $x2 |} ]
        | class_sig_item{x} -> x
        | -> {||} ] 
    with_constr_quot:
        [ with_constr{x} -> x
        | -> {:with_constr||} ] 
    rec_flag_quot:  [ opt_rec{x} -> x ]
    direction_flag_quot:  [ direction_flag{x} -> x ] 
    mutable_flag_quot: [  opt_mutable{x} -> x ] 
    private_flag_quot: [  opt_private{x} -> x ]
    virtual_flag_quot: [  opt_virtual{x} -> x ] 
    row_var_flag_quot: [  opt_dot_dot{x} -> x ] 
    override_flag_quot:[  opt_override{x} -> x ] 
    patt_eoi:  [ patt{x}; `EOI -> x ] 
    expr_eoi:  [ expr{x}; `EOI -> x ]  |} ;
end;

module IdRevisedParserParser : Sig.Id = struct
  let name = "Camlp4OCamlRevisedParserParser";
  let version = Sys.ocaml_version;
end;


(**************Stream Parser***********************)  
module MakeRevisedParserParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  open FanStreamTools;
  {:extend|Gram
      local: parser_ipatt stream_expr_comp  stream_expr_comp_list
      stream_patt_comp stream_patt_comp_err 
      stream_patt_comp_err_list stream_begin stream_end stream_patt
      parser_case parser_case_list stream_expr stream_quot; 
    expr: Level "top"
        [ "parser";  OPT [ `UID(n) -> n]  {name}; OPT parser_ipatt{po}; parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser _loc po pcl)
            | None -> cparser _loc po pcl]
        | "match"; S{e}; "with"; "parser";  OPT [`UID(n) -> n ] {name}; OPT parser_ipatt{po};
          parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser_match _loc e po pcl)
            | None -> cparser_match _loc e po pcl ] ] 
    expr: Level "simple"
       [ stream_begin{name};  stream_end
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ ->
                FanStreamTools.empty _loc )
            | None -> FanStreamTools.empty _loc 
            ]
      | stream_begin{name}; stream_expr_comp_list{sel}; stream_end
          ->
            match name with
            [ Some o ->   
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cstream _loc sel)
            | None -> cstream _loc sel ] ]
    parser_ipatt:
      [ a_LIDENT{i} -> {:patt| $lid:i |}  | "_" -> {:patt| _ |}  ]         
    parser_case_list:
      [ "["; L0 parser_case SEP "|"{pcl}; "]" -> pcl
      | parser_case{pc} -> [pc] ] 
    parser_case:
      [ "[<"; stream_patt{sp}; stream_end; OPT parser_ipatt{po}; "->"; expr{e}
        ->   (sp, po, e) ] 
    stream_begin: [ "[<"; OPT [ "!"; `UID(n)->n]{name} -> name  ]   
    stream_end:   [ ">]" -> () ] 
    stream_quot:  [ "'" -> () ]
    stream_expr:  [ expr{e} -> e ] 
    stream_patt:
      [ stream_patt_comp{spc} -> [(spc, None)]
      | stream_patt_comp{spc}; ";"; stream_patt_comp_err_list{sp}
        ->    [(spc, None) :: sp]
      | -> [] ]
    stream_patt_comp: (* FIXME here *)
      [  patt{p}; OPT [ "when"; stream_expr{e} -> e ]{eo}
         ->  SpTrm _loc p eo
      | patt{p}; "="; stream_expr{e} -> SpNtr _loc p e
      | stream_quot; patt{p} -> SpStr _loc p ]
    stream_patt_comp_err:
      [ stream_patt_comp{spc};  OPT [ "??"; stream_expr{e} -> e ]{eo }
        ->  (spc, eo) ] 
    stream_patt_comp_err_list:
        [ stream_patt_comp_err{spc} -> [spc]
        | stream_patt_comp_err{spc}; ";" -> [spc]
        | stream_patt_comp_err{spc}; ";"; stream_patt_comp_err_list{sp} ->
            [spc :: sp] ] 
    stream_expr_comp_list:
        [ stream_expr_comp{se}; ";"; stream_expr_comp_list{sel} -> [se :: sel]
        | stream_expr_comp{se}; ";" -> [se]
        | stream_expr_comp{se} -> [se] ] 
    stream_expr_comp: 
        [  stream_expr{e} -> SeTrm _loc e
        | stream_quot;stream_expr{e} -> SeNtr _loc e ]  |};
end;
  
module IdQuotationCommon = struct (* FIXME unused here *)
  let name = "Camlp4QuotationCommon";
  let version = Sys.ocaml_version;
end;

module MakeQuotationCommon (Syntax : Sig.Camlp4Syntax)
= struct
  include Syntax; 
  open Quotation;
  open Meta;
  add_quotation "sig_item" sig_item_quot ME.meta_sig_item MP.meta_sig_item;
  add_quotation "str_item" str_item_quot ME.meta_str_item MP.meta_str_item;
  add_quotation "ctyp" ctyp_quot ME.meta_ctyp MP.meta_ctyp;
  add_quotation "patt" patt_quot ME.meta_patt MP.meta_patt;
  add_quotation "expr" expr_quot ME.meta_expr MP.meta_expr;
  add_quotation "module_type" module_type_quot ME.meta_module_type MP.meta_module_type;
  add_quotation "module_expr" module_expr_quot ME.meta_module_expr MP.meta_module_expr;
  add_quotation "class_type" class_type_quot ME.meta_class_type MP.meta_class_type;
  add_quotation "class_expr" class_expr_quot ME.meta_class_expr MP.meta_class_expr;
  add_quotation "class_sig_item"
                class_sig_item_quot ME.meta_class_sig_item MP.meta_class_sig_item;
  add_quotation "class_str_item"
                class_str_item_quot ME.meta_class_str_item MP.meta_class_str_item;
  add_quotation "with_constr" with_constr_quot ME.meta_with_constr MP.meta_with_constr;
  add_quotation "binding" binding_quot ME.meta_binding MP.meta_binding;
  add_quotation "rec_binding" rec_binding_quot ME.meta_rec_binding MP.meta_rec_binding;
  add_quotation "match_case" match_case_quot ME.meta_match_case MP.meta_match_case;
  add_quotation "module_binding"
                module_binding_quot ME.meta_module_binding MP.meta_module_binding;
  add_quotation "ident" ident_quot ME.meta_ident MP.meta_ident;
  add_quotation "rec_flag" rec_flag_quot ME.meta_rec_flag MP.meta_rec_flag;
  add_quotation "private_flag" private_flag_quot ME.meta_private_flag MP.meta_private_flag;
  add_quotation "row_var_flag" row_var_flag_quot ME.meta_row_var_flag MP.meta_row_var_flag;
  add_quotation "mutable_flag" mutable_flag_quot ME.meta_mutable_flag MP.meta_mutable_flag;
  add_quotation "virtual_flag" virtual_flag_quot ME.meta_virtual_flag MP.meta_virtual_flag;
  add_quotation "override_flag" override_flag_quot ME.meta_override_flag MP.meta_override_flag;
  add_quotation "direction_flag" direction_flag_quot ME.meta_direction_flag MP.meta_direction_flag;
  Options.add ("-dlang", FanArg.Set_string Quotation.default," Set the default language");
end;



module IdQuotationExpander = struct
  let name = "Camlp4QuotationExpander";
  let version = Sys.ocaml_version;
end;

module MakeQuotationExpander (Syntax : Sig.Camlp4Syntax)
= struct
  module M = MakeQuotationCommon Syntax (* Syntax.AntiquotSyntax *);
  include M;
end;

let pa_r  = "Camlp4OCamlRevisedParser";    
let pa_r (module P:Sig.PRECAST) =
  P.syntax_extension (module IdRevisedParser)  (module MakeRevisedParser);

(*  "Camlp4OCamlRevisedParserParser"; *)
let pa_rp (module P:Sig.PRECAST) =
  P.syntax_extension (module IdRevisedParserParser)
    (module MakeRevisedParserParser);


let pa_g (module P:Sig.PRECAST) =
  P.syntax_extension (module IdGrammarParser) (module MakeGrammarParser);

(*  "Camlp4MacroParser"; *)
let pa_m (module P:Sig.PRECAST) =
  let () = P.syntax_extension (module IdMacroParser) (module MakeMacroParser) in
  P.syntax_plugin (module IdMacroParser) (module MakeNothing);

(*  "Camlp4QuotationExpander"; *)
let pa_q (module P:Sig.PRECAST) =
  P.syntax_extension (module IdQuotationExpander) (module MakeQuotationExpander);
  

let pa_l  (module P: Sig.PRECAST) =
  P.syntax_extension (module IdListComprehension) (module MakeListComprehension);


(* load debug parser for bootstrapping *)
let pa_debug (module P: Sig.PRECAST) =
  P.syntax_extension (module IdDebugParser) (module MakeDebugParser);



