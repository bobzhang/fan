open LibUtil;
open FanUtil;
open Lib;

module IdDebugParser = struct
  let name = "Camlp4DebugParser";
  let version = Sys.ocaml_version;
end;

module MakeDebugParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  open FanSig ; (* For FanToken, probably we should fix FanToken as well  *)
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
      if SSet.mem "*" sections then fun _ -> True
      else fun x -> SSet.mem x sections
    with [ Not_found -> fun _ -> False ];

  let mk_debug_mode _loc = fun [ None -> <:expr< Debug.mode >>
                                 | Some m -> <:expr< $uid:m.Debug.mode >> ];

  let mk_debug _loc m fmt section args =
    let call = Expr.apply <:expr< Debug.printf $str:section $str:fmt >> args in
      <:expr< if $(mk_debug_mode _loc m) $str:section then $call else () >>;
  EXTEND Gram
    GLOBAL: expr;
    expr:
    [ [ m = start_debug; `LIDENT section; `STRING _ fmt;
        args = LIST0 expr Level "."; x = end_or_in ->
      match (x, debug_mode section) with
      [ (None,   False) -> <:expr< () >>
      | (Some e, False) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> <:expr< let () = $(mk_debug _loc m fmt section args) in $e >> ]  ] ]
    end_or_in:
    [ [ "end" -> None
      | "in"; e = expr -> Some e  ] ]
    start_debug:
    [ [ `LIDENT "debug" -> None
      | `LIDENT "camlp4_debug" -> Some "Camlp4"  ] ]
  END;
end;

module IdGrammarParser = struct
  let name = "Camlp4GrammarParser";
  let version = Sys.ocaml_version;
end;

  
module MakeGrammarParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  open FanSig;
  open FanGrammar;
  open FanGrammarTools;
  FanConfig.antiquotations := True;
  EXTEND Gram GLOBAL: expr symbol rule rule_list psymbol level level_list;
    expr: After "top"
      [ [ "EXTEND"; e = extend_body; "END" -> e
        | "DELETE_RULE"; e = delete_rule_body; "END" -> e ] ] 
    extend_header:
      [ [ "("; i = qualid; ":"; t = t_qualid; ")" -> 
        let old=gm() in 
        let () = grammar_module_name := t in
        (Some i,old)
        | t = qualuid -> begin
            let old = gm() in
            let () = grammar_module_name := t in 
            (None,old)
        end
        | -> (None,gm()) (* FIXME *)   ] ]
    extend_body:
      [ [ (gram,old) = extend_header; global_list = OPT global;
          el = LIST1 [ e = entry  -> e ] -> (* semi_sep removed *)
            let res = text_of_functorial_extend _loc  gram global_list el in 
            let () = grammar_module_name := old in
            res      ] ] 
    delete_rule_body:
      [ [ old = delete_rule_header; n = name; ":"; sl = LIST0 symbol SEP semi_sep -> 
        let (e, b) = expr_of_delete_rule _loc n sl in (*FIXME*)
        let res =  <:expr< $(id:gm()).delete_rule $e $b >>  in
        let () = grammar_module_name := old  in 
        res   ] ]
     delete_rule_header: (*for side effets, parser action *)
        [[ g = qualuid ->
          let old = gm () in
          let () = grammar_module_name := g in
          old  ]]
    qualuid:
      [ [ `UIDENT x; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | `UIDENT x -> <:ident< $uid:x >> ] ] 
    qualid:
      [ [ `UIDENT x; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | `UIDENT i -> <:ident< $uid:i >>
        | `LIDENT i -> <:ident< $lid:i >> ] ]
    t_qualid:
      [ [ `UIDENT x; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | `UIDENT x; "."; `LIDENT "t" -> <:ident< $uid:x >> ] ] 
    global:
      [ [ `UIDENT "GLOBAL"; ":"; sl = LIST1 name; semi_sep -> sl ] ]
    entry:
      [ [ n = name; ":"; pos = OPT position; ll = level_list ->
            mk_entry ~name:n ~pos ~levels:ll ] ]
    position:
      [ [ `UIDENT ("First"|"Last" as x ) ->   <:expr< `$uid:x >>
        | `UIDENT ("Before" | "After" | "Level" as x) ; n = string ->
            <:expr< ` $uid:x  $n >> (*FIXME string escape?*)    ] ]
    level_list:
      [ [ "["; ll = LIST0 level SEP "|"; "]" -> ll ] ]
    level:
      [ [ lab = OPT [  `STRING _ x  -> x ]; ass = OPT assoc; rules = rule_list ->
            mk_level ~label:lab ~assoc:ass ~rules ]]
    assoc:
      [[ `UIDENT ("LA"|"RA"|"NA" as x) ->
         <:expr< `$uid:x >> 
       | `UIDENT x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ] ]
    rule_list:
      [ [ "["; "]" -> []
        | "["; rules = LIST1 rule SEP "|"; "]" ->
            retype_rule_list_without_patterns _loc rules ] ]
    rule:
      [ [ psl = LIST0 psymbol SEP semi_sep; "->"; act = expr ->
            mk_rule ~prod:psl ~action:(Some act )
        | psl = LIST0 psymbol SEP semi_sep ->
            mk_rule ~prod:psl ~action:None ] ]
    psymbol:
      [ [ `LIDENT p; "="; s = symbol ->
            match s.pattern with
            [ Some (<:patt< $uid:u $(tup:<:patt< _ >>) >> as p') ->
                let match_fun = <:expr< fun [ $pat:p' -> True | _ -> False ] >> in
                let p' = <:patt< ($p' as $lid:p) >> in
                let descr = u ^ " _" in
                let text = TXtok _loc match_fun descr in
                { (s) with text = text; pattern = Some p' }
            | _ -> { (s) with pattern = Some <:patt< $lid:p >> } ]

        | `LIDENT i; lev = OPT [`UIDENT "Level"; `STRING _ s -> s ] ->
            let name = mk_name _loc <:ident< $lid:i >> in
            let text = TXnterm _loc name lev in
            let styp = STquo _loc i in
            {used = [i]; text = text; styp = styp; pattern = None}
        |  p = pattern; "="; s = symbol ->
            match s.pattern with
            [ Some <:patt< $uid:u $(tup:<:patt< _ >>) >> ->
                mk_tok _loc <:patt< $uid:u $p >> s.styp
            | _ -> { (s) with pattern = Some p } ]
        | s = symbol -> s ] ]

    symbol:
      [ "top" NA
        [ `UIDENT ("LIST0"| "LIST1" as x); s = SELF; sep = OPT [`UIDENT "SEP"; t = symbol -> t ] ->
            let () = check_not_tok s in
            let used =  match sep with
              [ Some symb -> symb.used @ s.used
              | None -> s.used ]   in
            let styp = STapp _loc (STlid _loc "list") s.styp in
            let text = slist _loc
                (match x with
                ["LIST0" -> False | "LIST1" -> True
                | _ -> failwithf "only (LIST0|LIST1) allowed here"])  sep s in
            mk_symbol ~used ~text ~styp ~pattern:None

        |`UIDENT "OPT"; s = SELF ->
            let () = check_not_tok s in
            let styp = STapp _loc (STlid _loc "option") s.styp in
            let text = TXopt _loc s.text in
            mk_symbol ~used:s.used ~text ~styp ~pattern:None

        |`UIDENT "TRY"; s = SELF ->
            let text = TXtry _loc s.text in
            mk_symbol ~used:s.used ~text ~styp:(s.styp) ~pattern:None]

      | [`UIDENT "SELF" ->
          mk_symbol ~used:[] ~text:(TXself _loc)  ~styp:(STself _loc "SELF") ~pattern:None

        |`UIDENT "NEXT" ->
            mk_symbol ~used:[] ~text:(TXnext _loc)   ~styp:(STself _loc "NEXT") ~pattern:None

        | "["; rl = LIST0 rule SEP "|"; "]" ->
            let rl = retype_rule_list_without_patterns _loc rl in
            let t = new_type_var () in
            let used = used_of_rule_list rl in
            mk_symbol ~used ~text:(TXrules _loc (srules _loc t rl ""))
              ~styp:(STquo _loc t) ~pattern:None
        
            
        | "`"; p = patt -> mk_tok _loc p (STtok _loc)


        | `UIDENT x; `ANTIQUOT "" s ->
            let e = AntiquotSyntax.parse_expr _loc s in
            let match_fun = <:expr<
              fun [ $uid:x camlp4_x when camlp4_x = $e -> True | _ -> False ] >> in
            let descr = "$" ^ x ^ " " ^ s in
            let text = TXtok _loc match_fun descr in
            let p = <:patt< $uid:x $(tup:<:patt< _ >>) >> in
            mk_symbol ~used:[] ~text ~styp:(STtok _loc) ~pattern:(Some p)
              
        | `STRING _ s ->
            mk_symbol ~used:[] ~text:(TXkwd _loc s) ~styp:(STtok _loc) ~pattern:None

            
        | n = name; lev = OPT [`UIDENT "Level"; `STRING _ s -> s ] ->
            mk_symbol ~used:[n.tvar] ~text:(TXnterm _loc n lev) ~styp:(STquo _loc n.tvar) ~pattern:None
            
        | "("; s_t = SELF; ")" -> s_t ] ]
    pattern:
      [ [ `LIDENT i -> <:patt< $lid:i >>
        | "_" -> <:patt< _ >>
        | "("; p = pattern; ")" -> <:patt< $p >>
        | "("; p1 = pattern; ","; p2 = comma_patt; ")" -> <:patt< ( $p1, $p2 ) >>
      ] ]
    comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | p = pattern -> p
      ] ]
    name:
      [ [ il = qualid -> mk_name _loc il ] ]
    string:
      [ [ `STRING _ s -> <:expr< $str:s >>
        | `ANTIQUOT "" s -> AntiquotSyntax.parse_expr _loc s ] ]
    semi_sep:
      [ [ ";" -> () ] ]
  END;
  let sfold _loc  n foldfun f e s =
    let styp = STquo _loc (new_type_var ()) in
    let e = <:expr< $(id:gm()).$lid:foldfun $f $e >> in
    let t = STapp _loc (STapp _loc (STtyp <:ctyp< $(id:gm()).fold _ >>) s.styp) styp in
    {used = s.used; text = TXmeta _loc n [s.text] e t; styp = styp; pattern = None } ;

  let sfoldsep  _loc n foldfun f e s sep =
    let styp = STquo _loc (new_type_var ()) in
    let e = <:expr< $(id:gm()).$lid:foldfun $f $e >> in
    let t =
      STapp _loc (STapp _loc (STtyp <:ctyp< $(id:gm()).foldsep _ >>) s.styp) styp
    in
    {used = s.used @ sep.used; text = TXmeta _loc n [s.text; sep.text] e t;
    styp = styp; pattern = None} ;

  EXTEND Gram
    GLOBAL: symbol;
    symbol: Level "top"
      [ [`UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF ->
            sfold _loc "FOLD0" "sfold0" f e s
        |`UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF ->
            sfold _loc "FOLD1" "sfold1" f e s
        |`UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF;`UIDENT "SEP"; sep = symbol ->
            sfoldsep _loc "FOLD0 SEP" "sfold0sep" f e s sep
        |`UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF;
`UIDENT "SEP"; sep = symbol ->
            sfoldsep _loc "FOLD1 SEP" "sfold1sep" f e s sep ] ]
    simple_expr:
      [ [ i = a_LIDENT -> <:expr< $lid:i >>
        | "("; e = expr; ")" -> e ] ]
  END;

  Options.add "-split_ext" (Arg.Set split_ext)
    "Split EXTEND by functions to turn around a PowerPC problem.";

  Options.add "-split_gext" (Arg.Set split_ext)
    "Old name for the option -split_ext.";

  Options.add "-meta_action" (Arg.Set meta_action)
    "Undocumented"; (* FIXME *)

end;

module IdListComprehension = struct
  let name = "Camlp4ListComprehension";
  let version = Sys.ocaml_version;
end;

module MakeListComprehension (Syntax : Sig.Camlp4Syntax) = struct
  open FanSig;
  include Syntax;
  module Ast = Camlp4Ast;

  (* usual trick *) (* FIXME utilities based on Gram *)
  let test_patt_lessminus =
    Gram.of_parser "test_patt_lessminus"
      (fun strm ->
        let rec skip_patt n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD "<-") -> n
          | Some (KEYWORD ("[" | "[<")) ->
              skip_patt (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") ->
              skip_patt (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") ->
              skip_patt (ignore_upto "}" (n + 1) + 1)
          | Some (KEYWORD ("as" | "::" | "," | "_"))
          | Some (LIDENT _ | UIDENT _) -> skip_patt (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD prm) when prm = end_kwd -> n
          | Some (KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") ->
              ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") ->
              ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1)
          | Some _ -> ignore_upto end_kwd (n + 1)
          | None -> raise Stream.Failure ]
        in
        skip_patt 1);

  DELETE_RULE Gram expr: "["; sem_expr_for_list; "]" END;

  (* test wheter revised or not hack*)  
  let is_revised =
    try do {
      DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
      True
    } with [ Not_found -> False ];

  let comprehension_or_sem_expr_for_list =
    Gram.mk "comprehension_or_sem_expr_for_list";
  EXTEND Gram
    GLOBAL: expr comprehension_or_sem_expr_for_list;
    expr: Level "simple"
      [ [ "["; e = comprehension_or_sem_expr_for_list; "]" -> e ] ]  
    comprehension_or_sem_expr_for_list:
      [ [ e = expr Level "top"; ";"; mk = sem_expr_for_list ->
            <:expr< [ $e :: $(mk <:expr< [] >>) ] >>
        | e = expr Level "top"; ";" -> <:expr< [$e] >>
        | e = expr Level "top"; "|"; l = LIST1 item SEP ";" -> Expr.compr _loc e l
        | e = expr Level "top" -> <:expr< [$e] >> ] ]  
    item:
      (* NP: These rules rely on being on this particular order. Which should
             be improved. *)
      [ [ p = TRY [p = patt; "<-" -> p] ; e = expr Level "top" -> `gen (p, e)
        | e = expr Level "top" -> `cond e ] ] 
  END;
  if is_revised then
    EXTEND Gram
      GLOBAL: expr comprehension_or_sem_expr_for_list;
      comprehension_or_sem_expr_for_list:
      [ [ e = expr Level "top"; ";"; mk = sem_expr_for_list; "::"; last = expr ->
            <:expr< [ $e :: $(mk last) ] >>
        | e = expr Level "top"; "::"; last = expr ->
            <:expr< [ $e :: $last ] >> ] ] 
    END
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
  open FanSig;
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
        EXTEND Gram
        expr: Level "simple"
          [ [ UIDENT $x -> (new Ast.reloc _loc)#expr e ]] 
        patt: Level "simple"
          [ [ UIDENT $x ->
            let p = Expr.substp _loc [] e
            in (new Ast.reloc _loc)#patt p ]]
        END
      | Some (sl, e) ->
          EXTEND Gram
            expr: Level "apply"
            [ [ UIDENT $x; param = SELF ->
              let el =  match param with
              [ <:expr< ($tup:e) >> -> Ast.list_of_expr e []
              | e -> [e] ]  in
              if List.length el = List.length sl then
                let env = List.combine sl el in
                (new Expr.subst _loc env)#expr e
              else
                incorrect_number _loc el sl ] ] 
          patt: Level "simple"
            [ [ UIDENT $x; param = SELF ->
              let pl = match param with
              [ <:patt< ($tup:p) >> -> Ast.list_of_patt p []
              | p -> [p] ] in
              if List.length pl = List.length sl then
                let env = List.combine sl pl in
                let p = Expr.substp _loc env e in
                (new Ast.reloc _loc)#patt p
              else
                incorrect_number _loc pl sl ] ]
          END
      | None -> () ];
      defined := [(x, eo) :: !defined]
    end;

  let undef x =
    try
      begin
        let eo = List.assoc x !defined in
        match eo with
        [ Some ([], _) ->
            do {
              DELETE_RULE Gram expr: UIDENT $x END;
              DELETE_RULE Gram patt: UIDENT $x END;
            }
        | Some (_, _) ->
            do {
              DELETE_RULE Gram expr: UIDENT $x; SELF END;
              DELETE_RULE Gram patt: UIDENT $x; SELF END;
            }
        | None -> () ];
        defined := list_remove x !defined;
      end
    with
    [ Not_found -> () ];

  let parse_def s =
    match Gram.parse_string expr (FanLoc.mk "<command line>") s with
    [ <:expr< $uid:n >> -> define None n
    | <:expr< $uid:n = $e >> -> define (Some ([],e)) n
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

  let rec execute_macro nil cons =
    fun
    [ SdStr i -> i
    | SdDef x eo -> do { define eo x; nil }
    | SdUnd x -> do { undef x; nil }
    | SdITE b l1 l2 -> execute_macro_list nil cons (if b then l1 else l2)
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

  EXTEND Gram
    GLOBAL: expr patt str_item sig_item;
    str_item: First
      [ [ x = macro_def ->
            execute_macro <:str_item<>> (fun a b -> <:str_item< $a; $b >>) x ] ]
    sig_item: First
      [ [ x = macro_def_sig ->
            execute_macro <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) x ] ]
    macro_def:
      [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; st1 = smlist_then; st2 = else_macro_def ->
            make_SdITE_result st1 st2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; st1 = smlist_then; st2 = else_macro_def ->
            make_SdITE_result st1 st2
        | "INCLUDE"; `STRING _ fname ->
            SdLazy (lazy (parse_include_file str_items fname)) ] ] 
    macro_def_sig:
      [ [ "DEFINE"; i = uident -> SdDef i None
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "INCLUDE"; `STRING _ fname ->
            SdLazy (lazy (parse_include_file sig_items fname)) ] ] 
    uident_eval_ifdef:
      [ [ i = uident -> Stack.push (is_defined i) stack ]] 
    uident_eval_ifndef:
      [ [ i = uident -> Stack.push (not (is_defined i)) stack ]] 
    else_macro_def:
      [ [ "ELSE"; st = smlist_else; endif -> st
        | endif -> [] ] ]  
    else_macro_def_sig:
      [ [ "ELSE"; st = sglist_else; endif -> st
        | endif -> [] ] ]  
    else_expr:
      [ [ "ELSE"; e = expr; endif -> e
      | endif -> <:expr< () >> ] ] 
    smlist_then:
      [ [ sml = LIST1 [ d = macro_def; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Then d
                      | si = str_item; semi -> SdStr si ] -> sml ] ] 
    smlist_else:
      [ [ sml = LIST1 [ d = macro_def; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Else d
                      | si = str_item; semi -> SdStr si ] -> sml ] ] 
    sglist_then:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Then d
                      | si = sig_item; semi -> SdStr si ] -> sgl ] ]  
    sglist_else:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Else d
                      | si = sig_item; semi -> SdStr si ] -> sgl ] ]  
    endif:
      [ [ "END" -> ()
        | "ENDIF" -> () ] ]  
    opt_macro_value:
      [ [ "("; pl = LIST1 [ `LIDENT x -> x ] SEP ","; ")"; "="; e = expr -> Some (pl, e)
        | "="; e = expr -> Some ([], e)
        | -> None ] ]  
    expr: Level "top"
      [ [ "IFDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e1 else e2
        | "IFNDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e2 else e1
        | "DEFINE"; `LIDENT i; "="; def = expr; "IN"; body = expr ->
            (new Expr.subst _loc [(i, def)])#expr body ] ] 
    patt:
      [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p1 else p2
        | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p2 else p1 ] ] 
    uident:
      [ [ `UIDENT i -> i ] ]  
    (* dirty hack to allow polymorphic variants using the introduced keywords. *)
    expr: Before "simple"
      [ [ "`"; kwd = [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
                     | "DEFINE" | "IN" ] -> <:expr< `$uid:kwd >>
        | "`"; s = a_ident -> <:expr< ` $s >> ] ] 
    (* idem *)
    patt: Before "simple"
      [ [ "`"; kwd = [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ] ->
            <:patt< `$uid:kwd >>
        | "`"; s = a_ident -> <:patt< ` $s >> ] ]
  END;

  Options.add "-D" (Arg.String parse_def)
    "<string> Define for IFDEF instruction.";
  Options.add "-U" (Arg.String undef)
    "<string> Undefine for IFDEF instruction.";
  Options.add "-I" (Arg.String add_include_dir)
    "<string> Add a directory to INCLUDE search path.";
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
  open FanSig;
  include Syntax;
  module Ast = Camlp4Ast;
  FanConfig.constructors_arity := False;

  let help_sequences () =
    do {
      Printf.eprintf "\
New syntax:\
\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\
\n    while e do e1; e2; ... ; en done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\
\nOld syntax (still supported):\
\n    do {e1; e2; ... ; en}\
\n    while e do {e1; e2; ... ; en}\
\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\
\nVery old (no more supported) syntax:\
\n    do e1; e2; ... ; en-1; return en\
\n    while e do e1; e2; ... ; en; done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\
\n";
      flush stderr;
      exit 1
    }
  ;
  Options.add "-help_seq" (Arg.Unit help_sequences)
    "Print explanations about new sequences and exit.";
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
  Gram.clear label_ipatt;
  Gram.clear label_ipatt_list;
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

  let setup_op_parser entry p =
    Gram.setup_parser entry
      (parser
        [< (KEYWORD x | SYMBOL x, ti) when p x >] ->
          let _loc = Gram.token_location ti in
          <:expr< $lid:x >>);

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

  let rec infix_kwds_filter =
    parser
    [ [< ((KEYWORD "(", _) as tok); 'xs >] ->
        match xs with parser
        [ [< (KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i), _loc);
             (KEYWORD ")", _); 'xs >] ->
                [< (LIDENT i, _loc); '(infix_kwds_filter xs) >]
        | [< 'xs >] ->
                [< tok; '(infix_kwds_filter xs) >] ]
    | [< x; 'xs >] -> [< x; '(infix_kwds_filter xs) >] ];

  Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.setup_parser sem_expr begin
    let symb1 = Gram.parse_origin_tokens expr in
    let symb =
      parser
      [ [< (ANTIQUOT ("list" as n) s, ti) >] ->
        let _loc = Gram.token_location ti in
        <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
      | [< a = symb1 >] -> a ]
    in
    let rec kont al =
      parser
      [ [< (KEYWORD ";", _); a = symb; 's >] ->
        let _loc = FanLoc.merge (Ast.loc_of_expr al)
                             (Ast.loc_of_expr a) in
        kont <:expr< $al; $a >> s
      | [< >] -> al ]
    in
    parser [< a = symb; 's >] -> kont a s
  end;

  EXTEND Gram
    GLOBAL:
      a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT rec_binding_quot
      a_NATIVEINT a_OPTLABEL a_STRING a_UIDENT a_ident
      amp_ctyp and_ctyp match_case match_case0 match_case_quot binding binding_quot
      class_declaration class_description class_expr class_expr_quot
      class_fun_binding class_fun_def class_info_for_class_expr
      class_info_for_class_type class_longident class_longident_and_param
      class_name_and_param class_sig_item class_sig_item_quot class_signature
      class_str_item class_str_item_quot class_structure class_type
      class_type_declaration class_type_longident
      class_type_longident_and_param class_type_plus class_type_quot
      comma_ctyp comma_expr comma_ipatt comma_patt comma_type_parameter
      constrain constructor_arg_list constructor_declaration
      constructor_declarations ctyp ctyp_quot cvalue_binding direction_flag
      dummy eq_expr expr expr_eoi expr_quot field_expr field_expr_list fun_binding
      fun_def ident ident_quot implem interf ipatt ipatt_tcon label
      label_declaration label_declaration_list label_expr label_expr_list
      label_ipatt label_ipatt_list label_longident label_patt label_patt_list
      labeled_ipatt let_binding meth_list meth_decl module_binding module_binding0
      module_binding_quot module_declaration module_expr module_expr_quot
      module_longident module_longident_with_app module_rec_declaration
      module_type module_type_quot more_ctyp name_tags opt_as_lident
      opt_class_self_patt opt_class_self_type opt_comma_ctyp opt_dot_dot opt_eq_ctyp opt_expr
      opt_meth_list opt_mutable opt_polyt opt_private opt_rec
      opt_virtual opt_when_expr patt patt_as_patt_opt patt_eoi
      patt_quot patt_tcon phrase poly_type row_field
      sem_expr sem_expr_for_list sem_patt sem_patt_for_list semi sequence
      sig_item sig_item_quot sig_items star_ctyp str_item str_item_quot
      str_items top_phrase type_constraint type_declaration
      type_ident_and_parameters type_kind type_longident
      type_longident_and_parameters type_parameter type_parameters typevars
      use_file val_longident (* value_let *) (* value_val *) with_constr with_constr_quot
      infixop0 infixop1 infixop2 infixop3 infixop4 do_sequence package_type
      rec_flag_quot direction_flag_quot mutable_flag_quot private_flag_quot
      virtual_flag_quot row_var_flag_quot override_flag_quot;
    module_expr:
      [ "top"
        [ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
          me = SELF ->
            <:module_expr< functor ( $i : $t ) -> $me >>
        | "struct"; st = str_items; "end" ->
            <:module_expr< struct $st end >> ]
      | "apply"
        [ me1 = SELF; me2 = SELF -> <:module_expr< $me1 $me2 >> ]
      | "simple"
        [ `ANTIQUOT (""|"mexp"|"anti"|"list" as n) s ->
            <:module_expr< $(anti:mk_anti ~c:"module_expr" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.module_expr_tag
        | i = module_longident -> <:module_expr< $id:i >>
        | "("; me = SELF; ":"; mt = module_type; ")" ->
            <:module_expr< ( $me : $mt ) >>
        | "("; me = SELF; ")" -> <:module_expr< $me >>
        | "("; "val"; e = expr; ")" -> (* val *)
            <:module_expr< (val $e) >>  (* first class modules *)
        | "("; "val"; e = expr; ":"; p = package_type; ")" ->
            <:module_expr< (val $e : $p) >> ] ]
    str_item:
      [ "top"
        [ "exception"; t = constructor_declaration ->
            <:str_item< exception $t >>
        | "exception"; t = constructor_declaration; "="; i = type_longident ->
            <:str_item< exception $t = $i >>
        | "external"; i = a_LIDENT; ":"; t = ctyp; "="; sl = string_list ->
            <:str_item< external $i : $t = $sl >>
        | "include"; me = module_expr -> <:str_item< include $me >>
        | "module"; i = a_UIDENT; mb = module_binding0 ->
            <:str_item< module $i = $mb >>
        | "module"; "rec"; mb = module_binding ->
            <:str_item< module rec $mb >>
        | "module"; "type"; i = a_ident; "="; mt = module_type ->
            <:str_item< module type $i = $mt >>
        | "open"; i = module_longident -> <:str_item< open $i >>
        | "type"; td = type_declaration ->
            <:str_item< type $td >>
        | "let"; r = opt_rec; bi = binding; "in"; x = expr ->
              <:str_item< let $rec:r $bi in $x >>
        | "let"; r = opt_rec; bi = binding ->   match bi with
            [ <:binding< _ = $e >> -> <:str_item< $exp:e >>
            | _ -> <:str_item< let $rec:r $bi >> ]
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr ->
              <:str_item< let module $m = $mb in $e >>
        | "let"; "open"; i = module_longident; "in"; e = expr ->
              <:str_item< let open $id:i in $e >>

        | "class"; cd = class_declaration ->
            <:str_item< class $cd >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:str_item< class type $ctd >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $(anti:mk_anti ~c:"str_item" n s) >>
            (*
              first, it gives "mk_anti ~c:"str_item" n s" , and then through
              the meta operation, it gets
              (Ast.StAnt (_loc, ( (mk_anti ~c:"str_item" n s) )))
             *)
        | `QUOTATION x -> Quotation.expand _loc x DynAst.str_item_tag
        | e = expr -> <:str_item< $exp:e >>
        (* this entry makes <:str_item< let $rec:r $bi in $x >> parsable *)
        ] ]
    module_binding0:
      [ RA
        [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
            <:module_expr< functor ( $m : $mt ) -> $mb >>
        | ":"; mt = module_type; "="; me = module_expr ->
            <:module_expr< ( $me : $mt ) >>
        | "="; me = module_expr -> <:module_expr< $me >> ] ]
    module_binding:
      [ LA
        [ b1 = SELF; "and"; b2 = SELF ->
            <:module_binding< $b1 and $b2 >>
        | `ANTIQUOT ("module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $(mk_anti n m) : $mt = $me >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $m : $mt = $me >> ] ]
    module_type:
      [ "top"
        [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
            <:module_type< functor ( $i : $t ) -> $mt >> ]
      | "with"
        [ mt = SELF; "with"; wc = with_constr ->
            <:module_type< $mt with $wc >> ]
      | "apply"
        [ mt1 = SELF; mt2 = SELF; dummy -> ModuleType.app mt1 mt2 ]
      | "."
        [ mt1 = SELF; "."; mt2 = SELF -> ModuleType.acc mt1 mt2 ]
      | "sig"
        [ "sig"; sg = sig_items; "end" ->
            <:module_type< sig $sg end >> ]
      | "simple"
        [ `ANTIQUOT (""|"mtyp"|"anti"|"list" as n) s ->
            <:module_type< $(anti:mk_anti ~c:"module_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_type_tag
        | i = module_longident_with_app -> <:module_type< $id:i >>
        | "'"; i = a_ident -> <:module_type< ' $i >>
        | "("; mt = SELF; ")" -> <:module_type< $mt >>
        | "module"; "type"; "of"; me = module_expr ->
            <:module_type< module type of $me >> ] ]
    sig_item:
      [ "top"
        [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $(anti:mk_anti ~c:"sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.sig_item_tag
        | "exception"; t = constructor_declaration ->
            <:sig_item< exception $t >>
        | "external"; i = a_LIDENT; ":"; t = ctyp; "="; sl = string_list ->
            <:sig_item< external $i : $t = $sl >>
        | "include"; mt = module_type -> <:sig_item< include $mt >>
        | "module"; i = a_UIDENT; mt = module_declaration ->
            <:sig_item< module $i : $mt >>
        | "module"; "rec"; mb = module_rec_declaration ->
            <:sig_item< module rec $mb >>
        | "module"; "type"; i = a_ident; "="; mt = module_type ->
            <:sig_item< module type $i = $mt >>
        | "module"; "type"; i = a_ident ->
            <:sig_item< module type $i >>
        | "open"; i = module_longident -> <:sig_item< open $i >>
        | "type"; t = type_declaration ->
            <:sig_item< type $t >>
        | "val"; i = a_LIDENT; ":"; t = ctyp ->
            <:sig_item< val $i : $t >>
        | "class"; cd = class_description ->
            <:sig_item< class $cd >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:sig_item< class type $ctd >> ] ]
    module_declaration:
      [ RA
        [ ":"; mt = module_type -> <:module_type< $mt >>
        | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
            <:module_type< functor ( $i : $t ) -> $mt >> ] ]
    module_rec_declaration:
      [ LA
        [ m1 = SELF; "and"; m2 = SELF -> <:module_binding< $m1 and $m2 >>
        | `ANTIQUOT (""|"module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type -> <:module_binding< $m : $mt >>
      ] ]
    with_constr:
      [ LA
        [ wc1 = SELF; "and"; wc2 = SELF -> <:with_constr< $wc1 and $wc2 >>
        | `ANTIQUOT (""|"with_constr"|"anti"|"list" as n) s ->
            <:with_constr< $(anti:mk_anti ~c:"with_constr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.with_constr_tag
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; "="; t = ctyp ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) = $t >>
        | "type"; t1 = type_longident_and_parameters; "="; t2 = ctyp ->
            <:with_constr< type $t1 = $t2 >>
        | "module"; i1 = module_longident; "="; i2 = module_longident_with_app ->
            <:with_constr< module $i1 = $i2 >>
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; ":="; t = ctyp ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) := $t >>
        | "type"; t1 = type_longident_and_parameters; ":="; t2 = ctyp ->
            <:with_constr< type $t1 := $t2 >>
        | "module"; i1 = module_longident; ":="; i2 = module_longident_with_app ->
            <:with_constr< module $i1 := $i2 >> ] ]
    expr:
      [ "top" RA
        [ "let"; r = opt_rec; bi = binding; "in"; x = SELF ->
            <:expr< let $rec:r $bi in $x >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = SELF ->
            <:expr< let module $m = $mb in $e >>
        | "let"; "open"; i = module_longident; "in"; e = SELF ->
            <:expr< let open $id:i in $e >>
        | "fun"; "["; a = LIST0 match_case0 SEP "|"; "]" ->
            <:expr< fun [ $list:a ] >>
        | "fun"; e = fun_def -> e
        | "match"; e = sequence; "with"; a = match_case ->
            <:expr< match $(Expr.mksequence' _loc e) with [ $a ] >>
        | "try"; e = sequence; "with"; a = match_case ->
            <:expr< try $(Expr.mksequence' _loc e) with [ $a ] >>
        | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
            <:expr< if $e1 then $e2 else $e3 >>
        | "do"; seq = do_sequence -> Expr.mksequence _loc seq
        | "for"; i = a_LIDENT; "="; e1 = sequence; df = direction_flag;
          e2 = sequence; "do"; seq = do_sequence ->
            <:expr< for $i = $(Expr.mksequence' _loc e1) $to:df $(Expr.mksequence' _loc e2) do
              { $seq } >>
        | "while"; e = sequence; "do"; seq = do_sequence ->
            <:expr< while $(Expr.mksequence' _loc e) do { $seq } >>
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:expr< object ($csp) $cst end >> ]
      | "where"
        [ e = SELF; "where"; rf = opt_rec; lb = let_binding ->
            <:expr< let $rec:rf $lb in $e >> ]
      | ":=" NA
        [ e1 = SELF; ":="; e2 = SELF; dummy ->
              <:expr< $e1 := $e2 >> 
        | e1 = SELF; "<-"; e2 = SELF; dummy -> (* FIXME should be deleted in original syntax later? *)
            match Expr.bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1 <- $e2 >> 
            ]  
        ]
      | "||" RA
        [ e1 = SELF; op = infixop6; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "&&" RA
        [ e1 = SELF; op = infixop5; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "<" LA
        [ e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "^" RA
        [ e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "+" LA
        [ e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "*" LA
        [ e1 = SELF; "land"; e2 = SELF -> <:expr< $e1 land $e2 >>
        | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1 lor $e2 >>
        | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1 lxor $e2 >>
        | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1 mod $e2 >>
        | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "**" RA
        [ e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1 asr $e2 >>
        | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1 lsl $e2 >>
        | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1 lsr $e2 >>
        | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "unary minus" NA
        [ "-"; e = SELF -> Expr.mkumin _loc "-" e
        | "-."; e = SELF -> Expr.mkumin _loc "-." e ]
      | "apply" LA
        [ e1 = SELF; e2 = SELF -> <:expr< $e1 $e2 >>
        | "assert"; e = SELF -> Expr.mkassert _loc e
        | "new"; i = class_longident -> <:expr< new $i >>
        | "lazy"; e = SELF -> <:expr< lazy $e >> ]
      | "label" NA
        [ "~"; i = a_LIDENT; ":"; e = SELF -> <:expr< ~ $i : $e >>
        | "~"; i = a_LIDENT -> <:expr< ~ $i >>

        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; e = SELF -> <:expr< ~ $i : $e >>

        (* Same remark for ?a:b *)
        | `OPTLABEL i; e = SELF -> <:expr< ? $i : $e >>

        | "?"; i = a_LIDENT; ":"; e = SELF -> <:expr< ? $i : $e >>
        | "?"; i = a_LIDENT -> <:expr< ? $i >> ]
      | "." LA
        [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1 .( $e2 ) >>
        | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1 .[ $e2 ] >>
        | e1 = SELF; "."; "{"; e2 = comma_expr; "}" -> Expr.bigarray_get _loc e1 e2
        | e1 = SELF; "."; e2 = SELF -> <:expr< $e1 . $e2 >>
        | e = SELF; "#"; lab = label -> <:expr< $e # $lab >> ]
      | "~-" NA
        [ "!"; e = SELF ->  <:expr< ! $e>>
        | f = prefixop; e = SELF -> <:expr< $f $e >> ]
      | "simple"
        [ `QUOTATION x -> Quotation.expand _loc x DynAst.expr_tag
        | `ANTIQUOT ("exp"|""|"anti" as n) s ->
            <:expr< $(anti:mk_anti ~c:"expr" n s) >>
        | `ANTIQUOT ("`bool" as n) s ->
            <:expr< $(id:<:ident< $(anti:mk_anti n s) >>) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:expr< $(tup: <:expr< $(anti:mk_anti ~c:"expr" n s) >>) >>
        | `ANTIQUOT ("seq" as n) s ->
            <:expr< do $(anti:mk_anti ~c:"expr" n s) done >>
        | s = a_INT -> <:expr< $int:s >>
        | s = a_INT32 -> <:expr< $int32:s >>
        | s = a_INT64 -> <:expr< $int64:s >>
        | s = a_NATIVEINT -> <:expr< $nativeint:s >>
        | s = a_FLOAT -> <:expr< $flo:s >>
        | s = a_STRING -> <:expr< $str:s >>
        | s = a_CHAR -> <:expr< $chr:s >>
        | i = TRY module_longident_dot_lparen; e = sequence; ")" ->
            <:expr< let open $i in $e >>
        | i = TRY val_longident -> <:expr< $id:i >>
        | "`"; s = a_ident -> <:expr< ` $s >>
        | "["; "]" -> <:expr< [] >>
        | "["; mk_list = sem_expr_for_list; "::"; last = expr; "]" ->
            mk_list last
        | "["; mk_list = sem_expr_for_list; "]" ->
            mk_list <:expr< [] >>
        | "[|"; "|]" -> <:expr< [| $(<:expr<>>) |] >>
        | "[|"; el = sem_expr; "|]" -> <:expr< [| $el |] >>
        | "{"; el = label_expr_list; "}" -> <:expr< { $el } >>
        | "{"; "("; e = SELF; ")"; "with"; el = label_expr_list; "}" ->
            <:expr< { ($e) with $el } >>
        | "{<"; ">}" -> <:expr< {<>} >>
        | "{<"; fel = field_expr_list; ">}" -> <:expr< {< $fel >} >>
        | "("; ")" -> <:expr< () >>
        | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e : $t) >>
        | "("; e = SELF; ","; el = comma_expr; ")" -> <:expr< ( $e, $el ) >>
        | "("; e = SELF; ";"; seq = sequence; ")" -> Expr.mksequence _loc <:expr< $e; $seq >>
        | "("; e = SELF; ";"; ")" -> Expr.mksequence _loc e
        | "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
            <:expr< ($e : $t :> $t2 ) >>
        | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e :> $t) >>
        | "("; e = SELF; ")" -> e
        | "begin"; seq = sequence; "end" -> Expr.mksequence _loc seq
        | "begin"; "end" -> <:expr< () >>
        | "("; "module"; me = module_expr; ")" ->
            <:expr< (module $me) >>
        | "("; "module"; me = module_expr; ":"; pt = package_type; ")" ->
            <:expr< (module $me : $pt) >>
        ] ]
    do_sequence:
      [ [ seq = TRY ["{"; seq = sequence; "}" -> seq] -> seq
        | TRY ["{"; "}"] -> <:expr< () >>
        | seq = TRY [seq = sequence; "done" -> seq] -> seq
        | "done" -> <:expr< () >>
      ] ]
    infixop5:
      [ [ x = [ "&" | "&&" ] -> <:expr< $lid:x >> ] ]
    infixop6:
      [ [ x = [ "or" | "||" ] -> <:expr< $lid:x >> ] ]
    sem_expr_for_list:
      [ [ e = expr; ";"; el = SELF -> fun acc -> <:expr< [ $e :: $(el acc) ] >>
        | e = expr; ";" -> fun acc -> <:expr< [ $e :: $acc ] >>
        | e = expr -> fun acc -> <:expr< [ $e :: $acc ] >>
      ] ]
    comma_expr:
      [ [ e1 = SELF; ","; e2 = SELF -> <:expr< $e1, $e2 >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $(anti:mk_anti ~c:"expr," n s) >>
        | e = expr Level "top" -> e ] ]
    dummy:
      [ [ -> () ] ]
    sequence':
      [ [ -> fun e -> e
        | ";" -> fun e -> e
        | ";"; el = sequence -> fun e -> <:expr< $e; $el >> ] ]
    sequence:
      [ [ "let"; rf = opt_rec; bi = binding; "in"; e = expr; k = sequence' ->
            k <:expr< let $rec:rf $bi in $e >>
        | "let"; rf = opt_rec; bi = binding; ";"; el = SELF ->
            <:expr< let $rec:rf $bi in $(Expr.mksequence _loc el) >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr; k = sequence' ->
            k <:expr< let module $m = $mb in $e >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; ";"; el = SELF ->
            <:expr< let module $m = $mb in $(Expr.mksequence _loc el) >>
        | "let"; "open"; i = module_longident; "in"; e = SELF ->
            <:expr< let open $id:i in $e >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
        | e = expr; k = sequence' -> k e ] ]
    binding:
      [ LA
        [ `ANTIQUOT ("binding"|"list" as n) s ->
            <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | `ANTIQUOT (""|"anti" as n) s; "="; e = expr ->
            <:binding< $(anti:mk_anti ~c:"patt" n s) = $e >>
        | `ANTIQUOT (""|"anti" as n) s -> <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | b1 = SELF; "and"; b2 = SELF -> <:binding< $b1 and $b2 >>
        | b = let_binding -> b
      ] ]
    let_binding:
      [ [ p = ipatt; e = fun_binding -> <:binding< $p = $e >> ] ]
    fun_binding:
      [ RA
        [ TRY ["("; "type"]; i = a_LIDENT; ")"; e = SELF ->
            <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; e = SELF ->
            <:expr< fun $p -> $e >>
        | bi = cvalue_binding -> bi
      ] ]
    match_case:
      [ [ "["; l = LIST0 match_case0 SEP "|"; "]" -> Ast.mcOr_of_list l
        | p = ipatt; "->"; e = expr -> <:match_case< $p -> $e >> ] ]
    match_case0:
      [ [ `ANTIQUOT ("match_case"|"list" as n) s ->
            <:match_case< $(anti:mk_anti ~c:"match_case" n s) >>
        | `ANTIQUOT (""|"anti" as n) s ->
            <:match_case< $(anti:mk_anti ~c:"match_case" n s) >>
        | `ANTIQUOT (""|"anti" as n) s; "->"; e = expr ->
            <:match_case< $(anti:mk_anti ~c:"patt" n s) -> $e >>
        | `ANTIQUOT (""|"anti" as n) s; "when"; w = expr; "->"; e = expr ->
            <:match_case< $(anti:mk_anti ~c:"patt" n s) when $w -> $e >>
        | p = patt_as_patt_opt; w = opt_when_expr; "->"; e = expr ->
            <:match_case< $p when $w -> $e >>
      ] ]
    opt_when_expr:
      [ [ "when"; w = expr -> w
        | -> <:expr<>>
      ] ]
    patt_as_patt_opt:
      [ [ p1 = patt; "as"; p2 = patt -> <:patt< ($p1 as $p2) >>
        | p = patt -> p
      ] ]
    label_expr_list:
      [ [ b1 = label_expr; ";"; b2 = SELF -> <:rec_binding< $b1 ; $b2 >>
        | b1 = label_expr; ";"            -> b1
        | b1 = label_expr                 -> b1
      ] ]
    label_expr:
      [ [ `ANTIQUOT ("rec_binding" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT (""|"anti" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT (""|"anti" as n) s; "="; e = expr ->
            <:rec_binding< $(anti:mk_anti ~c:"ident" n s) = $e >>
        | `ANTIQUOT ("list" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | i = label_longident; e = fun_binding -> <:rec_binding< $i = $e >>
        | i = label_longident ->
            <:rec_binding< $i = $(lid:Ident.to_lid i) >> ] ]
    fun_def:
      [ [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; (w, e) = fun_def_cont ->
            <:expr< fun [ $p when $w -> $e ] >> ] ]
    fun_def_cont:
      [ RA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            (<:expr<>>, <:expr< fun (type $i) -> $e >>)
        | p = TRY labeled_ipatt; (w,e) = SELF ->
            (<:expr<>>, <:expr< fun [ $p when $w -> $e ] >>)
        | "when"; w = expr; "->"; e = expr -> (w, e)
        | "->"; e = expr -> (<:expr<>>, e) ] ]
    fun_def_cont_no_when:
      [ RA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when -> <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; (w,e) = fun_def_cont ->
            <:expr< fun [ $p when $w -> $e ] >>
        | "->"; e = expr -> e ] ]
    patt:
      [ "|" LA
        [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1 | $p2 >> ]
      | ".." NA
        [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1 .. $p2 >> ]
      | "apply" LA
        [ p1 = SELF; p2 = SELF -> <:patt< $p1 $p2 >>
        | "lazy"; p = SELF -> <:patt< lazy $p >>  ]
      | "simple"
        [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >> )) >>
        | `ANTIQUOT ("`bool" as n) s ->
            <:patt< $(id:<:ident< $(anti:mk_anti n s) >>) >>
        | i = ident -> <:patt< $id:i >>
        | s = a_INT -> <:patt< $int:s >>
        | s = a_INT32 -> <:patt< $int32:s >>
        | s = a_INT64 -> <:patt< $int64:s >>
        | s = a_NATIVEINT -> <:patt< $nativeint:s >>
        | s = a_FLOAT -> <:patt< $flo:s >>
        | s = a_STRING -> <:patt< $str:s >>
        | s = a_CHAR -> <:patt< $chr:s >>
        | "-"; s = a_INT -> <:patt< $(int:neg_string s) >>
        | "-"; s = a_INT32 -> <:patt< $(int32:neg_string s) >>
        | "-"; s = a_INT64 -> <:patt< $(int64:neg_string s) >>
        | "-"; s = a_NATIVEINT -> <:patt< $(nativeint:neg_string s) >>
        | "-"; s = a_FLOAT -> <:patt< $(flo:neg_string s) >>
        | "["; "]" -> <:patt< [] >>
        | "["; mk_list = sem_patt_for_list; "::"; last = patt; "]" ->
            mk_list last
        | "["; mk_list = sem_patt_for_list; "]" ->
            mk_list <:patt< [] >>
        | "[|"; "|]" -> <:patt< [| $(<:patt<>>) |] >>
        | "[|"; pl = sem_patt; "|]" -> <:patt< [| $pl |] >>
        | "{"; pl = label_patt_list; "}" -> <:patt< { $pl } >>
        | "("; ")" -> <:patt< () >>
        | "("; "module"; m = a_UIDENT; ")" -> <:patt< (module $m) >>
        | "("; "module"; m = a_UIDENT; ":"; pt = package_type; ")" ->
            <:patt< ((module $m) : (module $pt)) >>
        | "("; p = SELF; ")" -> p
        | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p : $t) >>
        | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p as $p2) >>
        | "("; p = SELF; ","; pl = comma_patt; ")" -> <:patt< ($p, $pl) >>
        | "_" -> <:patt< _ >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "`"; s = a_ident -> <:patt< ` $s >>
        | "#"; i = type_longident -> <:patt< # $i >>
        | `LABEL i; p = SELF -> <:patt< ~ $i : $p >>
        | "~"; `ANTIQUOT (""|"lid" as n) i; ":"; p = SELF ->
            <:patt< ~ $(mk_anti n i) : $p >>
        | "~"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ~ $(mk_anti n i) >>
        | "~"; `LIDENT i -> <:patt< ~ $i >>
        (* | i = opt_label; "("; p = patt_tcon; ")" -> *)
            (* <:patt< ? $i$ : ($p$) >> *)
        | `OPTLABEL i; "("; p = patt_tcon; f = eq_expr; ")" -> f i p
        | "?"; `ANTIQUOT (""|"lid" as n) i; ":"; "("; p = patt_tcon; f = eq_expr; ")" ->
            f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i >>
        | "?"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ? $(mk_anti n i) >>
        | "?"; "("; p = patt_tcon; ")" ->
            <:patt< ? ($p) >>
        | "?"; "("; p = patt_tcon; "="; e = expr; ")" ->
            <:patt< ? ($p = $e) >> ] ]
    comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | p = patt -> p ] ]
    sem_patt:
      [ LA
        [ p1 = patt; ";"; p2 = SELF -> <:patt< $p1; $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | p = patt; ";" -> p
        | p = patt -> p ] ]
    sem_patt_for_list:
      [ [ p = patt; ";"; pl = SELF -> fun acc -> <:patt< [ $p :: $(pl acc) ] >>
        | p = patt; ";" -> fun acc -> <:patt< [ $p :: $acc ] >>
        | p = patt -> fun acc -> <:patt< [ $p :: $acc ] >>
      ] ]
    label_patt_list:
      [ [ p1 = label_patt; ";"; p2 = SELF -> <:patt< $p1 ; $p2 >>
        | p1 = label_patt; ";"; "_"       -> <:patt< $p1 ; _ >>
        | p1 = label_patt; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | p1 = label_patt; ";"            -> p1
        | p1 = label_patt                 -> p1
      ] ]
    label_patt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | `ANTIQUOT ("list" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | i = label_longident; "="; p = patt -> <:patt< $i = $p >>
        | i = label_longident -> <:patt< $i = $(lid:Ident.to_lid i) >>
      ] ]
    ipatt:
      [ [ "{"; pl = label_ipatt_list; "}" -> <:patt< { $pl } >>
        | `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >>)) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "("; ")" -> <:patt< () >>
        | "("; "module"; m = a_UIDENT; ")" -> <:patt< (module $m) >>
        | "("; "module"; m = a_UIDENT; ":"; pt = package_type; ")" ->
            <:patt< ((module $m) : (module $pt)) >>
        | "("; p = SELF; ")" -> p
        | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p : $t) >>
        | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p as $p2) >>
        | "("; p = SELF; ","; pl = comma_ipatt; ")" -> <:patt< ($p, $pl) >>
        | s = a_LIDENT -> <:patt< $lid:s >>
        | "_" -> <:patt< _ >> ] ]
    labeled_ipatt:
      [ [ p = ipatt -> p ] ]
    comma_ipatt:
      [ LA
        [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | p = ipatt -> p ] ]
    label_ipatt_list:
      [ [ p1 = label_ipatt; ";"; p2 = SELF -> <:patt< $p1 ; $p2 >>
        | p1 = label_ipatt; ";"; "_"       -> <:patt< $p1 ; _ >>
        | p1 = label_ipatt; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | p1 = label_ipatt; ";"            -> p1
        | p1 = label_ipatt                 -> p1
      ] ]
    label_ipatt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.patt_tag
        | i = label_longident; "="; p = ipatt -> <:patt< $i = $p >>
      ] ]
    type_declaration:
      [ LA
        [ `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1 and $t2 >>
        | (n, tpl) = type_ident_and_parameters; tk = opt_eq_ctyp;
          cl = LIST0 constrain -> Ast.TyDcl _loc n tpl tk cl ] ]
    constrain:
      [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
    opt_eq_ctyp:
      [ [ "="; tk = type_kind -> tk
        | -> <:ctyp<>> ] ]
    type_kind:
      [ [ t = ctyp -> t ] ]
    type_ident_and_parameters:
      [ [ i = a_LIDENT; tpl = LIST0 optional_type_parameter -> (i, tpl) ] ]
    type_longident_and_parameters:
      [ [ i = type_longident; tpl = type_parameters -> tpl <:ctyp< $id:i >>
      ] ]
    type_parameters:
      [ [ t1 = type_parameter; t2 = SELF ->
            fun acc -> t2 <:ctyp< $acc $t1 >>
        | t = type_parameter -> fun acc -> <:ctyp< $acc $t >>
        | -> fun t -> t
      ] ]
    type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i >> ] ]
    optional_type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i >>
        | "+"; "_" -> Ast.TyAnP _loc  
        | "-"; "_" -> Ast.TyAnM _loc  
        | "_" -> <:ctyp< _ >>  ] ]
    ctyp:
      [ "==" LA
        [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1 == $t2 >> ]
      | "private" NA
        [ "private"; t = ctyp Level "alias" -> <:ctyp< private $t >> ]
      | "alias" LA
        [ t1 = SELF; "as"; t2 = SELF ->
          <:ctyp< $t1 as $t2 >> ]
      | "forall" LA
        [ "!"; t1 = typevars; "."; t2 = ctyp ->
          <:ctyp< ! $t1 . $t2 >> ]
      | "arrow" RA
        [ t1 = SELF; "->"; t2 = SELF ->
          <:ctyp< $t1 -> $t2 >> ]
      | "label" NA
        [ "~"; i = a_LIDENT; ":"; t = SELF ->
          <:ctyp< ~ $i : $t >>
        | i = a_LABEL; t =  SELF  ->
          <:ctyp< ~ $i : $t >>
        | "?"; i = a_LIDENT; ":"; t = SELF ->
            <:ctyp< ? $i : $t >>
        | i = a_OPTLABEL; t = SELF ->
            <:ctyp< ? $i : $t >> ]
      | "apply" LA
        [ t1 = SELF; t2 = SELF ->
            let t = <:ctyp< $t1 $t2 >> in
            try <:ctyp< $(id:Ast.ident_of_ctyp t) >>
            with [ Invalid_argument _ -> t ] ]
      | "." LA
        [ t1 = SELF; "."; t2 = SELF ->
            try <:ctyp< $(id:Ast.ident_of_ctyp t1).$(id:Ast.ident_of_ctyp t2) >>
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
      | "simple"
        [ "'"; i = a_ident -> <:ctyp< '$i >>
        | "_" -> <:ctyp< _ >>
        | `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:ctyp< ($(tup:<:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>)) >>
        | `ANTIQUOT ("id" as n) s ->
            <:ctyp< $(id:<:ident< $(anti:mk_anti ~c:"ident" n s) >>) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | i = a_LIDENT -> <:ctyp< $lid:i >>
        | i = a_UIDENT -> <:ctyp< $uid:i >>
        | "("; t = SELF; "*"; tl = star_ctyp; ")" ->
            <:ctyp< ( $t * $tl ) >>
        | "("; t = SELF; ")" -> t
        | "["; "]" -> <:ctyp< [ ] >>
        | "["; t = constructor_declarations; "]" -> <:ctyp< [ $t ] >>
        | "["; "="; rfl = row_field; "]" ->
            <:ctyp< [ = $rfl ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $(<:ctyp<>>) ] >>
        | "["; ">"; rfl = row_field; "]" ->
            <:ctyp< [ > $rfl ] >>
        | "["; "<"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl ] >>
        | "["; "<"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl > $ntl ] >>
        | "[<"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl ] >>
        | "[<"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl > $ntl ] >>
        | "{"; t = label_declaration_list; "}" -> <:ctyp< { $t } >>
        | "#"; i = class_longident -> <:ctyp< # $i >>
        | "<"; t = opt_meth_list; ">" -> t
        | "("; "module"; p = package_type; ")" -> <:ctyp< (module $p) >>  ] ]
    star_ctyp:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp*" n s) >>
        | t1 = SELF; "*"; t2 = SELF ->
            <:ctyp< $t1 * $t2 >>
        | t = ctyp -> t  ] ]
    constructor_declarations:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp|" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | t1 = SELF; "|"; t2 = SELF ->
            <:ctyp< $t1 | $t2 >>
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s of $t >>
        | s = a_UIDENT; ":"; t = ctyp ->
            let (tl, rt) = Ctyp.to_generalized t in
            <:ctyp< $uid:s : ($(Ast.tyAnd_of_list tl) -> $rt) >>
        | s = a_UIDENT ->
	  <:ctyp< $uid:s >>  ] ]
    constructor_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s of $t >>
        | s = a_UIDENT ->
            <:ctyp< $uid:s >>
      ] ]
    constructor_arg_list:
      [ [ `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1 and $t2 >>
        | t = ctyp -> t
      ] ]
    label_declaration_list:
      [ [ t1 = label_declaration; ";"; t2 = SELF -> <:ctyp< $t1; $t2 >>
        | t1 = label_declaration; ";"            -> t1
        | t1 = label_declaration                 -> t1  ] ]
    label_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | s = a_LIDENT; ":"; t = poly_type ->
            <:ctyp< $lid:s : $t >>
        | s = a_LIDENT; ":"; "mutable"; t = poly_type ->
            <:ctyp< $lid:s : mutable $t >>  ] ]
    a_ident:
      [ [ i = a_LIDENT -> i
        | i = a_UIDENT -> i ] ]
    ident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s -> (* id it self does not support ANTIQUOT "lid", however [a_UIDENT] supports*)
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | i = a_LIDENT -> <:ident< $lid:i >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i.$j >> ] ]
    module_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_UIDENT -> <:ident< $uid:i >> ] ]
    module_longident_with_app:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i $j >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | "("; i = SELF; ")" -> i ] ]
    module_longident_dot_lparen:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; "(" ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_UIDENT; "."; "(" -> <:ident< $uid:i >> ] ]
    type_longident:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i $j >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_LIDENT -> <:ident< $lid:i >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | "("; i = SELF; ")" -> i ] ]
    label_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_LIDENT -> <:ident< $lid:i >> ] ]
    class_type_longident:
      [ [ x = type_longident -> x ] ]
    val_longident:
      [ [ x = ident -> x ] ]
    class_longident:
      [ [ x = label_longident -> x ] ]
    class_declaration:
      [ LA
        [ c1 = SELF; "and"; c2 = SELF ->
            <:class_expr< $c1 and $c2 >>
        | `ANTIQUOT (""|"cdcl"|"anti"|"list" as n) s ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
        | ci = class_info_for_class_expr; ce = class_fun_binding ->
            <:class_expr< $ci = $ce >> ] ]
    class_fun_binding:
      [ [ "="; ce = class_expr -> ce
        | ":"; ct = class_type_plus; "="; ce = class_expr ->
            <:class_expr< ($ce : $ct) >>
        | p = labeled_ipatt; cfb = SELF ->
            <:class_expr< fun $p -> $cfb >>  ] ]
    class_info_for_class_type:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_type< $virtual:mv $lid:i [ $ot ] >>  ] ]
    class_info_for_class_expr:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_expr< $virtual:mv $lid:i [ $ot ] >>  ] ]
    class_name_and_param:
      [ [ i = a_LIDENT; "["; x = comma_type_parameter; "]" -> (i, x)
        | i = a_LIDENT -> (i, <:ctyp<>>)
      ] ]
    comma_type_parameter:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | t = type_parameter -> t  ] ]
    opt_comma_ctyp:
      [ [ "["; x = comma_ctyp; "]" -> x
        | -> <:ctyp<>>  ] ]
    comma_ctyp:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | t = ctyp -> t  ] ]
    class_fun_def:
      [ [ p = labeled_ipatt; ce = SELF -> <:class_expr< fun $p -> $ce >>
        | "->"; ce = class_expr -> ce ] ]
    class_expr:
      [ "top"
        [ "fun"; p = labeled_ipatt; ce = class_fun_def ->
            <:class_expr< fun $p -> $ce >>
        | "let"; rf = opt_rec; bi = binding; "in"; ce = SELF ->
            <:class_expr< let $rec:rf $bi in $ce >> ]
      | "apply" NA
        [ ce = SELF; e = expr Level "label" ->
            <:class_expr< $ce $e >> ]
      | "simple"
        [ `ANTIQUOT (""|"cexp"|"anti" as n) s ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
        | ce = class_longident_and_param -> ce
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:class_expr< object ($csp) $cst end >>
        | "("; ce = SELF; ":"; ct = class_type; ")" ->
            <:class_expr< ($ce : $ct) >>
        | "("; ce = SELF; ")" -> ce ] ]
    class_longident_and_param:
      [ [ ci = class_longident; "["; t = comma_ctyp; "]" ->
          <:class_expr< $id:ci [ $t ] >>
        | ci = class_longident -> <:class_expr< $id:ci >>  ] ]
    class_structure:
      [ [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `ANTIQUOT (""|"cst"|"anti"|"list" as n) s; semi; cst = SELF ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s); $cst >>
        | l = LIST0 [ cst = class_str_item; semi -> cst ] -> Ast.crSem_of_list l  ] ]
    opt_class_self_patt:
      [ [ "("; p = patt; ")" -> p
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p : $t) >>
        | -> <:patt<>> ] ]
    class_str_item:
      [ LA
        [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_str_item_tag
        | "inherit"; o = opt_override; ce = class_expr; pb = opt_as_lident ->
            <:class_str_item< inherit $override:o $ce as $pb >>
        | o = value_val_opt_override; mf = opt_mutable; lab = label; e = cvalue_binding
          ->
            <:class_str_item< val $override:o $mutable:mf $lab = $e >>
        | o = value_val_opt_override; mf = opt_mutable; "virtual"; l = label; ":";
              t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< val virtual $mutable:mf $l : $t >>
        | o = value_val_opt_override; "virtual"; mf = opt_mutable; l = label; ":";
                t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< val virtual $mutable:mf $l : $t >>
        | o = method_opt_override; "virtual"; pf = opt_private; l = label; ":";
                t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf $l : $t >>
        | o = method_opt_override; pf = opt_private; l = label; topt = opt_polyt;
                e = fun_binding ->
            <:class_str_item< method $override:o $private:pf $l : $topt = $e >>
        | o = method_opt_override; pf = opt_private; "virtual"; l = label; ":";
             t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf $l : $t >>
        | type_constraint; t1 = ctyp; "="; t2 = ctyp ->
            <:class_str_item< type $t1 = $t2 >>
        | "initializer"; se = expr -> <:class_str_item< initializer $se >> ] ]
    method_opt_override:
      [ [ "method"; "!" -> <:override_flag< ! >>
        | "method"; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | "method" -> <:override_flag<>>  ] ]
    value_val_opt_override:
      [ [ "val"; "!" -> <:override_flag< ! >>
        | "val"; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | "val" -> <:override_flag<>>   ] ]
    opt_as_lident:
      [ [ "as"; i = a_LIDENT -> i
        | -> ""  ] ]
    opt_polyt:
      [ [ ":"; t = poly_type -> t
        | -> <:ctyp<>> ] ]
    cvalue_binding:
      [ [ "="; e = expr -> e
        | ":"; "type"; t1 = unquoted_typevars; "." ; t2 = ctyp ; "="; e = expr -> 
	(* let u = Ast.TyTypePol _loc t1 t2 in *)
         let u = <:ctyp< ! $t1 . $t2 >> in   
         <:expr< ($e : $u) >>
        | ":"; t = poly_type; "="; e = expr -> <:expr< ($e : $t) >>
        | ":"; t = poly_type; ":>"; t2 = ctyp; "="; e = expr ->
            match t with
            [ <:ctyp< ! $_ . $_ >> -> raise (Stream.Error "unexpected polytype here")
            | _ -> <:expr< ($e : $t :> $t2) >> ]
        | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e :> $t) >> ] ]
    label:
      [ [ i = a_LIDENT -> i ] ]
    class_type:
      [ [ `ANTIQUOT (""|"ctyp"|"anti" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | ct = class_type_longident_and_param -> ct
        | "object"; cst = opt_class_self_type; csg = class_signature; "end" ->
            <:class_type< object ($cst) $csg end >> ] ]
    class_type_longident_and_param:
      [ [ i = class_type_longident; "["; t = comma_ctyp; "]" ->
            <:class_type< $id:i [ $t ] >>
        | i = class_type_longident -> <:class_type< $id:i >> ] ]
    class_type_plus:
      [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
        <:class_type< [ $t ] -> $ct >>
        | ct = class_type -> ct ] ]
    opt_class_self_type:
      [ [ "("; t = ctyp; ")" -> t
        | -> <:ctyp<>> ] ]
    class_signature:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `ANTIQUOT (""|"csg"|"anti"|"list" as n) s; semi; csg = SELF ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s); $csg >>
        | l = LIST0 [ csg = class_sig_item; semi -> csg ] ->
            Ast.cgSem_of_list l  ] ]
    class_sig_item:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_sig_item_tag
        | "inherit"; cs = class_type ->
            <:class_sig_item< inherit $cs >>
        | "val"; mf = opt_mutable; mv = opt_virtual;
          l = label; ":"; t = ctyp ->
            <:class_sig_item< val $mutable:mf $virtual:mv $l : $t >>
        | "method"; "virtual"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | "method"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method $private:pf $l : $t >>
        | "method"; pf = opt_private; "virtual"; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | type_constraint; t1 = ctyp; "="; t2 = ctyp ->
            <:class_sig_item< type $t1 = $t2 >> ] ]
    type_constraint:
      [ [ "type" | "constraint" -> () ] ]
    class_description:
      [ [ cd1 = SELF; "and"; cd2 = SELF ->
            <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.class_type_tag
        | ci = class_info_for_class_type; ":"; ct = class_type_plus ->
            <:class_type< $ci : $ct >>  ] ]
    class_type_declaration:
      [ LA
        [ cd1 = SELF; "and"; cd2 = SELF ->
          <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | ci = class_info_for_class_type; "="; ct = class_type ->
            <:class_type< $ci = $ct >> ] ]
    field_expr_list:
      [ [ b1 = field_expr; ";"; b2 = SELF -> <:rec_binding< $b1 ; $b2 >>
        | b1 = field_expr; ";"            -> b1
        | b1 = field_expr                 -> b1
      ] ]
    field_expr:
      [ [ `ANTIQUOT (""|"bi"|"anti" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | l = label; "="; e = expr Level "top" ->
            <:rec_binding< $lid:l = $e >> ] ]
    meth_list:
      [ [ m = meth_decl; ";"; (ml, v) = SELF  -> (<:ctyp< $m; $ml >>, v)
        | m = meth_decl; ";"; v = opt_dot_dot -> (m, v)
        | m = meth_decl; v = opt_dot_dot      -> (m, v)
      ] ]
    meth_decl:
      [ [ `ANTIQUOT (""|"typ" as n) s        -> <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s          -> <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x                       -> Quotation.expand _loc x DynAst.ctyp_tag
        | lab = a_LIDENT; ":"; t = poly_type -> <:ctyp< $lid:lab : $t >> ] ]
    opt_meth_list:
      [ [ (ml, v) = meth_list -> <:ctyp< < $ml $(..:v) > >>
        | v = opt_dot_dot     -> <:ctyp< < $(..:v) > >>
      ] ]
    poly_type:
      [ [ t = ctyp -> t ] ]
    package_type:
      [ [ p = module_type -> p ] ]
    typevars:
      [ LA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >> ] ]
    unquoted_typevars:
      [ LA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | i = a_ident -> <:ctyp< $lid:i >>
      ] ]
    row_field:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp|" n s) >>
        | t1 = SELF; "|"; t2 = SELF -> <:ctyp< $t1 | $t2 >>
        | "`"; i = a_ident -> <:ctyp< `$i >>
        | "`"; i = a_ident; "of"; "&"; t = amp_ctyp -> <:ctyp< `$i of & $t >>
        | "`"; i = a_ident; "of"; t = amp_ctyp -> <:ctyp< `$i of $t >>
        | t = ctyp -> t ] ]
    amp_ctyp:
      [ [ t1 = SELF; "&"; t2 = SELF -> <:ctyp< $t1 & $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp&" n s) >>
        | t = ctyp -> t
      ] ]
    name_tags:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | "`"; i = a_ident -> <:ctyp< `$i >>  ] ]
    eq_expr:
      [ [ "="; e = expr -> fun i p -> <:patt< ? $i : ($p = $e) >>
        | -> fun i p -> <:patt< ? $i : ($p) >> ] ]
    patt_tcon:
      [ [ p = patt; ":"; t = ctyp -> <:patt< ($p : $t) >>
        | p = patt -> p ] ]
    ipatt:
      [ [ `LABEL i; p = SELF -> <:patt< ~ $i : $p >>
        | "~"; `ANTIQUOT (""|"lid" as n) i; ":"; p = SELF ->
            <:patt< ~ $(mk_anti n i) : $p >>
        | "~"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ~ $(mk_anti n i) >>
        | "~"; `LIDENT i -> <:patt< ~ $i >>
        (* | i = opt_label; "("; p = ipatt_tcon; ")" ->
            <:patt< ? $i$ : ($p$) >>
        | i = opt_label; "("; p = ipatt_tcon; "="; e = expr; ")" ->
            <:patt< ? $i$ : ($p$ = $e$) >>                             *)
        | `OPTLABEL i; "("; p = ipatt_tcon; f = eq_expr; ")" -> f i p
        | "?"; `ANTIQUOT (""|"lid" as n) i; ":"; "("; p = ipatt_tcon;
          f = eq_expr; ")" -> f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i >>
        | "?"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ? $(mk_anti n i) >>
        | "?"; "("; p = ipatt_tcon; ")" ->
            <:patt< ? ($p) >>
        | "?"; "("; p = ipatt_tcon; "="; e = expr; ")" ->
            <:patt< ? ($p = $e) >> ] ]
    ipatt_tcon:
      [ [ p = ipatt; ":"; t = ctyp -> <:patt< ($p : $t) >>
        | p = ipatt -> p ] ]
    direction_flag:
      [ [ "to" -> <:direction_flag< to >>
        | "downto" -> <:direction_flag< downto >>
        | `ANTIQUOT ("to"|"anti" as n) s -> Ast.DiAnt (mk_anti n s) ] ]
    opt_private:
      [ [ "private" -> <:private_flag< private >>
        | `ANTIQUOT ("private"|"anti" as n) s -> Ast.PrAnt (mk_anti n s)
        | -> <:private_flag<>>  ] ]
    opt_mutable:
      [ [ "mutable" -> <:mutable_flag< mutable >>
        | `ANTIQUOT ("mutable"|"anti" as n) s -> Ast.MuAnt (mk_anti n s)
        | -> <:mutable_flag<>>  ] ]
    opt_virtual:
      [ [ "virtual" -> <:virtual_flag< virtual >>
        | `ANTIQUOT ("virtual"|"anti" as n) s -> Ast.ViAnt (mk_anti n s)
        | -> <:virtual_flag<>>  ] ]
    opt_dot_dot:
      [ [ ".." -> <:row_var_flag< .. >>
        | `ANTIQUOT (".."|"anti" as n) s -> Ast.RvAnt (mk_anti n s)
        | -> <:row_var_flag<>>  ] ]
    opt_rec:
      [ [ "rec" -> <:rec_flag< rec >>
        | `ANTIQUOT ("rec"|"anti" as n) s -> Ast.ReAnt (mk_anti n s)
        | -> <:rec_flag<>> ] ]
    opt_override:
      [ [ "!" -> <:override_flag< ! >>
        | `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | -> <:override_flag<>> ] ]
    opt_expr:
      [ [ e = expr -> e
        | -> <:expr<>> ] ]
    interf:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:sig_item< # $n $dp >> ], stopped_at _loc)
          (* Ast.SgDir(_loc,n,dp), stopped is of type FanLoc.t option *)
        | si = sig_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    sig_items:
      [ [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s) >>
        | `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s; semi; sg = SELF ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s); $sg >> 
        | l = LIST0 [ sg = sig_item; semi -> sg ] -> Ast.sgSem_of_list l  ] ]
    implem:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    str_items:
      [ [ `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s) >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s; semi; st = SELF ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s); $st >>
        | l = LIST0 [ st = str_item; semi -> st ] -> Ast.stSem_of_list l  ] ]
    top_phrase:
      [ [ ph = phrase -> Some ph
        | `EOI -> None ] ]
    use_file:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    phrase:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; ";;" -> (* directive to be the same as normal syntax*)
            <:str_item< # $n $dp >>
        | st = str_item; semi -> st  ] ]
    a_INT:
      [ [ `ANTIQUOT (""|"int"|"`int" as n) s -> mk_anti n s
        | `INT _ s -> s ] ]
    a_INT32:
      [ [ `ANTIQUOT (""|"int32"|"`int32" as n) s -> mk_anti n s
        | `INT32 _ s -> s ] ]
    a_INT64:
      [ [ `ANTIQUOT (""|"int64"|"`int64" as n) s -> mk_anti n s
        | `INT64 _ s -> s ] ]
    a_NATIVEINT:
      [ [ `ANTIQUOT (""|"nativeint"|"`nativeint" as n) s -> mk_anti n s
        | `NATIVEINT _ s -> s ] ]
    a_FLOAT:
      [ [ `ANTIQUOT (""|"flo"|"`flo" as n) s -> mk_anti n s
        | `FLOAT _ s -> s ] ]
    a_CHAR:
      [ [ `ANTIQUOT (""|"chr"|"`chr" as n) s -> mk_anti n s
        | `CHAR _ s -> s ] ]
    a_UIDENT:
      [ [ `ANTIQUOT (""|"uid" as n) s -> mk_anti n s
        | `UIDENT s -> s ] ]
    a_LIDENT:
      [ [ `ANTIQUOT (""|"lid" as n) s -> mk_anti n s
        | `LIDENT s -> s ] ]
    a_LABEL:
      [ [ "~"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `LABEL s -> s ] ]
    a_OPTLABEL:
      [ [ "?"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `OPTLABEL s -> s ] ]
    a_STRING:
      [ [ `ANTIQUOT (""|"str"|"`str" as n) s -> mk_anti n s
        | `STRING _ s -> s ] ]
    string_list:
      [ [ `ANTIQUOT (""|"str_list") s -> Ast.LAnt (mk_anti "str_list" s)
        | `STRING _ x; xs = string_list -> Ast.LCons x xs
        | `STRING _ x -> Ast.LCons x Ast.LNil ] ]
    semi:
      [ [ ";" -> () ] ] 
    expr_quot:
      [ [ e1 = expr; ","; e2 = comma_expr -> <:expr< $e1, $e2 >>
        | e1 = expr; ";"; e2 = sem_expr -> <:expr< $e1; $e2 >>
        | e = expr -> e
        | -> <:expr<>> ] ]
    patt_quot:
      [ [ x = patt; ","; y = comma_patt -> <:patt< $x, $y >>
        | x = patt; ";"; y = sem_patt -> <:patt< $x; $y >>
        | x = patt; "="; y = patt ->
            let i =
              match x with
              [ <:patt@loc< $anti:s >> -> <:ident@loc< $anti:s >>
              | p -> Ast.ident_of_patt p ]
            in
            <:patt< $i = $y >>
        | x = patt -> x
        | -> <:patt<>>
      ] ]
    ctyp_quot:
      [ [ x = more_ctyp; ","; y = comma_ctyp -> <:ctyp< $x, $y >>
        | x = more_ctyp; ";"; y = label_declaration_list -> <:ctyp< $x; $y >>
        | x = more_ctyp; "|"; y = constructor_declarations -> <:ctyp< $x | $y >>
        | x = more_ctyp; "of"; y = constructor_arg_list -> <:ctyp< $x of $y >>
        | x = more_ctyp; "of"; y = constructor_arg_list; "|"; z = constructor_declarations ->
            <:ctyp< $(<:ctyp< $x of $y >> ) | $z >>
        | x = more_ctyp; "of"; "&"; y = amp_ctyp -> <:ctyp< $x of & $y >>
        | x = more_ctyp; "of"; "&"; y = amp_ctyp; "|"; z = row_field ->
            <:ctyp< $(<:ctyp< $x of & $y >> ) | $z >>
        | x = more_ctyp; ":"; y = more_ctyp -> <:ctyp< $x : $y >>
        | x = more_ctyp; ":"; y = more_ctyp; ";"; z = label_declaration_list ->
            <:ctyp< $(<:ctyp< $x : $y >> ) ; $z >>
        | x = more_ctyp; "*"; y = star_ctyp -> <:ctyp< $x * $y >>
        | x = more_ctyp; "&"; y = amp_ctyp -> <:ctyp< $x & $y >>
        | x = more_ctyp; "and"; y = constructor_arg_list -> <:ctyp< $x and $y >>
        | x = more_ctyp -> x
        | -> <:ctyp<>>  ] ]
    more_ctyp:
      [ [ "mutable"; x = SELF -> <:ctyp< mutable $x >>
        | "`"; x = a_ident -> <:ctyp< `$x >>
        | x = ctyp -> x
        | x = type_parameter -> x
      ] ]
    str_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:str_item< # $n $dp >>
        | st1 = str_item; semi; st2 = SELF ->
            match st2 with
            [ <:str_item<>> -> st1
            | _ -> <:str_item< $st1; $st2 >> ]
        | st = str_item -> st
        | -> <:str_item<>> ] ]
    sig_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:sig_item< # $n $dp >>
        | sg1 = sig_item; semi; sg2 = SELF ->
            match sg2 with
            [ <:sig_item<>> -> sg1
            | _ -> <:sig_item< $sg1; $sg2 >> ]
        | sg = sig_item -> sg
        | -> <:sig_item<>> ] ]
    module_type_quot:
      [ [ x = module_type -> x
        | -> <:module_type<>> ] ]
    module_expr_quot:
      [ [ x = module_expr -> x
        | -> <:module_expr<>> ] ]
    match_case_quot:
      [ [ x = LIST0 match_case0 SEP "|" -> <:match_case< $list:x >>
        | -> <:match_case<>> ] ]
    binding_quot:
      [ [ x = binding -> x
        | -> <:binding<>> ] ]
    rec_binding_quot:
      [ [ x = label_expr_list -> x
        | -> <:rec_binding<>> ] ]
    module_binding_quot:
      [ [ b1 = SELF; "and"; b2 = SELF ->
            <:module_binding< $b1 and $b2 >>
        | `ANTIQUOT ("module_binding"|"anti" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type ->
            <:module_binding< $(mk_anti n m) : $mt >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $(mk_anti n m) : $mt = $me >>
        | m = a_UIDENT; ":"; mt = module_type ->
            <:module_binding< $m : $mt >>
        | m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $m : $mt = $me >>
        | -> <:module_binding<>> ] ]
    ident_quot:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i $j >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | i = a_LIDENT -> <:ident< $lid:i >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | "("; i = SELF; ")" -> i  ] ]
    class_expr_quot:
      [ [ ce1 = SELF; "and"; ce2 = SELF -> <:class_expr< $ce1 and $ce2 >>
        | ce1 = SELF; "="; ce2 = SELF -> <:class_expr< $ce1 = $ce2 >>
        | "virtual"; (i, ot) = class_name_and_param ->
            <:class_expr< virtual $lid:i [ $ot ] >>
        | `ANTIQUOT ("virtual" as n) s; i = ident; ot = opt_comma_ctyp ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
            <:class_expr< $virtual:anti $id:i [ $ot ] >>
        | x = class_expr -> x
        | -> <:class_expr<>> ] ]
    class_type_quot:
      [ [ ct1 = SELF; "and"; ct2 = SELF -> <:class_type< $ct1 and $ct2 >>
        | ct1 = SELF; "="; ct2 = SELF -> <:class_type< $ct1 = $ct2 >>
        | ct1 = SELF; ":"; ct2 = SELF -> <:class_type< $ct1 : $ct2 >>
        | "virtual"; (i, ot) = class_name_and_param ->
            <:class_type< virtual $lid:i [ $ot ] >>
        | `ANTIQUOT ("virtual" as n) s; i = ident; ot = opt_comma_ctyp ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
            <:class_type< $virtual:anti $id:i [ $ot ] >>
        | x = class_type_plus -> x
        | -> <:class_type<>>   ] ]
    class_str_item_quot:
      [ [ x1 = class_str_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_str_item<>> -> x1
          | _ -> <:class_str_item< $x1; $x2 >> ]
        | x = class_str_item -> x
        | -> <:class_str_item<>> ] ]
    class_sig_item_quot:
      [ [ x1 = class_sig_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_sig_item<>> -> x1
          | _ -> <:class_sig_item< $x1; $x2 >> ]
        | x = class_sig_item -> x
        | -> <:class_sig_item<>> ] ]
    with_constr_quot:
      [ [ x = with_constr -> x
        | -> <:with_constr<>> ] ]
    rec_flag_quot: [ [ x = opt_rec -> x ] ]
    direction_flag_quot: [ [ x = direction_flag -> x ] ]
    mutable_flag_quot: [ [ x = opt_mutable -> x ] ]
    private_flag_quot: [ [ x = opt_private -> x ] ]
    virtual_flag_quot: [ [ x = opt_virtual -> x ] ]
    row_var_flag_quot: [ [ x = opt_dot_dot -> x ] ]
    override_flag_quot: [ [ x = opt_override -> x ] ]
    patt_eoi:
      [ [ x = patt; `EOI -> x ] ]
    expr_eoi:
      [ [ x = expr; `EOI -> x ] ]
  END;

end;

module IdRevisedParserParser : Sig.Id = struct
  let name = "Camlp4OCamlRevisedParserParser";
  let version = Sys.ocaml_version;
end;

module MakeRevisedParserParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  module Ast = Camlp4Ast;
  type spat_comp =
    [ SpTrm of FanLoc.t and Ast.patt and option Ast.expr
    | SpNtr of FanLoc.t and Ast.patt and Ast.expr
    | SpStr of FanLoc.t and Ast.patt ]
  ;
  type sexp_comp =
    [ SeTrm of FanLoc.t and Ast.expr | SeNtr of FanLoc.t and Ast.expr ]
  ;

  let stream_expr = Gram.mk "stream_expr";
  let stream_begin = Gram.mk "stream_begin";
  let stream_end = Gram.mk "stream_end";
  let stream_quot = Gram.mk "stream_quot";
  let parser_case = Gram.mk "parser_case";
  let parser_case_list = Gram.mk "parser_case_list";

  let strm_n = "__strm";
  let peek_fun _loc = <:expr< Stream.peek >>;
  let junk_fun _loc = <:expr< Stream.junk >>;

  let rec pattern_eq_expression p e =  match (p, e) with
    [ (<:patt< $lid:a >>, <:expr< $lid:b >>) -> a = b
    | (<:patt< $uid:a >>, <:expr< $uid:b >>) -> a = b
    | (<:patt< $p1 $p2 >>, <:expr< $e1 $e2 >>) ->
        pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
    | _ -> False ] ;

  let is_raise e =
    match e with
    [ <:expr< raise $_ >> -> True
    | _ -> False ] ;

  let is_raise_failure e =
    match e with
    [ <:expr< raise Stream.Failure >> -> True
    | _ -> False ] ;

  let rec handle_failure e =  match e with
    [ <:expr< try $_ with [ Stream.Failure -> $e] >> ->
        handle_failure e
    | <:expr< match $me with [ $a ] >> ->
        let rec match_case_handle_failure = fun
          [ <:match_case< $a1 | $a2 >> ->
              match_case_handle_failure a1 && match_case_handle_failure a2
          | <:match_case< $pat:_ -> $e >> -> handle_failure e
          | _ -> False ]
        in handle_failure me && match_case_handle_failure a
    | <:expr< let $bi in $e >> ->
        let rec binding_handle_failure =
          fun
          [ <:binding< $b1 and $b2 >> ->
              binding_handle_failure b1 && binding_handle_failure b2
          | <:binding< $_ = $e >> -> handle_failure e
          | _ -> False ]
        in binding_handle_failure bi && handle_failure e
    | <:expr< $lid:_ >> | <:expr< $int:_ >> | <:expr< $str:_ >> |
      <:expr< $chr:_ >> | <:expr< fun [ $_ ] >> | <:expr< $uid:_ >> ->
        True
    | <:expr< raise $e >> ->
        match e with
        [ <:expr< Stream.Failure >> -> False
        | _ -> True ]
    | <:expr< $f $x >> ->
        is_constr_apply f && handle_failure f && handle_failure x
    | _ -> False ]
  and is_constr_apply =
    fun
    [ <:expr< $uid:_ >> -> True
    | <:expr< $lid:_ >> -> False
    | <:expr< $x $_ >> -> is_constr_apply x
    | _ -> False ];

  let rec subst v e =
    let _loc = Ast.loc_of_expr e in
    match e with
    [ <:expr< $lid:x >> ->
        let x = if x = v then strm_n else x in
        <:expr< $lid:x >>
    | <:expr< $uid:_ >> -> e
    | <:expr< $int:_ >> -> e
    | <:expr< $chr:_ >> -> e
    | <:expr< $str:_ >> -> e
    | <:expr< $_ . $_ >> -> e
    | <:expr< let $rec:rf $bi in $e >> ->
        <:expr< let $rec:rf $(subst_binding v bi) in $(subst v e) >>
    | <:expr< $e1 $e2 >> -> <:expr< $(subst v e1) $(subst v e2) >>
    | <:expr< ( $tup:e ) >> -> <:expr< ( $(tup:subst v e) ) >>
    | <:expr< $e1, $e2 >> -> <:expr< $(subst v e1), $(subst v e2) >>
    | _ -> raise Not_found ]
  and subst_binding v =
    fun
    [ <:binding@_loc< $b1 and $b2 >> ->
        <:binding< $(subst_binding v b1) and $(subst_binding v b2) >>
    | <:binding@_loc< $lid:v' = $e >> ->
        <:binding< $lid:v' = $(if v = v' then e else subst v e) >>
    | _ -> raise Not_found ];

  let stream_pattern_component skont ckont =
    fun
    [ SpTrm _loc p None ->
        <:expr< match $(peek_fun _loc) $lid:strm_n with
                [ Some $p ->
                    do { $(junk_fun _loc) $lid:strm_n; $skont }
                | _ -> $ckont ] >>
    | SpTrm _loc p (Some w) ->
        <:expr< match $(peek_fun _loc) $lid:strm_n with
                [ Some $p when $w ->
                    do { $(junk_fun _loc) $lid:strm_n; $skont }
                | _ -> $ckont ] >>
    | SpNtr _loc p e ->
        let e =
          match e with
          [ <:expr< fun [ ($lid:v : Stream.t _) -> $e ] >> when v = strm_n -> e
          | _ -> <:expr< $e $lid:strm_n >> ]
        in
        if pattern_eq_expression p skont then
          if is_raise_failure ckont then e
          else if handle_failure e then e
          else <:expr< try $e with [ Stream.Failure -> $ckont ] >>
        else if is_raise_failure ckont then
          <:expr< let $p = $e in $skont >>
        else if pattern_eq_expression <:patt< Some $p >> skont then
          <:expr< try Some $e with [ Stream.Failure -> $ckont ] >>
        else if is_raise ckont then
          let tst =
            if handle_failure e then e
            else <:expr< try $e with [ Stream.Failure -> $ckont ] >>
          in
          <:expr< let $p = $tst in $skont >>
        else
          <:expr< match try Some $e with [ Stream.Failure -> None ] with
                  [ Some $p -> $skont
                  | _ -> $ckont ] >>
    | SpStr _loc p ->
        try
          match p with
          [ <:patt< $lid:v >> -> subst v skont
          | _ -> raise Not_found ]
        with
        [ Not_found -> <:expr< let $p = $lid:strm_n in $skont >> ] ];

  let rec stream_pattern _loc epo e ekont = fun
    [ [] ->
        match epo with
        [ Some ep -> <:expr< let $ep = Stream.count $lid:strm_n in $e >>
        | _ -> e ]
    | [(spc, err) :: spcl] ->
        let skont =
          let ekont err =
            let str = match err with
              [ Some estr -> estr
              | _ -> <:expr< "" >> ] in
            <:expr< raise (Stream.Error $str) >>
          in
          stream_pattern _loc epo e ekont spcl
        in
        let ckont = ekont err in stream_pattern_component skont ckont spc ];

  let stream_patterns_term _loc ekont tspel =
    let pel =
      List.fold_right
        (fun (p, w, _loc, spcl, epo, e) acc ->
          let p = <:patt< Some $p >> in
          let e =
            let ekont err =
              let str =
                match err with
                [ Some estr -> estr
                | _ -> <:expr< "" >> ]
              in
              <:expr< raise (Stream.Error $str) >>
            in
            let skont = stream_pattern _loc epo e ekont spcl in
            <:expr< do { $(junk_fun _loc) $lid:strm_n; $skont } >>
          in
          match w with
          [ Some w -> <:match_case< $pat:p when $w -> $e | $acc >>
          | None -> <:match_case< $pat:p -> $e | $acc >> ])
        tspel <:match_case<>>
    in
    <:expr< match $(peek_fun _loc) $lid:strm_n with [ $pel | _ -> $(ekont ()) ] >>
  ;

  let rec group_terms =
    fun
    [ [([(SpTrm _loc p w, None) :: spcl], epo, e) :: spel] ->
        let (tspel, spel) = group_terms spel in
        ([(p, w, _loc, spcl, epo, e) :: tspel], spel)
    | spel -> ([], spel) ]
  ;

  let rec parser_cases _loc = fun
    [ [] -> <:expr< raise Stream.Failure >>
    | spel ->
        match group_terms spel with
        [ ([], [(spcl, epo, e) :: spel]) ->
            stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl
        | (tspel, spel) ->
            stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel ] ];

  let cparser _loc bpo pc =
    let e = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $e >>
      | None -> e ]
    in
    let p = <:patt< ($lid:strm_n : Stream.t _) >> in
    <:expr< fun $p -> $e >> ;

  let cparser_match _loc me bpo pc =
    let pc = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $pc >>
      | None -> pc ]  in
    let me =   match me with
      [ <:expr@_loc< $_; $_ >> as e -> <:expr< do { $e } >>
      | e -> e ] in
    match me with
    [ <:expr< $lid:x >> when x = strm_n -> e
    | _ -> <:expr< let ($lid:strm_n : Stream.t _) = $me in $e >> ] ;

  (* streams *)

  let rec not_computing =
    fun
    [ <:expr< $lid:_ >> | <:expr< $uid:_ >> | <:expr< $int:_ >> |
      <:expr< $flo:_ >> | <:expr< $chr:_ >> | <:expr< $str:_ >> -> True
    | <:expr< $x $y >> -> is_cons_apply_not_computing x && not_computing y
    | _ -> False ]
  and is_cons_apply_not_computing =
    fun
    [ <:expr< $uid:_ >> -> True
    | <:expr< $lid:_ >> -> False
    | <:expr< $x $y >> -> is_cons_apply_not_computing x && not_computing y
    | _ -> False ];

  let slazy _loc e =
    match e with
    [ <:expr< $f () >> ->
        match f with
        [ <:expr< $lid:_ >> -> f
        | _ -> <:expr< fun _ -> $e >> ]
    | _ -> <:expr< fun _ -> $e >> ] ;

  let rec cstream gloc = 
    fun
    [ [] -> let _loc = gloc in <:expr< [< >] >>
    | [SeTrm _loc e] ->
        if not_computing e then <:expr< Stream.ising $e >>
        else <:expr< Stream.lsing $(slazy _loc e) >>
    | [SeTrm _loc e :: secl] ->
        if not_computing e then <:expr< Stream.icons $e $(cstream gloc secl) >>
        else <:expr< Stream.lcons $(slazy _loc e) $(cstream gloc secl) >>
    | [SeNtr _loc e] ->
        if not_computing e then e else <:expr< Stream.slazy $(slazy _loc e) >>
    | [SeNtr _loc e :: secl] ->
        if not_computing e then <:expr< Stream.iapp $e $(cstream gloc secl) >>
        else <:expr< Stream.lapp $(slazy _loc e) $(cstream gloc secl) >> ] ;
  (* Syntax extensions in Revised Syntax grammar *)

  EXTEND Gram
    GLOBAL: expr stream_expr stream_begin stream_end stream_quot
      parser_case parser_case_list;
    expr: Level "top"
      [ [ "parser"; po = OPT parser_ipatt; pcl = parser_case_list ->
            cparser _loc po pcl
        | "match"; e = sequence; "with"; "parser"; po = OPT parser_ipatt;
          pcl = parser_case_list ->
            cparser_match _loc e po pcl ] ]
    parser_ipatt:
      [ [ i = a_LIDENT -> <:patt< $lid:i >>  | "_" -> <:patt< _ >>  ] ]        
    parser_case_list:
      [ [ "["; pcl = LIST0 parser_case SEP "|"; "]" -> pcl
        | pc = parser_case -> [pc] ] ]
    parser_case:
      [ [ stream_begin; sp = stream_patt; stream_end; po = OPT parser_ipatt; "->"; e = expr
          ->   (sp, po, e) ] ]
    stream_begin: [ [ "[<" -> () ] ] stream_end: [ [ ">]" -> () ] ]
    stream_quot:  [ [ "'" -> () ] ]
    stream_expr:  [ [ e = expr -> e ] ]
    stream_patt:
      [ [ spc = stream_patt_comp -> [(spc, None)]
        | spc = stream_patt_comp; ";"; sp = stream_patt_comp_err_list
          ->    [(spc, None) :: sp]
        | -> [] ] ]
    (* stream_patt_comp: (\* FIXME here *\) *)
    (*   [ [ stream_quot; p = patt; eo = OPT [ "when"; e = stream_expr -> e ] *)
    (*       ->  SpTrm _loc p eo *)
    (*     | p = patt; "="; e = stream_expr -> SpNtr _loc p e *)
    (*     | p = patt -> SpStr _loc p ] ] *)
    stream_patt_comp: (* FIXME here *)
      [ [ p = patt; eo = OPT [ "when"; e = stream_expr -> e ]
          ->  SpTrm _loc p eo
        | p = patt; "="; e = stream_expr -> SpNtr _loc p e
        | stream_quot; p = patt -> SpStr _loc p ] ]
        
    stream_patt_comp_err:
      [ [ spc = stream_patt_comp; eo = OPT [ "??"; e = stream_expr -> e ]
          ->  (spc, eo) ] ]
    stream_patt_comp_err_list:
      [ [ spc = stream_patt_comp_err -> [spc]
        | spc = stream_patt_comp_err; ";" -> [spc]
        | spc = stream_patt_comp_err; ";"; sp = stream_patt_comp_err_list ->
            [spc :: sp] ] ]
    expr: Level "simple"
      [ [ stream_begin; stream_end -> <:expr< [< >] >>
        | stream_begin; sel = stream_expr_comp_list; stream_end
          ->  cstream _loc sel] ]
    stream_expr_comp_list:
      [ [ se = stream_expr_comp; ";"; sel = stream_expr_comp_list -> [se :: sel]
        | se = stream_expr_comp; ";" -> [se]
        | se = stream_expr_comp -> [se] ] ]
    (* stream_expr_comp: (\* FIXME *\) *)
    (*   [ [ stream_quot; e = stream_expr -> SeTrm _loc e *)
    (*     | e = stream_expr -> SeNtr _loc e ] ] *)
    stream_expr_comp: (* FIXME *)
      [ [  e = stream_expr -> SeTrm _loc e
        | stream_quot;e = stream_expr -> SeNtr _loc e ] ]
        
  END;

end;
  
module IdQuotationCommon = struct (* FIXME unused here *)
  let name = "Camlp4QuotationCommon";
  let version = Sys.ocaml_version;
end;

module MakeQuotationCommon (Syntax : Sig.Camlp4Syntax)
            (TheAntiquotSyntax : Sig.ParserExpr)
= struct
  open FanSig;
  include Syntax; (* Be careful an AntiquotSyntax module appears here *)
  module Ast = Camlp4Ast;
  module MetaAst = Ast.Meta.Make Lib.Meta.MetaLocQuotation;
  module ME = MetaAst.Expr;
  module MP = MetaAst.Patt;

  let anti_obj = Expr.antiquot_expander
      ~parse_expr:TheAntiquotSyntax.parse_expr
      ~parse_patt:TheAntiquotSyntax.parse_patt;
  let add_quotation name entry mexpr mpatt =
    let entry_eoi = Gram.mk (Gram.name entry) in
    let parse_quot_string entry loc s =
      let q = !FanConfig.antiquotations in
      let () = FanConfig.antiquotations := True in
      let res = Gram.parse_string entry loc s in
      let () = FanConfig.antiquotations := q in
      res in
    let expand_expr loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi loc s in
      let () = Lib.Meta.MetaLocQuotation.loc_name := loc_name_opt in
      let meta_ast = mexpr loc ast in
      let exp_ast = anti_obj#expr meta_ast in
      exp_ast in
    let expand_str_item loc loc_name_opt s =
      let exp_ast = expand_expr loc loc_name_opt s in
      <:str_item@loc< $(exp:exp_ast) >> in
    let expand_patt _loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi _loc s in
      let meta_ast = mpatt _loc ast in
      let exp_ast = anti_obj#patt meta_ast in
      match loc_name_opt with
      [ None -> exp_ast
      | Some name ->
        let rec subst_first_loc =  fun
          [ <:patt@_loc< Ast.$uid:u $_ >> -> <:patt< Ast.$uid:u $lid:name >>
          | <:patt@_loc< $a $b >> -> <:patt< $(subst_first_loc a) $b >>
          | p -> p ] in
        subst_first_loc exp_ast ] in begin 
          EXTEND Gram
            entry_eoi:
            [ [ x = entry; `EOI -> x ] ]
            END;
          Quotation.add name DynAst.expr_tag expand_expr;
          Quotation.add name DynAst.patt_tag expand_patt;
          Quotation.add name DynAst.str_item_tag expand_str_item;
        end;
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

end;



module IdQuotationExpander = struct
  let name = "Camlp4QuotationExpander";
  let version = Sys.ocaml_version;
end;

module MakeQuotationExpander (Syntax : Sig.Camlp4Syntax)
= struct
  module M = MakeQuotationCommon Syntax Syntax.AntiquotSyntax;
  include M;
end;

(* let pa_r  = "Camlp4OCamlRevisedParser"; *)    
let pa_r (module P:Sig.PRECAST) =
  P.syntax_extension (module IdRevisedParser)  (module MakeRevisedParser);

(* let pa_rp = "Camlp4OCamlRevisedParserParser"; *)
let pa_rp (module P:Sig.PRECAST) =
  P.syntax_extension (module IdRevisedParserParser)
    (module MakeRevisedParserParser);


let pa_g (module P:Sig.PRECAST) =
  P.syntax_extension (module IdGrammarParser) (module MakeGrammarParser);

(* let pa_m  = "Camlp4MacroParser"; *)
let pa_m (module P:Sig.PRECAST) =
  let () = P.syntax_extension (module IdMacroParser) (module MakeMacroParser) in
  P.syntax_plugin (module IdMacroParser) (module MakeNothing);

(* let pa_q  = "Camlp4QuotationExpander"; *)
let pa_q (module P:Sig.PRECAST) =
  P.syntax_extension (module IdQuotationExpander) (module MakeQuotationExpander);
  
(* let pa_rq = "Camlp4OCamlRevisedQuotationExpander"; *)
(*   unreflective*, quotation syntax use revised syntax. *)

let pa_rq (module P:Sig.PRECAST) =
  let module Gram = Grammar.Static.Make P.Lexer in
  let module M1 = OCamlInitSyntax.Make P.Gram in
  let module M2 = MakeRevisedParser M1 in
  let module M3 = MakeQuotationCommon M2 P.Syntax.AntiquotSyntax in ();

let pa_l  (module P: Sig.PRECAST) =
  P.syntax_extension (module IdListComprehension) (module MakeListComprehension);


(* load debug parser for bootstrapping *)
let pa_debug (module P: Sig.PRECAST) =
  P.syntax_extension (module IdDebugParser) (module MakeDebugParser);




