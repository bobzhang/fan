open LibUtil;
open FanUtil;
open Lib;

module IdDebugParser = struct
  let name = "Camlp4DebugParser";
  let version = Sys.ocaml_version;
end;

module MakeDebugParser (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  (* open FanSig ; (\* For FanToken, probably we should fix FanToken as well  *\) *)
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
    [ [ start_debug{m}; `LIDENT section; `STRING (_, fmt); (* FIXME move to `STRING(,_)*)
        LIST0 expr Level "."{args}; end_or_in{x} ->
      match (x, debug_mode section) with
      [ (None,   False) -> <:expr< () >>
      | (Some e, False) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> <:expr< let () = $(mk_debug _loc m fmt section args) in $e >> ]  ] ]
    end_or_in:
    [ [ "end" -> None
      | "in"; expr{e} -> Some e  ] ]
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
  (* open FanSig; *)
  open FanGrammar;
  open FanGrammarTools;
  FanConfig.antiquotations := True;
  EXTEND Gram GLOBAL: expr symbol rule rule_list psymbol level level_list;
    expr: After "top"
      [ [ "EXTEND"; extend_body{e}; "END" -> e
        | "DELETE_RULE"; delete_rule_body{e}; "END" -> e ] ] 
    extend_header:
      [ [ "("; qualid{i}; ":"; t_qualid{t}; ")" -> 
        let old=gm() in 
        let () = grammar_module_name := t in
        (Some i,old)
        | qualuid{t} -> begin
            let old = gm() in
            let () = grammar_module_name := t in 
            (None,old)
        end
        | -> (None,gm()) (* FIXME *)   ] ]
    extend_body:
      [ [ extend_header{(gram,old)};  OPT global{global_list };
          LIST1 [ entry{e}  -> e ]{el} -> (* semi_sep removed *)
            let res = text_of_functorial_extend _loc  gram global_list el in 
            let () = grammar_module_name := old in
            res      ] ] 
    delete_rule_body:
      [ [ delete_rule_header{old}; name{n}; ":"; LIST0 symbol SEP semi_sep{sl} -> 
        let (e, b) = expr_of_delete_rule _loc n sl in (*FIXME*)
        let res =  <:expr< $(id:gm()).delete_rule $e $b >>  in
        let () = grammar_module_name := old  in 
        res   ] ]
     delete_rule_header: (*for side effets, parser action *)
        [[ qualuid{g} ->
          let old = gm () in
          let () = grammar_module_name := g in
          old  ]]
    qualuid:
      [ [ `UIDENT x; ".";  SELF{xs} -> <:ident< $uid:x.$xs >>
        | `UIDENT x -> <:ident< $uid:x >> ] ] 
    qualid:
      [ [ `UIDENT x; ".";  SELF{xs} -> <:ident< $uid:x.$xs >>
        | `UIDENT i -> <:ident< $uid:i >>
        | `LIDENT i -> <:ident< $lid:i >> ] ]
    t_qualid:
      [ [ `UIDENT x; ".";  SELF{xs} -> <:ident< $uid:x.$xs >>
        | `UIDENT x; "."; `LIDENT "t" -> <:ident< $uid:x >> ] ] 
    global:
      [ [ `UIDENT "GLOBAL"; ":"; LIST1 name{sl}; semi_sep -> sl ] ]
    entry:
      [ [ name{n}; ":";  OPT position{pos}; level_list{ll} ->
            mk_entry ~name:n ~pos ~levels:ll ] ]
    position:
      [ [ `UIDENT ("First"|"Last" as x ) ->   <:expr< `$uid:x >>
        | `UIDENT ("Before" | "After" | "Level" as x) ; string{n} ->
            <:expr< ` $uid:x  $n >> (*FIXME string escape?*)    ] ]
    level_list:
      [ [ "["; LIST0 level SEP "|"{ll}; "]" -> ll ] ]
    level:
      [ [  OPT [  `STRING (_, x)  -> x ]{lab};  OPT assoc{ass}; rule_list{rules} ->
            mk_level ~label:lab ~assoc:ass ~rules ]]
    assoc:
      [[ `UIDENT ("LA"|"RA"|"NA" as x) ->
         <:expr< `$uid:x >> 
       | `UIDENT x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ] ]
    rule_list:
      [ [ "["; "]" -> []
        | "["; LIST1 rule SEP "|"{rules}; "]" ->  retype_rule_list_without_patterns _loc rules ] ]
    rule:
      [ [ LIST0 psymbol SEP semi_sep{psl}; "->"; expr{act} -> mk_rule ~prod:psl ~action:(Some act )
        | LIST0 psymbol SEP semi_sep{psl} ->  mk_rule ~prod:psl ~action:None ] ]
    psymbol:
      [ [ symbol{s} ; "{"; pattern{p} ; "}" ->   { (s) with pattern = Some p } 
        | symbol{s} -> s ] ]
    symbol:
      [ "top" NA
        [ `UIDENT ("LIST0"| "LIST1" as x); SELF{s};  OPT [`UIDENT "SEP"; symbol{t} -> t ]{sep } ->
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

        |`UIDENT "OPT"; SELF{s} ->
            let () = check_not_tok s in
            let styp = STapp _loc (STlid _loc "option") s.styp in
            let text = TXopt _loc s.text in
            mk_symbol ~used:s.used ~text ~styp ~pattern:None

        |`UIDENT "TRY"; SELF{s} ->
            let text = TXtry _loc s.text in
            mk_symbol ~used:s.used ~text ~styp:(s.styp) ~pattern:None]

      | [`UIDENT "SELF" ->
          mk_symbol ~used:[] ~text:(TXself _loc)  ~styp:(STself _loc "SELF") ~pattern:None
        |`UIDENT "NEXT" ->
            mk_symbol ~used:[] ~text:(TXnext _loc)   ~styp:(STself _loc "NEXT") ~pattern:None
        | "["; LIST0 rule SEP "|"{rl}; "]" ->
            let rl = retype_rule_list_without_patterns _loc rl in
            let t = new_type_var () in
            let used = used_of_rule_list rl in
            mk_symbol ~used ~text:(TXrules _loc (srules _loc t rl ""))
              ~styp:(STquo _loc t) ~pattern:None
              
        (* |  "`"; a_ident{i}; OPT patt{p} -> (\* FIXME could be more expressive here *\) *)
        (*     let p = match p with *)
        (*     [None ->  <:patt< `$i >> *)
        (*     |Some p -> <:patt< `$i $p >> ] in  *)
        (*     mk_tok _loc p (STtok _loc) *)
              
        | "`"; a_ident{i}; OPT patt{p} ->
            let p = match p with
              [None -> <:patt< `$i >>
              |Some p -> <:patt< `$i $p >> ] in 
            let (p,ls) = Expr.filter_patt_with_captured_variables p in
            match ls with
            [ [] -> mk_tok _loc p (STtok _loc)
            | [(x,y)::ys] ->
                let restrict =
                  List.fold_left (fun acc (x,y) -> <:expr< $acc && ( $x = $y ) >> )
                    <:expr< $x = $y >> ys  in 
                mk_tok _loc ~restrict p (STtok _loc) ]                

        (* | `UIDENT x; `ANTIQUOT ("", s) -> *)
        (*     let e = AntiquotSyntax.parse_expr _loc s in *)
        (*     let match_fun = <:expr< *)
        (*       fun [ $uid:x camlp4_x when camlp4_x = $e -> True | _ -> False ] >> in *)
        (*     let descr = "$" ^ x ^ " " ^ s in *)
        (*     let text = TXtok _loc match_fun descr in *)
        (*     let p = <:patt< $uid:x $(tup:<:patt< _ >>) >> in *)
        (*     mk_symbol ~used:[] ~text ~styp:(STtok _loc) ~pattern:(Some p) *)
        | `STRING (_, s) ->
            mk_symbol ~used:[] ~text:(TXkwd _loc s) ~styp:(STtok _loc) ~pattern:None
        | name{n};  OPT [`UIDENT "Level"; `STRING (_, s) -> s ]{lev} ->
            mk_symbol ~used:[n.tvar] ~text:(TXnterm _loc n lev) ~styp:(STquo _loc n.tvar) ~pattern:None
        | "("; SELF{s_t}; ")" -> s_t ] ]
    pattern:
      [ [ `LIDENT i -> <:patt< $lid:i >>
        | "_" -> <:patt< _ >>
        | "("; pattern{p}; ")" -> <:patt< $p >>
        | "("; pattern{p1}; ",";  comma_patt{p2}; ")" -> <:patt< ( $p1, $p2 ) >>  ] ]
    comma_patt:
      [ [ SELF{p1}; ",";  SELF{p2} -> <:patt< $p1, $p2 >>
        | pattern{p} -> p ] ]
    name:
      [ [ qualid{il} -> mk_name _loc il ] ]
    string:
      [ [ `STRING (_, s) -> <:expr< $str:s >>
        | `ANTIQUOT ("", s) -> AntiquotSyntax.parse_expr _loc s ] ]
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
      [ [`UIDENT "FOLD0"; simple_expr{f}; simple_expr{e}; SELF{s} ->
            sfold _loc "FOLD0" "sfold0" f e s
        |`UIDENT "FOLD1"; simple_expr{f}; simple_expr{e}; SELF{s} ->
            sfold _loc "FOLD1" "sfold1" f e s
        |`UIDENT "FOLD0"; simple_expr{f}; simple_expr{e}; SELF{s};`UIDENT "SEP"; symbol{sep} ->
            sfoldsep _loc "FOLD0 SEP" "sfold0sep" f e s sep
        |`UIDENT "FOLD1"; simple_expr{f}; simple_expr{e}; SELF{s};
`UIDENT "SEP"; symbol{sep} ->
            sfoldsep _loc "FOLD1 SEP" "sfold1sep" f e s sep ] ]
    simple_expr:
      [ [ a_LIDENT{i} -> <:expr< $lid:i >>
        | "("; expr{e}; ")" -> e ] ]
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
  (* open FanSig; *)
  include Syntax;
  module Ast = Camlp4Ast;

  (* usual trick *) (* FIXME utilities based on Gram *)
  (* let test_patt_lessminus = *)
  (*   Gram.of_parser "test_patt_lessminus" *)
  (*     (fun strm -> *)
  (*       let rec skip_patt n = *)
  (*         match stream_peek_nth n strm with *)
  (*         [ Some (KEYWORD "<-") -> n *)
  (*         | Some (KEYWORD ("[" | "[<")) -> *)
  (*             skip_patt (ignore_upto "]" (n + 1) + 1) *)
  (*         | Some (KEYWORD "(") -> *)
  (*             skip_patt (ignore_upto ")" (n + 1) + 1) *)
  (*         | Some (KEYWORD "{") -> *)
  (*             skip_patt (ignore_upto "}" (n + 1) + 1) *)
  (*         | Some (KEYWORD ("as" | "::" | "," | "_")) *)
  (*         | Some (LIDENT _ | UIDENT _) -> skip_patt (n + 1) *)
  (*         | Some _ | None -> raise Stream.Failure ] *)
  (*       and ignore_upto end_kwd n = *)
  (*         match stream_peek_nth n strm with *)
  (*         [ Some (KEYWORD prm) when prm = end_kwd -> n *)
  (*         | Some (KEYWORD ("[" | "[<")) -> *)
  (*             ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1) *)
  (*         | Some (KEYWORD "(") -> *)
  (*             ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1) *)
  (*         | Some (KEYWORD "{") -> *)
  (*             ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1) *)
  (*         | Some _ -> ignore_upto end_kwd (n + 1) *)
  (*         | None -> raise Stream.Failure ] *)
  (*       in *)
  (*       skip_patt 1); *)

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
      [ [ "["; comprehension_or_sem_expr_for_list{e}; "]" -> e ] ]  
    comprehension_or_sem_expr_for_list:
      [ [  expr Level "top"{e}; ";"; sem_expr_for_list{mk} ->
            <:expr< [ $e :: $(mk <:expr< [] >>) ] >>
        | expr Level "top"{e}; ";" -> <:expr< [$e] >>
        | expr Level "top"{e}; "|"; LIST1 item SEP ";"{l} -> Expr.compr _loc e l
        | expr Level "top"{e} -> <:expr< [$e] >> ] ]  
    item:
      (* NP: These rules rely on being on this particular order. Which should
             be improved. *)
      [ [  TRY [ patt{p}; "<-" -> p]{p} ;  expr Level "top"{e} -> `gen (p, e)
        | expr Level "top"{e} -> `cond e ] ] 
  END;
  if is_revised then
    EXTEND Gram
      GLOBAL: expr comprehension_or_sem_expr_for_list;
      comprehension_or_sem_expr_for_list:
      [ [  expr Level "top"{e}; ";"; sem_expr_for_list{mk}; "::"; expr{last} ->
            <:expr< [ $e :: $(mk last) ] >>
        | expr Level "top"{e}; "::"; expr{last} ->
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
  (* open FanSig; *)
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
          [ [ `UIDENT $x -> (new Ast.reloc _loc)#expr e ]] 
        patt: Level "simple"
          [ [ `UIDENT $x ->
            let p = Expr.substp _loc [] e
            in (new Ast.reloc _loc)#patt p ]]
        END
      | Some (sl, e) ->
          EXTEND Gram
            expr: Level "apply"
            [ [ `UIDENT $x; SELF{param} ->
              let el =  match param with
              [ <:expr< ($tup:e) >> -> Ast.list_of_expr e []
              | e -> [e] ]  in
              if List.length el = List.length sl then
                let env = List.combine sl el in
                (new Expr.subst _loc env)#expr e
              else
                incorrect_number _loc el sl ] ] 
          patt: Level "simple"
            [ [ `UIDENT $x; SELF{param} ->
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
            begin
              DELETE_RULE Gram expr: `UIDENT $x END;
              DELETE_RULE Gram patt: `UIDENT $x END;
            end
        | Some (_, _) ->
            begin
              DELETE_RULE Gram expr: `UIDENT $x; SELF END;
              DELETE_RULE Gram patt: `UIDENT $x; SELF END;
            end
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
      [ [ macro_def{x} ->
            execute_macro <:str_item<>> (fun a b -> <:str_item< $a; $b >>) x ] ]
    sig_item: First
      [ [ macro_def_sig{x} ->
            execute_macro <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) x ] ]
    macro_def:
      [ [ "DEFINE"; uident{i}; opt_macro_value{def} -> SdDef i def
        | "UNDEF";  uident{i} -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; smlist_then{st1}; else_macro_def{st2} ->
            make_SdITE_result st1 st2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; smlist_then{st1}; else_macro_def{st2} ->
            make_SdITE_result st1 st2
        | "INCLUDE"; `STRING (_, fname) ->
            SdLazy (lazy (parse_include_file str_items fname)) ] ] 
    macro_def_sig:
      [ [ "DEFINE"; uident{i} -> SdDef i None
        | "UNDEF";  uident{i} -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
            make_SdITE_result sg1 sg2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
            make_SdITE_result sg1 sg2
        | "INCLUDE"; `STRING (_, fname) ->
            SdLazy (lazy (parse_include_file sig_items fname)) ] ] 
    uident_eval_ifdef:
      [ [ uident{i} -> Stack.push (is_defined i) stack ]] 
    uident_eval_ifndef:
      [ [ uident{i} -> Stack.push (not (is_defined i)) stack ]] 
    else_macro_def:
      [ [ "ELSE"; smlist_else{st}; endif -> st
        | endif -> [] ] ]  
    else_macro_def_sig:
      [ [ "ELSE"; sglist_else{st}; endif -> st
        | endif -> [] ] ]  
    else_expr:
      [ [ "ELSE"; expr{e}; endif -> e
      | endif -> <:expr< () >> ] ] 
    smlist_then:
      [ [ LIST1 [ macro_def{d}; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Then d
                      | str_item{si}; semi -> SdStr si ]{sml} -> sml ] ] 
    smlist_else:
      [ [ LIST1 [ macro_def{d}; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Else d
                      | str_item{si}; semi -> SdStr si ]{sml} -> sml ] ] 
    sglist_then:
      [ [ LIST1 [ macro_def_sig{d}; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Then d
                      | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ] ]  
    sglist_else:
      [ [ LIST1 [ macro_def_sig{d}; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Else d
                      | sig_item{si}; semi -> SdStr si ]{sgl} -> sgl ] ]  
    endif:
      [ [ "END" -> ()
        | "ENDIF" -> () ] ]  
    opt_macro_value:
      [ [ "("; LIST1 [ `LIDENT x -> x ] SEP ","{pl}; ")"; "="; expr{e} -> Some (pl, e)
        | "="; expr{e} -> Some ([], e)
        | -> None ] ]  
    expr: Level "top"
      [ [ "IFDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
            if is_defined i then e1 else e2
        | "IFNDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
            if is_defined i then e2 else e1
        | "DEFINE"; `LIDENT i; "="; expr{def}; "IN"; expr{body} ->
            (new Expr.subst _loc [(i, def)])#expr body ] ] 
    patt:
      [ [ "IFDEF"; uident{i}; "THEN"; patt{p1}; "ELSE"; patt{p2}; endif ->
            if is_defined i then p1 else p2
        | "IFNDEF"; uident{i}; "THEN"; patt{p1}; "ELSE"; patt{p2}; endif ->
            if is_defined i then p2 else p1 ] ] 
    uident:
      [ [ `UIDENT i -> i ] ]  
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)
    expr: Before "simple"
      [ [ "`";  [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
                     | "DEFINE" | "IN" ]{kwd} -> <:expr< `$uid:kwd >>
        | "`"; a_ident{s} -> <:expr< ` $s >> ] ] 
    (* idem *)
    patt: Before "simple"
      [ [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ]{kwd} ->
            <:patt< `$uid:kwd >>
        | "`"; a_ident{s} -> <:patt< ` $s >> ] ]
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
  (* open FanSig; *)
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
        [< (`KEYWORD x | `SYMBOL x, ti) when p x >] ->
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
    [ [< ((`KEYWORD "(", _) as tok); 'xs >] ->
        match xs with parser
        [ [< (`KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i), _loc);
             (`KEYWORD ")", _); 'xs >] ->
                [< (`LIDENT i, _loc); '(infix_kwds_filter xs) >]
        | [< 'xs >] ->
                [< tok; '(infix_kwds_filter xs) >] ]
    | [< x; 'xs >] -> [< x; '(infix_kwds_filter xs) >] ];

  FanToken.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.setup_parser sem_expr begin
    let symb1 = Gram.parse_origin_tokens expr in
    let symb =
      parser
      [ [< (`ANTIQUOT (("list" as n), s), ti) >] ->
        let _loc = Gram.token_location ti in
        <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
      | [< a = symb1 >] -> a ]
    in
    let rec kont al =
      parser
      [ [< (`KEYWORD ";", _); a = symb; 's >] ->
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
        [ "functor"; "("; a_UIDENT{i}; ":"; module_type{t}; ")"; "->";
          SELF{me} ->
            <:module_expr< functor ( $i : $t ) -> $me >>
        | "struct"; str_items{st}; "end" ->
            <:module_expr< struct $st end >> ]
      | "apply"
        [ SELF{me1}; SELF{me2} -> <:module_expr< $me1 $me2 >> ]
      | "simple"
        [ `ANTIQUOT ((""|"mexp"|"anti"|"list" as n),s) ->
            <:module_expr< $(anti:mk_anti ~c:"module_expr" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.module_expr_tag
        | module_longident{i} -> <:module_expr< $id:i >>
        | "("; SELF{me}; ":"; module_type{mt}; ")" ->
            <:module_expr< ( $me : $mt ) >>
        | "("; SELF{me}; ")" -> <:module_expr< $me >>
        | "("; "val"; expr{e}; ")" -> (* val *)
            <:module_expr< (val $e) >>  (* first class modules *)
        | "("; "val"; expr{e}; ":"; package_type{p}; ")" ->
            <:module_expr< (val $e : $p) >> ] ]
    str_item:
      [ "top"
        [ "exception"; constructor_declaration{t} ->
            <:str_item< exception $t >>
        | "exception"; constructor_declaration{t}; "="; type_longident{i} ->
            <:str_item< exception $t = $i >>
        | "external"; a_LIDENT{i}; ":"; ctyp{t}; "="; string_list{sl} ->
            <:str_item< external $i : $t = $sl >>
        | "include"; module_expr{me} -> <:str_item< include $me >>
        | "module"; a_UIDENT{i}; module_binding0{mb} ->
            <:str_item< module $i = $mb >>
        | "module"; "rec"; module_binding{mb} ->
            <:str_item< module rec $mb >>
        | "module"; "type"; a_ident{i}; "="; module_type{mt} ->
            <:str_item< module type $i = $mt >>
        | "open"; module_longident{i} -> <:str_item< open $i >>
        | "type"; type_declaration{td} ->
            <:str_item< type $td >>
        | "let"; opt_rec{r}; binding{bi}; "in"; expr{x} ->
              <:str_item< let $rec:r $bi in $x >>
        | "let"; opt_rec{r}; binding{bi} ->   match bi with
            [ <:binding< _ = $e >> -> <:str_item< $exp:e >>
            | _ -> <:str_item< let $rec:r $bi >> ]
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e} ->
              <:str_item< let module $m = $mb in $e >>
        | "let"; "open"; module_longident{i}; "in"; expr{e} ->
              <:str_item< let open $id:i in $e >>

        | "class"; class_declaration{cd} ->
            <:str_item< class $cd >>
        | "class"; "type"; class_type_declaration{ctd} ->
            <:str_item< class type $ctd >>
        | `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s) ->
            <:str_item< $(anti:mk_anti ~c:"str_item" n s) >>
            (*
              first, it gives "mk_anti ~c:"str_item" n s" , and then through
              the meta operation, it gets
              (Ast.StAnt (_loc, ( (mk_anti ~c:"str_item" n s) )))
             *)
        | `QUOTATION x -> Quotation.expand _loc x DynAst.str_item_tag
        | expr{e} -> <:str_item< $exp:e >>
        (* this entry makes <:str_item< let $rec:r $bi in $x >> parsable *)
        ] ]
    module_binding0:
      [ RA
        [ "("; a_UIDENT{m}; ":"; module_type{mt}; ")"; SELF{mb} ->
            <:module_expr< functor ( $m : $mt ) -> $mb >>
        | ":"; module_type{mt}; "="; module_expr{me} ->
            <:module_expr< ( $me : $mt ) >>
        | "="; module_expr{me} -> <:module_expr< $me >> ] ]
    module_binding:
      [ LA
        [ SELF{b1}; "and"; SELF{b2} ->
            <:module_binding< $b1 and $b2 >>
        | `ANTIQUOT (("module_binding"|"anti"|"list" as n),s) ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT (("" as n),s) ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT (("" as n),m); ":"; module_type{mt}; "="; module_expr{me} ->
            <:module_binding< $(mk_anti n m) : $mt = $me >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} ->
            <:module_binding< $m : $mt = $me >> ] ]
    module_type:
      [ "top"
        [ "functor"; "("; a_UIDENT{i}; ":"; SELF{t}; ")"; "->"; SELF{mt} ->
            <:module_type< functor ( $i : $t ) -> $mt >> ]
      | "with"
        [ SELF{mt}; "with"; with_constr{wc} ->
            <:module_type< $mt with $wc >> ]
      | "apply"
        [ SELF{mt1}; SELF{mt2}; dummy -> ModuleType.app mt1 mt2 ]
      | "."
        [ SELF{mt1}; "."; SELF{mt2} -> ModuleType.acc mt1 mt2 ]
      | "sig"
        [ "sig"; sig_items{sg}; "end" ->
            <:module_type< sig $sg end >> ]
      | "simple"
        [ `ANTIQUOT ((""|"mtyp"|"anti"|"list" as n),s) ->
            <:module_type< $(anti:mk_anti ~c:"module_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_type_tag
        | module_longident_with_app{i} -> <:module_type< $id:i >>
        | "'"; a_ident{i} -> <:module_type< ' $i >>
        | "("; SELF{mt}; ")" -> <:module_type< $mt >>
        | "module"; "type"; "of"; module_expr{me} ->
            <:module_type< module type of $me >> ] ]
    sig_item:
      [ "top"
        [ `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s) ->
            <:sig_item< $(anti:mk_anti ~c:"sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.sig_item_tag
        | "exception"; constructor_declaration{t} ->
            <:sig_item< exception $t >>
        | "external"; a_LIDENT{i}; ":"; ctyp{t}; "="; string_list{sl} ->
            <:sig_item< external $i : $t = $sl >>
        | "include"; module_type{mt} -> <:sig_item< include $mt >>
        | "module"; a_UIDENT{i}; module_declaration{mt} ->
            <:sig_item< module $i : $mt >>
        | "module"; "rec"; module_rec_declaration{mb} ->
            <:sig_item< module rec $mb >>
        | "module"; "type"; a_ident{i}; "="; module_type{mt} ->
            <:sig_item< module type $i = $mt >>
        | "module"; "type"; a_ident{i} ->
            <:sig_item< module type $i >>
        | "open"; module_longident{i} -> <:sig_item< open $i >>
        | "type"; type_declaration{t} ->
            <:sig_item< type $t >>
        | "val"; a_LIDENT{i}; ":"; ctyp{t} ->
            <:sig_item< val $i : $t >>
        | "class"; class_description{cd} ->
            <:sig_item< class $cd >>
        | "class"; "type"; class_type_declaration{ctd} ->
            <:sig_item< class type $ctd >> ] ]
    module_declaration:
      [ RA
        [ ":"; module_type{mt} -> <:module_type< $mt >>
        | "("; a_UIDENT{i}; ":"; module_type{t}; ")"; SELF{mt} ->
            <:module_type< functor ( $i : $t ) -> $mt >> ] ]
    module_rec_declaration:
      [ LA
        [ SELF{m1}; "and"; SELF{m2} -> <:module_binding< $m1 and $m2 >>
        | `ANTIQUOT ((""|"module_binding"|"anti"|"list" as n),s) ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.module_binding_tag
        | a_UIDENT{m}; ":"; module_type{mt} -> <:module_binding< $m : $mt >>
      ] ]
    with_constr:
      [ LA
        [ SELF{wc1}; "and"; SELF{wc2} -> <:with_constr< $wc1 and $wc2 >>
        | `ANTIQUOT ((""|"with_constr"|"anti"|"list" as n),s) ->
            <:with_constr< $(anti:mk_anti ~c:"with_constr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.with_constr_tag
        | "type"; `ANTIQUOT ((""|"typ"|"anti" as n),s); "="; ctyp{t} ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) = $t >>
        | "type"; type_longident_and_parameters{t1}; "="; ctyp{t2} ->
            <:with_constr< type $t1 = $t2 >>
        | "module"; module_longident{i1}; "="; module_longident_with_app{i2} ->
            <:with_constr< module $i1 = $i2 >>
        | "type"; `ANTIQUOT ((""|"typ"|"anti" as n),s); ":="; ctyp{t} ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) := $t >>
        | "type"; type_longident_and_parameters{t1}; ":="; ctyp{t2} ->
            <:with_constr< type $t1 := $t2 >>
        | "module"; module_longident{i1}; ":="; module_longident_with_app{i2} ->
            <:with_constr< module $i1 := $i2 >> ] ]
    expr:
      [ "top" RA
        [ "let"; opt_rec{r}; binding{bi}; "in"; SELF{x} ->
            <:expr< let $rec:r $bi in $x >>
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; SELF{e} ->
            <:expr< let module $m = $mb in $e >>
        | "let"; "open"; module_longident{i}; "in"; SELF{e} ->
            <:expr< let open $id:i in $e >>
        | "fun"; "[";  LIST0 match_case0 SEP "|"{a}; "]" ->
            <:expr< fun [ $list:a ] >>
        | "fun"; fun_def{e} -> e
        | "match"; sequence{e}; "with"; match_case{a} ->
            <:expr< match $(Expr.mksequence' _loc e) with [ $a ] >>
        | "try"; sequence{e}; "with"; match_case{a} ->
            <:expr< try $(Expr.mksequence' _loc e) with [ $a ] >>
        | "if"; SELF{e1}; "then"; SELF{e2}; "else"; SELF{e3} ->
            <:expr< if $e1 then $e2 else $e3 >>
        | "do"; do_sequence{seq} -> Expr.mksequence _loc seq
        | "for"; a_LIDENT{i}; "="; sequence{e1}; direction_flag{df};
          sequence{e2}; "do"; do_sequence{seq} ->
            <:expr< for $i = $(Expr.mksequence' _loc e1) $to:df $(Expr.mksequence' _loc e2) do
              { $seq } >>
        | "while"; sequence{e}; "do"; do_sequence{seq} ->
            <:expr< while $(Expr.mksequence' _loc e) do { $seq } >>
        | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" ->
            <:expr< object ($csp) $cst end >> ]
      | "where"
        [ SELF{e}; "where"; opt_rec{rf}; let_binding{lb} ->
            <:expr< let $rec:rf $lb in $e >> ]
      | ":=" NA
        [ SELF{e1}; ":="; SELF{e2}; dummy ->
              <:expr< $e1 := $e2 >> 
        | SELF{e1}; "<-"; SELF{e2}; dummy -> (* FIXME should be deleted in original syntax later? *)
            match Expr.bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1 <- $e2 >> 
            ]  
        ]
      | "||" RA
        [ SELF{e1}; infixop6{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "&&" RA
        [ SELF{e1}; infixop5{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "<" LA
        [ SELF{e1}; infixop0{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "^" RA
        [ SELF{e1}; infixop1{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "+" LA
        [ SELF{e1}; infixop2{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "*" LA
        [ SELF{e1}; "land"; SELF{e2} -> <:expr< $e1 land $e2 >>
        | SELF{e1}; "lor"; SELF{e2} -> <:expr< $e1 lor $e2 >>
        | SELF{e1}; "lxor"; SELF{e2} -> <:expr< $e1 lxor $e2 >>
        | SELF{e1}; "mod"; SELF{e2} -> <:expr< $e1 mod $e2 >>
        | SELF{e1}; infixop3{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "**" RA
        [ SELF{e1}; "asr"; SELF{e2} -> <:expr< $e1 asr $e2 >>
        | SELF{e1}; "lsl"; SELF{e2} -> <:expr< $e1 lsl $e2 >>
        | SELF{e1}; "lsr"; SELF{e2} -> <:expr< $e1 lsr $e2 >>
        | SELF{e1}; infixop4{op}; SELF{e2} -> <:expr< $op $e1 $e2 >> ]
      | "unary minus" NA
        [ "-"; SELF{e} -> Expr.mkumin _loc "-" e
        | "-."; SELF{e} -> Expr.mkumin _loc "-." e ]
      | "apply" LA
        [ SELF{e1}; SELF{e2} -> <:expr< $e1 $e2 >>
        | "assert"; SELF{e} -> Expr.mkassert _loc e
        | "new"; class_longident{i} -> <:expr< new $i >>
        | "lazy"; SELF{e} -> <:expr< lazy $e >> ]
      | "label" NA
        [ "~"; a_LIDENT{i}; ":"; SELF{e} -> <:expr< ~ $i : $e >>
        | "~"; a_LIDENT{i} -> <:expr< ~ $i >>

        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; SELF{e} -> <:expr< ~ $i : $e >>

        (* Same remark for ?a:b *)
        | `OPTLABEL i; SELF{e} -> <:expr< ? $i : $e >>

        | "?"; a_LIDENT{i}; ":"; SELF{e} -> <:expr< ? $i : $e >>
        | "?"; a_LIDENT{i} -> <:expr< ? $i >> ]
      | "." LA
        [ SELF{e1}; "."; "("; SELF{e2}; ")" -> <:expr< $e1 .( $e2 ) >>
        | SELF{e1}; "."; "["; SELF{e2}; "]" -> <:expr< $e1 .[ $e2 ] >>
        | SELF{e1}; "."; "{"; comma_expr{e2}; "}" -> Expr.bigarray_get _loc e1 e2
        | SELF{e1}; "."; SELF{e2} -> <:expr< $e1 . $e2 >>
        | SELF{e}; "#"; label{lab} -> <:expr< $e # $lab >> ]
      | "~-" NA
        [ "!"; SELF{e} ->  <:expr< ! $e>>
        | prefixop{f}; SELF{e} -> <:expr< $f $e >> ]
      | "simple"
        [ `QUOTATION x -> Quotation.expand _loc x DynAst.expr_tag
        | `ANTIQUOT (("exp"|""|"anti" as n),s) ->
            <:expr< $(anti:mk_anti ~c:"expr" n s) >>
        | `ANTIQUOT (("`bool" as n),s) ->
            <:expr< $(id:<:ident< $(anti:mk_anti n s) >>) >>
        | `ANTIQUOT (("tup" as n),s) ->
            <:expr< $(tup: <:expr< $(anti:mk_anti ~c:"expr" n s) >>) >>
        | `ANTIQUOT (("seq" as n),s) ->
            <:expr< do $(anti:mk_anti ~c:"expr" n s) done >>
        | a_INT{s} -> <:expr< $int:s >>
        | a_INT32{s} -> <:expr< $int32:s >>
        | a_INT64{s} -> <:expr< $int64:s >>
        | a_NATIVEINT{s} -> <:expr< $nativeint:s >>
        | a_FLOAT{s} -> <:expr< $flo:s >>
        | a_STRING{s} -> <:expr< $str:s >>
        | a_CHAR{s} -> <:expr< $chr:s >>
        | TRY module_longident_dot_lparen{i}; sequence{e}; ")" ->
            <:expr< let open $i in $e >>
        | TRY val_longident{i} -> <:expr< $id:i >>
        | "`"; a_ident{s} -> <:expr< ` $s >>
        | "["; "]" -> <:expr< [] >>
        | "[";sem_expr_for_list{mk_list}; "::"; expr{last}; "]" ->
            mk_list last
        | "["; sem_expr_for_list{mk_list}; "]" ->
            mk_list <:expr< [] >>
        | "[|"; "|]" -> <:expr< [| $(<:expr<>>) |] >>
        | "[|"; sem_expr{el}; "|]" -> <:expr< [| $el |] >>
        | "{"; label_expr_list{el}; "}" -> <:expr< { $el } >>
        | "{"; "("; SELF{e}; ")"; "with"; label_expr_list{el}; "}" ->
            <:expr< { ($e) with $el } >>
        | "{<"; ">}" -> <:expr< {<>} >>
        | "{<"; field_expr_list{fel}; ">}" -> <:expr< {< $fel >} >>
        | "("; ")" -> <:expr< () >>
        | "("; SELF{e}; ":"; ctyp{t}; ")" -> <:expr< ($e : $t) >>
        | "("; SELF{e}; ","; comma_expr{el}; ")" -> <:expr< ( $e, $el ) >>
        | "("; SELF{e}; ";"; sequence{seq}; ")" -> Expr.mksequence _loc <:expr< $e; $seq >>
        | "("; SELF{e}; ";"; ")" -> Expr.mksequence _loc e
        | "("; SELF{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" ->
            <:expr< ($e : $t :> $t2 ) >>
        | "("; SELF{e}; ":>"; ctyp{t}; ")" -> <:expr< ($e :> $t) >>
        | "("; SELF{e}; ")" -> e
        | "begin"; sequence{seq}; "end" -> Expr.mksequence _loc seq
        | "begin"; "end" -> <:expr< () >>
        | "("; "module"; module_expr{me}; ")" ->
            <:expr< (module $me) >>
        | "("; "module"; module_expr{me}; ":"; package_type{pt}; ")" ->
            <:expr< (module $me : $pt) >>
        ] ]
    do_sequence:
      [ [ TRY ["{"; sequence{seq}; "}" -> seq]{seq} -> seq
        | TRY ["{"; "}"] -> <:expr< () >>
        | TRY [sequence{seq}; "done" -> seq]{seq} -> seq
        | "done" -> <:expr< () >>
      ] ]
    infixop5:
      [ [  [ "&" | "&&" ]{x} -> <:expr< $lid:x >> ] ]
    infixop6:
      [ [  [ "or" | "||" ]{x} -> <:expr< $lid:x >> ] ]
    sem_expr_for_list:
      [ [ expr{e}; ";"; SELF{el} -> fun acc -> <:expr< [ $e :: $(el acc) ] >>
        | expr{e}; ";" -> fun acc -> <:expr< [ $e :: $acc ] >>
        | expr{e} -> fun acc -> <:expr< [ $e :: $acc ] >>
      ] ]
    comma_expr:
      [ [ SELF{e1}; ","; SELF{e2} -> <:expr< $e1, $e2 >>
        | `ANTIQUOT (("list" as n),s) -> <:expr< $(anti:mk_anti ~c:"expr," n s) >>
        | expr Level "top"{e} -> e ] ]
    dummy:
      [ [ -> () ] ]
    sequence':
      [ [ -> fun e -> e
        | ";" -> fun e -> e
        | ";"; sequence{el} -> fun e -> <:expr< $e; $el >> ] ]
    sequence:
      [ [ "let"; opt_rec{rf}; binding{bi}; "in"; expr{e}; sequence'{k} ->
            k <:expr< let $rec:rf $bi in $e >>
        | "let"; opt_rec{rf}; binding{bi}; ";"; SELF{el} ->
            <:expr< let $rec:rf $bi in $(Expr.mksequence _loc el) >>
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e}; sequence'{k} ->
            k <:expr< let module $m = $mb in $e >>
        | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; ";"; SELF{el} ->
            <:expr< let module $m = $mb in $(Expr.mksequence _loc el) >>
        | "let"; "open"; module_longident{i}; "in"; SELF{e} ->
            <:expr< let open $id:i in $e >>
        | `ANTIQUOT (("list" as n),s) -> <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
        | expr{e}; sequence'{k} -> k e ] ]
    binding:
      [ LA
        [ `ANTIQUOT (("binding"|"list" as n),s) ->
            <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | `ANTIQUOT ((""|"anti" as n),s); "="; expr{e} ->
            <:binding< $(anti:mk_anti ~c:"patt" n s) = $e >>
        | `ANTIQUOT ((""|"anti" as n),s) -> <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | SELF{b1}; "and"; SELF{b2} -> <:binding< $b1 and $b2 >>
        | let_binding{b} -> b
      ] ]
    let_binding:
      [ [ ipatt{p}; fun_binding{e} -> <:binding< $p = $e >> ] ]
    fun_binding:
      [ RA
        [ TRY ["("; "type"]; a_LIDENT{i}; ")"; SELF{e} ->
            <:expr< fun (type $i) -> $e >>
        | TRY labeled_ipatt{p}; SELF{e} ->
            <:expr< fun $p -> $e >>
        | cvalue_binding{bi} -> bi
      ] ]
    match_case:
      [ [ "["; LIST0 match_case0 SEP "|"{l}; "]" -> Ast.mcOr_of_list l
        | ipatt{p}; "->"; expr{e} -> <:match_case< $p -> $e >> ] ]
    match_case0:
      [ [ `ANTIQUOT (("match_case"|"list" as n),s) ->
            <:match_case< $(anti:mk_anti ~c:"match_case" n s) >>
        | `ANTIQUOT ((""|"anti" as n),s) ->
            <:match_case< $(anti:mk_anti ~c:"match_case" n s) >>
        | `ANTIQUOT ((""|"anti" as n),s); "->"; expr{e} ->
            <:match_case< $(anti:mk_anti ~c:"patt" n s) -> $e >>
        | `ANTIQUOT ((""|"anti" as n),s); "when"; expr{w}; "->"; expr{e} ->
            <:match_case< $(anti:mk_anti ~c:"patt" n s) when $w -> $e >>
        | patt_as_patt_opt{p}; opt_when_expr{w}; "->"; expr{e} ->
            <:match_case< $p when $w -> $e >>
      ] ]
    opt_when_expr:
      [ [ "when"; expr{w} -> w
        | -> <:expr<>>   ] ]
    patt_as_patt_opt:
      [ [ patt{p1}; "as"; patt{p2} -> <:patt< ($p1 as $p2) >>
        | patt{p} -> p ] ]
    label_expr_list:
      [ [ label_expr{b1}; ";"; SELF{b2} -> <:rec_binding< $b1 ; $b2 >>
        | label_expr{b1}; ";"            -> b1
        | label_expr{b1}                 -> b1  ] ]
    label_expr:
      [ [ `ANTIQUOT (("rec_binding" as n),s) ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT ((""|"anti" as n),s) ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT ((""|"anti" as n),s); "="; expr{e} ->
            <:rec_binding< $(anti:mk_anti ~c:"ident" n s) = $e >>
        | `ANTIQUOT (("list" as n),s) ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | label_longident{i}; fun_binding{e} -> <:rec_binding< $i = $e >>
        | label_longident{i} ->
            <:rec_binding< $i = $(lid:Ident.to_lid i) >> ] ]
    fun_def:
      [ [ TRY ["("; "type"]; a_LIDENT{i}; ")";
          fun_def_cont_no_when{e} ->
            <:expr< fun (type $i) -> $e >>
        | TRY labeled_ipatt{p };  fun_def_cont{(w, e)} ->
            <:expr< fun [ $p when $w -> $e ] >> ] ]
    fun_def_cont:
      [ RA
        [ TRY ["("; "type"]; a_LIDENT{i}; ")";
          fun_def_cont_no_when{e} ->
            (<:expr<>>, <:expr< fun (type $i) -> $e >>)
        | TRY labeled_ipatt{p };  SELF{(w,e)} ->
            (<:expr<>>, <:expr< fun [ $p when $w -> $e ] >>)
        | "when"; expr{w}; "->"; expr{e} -> (w, e)
        | "->"; expr{e} -> (<:expr<>>, e) ] ]
    fun_def_cont_no_when:
      [ RA
        [ TRY ["("; "type"]; a_LIDENT{i}; ")";
          fun_def_cont_no_when{e} -> <:expr< fun (type $i) -> $e >>
        | TRY labeled_ipatt{ p}; fun_def_cont{(w,e)} ->
            <:expr< fun [ $p when $w -> $e ] >>
        | "->"; expr{e} -> e ] ]
    patt:
      [ "|" LA
        [ SELF{p1}; "|"; SELF{p2} -> <:patt< $p1 | $p2 >> ]
      | ".." NA
        [ SELF{p1}; ".."; SELF{p2} -> <:patt< $p1 .. $p2 >> ]
      | "apply" LA
        [ SELF{p1}; SELF{p2} -> <:patt< $p1 $p2 >>
        | "lazy"; SELF{p} -> <:patt< lazy $p >>  ]
      | "simple"
        [ `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT (("tup" as n),s) ->
            <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >> )) >>
        | `ANTIQUOT (("`bool" as n),s) ->
            <:patt< $(id:<:ident< $(anti:mk_anti n s) >>) >>
        | ident{i} -> <:patt< $id:i >>
        | a_INT{s} -> <:patt< $int:s >>
        | a_INT32{s} -> <:patt< $int32:s >>
        | a_INT64{s} -> <:patt< $int64:s >>
        | a_NATIVEINT{s} -> <:patt< $nativeint:s >>
        | a_FLOAT{s} -> <:patt< $flo:s >>
        | a_STRING{s} -> <:patt< $str:s >>
        | a_CHAR{s} -> <:patt< $chr:s >>
        | "-"; a_INT{s} -> <:patt< $(int:neg_string s) >>
        | "-"; a_INT32{s} -> <:patt< $(int32:neg_string s) >>
        | "-"; a_INT64{s} -> <:patt< $(int64:neg_string s) >>
        | "-"; a_NATIVEINT{s} -> <:patt< $(nativeint:neg_string s) >>
        | "-"; a_FLOAT{s} -> <:patt< $(flo:neg_string s) >>
        | "["; "]" -> <:patt< [] >>
        | "["; sem_patt_for_list{mk_list}; "::"; patt{last}; "]" ->
            mk_list last
        | "["; sem_patt_for_list{mk_list}; "]" ->
            mk_list <:patt< [] >>
        | "[|"; "|]" -> <:patt< [| $(<:patt<>>) |] >>
        | "[|"; sem_patt{pl}; "|]" -> <:patt< [| $pl |] >>
        | "{"; label_patt_list{pl}; "}" -> <:patt< { $pl } >>
        | "("; ")" -> <:patt< () >>
        | "("; "module"; a_UIDENT{m}; ")" -> <:patt< (module $m) >>
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" ->
            <:patt< ((module $m) : (module $pt)) >>
        | "("; SELF{p}; ")" -> p
        | "("; SELF{p}; ":"; ctyp{t}; ")" -> <:patt< ($p : $t) >>
        | "("; SELF{p}; "as"; SELF{p2}; ")" -> <:patt< ($p as $p2) >>
        | "("; SELF{p}; ","; comma_patt{pl}; ")" -> <:patt< ($p, $pl) >>
        | "_" -> <:patt< _ >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "`"; a_ident{s} -> <:patt< ` $s >>
        | "#"; type_longident{i} -> <:patt< # $i >>
        | `LABEL i; SELF{p} -> <:patt< ~ $i : $p >>
        | "~"; `ANTIQUOT ((""|"lid" as n),i); ":"; SELF{p} ->
            <:patt< ~ $(mk_anti n i) : $p >>
        | "~"; `ANTIQUOT ((""|"lid" as n),i) -> <:patt< ~ $(mk_anti n i) >>
        | "~"; `LIDENT i -> <:patt< ~ $i >>
        (* | opt_label{i}; "("; patt_tcon{p}; ")" -> *)
            (* <:patt< ? $i$ : ($p$) >> *)
        | `OPTLABEL i; "("; patt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANTIQUOT ((""|"lid" as n),i); ":"; "("; patt_tcon{p}; eq_expr{f}; ")" ->
            f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i >>
        | "?"; `ANTIQUOT ((""|"lid" as n),i) -> <:patt< ? $(mk_anti n i) >>
        | "?"; "("; patt_tcon{p}; ")" ->
            <:patt< ? ($p) >>
        | "?"; "("; patt_tcon{p}; "="; expr{e}; ")" ->
            <:patt< ? ($p = $e) >> ] ]
    comma_patt:
      [ [ SELF{p1}; ","; SELF{p2} -> <:patt< $p1, $p2 >>
        | `ANTIQUOT (("list" as n),s) -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | patt{p} -> p ] ]
    sem_patt:
      [ LA
        [ patt{p1}; ";"; SELF{p2} -> <:patt< $p1; $p2 >>
        | `ANTIQUOT (("list" as n),s) -> <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | patt{p}; ";" -> p
        | patt{p} -> p ] ]
    sem_patt_for_list:
      [ [ patt{p}; ";"; SELF{pl} -> fun acc -> <:patt< [ $p :: $(pl acc) ] >>
        | patt{p}; ";" -> fun acc -> <:patt< [ $p :: $acc ] >>
        | patt{p} -> fun acc -> <:patt< [ $p :: $acc ] >>
      ] ]
    label_patt_list:
      [ [ label_patt{p1}; ";"; SELF{p2} -> <:patt< $p1 ; $p2 >>
        | label_patt{p1}; ";"; "_"       -> <:patt< $p1 ; _ >>
        | label_patt{p1}; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | label_patt{p1}; ";"            -> p1
        | label_patt{p1}                 -> p1
      ] ]
    label_patt:
      [ [ `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | `ANTIQUOT (("list" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | label_longident{i}; "="; patt{p} -> <:patt< $i = $p >>
        | label_longident{i} -> <:patt< $i = $(lid:Ident.to_lid i) >>
      ] ]
    ipatt:
      [ [ "{"; label_ipatt_list{pl}; "}" -> <:patt< { $pl } >>
        | `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT (("tup" as n),s) ->
            <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >>)) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.patt_tag
        | "("; ")" -> <:patt< () >>
        | "("; "module"; a_UIDENT{m}; ")" -> <:patt< (module $m) >>
        | "("; "module"; a_UIDENT{m}; ":"; package_type{pt}; ")" ->
            <:patt< ((module $m) : (module $pt)) >>
        | "("; SELF{p}; ")" -> p
        | "("; SELF{p}; ":"; ctyp{t}; ")" -> <:patt< ($p : $t) >>
        | "("; SELF{p}; "as"; SELF{p2}; ")" -> <:patt< ($p as $p2) >>
        | "("; SELF{p}; ","; comma_ipatt{pl}; ")" -> <:patt< ($p, $pl) >>
        | a_LIDENT{s} -> <:patt< $lid:s >>
        | "_" -> <:patt< _ >> ] ]
    labeled_ipatt:
      [ [ ipatt{p} -> p ] ]
    comma_ipatt:
      [ LA
        [ SELF{p1}; ","; SELF{p2} -> <:patt< $p1, $p2 >>
        | `ANTIQUOT (("list" as n),s) -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | ipatt{p} -> p ] ]
    label_ipatt_list:
      [ [ label_ipatt{p1}; ";"; SELF{p2} -> <:patt< $p1 ; $p2 >>
        | label_ipatt{p1}; ";"; "_"       -> <:patt< $p1 ; _ >>
        | label_ipatt{p1}; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | label_ipatt{p1}; ";"            -> p1
        | label_ipatt{p1}                 -> p1
      ] ]
    label_ipatt:
      [ [ `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.patt_tag
        | label_longident{i}; "="; ipatt{p} -> <:patt< $i = $p >>
      ] ]
    type_declaration:
      [ LA
        [ `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | SELF{t1}; "and"; SELF{t2} -> <:ctyp< $t1 and $t2 >>
        |  type_ident_and_parameters{(n, tpl)}; opt_eq_ctyp{tk};
          LIST0 constrain{cl} -> Ast.TyDcl _loc n tpl tk cl ] ]
    constrain:
      [ [ "constraint"; ctyp{t1}; "="; ctyp{t2} -> (t1, t2) ] ]
    opt_eq_ctyp:
      [ [ "="; type_kind{tk} -> tk
        | -> <:ctyp<>> ] ]
    type_kind:
      [ [ ctyp{t} -> t ] ]
    type_ident_and_parameters:
      [ [ a_LIDENT{i}; LIST0 optional_type_parameter{tpl} -> (i, tpl) ] ]
    type_longident_and_parameters:
      [ [ type_longident{i}; type_parameters{tpl} -> tpl <:ctyp< $id:i >>
      ] ]
    type_parameters:
      [ [ type_parameter{t1}; SELF{t2} ->
            fun acc -> t2 <:ctyp< $acc $t1 >>
        | type_parameter{t} -> fun acc -> <:ctyp< $acc $t >>
        | -> fun t -> t
      ] ]
    type_parameter:
      [ [ `ANTIQUOT ((""|"typ"|"anti" as n),s) -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> <:ctyp< '$lid:i >>
        | "+"; "'"; a_ident{i} -> <:ctyp< +'$lid:i >>
        | "-"; "'"; a_ident{i} -> <:ctyp< -'$lid:i >> ] ]
    optional_type_parameter:
      [ [ `ANTIQUOT ((""|"typ"|"anti" as n),s) -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> <:ctyp< '$lid:i >>
        | "+"; "'"; a_ident{i} -> <:ctyp< +'$lid:i >>
        | "-"; "'"; a_ident{i} -> <:ctyp< -'$lid:i >>
        | "+"; "_" -> Ast.TyAnP _loc  
        | "-"; "_" -> Ast.TyAnM _loc  
        | "_" -> <:ctyp< _ >>  ] ]
    ctyp:
      [ "==" LA
        [ SELF{t1}; "=="; SELF{t2} -> <:ctyp< $t1 == $t2 >> ]
      | "private" NA
        [ "private"; ctyp Level "alias"{t} -> <:ctyp< private $t >> ]
      | "alias" LA
        [ SELF{t1}; "as"; SELF{t2} ->
          <:ctyp< $t1 as $t2 >> ]
      | "forall" LA
        [ "!"; typevars{t1}; "."; ctyp{t2} ->
          <:ctyp< ! $t1 . $t2 >> ]
      | "arrow" RA
        [ SELF{t1}; "->"; SELF{t2} ->
          <:ctyp< $t1 -> $t2 >> ]
      | "label" NA
        [ "~"; a_LIDENT{i}; ":"; SELF{t} ->
          <:ctyp< ~ $i : $t >>
        | a_LABEL{i}; SELF{t}  ->
          <:ctyp< ~ $i : $t >>
        | "?"; a_LIDENT{i}; ":"; SELF{t} ->
            <:ctyp< ? $i : $t >>
        | a_OPTLABEL{i}; SELF{t} ->
            <:ctyp< ? $i : $t >> ]
      | "apply" LA
        [ SELF{t1}; SELF{t2} ->
            let t = <:ctyp< $t1 $t2 >> in
            try <:ctyp< $(id:Ast.ident_of_ctyp t) >>
            with [ Invalid_argument _ -> t ] ]
      | "." LA
        [ SELF{t1}; "."; SELF{t2} ->
            try <:ctyp< $(id:Ast.ident_of_ctyp t1).$(id:Ast.ident_of_ctyp t2) >>
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
      | "simple"
        [ "'"; a_ident{i} -> <:ctyp< '$i >>
        | "_" -> <:ctyp< _ >>
        | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("tup" as n),s) ->
            <:ctyp< ($(tup:<:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>)) >>
        | `ANTIQUOT (("id" as n),s) ->
            <:ctyp< $(id:<:ident< $(anti:mk_anti ~c:"ident" n s) >>) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{i} -> <:ctyp< $lid:i >>
        | a_UIDENT{i} -> <:ctyp< $uid:i >>
        | "("; SELF{t}; "*"; star_ctyp{tl}; ")" ->
            <:ctyp< ( $t * $tl ) >>
        | "("; SELF{t}; ")" -> t
        | "["; "]" -> <:ctyp< [ ] >>
        | "["; constructor_declarations{t}; "]" -> <:ctyp< [ $t ] >>
        | "["; "="; row_field{rfl}; "]" ->
            <:ctyp< [ = $rfl ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $(<:ctyp<>>) ] >>
        | "["; ">"; row_field{rfl}; "]" ->
            <:ctyp< [ > $rfl ] >>
        | "["; "<"; row_field{rfl}; "]" ->
            <:ctyp< [ < $rfl ] >>
        | "["; "<"; row_field{rfl}; ">"; name_tags{ntl}; "]" ->
            <:ctyp< [ < $rfl > $ntl ] >>
        | "[<"; row_field{rfl}; "]" ->
            <:ctyp< [ < $rfl ] >>
        | "[<"; row_field{rfl}; ">"; name_tags{ntl}; "]" ->
            <:ctyp< [ < $rfl > $ntl ] >>
        | "{"; label_declaration_list{t}; "}" -> <:ctyp< { $t } >>
        | "#"; class_longident{i} -> <:ctyp< # $i >>
        | "<"; opt_meth_list{t}; ">" -> t
        | "("; "module"; package_type{p}; ")" -> <:ctyp< (module $p) >>  ] ]
    star_ctyp:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp*" n s) >>
        | SELF{t1}; "*"; SELF{t2} ->
            <:ctyp< $t1 * $t2 >>
        | ctyp{t} -> t  ] ]
    constructor_declarations:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp|" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | SELF{t1}; "|"; SELF{t2} ->
            <:ctyp< $t1 | $t2 >>
        | a_UIDENT{s}; "of"; constructor_arg_list{t} ->
            <:ctyp< $uid:s of $t >>
        | a_UIDENT{s}; ":"; ctyp{t} ->
            let (tl, rt) = Ctyp.to_generalized t in
            <:ctyp< $uid:s : ($(Ast.tyAnd_of_list tl) -> $rt) >>
        | a_UIDENT{s} ->
	  <:ctyp< $uid:s >>  ] ]
    constructor_declaration:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_UIDENT{s}; "of"; constructor_arg_list{t} ->
            <:ctyp< $uid:s of $t >>
        | a_UIDENT{s} ->
            <:ctyp< $uid:s >>
      ] ]
    constructor_arg_list:
      [ [ `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | SELF{t1}; "and"; SELF{t2} -> <:ctyp< $t1 and $t2 >>
        | ctyp{t} -> t
      ] ]
    label_declaration_list:
      [ [ label_declaration{t1}; ";"; SELF{t2} -> <:ctyp< $t1; $t2 >>
        | label_declaration{t1}; ";"            -> t1
        | label_declaration{t1}                 -> t1  ] ]
    label_declaration:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{s}; ":"; poly_type{t} ->
            <:ctyp< $lid:s : $t >>
        | a_LIDENT{s}; ":"; "mutable"; poly_type{t} ->
            <:ctyp< $lid:s : mutable $t >>  ] ]
    a_ident:
      [ [ a_LIDENT{i} -> i
        | a_UIDENT{i} -> i ] ]
    ident:
      [ [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) -> (* id it self does not support ANTIQUOT "lid", however [a_UIDENT] supports*)
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{i} -> <:ident< $uid:i >>
        | a_LIDENT{i} -> <:ident< $lid:i >>
        | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s); "."; SELF{i} ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | a_UIDENT{i}; "."; SELF{j} -> <:ident< $uid:i.$j >> ] ]
    module_longident:
      [ [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{m}; "."; SELF{l} -> <:ident< $uid:m.$l >>
        | a_UIDENT{i} -> <:ident< $uid:i >> ] ]
    module_longident_with_app:
      [ "apply"
        [ SELF{i}; SELF{j} -> <:ident< $i $j >> ]
      | "."
        [ SELF{i}; "."; SELF{j} -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{i} -> <:ident< $uid:i >>
        | "("; SELF{i}; ")" -> i ] ]
    module_longident_dot_lparen:
      [ [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s); "."; "(" ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{m}; "."; SELF{l} -> <:ident< $uid:m.$l >>
        | a_UIDENT{i}; "."; "(" -> <:ident< $uid:i >> ] ]
    type_longident:
      [ "apply"
        [ SELF{i}; SELF{j} -> <:ident< $i $j >> ]
      | "."
        [ SELF{i}; "."; SELF{j} -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_LIDENT{i} -> <:ident< $lid:i >>
        | a_UIDENT{i} -> <:ident< $uid:i >>
        | "("; SELF{i}; ")" -> i ] ]
    label_longident:
      [ [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{m}; "."; SELF{l} -> <:ident< $uid:m.$l >>
        | a_LIDENT{i} -> <:ident< $lid:i >> ] ]
    class_type_longident:
      [ [ type_longident{x} -> x ] ]
    val_longident:
      [ [ ident{x} -> x ] ]
    class_longident:
      [ [ label_longident{x} -> x ] ]
    class_declaration:
      [ LA
        [ SELF{c1}; "and"; SELF{c2} ->
            <:class_expr< $c1 and $c2 >>
        | `ANTIQUOT ((""|"cdcl"|"anti"|"list" as n),s) ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
        | class_info_for_class_expr{ci}; class_fun_binding{ce} ->
            <:class_expr< $ci = $ce >> ] ]
    class_fun_binding:
      [ [ "="; class_expr{ce} -> ce
        | ":"; class_type_plus{ct}; "="; class_expr{ce} ->
            <:class_expr< ($ce : $ct) >>
        | labeled_ipatt{p}; SELF{cfb} ->
            <:class_expr< fun $p -> $cfb >>  ] ]
    class_info_for_class_type:
      [ [ opt_virtual{mv};  class_name_and_param{(i, ot)} ->
            <:class_type< $virtual:mv $lid:i [ $ot ] >>  ] ]
    class_info_for_class_expr:
      [ [ opt_virtual{mv};  class_name_and_param{(i, ot)} ->
            <:class_expr< $virtual:mv $lid:i [ $ot ] >>  ] ]
    class_name_and_param:
      [ [ a_LIDENT{i}; "["; comma_type_parameter{x}; "]" -> (i, x)
        | a_LIDENT{i} -> (i, <:ctyp<>>)
      ] ]
    comma_type_parameter:
      [ [ SELF{t1}; ","; SELF{t2} -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT (("list" as n),s) -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | type_parameter{t} -> t  ] ]
    opt_comma_ctyp:
      [ [ "["; comma_ctyp{x}; "]" -> x
        | -> <:ctyp<>>  ] ]
    comma_ctyp:
      [ [ SELF{t1}; ","; SELF{t2} -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT (("list" as n),s) -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | ctyp{t} -> t  ] ]
    class_fun_def:
      [ [ labeled_ipatt{p}; SELF{ce} -> <:class_expr< fun $p -> $ce >>
        | "->"; class_expr{ce} -> ce ] ]
    class_expr:
      [ "top"
        [ "fun"; labeled_ipatt{p}; class_fun_def{ce} ->
            <:class_expr< fun $p -> $ce >>
        | "let"; opt_rec{rf}; binding{bi}; "in"; SELF{ce} ->
            <:class_expr< let $rec:rf $bi in $ce >> ]
      | "apply" NA
        [ SELF{ce}; expr Level "label"{e} ->
            <:class_expr< $ce $e >> ]
      | "simple"
        [ `ANTIQUOT ((""|"cexp"|"anti" as n),s) ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_expr_tag
        | class_longident_and_param{ce} -> ce
        | "object"; opt_class_self_patt{csp}; class_structure{cst}; "end" ->
            <:class_expr< object ($csp) $cst end >>
        | "("; SELF{ce}; ":"; class_type{ct}; ")" ->
            <:class_expr< ($ce : $ct) >>
        | "("; SELF{ce}; ")" -> ce ] ]
    class_longident_and_param:
      [ [ class_longident{ci}; "["; comma_ctyp{t}; "]" ->
          <:class_expr< $id:ci [ $t ] >>
        | class_longident{ci} -> <:class_expr< $id:ci >>  ] ]
    class_structure:
      [ [ `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s) ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s); semi; SELF{cst} ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s); $cst >>
        | LIST0 [ class_str_item{cst}; semi -> cst ]{l} -> Ast.crSem_of_list l  ] ]
    opt_class_self_patt:
      [ [ "("; patt{p}; ")" -> p
        | "("; patt{p}; ":"; ctyp{t}; ")" -> <:patt< ($p : $t) >>
        | -> <:patt<>> ] ]
    class_str_item:
      [ LA
        [ `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s) ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_str_item_tag
        | "inherit"; opt_override{o}; class_expr{ce}; opt_as_lident{pb} ->
            <:class_str_item< inherit $override:o $ce as $pb >>
        | value_val_opt_override{o}; opt_mutable{mf}; label{lab}; cvalue_binding{e}
          ->
            <:class_str_item< val $override:o $mutable:mf $lab = $e >>
        | value_val_opt_override{o}; opt_mutable{mf}; "virtual"; label{l}; ":";
              poly_type{t} ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< val virtual $mutable:mf $l : $t >>
        | value_val_opt_override{o}; "virtual"; opt_mutable{mf}; label{l}; ":";
                poly_type{t} ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< val virtual $mutable:mf $l : $t >>
        | method_opt_override{o}; "virtual"; opt_private{pf}; label{l}; ":";
                poly_type{t} ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf $l : $t >>
        | method_opt_override{o}; opt_private{pf}; label{l}; opt_polyt{topt};
                fun_binding{e} ->
            <:class_str_item< method $override:o $private:pf $l : $topt = $e >>
        | method_opt_override{o}; opt_private{pf}; "virtual"; label{l}; ":";
             poly_type{t} ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf $l : $t >>
        | type_constraint; ctyp{t1}; "="; ctyp{t2} ->
            <:class_str_item< type $t1 = $t2 >>
        | "initializer"; expr{se} -> <:class_str_item< initializer $se >> ] ]
    method_opt_override:
      [ [ "method"; "!" -> <:override_flag< ! >>
        | "method"; `ANTIQUOT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | "method" -> <:override_flag<>>  ] ]
    value_val_opt_override:
      [ [ "val"; "!" -> <:override_flag< ! >>
        | "val"; `ANTIQUOT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | "val" -> <:override_flag<>>   ] ]
    opt_as_lident:
      [ [ "as"; a_LIDENT{i} -> i
        | -> ""  ] ]
    opt_polyt:
      [ [ ":"; poly_type{t} -> t
        | -> <:ctyp<>> ] ]
    cvalue_binding:
      [ [ "="; expr{e} -> e
        | ":"; "type"; unquoted_typevars{t1}; "." ; ctyp{t2} ; "="; expr{e} -> 
	(* let u = Ast.TyTypePol _loc t1 t2 in *)
         let u = <:ctyp< ! $t1 . $t2 >> in   
         <:expr< ($e : $u) >>
        | ":"; poly_type{t}; "="; expr{e} -> <:expr< ($e : $t) >>
        | ":"; poly_type{t}; ":>"; ctyp{t2}; "="; expr{e} ->
            match t with
            [ <:ctyp< ! $_ . $_ >> -> raise (Stream.Error "unexpected polytype here")
            | _ -> <:expr< ($e : $t :> $t2) >> ]
        | ":>"; ctyp{t}; "="; expr{e} -> <:expr< ($e :> $t) >> ] ]
    label:
      [ [ a_LIDENT{i} -> i ] ]
    class_type:
      [ [ `ANTIQUOT ((""|"ctyp"|"anti" as n),s) ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | class_type_longident_and_param{ct} -> ct
        | "object"; opt_class_self_type{cst}; class_signature{csg}; "end" ->
            <:class_type< object ($cst) $csg end >> ] ]
    class_type_longident_and_param:
      [ [ class_type_longident{i}; "["; comma_ctyp{t}; "]" ->
            <:class_type< $id:i [ $t ] >>
        | class_type_longident{i} -> <:class_type< $id:i >> ] ]
    class_type_plus:
      [ [ "["; ctyp{t}; "]"; "->"; SELF{ct} ->
        <:class_type< [ $t ] -> $ct >>
        | class_type{ct} -> ct ] ]
    opt_class_self_type:
      [ [ "("; ctyp{t}; ")" -> t
        | -> <:ctyp<>> ] ]
    class_signature:
      [ [ `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s) ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s); semi; SELF{csg} ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s); $csg >>
        | LIST0 [ class_sig_item{csg}; semi -> csg ]{l} ->
            Ast.cgSem_of_list l  ] ]
    class_sig_item:
      [ [ `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s) ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_sig_item_tag
        | "inherit"; class_type{cs} ->
            <:class_sig_item< inherit $cs >>
        | "val"; opt_mutable{mf}; opt_virtual{mv};
          label{l}; ":"; ctyp{t} ->
            <:class_sig_item< val $mutable:mf $virtual:mv $l : $t >>
        | "method"; "virtual"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | "method"; opt_private{pf}; label{l}; ":"; poly_type{t} ->
            <:class_sig_item< method $private:pf $l : $t >>
        | "method"; opt_private{pf}; "virtual"; label{l}; ":"; poly_type{t} ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | type_constraint; ctyp{t1}; "="; ctyp{t2} ->
            <:class_sig_item< type $t1 = $t2 >> ] ]
    type_constraint:
      [ [ "type" | "constraint" -> () ] ]
    class_description:
      [ [ SELF{cd1}; "and"; SELF{cd2} ->
            <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT ((""|"typ"|"anti"|"list" as n),s) ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x DynAst.class_type_tag
        | class_info_for_class_type{ci}; ":"; class_type_plus{ct} ->
            <:class_type< $ci : $ct >>  ] ]
    class_type_declaration:
      [ LA
        [ SELF{cd1}; "and"; SELF{cd2} ->
          <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT ((""|"typ"|"anti"|"list" as n),s) ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.class_type_tag
        | class_info_for_class_type{ci}; "="; class_type{ct} ->
            <:class_type< $ci = $ct >> ] ]
    field_expr_list:
      [ [ field_expr{b1}; ";"; SELF{b2} -> <:rec_binding< $b1 ; $b2 >>
        | field_expr{b1}; ";"            -> b1
        | field_expr{b1}                 -> b1
      ] ]
    field_expr:
      [ [ `ANTIQUOT ((""|"bi"|"anti" as n),s) ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | label{l}; "=";  expr Level "top"{e} ->
            <:rec_binding< $lid:l = $e >> ] ]
    meth_list:
      [ [ meth_decl{m}; ";"; SELF{(ml, v) }  -> (<:ctyp< $m; $ml >>, v)
        | meth_decl{m}; ";"; opt_dot_dot{v} -> (m, v)
        | meth_decl{m}; opt_dot_dot{v}      -> (m, v)
      ] ]
    meth_decl:
      [ [ `ANTIQUOT ((""|"typ" as n),s)        -> <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s)          -> <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x                       -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_LIDENT{lab}; ":"; poly_type{t} -> <:ctyp< $lid:lab : $t >> ] ]
    opt_meth_list:
      [ [ meth_list{(ml, v) } -> <:ctyp< < $ml $(..:v) > >>
        | opt_dot_dot{v}     -> <:ctyp< < $(..:v) > >>
      ] ]
    poly_type:
      [ [ ctyp{t} -> t ] ]
    package_type:
      [ [ module_type{p} -> p ] ]
    typevars:
      [ LA
        [ SELF{t1}; SELF{t2} -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | "'"; a_ident{i} -> <:ctyp< '$lid:i >> ] ]
    unquoted_typevars:
      [ LA
        [ SELF{t1}; SELF{t2} -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x DynAst.ctyp_tag
        | a_ident{i} -> <:ctyp< $lid:i >>
      ] ]
    row_field:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT (("list" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp|" n s) >>
        | SELF{t1}; "|"; SELF{t2} -> <:ctyp< $t1 | $t2 >>
        | "`"; a_ident{i} -> <:ctyp< `$i >>
        | "`"; a_ident{i}; "of"; "&"; amp_ctyp{t} -> <:ctyp< `$i of & $t >>
        | "`"; a_ident{i}; "of"; amp_ctyp{t} -> <:ctyp< `$i of $t >>
        | ctyp{t} -> t ] ]
    amp_ctyp:
      [ [ SELF{t1}; "&"; SELF{t2} -> <:ctyp< $t1 & $t2 >>
        | `ANTIQUOT (("list" as n),s) -> <:ctyp< $(anti:mk_anti ~c:"ctyp&" n s) >>
        | ctyp{t} -> t
      ] ]
    name_tags:
      [ [ `ANTIQUOT ((""|"typ" as n),s) ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | SELF{t1}; SELF{t2} -> <:ctyp< $t1 $t2 >>
        | "`"; a_ident{i} -> <:ctyp< `$i >>  ] ]
    eq_expr:
      [ [ "="; expr{e} -> fun i p -> <:patt< ? $i : ($p = $e) >>
        | -> fun i p -> <:patt< ? $i : ($p) >> ] ]
    patt_tcon:
      [ [ patt{p}; ":"; ctyp{t} -> <:patt< ($p : $t) >>
        | patt{p} -> p ] ]
    ipatt:
      [ [ `LABEL i; SELF{p} -> <:patt< ~ $i : $p >>
        | "~"; `ANTIQUOT ((""|"lid" as n),i); ":"; SELF{p} ->
            <:patt< ~ $(mk_anti n i) : $p >>
        | "~"; `ANTIQUOT ((""|"lid" as n),i) -> <:patt< ~ $(mk_anti n i) >>
        | "~"; `LIDENT i -> <:patt< ~ $i >>
        (* | opt_label{i}; "("; ipatt_tcon{p}; ")" ->
            <:patt< ? $i$ : ($p$) >>
        | opt_label{i}; "("; ipatt_tcon{p}; "="; expr{e}; ")" ->
            <:patt< ? $i$ : ($p$ = $e$) >>                             *)
        | `OPTLABEL i; "("; ipatt_tcon{p}; eq_expr{f}; ")" -> f i p
        | "?"; `ANTIQUOT ((""|"lid" as n),i); ":"; "("; ipatt_tcon{p};
          eq_expr{f}; ")" -> f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i >>
        | "?"; `ANTIQUOT ((""|"lid" as n),i) -> <:patt< ? $(mk_anti n i) >>
        | "?"; "("; ipatt_tcon{p}; ")" ->
            <:patt< ? ($p) >>
        | "?"; "("; ipatt_tcon{p}; "="; expr{e}; ")" ->
            <:patt< ? ($p = $e) >> ] ]
    ipatt_tcon:
      [ [ ipatt{p}; ":"; ctyp{t} -> <:patt< ($p : $t) >>
        | ipatt{p} -> p ] ]
    direction_flag:
      [ [ "to" -> <:direction_flag< to >>
        | "downto" -> <:direction_flag< downto >>
        | `ANTIQUOT (("to"|"anti" as n),s) -> Ast.DiAnt (mk_anti n s) ] ]
    opt_private:
      [ [ "private" -> <:private_flag< private >>
        | `ANTIQUOT (("private"|"anti" as n),s) -> Ast.PrAnt (mk_anti n s)
        | -> <:private_flag<>>  ] ]
    opt_mutable:
      [ [ "mutable" -> <:mutable_flag< mutable >>
        | `ANTIQUOT (("mutable"|"anti" as n),s) -> Ast.MuAnt (mk_anti n s)
        | -> <:mutable_flag<>>  ] ]
    opt_virtual:
      [ [ "virtual" -> <:virtual_flag< virtual >>
        | `ANTIQUOT (("virtual"|"anti" as n),s) -> Ast.ViAnt (mk_anti n s)
        | -> <:virtual_flag<>>  ] ]
    opt_dot_dot:
      [ [ ".." -> <:row_var_flag< .. >>
        | `ANTIQUOT ((".."|"anti" as n),s) -> Ast.RvAnt (mk_anti n s)
        | -> <:row_var_flag<>>  ] ]
    opt_rec:
      [ [ "rec" -> <:rec_flag< rec >>
        | `ANTIQUOT (("rec"|"anti" as n),s) -> Ast.ReAnt (mk_anti n s)
        | -> <:rec_flag<>> ] ]
    opt_override:
      [ [ "!" -> <:override_flag< ! >>
        | `ANTIQUOT ((("!"|"override"|"anti") as n),s) -> Ast.OvAnt (mk_anti n s)
        | -> <:override_flag<>> ] ]
    opt_expr:
      [ [ expr{e} -> e
        | -> <:expr<>> ] ]
    interf:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp}; semi ->
            ([ <:sig_item< # $n $dp >> ], stopped_at _loc)
          (* Ast.SgDir(_loc,n,dp), stopped is of type FanLoc.t option *)
        | sig_item{si}; semi;  SELF{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    sig_items:
      [ [ `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s) ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s) >>
        | `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s); semi; SELF{sg} ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s); $sg >> 
        | LIST0 [ sig_item{sg}; semi -> sg ]{l} -> Ast.sgSem_of_list l  ] ]
    implem:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp}; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | str_item{si}; semi;  SELF{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    str_items:
      [ [ `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s) ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s) >>
        | `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s); semi; SELF{st} ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s); $st >>
        | LIST0 [ str_item{st}; semi -> st ]{l} -> Ast.stSem_of_list l  ] ]
    top_phrase:
      [ [ phrase{ph} -> Some ph
        | `EOI -> None ] ]
    use_file:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp}; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | str_item{si}; semi;  SELF{(sil, stopped)} -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    phrase:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp}; ";;" -> (* directive to be the same as normal syntax*)
            <:str_item< # $n $dp >>
        | str_item{st}; semi -> st  ] ]
    a_INT:
      [ [ `ANTIQUOT ((""|"int"|"`int" as n),s) -> mk_anti n s
        | `INT (_, s) -> s ] ]
    a_INT32:
      [ [ `ANTIQUOT ((""|"int32"|"`int32" as n),s) -> mk_anti n s
        | `INT32 (_, s) -> s ] ]
    a_INT64:
      [ [ `ANTIQUOT ((""|"int64"|"`int64" as n),s) -> mk_anti n s
        | `INT64 (_, s) -> s ] ]
    a_NATIVEINT:
      [ [ `ANTIQUOT ((""|"nativeint"|"`nativeint" as n),s) -> mk_anti n s
        | `NATIVEINT (_, s) -> s ] ]
    a_FLOAT:
      [ [ `ANTIQUOT ((""|"flo"|"`flo" as n),s) -> mk_anti n s
        | `FLOAT (_, s) -> s ] ]
    a_CHAR:
      [ [ `ANTIQUOT ((""|"chr"|"`chr" as n),s) -> mk_anti n s
        | `CHAR (_, s) -> s ] ]
    a_UIDENT:
      [ [ `ANTIQUOT ((""|"uid" as n),s) -> mk_anti n s
        | `UIDENT s -> s ] ]
    a_LIDENT:
      [ [ `ANTIQUOT ((""|"lid" as n),s) -> mk_anti n s
        | `LIDENT s -> s ] ]
    a_LABEL:
      [ [ "~"; `ANTIQUOT (("" as n),s); ":" -> mk_anti n s
        | `LABEL s -> s ] ]
    a_OPTLABEL:
      [ [ "?"; `ANTIQUOT (("" as n),s); ":" -> mk_anti n s
        | `OPTLABEL s -> s ] ]
    a_STRING:
      [ [ `ANTIQUOT ((""|"str"|"`str" as n),s) -> mk_anti n s
        | `STRING (_, s) -> s ] ]
    string_list:
      [ [ `ANTIQUOT ((""|"str_list"),s) -> Ast.LAnt (mk_anti "str_list" s)
        | `STRING (_, x); string_list{xs} -> Ast.LCons x xs
        | `STRING (_, x) -> Ast.LCons x Ast.LNil ] ]
    semi:
      [ [ ";" -> () ] ] 
    expr_quot:
      [ [ expr{e1}; ","; comma_expr{e2} -> <:expr< $e1, $e2 >>
        | expr{e1}; ";"; sem_expr{e2} -> <:expr< $e1; $e2 >>
        | expr{e} -> e
        | -> <:expr<>> ] ]
    patt_quot:
      [ [ patt{x}; ","; comma_patt{y} -> <:patt< $x, $y >>
        | patt{x}; ";"; sem_patt{y} -> <:patt< $x; $y >>
        | patt{x}; "="; patt{y} ->
            let i =
              match x with
              [ <:patt@loc< $anti:s >> -> <:ident@loc< $anti:s >>
              | p -> Ast.ident_of_patt p ]
            in
            <:patt< $i = $y >>
        | patt{x} -> x
        | -> <:patt<>>
      ] ]
    ctyp_quot:
      [ [ more_ctyp{x}; ","; comma_ctyp{y} -> <:ctyp< $x, $y >>
        | more_ctyp{x}; ";"; label_declaration_list{y} -> <:ctyp< $x; $y >>
        | more_ctyp{x}; "|"; constructor_declarations{y} -> <:ctyp< $x | $y >>
        | more_ctyp{x}; "of"; constructor_arg_list{y} -> <:ctyp< $x of $y >>
        | more_ctyp{x}; "of"; constructor_arg_list{y}; "|"; constructor_declarations{z} ->
            <:ctyp< $(<:ctyp< $x of $y >> ) | $z >>
        | more_ctyp{x}; "of"; "&"; amp_ctyp{y} -> <:ctyp< $x of & $y >>
        | more_ctyp{x}; "of"; "&"; amp_ctyp{y}; "|"; row_field{z} ->
            <:ctyp< $(<:ctyp< $x of & $y >> ) | $z >>
        | more_ctyp{x}; ":"; more_ctyp{y} -> <:ctyp< $x : $y >>
        | more_ctyp{x}; ":"; more_ctyp{y}; ";"; label_declaration_list{z} ->
            <:ctyp< $(<:ctyp< $x : $y >> ) ; $z >>
        | more_ctyp{x}; "*"; star_ctyp{y} -> <:ctyp< $x * $y >>
        | more_ctyp{x}; "&"; amp_ctyp{y} -> <:ctyp< $x & $y >>
        | more_ctyp{x}; "and"; constructor_arg_list{y} -> <:ctyp< $x and $y >>
        | more_ctyp{x} -> x
        | -> <:ctyp<>>  ] ]
    more_ctyp:
      [ [ "mutable"; SELF{x} -> <:ctyp< mutable $x >>
        | "`"; a_ident{x} -> <:ctyp< `$x >>
        | ctyp{x} -> x
        | type_parameter{x} -> x
      ] ]
    str_item_quot:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp} -> <:str_item< # $n $dp >>
        | str_item{st1}; semi; SELF{st2} ->
            match st2 with
            [ <:str_item<>> -> st1
            | _ -> <:str_item< $st1; $st2 >> ]
        | str_item{st} -> st
        | -> <:str_item<>> ] ]
    sig_item_quot:
      [ [ "#"; a_LIDENT{n}; opt_expr{dp} -> <:sig_item< # $n $dp >>
        | sig_item{sg1}; semi; SELF{sg2} ->
            match sg2 with
            [ <:sig_item<>> -> sg1
            | _ -> <:sig_item< $sg1; $sg2 >> ]
        | sig_item{sg} -> sg
        | -> <:sig_item<>> ] ]
    module_type_quot:
      [ [ module_type{x} -> x
        | -> <:module_type<>> ] ]
    module_expr_quot:
      [ [ module_expr{x} -> x
        | -> <:module_expr<>> ] ]
    match_case_quot:
      [ [ LIST0 match_case0 SEP "|"{x} -> <:match_case< $list:x >>
        | -> <:match_case<>> ] ]
    binding_quot:
      [ [ binding{x} -> x
        | -> <:binding<>> ] ]
    rec_binding_quot:
      [ [ label_expr_list{x} -> x
        | -> <:rec_binding<>> ] ]
    module_binding_quot:
      [ [ SELF{b1}; "and"; SELF{b2} ->
            <:module_binding< $b1 and $b2 >>
        | `ANTIQUOT (("module_binding"|"anti" as n),s) ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT (("" as n),s) ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT (("" as n),m); ":"; module_type{mt} ->
            <:module_binding< $(mk_anti n m) : $mt >>
        | `ANTIQUOT (("" as n),m); ":"; module_type{mt}; "="; module_expr{me} ->
            <:module_binding< $(mk_anti n m) : $mt = $me >>
        | a_UIDENT{m}; ":"; module_type{mt} ->
            <:module_binding< $m : $mt >>
        | a_UIDENT{m}; ":"; module_type{mt}; "="; module_expr{me} ->
            <:module_binding< $m : $mt = $me >>
        | -> <:module_binding<>> ] ]
    ident_quot:
      [ "apply"
        [ SELF{i}; SELF{j} -> <:ident< $i $j >> ]
      | "."
        [ SELF{i}; "."; SELF{j} -> <:ident< $i.$j >> ]
      | "simple"
        [ `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | a_UIDENT{i} -> <:ident< $uid:i >>
        | a_LIDENT{i} -> <:ident< $lid:i >>
        | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s); "."; SELF{i} ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | "("; SELF{i}; ")" -> i  ] ]
    class_expr_quot:
      [ [ SELF{ce1}; "and"; SELF{ce2} -> <:class_expr< $ce1 and $ce2 >>
        | SELF{ce1}; "="; SELF{ce2} -> <:class_expr< $ce1 = $ce2 >>
        | "virtual";   class_name_and_param{(i, ot)} ->
            <:class_expr< virtual $lid:i [ $ot ] >>
        | `ANTIQUOT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
            <:class_expr< $virtual:anti $id:i [ $ot ] >>
        | class_expr{x} -> x
        | -> <:class_expr<>> ] ]
    class_type_quot:
      [ [ SELF{ct1}; "and"; SELF{ct2} -> <:class_type< $ct1 and $ct2 >>
        | SELF{ct1}; "="; SELF{ct2} -> <:class_type< $ct1 = $ct2 >>
        | SELF{ct1}; ":"; SELF{ct2} -> <:class_type< $ct1 : $ct2 >>
        | "virtual";  class_name_and_param{(i, ot)} ->
            <:class_type< virtual $lid:i [ $ot ] >>
        | `ANTIQUOT (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
            <:class_type< $virtual:anti $id:i [ $ot ] >>
        | class_type_plus{x} -> x
        | -> <:class_type<>>   ] ]
    class_str_item_quot:
      [ [ class_str_item{x1}; semi; SELF{x2} ->
          match x2 with
          [ <:class_str_item<>> -> x1
          | _ -> <:class_str_item< $x1; $x2 >> ]
        | class_str_item{x} -> x
        | -> <:class_str_item<>> ] ]
    class_sig_item_quot:
      [ [ class_sig_item{x1}; semi; SELF{x2} ->
          match x2 with
          [ <:class_sig_item<>> -> x1
          | _ -> <:class_sig_item< $x1; $x2 >> ]
        | class_sig_item{x} -> x
        | -> <:class_sig_item<>> ] ]
    with_constr_quot:
      [ [ with_constr{x} -> x
        | -> <:with_constr<>> ] ]
    rec_flag_quot: [ [ opt_rec{x} -> x ] ]
    direction_flag_quot: [ [ direction_flag{x} -> x ] ]
    mutable_flag_quot: [ [ opt_mutable{x} -> x ] ]
    private_flag_quot: [ [ opt_private{x} -> x ] ]
    virtual_flag_quot: [ [ opt_virtual{x} -> x ] ]
    row_var_flag_quot: [ [ opt_dot_dot{x} -> x ] ]
    override_flag_quot: [ [ opt_override{x} -> x ] ]
    patt_eoi:
      [ [ patt{x}; `EOI -> x ] ]
    expr_eoi:
      [ [ expr{x}; `EOI -> x ] ]
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
      [ [ "parser";  OPT parser_ipatt{po}; parser_case_list{pcl} ->
            cparser _loc po pcl
        | "match"; sequence{e}; "with"; "parser";  OPT parser_ipatt{po};
          parser_case_list{pcl} ->
            cparser_match _loc e po pcl ] ]
    parser_ipatt:
      [ [ a_LIDENT{i} -> <:patt< $lid:i >>  | "_" -> <:patt< _ >>  ] ]        
    parser_case_list:
      [ [ "["; LIST0 parser_case SEP "|"{pcl}; "]" -> pcl
        | parser_case{pc} -> [pc] ] ]
    parser_case:
      [ [ stream_begin; stream_patt{sp}; stream_end; OPT parser_ipatt{po}; "->"; expr{e}
          ->   (sp, po, e) ] ]
    stream_begin: [ [ "[<" -> () ] ] stream_end: [ [ ">]" -> () ] ]
    stream_quot:  [ [ "'" -> () ] ]
    stream_expr:  [ [ expr{e} -> e ] ]
    stream_patt:
      [ [ stream_patt_comp{spc} -> [(spc, None)]
        | stream_patt_comp{spc}; ";"; stream_patt_comp_err_list{sp}
          ->    [(spc, None) :: sp]
        | -> [] ] ]
    (* stream_patt_comp: (\* FIXME here *\) *)
    (*   [ [ stream_quot; patt{p}; eo = OPT [ "when"; e = stream_expr -> e ] *)
    (*       ->  SpTrm _loc p eo *)
    (*     | p = patt; "="; e = stream_expr -> SpNtr _loc p e *)
    (*     | p = patt -> SpStr _loc p ] ] *)
    stream_patt_comp: (* FIXME here *)
      [ [ patt{p}; OPT [ "when"; stream_expr{e} -> e ]{eo}
          ->  SpTrm _loc p eo
        | patt{p}; "="; stream_expr{e} -> SpNtr _loc p e
        | stream_quot; patt{p} -> SpStr _loc p ] ]
        
    stream_patt_comp_err:
      [ [ stream_patt_comp{spc};  OPT [ "??"; stream_expr{e} -> e ]{eo }
          ->  (spc, eo) ] ]
    stream_patt_comp_err_list:
      [ [ stream_patt_comp_err{spc} -> [spc]
        | stream_patt_comp_err{spc}; ";" -> [spc]
        | stream_patt_comp_err{spc}; ";"; stream_patt_comp_err_list{sp} ->
            [spc :: sp] ] ]
    expr: Level "simple"
      [ [ stream_begin; stream_end -> <:expr< [< >] >>
        | stream_begin; stream_expr_comp_list{sel}; stream_end
          ->  cstream _loc sel] ]
    stream_expr_comp_list:
      [ [ stream_expr_comp{se}; ";"; stream_expr_comp_list{sel} -> [se :: sel]
        | stream_expr_comp{se}; ";" -> [se]
        | stream_expr_comp{se} -> [se] ] ]
    (* stream_expr_comp: (\* FIXME *\) *)
    (*   [ [ stream_quot; stream_expr{e} -> SeTrm _loc e *)
    (*     | stream_expr{e} -> SeNtr _loc e ] ] *)
    stream_expr_comp: (* FIXME *)
      [ [  stream_expr{e} -> SeTrm _loc e
        | stream_quot;stream_expr{e} -> SeNtr _loc e ] ]
        
  END;

end;
  
module IdQuotationCommon = struct (* FIXME unused here *)
  let name = "Camlp4QuotationCommon";
  let version = Sys.ocaml_version;
end;

module MakeQuotationCommon (Syntax : Sig.Camlp4Syntax)
            (TheAntiquotSyntax : Sig.ParserExpr)
= struct
  (* open FanSig; *)
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
            [ [  entry{x}; `EOI -> x ] ]
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

let pa_r  = "Camlp4OCamlRevisedParser";    
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
  let module Gram = Grammar.Static(* .Make P.Lexer *) in
  let module M1 = OCamlInitSyntax.Make Gram(* P.Gram *) in
  let module M2 = MakeRevisedParser M1 in
  let module M3 = MakeQuotationCommon M2 P.Syntax.AntiquotSyntax in ();

let pa_l  (module P: Sig.PRECAST) =
  P.syntax_extension (module IdListComprehension) (module MakeListComprehension);


(* load debug parser for bootstrapping *)
let pa_debug (module P: Sig.PRECAST) =
  P.syntax_extension (module IdDebugParser) (module MakeDebugParser);



