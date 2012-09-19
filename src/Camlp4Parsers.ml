open Camlp4;
open FanSig;
module IdAstLoader = struct
  value name = "Camlp4AstLoader";
  value version = Sys.ocaml_version;
end;

module MakeAstLoader (Ast : Sig.Ast) : (Sig.Parser Ast).S= struct
  module Ast = Ast;

  value parse ast_magic ?directive_handler:(_) _loc strm =
    let str =
      let buf = Buffer.create 2047 in
      let () = Stream.iter (Buffer.add_char buf) strm
      in Buffer.contents buf in
    let magic_len = String.length ast_magic in
    let buffer = String.create magic_len in
    do {
      String.blit str 0 buffer 0 magic_len;
      if buffer = ast_magic then ()
      else failwith (Format.sprintf "Bad magic: %S vs %S" buffer ast_magic);
      Marshal.from_string str magic_len;
    };

  value parse_implem = parse FanConfig.camlp4_ast_impl_magic_number;
  value parse_interf = parse FanConfig.camlp4_ast_intf_magic_number;
end;


module IdDebugParser = struct
  value name = "Camlp4DebugParser";
  value version = Sys.ocaml_version;
end;

module MakeDebugParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module StringSet = Set.Make String;

  value debug_mode =
    try
      let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
      let rec loop acc i =
        try
          let pos = String.index_from str i ':' in
          loop (StringSet.add (String.sub str i (pos - i)) acc) (pos + 1)
        with
        [ Not_found ->
            StringSet.add (String.sub str i (String.length str - i)) acc ] in
      let sections = loop StringSet.empty 0 in
      if StringSet.mem "*" sections then fun _ -> True
      else fun x -> StringSet.mem x sections
    with [ Not_found -> fun _ -> False ];

  value rec apply accu =
    fun
    [ [] -> accu
    | [x :: xs] ->
        let _loc = Ast.loc_of_expr x
        in apply <:expr< $accu $x >> xs ];

  value mk_debug_mode _loc = fun [ None -> <:expr< Debug.mode >>
                                 | Some m -> <:expr< $uid:m.Debug.mode >> ];

  value mk_debug _loc m fmt section args =
    let call = apply <:expr< Debug.printf $str:section $str:fmt >> args in
      <:expr< if $(mk_debug_mode _loc m) $str:section then $call else () >>;


  EXTEND Gram
    GLOBAL: expr;
    expr:
    [ [ m = start_debug; section = LIDENT; fmt = STRING;
        args = LIST0 expr LEVEL "."; x = end_or_in ->
      match (x, debug_mode section) with
      [ (None,   False) -> <:expr< () >>
      | (Some e, False) -> e
      | (None, _) -> mk_debug _loc m fmt section args
      | (Some e, _) -> <:expr< let () = $(mk_debug _loc m fmt section args) in $e >> ]
    ] ];
    end_or_in:
    [ [ "end" -> None
      | "in"; e = expr -> Some e
    ] ];
    start_debug:
    [ [ LIDENT "debug" -> None
      | LIDENT "camlp4_debug" -> Some "Camlp4"
    ] ];
  END;

end;

module IdGrammarParser = struct
  value name = "Camlp4GrammarParser";
  value version = Sys.ocaml_version;
end;

module MakeGrammarParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module MetaLoc = Ast.Meta.MetaGhostLoc;
  module MetaAst = Ast.Meta.Make MetaLoc;
  (* module PP = Camlp4.Printers.OCaml.Make Syntax; *)
  (* value pp = new PP.printer ~comments:False (); *)
  module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make Syntax.Ast;
  value string_of_patt patt =
    let buf = Buffer.create 42 in
    let () =
      Format.bprintf buf "%a@?"
        (fun fmt p -> Pprintast.pattern fmt (Ast2pt.patt p)) patt in
    (* let () = Format.bprintf buf "%a@?" pp#patt patt in *)
    let str = Buffer.contents buf in
    if str = "" then assert False else str;

  value split_ext = ref False;

  type loc = Loc.t;

  type name 'e = { expr : 'e; tvar : string; loc : loc };

  type styp =
    [ STlid of loc and string
    | STapp of loc and styp and styp
    | STquo of loc and string
    | STself of loc and string
    | STtok of loc
    | STstring_tok of loc
    | STtyp of Ast.ctyp ]
  ;

  type text 'e 'p =
    [ TXmeta of loc and string and list (text 'e 'p) and 'e and styp
    | TXlist of loc and bool and symbol 'e 'p and option (symbol 'e 'p)
    | TXnext of loc
    | TXnterm of loc and name 'e and option string
    | TXopt of loc and text 'e 'p
    | TXtry of loc and text 'e 'p
    | TXrules of loc and list (list (text 'e 'p) * 'e)
    | TXself of loc
    | TXkwd of loc and string
    | TXtok of loc and 'e and string
         (** The first is the match function expr,
             the second is the string description.
             The description string will be used for
             grammar insertion and left factoring.
             Keep this string normalized and well comparable. *) ]
  and entry 'e 'p =
    { name : name 'e; pos : option 'e; levels : list (level 'e 'p) }
  and level 'e 'p =
    { label : option string; assoc : option 'e; rules : list (rule 'e 'p) }
  and rule 'e 'p = { prod : list (symbol 'e 'p); action : option 'e }
  and symbol 'e 'p = { used : list string; text : text 'e 'p;
                       styp : styp; pattern : option 'p }
  ;

  type used = [ Unused | UsedScanned | UsedNotScanned ];

  value _loc = Loc.ghost;
  value gm = "Camlp4Grammar__";

  value mark_used modif ht n =
    try
      let rll = Hashtbl.find_all ht n in
      List.iter
        (fun (r, _) ->
          if r.val == Unused then do {
            r.val := UsedNotScanned; modif.val := True;
          }
          else ())
        rll
    with
    [ Not_found -> () ]
  ;

  value rec mark_symbol modif ht symb =
    List.iter (fun e -> mark_used modif ht e) symb.used
  ;

  value check_use nl el =
    let ht = Hashtbl.create 301 in
    let modif = ref False in
    do {
      List.iter
        (fun e ->
          let u =
            match e.name.expr with
            [ <:expr< $lid:_ >> -> Unused
            | _ -> UsedNotScanned ]
          in
          Hashtbl.add ht e.name.tvar (ref u, e))
        el;
      List.iter
        (fun n ->
          try
            let rll = Hashtbl.find_all ht n.tvar in
            List.iter (fun (r, _) -> r.val := UsedNotScanned) rll
          with _ ->
            ())
        nl;
      modif.val := True;
      while modif.val do {
        modif.val := False;
        Hashtbl.iter
          (fun _ (r, e) ->
            if r.val = UsedNotScanned then do {
              r.val := UsedScanned;
              List.iter
                (fun level ->
                    let rules = level.rules in
                    List.iter
                      (fun rule ->
                        List.iter (fun s -> mark_symbol modif ht s)
                          rule.prod)
                      rules)
                e.levels
            }
            else ())
          ht
      };
      Hashtbl.iter
        (fun s (r, e) ->
          if r.val = Unused then
            print_warning e.name.loc ("Unused local entry \"" ^ s ^ "\"")
          else ())
        ht;
    }
  ;

  value new_type_var =
    let i = ref 0 in fun () -> do { incr i; "e__" ^ string_of_int i.val }
  ;

  value used_of_rule_list rl =
    List.fold_left
      (fun nl r -> List.fold_left (fun nl s -> s.used @ nl) nl r.prod) []
      rl
  ;

  value retype_rule_list_without_patterns _loc rl =
    try
      List.map
        (fun
          (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Gram.Token.extract_string x); ... *)
        [ {prod = [({pattern = None; styp = STtok _} as s)]; action = None} ->
            {prod = [{ (s) with pattern = Some <:patt< x >> }];
              action = Some <:expr< $uid:gm.Token.extract_string x >>}
          (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
        | {prod = [({pattern = None} as s)]; action = None} ->
            {prod = [{ (s) with pattern = Some <:patt< x >> }];
              action = Some <:expr< x >>}
          (* ...; ([] -> a); ... *)
        | {prod = []; action = Some _} as r -> r
        | _ -> raise Exit ])
        rl
    with
    [ Exit -> rl ]
  ;

  value meta_action = ref False;

  value mklistexp _loc =
    loop True where rec loop top =
      fun
      [ [] -> <:expr< [] >>
      | [e1 :: el] ->
          let _loc =
            if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
          in
          <:expr< [$e1 :: $(loop False el)] >> ]
  ;

  value mklistpat _loc =
    loop True where rec loop top =
      fun
      [ [] -> <:patt< [] >>
      | [p1 :: pl] ->
          let _loc =
            if top then _loc else Loc.merge (Ast.loc_of_patt p1) _loc
          in
          <:patt< [$p1 :: $(loop False pl)] >> ]
  ;

  value rec expr_fa al =
    fun
    [ <:expr< $f $a >> -> expr_fa [a :: al] f
    | f -> (f, al) ]
  ;

  value rec make_ctyp styp tvar =
    match styp with
    [ STlid _loc s -> <:ctyp< $lid:s >>
    | STapp _loc t1 t2 -> <:ctyp< $(make_ctyp t1 tvar) $(make_ctyp t2 tvar) >>
    | STquo _loc s -> <:ctyp< '$s >>
    | STself _loc x ->
        if tvar = "" then
          Loc.raise _loc
            (Stream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else <:ctyp< '$tvar >>
    | STtok _loc -> <:ctyp< $uid:gm.Token.t >>
    | STstring_tok _loc -> <:ctyp< string >>
    | STtyp t -> t ]
  ;

  value make_ctyp_patt styp tvar patt =
    let styp = match styp with [ STstring_tok _loc -> STtok _loc | t -> t ] in
    match make_ctyp styp tvar with
    [ <:ctyp< _ >> -> patt
    | t -> let _loc = Ast.loc_of_patt patt in <:patt< ($patt : $t) >> ];

  value make_ctyp_expr styp tvar expr =
    match make_ctyp styp tvar with
    [ <:ctyp< _ >> -> expr
    | t -> let _loc = Ast.loc_of_expr expr in <:expr< ($expr : $t) >> ];

  value text_of_action _loc psl rtvar act tvar =
    let locid = <:patt< $(lid:Loc.name.val) >> in
    let act =
      match act with
      [ Some act -> act
      | None -> <:expr< () >> ]
    in
    let (tok_match_pl, act, _) =
      List.fold_left
        (fun ((tok_match_pl, act, i) as accu) ->
          fun
          [ { pattern = None } -> accu
          | { pattern = Some p } when Ast.is_irrefut_patt p -> accu
          | { pattern = Some <:patt< ($_ $(tup:<:patt< _ >>) as $lid:s) >> } ->
              (tok_match_pl,
               <:expr< let $lid:s = $uid:gm.Token.extract_string $lid:s
                       in $act >>, i)
          | { pattern = Some p; text=TXtok _ _ _ } ->
              let id = "__camlp4_"^string_of_int i in
              (Some (match (tok_match_pl) with
                     [ None -> (<:expr< $lid:id >>, p)
                     | Some (tok_pl, match_pl) ->
                        (<:expr< $lid:id, $tok_pl >>, <:patt< $p, $match_pl >>)]),
               act, succ i)
          | _ -> accu ])
        (None, act, 0) psl
    in
    let e =
      let e1 = <:expr< ($act : '$rtvar) >> in
      let e2 =
        match (tok_match_pl) with
        [ None -> e1
        | Some (<:expr< $t1, $t2 >>, <:patt< $p1, $p2 >>) ->
          <:expr< match ($t1, $t2) with
                  [ ($p1, $p2) -> $e1
                  | _ -> assert False ] >>
        | Some (tok, match_) ->
          <:expr< match $tok with
                  [ $pat:match_ -> $e1
                  | _ -> assert False ] >> ] in
      <:expr< fun ($locid : $uid:gm.Loc.t) -> $e2 >> in
    let (txt, _) =
      List.fold_left
        (fun (txt, i) s ->
          match s.pattern with
          [ None | Some <:patt< _ >> -> (<:expr< fun _ -> $txt >>, i)
          | Some <:patt< ($_ $(tup:<:patt< _ >>) as $p) >> ->
              let p = make_ctyp_patt s.styp tvar p in
              (<:expr< fun $p -> $txt >>, i)
          | Some p when Ast.is_irrefut_patt p ->
              let p = make_ctyp_patt s.styp tvar p in
              (<:expr< fun $p -> $txt >>, i)
          | Some _ ->
              let p = make_ctyp_patt s.styp tvar
                        <:patt< $(lid:"__camlp4_"^string_of_int i) >> in
              (<:expr< fun $p -> $txt >>, succ i) ])
        (e, 0) psl
    in
    let txt =
      if meta_action.val then
        <:expr< Obj.magic $(MetaAst.Expr.meta_expr _loc txt) >>
      else txt
    in
    <:expr< $uid:gm.Action.mk $txt >>
  ;

  value srules loc t rl tvar =
    List.map
      (fun r ->
        let sl = [ s.text | s <- r.prod ] in
        let ac = text_of_action loc r.prod t r.action tvar in
        (sl, ac))
      rl
  ;

  value rec make_expr entry tvar =
    fun
    [ TXmeta _loc n tl e t ->
        let el =
          List.fold_right
            (fun t el -> <:expr< [$(make_expr entry "" t) :: $el] >>)
            tl <:expr< [] >>
        in
        <:expr<
          $uid:gm.Smeta $str:n $el ($uid:gm.Action.mk $(make_ctyp_expr t tvar e)) >>
    | TXlist _loc min t ts ->
        let txt = make_expr entry "" t.text in
        match (min, ts) with
        [ (False, None) -> <:expr< $uid:gm.Slist0 $txt >>
        | (True, None) -> <:expr< $uid:gm.Slist1 $txt >>
        | (False, Some s) ->
            let x = make_expr entry tvar s.text in
            <:expr< $uid:gm.Slist0sep $txt $x >>
        | (True, Some s) ->
            let x = make_expr entry tvar s.text in
            <:expr< $uid:gm.Slist1sep $txt $x >> ]
    | TXnext _loc -> <:expr< $uid:gm.Snext >>
    | TXnterm _loc n lev ->
        match lev with
        [ Some lab ->
            <:expr<
              $uid:gm.Snterml
                ($uid:gm.Entry.obj ($(n.expr) : $uid:gm.Entry.t '$(n.tvar)))
                $str:lab >>
        | None ->
            if n.tvar = tvar then <:expr< $uid:gm.Sself >>
            else
              <:expr<
                $uid:gm.Snterm
                    ($uid:gm.Entry.obj ($(n.expr) : $uid:gm.Entry.t '$(n.tvar))) >> ]
    | TXopt _loc t -> <:expr< $uid:gm.Sopt $(make_expr entry "" t) >>
    | TXtry _loc t -> <:expr< $uid:gm.Stry $(make_expr entry "" t) >>
    | TXrules _loc rl ->
        <:expr< $uid:gm.srules $(entry.expr) $(make_expr_rules _loc entry rl "") >>
    | TXself _loc -> <:expr< $uid:gm.Sself >>
    | TXkwd _loc kwd -> <:expr< $uid:gm.Skeyword $str:kwd >>
    | TXtok _loc match_fun descr ->
        <:expr< $uid:gm.Stoken ($match_fun, $`str:descr) >> ]

  and make_expr_rules _loc n rl tvar =
    List.fold_left
      (fun txt (sl, ac) ->
        let sl =
          List.fold_right
            (fun t txt ->
                let x = make_expr n tvar t in
                <:expr< [$x :: $txt] >>)
            sl <:expr< [] >>
        in
        <:expr< [($sl, $ac) :: $txt ] >>)
      <:expr< [] >> rl
  ;

  value expr_of_delete_rule _loc n sl =
    let sl =
      List.fold_right
        (fun s e -> <:expr< [$(make_expr n "" s.text) :: $e ] >>) sl
        <:expr< [] >>
    in
    (<:expr< $(n.expr) >>, sl)
  ;

  value rec tvar_of_ident =
    fun
    [ <:ident< $lid:x >> | <:ident< $uid:x >> -> x
    | <:ident< $uid:x.$xs >> -> x ^ "__" ^ tvar_of_ident xs
    | _ -> failwith "internal error in the Grammar extension" ]
  ;

  value mk_name _loc i =
    {expr = <:expr< $id:i >>; tvar = tvar_of_ident i; loc = _loc};

  value slist loc min sep symb =
    TXlist loc min symb sep
  ;

  (*
  value sstoken _loc s =
    let n = mk_name _loc <:ident< $lid:"a_" ^ s$ >> in
    TXnterm _loc n None
  ;

  value mk_symbol p s t =
    {used = []; text = s; styp = t; pattern=Some p};

  value sslist _loc min sep s =
    let rl =
      let r1 =
        let prod =
          let n = mk_name _loc <:ident< a_list >> in
          [mk_symbol <:patt< a >> (TXnterm _loc n None) (STquo _loc "a_list")]
        in
        let act = <:expr< a >> in
        {prod = prod; action = Some act}
      in
      let r2 =
        let prod =
          [mk_symbol <:patt< a >> (slist _loc min sep s)
            (STapp _loc (STlid _loc "list") s.styp)]
        in
        let act = <:expr< Qast.List a >> in
        {prod = prod; action = Some act}
      in
      [r1; r2]
    in
    let used =
      match sep with
      [ Some symb -> symb.used @ s.used
      | None -> s.used ]
    in
    let used = ["a_list" :: used] in
    let text = TXrules _loc (srules _loc "a_list" rl "") in
    let styp = STquo _loc "a_list" in
    {used = used; text = text; styp = styp; pattern = None}
  ;

  value ssopt _loc s =
    let rl =
      let r1 =
        let prod =
          let n = mk_name _loc <:ident< a_opt >> in
          [mk_symbol <:patt< a >> (TXnterm _loc n None) (STquo _loc "a_opt")]
        in
        let act = <:expr< a >> in
        {prod = prod; action = Some act}
      in
      let r2 =
        let s =
          match s.text with
          [ TXkwd _loc _ | TXtok _loc _ _ ->
              let rl =
                [{prod = [{ (s) with pattern = Some <:patt< x >> }];
                  action = Some <:expr< Qast.Str (Token.extract_string x) >>}]
              in
              let t = new_type_var () in
              {used = []; text = TXrules _loc (srules _loc t rl "");
              styp = STquo _loc t; pattern = None}
          | _ -> s ]
        in
        let prod =
          [mk_symbol <:patt< a >> (TXopt _loc s.text)
            (STapp _loc (STlid _loc "option") s.styp)]
        in
        let act = <:expr< Qast.Option a >> in
        {prod = prod; action = Some act}
      in
      [r1; r2]
    in
    let used = ["a_opt" :: s.used] in
    let text = TXrules _loc (srules _loc "a_opt" rl "") in
    let styp = STquo _loc "a_opt" in
    {used = used; text = text; styp = styp; pattern = None}
  ;
  *)

  value text_of_entry _loc e =
    let ent =
      let x = e.name in
      let _loc = e.name.loc in
      <:expr< ($(x.expr) : $uid:gm.Entry.t '$(x.tvar)) >>
    in
    let pos =
      match e.pos with
      [ Some pos -> <:expr< Some $pos >>
      | None -> <:expr< None >> ]
    in
    let txt =
      List.fold_right
        (fun level txt ->
          let lab =
            match level.label with
            [ Some lab -> <:expr< Some $str:lab >>
            | None -> <:expr< None >> ]
          in
          let ass =
            match level.assoc with
            [ Some ass -> <:expr< Some $ass >>
            | None -> <:expr< None >> ]
          in
          let txt =
            let rl = srules _loc e.name.tvar level.rules e.name.tvar in
            let e = make_expr_rules _loc e.name rl e.name.tvar in
            <:expr< [($lab, $ass, $e) :: $txt] >>
          in
          txt)
        e.levels <:expr< [] >>
    in
    (ent, pos, txt)
  ;

  value let_in_of_extend _loc gram gl el args =
    match gl with
    [ None -> args
    | Some nl ->
        do {
          check_use nl el;
          let ll =
            let same_tvar e n = e.name.tvar = n.tvar in
            List.fold_right
              (fun e ll ->
                match e.name.expr with
                [ <:expr< $lid:_ >> ->
                    if List.exists (same_tvar e) nl then ll
                    else if List.exists (same_tvar e) ll then ll
                    else [e.name :: ll]
                | _ -> ll ])
              el []
          in
          let local_binding_of_name {expr = e; tvar = x; loc = _loc} =
            let i =
              match e with
              [ <:expr< $lid:i >> -> i
              | _ -> failwith "internal error in the Grammar extension" ]
            in <:binding< $lid:i =
                 (grammar_entry_create $str:i : $uid:gm.Entry.t '$x) >> in
          let expr_of_name {expr = e; tvar = x; loc = _loc} =
            <:expr< ($e : $uid:gm.Entry.t '$x) >> in
          let e =
            match ll with
            [ [] -> args
            | [x::xs] ->
                let locals =
                  List.fold_right
                    (fun name acc ->
                      <:binding< $acc and $(local_binding_of_name name) >>)
                    xs (local_binding_of_name x)
                in
                let entry_mk =
                  match gram with
                  [ Some g -> <:expr< $uid:gm.Entry.mk $id:g >>
                  | None   -> <:expr< $uid:gm.Entry.mk >> ]
                in <:expr<
                      let grammar_entry_create = $entry_mk in
                      let $locals in $args >> ]
          in
          match nl with
          [ [] -> e
          | [x::xs] ->
              let globals =
                List.fold_right
                  (fun name acc ->
                    <:binding< $acc and _ = $(expr_of_name name) >>)
                  xs <:binding< _ = $(expr_of_name x) >>
              in <:expr< let $globals in $e >> ]
        } ]
  ;

  class subst gmod =
    object
      inherit Ast.map as super;
      method ident =
        fun
        [ <:ident< $uid:x >> when x = gm -> gmod
        | x -> super#ident x ];
    end;

  value subst_gmod ast gmod = (new subst gmod)#expr ast;

  value text_of_functorial_extend _loc gmod gram gl el =
    let args =
      let el =
        List.map
          (fun e ->
            let (ent, pos, txt) = text_of_entry e.name.loc e in
            let e = <:expr< $uid:gm.extend $ent ((fun () -> ($pos, $txt)) ()) >> in
            if split_ext.val then <:expr< let aux () = $e in aux () >> else e)
          el
      in
      match el with
      [ [] -> <:expr< () >>
      | [e] -> e
      | [e::el] ->
          <:expr< do { $(List.fold_left
                          (fun acc x -> <:expr< $acc; $x >>) e el) } >>  ]
    in
    subst_gmod (let_in_of_extend _loc gram gl el args) gmod;

  value wildcarder = object (self)
    inherit Ast.map as super;
    method patt =
      fun
      [ <:patt@_loc< $lid:_ >> -> <:patt< _ >>
      | <:patt< ($p as $_) >> -> self#patt p
      | p -> super#patt p ];
  end;

  value mk_tok _loc p t =
    let p' = wildcarder#patt p in
    let match_fun =
      if Ast.is_irrefut_patt p' then
        <:expr< fun [ $pat:p' -> True ] >>
      else
        <:expr< fun [ $pat:p' -> True | _ -> False ] >> in
    let descr = string_of_patt p' in
    let text = TXtok _loc match_fun descr in
    {used = []; text = text; styp = t; pattern = Some p };

  value symbol = Gram.Entry.mk "symbol";

  value check_not_tok s =
    match s with
    [ {text = TXtok _loc _ _ } ->
        Loc.raise _loc (Stream.Error
          ("Deprecated syntax, use a sub rule. "^
           "LIST0 STRING becomes LIST0 [ x = STRING -> x ]"))
    | _ -> () ];

  FanConfig.antiquotations.val := True;

  EXTEND Gram
    GLOBAL: expr symbol;
    expr: AFTER "top"
      [ [ "EXTEND"; e = extend_body; "END" -> e
        | "DELETE_RULE"; e = delete_rule_body; "END" -> e
        | "GDELETE_RULE" ->
            Loc.raise _loc (Stream.Error
              "Deprecated syntax, use DELETE_RULE MyGramModule ... END instead")
        | "GEXTEND" ->
            Loc.raise _loc (Stream.Error
              "Deprecated syntax, use EXTEND MyGramModule ... END instead") ] ]
    ;
    extend_header:
      [ [ "("; i = qualid; ":"; t = t_qualid; ")" -> (Some i, t)
        | g = qualuid -> (None, g) ] ]
    ;
    extend_body:
      [ [ (gram, g) = extend_header; global_list = OPT global;
          el = LIST1 [ e = entry; semi_sep -> e ] ->
            text_of_functorial_extend _loc g gram global_list el ] ]
    ;
    delete_rule_body:
      [ [ g = qualuid; n = name; ":"; sl = LIST0 symbol SEP semi_sep ->
            let (e, b) = expr_of_delete_rule _loc n sl in
            subst_gmod <:expr< $uid:gm.delete_rule $e $b >> g ] ]
    ;
    qualuid:
      [ [ [ LIDENT | UIDENT "GLOBAL" ] ->
            Loc.raise _loc
              (Stream.Error
                    "Deprecated syntax, the grammar module is expected") ]
      | [ x = UIDENT; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | i = UIDENT -> <:ident< $uid:i >> ] ]
    ;
    qualuid:
      [ [ [ LIDENT | UIDENT "GLOBAL" ] ->
            Loc.raise _loc
              (Stream.Error
                    "Deprecated syntax, the grammar module is expected") ]
      | [ x = UIDENT; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | i = UIDENT -> <:ident< $uid:i >> ] ]
    ;
    qualid:
      [ [ x = UIDENT; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | i = UIDENT -> <:ident< $uid:i >>
        | i = LIDENT -> <:ident< $lid:i >>
      ] ]
    ;
    t_qualid:
      [ [ x = UIDENT; "."; xs = SELF -> <:ident< $uid:x.$xs >>
        | x = UIDENT; "."; `LIDENT "t" -> <:ident< $uid:x >>
        | `(LIDENT _ | UIDENT _) ->
              Loc.raise _loc (Stream.Error
                ("Wrong EXTEND header, the grammar type must finish by 't', "^
                  "like in EXTEND (g : Gram.t) ... END")) ] ]
    ;
    global:
      [ [ UIDENT "GLOBAL"; ":"; sl = LIST1 name; semi_sep -> sl ] ]
    ;
    entry:
      [ [ n = name; ":"; pos = OPT position; ll = level_list ->
            {name = n; pos = pos; levels = ll} ] ]
    ;
    position:
      [ [ UIDENT "FIRST" -> <:expr< Camlp4.Sig.Grammar.First >>
        | UIDENT "LAST" -> <:expr< Camlp4.Sig.Grammar.Last >>
        | UIDENT "BEFORE"; n = string -> <:expr< Camlp4.Sig.Grammar.Before $n >>
        | UIDENT "AFTER"; n = string -> <:expr< Camlp4.Sig.Grammar.After $n >>
        | UIDENT "LEVEL"; n = string -> <:expr< Camlp4.Sig.Grammar.Level $n >> ] ]
    ;
    level_list:
      [ [ "["; ll = LIST0 level SEP "|"; "]" -> ll ] ]
    ;
    level:
      [ [ lab = OPT [ x = STRING -> x ]; ass = OPT assoc; rules = rule_list ->
            {label = lab; assoc = ass; rules = rules} ] ]
    ;
    assoc:
      [ [ UIDENT "LEFTA" -> <:expr< Camlp4.Sig.Grammar.LeftA >>
        | UIDENT "RIGHTA" -> <:expr< Camlp4.Sig.Grammar.RightA >>
        | UIDENT "NONA" -> <:expr< Camlp4.Sig.Grammar.NonA >> ] ]
    ;
    rule_list:
      [ [ "["; "]" -> []
        | "["; rules = LIST1 rule SEP "|"; "]" ->
            retype_rule_list_without_patterns _loc rules ] ]
    ;
    rule:
      [ [ psl = LIST0 psymbol SEP semi_sep; "->"; act = expr ->
            {prod = psl; action = Some act}
        | psl = LIST0 psymbol SEP semi_sep ->
            {prod = psl; action = None} ] ]
    ;
    psymbol:
      [ [ p = LIDENT; "="; s = symbol ->
            match s.pattern with
            [ Some (<:patt< $uid:u $(tup:<:patt< _ >>) >> as p') ->
                let match_fun = <:expr< fun [ $pat:p' -> True | _ -> False ] >> in
                let p' = <:patt< ($p' as $lid:p) >> in
                let descr = u ^ " _" in
                let text = TXtok _loc match_fun descr in
                { (s) with text = text; pattern = Some p' }
            | _ -> { (s) with pattern = Some <:patt< $lid:p >> } ]
        | i = LIDENT; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
            let name = mk_name _loc <:ident< $lid:i >> in
            let text = TXnterm _loc name lev in
            let styp = STquo _loc i in
            {used = [i]; text = text; styp = styp; pattern = None}
        | p = pattern; "="; s = symbol ->
            match s.pattern with
            [ Some <:patt< $uid:u $(tup:<:patt< _ >>) >> ->
                mk_tok _loc <:patt< $uid:u $p >> s.styp
            | _ -> { (s) with pattern = Some p } ]
        | s = symbol -> s ] ]
    ;
    symbol:
      [ "top" NONA
        [ UIDENT "LIST0"; s = SELF;
          sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
            let () = check_not_tok s in
            let used =
              match sep with
              [ Some symb -> symb.used @ s.used
              | None -> s.used ]
            in
            let styp = STapp _loc (STlid _loc "list") s.styp in
            let text = slist _loc False sep s in
            {used = used; text = text; styp = styp; pattern = None}
        | UIDENT "LIST1"; s = SELF;
          sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
            let () = check_not_tok s in
            let used =
              match sep with
              [ Some symb -> symb.used @ s.used
              | None -> s.used ]
            in
            let styp = STapp _loc (STlid _loc "list") s.styp in
            let text = slist _loc True sep s in
            {used = used; text = text; styp = styp; pattern = None}
        | UIDENT "OPT"; s = SELF ->
            let () = check_not_tok s in
            let styp = STapp _loc (STlid _loc "option") s.styp in
            let text = TXopt _loc s.text in
            {used = s.used; text = text; styp = styp; pattern = None}
        | UIDENT "TRY"; s = SELF ->
            let text = TXtry _loc s.text in
            {used = s.used; text = text; styp = s.styp; pattern = None} ]
      | [ UIDENT "SELF" ->
            {used = []; text = TXself _loc; styp = STself _loc "SELF"; pattern = None}
        | UIDENT "NEXT" ->
            {used = []; text = TXnext _loc; styp = STself _loc "NEXT"; pattern = None}
        | "["; rl = LIST0 rule SEP "|"; "]" ->
            let rl = retype_rule_list_without_patterns _loc rl in
            let t = new_type_var () in
            {used = used_of_rule_list rl;
            text = TXrules _loc (srules _loc t rl "");
            styp = STquo _loc t; pattern = None}
        | "`"; p = patt -> mk_tok _loc p (STtok _loc)
        | x = UIDENT -> mk_tok _loc <:patt< $uid:x $(tup:<:patt< _ >>) >>
                               (STstring_tok _loc)
        | x = UIDENT; s = STRING -> mk_tok _loc <:patt< $uid:x $str:s >> (STtok _loc)
        | x = UIDENT; `ANTIQUOT "" s ->
            let e = AntiquotSyntax.parse_expr _loc s in
            let match_fun = <:expr< fun [ $uid:x camlp4_x when camlp4_x = $e -> True | _ -> False ] >> in
            let descr = "$" ^ x ^ " " ^ s in
            let text = TXtok _loc match_fun descr in
            let p = <:patt< $uid:x $(tup:<:patt< _ >>) >> in
            {used = []; text = text; styp = STtok _loc; pattern = Some p }
        | s = STRING ->
            {used = []; text = TXkwd _loc s;
             styp = STtok _loc; pattern = None }
        | i = UIDENT; "."; il = qualid;
          lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
            let n = mk_name _loc <:ident< $uid:i.$il >> in
            {used = [n.tvar]; text = TXnterm _loc n lev;
            styp = STquo _loc n.tvar; pattern = None}
        | n = name; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
            {used = [n.tvar]; text = TXnterm _loc n lev;
            styp = STquo _loc n.tvar; pattern = None}
        | "("; s_t = SELF; ")" -> s_t ] ]
    ;
    pattern:
      [ [ i = LIDENT -> <:patt< $lid:i >>
        | "_" -> <:patt< _ >>
        | "("; p = pattern; ")" -> <:patt< $p >>
        | "("; p1 = pattern; ","; p2 = comma_patt; ")" -> <:patt< ( $p1, $p2 ) >>
      ] ]
    ;
    comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | p = pattern -> p
      ] ]
    ;
    name:
      [ [ il = qualid -> mk_name _loc il ] ]
    ;
    string:
      [ [ s = STRING -> <:expr< $str:s >>
        | `ANTIQUOT "" s -> AntiquotSyntax.parse_expr _loc s ] ]
    ;
    semi_sep:
      [ [ ";" -> () ] ]
    ;
  END;


  (*
  EXTEND Gram
    symbol: LEVEL "top"
      [ NONA
        [ min = [ UIDENT "SLIST0" -> False | UIDENT "SLIST1" -> True ];
          s = SELF; sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
            sslist _loc min sep s
        | UIDENT "SOPT"; s = SELF ->
            ssopt _loc s ] ]
    ;
  END;
  *)

  value sfold _loc n foldfun f e s =
    let styp = STquo _loc (new_type_var ()) in
    let e = <:expr< $uid:gm.$lid:foldfun $f $e >> in
    let t = STapp _loc (STapp _loc (STtyp <:ctyp< $uid:gm.fold _ >>) s.styp) styp in
    {used = s.used; text = TXmeta _loc n [s.text] e t; styp = styp; pattern = None }
  ;

  value sfoldsep _loc n foldfun f e s sep =
    let styp = STquo _loc (new_type_var ()) in
    let e = <:expr< $uid:gm.$lid:foldfun $f $e >> in
    let t =
      STapp _loc (STapp _loc (STtyp <:ctyp< $uid:gm.foldsep _ >>) s.styp) styp
    in
    {used = s.used @ sep.used; text = TXmeta _loc n [s.text; sep.text] e t;
    styp = styp; pattern = None}
  ;

  EXTEND Gram
    GLOBAL: symbol;
    symbol: LEVEL "top"
      [ [ UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF ->
            sfold _loc "FOLD0" "sfold0" f e s
        | UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF ->
            sfold _loc "FOLD1" "sfold1" f e s
        | UIDENT "FOLD0"; f = simple_expr; e = simple_expr; s = SELF;
          UIDENT "SEP"; sep = symbol ->
            sfoldsep _loc "FOLD0 SEP" "sfold0sep" f e s sep
        | UIDENT "FOLD1"; f = simple_expr; e = simple_expr; s = SELF;
          UIDENT "SEP"; sep = symbol ->
            sfoldsep _loc "FOLD1 SEP" "sfold1sep" f e s sep ] ]
    ;
    simple_expr:
      [ [ i = a_LIDENT -> <:expr< $lid:i >>
        | "("; e = expr; ")" -> e ] ]
    ;
  END;

  Options.add "-split_ext" (Arg.Set split_ext)
    "Split EXTEND by functions to turn around a PowerPC problem.";

  Options.add "-split_gext" (Arg.Set split_ext)
    "Old name for the option -split_ext.";

  Options.add "-meta_action" (Arg.Set meta_action)
    "Undocumented"; (* FIXME *)

end;

module IdListComprehension = struct
  value name = "Camlp4ListComprehension";
  value version = Sys.ocaml_version;
end;

module MakeListComprehension (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  value rec loop n =
    fun
    [ [] -> None
    | [(x, _)] -> if n = 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ];

  value stream_peek_nth n strm = loop n (Stream.npeek n strm);

  (* usual trick *)
  value test_patt_lessminus =
    Gram.Entry.of_parser "test_patt_lessminus"
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

  value map _loc p e l =
    match (p, e) with
    [ (<:patt< $lid:x >>, <:expr< $lid:y >>) when x = y -> l
    | _ ->
        if Ast.is_irrefut_patt p then
          <:expr< List.map (fun $p -> $e) $l >>
        else
          <:expr< List.fold_right
                    (fun
                      [ $pat:p when True -> (fun x xs -> [ x :: xs ]) $e
                      | _ -> (fun l -> l) ])
                    $l [] >> ];

  value filter _loc p b l =
    if Ast.is_irrefut_patt p then
      <:expr< List.filter (fun $p -> $b) $l >>
    else
      <:expr< List.filter (fun [ $p when True -> $b | _ -> False ]) $l >>;

  value concat _loc l = <:expr< List.concat $l >>;

  value rec compr _loc e =
    fun
    [ [`gen (p, l)] -> map _loc p e l
    | [`gen (p, l); `cond b :: items] ->
        compr _loc e [`gen (p, filter _loc p b l) :: items]
    | [`gen (p, l) :: ([ `gen (_, _) :: _ ] as is )] ->
        concat _loc (map _loc p (compr _loc e is) l)
    | _ -> raise Stream.Failure ];

  DELETE_RULE Gram expr: "["; sem_expr_for_list; "]" END;

  value is_revised =
    try do {
      DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
      True
    } with [ Not_found -> False ];

  value comprehension_or_sem_expr_for_list =
    Gram.Entry.mk "comprehension_or_sem_expr_for_list";

  EXTEND Gram
    GLOBAL: expr comprehension_or_sem_expr_for_list;

    expr: LEVEL "simple"
      [ [ "["; e = comprehension_or_sem_expr_for_list; "]" -> e ] ]
    ;

    comprehension_or_sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; mk = sem_expr_for_list ->
            <:expr< [ $e :: $(mk <:expr< [] >>) ] >>
        | e = expr LEVEL "top"; ";" -> <:expr< [$e] >>
        | e = expr LEVEL "top"; "|"; l = LIST1 item SEP ";" -> compr _loc e l
        | e = expr LEVEL "top" -> <:expr< [$e] >> ] ]
    ;

    item:
      (* NP: These rules rely on being on this particular order. Which should
             be improved. *)
      [ [ p = TRY [p = patt; "<-" -> p] ; e = expr LEVEL "top" -> `gen (p, e)
        | e = expr LEVEL "top" -> `cond e ] ]
    ;

  END;

  if is_revised then
    EXTEND Gram
      GLOBAL: expr comprehension_or_sem_expr_for_list;

      comprehension_or_sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; mk = sem_expr_for_list; "::"; last = expr ->
            <:expr< [ $e :: $(mk last) ] >>
        | e = expr LEVEL "top"; "::"; last = expr ->
            <:expr< [ $e :: $last ] >> ] ]
      ;
    END
  else ();

end;
  
module IdMacroParser = struct
  value name = "Camlp4MacroParser";
  value version = Sys.ocaml_version;
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

     -D<uident> or -D<uident>=expr   define <uident> with optional value <expr>
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
  open Sig;
  include Syntax;

  type item_or_def 'a =
    [ SdStr of 'a
    | SdDef of string and option (list string * Ast.expr)
    | SdUnd of string
    | SdITE of bool and list (item_or_def 'a) and list (item_or_def 'a)
    | SdLazy of Lazy.t 'a ];

  value rec list_remove x =
    fun
    [ [(y, _) :: l] when y = x -> l
    | [d :: l] -> [d :: list_remove x l]
    | [] -> [] ];

  value defined = ref [];

  value is_defined i = List.mem_assoc i defined.val;

  value bad_patt _loc =
    Loc.raise _loc
      (Failure
         "this macro cannot be used in a pattern (see its definition)");

  value substp _loc env =
    loop where rec loop =
      fun
      [ <:expr< $e1 $e2 >> -> <:patt< $(loop e1) $(loop e2) >>
      | <:expr< >> -> <:patt< >>
      | <:expr< $lid:x >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $lid:x >> ]
      | <:expr< $uid:x >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $uid:x >> ]
      | <:expr< $int:x >> -> <:patt< $int:x >>
      | <:expr< $str:s >> -> <:patt< $str:s >>
      | <:expr< ($tup:x) >> -> <:patt< $(tup:loop x) >>
      | <:expr< $x1, $x2 >> -> <:patt< $(loop x1), $(loop x2) >>
      | <:expr< { $bi } >> ->
          let rec substbi = fun
            [ <:rec_binding< $b1; $b2 >> -> <:patt< $(substbi b1); $(substbi b2) >>
            | <:rec_binding< $i = $e >> -> <:patt< $i = $(loop e) >>
            | _ -> bad_patt _loc ]
          in <:patt< { $(substbi bi) } >>
      | _ -> bad_patt _loc ];

  class reloc _loc = object
    inherit Ast.map as super;
    method loc _ = _loc;
    (* method _Loc_t _ = _loc; *)
  end;

  class subst _loc env = object
    inherit reloc _loc as super;
    method expr =
      fun
      [ <:expr< $lid:x >> | <:expr< $uid:x >> as e ->
          try List.assoc x env with
          [ Not_found -> super#expr e ]
      | <:expr@_loc< LOCATION_OF $lid:x >> | <:expr@_loc< LOCATION_OF $uid:x >> as e ->
          try
            let loc = Ast.loc_of_expr (List.assoc x env) in
            let (a, b, c, d, e, f, g, h) = Loc.to_tuple loc in
            <:expr< Loc.of_tuple
              ($`str:a, $`int:b, $`int:c, $`int:d,
               $`int:e, $`int:f, $`int:g,
               $(if h then <:expr< True >> else <:expr< False >> )) >>
          with [ Not_found -> super#expr e ]
      | e -> super#expr e ];

    method patt =
      fun
      [ <:patt< $lid:x >> | <:patt< $uid:x >> as p ->
         try substp _loc [] (List.assoc x env) with
         [ Not_found -> super#patt p ]
      | p -> super#patt p ];
  end;

  value incorrect_number loc l1 l2 =
    Loc.raise loc
      (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1)));

  value define eo x =
    do {
      match eo with
      [ Some ([], e) ->
          EXTEND Gram
            expr: LEVEL "simple"
              [ [ UIDENT $x -> (new reloc _loc)#expr e ]]
            ;
            patt: LEVEL "simple"
              [ [ UIDENT $x ->
                    let p = substp _loc [] e
                    in (new reloc _loc)#patt p ]]
            ;
          END
      | Some (sl, e) ->
          EXTEND Gram
            expr: LEVEL "apply"
              [ [ UIDENT $x; param = SELF ->
                    let el =
                      match param with
                      [ <:expr< ($tup:e) >> -> Ast.list_of_expr e []
                      | e -> [e] ]
                    in
                    if List.length el = List.length sl then
                      let env = List.combine sl el in
                      (new subst _loc env)#expr e
                    else
                      incorrect_number _loc el sl ] ]
            ;
            patt: LEVEL "simple"
              [ [ UIDENT $x; param = SELF ->
                    let pl =
                      match param with
                      [ <:patt< ($tup:p) >> -> Ast.list_of_patt p []
                      | p -> [p] ]
                    in
                    if List.length pl = List.length sl then
                      let env = List.combine sl pl in
                      let p = substp _loc env e in
                      (new reloc _loc)#patt p
                    else
                      incorrect_number _loc pl sl ] ]
            ;
          END
      | None -> () ];
      defined.val := [(x, eo) :: defined.val];
    };

  value undef x =
    try
      do {
        let eo = List.assoc x defined.val in
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
        defined.val := list_remove x defined.val;
      }
    with
    [ Not_found -> () ];

  value parse_def s =
    match Gram.parse_string expr (Loc.mk "<command line>") s with
    [ <:expr< $uid:n >> -> define None n
    | <:expr< $uid:n = $e >> -> define (Some ([],e)) n
    | _ -> invalid_arg s ];

  (* This is a list of directories to search for INCLUDE statements. *)
  value include_dirs = ref [];

  (* Add something to the above, make sure it ends with a slash. *)
  value add_include_dir str =
    if str <> "" then
      let str =
        if String.get str ((String.length str)-1) = '/'
        then str else str ^ "/"
      in include_dirs.val := include_dirs.val @ [str]
    else ();

  value parse_include_file rule =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file ->
      let file =
        try (List.find (dir_ok file) (include_dirs.val @ ["./"])) ^ file
        with [ Not_found -> file ]
      in
      let ch = open_in file in
      let st = Stream.of_channel ch in
        Gram.parse rule (Loc.mk file) st;

  value rec execute_macro nil cons =
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
    cons il1 il2 ]
  ;

  (* Stack of conditionals. *)
  value stack = Stack.create () ;

  (* Make an SdITE value by extracting the result of the test from the stack. *)
  value make_SdITE_result st1 st2 =
   let test = Stack.pop stack in
   SdITE test st1 st2 ;

  type branch = [ Then | Else ];

  (* Execute macro only if it belongs to the currently active branch. *)
  value execute_macro_if_active_branch _loc nil cons branch macro_def =
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
    str_item: FIRST
      [ [ x = macro_def ->
            execute_macro <:str_item<>> (fun a b -> <:str_item< $a; $b >>) x ] ]
    ;
    sig_item: FIRST
      [ [ x = macro_def_sig ->
            execute_macro <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) x ] ]
    ;
    macro_def:
      [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; st1 = smlist_then; st2 = else_macro_def ->
            make_SdITE_result st1 st2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; st1 = smlist_then; st2 = else_macro_def ->
            make_SdITE_result st1 st2
        | "INCLUDE"; fname = STRING ->
            SdLazy (lazy (parse_include_file str_items fname)) ] ]
    ;
    macro_def_sig:
      [ [ "DEFINE"; i = uident -> SdDef i None
        | "UNDEF";  i = uident -> SdUnd i
        | "IFDEF";  uident_eval_ifdef;  "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "IFNDEF"; uident_eval_ifndef; "THEN"; sg1 = sglist_then; sg2 = else_macro_def_sig ->
            make_SdITE_result sg1 sg2
        | "INCLUDE"; fname = STRING ->
            SdLazy (lazy (parse_include_file sig_items fname)) ] ]
    ;
    uident_eval_ifdef:
      [ [ i = uident -> Stack.push (is_defined i) stack ]]
    ;
    uident_eval_ifndef:
      [ [ i = uident -> Stack.push (not (is_defined i)) stack ]]
    ;
    else_macro_def:
      [ [ "ELSE"; st = smlist_else; endif -> st
        | endif -> [] ] ]
    ;
    else_macro_def_sig:
      [ [ "ELSE"; st = sglist_else; endif -> st
        | endif -> [] ] ]
    ;
    else_expr:
      [ [ "ELSE"; e = expr; endif -> e
      | endif -> <:expr< () >> ] ]
    ;
    smlist_then:
      [ [ sml = LIST1 [ d = macro_def; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Then d
                      | si = str_item; semi -> SdStr si ] -> sml ] ]
    ;
    smlist_else:
      [ [ sml = LIST1 [ d = macro_def; semi ->
                          execute_macro_if_active_branch _loc <:str_item<>> (fun a b -> <:str_item< $a; $b >>) Else d
                      | si = str_item; semi -> SdStr si ] -> sml ] ]
    ;
    sglist_then:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Then d
                      | si = sig_item; semi -> SdStr si ] -> sgl ] ]
    ;
    sglist_else:
      [ [ sgl = LIST1 [ d = macro_def_sig; semi ->
                          execute_macro_if_active_branch _loc <:sig_item<>> (fun a b -> <:sig_item< $a; $b >>) Else d
                      | si = sig_item; semi -> SdStr si ] -> sgl ] ]
    ;
    endif:
      [ [ "END" -> ()
        | "ENDIF" -> () ] ]
    ;
    opt_macro_value:
      [ [ "("; pl = LIST1 [ x = LIDENT -> x ] SEP ","; ")"; "="; e = expr -> Some (pl, e)
        | "="; e = expr -> Some ([], e)
        | -> None ] ]
    ;
    expr: LEVEL "top"
      [ [ "IFDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e1 else e2
        | "IFNDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
            if is_defined i then e2 else e1
        | "DEFINE"; i = LIDENT; "="; def = expr; "IN"; body = expr ->
            (new subst _loc [(i, def)])#expr body ] ]
    ;
    patt:
      [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p1 else p2
        | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; endif ->
            if is_defined i then p2 else p1 ] ]
    ;
    uident:
      [ [ i = UIDENT -> i ] ]
    ;
    (* dirty hack to allow polymorphic variants using the introduced keywords. *)
    expr: BEFORE "simple"
      [ [ "`"; kwd = [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"
                     | "DEFINE" | "IN" ] -> <:expr< `$uid:kwd >>
        | "`"; s = a_ident -> <:expr< ` $s >> ] ]
    ;
    (* idem *)
    patt: BEFORE "simple"
      [ [ "`"; kwd = [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ] ->
            <:patt< `$uid:kwd >>
        | "`"; s = a_ident -> <:patt< ` $s >> ] ]
    ;
  END;

  Options.add "-D" (Arg.String parse_def)
    "<string> Define for IFDEF instruction.";
  Options.add "-U" (Arg.String undef)
    "<string> Undefine for IFDEF instruction.";
  Options.add "-I" (Arg.String add_include_dir)
    "<string> Add a directory to INCLUDE search path.";

end;



module MakeNothing (AstFilters : Camlp4.Sig.AstFilters) = struct
 open AstFilters;
 open Ast;

 (* Remove NOTHING and expanse __FILE__ and __LOCATION__ *)
 value map_expr =
   fun
   [ <:expr< $e NOTHING >> | <:expr< fun $(<:patt< NOTHING >> ) -> $e >> -> e
   | <:expr@_loc< $(lid:"__FILE__") >> -> <:expr< $(`str:Loc.file_name _loc) >>
   | <:expr@_loc< $(lid:"__LOCATION__") >> ->
     let (a, b, c, d, e, f, g, h) = Loc.to_tuple _loc in
     <:expr< Loc.of_tuple
       ($`str:a, $`int:b, $`int:c, $`int:d,
        $`int:e, $`int:f, $`int:g,
        $(if h then <:expr< True >> else <:expr< False >> )) >>
   | e -> e];

 register_str_item_filter (Ast.map_expr map_expr)#str_item;

end;


module IdReloadedParser = struct
  value name = "Camlp4Reloaded";
  value version = Sys.ocaml_version;
end;

module MakeReloadedParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  Gram.Entry.clear match_case;
  Gram.Entry.clear semi;

  value mkseq _loc =
    fun
    [ <:expr< $_; $_ >> as e -> <:expr< do { $e } >>
    | e -> e ]
  ;

  DELETE_RULE Gram match_case0: patt_as_patt_opt; opt_when_expr; "->"; expr END;

  value revised =
    try
      (DELETE_RULE Gram expr: "if"; SELF; "then"; SELF; "else"; SELF END; True)
    with [ Not_found -> begin
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top" END;
      DELETE_RULE Gram expr: "if"; SELF; "then"; expr LEVEL "top" END; False
    end ];

  if revised then begin
    DELETE_RULE Gram expr: "fun"; "["; LIST0 match_case0 SEP "|"; "]" END;
    EXTEND Gram
      expr: LEVEL "top"
      [ [ "function"; a = match_case -> <:expr< fun [ $a ] >> ] ];
    END;
    DELETE_RULE Gram value_let: "value" END;
    DELETE_RULE Gram value_val: "value" END;
  end else begin
    DELETE_RULE Gram value_let: "let" END;
    DELETE_RULE Gram value_val: "val" END;
  end;

  EXTEND Gram
    GLOBAL: match_case match_case0 expr value_let value_val semi;

    match_case:
      [ [ OPT "|"; l = LIST1 match_case0 SEP "|"; "end" -> Ast.mcOr_of_list l
        | "end" -> <:match_case<>> ] ]
    ;

    match_case0:
      [ [ p = patt_as_patt_opt; w = opt_when_expr; "->"; e = sequence ->
            <:match_case< $p when $w -> $(mkseq _loc e) >> ] ]
    ;

    expr: LEVEL "top"
      [ [ "if"; e1 = sequence; "then"; e2 = sequence; "else"; e3 = sequence; "end" ->
            <:expr< if $(mkseq _loc e1) then $(mkseq _loc e2) else $(mkseq _loc e3) >>
        | "if"; e1 = sequence; "then"; e2 = sequence; "end" ->
            <:expr< if $(mkseq _loc e1) then $(mkseq _loc e2) else () >> ] ]
    ;

    value_let:
      [ [ "val" -> () ] ]
    ;
    value_val:
      [ [ "val" -> () ] ]
    ;
    semi:
      [ [ ";;" -> () | ";" -> () | -> () ] ]
    ;
  END;

end;


module IdRevisedParser = struct
  value name = "Camlp4OCamlRevisedParser";
  value version = Sys.ocaml_version;
end;

module MakeRevisedParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  (* FanConfig.constructors_arity.val := True; *)
  FanConfig.constructors_arity.val := False;

  value help_sequences () =
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

  Gram.Entry.clear a_CHAR;
  Gram.Entry.clear a_FLOAT;
  Gram.Entry.clear a_INT;
  Gram.Entry.clear a_INT32;
  Gram.Entry.clear a_INT64;
  Gram.Entry.clear a_LABEL;
  Gram.Entry.clear a_LIDENT;
  Gram.Entry.clear a_NATIVEINT;
  Gram.Entry.clear a_OPTLABEL;
  Gram.Entry.clear a_STRING;
  Gram.Entry.clear a_UIDENT;
  Gram.Entry.clear a_ident;
  Gram.Entry.clear amp_ctyp;
  Gram.Entry.clear and_ctyp;
  Gram.Entry.clear match_case;
  Gram.Entry.clear match_case0;
  Gram.Entry.clear match_case_quot;
  Gram.Entry.clear binding;
  Gram.Entry.clear binding_quot;
  Gram.Entry.clear rec_binding_quot;
  Gram.Entry.clear class_declaration;
  Gram.Entry.clear class_description;
  Gram.Entry.clear class_expr;
  Gram.Entry.clear class_expr_quot;
  Gram.Entry.clear class_fun_binding;
  Gram.Entry.clear class_fun_def;
  Gram.Entry.clear class_info_for_class_expr;
  Gram.Entry.clear class_info_for_class_type;
  Gram.Entry.clear class_longident;
  Gram.Entry.clear class_longident_and_param;
  Gram.Entry.clear class_name_and_param;
  Gram.Entry.clear class_sig_item;
  Gram.Entry.clear class_sig_item_quot;
  Gram.Entry.clear class_signature;
  Gram.Entry.clear class_str_item;
  Gram.Entry.clear class_str_item_quot;
  Gram.Entry.clear class_structure;
  Gram.Entry.clear class_type;
  Gram.Entry.clear class_type_declaration;
  Gram.Entry.clear class_type_longident;
  Gram.Entry.clear class_type_longident_and_param;
  Gram.Entry.clear class_type_plus;
  Gram.Entry.clear class_type_quot;
  Gram.Entry.clear comma_ctyp;
  Gram.Entry.clear comma_expr;
  Gram.Entry.clear comma_ipatt;
  Gram.Entry.clear comma_patt;
  Gram.Entry.clear comma_type_parameter;
  Gram.Entry.clear constrain;
  Gram.Entry.clear constructor_arg_list;
  Gram.Entry.clear constructor_declaration;
  Gram.Entry.clear constructor_declarations;
  Gram.Entry.clear ctyp;
  Gram.Entry.clear ctyp_quot;
  Gram.Entry.clear cvalue_binding;
  Gram.Entry.clear direction_flag;
  Gram.Entry.clear dummy;
  Gram.Entry.clear eq_expr;
  Gram.Entry.clear expr;
  Gram.Entry.clear expr_eoi;
  Gram.Entry.clear expr_quot;
  Gram.Entry.clear field_expr;
  Gram.Entry.clear field_expr_list;
  Gram.Entry.clear fun_binding;
  Gram.Entry.clear fun_def;
  Gram.Entry.clear ident;
  Gram.Entry.clear ident_quot;
  Gram.Entry.clear implem;
  Gram.Entry.clear interf;
  Gram.Entry.clear ipatt;
  Gram.Entry.clear ipatt_tcon;
  Gram.Entry.clear label;
  Gram.Entry.clear label_declaration;
  Gram.Entry.clear label_declaration_list;
  Gram.Entry.clear label_expr_list;
  Gram.Entry.clear label_expr;
  Gram.Entry.clear label_ipatt;
  Gram.Entry.clear label_ipatt_list;
  Gram.Entry.clear label_longident;
  Gram.Entry.clear label_patt;
  Gram.Entry.clear label_patt_list;
  Gram.Entry.clear labeled_ipatt;
  Gram.Entry.clear let_binding;
  Gram.Entry.clear meth_list;
  Gram.Entry.clear meth_decl;
  Gram.Entry.clear module_binding;
  Gram.Entry.clear module_binding0;
  Gram.Entry.clear module_binding_quot;
  Gram.Entry.clear module_declaration;
  Gram.Entry.clear module_expr;
  Gram.Entry.clear module_expr_quot;
  Gram.Entry.clear module_longident;
  Gram.Entry.clear module_longident_with_app;
  Gram.Entry.clear module_rec_declaration;
  Gram.Entry.clear module_type;
  Gram.Entry.clear module_type_quot;
  Gram.Entry.clear more_ctyp;
  Gram.Entry.clear name_tags;
  Gram.Entry.clear opt_as_lident;
  Gram.Entry.clear opt_class_self_patt;
  Gram.Entry.clear opt_class_self_type;
  Gram.Entry.clear opt_comma_ctyp;
  Gram.Entry.clear opt_dot_dot;
  Gram.Entry.clear opt_eq_ctyp;
  Gram.Entry.clear opt_expr;
  Gram.Entry.clear opt_meth_list;
  Gram.Entry.clear opt_mutable;
  Gram.Entry.clear opt_polyt;
  Gram.Entry.clear opt_private;
  Gram.Entry.clear opt_rec;
  Gram.Entry.clear opt_virtual;
  Gram.Entry.clear opt_when_expr;
  Gram.Entry.clear patt;
  Gram.Entry.clear patt_as_patt_opt;
  Gram.Entry.clear patt_eoi;
  Gram.Entry.clear patt_quot;
  Gram.Entry.clear patt_tcon;
  Gram.Entry.clear phrase;
  Gram.Entry.clear poly_type;
  Gram.Entry.clear row_field;
  Gram.Entry.clear sem_expr;
  Gram.Entry.clear sem_expr_for_list;
  Gram.Entry.clear sem_patt;
  Gram.Entry.clear sem_patt_for_list;
  Gram.Entry.clear semi;
  Gram.Entry.clear sequence;
  Gram.Entry.clear sig_item;
  Gram.Entry.clear sig_item_quot;
  Gram.Entry.clear sig_items;
  Gram.Entry.clear star_ctyp;
  Gram.Entry.clear str_item;
  Gram.Entry.clear str_item_quot;
  Gram.Entry.clear str_items;
  Gram.Entry.clear top_phrase;
  Gram.Entry.clear type_constraint;
  Gram.Entry.clear type_declaration;
  Gram.Entry.clear type_ident_and_parameters;
  Gram.Entry.clear type_kind;
  Gram.Entry.clear type_longident;
  Gram.Entry.clear type_longident_and_parameters;
  Gram.Entry.clear type_parameter;
  Gram.Entry.clear type_parameters;
  Gram.Entry.clear typevars;
  Gram.Entry.clear use_file;
  Gram.Entry.clear val_longident;
  Gram.Entry.clear value_let;
  Gram.Entry.clear value_val;
  Gram.Entry.clear with_constr;
  Gram.Entry.clear with_constr_quot;

  value neg_string n =
    let len = String.length n in
    if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
    else "-" ^ n
  ;

  value mkumin _loc f arg =
    match arg with
    [ <:expr< $int:n >> -> <:expr< $(int:neg_string n) >>
    | <:expr< $int32:n >> -> <:expr< $(int32:neg_string n) >>
    | <:expr< $(int64:n) >> -> <:expr< $(int64:neg_string n) >>
    | <:expr< $nativeint:n >> -> <:expr< $(nativeint:neg_string n) >>
    | <:expr< $flo:n >> -> <:expr< $(flo:neg_string n) >>
    | _ -> <:expr< $(lid:"~" ^ f) $arg >> ];

  value mklistexp _loc last =
    loop True where rec loop top =
      fun
      [ [] ->
          match last with
          [ Some e -> e
          | None -> <:expr< [] >> ]
      | [e1 :: el] ->
          let _loc =
            if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
          in
          <:expr< [$e1 :: $(loop False el)] >> ]
  ;

  value mkassert _loc =
    fun
    [ <:expr< False >> ->
        <:expr< assert False >> (* this case takes care about
                                   the special assert false node *)
    | e -> <:expr< assert $e >> ]
  ;

  value append_eLem el e = el @ [e];
  value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;

  value mksequence _loc =
    fun
    [ <:expr< $_; $_ >> | <:expr< $anti:_ >> as e -> <:expr< do { $e } >>
    | e -> e ]
  ;

  value mksequence' _loc =
    fun
    [ <:expr< $_; $_ >> as e -> <:expr< do { $e } >>
    | e -> e ]
  ;

  value rec lid_of_ident =
    fun
    [ <:ident< $_ . $i >> -> lid_of_ident i
    | <:ident< $lid:lid >> -> lid
    | _                     -> assert False ];

  value module_type_app mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1 >>, <:module_type< $id:i2 >>) ->
        <:module_type< $(id:<:ident< $i1 $i2 >>) >>
    | _ -> raise Stream.Failure ];

  value module_type_acc mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1 >>, <:module_type< $id:i2 >>) ->
        <:module_type< $(id:<:ident< $i1.$i2 >>) >>
    | _ -> raise Stream.Failure ];

  value bigarray_get _loc arr arg =
    let coords =
      match arg with
      [ <:expr< ($e1, $e2) >> | <:expr< $e1, $e2 >> ->
          Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
      | _ -> [arg] ]
    in
    match coords with
    [ [c1] -> <:expr< Bigarray.Array1.get $arr $c1 >>
    | [c1; c2] -> <:expr< Bigarray.Array2.get $arr $c1 $c2 >>
    | [c1; c2; c3] -> <:expr< Bigarray.Array3.get $arr $c1 $c2 $c3 >>
    (* | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ] *)
    | coords ->
       <:expr< Bigarray.Genarray.get $arr [| $(Ast.exSem_of_list coords) |] >> ];

  value bigarray_set _loc var newval =
    match var with
    [ <:expr< Bigarray.Array1.get $arr $c1 >> ->
        Some <:expr< Bigarray.Array1.set $arr $c1 $newval >>
    | <:expr< Bigarray.Array2.get $arr $c1 $c2 >> ->
        Some <:expr< Bigarray.Array2.set $arr $c1 $c2 $newval >>
    | <:expr< Bigarray.Array3.get $arr $c1 $c2 $c3 >> ->
        Some <:expr< Bigarray.Array3.set $arr $c1 $c2 $c3 $newval >>
    | <:expr< Bigarray.Genarray.get $arr [| $coords |] >> ->
        Some <:expr< Bigarray.Genarray.set $arr [| $coords |] $newval >>
    | _ -> None ];

  value stopped_at _loc =
    Some (Loc.move_line 1 _loc) (* FIXME be more precise *);

  value rec generalized_type_of_type =
    fun
    [ <:ctyp< $t1 -> $t2 >> ->
        let (tl, rt) = generalized_type_of_type t2 in
        ([t1 :: tl], rt)
    | t ->
        ([], t) ]
  ;

  value symbolchar =
    let list =
      ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
       '@'; '^'; '|'; '~'; '\\']
    in
    let rec loop s i =
      if i == String.length s then True
      else if List.mem s.[i] list then loop s (i + 1)
      else False
    in
    loop
  ;

  value setup_op_parser entry p =
    Gram.Entry.setup_parser entry
      (parser
        [: `(KEYWORD x | SYMBOL x, ti) when p x :] ->
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

  value rec infix_kwds_filter =
    parser
    [ [: `((KEYWORD "(", _) as tok); xs :] ->
        match xs with parser
        [ [: `(KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i), _loc);
             `(KEYWORD ")", _); xs :] ->
                [: `(LIDENT i, _loc); infix_kwds_filter xs :]
        | [: xs :] ->
                [: `tok; infix_kwds_filter xs :] ]
    | [: `x; xs :] -> [: `x; infix_kwds_filter xs :] ];

  Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.Entry.setup_parser sem_expr begin
    let symb1 = Gram.parse_tokens_after_filter expr in
    let symb =
      parser
      [ [: `(ANTIQUOT ("list" as n) s, ti) :] ->
        let _loc = Gram.token_location ti in
        <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
      | [: a = symb1 :] -> a ]
    in
    let rec kont al =
      parser
      [ [: `(KEYWORD ";", _); a = symb; s :] ->
        let _loc = Loc.merge (Ast.loc_of_expr al)
                             (Ast.loc_of_expr a) in
        kont <:expr< $al; $a >> s
      | [: :] -> al ]
    in
    parser [: a = symb; s :] -> kont a s
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
      use_file val_longident value_let value_val with_constr with_constr_quot
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
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_expr_tag
        | i = module_longident -> <:module_expr< $id:i >>
        | "("; me = SELF; ":"; mt = module_type; ")" ->
            <:module_expr< ( $me : $mt ) >>
        | "("; me = SELF; ")" -> <:module_expr< $me >>
        | "("; value_val; e = expr; ")" ->
            <:module_expr< (value $e) >>
        | "("; value_val; e = expr; ":"; p = package_type; ")" ->
            <:module_expr< (value $e : $p) >> ] ]
    ;
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
        | value_let; r = opt_rec; bi = binding ->
            <:str_item< value $rec:r $bi >>
        | "class"; cd = class_declaration ->
            <:str_item< class $cd >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:str_item< class type $ctd >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $(anti:mk_anti ~c:"str_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.str_item_tag
        | e = expr -> <:str_item< $exp:e >> ] ]
    ;
    module_binding0:
      [ RIGHTA
        [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
            <:module_expr< functor ( $m : $mt ) -> $mb >>
        | ":"; mt = module_type; "="; me = module_expr ->
            <:module_expr< ( $me : $mt ) >>
        | "="; me = module_expr -> <:module_expr< $me >> ] ]
    ;
    module_binding:
      [ LEFTA
        [ b1 = SELF; "and"; b2 = SELF ->
            <:module_binding< $b1 and $b2 >>
        | `ANTIQUOT ("module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $(mk_anti n m) : $mt = $me >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $m : $mt = $me >> ] ]
    ;
    module_type:
      [ "top"
        [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
            <:module_type< functor ( $i : $t ) -> $mt >> ]
      | "with"
        [ mt = SELF; "with"; wc = with_constr ->
            <:module_type< $mt with $wc >> ]
      | "apply"
        [ mt1 = SELF; mt2 = SELF; dummy -> module_type_app mt1 mt2 ]
      | "."
        [ mt1 = SELF; "."; mt2 = SELF -> module_type_acc mt1 mt2 ]
      | "sig"
        [ "sig"; sg = sig_items; "end" ->
            <:module_type< sig $sg end >> ]
      | "simple"
        [ `ANTIQUOT (""|"mtyp"|"anti"|"list" as n) s ->
            <:module_type< $(anti:mk_anti ~c:"module_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_type_tag
        | i = module_longident_with_app -> <:module_type< $id:i >>
        | "'"; i = a_ident -> <:module_type< ' $i >>
        | "("; mt = SELF; ")" -> <:module_type< $mt >>
        | "module"; "type"; "of"; me = module_expr ->
            <:module_type< module type of $me >> ] ]
    ;
    sig_item:
      [ "top"
        [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $(anti:mk_anti ~c:"sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.sig_item_tag
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
        | value_val; i = a_LIDENT; ":"; t = ctyp ->
            <:sig_item< value $i : $t >>
        | "class"; cd = class_description ->
            <:sig_item< class $cd >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:sig_item< class type $ctd >> ] ]
    ;
    module_declaration:
      [ RIGHTA
        [ ":"; mt = module_type -> <:module_type< $mt >>
        | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
            <:module_type< functor ( $i : $t ) -> $mt >> ] ]
    ;
    module_rec_declaration:
      [ LEFTA
        [ m1 = SELF; "and"; m2 = SELF -> <:module_binding< $m1 and $m2 >>
        | `ANTIQUOT (""|"module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $(anti:mk_anti ~c:"module_binding" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type -> <:module_binding< $m : $mt >>
      ] ]
    ;
    with_constr:
      [ LEFTA
        [ wc1 = SELF; "and"; wc2 = SELF -> <:with_constr< $wc1 and $wc2 >>
        | `ANTIQUOT (""|"with_constr"|"anti"|"list" as n) s ->
            <:with_constr< $(anti:mk_anti ~c:"with_constr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.with_constr_tag
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
    ;
    expr:
      [ "top" RIGHTA
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
            <:expr< match $(mksequence' _loc e) with [ $a ] >>
        | "try"; e = sequence; "with"; a = match_case ->
            <:expr< try $(mksequence' _loc e) with [ $a ] >>
        | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
            <:expr< if $e1 then $e2 else $e3 >>
        | "do"; seq = do_sequence -> mksequence _loc seq
        | "for"; i = a_LIDENT; "="; e1 = sequence; df = direction_flag;
          e2 = sequence; "do"; seq = do_sequence ->
            <:expr< for $i = $(mksequence' _loc e1) $to:df $(mksequence' _loc e2) do
              { $seq } >>
        | "while"; e = sequence; "do"; seq = do_sequence ->
            <:expr< while $(mksequence' _loc e) do { $seq } >>
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:expr< object ($csp) $cst end >> ]
      | "where"
        [ e = SELF; "where"; rf = opt_rec; lb = let_binding ->
            <:expr< let $rec:rf $lb in $e >> ]
      | ":=" NONA
        [ e1 = SELF; ":="; e2 = SELF; dummy ->
            match bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1 := $e2 >> ] ]
      | "||" RIGHTA
        [ e1 = SELF; op = infixop6; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "&&" RIGHTA
        [ e1 = SELF; op = infixop5; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "<" LEFTA
        [ e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "^" RIGHTA
        [ e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "+" LEFTA
        [ e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "*" LEFTA
        [ e1 = SELF; "land"; e2 = SELF -> <:expr< $e1 land $e2 >>
        | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1 lor $e2 >>
        | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1 lxor $e2 >>
        | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1 mod $e2 >>
        | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "**" RIGHTA
        [ e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1 asr $e2 >>
        | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1 lsl $e2 >>
        | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1 lsr $e2 >>
        | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $op $e1 $e2 >> ]
      | "unary minus" NONA
        [ "-"; e = SELF -> mkumin _loc "-" e
        | "-."; e = SELF -> mkumin _loc "-." e ]
      | "apply" LEFTA
        [ e1 = SELF; e2 = SELF -> <:expr< $e1 $e2 >>
        | "assert"; e = SELF -> mkassert _loc e
        | "new"; i = class_longident -> <:expr< new $i >>
        | "lazy"; e = SELF -> <:expr< lazy $e >> ]
      | "label" NONA
        [ "~"; i = a_LIDENT; ":"; e = SELF -> <:expr< ~ $i : $e >>
        | "~"; i = a_LIDENT -> <:expr< ~ $i >>

        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; e = SELF -> <:expr< ~ $i : $e >>

        (* Same remark for ?a:b *)
        | `OPTLABEL i; e = SELF -> <:expr< ? $i : $e >>

        | "?"; i = a_LIDENT; ":"; e = SELF -> <:expr< ? $i : $e >>
        | "?"; i = a_LIDENT -> <:expr< ? $i >> ]
      | "." LEFTA
        [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1 .( $e2 ) >>
        | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1 .[ $e2 ] >>
        | e1 = SELF; "."; "{"; e2 = comma_expr; "}" -> bigarray_get _loc e1 e2
        | e1 = SELF; "."; e2 = SELF -> <:expr< $e1 . $e2 >>
        | e = SELF; "#"; lab = label -> <:expr< $e # $lab >> ]
      | "~-" NONA
        [ "!"; e = SELF -> <:expr< $e.val >>
        | f = prefixop; e = SELF -> <:expr< $f $e >> ]
      | "simple"
        [ `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.expr_tag
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
        | "("; e = SELF; ";"; seq = sequence; ")" -> mksequence _loc <:expr< $e; $seq >>
        | "("; e = SELF; ";"; ")" -> mksequence _loc e
        | "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
            <:expr< ($e : $t :> $t2 ) >>
        | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e :> $t) >>
        | "("; e = SELF; ")" -> e
        | "begin"; seq = sequence; "end" -> mksequence _loc seq
        | "begin"; "end" -> <:expr< () >>
        | "("; "module"; me = module_expr; ")" ->
            <:expr< (module $me) >>
        | "("; "module"; me = module_expr; ":"; pt = package_type; ")" ->
            <:expr< (module $me : $pt) >>
        ] ]
    ;
    do_sequence:
      [ [ seq = TRY ["{"; seq = sequence; "}" -> seq] -> seq
        | TRY ["{"; "}"] -> <:expr< () >>
        | seq = TRY [seq = sequence; "done" -> seq] -> seq
        | "done" -> <:expr< () >>
      ] ]
    ;
    infixop5:
      [ [ x = [ "&" | "&&" ] -> <:expr< $lid:x >> ] ]
    ;
    infixop6:
      [ [ x = [ "or" | "||" ] -> <:expr< $lid:x >> ] ]
    ;
    sem_expr_for_list:
      [ [ e = expr; ";"; el = SELF -> fun acc -> <:expr< [ $e :: $(el acc) ] >>
        | e = expr; ";" -> fun acc -> <:expr< [ $e :: $acc ] >>
        | e = expr -> fun acc -> <:expr< [ $e :: $acc ] >>
      ] ]
    ;
    comma_expr:
      [ [ e1 = SELF; ","; e2 = SELF -> <:expr< $e1, $e2 >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $(anti:mk_anti ~c:"expr," n s) >>
        | e = expr LEVEL "top" -> e ] ]
    ;
    dummy:
      [ [ -> () ] ]
    ;
    sequence':
      [ [ -> fun e -> e
        | ";" -> fun e -> e
        | ";"; el = sequence -> fun e -> <:expr< $e; $el >> ] ]
    ;
    sequence:
      [ [ "let"; rf = opt_rec; bi = binding; "in"; e = expr; k = sequence' ->
            k <:expr< let $rec:rf $bi in $e >>
        | "let"; rf = opt_rec; bi = binding; ";"; el = SELF ->
            <:expr< let $rec:rf $bi in $(mksequence _loc el) >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr; k = sequence' ->
            k <:expr< let module $m = $mb in $e >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; ";"; el = SELF ->
            <:expr< let module $m = $mb in $(mksequence _loc el) >>
        | "let"; "open"; i = module_longident; "in"; e = SELF ->
            <:expr< let open $id:i in $e >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $(anti:mk_anti ~c:"expr;" n s) >>
        | e = expr; k = sequence' -> k e ] ]
    ;
    binding:
      [ LEFTA
        [ `ANTIQUOT ("binding"|"list" as n) s ->
            <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | `ANTIQUOT (""|"anti" as n) s; "="; e = expr ->
            <:binding< $(anti:mk_anti ~c:"patt" n s) = $e >>
        | `ANTIQUOT (""|"anti" as n) s -> <:binding< $(anti:mk_anti ~c:"binding" n s) >>
        | b1 = SELF; "and"; b2 = SELF -> <:binding< $b1 and $b2 >>
        | b = let_binding -> b
      ] ]
    ;
    let_binding:
      [ [ p = ipatt; e = fun_binding -> <:binding< $p = $e >> ] ]
    ;
    fun_binding:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")"; e = SELF ->
            <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; e = SELF ->
            <:expr< fun $p -> $e >>
        | bi = cvalue_binding -> bi
      ] ]
    ;
    match_case:
      [ [ "["; l = LIST0 match_case0 SEP "|"; "]" -> Ast.mcOr_of_list l
        | p = ipatt; "->"; e = expr -> <:match_case< $p -> $e >> ] ]
    ;
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
    ;
    opt_when_expr:
      [ [ "when"; w = expr -> w
        | -> <:expr<>>
      ] ]
    ;
    patt_as_patt_opt:
      [ [ p1 = patt; "as"; p2 = patt -> <:patt< ($p1 as $p2) >>
        | p = patt -> p
      ] ]
    ;
    label_expr_list:
      [ [ b1 = label_expr; ";"; b2 = SELF -> <:rec_binding< $b1 ; $b2 >>
        | b1 = label_expr; ";"            -> b1
        | b1 = label_expr                 -> b1
      ] ];
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
            <:rec_binding< $i = $(lid:lid_of_ident i) >> ] ]
    ;
    fun_def:
      [ [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; (w, e) = fun_def_cont ->
            <:expr< fun [ $p when $w -> $e ] >> ] ]
    ;
    fun_def_cont:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            (<:expr<>>, <:expr< fun (type $i) -> $e >>)
        | p = TRY labeled_ipatt; (w,e) = SELF ->
            (<:expr<>>, <:expr< fun [ $p when $w -> $e ] >>)
        | "when"; w = expr; "->"; e = expr -> (w, e)
        | "->"; e = expr -> (<:expr<>>, e) ] ]
    ;
    fun_def_cont_no_when:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when -> <:expr< fun (type $i) -> $e >>
        | p = TRY labeled_ipatt; (w,e) = fun_def_cont ->
            <:expr< fun [ $p when $w -> $e ] >>
        | "->"; e = expr -> e ] ]
    ;
    patt:
      [ "|" LEFTA
        [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1 | $p2 >> ]
      | ".." NONA
        [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1 .. $p2 >> ]
      | "apply" LEFTA
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
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
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
    ;
    comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | p = patt -> p ] ]
    ;
    sem_patt:
      [ LEFTA
        [ p1 = patt; ";"; p2 = SELF -> <:patt< $p1; $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | p = patt; ";" -> p
        | p = patt -> p ] ]
    ;
    sem_patt_for_list:
      [ [ p = patt; ";"; pl = SELF -> fun acc -> <:patt< [ $p :: $(pl acc) ] >>
        | p = patt; ";" -> fun acc -> <:patt< [ $p :: $acc ] >>
        | p = patt -> fun acc -> <:patt< [ $p :: $acc ] >>
      ] ]
    ;
    label_patt_list:
      [ [ p1 = label_patt; ";"; p2 = SELF -> <:patt< $p1 ; $p2 >>
        | p1 = label_patt; ";"; "_"       -> <:patt< $p1 ; _ >>
        | p1 = label_patt; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | p1 = label_patt; ";"            -> p1
        | p1 = label_patt                 -> p1
      ] ];
    label_patt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | `ANTIQUOT ("list" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | i = label_longident; "="; p = patt -> <:patt< $i = $p >>
        | i = label_longident -> <:patt< $i = $(lid:lid_of_ident i) >>
      ] ]
    ;
    ipatt:
      [ [ "{"; pl = label_ipatt_list; "}" -> <:patt< { $pl } >>
        | `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >>)) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
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
    ;
    labeled_ipatt:
      [ [ p = ipatt -> p ] ]
    ;
    comma_ipatt:
      [ LEFTA
        [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1, $p2 >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $(anti:mk_anti ~c:"patt," n s) >>
        | p = ipatt -> p ] ]
    ;
    label_ipatt_list:
      [ [ p1 = label_ipatt; ";"; p2 = SELF -> <:patt< $p1 ; $p2 >>
        | p1 = label_ipatt; ";"; "_"       -> <:patt< $p1 ; _ >>
        | p1 = label_ipatt; ";"; "_"; ";"  -> <:patt< $p1 ; _ >>
        | p1 = label_ipatt; ";"            -> p1
        | p1 = label_ipatt                 -> p1
      ] ];
    label_ipatt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt;" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x Quotation.DynAst.patt_tag
        | i = label_longident; "="; p = ipatt -> <:patt< $i = $p >>
      ] ]
    ;
    type_declaration:
      [ LEFTA
        [ `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1 and $t2 >>
        | (n, tpl) = type_ident_and_parameters; tk = opt_eq_ctyp;
          cl = LIST0 constrain -> Ast.TyDcl _loc n tpl tk cl ] ]
    ;
    constrain:
      [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
    ;
    opt_eq_ctyp:
      [ [ "="; tk = type_kind -> tk
        | -> <:ctyp<>> ] ]
    ;
    type_kind:
      [ [ t = ctyp -> t ] ]
    ;
    type_ident_and_parameters:
      [ [ i = a_LIDENT; tpl = LIST0 optional_type_parameter -> (i, tpl) ] ]
    ;
    type_longident_and_parameters:
      [ [ i = type_longident; tpl = type_parameters -> tpl <:ctyp< $id:i >>
      ] ]
    ;
    type_parameters:
      [ [ t1 = type_parameter; t2 = SELF ->
            fun acc -> t2 <:ctyp< $acc $t1 >>
        | t = type_parameter -> fun acc -> <:ctyp< $acc $t >>
        | -> fun t -> t
      ] ]
    ;

    type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i >> ] ]
    ;
    optional_type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i >>
        | "+"; "_" -> Ast.TyAnP _loc 
        | "-"; "_" -> Ast.TyAnM _loc
        | "_" -> Ast.TyAny _loc

 ] ]
    ;


    ctyp:
      [ "==" LEFTA
        [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1 == $t2 >> ]
      | "private" NONA
        [ "private"; t = ctyp LEVEL "alias" -> <:ctyp< private $t >> ]
      | "alias" LEFTA
        [ t1 = SELF; "as"; t2 = SELF ->
          <:ctyp< $t1 as $t2 >> ]
      | "forall" LEFTA
        [ "!"; t1 = typevars; "."; t2 = ctyp ->
          <:ctyp< ! $t1 . $t2 >> ]
      | "arrow" RIGHTA
        [ t1 = SELF; "->"; t2 = SELF ->
          <:ctyp< $t1 -> $t2 >> ]
      | "label" NONA
        [ "~"; i = a_LIDENT; ":"; t = SELF ->
          <:ctyp< ~ $i : $t >>
        | i = a_LABEL; t =  SELF  ->
          <:ctyp< ~ $i : $t >>
        | "?"; i = a_LIDENT; ":"; t = SELF ->
            <:ctyp< ? $i : $t >>
        | i = a_OPTLABEL; t = SELF ->
            <:ctyp< ? $i : $t >> ]
      | "apply" LEFTA
        [ t1 = SELF; t2 = SELF ->
            let t = <:ctyp< $t1 $t2 >> in
            try <:ctyp< $(id:Ast.ident_of_ctyp t) >>
            with [ Invalid_argument _ -> t ] ]
      | "." LEFTA
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
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
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
        | "("; "module"; p = package_type; ")" -> <:ctyp< (module $p) >>
      ] ]
    ;
    star_ctyp:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp*" n s) >>
        | t1 = SELF; "*"; t2 = SELF ->
            <:ctyp< $t1 * $t2 >>
        | t = ctyp -> t
      ] ]
    ;
    constructor_declarations:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp|" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | t1 = SELF; "|"; t2 = SELF ->
            <:ctyp< $t1 | $t2 >>
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s of $t >>
        | s = a_UIDENT; ":"; t = ctyp ->
            let (tl, rt) = generalized_type_of_type t in
            <:ctyp< $uid:s : ($(Ast.tyAnd_of_list tl) -> $rt) >>
        | s = a_UIDENT ->
	  <:ctyp< $uid:s >>
      ] ]
    ;
    constructor_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s of $t >>
        | s = a_UIDENT ->
            <:ctyp< $uid:s >>
      ] ]
    ;
    constructor_arg_list:
      [ [ `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctypand" n s) >>
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1 and $t2 >>
        | t = ctyp -> t
      ] ]
    ;
    label_declaration_list:
      [ [ t1 = label_declaration; ";"; t2 = SELF -> <:ctyp< $t1; $t2 >>
        | t1 = label_declaration; ";"            -> t1
        | t1 = label_declaration                 -> t1
      ] ]
    ;
    label_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_LIDENT; ":"; t = poly_type ->
            <:ctyp< $lid:s : $t >>
        | s = a_LIDENT; ":"; "mutable"; t = poly_type ->
            <:ctyp< $lid:s : mutable $t >>
      ] ]
    ;
    a_ident:
      [ [ i = a_LIDENT -> i
        | i = a_UIDENT -> i ] ]
    ;
    ident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | i = a_LIDENT -> <:ident< $lid:i >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i.$j >> ] ]
    ;
    module_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_UIDENT -> <:ident< $uid:i >> ] ]
    ;
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
    ;
    module_longident_dot_lparen:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; "(" ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_UIDENT; "."; "(" -> <:ident< $uid:i >> ] ]
    ;
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
    ;
    label_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m.$l >>
        | i = a_LIDENT -> <:ident< $lid:i >> ] ]
    ;
    class_type_longident:
      [ [ x = type_longident -> x ] ]
    ;
    val_longident:
      [ [ x = ident -> x ] ]
    ;
    class_longident:
      [ [ x = label_longident -> x ] ]
    ;
    class_declaration:
      [ LEFTA
        [ c1 = SELF; "and"; c2 = SELF ->
            <:class_expr< $c1 and $c2 >>
        | `ANTIQUOT (""|"cdcl"|"anti"|"list" as n) s ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_expr_tag
        | ci = class_info_for_class_expr; ce = class_fun_binding ->
            <:class_expr< $ci = $ce >>
      ] ]
    ;
    class_fun_binding:
      [ [ "="; ce = class_expr -> ce
        | ":"; ct = class_type_plus; "="; ce = class_expr ->
            <:class_expr< ($ce : $ct) >>
        | p = labeled_ipatt; cfb = SELF ->
            <:class_expr< fun $p -> $cfb >>
      ] ]
    ;
    class_info_for_class_type:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_type< $virtual:mv $lid:i [ $ot ] >>
      ] ]
    ;
    class_info_for_class_expr:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_expr< $virtual:mv $lid:i [ $ot ] >>
      ] ]
    ;
    class_name_and_param:
      [ [ i = a_LIDENT; "["; x = comma_type_parameter; "]" -> (i, x)
        | i = a_LIDENT -> (i, <:ctyp<>>)
      ] ]
    ;
    comma_type_parameter:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | t = type_parameter -> t
      ] ]
    ;
    opt_comma_ctyp:
      [ [ "["; x = comma_ctyp; "]" -> x
        | -> <:ctyp<>>
      ] ]
    ;
    comma_ctyp:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1, $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp," n s) >>
        | t = ctyp -> t
      ] ]
    ;
    class_fun_def:
      [ [ p = labeled_ipatt; ce = SELF -> <:class_expr< fun $p -> $ce >>
        | "->"; ce = class_expr -> ce ] ]
    ;
    class_expr:
      [ "top"
        [ "fun"; p = labeled_ipatt; ce = class_fun_def ->
            <:class_expr< fun $p -> $ce >>
        | "let"; rf = opt_rec; bi = binding; "in"; ce = SELF ->
            <:class_expr< let $rec:rf $bi in $ce >> ]
      | "apply" NONA
        [ ce = SELF; e = expr LEVEL "label" ->
            <:class_expr< $ce $e >> ]
      | "simple"
        [ `ANTIQUOT (""|"cexp"|"anti" as n) s ->
            <:class_expr< $(anti:mk_anti ~c:"class_expr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_expr_tag
        | ce = class_longident_and_param -> ce
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:class_expr< object ($csp) $cst end >>
        | "("; ce = SELF; ":"; ct = class_type; ")" ->
            <:class_expr< ($ce : $ct) >>
        | "("; ce = SELF; ")" -> ce ] ]
    ;
    class_longident_and_param:
      [ [ ci = class_longident; "["; t = comma_ctyp; "]" ->
          <:class_expr< $id:ci [ $t ] >>
        | ci = class_longident -> <:class_expr< $id:ci >>
      ] ]
    ;
    class_structure:
      [ [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `ANTIQUOT (""|"cst"|"anti"|"list" as n) s; semi; cst = SELF ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s); $cst >>
        | l = LIST0 [ cst = class_str_item; semi -> cst ] -> Ast.crSem_of_list l
      ] ]
    ;
    opt_class_self_patt:
      [ [ "("; p = patt; ")" -> p
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p : $t) >>
        | -> <:patt<>> ] ]
    ;
    class_str_item:
      [ LEFTA
        [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $(anti:mk_anti ~c:"class_str_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_str_item_tag
        | "inherit"; o = opt_override; ce = class_expr; pb = opt_as_lident ->
            <:class_str_item< inherit $override:o $ce as $pb >>
        | o = value_val_opt_override; mf = opt_mutable; lab = label; e = cvalue_binding
          ->
            <:class_str_item< value $override:o $mutable:mf $lab = $e >>
        | o = value_val_opt_override; mf = opt_mutable; "virtual"; l = label; ":";
              t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< value virtual $mutable:mf $l : $t >>
        | o = value_val_opt_override; "virtual"; mf = opt_mutable; l = label; ":";
                t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< value virtual $mutable:mf $l : $t >>
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
    ;
    method_opt_override:
      [ [ "method"; "!" -> <:override_flag< ! >>
        | "method"; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | "method" -> <:override_flag<>>
      ] ]
    ;
    value_val_opt_override:
      [ [ value_val; "!" -> <:override_flag< ! >>
        | value_val; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | value_val -> <:override_flag<>>
      ] ]
    ;
    opt_as_lident:
      [ [ "as"; i = a_LIDENT -> i
        | -> ""
      ] ]
    ;
    opt_polyt:
      [ [ ":"; t = poly_type -> t
        | -> <:ctyp<>> ] ]
    ;
    cvalue_binding:
      [ [ "="; e = expr -> e
        | ":"; "type"; t1 = unquoted_typevars; "." ; t2 = ctyp ; "="; e = expr -> 
	let u = Ast.TyTypePol _loc t1 t2 in
	<:expr< ($e : $u) >>
        | ":"; t = poly_type; "="; e = expr -> <:expr< ($e : $t) >>
        | ":"; t = poly_type; ":>"; t2 = ctyp; "="; e = expr ->
            match t with
            [ <:ctyp< ! $_ . $_ >> -> raise (Stream.Error "unexpected polytype here")
            | _ -> <:expr< ($e : $t :> $t2) >> ]
        | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e :> $t) >> ] ]
    ;
    label:
      [ [ i = a_LIDENT -> i ] ]
    ;
    class_type:
      [ [ `ANTIQUOT (""|"ctyp"|"anti" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ct = class_type_longident_and_param -> ct
        | "object"; cst = opt_class_self_type; csg = class_signature; "end" ->
            <:class_type< object ($cst) $csg end >> ] ]
    ;
    class_type_longident_and_param:
      [ [ i = class_type_longident; "["; t = comma_ctyp; "]" ->
            <:class_type< $id:i [ $t ] >>
        | i = class_type_longident -> <:class_type< $id:i >> ] ]
    ;
    class_type_plus:
      [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
        <:class_type< [ $t ] -> $ct >>
        | ct = class_type -> ct ] ]
    ;
    opt_class_self_type:
      [ [ "("; t = ctyp; ")" -> t
        | -> <:ctyp<>> ] ]
    ;
    class_signature:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `ANTIQUOT (""|"csg"|"anti"|"list" as n) s; semi; csg = SELF ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s); $csg >>
        | l = LIST0 [ csg = class_sig_item; semi -> csg ] ->
            Ast.cgSem_of_list l
      ] ]
    ;
    class_sig_item:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $(anti:mk_anti ~c:"class_sig_item" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_sig_item_tag
        | "inherit"; cs = class_type ->
            <:class_sig_item< inherit $cs >>
        | value_val; mf = opt_mutable; mv = opt_virtual;
          l = label; ":"; t = ctyp ->
            <:class_sig_item< value $mutable:mf $virtual:mv $l : $t >>
        | "method"; "virtual"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | "method"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method $private:pf $l : $t >>
        | "method"; pf = opt_private; "virtual"; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf $l : $t >>
        | type_constraint; t1 = ctyp; "="; t2 = ctyp ->
            <:class_sig_item< type $t1 = $t2 >> ] ]
    ;
    type_constraint:
      [ [ "type" | "constraint" -> () ] ]
    ;
    class_description:
      [ [ cd1 = SELF; "and"; cd2 = SELF ->
            <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x ->
            Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ci = class_info_for_class_type; ":"; ct = class_type_plus ->
            <:class_type< $ci : $ct >>
      ] ]
    ;
    class_type_declaration:
      [ LEFTA
        [ cd1 = SELF; "and"; cd2 = SELF ->
          <:class_type< $cd1 and $cd2 >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $(anti:mk_anti ~c:"class_type" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ci = class_info_for_class_type; "="; ct = class_type ->
            <:class_type< $ci = $ct >>
      ] ]
    ;
    field_expr_list:
      [ [ b1 = field_expr; ";"; b2 = SELF -> <:rec_binding< $b1 ; $b2 >>
        | b1 = field_expr; ";"            -> b1
        | b1 = field_expr                 -> b1
      ] ];
    field_expr:
      [ [ `ANTIQUOT (""|"bi"|"anti" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:rec_binding< $(anti:mk_anti ~c:"rec_binding" n s) >>
        | l = label; "="; e = expr LEVEL "top" ->
            <:rec_binding< $lid:l = $e >> ] ]
    ;
    meth_list:
      [ [ m = meth_decl; ";"; (ml, v) = SELF  -> (<:ctyp< $m; $ml >>, v)
        | m = meth_decl; ";"; v = opt_dot_dot -> (m, v)
        | m = meth_decl; v = opt_dot_dot      -> (m, v)
      ] ]
    ;
    meth_decl:
      [ [ `ANTIQUOT (""|"typ" as n) s        -> <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s          -> <:ctyp< $(anti:mk_anti ~c:"ctyp;" n s) >>
        | `QUOTATION x                       -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | lab = a_LIDENT; ":"; t = poly_type -> <:ctyp< $lid:lab : $t >> ] ]
    ;
    opt_meth_list:
      [ [ (ml, v) = meth_list -> <:ctyp< < $ml $(..:v) > >>
        | v = opt_dot_dot     -> <:ctyp< < $(..:v) > >>
      ] ]
    ;
    poly_type:
      [ [ t = ctyp -> t ] ]
    ;
    package_type:
      [ [ p = module_type -> p ] ]
    ;
    typevars:
      [ LEFTA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>
      ] ]
    ;
    unquoted_typevars:
      [ LEFTA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | i = a_ident -> <:ctyp< $lid:i >>
      ] ]
    ;

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
    ;
    amp_ctyp:
      [ [ t1 = SELF; "&"; t2 = SELF -> <:ctyp< $t1 & $t2 >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $(anti:mk_anti ~c:"ctyp&" n s) >>
        | t = ctyp -> t
      ] ]
    ;
    name_tags:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | t1 = SELF; t2 = SELF -> <:ctyp< $t1 $t2 >>
        | "`"; i = a_ident -> <:ctyp< `$i >>
      ] ]
    ;
    eq_expr:
      [ [ "="; e = expr -> fun i p -> <:patt< ? $i : ($p = $e) >>
        | -> fun i p -> <:patt< ? $i : ($p) >> ] ]
    ;
    patt_tcon:
      [ [ p = patt; ":"; t = ctyp -> <:patt< ($p : $t) >>
        | p = patt -> p ] ]
    ;
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
    ;
    ipatt_tcon:
      [ [ p = ipatt; ":"; t = ctyp -> <:patt< ($p : $t) >>
        | p = ipatt -> p ] ]
    ;
    direction_flag:
      [ [ "to" -> <:direction_flag< to >>
        | "downto" -> <:direction_flag< downto >>
        | `ANTIQUOT ("to"|"anti" as n) s -> Ast.DiAnt (mk_anti n s) ] ]
    ;
    opt_private:
      [ [ "private" -> <:private_flag< private >>
        | `ANTIQUOT ("private"|"anti" as n) s -> Ast.PrAnt (mk_anti n s)
        | -> <:private_flag<>>
      ] ]
    ;
    opt_mutable:
      [ [ "mutable" -> <:mutable_flag< mutable >>
        | `ANTIQUOT ("mutable"|"anti" as n) s -> Ast.MuAnt (mk_anti n s)
        | -> <:mutable_flag<>>
      ] ]
    ;
    opt_virtual:
      [ [ "virtual" -> <:virtual_flag< virtual >>
        | `ANTIQUOT ("virtual"|"anti" as n) s -> Ast.ViAnt (mk_anti n s)
        | -> <:virtual_flag<>>
      ] ]
    ;
    opt_dot_dot:
      [ [ ".." -> <:row_var_flag< .. >>
        | `ANTIQUOT (".."|"anti" as n) s -> Ast.RvAnt (mk_anti n s)
        | -> <:row_var_flag<>>
      ] ]
    ;
    opt_rec:
      [ [ "rec" -> <:rec_flag< rec >>
        | `ANTIQUOT ("rec"|"anti" as n) s -> Ast.ReAnt (mk_anti n s)
        | -> <:rec_flag<>>
      ] ]
    ;
    opt_override:
      [ [ "!" -> <:override_flag< ! >>
        | `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | -> <:override_flag<>>
      ] ]
    ;
    opt_expr:
      [ [ e = expr -> e
        | -> <:expr<>>
      ] ]
    ;
    interf:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:sig_item< # $n $dp >> ], stopped_at _loc)
        | si = sig_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    ;
    sig_items:
      [ [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s) >>
        | `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s; semi; sg = SELF ->
            <:sig_item< $(anti:mk_anti n ~c:"sig_item" s); $sg >>
        | l = LIST0 [ sg = sig_item; semi -> sg ] -> Ast.sgSem_of_list l
      ] ]
    ;
    implem:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None)
      ] ]
    ;
    str_items:
      [ [ `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s) >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s; semi; st = SELF ->
            <:str_item< $(anti:mk_anti n ~c:"str_item" s); $st >>
        | l = LIST0 [ st = str_item; semi -> st ] -> Ast.stSem_of_list l
      ] ]
    ;
    top_phrase:
      [ [ ph = phrase -> Some ph
        | `EOI -> None
      ] ]
    ;
    use_file:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n $dp >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None)
      ] ]
    ;
    phrase:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            <:str_item< # $n $dp >>
        | st = str_item; semi -> st
      ] ]
    ;
    a_INT:
      [ [ `ANTIQUOT (""|"int"|"`int" as n) s -> mk_anti n s
        | `INT _ s -> s ] ]
    ;
    a_INT32:
      [ [ `ANTIQUOT (""|"int32"|"`int32" as n) s -> mk_anti n s
        | `INT32 _ s -> s ] ]
    ;
    a_INT64:
      [ [ `ANTIQUOT (""|"int64"|"`int64" as n) s -> mk_anti n s
        | `INT64 _ s -> s ] ]
    ;
    a_NATIVEINT:
      [ [ `ANTIQUOT (""|"nativeint"|"`nativeint" as n) s -> mk_anti n s
        | `NATIVEINT _ s -> s ] ]
    ;
    a_FLOAT:
      [ [ `ANTIQUOT (""|"flo"|"`flo" as n) s -> mk_anti n s
        | `FLOAT _ s -> s ] ]
    ;
    a_CHAR:
      [ [ `ANTIQUOT (""|"chr"|"`chr" as n) s -> mk_anti n s
        | `CHAR _ s -> s ] ]
    ;
    a_UIDENT:
      [ [ `ANTIQUOT (""|"uid" as n) s -> mk_anti n s
        | `UIDENT s -> s ] ]
    ;
    a_LIDENT:
      [ [ `ANTIQUOT (""|"lid" as n) s -> mk_anti n s
        | `LIDENT s -> s ] ]
    ;
    a_LABEL:
      [ [ "~"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `LABEL s -> s ] ]
    ;
    a_OPTLABEL:
      [ [ "?"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `OPTLABEL s -> s ] ]
    ;
    a_STRING:
      [ [ `ANTIQUOT (""|"str"|"`str" as n) s -> mk_anti n s
        | `STRING _ s -> s ] ]
    ;
    string_list:
      [ [ `ANTIQUOT (""|"str_list") s -> Ast.LAnt (mk_anti "str_list" s)
        | `STRING _ x; xs = string_list -> Ast.LCons x xs
        | `STRING _ x -> Ast.LCons x Ast.LNil ] ]
    ;
    value_let:
      [ [ "value" -> () ] ]
    ;
    value_val:
      [ [ "value" -> () ] ]
    ;
    semi:
      [ [ ";" -> () ] ]
    ;
    expr_quot:
      [ [ e1 = expr; ","; e2 = comma_expr -> <:expr< $e1, $e2 >>
        | e1 = expr; ";"; e2 = sem_expr -> <:expr< $e1; $e2 >>
        | e = expr -> e
        | -> <:expr<>>
      ] ]
    ;
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
    ;
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
        | -> <:ctyp<>>
      ] ]
    ;
    more_ctyp:
      [ [ "mutable"; x = SELF -> <:ctyp< mutable $x >>
        | "`"; x = a_ident -> <:ctyp< `$x >>
        | x = ctyp -> x
        | x = type_parameter -> x
      ] ]
    ;
    str_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:str_item< # $n $dp >>
        | st1 = str_item; semi; st2 = SELF ->
            match st2 with
            [ <:str_item<>> -> st1
            | _ -> <:str_item< $st1; $st2 >> ]
        | st = str_item -> st
        | -> <:str_item<>> ] ]
    ;
    sig_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:sig_item< # $n $dp >>
        | sg1 = sig_item; semi; sg2 = SELF ->
            match sg2 with
            [ <:sig_item<>> -> sg1
            | _ -> <:sig_item< $sg1; $sg2 >> ]
        | sg = sig_item -> sg
        | -> <:sig_item<>> ] ]
    ;
    module_type_quot:
      [ [ x = module_type -> x
        | -> <:module_type<>>
      ] ]
    ;
    module_expr_quot:
      [ [ x = module_expr -> x
        | -> <:module_expr<>>
      ] ]
    ;
    match_case_quot:
      [ [ x = LIST0 match_case0 SEP "|" -> <:match_case< $list:x >>
        | -> <:match_case<>> ] ]
    ;
    binding_quot:
      [ [ x = binding -> x
        | -> <:binding<>>
      ] ]
    ;
    rec_binding_quot:
      [ [ x = label_expr_list -> x
        | -> <:rec_binding<>> ] ]
    ;
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
        | -> <:module_binding<>>
      ] ]
    ;
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
        | "("; i = SELF; ")" -> i
      ] ]
    ;
    class_expr_quot:
      [ [ ce1 = SELF; "and"; ce2 = SELF -> <:class_expr< $ce1 and $ce2 >>
        | ce1 = SELF; "="; ce2 = SELF -> <:class_expr< $ce1 = $ce2 >>
        | "virtual"; (i, ot) = class_name_and_param ->
            <:class_expr< virtual $lid:i [ $ot ] >>
        | `ANTIQUOT ("virtual" as n) s; i = ident; ot = opt_comma_ctyp ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
            <:class_expr< $virtual:anti $id:i [ $ot ] >>
        | x = class_expr -> x
        | -> <:class_expr<>>
      ] ]
    ;
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
        | -> <:class_type<>>
      ] ]
    ;
    class_str_item_quot:
      [ [ x1 = class_str_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_str_item<>> -> x1
          | _ -> <:class_str_item< $x1; $x2 >> ]
        | x = class_str_item -> x
        | -> <:class_str_item<>> ] ]
    ;
    class_sig_item_quot:
      [ [ x1 = class_sig_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_sig_item<>> -> x1
          | _ -> <:class_sig_item< $x1; $x2 >> ]
        | x = class_sig_item -> x
        | -> <:class_sig_item<>> ] ]
    ;
    with_constr_quot:
      [ [ x = with_constr -> x
        | -> <:with_constr<>> ] ]
    ;
    rec_flag_quot: [ [ x = opt_rec -> x ] ];
    direction_flag_quot: [ [ x = direction_flag -> x ] ];
    mutable_flag_quot: [ [ x = opt_mutable -> x ] ];
    private_flag_quot: [ [ x = opt_private -> x ] ];
    virtual_flag_quot: [ [ x = opt_virtual -> x ] ];
    row_var_flag_quot: [ [ x = opt_dot_dot -> x ] ];
    override_flag_quot: [ [ x = opt_override -> x ] ];
    patt_eoi:
      [ [ x = patt; `EOI -> x ] ]
    ;
    expr_eoi:
      [ [ x = expr; `EOI -> x ] ]
    ;
  END;

end;

module IdRevisedParserParser : Sig.Id = struct
  value name = "Camlp4OCamlRevisedParserParser";
  value version = Sys.ocaml_version;
end;

module MakeRevisedParserParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  type spat_comp =
    [ SpTrm of Loc.t and Ast.patt and option Ast.expr
    | SpNtr of Loc.t and Ast.patt and Ast.expr
    | SpStr of Loc.t and Ast.patt ]
  ;
  type sexp_comp =
    [ SeTrm of Loc.t and Ast.expr | SeNtr of Loc.t and Ast.expr ]
  ;

  value stream_expr = Gram.Entry.mk "stream_expr";
  value stream_begin = Gram.Entry.mk "stream_begin";
  value stream_end = Gram.Entry.mk "stream_end";
  value stream_quot = Gram.Entry.mk "stream_quot";
  value parser_case = Gram.Entry.mk "parser_case";
  value parser_case_list = Gram.Entry.mk "parser_case_list";

  value strm_n = "__strm";
  value peek_fun _loc = <:expr< Stream.peek >>;
  value junk_fun _loc = <:expr< Stream.junk >>;

  (* Parsers. *)
  (* In syntax generated, many cases are optimisations. *)

  value rec pattern_eq_expression p e =
    match (p, e) with
    [ (<:patt< $lid:a >>, <:expr< $lid:b >>) -> a = b
    | (<:patt< $uid:a >>, <:expr< $uid:b >>) -> a = b
    | (<:patt< $p1 $p2 >>, <:expr< $e1 $e2 >>) ->
        pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
    | _ -> False ]
  ;

  value is_raise e =
    match e with
    [ <:expr< raise $_ >> -> True
    | _ -> False ]
  ;

  value is_raise_failure e =
    match e with
    [ <:expr< raise Stream.Failure >> -> True
    | _ -> False ]
  ;

  value rec handle_failure e =
    match e with
    [ <:expr< try $_ with [ Stream.Failure -> $e] >> ->
        handle_failure e
    | <:expr< match $me with [ $a ] >> ->
        let rec match_case_handle_failure =
          fun
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
    | _ -> False ]
  ;

  value rec subst v e =
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

  value stream_pattern_component skont ckont =
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
        [ Not_found -> <:expr< let $p = $lid:strm_n in $skont >> ] ]
  ;

  value rec stream_pattern _loc epo e ekont =
    fun
    [ [] ->
        match epo with
        [ Some ep -> <:expr< let $ep = Stream.count $lid:strm_n in $e >>
        | _ -> e ]
    | [(spc, err) :: spcl] ->
        let skont =
          let ekont err =
            let str =
              match err with
              [ Some estr -> estr
              | _ -> <:expr< "" >> ]
            in
            <:expr< raise (Stream.Error $str) >>
          in
          stream_pattern _loc epo e ekont spcl
        in
        let ckont = ekont err in stream_pattern_component skont ckont spc ]
  ;

  value stream_patterns_term _loc ekont tspel =
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

  value rec group_terms =
    fun
    [ [([(SpTrm _loc p w, None) :: spcl], epo, e) :: spel] ->
        let (tspel, spel) = group_terms spel in
        ([(p, w, _loc, spcl, epo, e) :: tspel], spel)
    | spel -> ([], spel) ]
  ;

  value rec parser_cases _loc =
    fun
    [ [] -> <:expr< raise Stream.Failure >>
    | spel ->
        match group_terms spel with
        [ ([], [(spcl, epo, e) :: spel]) ->
            stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl
        | (tspel, spel) ->
            stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel ] ]
  ;

  value cparser _loc bpo pc =
    let e = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $e >>
      | None -> e ]
    in
    let p = <:patt< ($lid:strm_n : Stream.t _) >> in
    <:expr< fun $p -> $e >>
  ;

  value cparser_match _loc me bpo pc =
    let pc = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $pc >>
      | None -> pc ]
    in
    let me =
      match me with
      [ <:expr@_loc< $_; $_ >> as e -> <:expr< do { $e } >>
      | e -> e ]
    in
    match me with
    [ <:expr< $lid:x >> when x = strm_n -> e
    | _ -> <:expr< let ($lid:strm_n : Stream.t _) = $me in $e >> ]
  ;

  (* streams *)

  value rec not_computing =
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
    | _ -> False ]
  ;

  value slazy _loc e =
    match e with
    [ <:expr< $f () >> ->
        match f with
        [ <:expr< $lid:_ >> -> f
        | _ -> <:expr< fun _ -> $e >> ]
    | _ -> <:expr< fun _ -> $e >> ]
  ;

  value rec cstream gloc =
    fun
    [ [] -> let _loc = gloc in <:expr< Stream.sempty >>
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
        else <:expr< Stream.lapp $(slazy _loc e) $(cstream gloc secl) >> ]
  ;
  (* Syntax extensions in Revised Syntax grammar *)

  EXTEND Gram
    GLOBAL: expr stream_expr stream_begin stream_end stream_quot
      parser_case parser_case_list;
    expr: LEVEL "top"
      [ [ "parser"; po = OPT parser_ipatt; pcl = parser_case_list ->
            cparser _loc po pcl
        | "match"; e = sequence; "with"; "parser"; po = OPT parser_ipatt;
          pcl = parser_case_list ->
            cparser_match _loc e po pcl
      ] ]
    ;
    parser_case_list:
      [ [ "["; pcl = LIST0 parser_case SEP "|"; "]" -> pcl
        | pc = parser_case -> [pc]
      ] ]
    ;
    parser_case:
      [ [ stream_begin; sp = stream_patt; stream_end; po = OPT parser_ipatt; "->"; e = expr ->
            (sp, po, e) ] ]
    ;
    stream_begin:
      [ [ "[:" -> () ] ]
    ;
    stream_end:
      [ [ ":]" -> () ] ]
    ;
    stream_quot:
      [ [ "`" -> () ] ]
    ;
    stream_expr:
      [ [ e = expr -> e ] ]
    ;
    stream_patt:
      [ [ spc = stream_patt_comp -> [(spc, None)]
        | spc = stream_patt_comp; ";"; sp = stream_patt_comp_err_list ->
            [(spc, None) :: sp]
        | -> [] ] ]
    ;
    stream_patt_comp_err:
      [ [ spc = stream_patt_comp; eo = OPT [ "??"; e = stream_expr -> e ] ->
            (spc, eo) ] ]
    ;
    stream_patt_comp_err_list:
      [ [ spc = stream_patt_comp_err -> [spc]
        | spc = stream_patt_comp_err; ";" -> [spc]
        | spc = stream_patt_comp_err; ";"; sp = stream_patt_comp_err_list ->
            [spc :: sp] ] ]
    ;
    stream_patt_comp:
      [ [ stream_quot; p = patt; eo = OPT [ "when"; e = stream_expr -> e ] -> SpTrm _loc p eo
        | p = patt; "="; e = stream_expr -> SpNtr _loc p e
        | p = patt -> SpStr _loc p ] ]
    ;
    parser_ipatt:
      [ [ i = a_LIDENT -> <:patt< $lid:i >>
        | "_" -> <:patt< _ >>
      ] ]
    ;
    expr: LEVEL "simple"
      [ [ stream_begin; stream_end -> <:expr< $(cstream _loc []) >>
        | stream_begin; sel = stream_expr_comp_list; stream_end ->
            <:expr< $(cstream _loc sel) >> ] ]
    ;
    stream_expr_comp_list:
      [ [ se = stream_expr_comp; ";"; sel = stream_expr_comp_list -> [se :: sel]
        | se = stream_expr_comp; ";" -> [se]
        | se = stream_expr_comp -> [se] ] ]
    ;
    stream_expr_comp:
      [ [ stream_quot; e = stream_expr -> SeTrm _loc e
        | e = stream_expr -> SeNtr _loc e ] ]
    ;
  END;

end;
  
module IdParser : Sig.Id = struct
  value name = "Camlp4OCamlParser";
  value version = Sys.ocaml_version;
end;

module MakeParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  FanConfig.constructors_arity.val := False;

  (*FIXME remove this and use OCaml ones *)
  value bigarray_set _loc var newval =
    match var with
    [ <:expr< Bigarray.Array1.get $arr $c1 >> ->
        Some <:expr< Bigarray.Array1.set $arr $c1 $newval >>
    | <:expr< Bigarray.Array2.get $arr $c1 $c2 >> ->
        Some <:expr< Bigarray.Array2.set $arr $c1 $c2 $newval >>
    | <:expr< Bigarray.Array3.get $arr $c1 $c2 $c3 >> ->
        Some <:expr< Bigarray.Array3.set $arr $c1 $c2 $c3 $newval >>
    | <:expr< Bigarray.Genarray.get $arr [| $coords |] >> ->
        Some <:expr< Bigarray.Genarray.set $arr [| $coords |] $newval >>
    | _ -> None ];
  value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;
  (*FIXME*)

  value conc_seq e1 e2 =
    match (e1, e2) with
    [ (<:expr@_loc< do { $e1 } >>, <:expr< do { $e2 } >>) ->
        <:expr< do { $e1; $e2 } >>
    | (<:expr@_loc< do { $e1 } >>, _) ->
        <:expr< do { $e1; $e2 } >>
    | (_, <:expr@_loc< do { $e2 } >>) ->
        <:expr< do { $e1; $e2 } >>
    | _ ->
        let _loc =
          Loc.merge (Ast.loc_of_expr e1)
                    (Ast.loc_of_expr e2) in
        <:expr< do { $e1; $e2 } >> ];

  value stream_peek_nth n strm =
    loop n (Stream.npeek n strm) where rec loop n =
      fun
      [ [] -> None
      | [(x, _)] -> if n == 1 then Some x else None
      | [_ :: l] -> loop (n - 1) l ]
  ;

  value test_not_dot_nor_lparen =
    Gram.Entry.of_parser "test_not_dot_nor_lparen" (fun strm ->
      match Stream.peek strm with
      [ Some (KEYWORD ("."|"("),_) -> raise Stream.Failure
      | _ -> () ]);

  (* horrible hacks to be able to parse class_types *)

  value test_ctyp_minusgreater =
    Gram.Entry.of_parser "test_ctyp_minusgreater"
      (fun strm ->
        let rec skip_simple_ctyp n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD "->") -> n
          | Some (KEYWORD ("[" | "[<")) ->
              skip_simple_ctyp (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> skip_simple_ctyp (ignore_upto ")" (n + 1) + 1)
          | Some
              (KEYWORD
                ("as" | "'" | ":" | "*" | "." | "#" | "<" | ">" | ".." | ";" |
                "_" | "?")) ->
              skip_simple_ctyp (n + 1)
          | Some (LIDENT _ | UIDENT _) ->
              skip_simple_ctyp (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD prm) when prm = end_kwd -> n
          | Some (KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some _ -> ignore_upto end_kwd (n + 1)
          | None -> raise Stream.Failure ]
        in
        match Stream.peek strm with
        [ Some ((KEYWORD "[" | LIDENT _ | UIDENT _), _) -> skip_simple_ctyp 1
        | Some (KEYWORD "object", _) -> raise Stream.Failure
        | _ -> 1 ])
  ;

  value lident_colon =	 
     Gram.Entry.of_parser "lident_colon"	 
       (fun strm ->	 
         match Stream.npeek 2 strm with	 
         [ [(LIDENT i, _); (KEYWORD ":", _)] ->	 
             do { Stream.junk strm; Stream.junk strm; i }	 
         | _ -> raise Stream.Failure ])	 
   ;

  value rec is_ident_constr_call =
    fun
    [ <:ident< $uid:_ >> -> True
    | <:ident< $_.$i >> -> is_ident_constr_call i
    | _ -> False ];

  value rec is_expr_constr_call =
    fun
    [ <:expr< $id:i >> -> is_ident_constr_call i
    | <:expr< `$_ >> -> True
    | <:expr< $_.$e >> -> is_expr_constr_call e
    | <:expr@_loc< $e $_ >> ->
        let res = is_expr_constr_call e in
        if (not FanConfig.constructors_arity.val) && res then
          Loc.raise _loc (Stream.Error "currified constructor")
        else res
    | _ -> False ];

  DELETE_RULE Gram expr: SELF; "where"; opt_rec; let_binding END;
  DELETE_RULE Gram value_let: "value" END;
  DELETE_RULE Gram value_val: "value" END;
  DELETE_RULE Gram str_item: value_let; opt_rec; binding END;
  DELETE_RULE Gram module_type: "'"; a_ident END;
  DELETE_RULE Gram module_type: SELF; SELF; dummy END;
  DELETE_RULE Gram module_type: SELF; "."; SELF END;
  DELETE_RULE Gram label_expr: label_longident; fun_binding END;
  DELETE_RULE Gram meth_list: meth_decl; opt_dot_dot END;
  DELETE_RULE Gram expr: "let"; opt_rec; binding; "in"; SELF END;
  DELETE_RULE Gram expr: "let"; "module"; a_UIDENT; module_binding0; "in"; SELF END;
  DELETE_RULE Gram expr: "let"; "open"; module_longident; "in"; SELF END;
  DELETE_RULE Gram expr: "fun"; "["; LIST0 match_case0 SEP "|"; "]" END;
  DELETE_RULE Gram expr: "if"; SELF; "then"; SELF; "else"; SELF END;
  DELETE_RULE Gram expr: "do"; do_sequence END;
  DELETE_RULE Gram expr: SELF; SELF END;
  DELETE_RULE Gram expr: "new"; class_longident END;
  DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
  DELETE_RULE Gram expr: "{"; label_expr_list; "}" END;
  DELETE_RULE Gram expr: "{"; "("; SELF; ")"; "with"; label_expr_list; "}" END;
  DELETE_RULE Gram expr: "("; SELF; ","; comma_expr; ")" END;
  DELETE_RULE Gram expr: SELF; ":="; SELF; dummy END;
  DELETE_RULE Gram expr: "~"; a_LIDENT; ":"; SELF END;
  DELETE_RULE Gram expr: "?"; a_LIDENT; ":"; SELF END;
  DELETE_RULE Gram constructor_declarations: a_UIDENT; ":"; ctyp END;
  (* Some other DELETE_RULE are after the grammar *)

  value clear = Gram.Entry.clear;
  clear ctyp;
  clear patt;
  clear a_UIDENT;
  clear type_longident_and_parameters;
  clear type_parameters;
  clear ipatt;
  clear labeled_ipatt;
  clear semi;
  clear do_sequence;
  clear type_kind;
  clear constructor_arg_list;
  clear poly_type;
  clear class_name_and_param;
  clear class_longident_and_param;
  clear class_type_longident_and_param;
  clear class_type_plus;
  clear type_constraint;
  clear comma_patt;
  clear sequence;
  clear sem_expr_for_list;
  clear sem_expr;
  clear label_declaration;
  clear star_ctyp;
  clear match_case;
  clear with_constr;
  clear package_type;
  clear top_phrase;

  EXTEND Gram
    GLOBAL:
      a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT
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
      dummy eq_expr expr expr_eoi expr_quot fun_binding
      fun_def ident ident_quot implem interf ipatt ipatt_tcon label
      label_declaration label_declaration_list label_expr label_expr_list
      label_longident label_patt_list meth_list
      labeled_ipatt let_binding module_binding module_binding0
      module_binding_quot module_declaration module_expr module_expr_quot
      module_longident module_longident_with_app module_rec_declaration
      module_type module_type_quot more_ctyp name_tags opt_as_lident
      opt_class_self_patt opt_class_self_type
      opt_comma_ctyp opt_dot_dot opt_eq_ctyp opt_expr
      opt_meth_list opt_mutable opt_polyt opt_private opt_rec
      opt_virtual opt_when_expr patt patt_as_patt_opt patt_eoi
      patt_quot patt_tcon phrase poly_type row_field
      sem_expr sem_expr_for_list sem_patt sem_patt_for_list semi sequence
      sig_item sig_item_quot sig_items star_ctyp str_item str_item_quot
      str_items top_phrase type_constraint type_declaration
      type_ident_and_parameters type_kind type_longident
      type_longident_and_parameters type_parameter type_parameters typevars
      use_file val_longident value_let value_val with_constr with_constr_quot
      infixop0 infixop1 infixop2 infixop3 infixop4 do_sequence package_type
    ;
    sem_expr:
      [ [ e1 = expr LEVEL "top"; ";"; e2 = SELF -> <:expr< $e1; $e2 >>
        | e = expr LEVEL "top"; ";" -> e
        | e = expr LEVEL "top" -> e ] ]
    ;
    sequence:
      [ [ e = sem_expr -> e ] ]
    ;
    do_sequence:
      [ [ seq = sequence; "done" -> seq
      ] ]
    ;
    sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; el = SELF -> fun acc ->
        <:expr< [ $e :: $(el acc) ] >>
        | e = expr LEVEL "top"; ";" -> fun acc -> <:expr< [ $e :: $acc ] >>
        | e = expr LEVEL "top" -> fun acc -> <:expr< [ $e :: $acc ] >>
      ] ]
    ;
    str_item:
      [ "top"
          [ "let"; r = opt_rec; bi = binding; "in"; x = expr ->
              <:str_item< let $rec:r $bi in $x >>
          | "let"; r = opt_rec; bi = binding ->
              match bi with
              [ <:binding< _ = $e >> -> <:str_item< $exp:e >>
              | _ -> <:str_item< value $rec:r $bi >> ]
          | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr ->
              <:str_item< let module $m = $mb in $e >>
          | "let"; "open"; i = module_longident; "in"; e = expr ->
              <:str_item< let open $id:i in $e >>
      ] ]
    ;
    seq_expr:
      [ [ e1 = expr LEVEL "top"; ";"; e2 = SELF ->
            conc_seq e1 e2
        | e1 = expr LEVEL "top"; ";" -> e1
        | e1 = expr LEVEL "top" -> e1 ] ];
    expr: BEFORE "top"
      [ ";" [ e = seq_expr -> e ] ];
    expr: LEVEL "top"
      [ [ "let"; r = opt_rec; bi = binding; "in";
          x = expr LEVEL ";" ->
            <:expr< let $rec:r $bi in $x >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in";
          e = expr LEVEL ";" ->
            <:expr< let module $m = $mb in $e >>
        | "let"; "open"; i = module_longident; "in"; e = expr LEVEL ";" ->
            <:expr< let open $id:i in $e >>
        | "function"; a = match_case ->
            <:expr< fun [ $a ] >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top";
          "else"; e3 = expr LEVEL "top" ->
            <:expr< if $e1 then $e2 else $e3 >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top" ->
            <:expr< if $e1 then $e2 else () >>
      ] ];
    expr: BEFORE "||"
      [ ","
        [ e1 = SELF; ","; e2 = comma_expr ->
            <:expr< ( $e1, $e2 ) >> ]
      | ":=" NONA
        [ e1 = SELF; ":="; e2 = expr LEVEL "top" ->
            <:expr< $e1.val := $e2 >>
        | e1 = SELF; "<-"; e2 = expr LEVEL "top" ->
            match bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1 := $e2 >> ]
      ] ];
    expr: AFTER "^"
      [ "::" RIGHTA
        [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1 :: $e2] >> ]
      ];
    expr: LEVEL "apply" (* LEFTA *)
      [ [ e1 = SELF; e2 = SELF ->
            match (is_expr_constr_call e1, e2) with
            [ (True, <:expr< ( $tup:e ) >>) ->
                List.fold_left (fun e1 e2 -> <:expr< $e1 $e2 >>) e1
                                (Ast.list_of_expr e [])
            | _ -> <:expr< $e1 $e2 >> ]
      ] ];
    expr: LEVEL "simple" (* LEFTA *)
      [ [ "false" -> <:expr< False >>
        | "true" -> <:expr< True >>
        | "{"; lel = TRY [lel = label_expr_list; "}" -> lel] ->
            <:expr< { $lel } >>
        | "{"; e = TRY [e = expr LEVEL "."; "with" -> e]; lel = label_expr_list; "}" ->
            <:expr< { ($e) with $lel } >>
        | "new"; i = class_longident -> <:expr< new $i >>
      ] ]
    ;
    val_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $(anti:mk_anti ~c:"ident" n s) >>
        | i = a_UIDENT -> <:ident< $uid:i >>
        | i = a_LIDENT -> <:ident< $lid:i >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $(anti:mk_anti ~c:"ident" n s).$i >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i.$j >> ] ]
    ;
    match_case:
      [ [ OPT "|"; l = LIST1 match_case0 SEP "|" -> Ast.mcOr_of_list l ] ]
    ;
    patt_constr:
      [ [ i = module_longident -> <:patt< $id:i >>
        | "`"; s = a_ident -> <:patt< `$s >> ] ]
    ;
    (* Patterns *)
    patt:
      [ "as" LEFTA
        [ p1 = SELF; "as"; i = a_LIDENT -> <:patt< ($p1 as $lid:i) >> ]
      | "|" LEFTA
        [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1 | $p2 >> ]
      | ","
        [ p = SELF; ","; pl = (*FIXME comma_patt*) LIST1 NEXT SEP "," ->
            <:patt< ( $p, $(Ast.paCom_of_list pl) ) >> ]
      | "::" RIGHTA
        [ p1 = SELF; "::"; p2 = SELF -> <:patt< [$p1 :: $p2] >> ]
      | "apply" RIGHTA
        [ p1 = patt_constr; p2 = SELF ->
            match p2 with
            [ <:patt< ( $tup:p ) >> ->
                List.fold_left (fun p1 p2 -> <:patt< $p1 $p2 >>) p1
                                (Ast.list_of_patt p [])
            | _ -> <:patt< $p1 $p2 >> ]
        | "lazy"; p = SELF -> <:patt< lazy $p >>
        | `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | p = patt_constr -> p ]
      | "simple"
        [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $(anti:mk_anti ~c:"patt" n s) >>
        | `ANTIQUOT ("tup" as n) s -> <:patt< ($(tup:<:patt< $(anti:mk_anti ~c:"patt" n s) >>)) >>
        | `ANTIQUOT ("`bool" as n) s -> <:patt< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | i = ident -> <:patt< $id:i >>
        | s = a_INT -> <:patt< $int:s >>
        | s = a_INT32 -> <:patt< $int32:s >>
        | s = a_INT64 -> <:patt< $int64:s >>
        | s = a_NATIVEINT -> <:patt< $nativeint:s >>
        | "-"; s = a_INT -> <:patt< $(int:"-" ^ s) >>
        | "-"; s = a_INT32 -> <:patt< $(int32:"-" ^ s) >>
        | "-"; s = a_INT64 -> <:patt< $(int64:"-" ^ s) >>
        | "-"; s = a_NATIVEINT -> <:patt< $(nativeint:"-" ^ s) >>
        | "-"; s = a_FLOAT -> <:patt< $(flo:"-" ^ s) >>
        | s = a_FLOAT -> <:patt< $(flo:s) >>
        | s = a_STRING -> <:patt< $(str:s) >>
        | s1 = a_CHAR; ".."; s2 = a_CHAR -> <:patt< $chr:s1 .. $chr:s2 >>
        | s = a_CHAR -> <:patt< $chr:s >>
        | "false" -> <:patt< False >>
        | "true" -> <:patt< True >>
        | "["; "]" -> <:patt< [] >>
        | "["; mk_list = sem_patt_for_list; "::"; last = patt; "]" ->
            mk_list last
        | "["; mk_list = sem_patt_for_list; "]" ->
            mk_list <:patt< [] >>
        | "[|"; "|]" -> <:patt< [||] >>
        | "[|"; pl = sem_patt; "|]" -> <:patt< [| $pl |] >>
        | "{"; pl = label_patt_list; "}" -> <:patt< { $pl } >>
        | "("; ")" -> <:patt< () >>
        | "("; "module"; m = a_UIDENT; ")" -> <:patt< (module $m) >>
        | "("; "module"; m = a_UIDENT; ":"; pt = package_type; ")" ->
            <:patt< ((module $m) : (module $pt)) >>
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p : $t) >>
        | "("; p = patt; ")" -> <:patt< $p >>
        | "_" -> <:patt< _ >>
        | "`"; s = a_ident -> <:patt< ` $s >>
        | "#"; i = type_longident -> <:patt< # $i >> ] ]
    ;
    comma_expr:
      [ [ e1 = expr LEVEL ":="; ","; e2 = SELF -> <:expr< $e1, $e2 >>
        | e1 = expr LEVEL ":=" -> e1 ] ]
    ;
    (* comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1$, $p2$ >>
        | p = patt LEVEL ".." -> p ] ]
    ;                                                           *)
    type_constraint:
      [ [ "constraint" -> () ] ]
    ;
    with_constr:
      [ LEFTA
        [ wc1 = SELF; "and"; wc2 = SELF -> <:with_constr< $wc1 and $wc2 >>
        | `ANTIQUOT (""|"with_constr"|"anti"|"list" as n) s ->
            <:with_constr< $(anti:mk_anti ~c:"with_constr" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.with_constr_tag
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; "="; t = opt_private_ctyp ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) = $t >>
        | "type"; t1 = type_longident_and_parameters; "="; t2 = opt_private_ctyp ->
            <:with_constr< type $t1 = $t2 >>
        | "module"; i1 = module_longident; "="; i2 = module_longident_with_app ->
            <:with_constr< module $i1 = $i2 >>
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; ":="; t = opt_private_ctyp ->
            <:with_constr< type $(anti:mk_anti ~c:"ctyp" n s) := $t >>
        | "type"; t1 = type_longident_and_parameters; ":="; t2 = opt_private_ctyp ->
            <:with_constr< type $t1 := $t2 >>
        | "module"; i1 = module_longident; ":="; i2 = module_longident_with_app ->
            <:with_constr< module $i1 := $i2 >> ] ]
    ;
    package_type:
      [ [ i = module_longident_with_app -> <:module_type< $id:i >>
        | i = module_longident_with_app; "with"; cs = package_type_cstrs ->
            <:module_type< $id:i with $cs >>
      ] ]
    ;
    package_type_cstr:
      [ [ "type"; i = ident; "="; ty = ctyp ->
            <:with_constr< type $id:i = $ty >>
      ] ]
    ;
    package_type_cstrs:
      [ [ c = package_type_cstr -> c
        | c = package_type_cstr; "and"; cs = package_type_cstrs ->
            <:with_constr< $c and $cs >>
      ] ]
    ;
    opt_private_ctyp:
      [ [ "private"; t = ctyp -> <:ctyp< private $t >>
        | t = ctyp -> t ] ]
    ;
    class_type_plus:
      [ [ i = lident_colon; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ~ $i : $t ] -> $ct >>
        | "?"; i = a_LIDENT; ":"; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ? $i : $t ] -> $ct >>
        | i = OPTLABEL (* FIXME inline a_OPTLABEL *); t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ? $i : $t ] -> $ct >>
        | test_ctyp_minusgreater; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ $t ] -> $ct >>
        | ct = class_type -> ct ] ]
    ;
    class_type_longident_and_param:
      [ [ "["; t = comma_ctyp; "]"; i = class_type_longident ->
            <:class_type< $id:i [ $t ] >>
        | i = class_type_longident -> <:class_type< $id:i >> ] ]
    ;
    class_longident_and_param:
      [ [ "["; t = comma_ctyp; "]"; ci = class_longident ->
          <:class_expr< $id:ci [ $t ] >>
        | ci = class_longident -> <:class_expr< $id:ci >>
      ] ]
    ;
    class_name_and_param:
      [ [ "["; x = comma_type_parameter; "]"; i = a_LIDENT -> (i, x)
        | i = a_LIDENT -> (i, <:ctyp<>>)
      ] ]
    ;
    ctyp:
      [ [ t1 = SELF; "as"; "'"; i = a_ident -> <:ctyp< $t1 as '$i >> ]
      | "arrow" RIGHTA
        [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1 -> $t2 >>
        | i = TRY [i = a_LIDENT; ":" -> i]; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ~ $i : $t1 ) -> $t2 >>
        | i = a_OPTLABEL; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ? $i : $t1 ) -> $t2 >>
        | "?"; i = a_LIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ? $i : $t1 ) -> $t2 >> ]
      | "star"
        [ t = SELF; "*"; tl = star_ctyp ->
            <:ctyp< ( $t * $tl ) >> ]
      | "ctyp1"
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t2 $t1 >> ]
      | "ctyp2"
        [ t1 = SELF; "."; t2 = SELF ->
            try <:ctyp< $(id:Ast.ident_of_ctyp t1).$(id:Ast.ident_of_ctyp t2) >>
            with [ Invalid_argument s -> raise (Stream.Error s) ]
        | t1 = SELF; "("; t2 = SELF; ")" ->
            let t = <:ctyp< $t1 $t2 >> in
            try <:ctyp< $(id:Ast.ident_of_ctyp t) >>
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
      | "simple"
        [ "'"; i = a_ident -> <:ctyp< '$i >>
        | "_" -> <:ctyp< _ >>
        | i = a_LIDENT -> <:ctyp< $lid:i >>
        | i = a_UIDENT -> <:ctyp< $uid:i >>
        | `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("tup" as n) s ->
            <:ctyp< ($(tup:<:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>)) >>
        | `ANTIQUOT ("id" as n) s ->
            <:ctyp< $(id:<:ident< $(anti:mk_anti ~c:"ident" n s) >>) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "("; t = SELF; ","; mk = comma_ctyp_app; ")";
          i = ctyp LEVEL "ctyp2" ->
            mk <:ctyp< $i $t >>
        | "("; t = SELF; ")" -> <:ctyp< $t >>
        | "#"; i = class_longident -> <:ctyp< # $i >>
        | "<"; t = opt_meth_list; ">" -> t
        | "["; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ = $rfl ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $(<:ctyp<>>) ] >>
        | "["; ">"; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ > $rfl ] >>
        | "[<"; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl ] >>
        | "[<"; OPT "|"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl > $ntl ] >>
        | "("; "module"; p = package_type; ")" -> <:ctyp< (module $p) >>
        ] ]
    ;
    meth_list:
      [ [ m = meth_decl -> (m, <:row_var_flag<>>) ] ];
    comma_ctyp_app:
      [ [ t1 = ctyp; ","; t2 = SELF -> fun acc -> t2 <:ctyp< $acc $t1 >>
        | t = ctyp -> fun acc -> <:ctyp< $acc $t >>
      ] ]
    ;
    star_ctyp:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp*" n s) >>
        | t1 = ctyp LEVEL "ctyp1"; "*"; t2 = SELF ->
            <:ctyp< $t1 * $t2 >>
        | t = ctyp LEVEL "ctyp1" -> t
      ] ]
    ;
    constructor_declarations:
      [ [ s = a_UIDENT; ":"; t = constructor_arg_list ; "->" ; ret = ctyp ->
            <:ctyp< $uid:s : ($t -> $ret) >>
        | s = a_UIDENT; ":"; ret = constructor_arg_list ->
           match Ast.list_of_ctyp ret [] with
               [ [c] -> <:ctyp< $uid:s : $c >>
               | _ -> raise (Stream.Error "invalid generalized constructor type") ]
        ] ]
    ;
    semi:
      [ [ ";;" -> () | -> () ] ]
    ;
    ipatt:
      [ [ p = patt -> p ] ]
    ;
    type_longident_and_parameters:
      [ [ "("; tpl = type_parameters; ")"; i = type_longident ->
            tpl <:ctyp< $id:i >>
        | tp = type_parameter; i = type_longident ->
            <:ctyp< $id:i $tp >>
        | i = type_longident ->
            <:ctyp< $id:i >>
      ] ]
    ;
    type_parameters:
      [ [ t1 = type_parameter; ","; t2 = SELF ->
            fun acc -> t2 <:ctyp< $acc $t1 >>
        | t = type_parameter -> fun acc -> <:ctyp< $acc $t >>
      ] ]
    ;

    optional_type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $(anti:mk_anti n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "+"; "_" -> Ast.TyAnP _loc 
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i >>
        | "-"; "_" -> Ast.TyAnM _loc
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i >>
        | "_" -> Ast.TyAny _loc
        | "'"; i = a_ident -> <:ctyp< '$lid:i >>

 ] ]
    ;

    type_ident_and_parameters:
      [ [ "("; tpl = LIST1 optional_type_parameter SEP ","; ")"; i = a_LIDENT -> (i, tpl)
        | t = optional_type_parameter; i = a_LIDENT -> (i, [t])
        | i = a_LIDENT -> (i, [])
      ] ]
    ;
    type_kind:
      [ [ "private"; tk = type_kind -> <:ctyp< private $tk >>
        | (x, t) = TRY [x = OPT "|"; t = constructor_declarations;
                        test_not_dot_nor_lparen -> (x, t)] ->
            (* If there is no "|" and [t] is an antiquotation,
               then it is not a sum type. *)
            match (x, t) with
            [ (None, Ast.TyAnt _) -> t
            | _ -> <:ctyp< [ $t ] >> ]
        | t = TRY ctyp -> <:ctyp< $t >>
        | t = TRY ctyp; "="; "private"; tk = type_kind ->
            <:ctyp< $t == private $tk >>
        | t1 = TRY ctyp; "="; "{"; t2 = label_declaration_list; "}" ->
            <:ctyp< $t1 == { $t2 } >>
        | t1 = TRY ctyp; "="; OPT "|"; t2 = constructor_declarations ->
            <:ctyp< $t1 == [ $t2 ] >>
        | "{"; t = label_declaration_list; "}" ->
            <:ctyp< { $t } >> ] ]
    ;
    ctyp_quot:
      [ [ "private"; t = ctyp_quot -> <:ctyp< private $t >>
        | "|"; t = constructor_declarations -> <:ctyp< [ $t ] >>
        | x = more_ctyp; "="; y = ctyp_quot -> <:ctyp< $x == $y >>
        | "{"; t = label_declaration_list; "}" -> <:ctyp< { $t } >>
      ] ]
    ;
    module_expr: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:module_expr< $i $j >> ] ]
    ;
    ident_quot: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i $j >> ] ]
    ;
    module_longident_with_app: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i $j >> ] ]
    ;
    type_longident: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i $j >> ] ]
    ;
    constructor_arg_list:
      [ [ t1 = SELF; "*"; t2 = SELF -> <:ctyp< $t1 and $t2 >>
        | t = ctyp LEVEL "ctyp1" -> t
      ] ]
    ;
    value_let:
      [ [ "let" -> () ] ]
    ;
    value_val:
      [ [ "val" -> () ] ]
    ;
    label_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $(anti:mk_anti ~c:"ctyp" n s) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_LIDENT; ":"; t = poly_type ->  <:ctyp< $lid:s : $t >>
        | "mutable"; s = a_LIDENT; ":"; t = poly_type ->
            <:ctyp< $lid:s : mutable $t >>
      ] ]
    ;
    poly_type:
      [ [ t1 = TRY [t = typevars; "." -> t]; t2 = ctyp ->
            <:ctyp< ! $t1 . $t2 >>
        | t = TRY ctyp -> t ] ]
    ;
    labeled_ipatt:
      [ [ i = a_LABEL; p = patt LEVEL "simple" ->
            <:patt< ~ $i : $p >>
        | "~"; i = a_LIDENT -> <:patt< ~ $i >>
        | "~"; "("; i = a_LIDENT; ")" ->
            <:patt< ~ $i >>
        | "~"; "("; i = a_LIDENT; ":"; t = ctyp; ")" ->
            <:patt< ~ $i : ($lid:i : $t) >>
        | i = a_OPTLABEL; j = a_LIDENT -> (* ?a:b <> ?a : b *)
            <:patt< ? $i : ($lid:j) >>
        | i = a_OPTLABEL; "_" ->
            <:patt< ? $i : (_) >>
        | i = a_OPTLABEL; "("; p = patt; ")" ->
            <:patt< ? $i : ($p) >>
        | i = a_OPTLABEL; "("; p = patt; "="; e = expr; ")" ->
            <:patt< ? $i : ( $p = $e ) >>
        | i = a_OPTLABEL; "("; p = patt; ":"; t = ctyp; ")" ->
            <:patt< ? $i : ( $p : $t ) >>
        | i = a_OPTLABEL; "("; p = patt; ":"; t = ctyp; "=";
          e = expr; ")" ->
            <:patt< ? $i : ( $p : $t = $e ) >>
        | "?"; i = a_LIDENT -> <:patt< ? $i >>
        | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
            <:patt< ? ( $lid:i = $e ) >>
        | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
            <:patt< ? ( $lid:i : $t = $e ) >>
        | "?"; "("; i = a_LIDENT; ")" ->
            <:patt< ? $i >>
        | "?"; "("; i = a_LIDENT; ":"; t = ctyp; ")" ->
            <:patt< ? ( $lid:i : $t ) >>
        | p = patt LEVEL "simple" -> p
      ] ]
    ;
    label_expr:
      [ [ i = label_longident; "="; e = expr LEVEL "top" ->
            <:rec_binding< $i = $e >> ] ]
    ;
    a_UIDENT:
      [ [ `ANTIQUOT (""|"uid" as n) s -> mk_anti n s
        | `UIDENT "True" -> " True"
        | `UIDENT "False" -> " False"
        | `UIDENT s -> s
      ] ]
    ;
    top_phrase:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; ";;" ->
            Some <:str_item< # $n $dp >>
        | l = LIST1 str_item; ";;" -> Some (Ast.stSem_of_list l)
        | `EOI -> None
      ] ]
    ;
  END;

  (* Some other DELETE_RULE are before the grammar *)
  DELETE_RULE Gram module_longident_with_app: "("; SELF; ")" END;
  DELETE_RULE Gram type_longident: "("; SELF; ")" END;
  DELETE_RULE Gram ident_quot: "("; SELF; ")" END;
  DELETE_RULE Gram module_longident_with_app: SELF; SELF END;
  DELETE_RULE Gram type_longident: SELF; SELF END;
  DELETE_RULE Gram ident_quot: SELF; SELF END;
  DELETE_RULE Gram module_expr: SELF; SELF END;
end;  


module IdParserParser : Sig.Id = struct
  value name = "Camlp4OCamlParserParser";
  value version = Sys.ocaml_version;
end;

module MakeParserParser (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  module M = MakeRevisedParserParser  Syntax;
  open M;

  Gram.Entry.clear stream_expr;
  Gram.Entry.clear stream_begin;
  Gram.Entry.clear stream_end;
  Gram.Entry.clear stream_quot;
  Gram.Entry.clear parser_case_list;

  EXTEND Gram
    stream_expr:
      [ [ e = expr LEVEL "top" -> e ] ]
    ;
    stream_begin:
      [ [ "[<" -> () ] ]
    ;
    stream_end:
      [ [ ">]" -> () ] ]
    ;
    stream_quot:
      [ [ "'" -> () ] ]
    ;
    parser_case_list:
      [ [ OPT "|"; pcl = LIST1 parser_case SEP "|" -> pcl ] ]
    ;
  END;
end;
  
module IdQuotationCommon = struct (* FIXME unused here *)
  value name = "Camlp4QuotationCommon";
  value version = Sys.ocaml_version;
end;

module MakeQuotationCommon (Syntax : Sig.Camlp4Syntax)
            (TheAntiquotSyntax : (Sig.Parser Syntax.Ast).SIMPLE)
= struct
  open Sig;
  include Syntax; (* Be careful an AntiquotSyntax module appears here *)

  module MetaLocHere = Ast.Meta.MetaLoc;
  module MetaLoc = struct
    module Ast = Ast;
    value loc_name = ref None;
    value meta_loc_expr _loc loc =
      match loc_name.val with
      [ None -> <:expr< $(lid:Loc.name.val) >>
      | Some "here" -> MetaLocHere.meta_loc_expr _loc loc
      | Some x -> <:expr< $lid:x >> ];
    value meta_loc_patt _loc _ = <:patt< _ >>;
  end;
  module MetaAst = Ast.Meta.Make MetaLoc;
  module ME = MetaAst.Expr;
  module MP = MetaAst.Patt;

  value is_antiquot s =
    let len = String.length s in
    len > 2 && s.[0] = '\\' && s.[1] = '$';

  value handle_antiquot_in_string s term parse loc decorate =
    if is_antiquot s then
      let pos = String.index s ':' in
      let name = String.sub s 2 (pos - 2)
      and code = String.sub s (pos + 1) (String.length s - pos - 1) in
      decorate name (parse loc code)
    else term;

  value antiquot_expander = object
    inherit Ast.map as super;
    method patt = fun
      [ <:patt@_loc< $anti:s >> | <:patt@_loc< $str:s >> as p ->
          let mloc _loc = MetaLoc.meta_loc_patt _loc _loc in
          handle_antiquot_in_string s p TheAntiquotSyntax.parse_patt _loc (fun n p ->
            match n with
            [ "antisig_item" -> <:patt< Ast.SgAnt $(mloc _loc) $p >>
            | "antistr_item" -> <:patt< Ast.StAnt $(mloc _loc) $p >>
            | "antictyp" -> <:patt< Ast.TyAnt $(mloc _loc) $p >>
            | "antipatt" -> <:patt< Ast.PaAnt $(mloc _loc) $p >>
            | "antiexpr" -> <:patt< Ast.ExAnt $(mloc _loc) $p >>
            | "antimodule_type" -> <:patt< Ast.MtAnt $(mloc _loc) $p >>
            | "antimodule_expr" -> <:patt< Ast.MeAnt $(mloc _loc) $p >>
            | "anticlass_type" -> <:patt< Ast.CtAnt $(mloc _loc) $p >>
            | "anticlass_expr" -> <:patt< Ast.CeAnt $(mloc _loc) $p >>
            | "anticlass_sig_item" -> <:patt< Ast.CgAnt $(mloc _loc) $p >>
            | "anticlass_str_item" -> <:patt< Ast.CrAnt $(mloc _loc) $p >>
            | "antiwith_constr" -> <:patt< Ast.WcAnt $(mloc _loc) $p >>
            | "antibinding" -> <:patt< Ast.BiAnt $(mloc _loc) $p >>
            | "antirec_binding" -> <:patt< Ast.RbAnt $(mloc _loc) $p >>
            | "antimatch_case" -> <:patt< Ast.McAnt $(mloc _loc) $p >>
            | "antimodule_binding" -> <:patt< Ast.MbAnt $(mloc _loc) $p >>
            | "antiident" -> <:patt< Ast.IdAnt $(mloc _loc) $p >>
            | _ -> p ])
      | p -> super#patt p ];
    method expr = fun
      [ <:expr@_loc< $anti:s >> | <:expr@_loc< $str:s >> as e ->
          let mloc _loc = MetaLoc.meta_loc_expr _loc _loc in
          handle_antiquot_in_string s e TheAntiquotSyntax.parse_expr _loc (fun n e ->
            match n with
            [ "`int" -> <:expr< string_of_int $e >>
            | "`int32" -> <:expr< Int32.to_string $e >>
            | "`int64" -> <:expr< Int64.to_string $e >>
            | "`nativeint" -> <:expr< Nativeint.to_string $e >>
            | "`flo" -> <:expr< FanUtil.float_repres $e >>
            | "`str" -> <:expr< Ast.safe_string_escaped $e >>
            | "`chr" -> <:expr< Char.escaped $e >>
            | "`bool" -> <:expr< Ast.IdUid $(mloc _loc) (if $e then "True" else "False") >>
            | "liststr_item" -> <:expr< Ast.stSem_of_list $e >>
            | "listsig_item" -> <:expr< Ast.sgSem_of_list $e >>
            | "listclass_sig_item" -> <:expr< Ast.cgSem_of_list $e >>
            | "listclass_str_item" -> <:expr< Ast.crSem_of_list $e >>
            | "listmodule_expr" -> <:expr< Ast.meApp_of_list $e >>
            | "listmodule_type" -> <:expr< Ast.mtApp_of_list $e >>
            | "listmodule_binding" -> <:expr< Ast.mbAnd_of_list $e >>
            | "listbinding" -> <:expr< Ast.biAnd_of_list $e >>
            | "listbinding;" -> <:expr< Ast.biSem_of_list $e >>
            | "listrec_binding" -> <:expr< Ast.rbSem_of_list $e >>
            | "listclass_type" -> <:expr< Ast.ctAnd_of_list $e >>
            | "listclass_expr" -> <:expr< Ast.ceAnd_of_list $e >>
            | "listident" -> <:expr< Ast.idAcc_of_list $e >>
            | "listctypand" -> <:expr< Ast.tyAnd_of_list $e >>
            | "listctyp;" -> <:expr< Ast.tySem_of_list $e >>
            | "listctyp*" -> <:expr< Ast.tySta_of_list $e >>
            | "listctyp|" -> <:expr< Ast.tyOr_of_list $e >>
            | "listctyp," -> <:expr< Ast.tyCom_of_list $e >>
            | "listctyp&" -> <:expr< Ast.tyAmp_of_list $e >>
            | "listwith_constr" -> <:expr< Ast.wcAnd_of_list $e >>
            | "listmatch_case" -> <:expr< Ast.mcOr_of_list $e >>
            | "listpatt," -> <:expr< Ast.paCom_of_list $e >>
            | "listpatt;" -> <:expr< Ast.paSem_of_list $e >>
            | "listexpr," -> <:expr< Ast.exCom_of_list $e >>
            | "listexpr;" -> <:expr< Ast.exSem_of_list $e >>
            | "antisig_item" -> <:expr< Ast.SgAnt $(mloc _loc) $e >>
            | "antistr_item" -> <:expr< Ast.StAnt $(mloc _loc) $e >>
            | "antictyp" -> <:expr< Ast.TyAnt $(mloc _loc) $e >>
            | "antipatt" -> <:expr< Ast.PaAnt $(mloc _loc) $e >>
            | "antiexpr" -> <:expr< Ast.ExAnt $(mloc _loc) $e >>
            | "antimodule_type" -> <:expr< Ast.MtAnt $(mloc _loc) $e >>
            | "antimodule_expr" -> <:expr< Ast.MeAnt $(mloc _loc) $e >>
            | "anticlass_type" -> <:expr< Ast.CtAnt $(mloc _loc) $e >>
            | "anticlass_expr" -> <:expr< Ast.CeAnt $(mloc _loc) $e >>
            | "anticlass_sig_item" -> <:expr< Ast.CgAnt $(mloc _loc) $e >>
            | "anticlass_str_item" -> <:expr< Ast.CrAnt $(mloc _loc) $e >>
            | "antiwith_constr" -> <:expr< Ast.WcAnt $(mloc _loc) $e >>
            | "antibinding" -> <:expr< Ast.BiAnt $(mloc _loc) $e >>
            | "antirec_binding" -> <:expr< Ast.RbAnt $(mloc _loc) $e >>
            | "antimatch_case" -> <:expr< Ast.McAnt $(mloc _loc) $e >>
            | "antimodule_binding" -> <:expr< Ast.MbAnt $(mloc _loc) $e >>
            | "antiident" -> <:expr< Ast.IdAnt $(mloc _loc) $e >>
            | _ -> e ])
      | e -> super#expr e ];
  end;

  value add_quotation name entry mexpr mpatt =
    let entry_eoi = Gram.Entry.mk (Gram.Entry.name entry) in
    let parse_quot_string entry loc s =
      let q = FanConfig.antiquotations.val in
      let () = FanConfig.antiquotations.val := True in
      let res = Gram.parse_string entry loc s in
      let () = FanConfig.antiquotations.val := q in
      res in
    let expand_expr loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi loc s in
      let () = MetaLoc.loc_name.val := loc_name_opt in
      let meta_ast = mexpr loc ast in
      let exp_ast = antiquot_expander#expr meta_ast in
      exp_ast in
    let expand_str_item loc loc_name_opt s =
      let exp_ast = expand_expr loc loc_name_opt s in
      <:str_item@loc< $(exp:exp_ast) >> in
    let expand_patt _loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi _loc s in
      let meta_ast = mpatt _loc ast in
      let exp_ast = antiquot_expander#patt meta_ast in
      match loc_name_opt with
      [ None -> exp_ast
      | Some name ->
        let rec subst_first_loc =
          fun
          [ <:patt@_loc< Ast.$uid:u $_ >> -> <:patt< Ast.$uid:u $lid:name >>
          | <:patt@_loc< $a $b >> -> <:patt< $(subst_first_loc a) $b >>
          | p -> p ] in
        subst_first_loc exp_ast ] in
    do {
      EXTEND Gram
        entry_eoi:
          [ [ x = entry; `EOI -> x ] ]
        ;
      END;
      Quotation.add name Quotation.DynAst.expr_tag expand_expr;
      Quotation.add name Quotation.DynAst.patt_tag expand_patt;
      Quotation.add name Quotation.DynAst.str_item_tag expand_str_item;
    };

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
  value name = "Camlp4QuotationExpander";
  value version = Sys.ocaml_version;
end;

module MakeQuotationExpander (Syntax : Sig.Camlp4Syntax)
= struct
  module M = MakeQuotationCommon Syntax Syntax.AntiquotSyntax;
  include M;
end;

(* value pa_r  = "Camlp4OCamlRevisedParser"; *)    
value pa_r (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdRevisedParser)  (module MakeRevisedParser);

(* value pa_rr = "Camlp4OCamlReloadedParser"; *)
value pa_rr (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdReloadedParser) (module MakeReloadedParser);
  
(* value pa_o  = "Camlp4OCamlParser"; *)
value pa_o (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdParser) (module MakeParser);
  
(* value pa_rp = "Camlp4OCamlRevisedParserParser"; *)
value pa_rp (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdRevisedParserParser)
    (module MakeRevisedParserParser);

(* value pa_op = "Camlp4OCamlParserParser"; *)
value pa_op (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdParserParser) (module MakeParserParser);

value pa_g (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdGrammarParser) (module MakeGrammarParser);

(* value pa_m  = "Camlp4MacroParser"; *)
value pa_m (module P:Sig.PRECAST) =
  let () = P.ocaml_syntax_extension (module IdMacroParser) (module MakeMacroParser) in
  P.ast_filter (module IdMacroParser) (module MakeNothing);

(* value pa_q  = "Camlp4QuotationExpander"; *)
value pa_q (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdQuotationExpander) (module MakeQuotationExpander);
  
(* value pa_rq = "Camlp4OCamlRevisedQuotationExpander";
   *unreflective*, quotation syntax use revised syntax.
 *)
value pa_rq (module P:Sig.PRECAST) =
  let module Gram = P.MakeGram P.Lexer in
  let module M1 = OCamlInitSyntax.Make P.Ast P.Gram P.Quotation in
  let module M2 = MakeRevisedParser M1 in
  let module M3 = MakeQuotationCommon M2 P.Syntax.AntiquotSyntax in ();
  
(* value pa_oq = "Camlp4OCamlOriginalQuotationExpander";
   *unreflective*, quotation syntax use original syntax.
   Build the whole parser used by quotation. 
 *)
value pa_oq (module P:Sig.PRECAST)   =
  let module Gram = P.MakeGram P.Lexer in
  let module M1 = OCamlInitSyntax.Make P.Ast P.Gram P.Quotation in
  let module M2 = MakeRevisedParser M1 in
  let module M3 = MakeParser M2 in
  let module M4 = MakeQuotationCommon M3 P.Syntax.AntiquotSyntax in ();


value pa_l  (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdListComprehension) (module MakeListComprehension);


(* load debug parser for bootstrapping *)  
value pa_debug (module P:Sig.PRECAST) =
  P.ocaml_syntax_extension (module IdDebugParser) (module MakeDebugParser);

  


