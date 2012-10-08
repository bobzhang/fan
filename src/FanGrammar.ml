open Format;
type loc = FanLoc.t;
open Lib;
module MetaAst = Camlp4Ast.Meta.Make Lib.Meta.MetaGhostLoc ;
module Ast = Camlp4Ast;  
type name 'e = { expr : 'e; tvar : string; loc : loc };
type styp =
    [ STlid of loc and string
    | STapp of loc and styp and styp
    | STquo of loc and string
    | STself of loc and string
    | STtok of loc
    | STstring_tok of loc
    | STtyp of Ast.ctyp ] ;

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
and symbol 'e 'p =
    { used : list string; text : text 'e 'p;
      styp : styp; pattern : option 'p } ;

type used = [ Unused | UsedScanned | UsedNotScanned ];

let print_warning = eprintf "%a:\n%s@." FanLoc.print;  
let split_ext = ref False;
let prefix = "__camlp4_"  ;  
let meta_action = ref False;
let grammar_module_name = let _loc = FanLoc.ghost in ref <:ident< $(uid:"")>> ;
let gm () = !grammar_module_name;
  
let string_of_patt patt =
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt p -> Pprintast.pattern fmt (Ast2pt.patt p)) patt in
  let str = Buffer.contents buf in
  if str = "" then assert False else str;

(* FIXME why deprecate such syntax *)  
let check_not_tok s =
    match s with
    [ {text = TXtok _loc _ _ ;_} ->
        FanLoc.raise _loc (Stream.Error
          ("Deprecated syntax, use a sub rule. "^
           "LIST0 STRING becomes LIST0 [ x = STRING -> x ]"))
    | _ -> () ];
  
let mark_used modif ht n =
    try
      let rll = Hashtbl.find_all ht n in
      List.iter
        (fun [ (({contents=Unused} as r), _)   ->  begin 
            r := UsedNotScanned; modif := True;
          end
          |  _ -> () ])
        rll
    with
    [ Not_found -> () ] ;

  let  mark_symbol modif ht symb =
    List.iter (fun e -> mark_used modif ht e) symb.used ;

  let check_use nl el =
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
            List.iter (fun (r, _) -> r := UsedNotScanned) rll
          with _ ->
            ())
        nl;
      modif := True;
      while !modif do {
        modif := False;
        Hashtbl.iter
          (fun _ (r, e) ->
            if !r = UsedNotScanned then do {
              r := UsedScanned;
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
          if !r = Unused then
            print_warning e.name.loc ("Unused local entry \"" ^ s ^ "\"")
          else ())
        ht;
    }
  ;

  let new_type_var =
    let i = ref 0 in fun () -> do { incr i; "e__" ^ string_of_int !i } ;

  let used_of_rule_list rl = (*('h, 'i) rule list -> string list *)
    List.fold_left
      (fun nl r -> List.fold_left (fun nl s -> s.used @ nl) nl r.prod) [] rl ;

  let retype_rule_list_without_patterns _loc rl =
    (*Ast.loc -> (Ast.expr, Ast.patt) rule list -> (Ast.expr, Ast.patt) rule list*)
    try
      List.map
        (fun
          (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Gram.Token.extract_string x); ... *)
        [ {prod = [({pattern = None; styp = STtok _ ;_} as s)]; action = None} ->
            {prod = [{ (s) with pattern = Some <:patt< x >> }];
              action = Some <:expr< $(id:gm()).string_of_token x >>}
          (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
        | {prod = [({pattern = None; _ } as s)]; action = None} ->
            {prod = [{ (s) with pattern = Some <:patt< x >> }];
              action = Some <:expr< x >>}
          (* ...; ([] -> a); ... *)
        | {prod = []; action = Some _} as r -> r
        | _ -> raise Exit ])
        rl
    with
    [ Exit -> rl ];

  let  make_ctyp  styp tvar = (* styp -> string -> Ast.ctyp *)
    let rec aux  = fun  
    [ STlid _loc s -> <:ctyp< $lid:s >>
    | STapp _loc t1 t2 -> <:ctyp< $(aux t1) $(aux t2 ) >>
    | STquo _loc s -> <:ctyp< '$s >>
    | STself _loc x ->
        if tvar = "" then
          FanLoc.raise _loc
            (Stream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else <:ctyp< '$tvar >>
    | STtok _loc -> <:ctyp< $(id:gm()).token >> (*FIXME*)
    | STstring_tok _loc -> <:ctyp< string >>
    | STtyp t -> t ] in aux styp ;

  (*
    {[
    styp generates type constraints which are used to constrain patt
    ]}
   *)    
  let make_ctyp_patt styp tvar patt =
    let styp = match styp with [ STstring_tok _loc -> STtok _loc | t -> t ] in
    match make_ctyp styp tvar with
    [ <:ctyp< _ >> -> patt (* FIXME *)
    | t -> let _loc = Camlp4Ast.loc_of_patt patt in <:patt< ($patt : $t) >> ];

  let make_ctyp_expr styp tvar expr =
    match make_ctyp styp tvar with
    [ <:ctyp< _ >> -> expr
    | t -> let _loc = Camlp4Ast.loc_of_expr expr in <:expr< ($expr : $t) >> ];
      
  (*
    {[
    ('j, Ast.patt) symbol list
    ]}
   *)    
  let text_of_action _loc  (psl) (rtvar:string) (act:option Ast.expr) (tvar:string) =
    (* Ast.loc -> ('j, Ast.patt) symbol list ->
       string -> Ast.expr option -> string -> Ast.expr *)
    let locid = <:patt< $(lid: !FanLoc.name) >> in (* default is [_loc]*)
    let act = match act with
      [ Some act -> act (* get the action *)
      | None -> <:expr< () >> ] in
    let (tok_match_pl, act, _) =
      List.fold_left
        (fun ((tok_match_pl, act, i) as accu) ->
          fun
          [ { pattern = None; _ } -> accu
          | { pattern = Some p ; _} when Camlp4Ast.is_irrefut_patt p -> accu
                (*
                  EXTEND Gram
                  expr:[[ x = uid -> x ]]
                  END
                 *)
          | { pattern = Some <:patt< ($_ $(tup:<:patt< _ >>) as $lid:s) >> ; _} ->
              (tok_match_pl,
               <:expr< let $lid:s = $(id:gm()).string_of_token $lid:s
                       in $act >>, i) (* FIXME should be removed later *)
          | { pattern = Some p; text=TXtok _ _ _ ; _ } ->
              let id = prefix ^ string_of_int i in
              (Some (match (tok_match_pl) with
                     [ None -> (<:expr< $lid:id >>, p)
                     | Some (tok_pl, match_pl) ->
                        (<:expr< $lid:id, $tok_pl >>, <:patt< $p, $match_pl >>)]),
               act, succ i)
          | _ -> accu ])
        (None, act, 0) psl  in
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
      <:expr< fun ($locid : FanLoc.t) -> $e2 >> in (*FIXME hard coded Loc*)
    let (txt, _) =
      List.fold_left
        (fun (txt, i) s ->
          match s.pattern with
          [ None | Some <:patt< _ >> -> (<:expr< fun _ -> $txt >>, i)
          | Some <:patt< ($_ $(tup:<:patt< _ >>) as $p) >> ->
              let p = make_ctyp_patt s.styp tvar p in
              (<:expr< fun $p -> $txt >>, i)
          | Some p when Camlp4Ast.is_irrefut_patt p ->
              let p = make_ctyp_patt s.styp tvar p in
              (<:expr< fun $p -> $txt >>, i)
          | Some _ ->
              let p = make_ctyp_patt s.styp tvar
                        <:patt< $(lid:"__camlp4_"^string_of_int i) >> in
              (<:expr< fun $p -> $txt >>, succ i) ])
        (e, 0) psl in
    let txt =
      if !meta_action then
        <:expr< Obj.magic $(MetaAst.Expr.meta_expr _loc txt) >>
      else txt  in
    <:expr< $(id:gm()).mk_action $txt >>  ;
  let srules loc t rl tvar =
    List.map
      (fun r ->
        let sl = [ s.text | s <- r.prod ] in
        let ac = text_of_action loc r.prod t r.action tvar in
        (sl, ac))
      rl ;

  let rec make_expr entry tvar =  fun
    [ TXmeta _loc n tl e t ->
        let el =
          List.fold_right
            (fun t el -> <:expr< [$(make_expr entry "" t) :: $el] >>)
            tl <:expr< [] >>
        in
        <:expr<
          $(id:gm()).Smeta $str:n $el ($(id:gm()).Action.mk $(make_ctyp_expr t tvar e)) >>
    | TXlist _loc min t ts ->
        let txt = make_expr entry "" t.text in
        match (min, ts) with
        [ (False, None) -> <:expr< $(id:gm()).Slist0 $txt >>
        | (True, None) -> <:expr< $(id:gm()).Slist1 $txt >>
        | (False, Some s) ->
            let x = make_expr entry tvar s.text in
            <:expr< $(id:gm()).Slist0sep $txt $x >>
        | (True, Some s) ->
            let x = make_expr entry tvar s.text in
            <:expr< $(id:gm()).Slist1sep $txt $x >> ]
    | TXnext _loc -> <:expr< $(id:gm()).Snext >>
    | TXnterm _loc n lev ->
        match lev with
        [ Some lab ->
            <:expr<
              $(id:gm()).Snterml
                ($(id:gm()).obj ($(n.expr) : $(id:gm()).t '$(n.tvar)))
                $str:lab >>
        | None ->
            if n.tvar = tvar then <:expr< $(id:gm()).Sself >>
            else
              <:expr<
                $(id:gm()).Snterm
                    ($(id:gm()).obj ($(n.expr) : $(id:gm()).t '$(n.tvar))) >> ]
    | TXopt _loc t -> <:expr< $(id:gm()).Sopt $(make_expr entry "" t) >>
    | TXtry _loc t -> <:expr< $(id:gm()).Stry $(make_expr entry "" t) >>
    | TXrules _loc rl ->
        <:expr< $(id:gm()).srules $(entry.expr) $(make_expr_rules _loc entry rl "") >>
    | TXself _loc -> <:expr< $(id:gm()).Sself >>
    | TXkwd _loc kwd -> <:expr< $(id:gm()).Skeyword $str:kwd >>
    | TXtok _loc match_fun descr ->
        <:expr< $(id:gm()).Stoken ($match_fun, $`str:descr) >> ]

  and make_expr_rules _loc n rl tvar =  List.fold_left
      (fun txt (sl, ac) ->
        let sl =
          List.fold_right
            (fun t txt ->
                let x = make_expr n tvar t in
                <:expr< [$x :: $txt] >>)
            sl <:expr< [] >>
        in
        <:expr< [($sl, $ac) :: $txt ] >>)
      <:expr< [] >> rl  ;

  let expr_of_delete_rule _loc n sl =
    let sl =
      List.fold_right
        (fun s e -> <:expr< [$(make_expr n "" s.text) :: $e ] >>) sl
        <:expr< [] >>  in
    (<:expr< $(n.expr) >>, sl)  ;


  let mk_name _loc i = {expr = <:expr< $id:i >>; tvar = Ident.tvar_of_ident i; loc = _loc};

  let slist loc min sep symb = TXlist loc min symb sep ;
  let text_of_entry  _loc e =
    let ent =
      let x = e.name in
      let _loc = e.name.loc in
      <:expr< ($(x.expr) : $(id:gm()).t '$(x.tvar)) >>   in
    let pos =
      match e.pos with
      [ Some pos -> <:expr< Some $pos >>
      | None -> <:expr< None >> ] in
    let txt =
      List.fold_right
        (fun level txt ->
          let lab =
            match level.label with
            [ Some lab -> <:expr< Some $str:lab >>
            | None -> <:expr< None >> ]  in
          let ass =
            match level.assoc with
            [ Some ass -> <:expr< Some $ass >>
            | None -> <:expr< None >> ]  in
          let txt =
            let rl = srules _loc e.name.tvar level.rules e.name.tvar in
            let e = make_expr_rules _loc e.name rl e.name.tvar in
            <:expr< [($lab, $ass, $e) :: $txt] >> in
          txt)
        e.levels <:expr< [] >> in
    (ent, pos, txt)
  ;
  (* [gl] is the name  list option *)   
  let let_in_of_extend _loc gram gl el args =
    match gl with
    [ None -> args
    | Some nl -> begin
        check_use nl el;
        let ll =
          let same_tvar e n = e.name.tvar = n.tvar in
          List.fold_right
            (fun e ll -> match e.name.expr with
              [ <:expr< $lid:_ >> ->
                    if List.exists (same_tvar e) nl then ll
                    else if List.exists (same_tvar e) ll then ll
                    else [e.name :: ll]
              | _ -> ll ])  el [] in
        let local_binding_of_name = fun
          [ {expr = <:expr< $lid:i >> ; tvar = x; loc = _loc} ->
            <:binding< $lid:i =  (grammar_entry_create $str:i : $(id:gm()).t '$x) >>
          | _ -> failwith "internal error in the Grammar extension" ]  in
        let expr_of_name {expr = e; tvar = x; loc = _loc} =
          <:expr< ($e : $(id:gm()).t '$x) >> in
        let e = match ll with
          [ [] -> args
          | [x::xs] ->
              let locals =
                List.fold_right
                  (fun name acc ->
                    <:binding< $acc and $(local_binding_of_name name) >>)
                  xs (local_binding_of_name x) in
              let entry_mk =  match gram with
              [ Some g -> <:expr< $(id:gm()).mk $id:g >>
              | None   -> <:expr< $(id:gm()).mk >> ] in <:expr<
              let grammar_entry_create = $entry_mk in
              let $locals in $args >> ] in
          match nl with
          [ [] -> e
          | [x::xs] ->
              let globals =
                List.fold_right
                  (fun name acc ->
                    <:binding< $acc and _ = $(expr_of_name name) >>)
                  xs <:binding< _ = $(expr_of_name x) >>
              in <:expr< let $globals in $e >> ]
        end ]  ;

  (* the [gl] is global entry name list, [el] is entry list
     [gram] is the grammar, [gmod] is the [Gram] module
   *)
  let text_of_functorial_extend _loc  gram gl el = (* FIXME remove gmod later*)
    let args =
      let el =
        List.map
          (fun e ->
            let (ent, pos, txt) = text_of_entry e.name.loc e in
            <:expr< $(id:gm()).extend $ent ((fun () -> ($pos, $txt)) ()) >> ) el  in
      match el with
      [ [] -> <:expr< () >>
      | [e] -> e
      | [e::el] -> <:expr< do { $(List.fold_left (fun acc x -> <:expr< $acc; $x >>) e el) } >>  ]  in
    let_in_of_extend _loc gram gl el args;

  let mk_tok _loc p t =
    let p' = Camlp4Ast.wildcarder#patt p in
    let match_fun =
      if Camlp4Ast.is_irrefut_patt p' then
        <:expr< fun [ $pat:p' -> True ] >>
      else
        <:expr< fun [ $pat:p' -> True | _ -> False ] >> in
    let descr = string_of_patt p' in
    let text = TXtok _loc match_fun descr in
    {used = []; text = text; styp = t; pattern = Some p };
