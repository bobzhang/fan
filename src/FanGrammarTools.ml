open Ast;
open Format;
open Lib;
open LibUtil;
module MetaAst = FanAst.Make Lib.Meta.MetaGhostLoc;  

open FanGrammar;

let print_warning = eprintf "%a:\n%s@." FanLoc.print;  

  
let prefix = "__fan_"  ;
  
let meta_action = ref false;
  
let ghost = FanLoc.ghost ;  
let grammar_module_name =
  ref (`Uid (ghost,"Gram")) ;
  
let gm () =
  match !FanConfig.compilation_unit with
  [Some "Gram" -> `Uid(ghost,"")
  |Some _ | None -> 
      !grammar_module_name];

let mk_entry ~name ~pos ~levels =
  {name;pos;levels};
  
let mk_level ~label ~assoc ~rules =
  {label; assoc;rules};
  
let mk_rule ~prod ~action =
  {prod;action};
  
let mk_symbol  ?(pattern=None)  ~text ~styp =
  { text;styp;pattern};

let string_of_patt patt = 
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt p -> AstPrint.pattern fmt (Ast2pt.patt p)) patt in
  let str = Buffer.contents buf in
  if str = "" then assert false else str;

(** FIXME why deprecate such syntax
    It makes
    [OPT STRING] invalid
    You shoud write [OPT [x=STRING -> x] ]
 *)  
let check_not_tok s = 
    match s with
    [ {text = `TXtok (_loc, _, _, _) ;_} ->
        FanLoc.raise _loc (XStream.Error
          ("Deprecated syntax, use a sub rule. "^
           "L0 STRING becomes L0 [ x = STRING -> x ]"))
    | _ -> () ];
      
let new_type_var = 
  let i = ref 0 in fun () -> begin
    incr i; "e__" ^ string_of_int !i
  end ;
    
let gensym  =
  let i = ref 0 in fun () -> begin
    incr i;
    i;
  end;

let gen_lid ()=
  prefix^string_of_int (!(gensym ()));
  
(* transform rule list *)  
let retype_rule_list_without_patterns _loc rl =
  try
    List.map(fun
      (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Gram.Token.extract_string x); ... *)
    [ {prod = [({pattern = None; styp = `Tok _ ;_} as s)]; action = None} ->
      {prod =
       [{ (s) with pattern = Some {:patt| x |} }];
       action = Some {:expr| $(id:gm()).string_of_token x |}}
    (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
    | {prod = [({pattern = None; _ } as s)]; action = None} ->

        {prod = [{ (s) with pattern = Some {:patt| x |} }];
         action = Some {:expr| x |}}
    (* ...; ([] -> a); ... *)
    | {prod = []; action = Some _} as r -> r
    | _ -> raise Exit ]) rl
  with
    [ Exit -> rl ];

exception NotneededTyping ;

(*
  translate [styp] into [ctyp],
  given the assumption that the entry output [tvar] type
 *)
    
let  make_ctyp  (styp:styp) tvar : option ctyp = 
  let rec aux  = with ctyp fun  
    [ `Id _ | `Quote _ as x -> x  
    | {| $t1 $t2|} -> {| $(aux t1) $(aux t2) |}
    | `Self (_loc, x) ->
        if tvar = "" then
          FanLoc.raise _loc
            (XStream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else {| '$lid:tvar |}
    | `Tok _loc ->  {| [> FanToken.t ] |}  (* BOOTSTRAPPING*)
    | `Type t -> t ] in
    try Some (aux styp) with [NotneededTyping -> None ];
      
(*
  {[ [styp] generates type constraints which are used to constrain patt ]}
*)    
let make_ctyp_patt styp tvar patt = 
  match make_ctyp styp tvar with
  [ None -> patt (* FIXME *)
  | Some t -> let _loc = FanAst.loc_of patt in {:patt| ($patt : $t) |} ];

let make_ctyp_expr styp tvar expr = 
  match make_ctyp styp tvar with
  [ None -> expr
  | Some t -> let _loc = FanAst.loc_of expr in {:expr| ($expr : $t) |} ];

(* transform [text] to [expr] which represents [symbol]
   compute the [lhs]
 *)    
let rec make_expr entry tvar = with expr
  fun
  [ `TXmeta (_loc, n, tl, e, t) ->
    let el = Lib.Expr.mklist _loc (List.map (fun t -> make_expr entry "" t ) tl) in 
    let ns = Lib.Expr.mklist _loc (List.map (fun n -> {| $str:n |} ) n) in 
    {| `Smeta ($ns, $el, ($(id:gm()).Action.mk $(make_ctyp_expr t tvar e))) |}
  | `TXlist (_loc, min, t, ts) ->
      let txt = make_expr entry "" t.text in
      match (min, ts) with
      [ (false, None) -> {| `Slist0 $txt |} 
      | (true, None) ->  {| `Slist1 $txt |} 
      | (false, Some s) ->
          let x = make_expr entry tvar s.text in
          {| `Slist0sep ($txt,$x) |}
      | (true, Some s) ->
            let x = make_expr entry tvar s.text in
            {| `Slist1sep ($txt,$x) |} ]
  | `TXnext _loc ->  {| `Snext |}
  | `TXself _loc ->  {| `Sself|}
  | `TXkwd (_loc, kwd) ->  {| `Skeyword $str:kwd |}

  | `TXnterm (_loc, n, lev) ->
      match lev with
      [ Some lab ->
        {| `Snterml
          (($(id:gm()).obj ($(n.expr) : $(id:gm()).t '$(lid:n.tvar) )), $str:lab) |} 
      | None ->
          if n.tvar = tvar then {| `Sself|}
          else
            {|
            `Snterm ($(id:gm()).obj ($(n.expr) : $(id:gm()).t '$(lid:n.tvar)))  |}   ]
  | `TXopt (_loc, t) -> {| `Sopt $(make_expr entry "" t) |}
  | `TXtry (_loc, t) -> {| `Stry $(make_expr entry "" t) |}
  | `TXpeek (_loc, t) -> {| `Speek $(make_expr entry "" t) |}
  | `TXrules (_loc, rl) ->
      {| $(id:gm()).srules $(entry.expr) $(make_expr_rules _loc entry rl "") |}
  | `TXtok (_loc, match_fun, attr, descr) ->
      {| `Stoken ($match_fun, ($vrn:attr, $`str:descr)) |} ]
(* the [rhs] was computed, compute the [lhs] *)    
and make_expr_rules _loc n rl tvar = with expr
  Lib.Expr.mklist _loc
    (List.map (fun (sl,action) ->
      let sl = Lib.Expr.mklist _loc (List.map (fun t -> make_expr n tvar t) sl) in
      {| ($sl,$action) |} ) rl);
  
(* generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type
 *)
let text_of_action _loc  psl  rtvar act tvar = with expr
  let locid = {:patt| $(lid:!FanLoc.name) |} in 
  let act =
    match act with
    [ Some act -> act (* get the action *)
    | None -> {| () |} ] in
  let (_,tok_match_pl) = List.fold_lefti
    (fun i tok_match_pl x ->
      match x with
      [ {pattern=Some p ; text=`TXtok _;_ } ->
          let id = prefix ^ string_of_int i in
          (Some
             (match tok_match_pl with
             [None -> ( {|$lid:id|}, p)
             |Some (oe,op) ->
                 ({| $lid:id, $oe |}, {:patt|$p,$op |}) ]))
      | _ -> tok_match_pl]  ) None psl in
  let e =
    let e1 = {| ($act : '$lid:rtvar ) |} in
    let e2 =
      match tok_match_pl with
      [ None -> e1
      | Some ({| $t1, $t2 |}, {:patt@_| $p1, $p2 |}) ->
          {|
            match ($t1, $t2) with (* two and more patterns here *)
            [ ($p1, $p2) -> $e1
            | _ -> assert false ] |}
      | Some (tok, match_) ->
            {|
            match $tok with
            [ $pat:match_ -> $e1
            | _ -> assert false ] |} ] in
    {| fun ($locid : FanLoc.t) -> $e2 |} in (*FIXME hard coded Loc*)
  (* add prefix now *)
  let (_,txt) =
    List.fold_lefti
      (fun i txt s ->
        match s.pattern with
        [ None | Some {:patt@_| _ |} -> {| fun _ -> $txt |}
        | Some {:patt| ($_ $(tup:{:patt@_| _ |}) as $p) |} ->
            let p = make_ctyp_patt s.styp tvar {:patt| $(id:(p:>ident)) |} in  {| fun $p -> $txt |}
        | Some p when FanAst.is_irrefut_patt p ->
            let p = make_ctyp_patt s.styp tvar p in
            {| fun $p -> $txt |}
        | Some _ ->
            let p = make_ctyp_patt s.styp tvar {:patt| $(lid:prefix^string_of_int i) |} in
            {| fun $p -> $txt |} ])  e psl in
  let txt =
    if !meta_action then
      {| Obj.magic $(MetaAst.Expr.meta_expr _loc txt) |}
    else txt  in
  {| $(id:gm()).mk_action $txt |}  ;

(* the [rhs] was already computed, the [lhs] was left *)
let mk_srules loc t rl tvar =
  List.map
    (fun r ->
      let sl = [ s.text | s <- r.prod ] in
      let ac = text_of_action loc r.prod t r.action tvar in
      (sl, ac)) rl ;
    

let expr_of_delete_rule _loc n sl =  with expr
   let sl =
    List.fold_right
      (fun s e -> {| [$(make_expr n "" s.text) :: $e ] |}) sl {| [] |}  in
  ({:expr| $(n.expr) |}, sl)  ;

(* given the entry of the name, make a name *)
let mk_name _loc i = {expr = {:expr| $id:i |}; tvar = Ident.tvar_of_ident i; loc = _loc};
  
let mk_slist loc min sep symb = `TXlist loc min symb sep ;
  
let text_of_entry  _loc e =  with expr
  let ent =
    let x = e.name in
    let _loc = e.name.loc in
    {| ($(x.expr) : $(id:gm()).t '$(lid:x.tvar)) |}   in
  let pos =
    match e.pos with
    [ Some pos -> {| Some $pos |}
    | None -> {| None |} ] in
  let txt =
    List.fold_right
      (fun level txt ->
        let lab =
          match level.label with
          [ Some lab -> {| Some $str:lab |}
          | None -> {| None |} ]  in
        let ass =
          match level.assoc with
          [ Some ass -> {| Some $ass |}
          | None -> {| None |} ]  in
        let txt =
          let rl = mk_srules _loc e.name.tvar level.rules e.name.tvar in
          let e = make_expr_rules _loc e.name rl e.name.tvar in
          {| [($lab, $ass, $e) :: $txt] |} in txt) e.levels {| [] |} in
  (ent, pos, txt) ;
  

(* [gl] is the name  list option

   {[
   loc -> ident option ->expr name list option ->
   (expr, 'a) entry list -> expr -> expr
   ]}
   
 *)   
let let_in_of_extend _loc gram gl  default =
  let entry_mk =
    match gram with
    [ Some g -> {:expr| $(id:gm()).mk $id:g |}
    | None   -> {:expr| $(id:gm()).mk |} ] in
  let local_binding_of_name = fun
    [ {expr = {:expr@_| $lid:i |} ; tvar = x; loc = _loc} ->
      {:binding| $lid:i =  (grammar_entry_create $str:i : $(id:gm()).t '$lid:x) |}
    | _ -> failwith "internal error in the Grammar extension" ]  in
  match gl with
  [ None -> default
  | Some ll ->
        match ll with
        [ [] -> default
        | [x::xs] ->
            let locals =
              List.fold_right
                (fun name acc -> {:binding| $acc and $(local_binding_of_name name) |})
                xs (local_binding_of_name x) in
              {:expr|
              let grammar_entry_create = $entry_mk in
              let $locals in $default |} ] ]  ;

(* the [locals] is local entry name list,
   [el] is entry list
   [gram] is the grammar
   [gmod] is the [Gram] module true
   generate the extend, the main entrance
 *)
let text_of_functorial_extend _loc  gram locals el = 
  let args =
    let el =
      List.map  (fun e ->
        let (ent, pos, txt) = text_of_entry e.name.loc e in
        {:expr| $(id:gm()).extend $ent  ($pos, $txt) |} ) el  in
    match el with
    [ [] -> {:expr| () |}
    | [e] -> e
    | [e::el] ->
        {:expr| begin  $(List.fold_left (fun acc x -> {:expr| $acc; $x |}) e el) end |}  ]  in
  let_in_of_extend _loc gram locals  args;


(* generate TXtok *)  
let mk_tok _loc ?restrict ~pattern styp = with expr
 match restrict with
 [ None ->
   let no_variable = FanAst.wildcarder#patt pattern in
   let match_fun =
     if FanAst.is_irrefut_patt no_variable
     then 
       {| fun [ $pat:no_variable -> true ] |}
     else {| fun [$pat:no_variable -> true | _ -> false ] |} in 
   let descr = string_of_patt no_variable in
   let text = `TXtok (_loc, match_fun, "Normal", descr) in
   {text; styp; pattern = Some pattern }
     
 | Some restrict ->
     let p'= FanAst.wildcarder#patt pattern in
     let match_fun = 
       {| fun [$pat:pattern when $restrict -> true | _ -> false ] |}  in
     let descr = string_of_patt pattern in
     let text = `TXtok (_loc, match_fun, "Antiquot", descr) in
     {text; styp; pattern = Some p'} ] ;
   
let sfold ?sep _loc  (ns:list string)  f e s = with ctyp
  let fs = [("FOLD0","sfold0");("FOLD1","sfold1")] in
  let suffix = match sep with [None -> ""|Some  _ -> "sep"] in
  let n = List.hd ns in 
  let foldfun =
    try List.assoc n fs ^ suffix  with [Not_found -> invalid_arg "sfold"] in
  let styp = {| '$(lid:new_type_var ()) |} in
  let e = {:expr| $(id:gm()).$lid:foldfun $f $e |} in
  let( t:styp) =
    {| $(`Type {| $(id:gm()).$(lid:"fold"^suffix) _ |})
      $(s.styp) $styp |} in 
  let text = `TXmeta _loc ns (match sep with [None -> [s.text] | Some sep -> [s.text;sep.text] ])  e t   in 
  {text ; styp ; pattern = None } ;



















        
  (* (\*                 ) *\) *)
  (* let (tok_match_pl(\* , act *\), _) = *)
  (*   List.fold_left *)
  (*     (fun *)
  (*       ((tok_match_pl, i) as accu) *)
  (*       -> fun *)
  (*         [ { pattern = None; _ } -> accu *)
  (*         | { pattern = Some p ; _} when FanAst.is_irrefut_patt p -> accu *)
  (*         | { pattern = Some p; text=`TXtok (_, _,  _, _) ; _ } -> *)
  (*             let id = prefix ^ string_of_int i in *)
  (*             (Some *)
  (*                (match tok_match_pl with *)
  (*                [ None -> ({| $lid:id |}, p) *)
  (*                | Some (tok_pl, match_pl) -> *)
  (*                    ({| $lid:id, $tok_pl |}, {:patt| $p, $match_pl |})]),  i+1) *)
  (*         | _ -> accu ])  (None, 0) psl  in *)
