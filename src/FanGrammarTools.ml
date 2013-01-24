open FanAst;
open Format;
open Lib;
open LibUtil;
module MetaAst = FanAst.Make Lib.Meta.MetaGhostLoc;  

open FanGrammar;

let print_warning = eprintf "%a:\n%s@." FanLoc.print;  

  
let prefix = "__fan_"  ;
  
  
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
    [ {text = `Stok (_loc, _, _, _) ;_} ->
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



(*
  translate [styp] into [ctyp],
  given the assumption that the entry output [tvar] type
 *)
    
let  make_ctyp (styp:styp) tvar : ctyp = 
  let rec aux  = with ctyp fun  
    [ `Id _ | `Quote _ as x -> x  
    | {| $t1 $t2|} -> {| $(aux t1) $(aux t2) |}
    | `Self (_loc, x) ->
        if tvar = "" then
          FanLoc.raise _loc
            (XStream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else {| '$lid:tvar |}
    | `Tok _loc ->  {| [> FanToken.t ] |}  (* BOOTSTRAPPING*)
    | `Type t -> t ] in aux styp;

      

(* transform [text] to [expr] which represents [symbol]
   compute the [lhs]
   it generates code which has type [Gram.symbol]
   {[
   `Skeyword "let"

   `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))

   `Smeta
      (["FOLD1"; "SEP"],
          [Gram.srules declare_regexp
                [([`Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     `Skeyword ":";
                     `Snterm (Gram.obj (regexp : 'regexp Gram.t ))],
                      (Gram.mk_action
                         (fun (r : 'regexp)  _  (__fan_0 : [> FanToken.t]) 
                            (_loc : FanLoc.t)  ->
                            match __fan_0 with
                            | `Lid x -> ((x, r) : 'e__2 )
                            | _ -> assert false)))];
                `Skeyword ";"],
               (Gram.Action.mk
                   (Gram.sfold1sep
                      (fun (x,r)  ()  ->
                         if Hashtbl.mem FanLexTools.named_regexps x
                         then
                           Printf.eprintf
                             "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                             x
                         else ();
                         Hashtbl.add FanLexTools.named_regexps x r) () : 
                   (_,'e__2,'e__3) Gram.foldsep )))
   `Slist0
     (Gram.srules sig_items
                 [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                          (sg : 'e__1 ))))])

   `Slist0sep
       ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
        (`Skeyword "|"))


   `Slist1sep
        ((Gram.srules pos_exprs
          [([`Stoken
                        (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     `Skeyword ":";
                     `Snterm
                       (Gram.obj (dot_lstrings : 'dot_lstrings Gram.t ))],
                      (Gram.mk_action
                         (fun (y : 'dot_lstrings)  _ 
                            (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                            match __fan_0 with
                            | `Lid x ->
                                (((x : string ), (FanToken.resolve_name y)) : 
                                'e__2 )
                            | _ -> assert false)));
                   ([`Stoken
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"))],
                     (Gram.mk_action
                        (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                           ->
                           match __fan_0 with
                           | `Lid x ->
                               (((x : string ),
                                  (FanToken.resolve_name ((`Sub []), x))) : 
                               'e__2 )
                           | _ -> assert false)))]), (`Skeyword ";"))
   `Snext
   `Sself
   `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")
   `Stoken
     (((function | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
        | _ -> false)),
       (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))
   ]}
 *)    
let rec make_expr entry (tvar:string) x =
  with expr
  let rec aux tvar x =
    match x with
    [ `Smeta (_loc, n, tl, e, t) ->
      let el = list_of_list _loc (List.map (fun t -> aux "" t ) tl) in 
      let ns = list_of_list _loc (List.map (fun n -> {| $str:n |} ) n) in 
      {| `Smeta ($ns, $el,
               ($(id:gm()).Action.mk $(typing e (make_ctyp t tvar)))) |}
    | `Slist (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        match  ts with
        [  None -> if min then  {| `Slist1 $txt |} else {| `Slist0 $txt |} 
        | Some s ->
            let x = aux tvar s.text in
            if min then {| `Slist1sep ($txt,$x)|} else {| `Slist0sep ($txt,$x) |} ]
    | `Snext _loc ->  {| `Snext |}
    | `Sself _loc ->  {| `Sself|}
    | `Skeyword (_loc, kwd) ->  {| `Skeyword $str:kwd |}
    | `Snterm (_loc, n, lev) ->
        let obj = {| ($(id:gm()).obj ($(n.expr) : $(id:gm()).t '$(lid:n.tvar)))|} in 
        match lev with
       [ Some lab ->
         {| `Snterml ($obj,$str:lab)|}
       | None ->
           if n.tvar = tvar then {| `Sself|} else {| `Snterm $obj |} ]
    | `Sopt (_loc, t) -> {| `Sopt $(aux "" t) |}
    | `Stry (_loc, t) -> {| `Stry $(aux "" t) |}
    | `Speek (_loc, t) -> {| `Speek $(aux "" t) |}
    | `Srules (_loc, rl) ->
        {| $(id:gm()).srules
          (* ((\* $(id:gm()).name_of_entry *\) *)
          (*    $(entry.expr)) *) $(make_expr_rules _loc entry rl "") |}
    | `Stok (_loc, match_fun, attr, descr) ->
      {| `Stoken ($match_fun, ($vrn:attr, $`str:descr)) |} ] in aux  tvar x


(* the [rhs] was computed, compute the [lhs]
   the generated expression has type [production]
 *)    
and make_expr_rules _loc n rl tvar = with expr
  list_of_list _loc
    (List.map (fun (sl,action) ->
      let sl = list_of_list _loc (List.map (fun t -> make_expr n tvar t) sl) in
      {| ($sl,$action) |} ) rl);
  
(* generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type

   It is in charge of generating code like this 
   {[
   (Gram.mk_action
               (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t) 
                  -> (`Try (_loc, e, a) : 'expr )))
   ]}
 *)
let text_of_action (_loc:loc)  (psl: list symbol) ?action:(act:option expr)
    (rtvar:string)  (tvar:string) = with expr
  let locid = {:patt| $(lid:!FanLoc.name) |} in 
  let act =
    match act with
    [ Some act -> act | None -> {| () |} ] in
  (* collect the patterns *)
  let (_,tok_match_pl) =
    List.fold_lefti
      (fun i ((oe,op) as ep)  x -> match x with 
      [ {pattern=Some p ; text=`Stok _;_ } ->
          let id = prefix ^ string_of_int i in
          ([ {|$lid:id|} :: oe], [p:: op])
      | _ ->  ep ]  ) ([],[])  psl in
  let e =
    let e1 = {| ($act : '$lid:rtvar ) |} in
      match tok_match_pl with
      [ ([],_) ->  {| fun ($locid :FanLoc.t) -> $e1 |}
      | (e,p) ->
          let (expr,patt) =
            match (e,p) with [([x],[y]) -> (x,y) | _ -> (tuple_com e, tuple_com p)] in 
          {|fun ($locid :FanLoc.t) ->
            match $expr with [ $(pat:patt) -> $e1 | _ -> assert false]|} ] in
  let (_,txt) =
    List.fold_lefti
      (fun i txt s ->
        match s.pattern with
        [Some {:patt| ($_ $(tup:{:patt@_| _ |}) as $p) |} ->
            let p = 
              typing {:patt| $(id:(p:>ident)) |} (make_ctyp s.styp tvar)  in  {| fun $p -> $txt |}
        | Some p when FanAst.is_irrefut_patt p ->
            let p = typing p (make_ctyp s.styp tvar) in
            {| fun $p -> $txt |}
        | None -> {| fun _ -> $txt |}
        | Some _ ->
            let p =
              typing {:patt| $(lid:prefix^string_of_int i) |} (make_ctyp s.styp tvar)  in
            {| fun $p -> $txt |} ])  e psl in
  {| $(id:gm()).mk_action $txt |}  ;

(* the [rhs] was already computed, the [lhs] was left *)
let mk_srules loc t rl tvar =
  List.map
    (fun r ->
      let sl = List.map (fun s  -> s.text) r.prod in
      let ac = text_of_action loc r.prod t ?action:r.action tvar in
      (sl, ac)) rl ;
    


let expr_delete_rule _loc n (symbolss:list (list symbol)) = with expr
  let f _loc n sl =  
   let sl = list_of_list _loc (List.map (fun  s -> make_expr n "" s.text) sl) in 
   ({| $(n.expr) |}, sl)  in
  let rest = List.map
      (fun sl  ->
          let (e,b) = f _loc n sl in
          {:expr| $(id:gm()).delete_rule $e $b |}) symbolss in
  seq (sem_of_list rest);
  
(* given the entry of the name, make a name *)
let mk_name _loc i = {expr = {:expr| $id:i |}; tvar = Ident.tvar_of_ident i; loc = _loc};
  
let mk_slist loc min sep symb = `Slist loc min symb sep ;


(*
  return [(ent,pos,txt)] the [txt] has type [olevel],

  [ent] is something like
  {[
  (module_expr : 'module_expr Gram.t )
  ]}

  
  [pos] is something like
  {[(Some `LA)]} it has type [position option]
  
 *)  
let text_of_entry e =  with expr
  let _loc = e.name.loc in    
  let ent =
    let x = e.name in
    {| ($(x.expr) : $(id:gm()).t '$(lid:x.tvar)) |}   in
  let pos =
    match e.pos with
    [ Some pos -> {| Some $pos |} 
    | None -> {| None |} (* {| `LA |} *) ] in
  let txt =
    List.fold_right
      (fun level txt ->
        let lab =
          match level.label with
          [ Some lab -> (* {| Some $str:lab |} *) {|$str:lab|}
          | None -> (* {| None |} *) {| "" |}]  in
        let ass =
          match level.assoc with
          [ Some ass -> (* {| Some $ass |} *) ass 
          | None -> (* {| None |} *)  {| `LA |} ]  in
        let txt =
          let rl = mk_srules _loc e.name.tvar level.rules e.name.tvar in
          let prod = make_expr_rules _loc e.name rl e.name.tvar in
          (* generated code of type [olevel] *)
          {| [($lab, $ass, $prod) :: $txt] |} in txt) e.levels {| [] |} in
  {| $(id:gm()).extend $ent ($pos,$txt) |}
  (* (ent, pos, txt) *) ;
  

(* [gl] is the name  list option

   {[
   loc -> ident option ->expr name list option ->
   (expr, 'a) entry list -> expr -> expr
   ]}

   This function generate some local entries
 *)   
let let_in_of_extend _loc gram locals  default =
  let entry_mk =
    match gram with
    [ Some g -> {:expr| $(id:gm()).mk $id:g |}
    | None   -> {:expr| $(id:gm()).mk |} ] in
  let local_binding_of_name = fun
    [ {expr = {:expr@_| $lid:i |} ; tvar = x; loc = _loc} ->
      {:binding| $lid:i =  (grammar_entry_create $str:i : $(id:gm()).t '$lid:x) |}
    | _ -> failwith "internal error in the Grammar extension" ]  in
  match locals with
  [ None 
  | Some [] -> default
  | Some ll ->
      let locals = and_of_list' (List.map local_binding_of_name ll)  in
      {:expr| let grammar_entry_create = $entry_mk in let $locals in $default |}  ]  ;

(* the [locals] is local entry name list,
   [el] is entry list
   [gram] is the grammar
   [gmod] is the [Gram] module true
   generate the extend, the main entrance
   the [entrance] point for generating code

   It call [text_of_entry]
 *)
let text_of_functorial_extend _loc  gram locals el = 
  let args =
    let el =
      List.map  text_of_entry el  in
    match el with
    [ [] -> {:expr| () |}
    | _ -> seq (sem_of_list' el) ]  in
  let_in_of_extend _loc gram locals  args;


(* generate Stok *)  
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
   let text = `Stok (_loc, match_fun, "Normal", descr) in
   {text; styp; pattern = Some pattern }
     
 | Some restrict ->
     let p'= FanAst.wildcarder#patt pattern in
     let match_fun = 
       {| fun [$pat:pattern when $restrict -> true | _ -> false ] |}  in
     let descr = string_of_patt pattern in
     let text = `Stok (_loc, match_fun, "Antiquot", descr) in
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
  let text = `Smeta _loc ns (match sep with [None -> [s.text] | Some sep -> [s.text;sep.text] ])  e t   in 
  {text ; styp ; pattern = None } ;



















        
  (* (\*                 ) *\) *)
  (* let (tok_match_pl(\* , act *\), _) = *)
  (*   List.fold_left *)
  (*     (fun *)
  (*       ((tok_match_pl, i) as accu) *)
  (*       -> fun *)
  (*         [ { pattern = None; _ } -> accu *)
  (*         | { pattern = Some p ; _} when FanAst.is_irrefut_patt p -> accu *)
  (*         | { pattern = Some p; text=`Stok (_, _,  _, _) ; _ } -> *)
  (*             let id = prefix ^ string_of_int i in *)
  (*             (Some *)
  (*                (match tok_match_pl with *)
  (*                [ None -> ({| $lid:id |}, p) *)
  (*                | Some (tok_pl, match_pl) -> *)
  (*                    ({| $lid:id, $tok_pl |}, {:patt| $p, $match_pl |})]),  i+1) *)
  (*         | _ -> accu ])  (None, 0) psl  in *)
