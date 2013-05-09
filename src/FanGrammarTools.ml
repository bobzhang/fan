open Ast
open FanOps
open Format
open AstLoc
open LibUtil
open FanGrammar

let print_warning = eprintf "%a:\n%s@." FanLoc.print

  
let prefix = "__fan_"  
let ghost = FanLoc.ghost
let grammar_module_name = ref (`Uid (ghost,"Gram")) 
  
let gm () : vid =
  match !FanConfig.compilation_unit with
  |Some "Gram" -> `Uid(ghost,"")
  |Some _ | None -> 
      !grammar_module_name

let mk_entry ~name ~pos ~levels =
  {name;pos;levels}
  
let mk_level ~label ~assoc ~rules =
  {label; assoc;rules}
  
let mk_rule ~prod ~action =
  {prod;action}
  
let mk_symbol  ?(pattern=None)  ~text ~styp =
  { text;styp;pattern}

let string_of_pat pat = 
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt p -> AstPrint.pattern fmt (Ast2pt.pat p)) pat in
  let str = Buffer.contents buf in
  if str = "" then assert false else str

(** FIXME why deprecate such syntax
    It makes
    [OPT STRING] invalid
    You shoud write [OPT [x=STRING -> x] ]
 *)  
let check_not_tok s = 
    match s with
    | {text = `Stok (_loc, _, _, _) ;_} ->
        FanLoc.raise _loc (XStream.Error
          ("Deprecated syntax, use a sub rule. "^
           "L0 STRING becomes L0 [ x = STRING -> x ]"))
    | _ -> () 
      
let new_type_var = 
  let i = ref 0 in fun () -> begin
    incr i; "e__" ^ string_of_int !i
  end 
    
let gensym  = let i = ref 0 in fun () -> (incr i; i)

let gen_lid ()=  prefix^string_of_int (!(gensym ()))
  
(* transform rule list *)  
let retype_rule_list_without_patterns _loc rl =
  try
    List.map(function
        (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Gram.Token.extract_string x); ... *)
      | {prod = [({pattern = None; styp = `Tok _ ;_} as s)]; action = None} ->
          {prod =
           [{ (s) with pattern = Some {:pat| x |} }];
           action =
           Some {:exp|$((gm() : vid :> exp)).string_of_token x |}
             (* {:exp| $(id:gm()).string_of_token x |} *)
         }
            (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
      | {prod = [({pattern = None; _ } as s)]; action = None} ->

          {prod = [{ (s) with pattern = Some {:pat| x |} }];
           action = Some {:exp| x |}}
            (* ...; ([] -> a); ... *)
      | {prod = []; action = Some _} as r -> r
      | _ -> raise Exit ) rl
  with
    Exit -> rl 



(*
  translate [styp] into [ctyp],
  given the assumption that the entry output [tvar] type
 *)
        
let make_ctyp (styp:styp) tvar : ctyp = 
  let rec aux  = with ctyp function  
    | #ident' | `Quote _ as x -> x  
    | `App(_loc,t1,t2) -> `App(_loc,aux t1,aux t2)
    | `Self (_loc, x) ->
        if tvar = "" then
          FanLoc.raise _loc
            (XStream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else {| '$lid:tvar |}
    | `Tok _loc -> {| [> FanToken.t ] |}  (* BOOTSTRAPPING*)
    | `Type t -> t  in aux styp

      

(* transform [text] to [exp] which represents [symbol]
   compute the [lhs]
   it generates code which has type [Gram.symbol]

   tvar provides type informatoin
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
     (Gram.srules sigis
                 [([`Snterm (Gram.obj (sigi : 'sigi Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (sg : 'sigi)  (_loc : FanLoc.t)  ->
                          (sg : 'e__1 ))))])

   `Slist0sep
       ((`Snterm (Gram.obj (case0 : 'case0 Gram.t ))),
        (`Skeyword "|"))


   `Slist1sep
        ((Gram.srules pos_exps
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
   `Snterml ((Gram.obj (exp : 'exp Gram.t )), "top")
   `Stoken
     (((function | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
        | _ -> false)),
       (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))
   ]}
 *)    
let rec make_exp (tvar : string) (x:text) =
  with exp
  let rec aux tvar x =
    match x with
    | `Smeta (_loc, n, tl, e, t) ->
      let el = list_of_list _loc (List.map (fun t -> aux "" t ) tl) in 
      let (ns:exp) = list_of_list _loc (List.map (fun n -> {| $str:n |} ) n) in
      let act = typing e (make_ctyp t tvar) in
      {| `Smeta ($ns, $el, ($((gm() : vid :> exp)).Action.mk $act )) |}
      (* {| `Smeta ($ns, $el, ($(id:gm()).Action.mk $act )) |} *)
    | `Slist (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        begin match  ts with
        |  None -> if min then  {| `Slist1 $txt |} else {| `Slist0 $txt |} 
        | Some s ->
            let x = aux tvar s.text in
            if min then {| `Slist1sep ($txt,$x)|} else {| `Slist0sep ($txt,$x) |}
        end
    | `Snext _loc ->  {| `Snext |}
    | `Sself _loc ->  {| `Sself|}
    | `Skeyword (_loc, kwd) ->  {| `Skeyword $str:kwd |}
    | `Snterm (_loc, n, lev) ->
        let obj =
          {| ($((gm() : vid :> exp)).obj
                ($(n.exp) : '$(lid:n.tvar) $(id:(gm(): vid :> ident)).t ))|}
          (* {| ($(id:gm()).obj *)
          (*       ($(n.exp) : $(id:(gm()(\* : vid :> ident *\))).t '$(lid:n.tvar)))|} *) in 
        begin match lev with
        | Some lab -> {| `Snterml ($obj,$str:lab)|}
        | None ->
           if n.tvar = tvar then {| `Sself|} else {| `Snterm $obj |}
        end
    | `Sopt (_loc, t) -> {| `Sopt $(aux "" t) |}
    | `Stry (_loc, t) -> {| `Stry $(aux "" t) |}
    | `Speek (_loc, t) -> {| `Speek $(aux "" t) |}
    | `Srules (_loc, rl) ->
        {| $((gm():vid:>exp)).srules $(make_exp_rules _loc rl "") |}
        (* {| $(id:gm()).srules $(make_exp_rules _loc rl "") |} *)
    | `Stok (_loc, match_fun, attr, descr) ->
      {| `Stoken ($match_fun, ($vrn:attr, $`str:descr)) |}  in aux  tvar x


(* the [rhs] was computed, compute the [lhs]
   the generated expession has type [production]
 *)    
and make_exp_rules (_loc:loc)  (rl : (text list  * exp) list  ) (tvar:string) =
  with exp
  list_of_list _loc
    (List.map (fun (sl,action) ->
      (* let number = List.length sl in *)
      let action_string = Ast2pt.to_string_exp action in
      (* let exp = (Filters.ME.meta_exp _loc action) in *)
      let sl = list_of_list _loc (List.map (fun t -> make_exp tvar t) sl) in
      {| ($sl,($str:action_string,$action(* ,$exp *))) |} ) rl)
  
(* generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type

   It is in charge of generating code like this 
   {[
   (Gram.mk_action
               (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FanLoc.t) 
                  -> (`Try (_loc, e, a) : 'exp )))
   ]}
 *)
let text_of_action (_loc:loc)  (psl:  symbol list) ?action:(act: exp option)
    (rtvar:string)  (tvar:string) : exp = with exp
  let locid = {:pat| $(lid:!FanLoc.name) |} in 
  let act =
    match act with
    | Some act -> act | None -> {| () |}  in
  (* collect the patterns *)
  let (_,tok_match_pl) =
    List.fold_lefti
      (fun i ((oe,op) as ep)  x -> match x with 
      | {pattern=Some p ; text=`Stok _;_ } ->
          let id = prefix ^ string_of_int i in
          ( {|$lid:id|} :: oe, p:: op)
      | _ ->  ep   ) ([],[])  psl in
  let e =
    let e1 = {| ($act : '$lid:rtvar ) |} in
      match tok_match_pl with
      | ([],_) ->
          {| fun ($locid :FanLoc.t) -> $e1 |}
      | (e,p) ->
          let (exp,pat) =
            match (e,p) with
            | ([x],[y]) -> (x,y) | _ -> (tuple_com e, tuple_com p) in
          let action_string = Ast2pt.to_string_exp act in
          {|fun ($locid :FanLoc.t) ->
            match $exp with
            | $(pat:pat) -> $e1
            | _ -> failwith $`str:action_string |}  in
  let (_,txt) =
    List.fold_lefti
      (fun i txt s ->
        match s.pattern with
        |Some {:pat'| ($_ $(par:{:pat@_| _ |}) as $p) |} ->
            let p = typing (p:alident :> pat)(* {:pat'| $(id:(p:>ident)) |} *)

                (make_ctyp s.styp tvar)  in
            {| fun $p -> $txt |}
        | Some p when is_irrefut_pat p ->
            let p = typing p (make_ctyp s.styp tvar) in
            {| fun $p -> $txt |}
        | None -> {| fun _ -> $txt |}
        | Some _ ->
            let p =
              typing {:pat| $(lid:prefix^string_of_int i) |} (make_ctyp s.styp tvar)  in
            {| fun $p -> $txt |} )  e psl in
  {| $((gm():vid:>exp)).mk_action $txt |}

let mk_srule loc (t : string)  (tvar : string) (r : rule) : (text list  *  exp) =
  let sl = List.map (fun s  -> s.text) r.prod in
  let ac = text_of_action loc r.prod t ?action:r.action tvar in
  (sl, ac)
  
(* the [rhs] was already computed, the [lhs] was left *)
let mk_srules loc ( t : string) (rl:rule list ) (tvar:string) : (text list  * exp)list  =
  List.map (mk_srule loc t tvar) rl
    


let exp_delete_rule _loc n (symbolss:symbol list list ) = with exp
  let f _loc n sl =  
   let sl = list_of_list _loc (List.map (fun  s -> make_exp (* n *) "" s.text) sl) in 
   ({| $(n.exp) |}, sl)  in
  let rest = List.map
      (fun sl  ->
          let (e,b) = f _loc n sl in
          {:exp| $((gm():vid:>exp)).delete_rule $e $b |}) symbolss in
  match symbolss with
  | [] -> {| () |}
  |_ -> seq_sem rest 
  (* seq (sem_of_list rest); *)
  
(* given the entry of the name, make a name *)
let mk_name _loc (i:vid) =
  {exp = (i:vid :> exp) (* {:exp| $id:i |} *);
   tvar = Id.tvar_of_ident i; loc = _loc}
  
let mk_slist loc min sep symb = `Slist loc min symb sep 


(*
  return [(ent,pos,txt)] the [txt] has type [olevel],

  [ent] is something like
  {[
  (module_exp : 'mexp Gram.t )
  ]}

  
  [pos] is something like
  {[(Some `LA)]} it has type [position option]
  
 *)  
let text_of_entry (e:entry) :exp =  with exp
  let _loc = e.name.loc in    
  let ent =
    let x = e.name in
    {| ($(x.exp) : '$(lid:x.tvar) $(id:(gm():vid :> ident)).t)  |}   in
  let pos =
    match e.pos with
    | Some pos -> {| Some $pos |} 
    | None -> {| None |}   in
    let apply =
      (fun level  ->
        let lab =
          match level.label with
          | Some lab ->   {| Some $str:lab |}
          | None ->   {|None|}   in
        let ass =
          match level.assoc with
          | Some ass ->   {| Some $ass |}
          | None ->    {|None|}   in
          let rl = mk_srules _loc e.name.tvar level.rules e.name.tvar in
          let prod = make_exp_rules _loc rl e.name.tvar in
          (* generated code of type [olevel] *)
          {| ($lab, $ass, $prod) |}) in
    match e.levels with
    |`Single l ->
      {| $((gm():vid:>exp)).extend_single $ent ($pos, $(apply l) ) |}
      (* {| $(id:gm()).extend_single $ent ($pos, $(apply l) ) |} *)
    |`Group ls ->
        let txt = list_of_list _loc (List.map apply ls) in
        {|$((gm():vid:>exp)).extend $ent ($pos,$txt)|}

  

(* [gl] is the name  list option

   {[
   loc -> ident option ->exp name list option ->
   (exp, 'a) entry list -> exp -> exp
   ]}

   This function generate some local entries
 *)   
let let_in_of_extend _loc (gram: vid option ) locals  default =
  let entry_mk =
    match gram with
    | Some g -> let g = (g:vid :> exp) in
    {:exp| $((gm():vid:>exp)).mk_dynamic $g |}
    (* {:exp| $(id:gm()).mk_dynamic $g |} *)
    | None   -> (* {:exp| $(id:gm()).mk |} *)
        {:exp| $((gm():vid:>exp)).mk |} in
  let local_binding_of_name = function
    | {exp = {:exp@_| $lid:i |} ; tvar = x; loc = _loc} ->
      {:binding| $lid:i =  (grammar_entry_create $str:i : '$lid:x $(id:(gm():vid :> ident)).t ) |}
    | {exp;_} -> failwithf "internal error in the Grammar extension %s" (Objs.dump_exp exp)   in
  match locals with
  | None | Some [] -> default
  | Some ll ->
      let locals = and_of_list (List.map local_binding_of_name ll)  in
      {:exp| let grammar_entry_create = $entry_mk in let $locals in $default |}    

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
    | [] -> {:exp| () |}
    | _ -> seq_sem el    in
  let_in_of_extend _loc gram locals  args


(* generate Stok *)  
let mk_tok _loc ?restrict ~pattern styp = with exp
 match restrict with
 | None ->
   let no_variable = Objs.wildcarder#pat pattern in
   let match_fun =
     if is_irrefut_pat no_variable
     then 
       {| function | $pat:no_variable -> true  |}
     else {| function | $pat:no_variable -> true | _ -> false  |} in 
   let descr = string_of_pat no_variable in
   let text = `Stok (_loc, match_fun, "Normal", descr) in
   {text; styp; pattern = Some pattern }
     
 | Some restrict ->
     let p'= Objs.wildcarder#pat pattern in
     let match_fun = 
       {| function | $pat:pattern when $restrict -> true | _ -> false  |}  in
     let descr = string_of_pat pattern in
     let text = `Stok (_loc, match_fun, "Antiquot", descr) in
     {text; styp; pattern = Some p'} 
   
let sfold ?sep _loc  (ns:string list )  f e s = with ctyp'
  let fs = [("FOLD0","sfold0");("FOLD1","sfold1")] in
  let suffix =
    match sep with
    | None -> ""|Some  _ -> "sep" in
  let n = List.hd ns in 
  let foldfun =
    try List.assoc n fs ^ suffix  with Not_found -> invalid_arg "sfold" in
  let styp = {| '$(lid:new_type_var ()) |} in
  let e =
    {:exp| $((gm():vid:>exp)).$lid:foldfun $f $e |}
(* {:exp| $(id:gm()).$lid:foldfun $f $e |} *) in
  let( t:styp) =
    {| ($(s.styp), $styp) $(`Type {|  _ $(id:(gm():vid:>ident)).$(lid:"fold"^suffix) |})
       |} in 
  let text = `Smeta _loc ns (match sep with | None -> [s.text] | Some sep -> [s.text;sep.text] )  e t   in 
  {text ; styp ; pattern = None } 



