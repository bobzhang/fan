open FAst
open FanOps
open Format
open AstLib
open LibUtil
open FGramDef

let print_warning = eprintf "%a:\n%s@." FLoc.print

  
let prefix = "__fan_"  
let ghost = FLoc.ghost

(* let grammar_module_name = ref (`Uid (ghost,"Fgram"))  *)
let grammar_module_name = ref (`Uid (ghost,"Fgram")) (* BOOTSTRAPING*)  
let gm () : vid =
  match !FConfig.compilation_unit with
  |Some "Fgram" (* BOOTSTRAPING*)
    -> `Uid(ghost,"")
  |Some _ | None -> 
      !grammar_module_name

let mk_entry ~local ~name ~pos ~levels =
  {name;pos;levels;local}
  
let mk_level ~label ~assoc ~rules =
  {label; assoc;rules}
  
let mk_rule ~prod ~action =
  {prod;action}
  
let mk_symbol  ?(pattern=None)  ~text ~styp =
  { text;styp;pattern}

let string_of_pat pat = 
  let buf = Buffer.create 42 in
  let  _ =
    try 
    Format.bprintf buf "%a@?"
      (fun fmt p -> AstPrint.pattern fmt (Ast2pt.pat p)) pat
    with
      FLoc.Exc_located(loc,_) ->
        FLoc.errorf loc "invalid pattern when printing %s" (Objs.dump_pat pat)
  in
  let str = Buffer.contents buf in
  if str = "" then assert false else str

let check_not_tok s = 
    match s with
    | {text = `Stok (_loc, _, _, _) ;_} ->
        FLoc.raise _loc (XStream.Error
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
        (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Fgram.Token.string_of_token x); ... *)
      | {prod = [({pattern = None; styp = `Tok _ ;_} as s)]; action = None} ->
          {prod =
           [{ (s) with pattern = Some {:pat| x |} }];
           action =
           Some {:exp|$(id:gm()).string_of_token x |}
         }
            (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
      | {prod = [({pattern = None; _ } as s)]; action = None} ->

          {prod = [{ s with pattern = Some {:pat| x |} }];
           action = Some {:exp| x |}}
            (* ...; ([] -> a); ... *)
      | {prod = []; action = Some _} as r -> r
      | _ -> raise Exit ) rl
  with
    Exit -> rl 



let make_ctyp (styp:styp) tvar : ctyp = 
  let rec aux  = with ctyp function  
    | #ident' | `Quote _ as x -> x  
    | `App(_loc,t1,t2) -> `App(_loc,aux t1,aux t2)
    | `Self (_loc, x) ->
        if tvar = "" then
          FLoc.raise _loc
            (XStream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
        else {| '$lid:tvar |}
    | `Tok _loc -> {| [> FToken.t ] |}  (* BOOTSTRAPPING*)
    | `Type t -> t  in aux styp

      

let rec make_exp (tvar : string) (x:text) =
  with exp
  let rec aux tvar x =
    match x with
    | `Slist (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        (match  ts with
        |  None -> if min then  {| `Slist1 $txt |} else {| `Slist0 $txt |} 
        | Some s ->
            let x = aux tvar s.text in
            if min then {| `Slist1sep ($txt,$x)|} else {| `Slist0sep ($txt,$x) |})
    | `Snext _loc ->  {| `Snext |}
    | `Sself _loc ->  {| `Sself|}
    | `Skeyword (_loc, kwd) ->  {| `Skeyword $str:kwd |}
    | `Snterm (_loc, n, lev) ->
        let obj =
          {| ($((gm() : vid :> exp)).obj
                ($(n.exp) : '$(lid:n.tvar) $(id:(gm(): vid :> ident)).t ))|} in 
        (match lev with
        | Some lab -> {| `Snterml ($obj,$str:lab)|}
        | None ->
           if n.tvar = tvar then {| `Sself|} else {| `Snterm $obj |})
    | `Sopt (_loc, t) -> {| `Sopt $(aux "" t) |}
    | `Stry (_loc, t) -> {| `Stry $(aux "" t) |}
    | `Speek (_loc, t) -> {| `Speek $(aux "" t) |}
    | `Srules (_loc, rl) ->
        {| $(id:(gm())).srules $(make_exp_rules _loc rl "") |}
    | `Stok (_loc, match_fun, attr, descr) ->
      {| `Stoken ($match_fun, ($vrn:attr, $`str:descr)) |}  in aux  tvar x


and make_exp_rules (_loc:loc)  (rl : (text list  * exp * exp option) list  ) (tvar:string) =
  with exp
  list_of_list _loc
    (List.map (fun (sl,action,raw) ->
      let action_string =
        match raw with
        | None -> ""
        |Some e -> Ast2pt.to_string_exp e in
      let sl = list_of_list _loc (List.map (fun t -> make_exp tvar t) sl) in
      {| ($sl,($str:action_string,$action(* ,$exp *))) |} ) rl)
  
let text_of_action (_loc:loc)  (psl:  symbol list) ?action:(act: exp option)
    (rtvar:string)  (tvar:string) : exp = with exp
  let locid = {:pat| $(lid:!FLoc.name) |} in 
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
          {| fun ($locid :FLoc.t) -> $e1 |} (* BOOTSTRAPING *)
      | (e,p) ->
          let (exp,pat) =
            match (e,p) with
            | ([x],[y]) -> (x,y) | _ -> (tuple_com e, tuple_com p) in
          let action_string = Ast2pt.to_string_exp act in
          {|fun ($locid :FLoc.t) ->
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

let mk_srule loc (t : string)  (tvar : string) (r : rule) : (text list  *  exp * exp option) =
  let sl = List.map (fun s  -> s.text) r.prod in
  let ac = text_of_action loc r.prod t ?action:r.action tvar in
  (sl, ac,r.action)
  
(* the [rhs] was already computed, the [lhs] was left *)
let mk_srules loc ( t : string) (rl:rule list ) (tvar:string)  =
  List.map (mk_srule loc t tvar) rl
    


let exp_delete_rule _loc n (symbolss:symbol list list ) = with exp
  let f _loc n sl =  
   let sl = list_of_list _loc (List.map (fun  s -> make_exp (* n *) "" s.text) sl) in 
   ({| $(n.exp) |}, sl)  in
  let rest = List.map
      (fun sl  ->
          let (e,b) = f _loc n sl in
          {:exp| $(id:gm()).delete_rule $e $b |}) symbolss in
  match symbolss with
  | [] -> {| () |}
  |_ -> seq_sem rest 

  
(* given the entry of the name, make a name *)
let mk_name _loc (i:vid) =
  let rec aux : vid -> string =  function
    | `Lid (_,x) | `Uid(_,x) -> x
    | `Dot(_,`Uid(_,x),xs) -> x ^ "__" ^ aux xs
    | _ -> failwith "internal error in the Grammar extension" in
  {exp = (i :> exp) ; tvar = aux i; loc = _loc}
  
let mk_slist loc min sep symb = `Slist (loc, min, symb, sep) 


(*
  return [(ent,pos,txt)] the [txt] has type [olevel],
  [ent] is something like
  {[
  (module_exp : 'mexp Fgram.t )
  ]}
  [pos] is something like
  {[(Some `LA)]} it has type [position option] *)        
let text_of_entry ?(safe=true) (e:entry) :exp =  with exp
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
        if safe then
          {| $(id:(gm())).extend_single $ent ($pos, $(apply l) ) |}
        else
          {| $(id:(gm())).unsafe_extend_single $ent ($pos, $(apply l) ) |}
    |`Group ls ->
        let txt = list_of_list _loc (List.map apply ls) in
        if safe then 
          {|$(id:(gm())).extend $ent ($pos,$txt)|}
        else 
          {|$(id:(gm())).unsafe_extend $ent ($pos,$txt)|}

(** [gl] is the name  list option

   {[
   loc -> ident option ->exp name list option ->
   (exp, 'a) entry list -> exp -> exp
   ]}

   This function generate some local entries *)               
let let_in_of_extend _loc (gram: vid option ) locals  default =
  let entry_mk =
    match gram with
    | Some g -> let g = (g:vid :> exp) in
    {:exp| $(id:gm()).mk_dynamic $g |}

    | None -> 
        {:exp| $(id:gm()).mk |} in
  let local_bind_of_name = function
    | {exp = {:exp@_| $lid:i |} ; tvar = x; loc = _loc} ->
      {:bind| $lid:i =  (grammar_entry_create $str:i : '$lid:x $(id:(gm():>ident)).t ) |}
    | {exp;_} -> failwithf "internal error in the Grammar extension %s" (Objs.dump_exp exp)   in
  match locals with
  | [] -> default 
  (* | None | Some [] -> default *)
  | (* Some *) ll ->
      let locals = and_of_list (List.map local_bind_of_name ll)  in
      (** eta-expansion to avoid specialized types here  *)
      {:exp| let grammar_entry_create x = $entry_mk  x in let $locals in $default |}    

(** entrance *)        
let text_of_functorial_extend ?safe _loc   gram  el = 
  let args =
    let el =
      List.map  (text_of_entry ?safe)  el  in
    match el with
    | [] -> {:exp| () |}
    | _ -> seq_sem el    in
  (* let_in_of_extend _loc gram locals  args *)
  let locals  = (** FIXME the order matters here, check duplication later!!! *)
    List.filter_map (fun {name;local;_} -> if local then Some name else None ) el in
  let_in_of_extend _loc gram locals args 



let mk_tok _loc ?restrict ~pattern styp = with exp
 match restrict with
 | None ->
   let no_variable = Objs.wildcarder#pat pattern in
   let match_fun =
     if is_irrefut_pat no_variable
     then 
       {| function | $no_variable -> true  |}
     else {| function | $no_variable -> true | _ -> false  |} in 
   let descr = string_of_pat no_variable in
   let text = `Stok (_loc, match_fun, "Normal", descr) in
   {text; styp; pattern = Some pattern }
     
 | Some restrict ->
     let p'= Objs.wildcarder#pat pattern in
     let match_fun = 
       {| function | $pattern when $restrict -> true | _ -> false  |}  in
     let descr = string_of_pat pattern in
     let text = `Stok (_loc, match_fun, "Antiquot", descr) in
     {text; styp; pattern = Some p'} 
   
