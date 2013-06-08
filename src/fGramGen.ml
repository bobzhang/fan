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


let check_not_tok s = 
    match s with
    | {text = `Stok (_loc,  _, _) ;_} ->
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
           [{ s with pattern = Some {:pat| x |} }];
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
  let rec aux  v = 
    match (v:styp) with
    | #vid' as x -> (x : vid' :>ctyp) 
    | `Quote _ as x -> x
    | {:ctyp'| $t2 $t1|}-> {:ctyp|$(aux t2) $(aux t1)|}
    | `Self (_loc) ->
        if tvar = "" then
          FLoc.raise _loc
            (XStream.Error ("S: illegal in anonymous entry level"))
        else {:ctyp| '$lid:tvar |}
    | `Tok _loc -> {:ctyp| [> FToken.t ] |}  (* BOOTSTRAPPING*)
    | `Type t -> t  in aux styp

      

let rec make_exp (tvar : string) (x:text) =
  with exp
  let rec aux tvar (x:text) =
    match x with
    | `Slist (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        (match  ts with
        |  None -> if min then  {| `Slist1 $txt |} else {| `Slist0 $txt |} 
        | Some s ->
            let x = aux tvar s.text in
            if min then {| `Slist1sep ($txt,$x)|} else {| `Slist0sep ($txt,$x) |})
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
    | `Stok (_loc, match_fun,  descr) ->
        let v = object
          inherit FanAstN.meta
          method! ant _loc x =
            match x with
            | `Ant(_loc,{FanUtil.content=x;_}) ->
                {:ep| `Str $lid:x |}
        end in 
        let mdescr = (v#pat _loc descr :> exp) in (* FIXME [_loc] is not necessary?*)
        {|`Stoken ($match_fun, $mdescr)|}
  in aux  tvar x


and make_exp_rules (_loc:loc)  (rl : (text list  * exp * exp option) list  ) (tvar:string) =
  with exp
  list_of_list _loc
    (List.map (fun (sl,action,raw) ->
      let action_string =
        match raw with
        | None -> ""
        |Some e -> Ast2pt.to_string_exp e in
      let sl = list_of_list _loc (List.map (fun t -> make_exp tvar t) sl) in
      {| ($sl,($str:action_string,$action)) |} ) rl)
  
let text_of_action (_loc:loc)  (psl :  symbol list) ?action:(act: exp option)
    (rtvar:string)  (tvar:string) : exp = with exp
  let locid = {:pat| $(lid:!FLoc.name) |} in 
  let act = Option.default {|()|} act in
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
  {| $(id:(gm())).mk_action $txt |}

    


let exp_delete_rule _loc n (symbolss:symbol list list ) = with exp
  let f _loc n sl =  
   let sl = list_of_list _loc (List.map (fun  s -> make_exp "" s.text) sl) in 
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
        let mk_srule loc (t : string)  (tvar : string) (r : rule) :
            (text list  *  exp * exp option) =
          let sl = List.map (fun s  -> s.text) r.prod in
          let ac = text_of_action loc r.prod t ?action:r.action tvar in
          (sl, ac,r.action) in
        (* the [rhs] was already computed, the [lhs] was left *)
        let mk_srules loc ( t : string) (rl:rule list ) (tvar:string)  =
          List.map (mk_srule loc t tvar) rl in
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


      
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot  = object
  inherit Objs.map as super
  val mutable constraints : (exp * exp) list  =[]
  method! pat = function
    | `Ant(_loc,s) -> 
        begin match s with
        {FanUtil.content=code;_} ->
          let cons = {:exp| $lid:code |} in
          let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
          let cons' = {:exp| $lid:code' |} in 
          let () = constraints <- (cons,cons')::constraints in 
          {:pat| $lid:code' |} (* only allows lidentifiers here *)
        end
    | p -> super#pat p 
  method get_captured_variables =
    constraints
  method clear_captured_variables =
    constraints <- []
end

let filter_pat_with_captured_variables pat= begin 
  capture_antiquot#clear_captured_variables;
  let pat=capture_antiquot#pat pat in
  let constraints = capture_antiquot#get_captured_variables in
  (pat,constraints)
end
        
(** entrance *)        
let text_of_functorial_extend ?safe _loc   gram  el = 
  let args =
    let el =
      List.map  (text_of_entry ?safe)  el  in
    match el with
    | [] -> {:exp| () |}
    | _ -> seq_sem el    in
  let locals  = (** FIXME the order matters here, check duplication later!!! *)
    List.filter_map (fun {name;local;_} -> if local then Some name else None ) el in
  let_in_of_extend _loc gram locals args 


let token_of_simple_pat _loc (p:simple_pat)  =
  let p_pat = (p:simple_pat :> pat) in 
  let (po,ls) = filter_pat_with_captured_variables p_pat in
  match ls with
  | [] ->
      let no_variable = Objs.wildcarder#pat p_pat in (*po is the same as [p_pat]*)
      let match_fun =
        if is_irrefut_pat no_variable then
          {:exp|function | $no_variable -> true |}
        else
          {:exp|function | $no_variable -> true | _ -> false  |} in
      let descr = Objs.strip_pat no_variable in
      let text = `Stok(_loc,match_fun,descr) in
      {text;styp=`Tok _loc;pattern = Some p_pat}
  | (x,y)::ys ->
      let guard =
          List.fold_left (fun acc (x,y) -> {:exp| $acc && ( $x = $y ) |} )
            {:exp| $x = $y |} ys  in
      let match_fun = {:exp| function |$po when $guard -> true | _ -> false |} in
      let descr = Objs.strip_pat (Objs.wildcarder#pat p_pat) in
      let text = `Stok(_loc,match_fun,descr) in
      {text;styp = `Tok _loc;pattern= Some (Objs.wildcarder#pat po) }
        
