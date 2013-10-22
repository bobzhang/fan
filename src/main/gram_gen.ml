%import{
Format:
  eprintf
  ;
Fan_ops:
  list_of_list
  is_irrefut_pat
  ;
};;

open FAst
open Ast_gen
open Util


let print_warning = eprintf "%a:\n%s@." Locf.print

  
let prefix = "__fan_"  
let ghost = Locf.ghost

(* let grammar_module_name = ref (`Uid (ghost,"Fgram"))  *)
let grammar_module_name = ref (`Uid (ghost,"Fgram")) (* BOOTSTRAPING*)  
let gm () =
  match !Configf.compilation_unit with
  |Some "Fgram" (* BOOTSTRAPING*)
    -> `Uid(ghost,"")
  |Some _ | None -> 
      !grammar_module_name

let mk_entry ~local ~name ~pos ~levels =
  {Gram_def.name;pos;levels;local}
  
let mk_level ~label ~assoc ~rules  =
  ({label; assoc;rules} : Gram_def.level)
  
let mk_rule ~prod ~action =
  ({prod;action}:Gram_def.rule)
  
let mk_symbol  ?(pattern=None)  ~text ~styp =
  ({ text;styp;pattern}:Gram_def.symbol)


let check_not_tok (_s:Gram_def.symbol) = 
  (* match s with  *)
  (* | {text = `Stoken (_loc,  _, _,_) ;_} -> *)
  (*     Locf.raise _loc (Fstream.Error *)
  (*                        ("Deprecated syntax, use a sub rule. "^ *)
  (*                         "L0 STRING becomes L0 [ x = STRING -> x ]")) *)
  (* | _ -> *) ()  (* removed soon*)
      
let new_type_var = 
  let i = ref 0 in fun () -> begin
    incr i; "e__" ^ string_of_int !i
  end 
    
let gensym  = let i = ref 0 in fun () -> (incr i; i)

let gen_lid ()=  prefix^string_of_int (!(gensym ()))
  
(* transform rule list *)  
let retype_rule_list_without_patterns _loc rl =
  try
    List.map (function (x:Gram_def.rule) -> 
      match x with
        (* ...; [ "foo" ]; ... ==> ...; (x = [ "foo" ] -> Fgram.Token.string_of_token x); ... *)
      | {prod = [({pattern = None; styp = `Tok _ ;_} as s)]; action = None} ->
          ({prod =
           [{ s with pattern = Some %pat{ x } }];
           action =
           Some %exp{$(id:gm()).string_of_token x }
          }:Gram_def.rule)
            (* ...; [ symb ]; ... ==> ...; (x = [ symb ] -> x); ... *)
      | {prod = [({pattern = None; _ } as s)]; action = None} ->

          ({prod = [{ s with pattern = Some %pat{ x } }];
           action = Some %exp{ x }} : Gram_def.rule)
            (* ...; ([] -> a); ... *)
      | {prod = []; action = Some _} as r -> r
      | _ -> raise Exit ) rl
  with
    Exit -> rl 



let make_ctyp (styp:Gram_def.styp) tvar : ctyp = 
  let rec aux  v = 
    match (v:Gram_def.styp) with
    | #vid' as x -> (x : vid' :>ctyp) 
    | `Quote _ as x -> x
    | %ctyp'{ $t2 $t1}-> %ctyp{$(aux t2) $(aux t1)}
    | `Self _loc ->
        if tvar = "" then
          Locf.raise _loc (Fstream.Error ("S: illegal in anonymous entry level"))
        else %ctyp{ '$lid:tvar }
    | `Tok _loc -> %ctyp{ Ftoken.t }  (** BOOTSTRAPPING, associated with module name Ftoken*)
          (* %ctyp{[Ftoken.t]} should be caught as error ealier *)
    | `Type t -> t  in
  aux styp

      

let rec make_exp (tvar : string) (x:Gram_def.text) =
  with exp
  let rec aux tvar (x:Gram_def.text) =
    match x with
    | `Slist (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        (match  ts with
        |  None -> if min then  %{ `Slist1 $txt } else %{ `Slist0 $txt } 
        | Some s ->
            let x = aux tvar s.text in
            if min then %{ `Slist1sep ($txt,$x)} else %{ `Slist0sep ($txt,$x) })
    | `Sself _loc ->  %{ `Sself}
    | `Skeyword (_loc, kwd) ->  %{ `Skeyword $str:kwd }
    | `Snterm (_loc, n, lev) ->
        let obj =
          %{ ($(id:gm()).obj
                ($(n.exp) : '$(lid:n.tvar) $(id:(gm(): vid :> ident)).t ))} in 
        (match lev with
        | Some lab -> %{ `Snterml ($obj,$str:lab)}
        | None ->
           if n.tvar = tvar then %{ `Sself} else %{ `Snterm $obj })
    | `Sopt (_loc, t) -> %{ `Sopt $(aux "" t) }
    | `Stry (_loc, t) -> %{ `Stry $(aux "" t) }
    | `Speek (_loc, t) -> %{ `Speek $(aux "" t) }
    | `Stoken (_loc, match_fun,  mdescr, mstr ) ->
        %{`Stoken ($match_fun, $mdescr, $str:mstr)} in
  aux  tvar x


and make_exp_rules (_loc:loc)
    (rl : (Gram_def.text list  * exp * exp option) list) (tvar:string) =
  with exp
  list_of_list _loc
    (List.map (fun (sl,action,raw) ->
      let action_string =
        match raw with
        | None -> ""
        | Some e -> Ast2pt.to_string_exp e in
      let sl = list_of_list _loc (List.map (fun t -> make_exp tvar t) sl) in
      %{ ($sl,($str:action_string,$action)) } ) rl)

(** generate action, compiling pattern match  *)  
let text_of_action (_loc:loc)  (psl :  Gram_def.symbol list) ?action:(act: exp option)
    (rtvar:string)  (tvar:string) : exp = with exp
  let locid = %pat{ $(lid:!Locf.name) } in 
  let act = Option.default %{()} act in
  (* collect the patterns *)
  let (_,tok_match_pl) =
    Listf.fold_lefti
      (fun i ((oe,op) as ep)  x ->
        match (x:Gram_def.symbol) with 
        | {pattern=Some p ; text=`Stoken _;_ } when not (is_irrefut_pat p)->
            let id = prefix ^ string_of_int i in
            ( %{$lid:id} :: oe, p:: op)
        | {pattern = Some p; text = `Skeyword _; _} ->
            let id = prefix ^ string_of_int i in 
            (%{$lid:id}::oe, p :: op) (* TO be improved*)
        | _ ->  ep   ) ([],[])  psl in
  let e =
    let e1 = %{ ($act : '$lid:rtvar ) } in
      match tok_match_pl with
      | ([],_) ->
          %{fun ($locid : Locf.t) -> $e1 } (* BOOTSTRAPING, associated with module name [Locf] *)
      | (e,p) ->
          let (exp,pat) =
            match (e,p) with
            | ([x],[y]) -> (x,y) | _ -> (tuple_com e, tuple_com p) in

          let len = List.length e in
          (** it's dangerous to combine generated string with [fprintf] or [sprintf] *)
          (* let action_string = Ast2pt.to_string_exp act in           *)
          let error_fmt =
            (* action_string ^  *)String.concat " " (Listf.init len (fun _ -> "%s")) in


          let es = List.map (fun x -> %{Ftoken.token_to_string $x}) e in
          let error =
            Ast_gen.appl_of_list
              ([ %{Printf.sprintf };
                %{$`str:error_fmt}]  @ es) in 
          %{fun ($locid : Locf.t) -> (* BOOTSTRAPING, associated with module name [Locf] *)
            match $exp with
            | $pat -> $e1
            | _ -> failwith $error}  in
  let (_,txt) =
    Listf.fold_lefti
      (fun i txt (s:Gram_def.symbol) ->
        match s.pattern with
        |Some %pat'{ ($_ $(par:%pat@_{ _ }) as $p) } ->
            let p = typing (p:alident :> pat) (make_ctyp s.styp tvar)  in
            %{ fun $p -> $txt }
        | Some p when is_irrefut_pat p ->
            let p = typing p (make_ctyp s.styp tvar) in
            %{ fun $p -> $txt }
        | Some _ ->
            let p =
              typing %pat{ $(lid:prefix^string_of_int i) } (make_ctyp s.styp tvar)  in
            %{ fun $p -> $txt }
        | None -> %{ fun _ -> $txt })  e psl in
  %{ $(id:(gm())).mk_action $txt }

    


(* let exp_delete_rule _loc n (symbolss:Gram_def.symbol list list ) = with exp *)
(*   let f _loc (n:Gram_def.name) sl =   *)
(*    let sl = list_of_list _loc *)
(*        (List.map (fun  (s:Gram_def.symbol) -> make_exp "" s.text) sl) in  *)
(*    (%{ $(n.exp) }, sl)  in *)
(*   let rest = List.map *)
(*       (fun sl  -> *)
(*           let (e,b) = f _loc n sl in *)
(*           %exp{ $(id:gm()).delete_rule $e $b }) symbolss in *)
(*   match symbolss with *)
(*   | [] -> %{ () } *)
(*   |_ -> seq_sem rest  *)

  
(* given the entry of the name, make a name *)
let mk_name _loc (i:vid) : Gram_def.name =
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
let text_of_entry ?(safe=true) (e:Gram_def.entry) :exp =  with exp
  let _loc = e.name.loc in    
  let ent =
    %{($(e.name.exp):'$(lid:e.name.tvar) $(id:(gm():vid :> ident)).t)  }   in
  let pos =
    match e.pos with
    | Some pos -> %{Some $pos} 
    | None -> %{None}   in
    let apply =
      (fun (level:Gram_def.level)  ->
        let lab =
          match level.label with
          | Some lab ->   %{Some $str:lab}
          | None ->   %{None}   in
        let ass =
          match level.assoc with
          | Some ass ->   %{Some $ass}
          | None ->    %{None}   in
        let mk_srule loc (t : string)  (tvar : string) (r : Gram_def.rule) :
            (Gram_def.text list  *  exp * exp option) =
          let sl = List.map (fun (s:Gram_def.symbol)  -> s.text) r.prod in
          let ac = text_of_action loc r.prod t ?action:r.action tvar in
          (sl, ac,r.action) in
        (* the [rhs] was already computed, the [lhs] was left *)
        let mk_srules loc ( t : string) (rl:Gram_def.rule list ) (tvar:string)  =
          List.map (mk_srule loc t tvar) rl in
        let rl = mk_srules _loc e.name.tvar level.rules e.name.tvar in
        let prod = make_exp_rules _loc rl e.name.tvar in
        (* generated code of type [olevel] *)
        %{ ($lab, $ass, $prod) }) in
    match e.levels with
    |`Single l ->
        if safe then
          %{ $(id:(gm())).extend_single $ent ($pos, $(apply l) ) }
        else
          %{ $(id:(gm())).unsafe_extend_single $ent ($pos, $(apply l) ) }
    |`Group ls ->
        let txt = list_of_list _loc (List.map apply ls) in
        if safe then 
          %{$(id:(gm())).extend $ent ($pos,$txt)}
        else 
          %{$(id:(gm())).unsafe_extend $ent ($pos,$txt)}

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
    %exp{ $(id:gm()).mk_dynamic $g }

    | None -> 
        %exp{ $(id:gm()).mk } in
  let local_bind_of_name = function x ->
    match (x:Gram_def.name) with 
    | {exp = %exp@_{ $lid:i } ; tvar = x; loc = _loc} ->
        %bind{ $lid:i = (grammar_entry_create $str:i : '$lid:x $(id:(gm():vid :> ident)).t )}
    | {exp;_} -> failwithf "internal error in the Grammar extension %s"
          (Objs.dump_exp exp)   in
  match locals with
  | [] -> default 
  | ll ->
      let locals = and_of_list (List.map local_bind_of_name ll)  in
      (** eta-expansion to avoid specialized types here  *)
      %exp{ let grammar_entry_create x = $entry_mk  x in let $locals in $default }    


      
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot  = object
  inherit Objs.map as super
  val mutable constraints : (exp * exp) list  =[]
  method! pat = function
    | `Ant(_loc,s) -> 
        begin match s with
        {FanUtil.content=code;_} ->
          let cons = %exp{ $lid:code } in
          let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
          let cons' = %exp{ $lid:code' } in 
          let () = constraints <- (cons,cons')::constraints in 
          %pat{ $lid:code' } (* only allows lidentifiers here *)
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
    | [] -> %exp{ () }
    | _ -> seq_sem el    in
  let locals  = (** FIXME the order matters here, check duplication later!!! *)
    Listf.filter_map
      (fun (x:Gram_def.entry) -> if x.local then Some x.name else None ) el in
  let_in_of_extend _loc gram locals args 

(** *)
        

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gram_gen.cmo " *)
(* end: *)
