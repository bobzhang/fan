%import{
Format:
  eprintf
  ;
Fan_ops:
  list_of_list
  ;
Ast_gen:
  tuple_com
  typing
  and_of_list
  seq_sem
  ;

};;

open FAst
open Util


let print_warning = eprintf "%a:\n%s@." Locf.print

  
let prefix = "__fan_"  
let ghost = Locf.ghost

let module_name =
  ref %exp'@ghost{Gramf} (* BOOTSTRAPING*)  

let gm () =
  match !Configf.compilation_unit with
  |Some "Gramf" (* BOOTSTRAPING*)
    -> `Uid(ghost,"")
  |Some _
  | None -> !module_name

  
let check_add ((loc,id),v) env  =
  (* if List.exists (fun ((_,i),_) -> i = id)  !env then *)
  (*   Locf.failf loc "This variable %s is bound several times" id *)
  (* else *)
    env := ((loc,id),v) :: !env
let add ?(check=true) ((loc,id),v) env =
  if check && List.exists (fun ((_,i),_) -> i = id)  !env then
    Locf.failf loc "This variable %s is bound several times" id
  else 
    env := ((loc,id),v) :: !env
                            
let enhance_env  (s:string) xs env  =
  xs |>
  List.iter
    (fun (((loc,_) as v),opt) ->
      match opt with
      | None -> add  (v, %exp@loc{$lid:s}) env
      | Some l -> add (v, %exp@loc{$lid:s.$lid:l}) env )
                        
let mk_prule ~prod ~action =
  let env = ref [] in
  let i = ref 0 in 
  let prod =
    Listf.filter_map
      (function  (p:Gram_def.osymbol Gram_def.decorate) ->
        match p with (* ? Lid i ? Lid *)
        | {kind = KSome;   txt = ({outer_pattern = None; bounds; _} as symbol) } ->
              let id = prefix ^ string_of_int !i in
               begin
                 enhance_env  id bounds env;
                 List.iter
                   (fun (((xloc,id) as z),_) -> add ~check:false (z, %exp@xloc{Some $lid:id}) env) bounds ;
                 incr i;
                 Some symbol;
               end
        | {kind = KSome; txt = ({outer_pattern = Some ((xloc,id) as z) ;
                                 bounds;  _} as s)}
          ->
            begin
              enhance_env id bounds env ;
              add ~check:false (z, %exp@xloc{Some $lid:id } ) env;
              List.iter
                  (fun (((xloc,id) as z),_) ->  add ~check:false (z, %exp@xloc{Some $lid:id}) env)
                bounds;
              incr i;
              Some s
            end                
        | {kind = KNormal; txt = ({outer_pattern = None; bounds;_} as symbol)} ->
            let id = prefix ^ string_of_int !i in
            begin
              enhance_env id bounds env ;
              incr i;
              Some symbol 
            end
        |{kind = KNormal; txt = ({outer_pattern = Some (_,id); bounds;_} as symbol)} ->
            begin
              enhance_env id bounds env;
              incr i;
              Some symbol
            end
        | {kind = KNone; txt = {outer_pattern = None ; bounds; _}} ->
            begin
              List.iter
                (fun (((xloc,_) as z),_) ->
                  add (z, %exp@xloc{None}) env) bounds;
              None
            end
        | {kind = KNone; txt = {outer_pattern = Some ((xloc,_) as z);
                                bounds; _}} ->
            begin
              add (z, %exp@xloc{None}) env;
              List.iter
                (fun (((xloc,_) as z),_) ->
                  add (z , %exp@xloc{None}) env) bounds;
              None 
            end) prod in
    ({prod;
      action;
      env = List.rev !env }:Gram_def.rule)



let gen_lid ()=
  let gensym  = let i = ref 0 in fun () -> (incr i; i) in
  prefix^string_of_int !(gensym ())
  


      

let rec make_exp (tvar : string) (x:Gram_def.text) =
  let rec aux tvar (x:Gram_def.text) =
    match x with
    | `List (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        (match  ts with
        |  None -> if min then  %exp{ `List1 $txt } else %exp{ `List0 $txt } 
        | Some s ->
            let x = aux tvar s.text in
            if min then %exp{ `List1sep ($txt,$x)} else %exp{ `List0sep ($txt,$x) })
    | `Self _loc ->  %exp{ `Self}
    | `Keyword (_loc, kwd) ->  %exp{ `Keyword $str:kwd }
    | `Nterm (_loc, n, lev) ->
        let obj =
          %exp{ ($id{gm()}.obj
                   (${(n.id:>exp)} : '$lid{n.tvar} $id{(gm(): vid :> ident)}.t ))} in 
        (match lev with
        | Some lab -> %exp{ `Snterml ($obj,$str:lab)}
        | None ->
           if n.tvar = tvar then %exp{ `Self} else %exp{ `Nterm $obj })
    | `Try (_loc, t) -> %exp{ `Try ${aux "" t} }
    | `Peek (_loc, t) -> %exp{ `Peek ${aux "" t} }
    | `Token (_loc, meta) ->
        %exp{`Token $meta} in
  aux  tvar x


and make_exp_rules 
    (rl : (Gram_def.text list  * exp * exp option) list) (tvar:string) =
  rl
  |> List.map (fun (sl,action,raw) ->
      let action_string =
        match raw with
        | None -> ""
        | Some e -> Ast2pt.to_string_exp e in
      let sl =
        sl
        |> List.map (make_exp tvar)
        |> list_of_list  in
      let _loc = Ast_loc.loc_of sl in 
      %exp{ ($sl,($str:action_string,$action)) } )
  |> list_of_list 

(**********************************************)
(* generate action of the right side   *)
(**********************************************)      
let make_action (_loc:loc)
    (x:Gram_def.rule)
    (rtvar:string)  : exp = 
  let locid = %pat{ $lid{!Locf.name} } in 
  let act = Option.default %exp{()} x.action in

  (* collect the patterns
     it is used for further destruction *)
  (* let tok_match_pl = *)
    (* snd @@ *)
  
  let e =
    let make_env env =
      env |> List.map (fun ((loc,id),e) -> %bind{${%pat@loc{$lid:id}} = $e}) in
    let binds = make_env x.env in
    let e1 = %exp{ ($act : '$lid:rtvar ) } in
    let e1 = Ast_gen.seq_binds binds e1 in
    %exp{fun ($locid : Locf.t) -> $e1 } in
  let make_ctyp (styp:Gram_def.styp) tvar : ctyp  =
    let rec aux  v = 
      match (v:Gram_def.styp) with
      | #vid' as x -> (x : vid' :>ctyp) 
      | `Quote _ as x -> x
      | %ctyp'{ $t2 $t1}-> %ctyp{${aux t2} ${aux t1}}
      | `Self _loc ->
          if tvar = "" then
            Locf.raise _loc @@ Streamf.Error ("S: illegal in anonymous entry level")
          else %ctyp{ '$lid:tvar }
      | `Type t -> t  in
    aux styp in
  let (+:) = typing in
  let txt =
    snd @@
    Listf.fold_lefti
      (fun i txt (s:Gram_def.osymbol) ->
        let mk_arg p = %pat{~$lid{ prefix ^string_of_int i} : $p } in
        match (s.outer_pattern, s.bounds) with
        | (Some (xloc,id),_)  -> (* (u:Tokenf.t)   *)
            let p =  %pat@xloc{$lid:id} +: make_ctyp s.styp rtvar in
            %exp{ fun ${mk_arg p} -> $txt }
        | (None, [] ) ->
            %exp{ fun ${mk_arg %pat{_}} -> $txt }
        | (None, _ ) -> (* __fan_i, since we have inner binding*)            
            let p =
              %pat{ $lid{prefix^string_of_int i} } +: make_ctyp s.styp rtvar  in
            %exp{ fun ${mk_arg p} -> $txt })  e x.prod in
  %exp{ $id{(gm())}.mk_action $txt }


  


(*
  return [(ent,pos,txt)] the [txt] has type [olevel],
  [ent] is something like
  {[
  (module_exp : 'mexp Gramf.t )
  ]}
  [pos] is something like
  {[(Some `LA)]} it has type [position option] *)
    
let make_extend safe  (e:Gram_def.entry) :exp =  with exp
  let _loc = e.name.loc in
  let gmid = (gm():vid:>ident) in
  let ent =
    %exp{(${(e.name.id :> exp)}:'$lid{e.name.tvar} $id:gmid.t)  }   in
  let pos = (* hastype Gramf.position option *)
    match e.pos with
    | Some pos -> %exp{Some $pos} 
    | None -> %exp{None}   in
  let apply (level:Gram_def.level)  =
    let lab =
      match level.label with 
      | Some lab ->   %exp{Some $str:lab}
      | None ->   %exp{None}   in
    let ass =
      match level.assoc with (* has type Gramf.assoc option *)
      | Some ass ->   %exp{Some $ass}
      | None ->    %exp{None}   in
    (* the [rhs] was already computed, the [lhs] was left *)
    let rl =
      level.rules 
      |> List.map
          (fun (r:Gram_def.rule) ->
          let sl =
            r.prod
            |> List.map (fun (s:Gram_def.osymbol) -> s.text) in
          (sl,
           make_action _loc r e.name.tvar, (* compose the right side *)
           r.action)) in
    let prod = make_exp_rules  rl e.name.tvar in
    (* generated code of type [olevel] *)
    %exp{ ($lab, $ass, $prod) } in
  match e.levels with
  |`Single l ->
      let f =
        if safe then
          %exp{$id{gm()}.extend_single}
        else %exp{$id{gm()}.unsafe_extend_single} in
        %exp{$f $ent ($pos, (${apply l} : $id{(gm() : vid :> ident)}.olevel ))}
  |`Group ls ->
      let txt = list_of_list (List.map apply ls) in
      let f =
        if safe then %exp{$id{gm()}.extend}
        else %exp{$id{gm()}.unsafe_extend} in
      %exp{$f $ent ($pos, ($txt : $id{(gm() : vid :>ident)}.olevel list))}



      
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot  = object
  inherit Objs.map as super
  val mutable constraints : (exp * exp) list  =[]
  method! pat = function
    | `Ant(_loc,s) -> 
        begin
          let code = s.txt in
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

(* let free_vars  = object *)
(*   inherit Objs.map as super *)
(*   method! exp (x:exp) = *)
(*     match x with *)
(*     | `Ant(_, (s:Tokenf.ant)) -> *)
        
(*     | p  -> super#exp p  *)
(* end *)
(* %exp{A.B.C.d.$x}     *)
(** [gl] is the name  list option

   {[
   loc -> ident option ->exp name list option ->
   (exp, 'a) entry list -> exp -> exp
   ]}

   This function generate some local entries *)               
let combine _loc (gram: vid option ) locals  extends  =
  let entry_mk =
    match gram with
    | Some g ->
        let g = (g:vid :> exp) in
        %exp{ $id{gm()}.mk_dynamic $g }
    | None -> %exp{ $id{gm()}.mk} in
  let local_bind_of_name (x:Gram_def.name) =
    match (x:Gram_def.name) with 
    | {id = `Lid (_,i) ; tvar = x; loc = _loc} ->
        %bind{ $lid:i =
               (grammar_entry_create $str:i : '$lid:x $id{(gm():vid :> ident)}.t )}
    | _  -> failwithf "internal error in the Grammar extension %s"
          (Objs.dump_vid x.id)   in
  match locals with
  | [] -> extends
  | ll ->
      let locals =
        ll |> List.map local_bind_of_name |> and_of_list in
      (** eta-expansion to avoid specialized types here  *)
      %exp{
      let grammar_entry_create x = $entry_mk  x in
      let $locals in
      $extends }    
    
(** entrance *)        
let make  _loc (x:Gram_def.entries) = 
  let extends =
    let el =
      x.items |> List.map (make_extend x.safe)    in
    match el with
    | [] ->  %exp{ () }
    | _ -> seq_sem el    in
  let locals  = (** FIXME the order matters here, check duplication later!!! *)
     x.items
     |> Listf.filter_map
         (fun (x:Gram_def.entry) -> if x.local then Some x.name else None ) in
  combine _loc x.gram locals extends




(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/compile_gram.cmo " *)
(* end: *)
