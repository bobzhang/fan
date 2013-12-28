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

open Astf
open Util


let print_warning = eprintf "%a:\n%s@." Locf.print

  
let prefix = "__fan_"  
let ghost = Locf.ghost

let module_name =
  ref %exp'@ghost{Gramf} (* BOOTSTRAPING -- *)  

let gm () : vid  =
  match !Configf.compilation_unit with
  |Some "Gramf" (* BOOTSTRAPING*)
    -> `Uid(ghost,"")
  |Some _
  | None -> !module_name

  
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
      (function  (p:Gram_def.osymbol list  Gram_def.decorate) ->
        match p with (* ? Lid i ? Lid *)
        | {kind = KSome;   txt } ->
            begin 
              Some
                (txt |> List.map 
                  (fun (symbol:Gram_def.osymbol) ->
                    match symbol with
                    | {outer_pattern = None; bounds; _} -> 
                        let id = prefix ^ string_of_int !i in
                        begin
                          enhance_env  id bounds env;
                          List.iter
                            (fun (((xloc,id) as z),_) -> add ~check:false (z, %exp@xloc{Some $lid:id}) env) bounds ;
                          incr i;
                          symbol;
                        end
                    | ({outer_pattern = Some ((xloc,id) as z) ;
                        bounds;  _} as s)
                      -> 
                    begin
                      enhance_env id bounds env ;
                      add ~check:false (z, %exp@xloc{Some $lid:id } ) env;
                      List.iter
                        (fun (((xloc,id) as z),_) ->  add ~check:false (z, %exp@xloc{Some $lid:id}) env)
                        bounds;
                      incr i;
                      s
                    end ))
            end
              
        | {kind = KNormal; txt}
          ->
            Some
              (List.map (fun (symbol:Gram_def.osymbol)  -> 
                match symbol with 
                | {outer_pattern = None; bounds;_} -> 
                    let id = prefix ^ string_of_int !i in
                    begin
                      enhance_env id bounds env ;
                      incr i;
                      symbol 
                    end
                | ({outer_pattern = Some (_,id); bounds;_} as symbol) ->
                    begin
                      enhance_env id bounds env;
                      incr i;
                      symbol
                    end) txt )

        | {kind = KNone; txt}
          ->
            begin 
              List.iter
              (fun (symbol:Gram_def.osymbol) ->
                  match symbol with
                  | {outer_pattern = None ; bounds; _} ->
                      begin
                        List.iter
                          (fun (((xloc,_) as z),_) ->
                            add (z, %exp@xloc{None}) env) bounds
                      end

                  | {outer_pattern = Some ((xloc,_) as z);
                     bounds; _} ->
                       begin
                         add (z, %exp@xloc{None}) env;
                         List.iter
                           (fun (((xloc,_) as z),_) ->
                             add (z , %exp@xloc{None}) env) bounds
                       end) txt;
              None
            end) prod in
    ({prod = List.concat prod ;
      action;
      env = List.rev !env }:Gram_def.rule)


let rec make_exp (tvar : string) (x:Gram_def.text) =
  let rec aux tvar (x:Gram_def.text) =
    match x with
    | List (_loc, min, t, ts) ->
        let txt = aux "" t.text in
        (match  ts with
        |  None -> if min then  %exp{ List1 $txt } else %exp{ List0 $txt } 
        | Some s ->
            let x = aux tvar s.text in
            if min then %exp{ List1sep ($txt,$x)} else %exp{ List0sep ($txt,$x) })
    | Self _loc ->  %exp{ Self}
    | Keyword (_loc, kwd) ->  %exp{ `Keyword $str:kwd }
    | Nterm (_loc, n, lev) ->
        let obj =
          (* [$id] -- type annotation needed *)
          %exp{ ($id{gm()}.obj
                   (${n.id} : '$lid{n.tvar} $id{(gm():vid)}.t ))} in 
        (match lev with
        | Some lab -> %exp{ Snterml ($obj,$int':lab)}
        | None ->
           if n.tvar = tvar then %exp{Self} else %exp{ Nterm $obj })
    | Try (_loc, t) -> %exp{ Try ${aux "" t} }
    | Peek (_loc, t) -> %exp{ Peek ${aux "" t} }
    | Token (_loc, meta) ->
        %exp{ Token $meta} in
  aux  tvar x


and make_exp_rules (rl : (Gram_def.text list  * exp * Gram_def.action ) list)
    (tvar:string) =
  rl
  |> List.map (fun (sl,action, (raw:Gram_def.action)) ->
      let action_string =
        match raw with
        | E None -> ""
        | E (Some e) ->  Ast2pt.to_string_exp e
        | Ant _ -> "" (* FIXME antiquot *) in
      let sl =
        sl
        |> List.map (make_exp tvar)
        |> list_of_list  in
      let _loc = Ast_loc.loc_of sl in 
      (* %exp{ ($sl,($str:action_string,$action)) } *)
      %exp{{symbols = $sl; annot  = $str:action_string; fn = $action}}
              )
  |> list_of_list 

(**********************************************)
(* generate action of the right side   *)
(**********************************************)      
let make_action (_loc:loc)
    (x:Gram_def.rule)
    (rtvar:string)  : exp = 
  let locid = %pat{ $lid{!Locf.name} } in
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
  match x.action with
  | Ant v ->
      let e =  Tokenf.ant_expand Parsef.exp v in
      let ty =
         List.fold_left
        (fun  ty (s:Gram_def.osymbol) ->
          let t = make_ctyp s.styp rtvar in
          %ctyp{$t -> $ty})
          (%ctyp{Locf.t -> '$lid:rtvar}) x.prod in
      %exp{ $id{gm()}.mk_action ($e : $ty)}
  | E v  -> 
      let e =
        let act = Option.default %exp{()} v (* x.action *) in
        let make_env env =
          env |> List.map (fun ((loc,id),e) -> %bind{${%pat@loc{$lid:id}} = $e}) in
        let binds = make_env x.env in
        let e1 = %exp{ ($act : '$lid:rtvar ) } in
        let e1 = Ast_gen.seq_binds binds e1 in
        %exp{fun ($locid : Locf.t) -> $e1 } in
      let (ty,txt) =
        snd @@
        Listf.fold_lefti
          (fun i (ty,txt) (s:Gram_def.osymbol) ->
            match (s.outer_pattern, s.bounds) with
            | (Some (xloc,id),_)  -> (* (u:Tokenf.t)   *)
                let t  = make_ctyp s.styp rtvar in
                let p =  %pat@xloc{$lid:id} +:  t in
                (%ctyp{$t -> $ty}, %exp{ fun $p -> $txt })
            | (None, [] ) ->
                let t = make_ctyp s.styp rtvar in
                (%ctyp{$t -> $ty}, %exp{ fun _ -> $txt })
            | (None, _ ) -> (* __fan_i, since we have inner binding*)
                let t = make_ctyp s.styp rtvar in
                let p =
                  %pat{ $lid{prefix^string_of_int i} } +: t  in
                (%ctyp{$t -> $ty}, %exp{ fun $p -> $txt }))
          (%ctyp{Locf.t -> '$lid:rtvar},e) x.prod in
      %exp{ $id{gm()}.mk_action ( $txt : $ty) }


  


(*
  return [(ent,pos,txt)] the [txt] has type [olevel],
  [ent] is something like
  {[
  (module_exp : 'mexp Gramf.t )
  ]}
  [pos] is something like
  {[(Some `LA)]} it has type [position option] *)
let make_single_extend_statement (e : Gram_def.entry)  : exp =
  let _loc = e.name.loc in
  let gmid = (gm():vid:>ident) in (* FIXME *)
  let ent =
    %exp{(${e.name.id}:'$lid{e.name.tvar} $id:gmid.t)  }   in
  let pos = (* hastype Gramf.position option *)
    match e.pos with
    | Some pos -> %exp{Some $pos} 
    | None -> %exp{None}   in
  let apply (level:Gram_def.level)  =
    let ass =
      match level.assoc with (* has type Gramf.assoc option *)
      | Some ass ->   ass 
      | None ->    %exp{true}   in
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
    %exp{
    ({label = $pos; lassoc = $ass; productions = $prod } :
       $id{(gm():vid)}.olevel)} in
  let l = e.level in
  %exp{({entry = $ent ; olevel = ${apply l} } : _ Gramf.single_extend_statement)}

let make_extend safe  (e:Gram_def.entry) :exp =
  let _loc = e.name.loc in
  let f =
    if safe then
      %exp{$id{gm()}.extend_single}
    else %exp{$id{gm()}.unsafe_extend_single} in
  %exp{$f ${make_single_extend_statement e}}

      
(** [gl] is the name  list option

   {[
   loc -> ident option ->exp name list option ->
   (exp, 'a) entry list -> exp -> exp
   ]}

   This function generate some local entries *)               
let make_localbinds _loc locals   =
  let local_bind_of_name (x:Gram_def.name) =
    match (x:Gram_def.name) with 
    | {id = `Lid (_,i) ; tvar = x; loc = _loc} ->
        %bind{ $lid:i = (* FIXME -- not type annotations*)
               (Gramf.mk $str:i : '$lid:x $id{(gm():vid)}.t )}
    | _  -> failwithf "internal error in the Grammar extension %s"
          (Objs.dump_vid x.id)   in
  List.map local_bind_of_name locals

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
  Ast_gen.binds (make_localbinds _loc locals) extends

let make_protects _loc (x:Gram_def.entries) action =
  let (_(* locals *),globals) =
    List.partition (fun (x:Gram_def.entry) -> x.local) x.items in
  (* let local_names  = (\** FIXME the order matters here, check duplication later!!! *\) *)
  (*   List.map (fun (x:Gram_def.entry) ->  x.name) locals in *)
  (* let binds = make_localbinds _loc local_names in *)
  (* let local_extends = *)
  (*   let el =  List.map (make_extend x.safe) locals  in *)
  (*   match el with *)
  (*   | [] -> %exp{()} *)
  (*   | _ -> seq_sem el in *)
  (* let global_extends = *)
  (*   list_of_list (List.map make_single_extend_statement globals) in *)
  let binds = List.map
      (fun (x:Gram_def.entry) ->
        (%fresh{tmp_entry}, %exp{${x.name.id}})) globals in
  let save_binds =
    List.map (fun (tmp, e) ->  %bind{  $lid:tmp = Gramf.get_levels $e }) binds in
  let pop_actions =
    seq_sem @@
    Listf.map (fun (tmp, e) -> %exp{Gramf.fresh_with_levels $e $lid:tmp } ) binds  in
  let e = make _loc x in
  Ast_gen.binds save_binds %exp{
  try begin
    $e;
    let result  = $action in
    begin 
      $pop_actions;
      result 
    end
  end
  with
    x ->
      begin
        $pop_actions;
        raise x;
      end
      }
  (* let save = List.map  *)
  (* Ast_gen.binds binds *)
  (*   %exp{ *)
  (* begin  *)
  (*   $local_extends; *)
  (*   Gramf.protects $global_extends *)
  (*     (fun _ -> *)
  (*       $action) *)
  (* end} *)

(* let make_protects _loc (x:Gram_def.entries) action = *)
(*   let (locals,globals) = *)
(*     x.items *)
(*     |> List.partition (fun (x:Gram_def.entry) -> x.local) in *)
  

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/compile_gram.cmo " *)
(* end: *)
