
open Structure;
open LibUtil;


let get_cur_loc = Tools.get_cur_loc;
let get_prev_loc = Tools.get_prev_loc;

(* [bp] means begining position, [ep] means ending position
   apply the [parse_fun] and get the result and the location of
   consumed areas
 *)

let add_loc bp parse_fun strm =
  let x = parse_fun strm in
  let ep = get_prev_loc strm in
  let loc =
    if FanLoc.start_off bp > FanLoc.stop_off ep then
      (* If nothing has been consumed, create a 0-length location. *)
      FanLoc.join bp
    else
      FanLoc.merge bp ep in
  (x, loc);


(* given a level string, return a number from 0 *)  
let level_number entry lab =
  let rec lookup levn = fun
    [ [] -> failwith ("unknown level " ^ lab)
    | [lev :: levs] ->
        if Tools.is_level_labelled lab lev then levn else lookup (succ levn) levs ] in
  match entry.edesc with
  [ Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found ] ;
    

(* given an entry and a symbol return the top symbol *)
let rec top_symb entry =fun
  [ `Sself | `Snext -> `Snterm entry
  | `Snterml (e, _) -> `Snterm e
  | `Slist1sep (s, sep) -> `Slist1sep ((top_symb entry s), sep)
  | _ -> raise Stream.Failure ];

(* given an entry and a tree return the top tree *)  
let top_tree entry = fun
  [ Node ({node = s; _} as x) ->
    Node ({(x) with node = top_symb entry s})
  | LocAct _ _ | DeadEnd -> raise Stream.Failure ];

let entry_of_symb entry = fun
  [ `Sself | `Snext -> entry
  | `Snterm e -> e
  | `Snterml (e, _) -> e
  | _ -> raise Stream.Failure ] ;

let continue entry  s son (p1:parse Action.t) loc a = parser
  [[< a = (entry_of_symb entry s).econtinue 0 loc a;
     act = p1 ?? Failed.tree_failed entry a s son >] ->
       Action.mk (fun _ -> Action.getf act a) ];

(* PR#4603, PR#4330, PR#4551:
   Here get_cur_loc replaced get_prev_loc to fix all these bugs.
   If you do change it again look at these bugs. *)
let skip_if_empty bp strm =
  if get_cur_loc strm = bp then
    Action.mk (fun _ -> raise Stream.Failure)
  else
    raise Stream.Failure ;


(* in case of syntax error, the system attempts to recover the error by applying
   the [continue] function of the previous symbol(if the symbol is a call to an entry),
   so there's no behavior difference between [LA] and [NA]
 *)
let do_recover parser_of_tree entry nlevn alevn  s son loc a = parser
  [ [< b = parser_of_tree entry nlevn alevn (top_tree entry son) >] -> b
  | [< b = skip_if_empty loc >] -> b
  | [< b = continue entry  s son (parser_of_tree entry nlevn alevn son) loc a >] ->  b];


let recover parser_of_tree entry nlevn alevn  s son loc a strm =
  if !FanConfig.strict_parsing then
    raise (Stream.Error (Failed.tree_failed entry a s son))
  else
    let _ =
      if !FanConfig.strict_parsing_warning then begin
        let msg = Failed.tree_failed entry a s son;
          Format.eprintf "Warning: trying to recover from syntax error";
          if entry.ename <> "" then Format.eprintf " in [%s]" entry.ename else ();
          Format.eprintf "\n%s%a@." msg FanLoc.print loc;
      end else () in
    do_recover parser_of_tree entry nlevn alevn  s son loc a strm ;


(*
  [aleven] was used by [estart]
  [nlevn] was used by [parser_of_symbol]
 *)    
let rec parser_of_tree entry nlevn alevn x =
  let rec aux  = fun 
  (* match x with  *)
  [ DeadEnd -> parser []

  | LocAct act _ -> parser [< >] -> act

  (* rules ending with [SELF] or with the current entry name, for this last symbol
     there's a call to the [start] function: of the current level if the level is
     [`RA] or of the next level otherwise. (This can be verified by
     [start_parser_of_levels])
   *)      
  | Node {node = `Sself; son = LocAct act _; brother = DeadEnd}
    ->
      parser
      [ [< a = entry.estart alevn >] -> Action.getf act a] (* estart only use aleven *)
          
  | Node {node = `Sself; son = LocAct act _; brother = bro}
    ->
      let p2 = aux bro in
      parser
        [ [< a = entry.estart alevn >] -> Action.getf act a
        | [< a = p2 >] -> a ]
            
  | Node ({node ; son ; brother = DeadEnd} as y) ->
      match Tools.get_terminals y with
      [ None ->
        let ps = parser_of_symbol entry node nlevn in (*only use nleven*)
        let p1 =  aux son |> parser_cont  entry nlevn alevn node son in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [[< a = ps; act = p1 bp a >] -> Action.getf act a]
       | Some (tokl, last_tok, son) ->
            aux son
           |> parser_cont entry nlevn alevn (last_tok:>symbol) son
           |> parser_of_terminals (* LL.test *) tokl ]
  | Node ({node ; son; brother } as y) ->
      match Tools.get_terminals  y with
      [ None ->
        let ps = parser_of_symbol entry node  nlevn  in
        let p1 = aux son |> parser_cont  entry nlevn alevn node son in
        let p2 = aux brother  in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [ [< a = ps; act = p1 bp a >] -> Action.getf act a
          | [< a = p2 >] -> a ]
      | Some (tokl, last_tok, son) ->
          let p1 =
            aux son
            |> parser_cont  entry nlevn alevn (last_tok:>symbol) son
            |>  parser_of_terminals (* LL.test *) tokl in 
          let p2 =  aux brother in
            parser
            [ [< a = p1 >] -> a
            | [< a = p2 >] -> a ] ] ] in
  aux x 

and parser_cont  entry nlevn alevn s son p1 loc a =  parser
  [ [< b = p1 >] -> b
  | [< b = recover parser_of_tree entry nlevn alevn  s son loc a >] -> b
  | [< >] -> raise (Stream.Error (Failed.tree_failed entry a s son)) ]

and parser_of_terminals tokl p1 =
  let rec loop n  =  fun
    [ [`Stoken (tematch, _) :: tokl] ->
      match tokl with
      [ [] ->
        let ps strm =
          match Stream.peek_nth strm n   with
          [ Some (tok, _) when tematch tok ->
            (Stream.njunk (n+1) strm ; Action.mk tok) (*at the end*)
          | _ -> raise Stream.Failure ] in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [[< a = ps; act = p1 bp a >] -> Action.getf act a]
              (* continuation *)
      | _ ->
          let ps strm =
            match Stream.peek_nth strm n  with
            [ Some (tok, _) when tematch tok -> tok
            | _ -> raise Stream.Failure ] in
          let p1 = loop (n + 1) tokl in
          parser
              [[< tok = ps; act=p1 >] -> Action.getf act tok ]]
    | [`Skeyword kwd :: tokl] ->
          match tokl with
          [ [] ->
            let ps strm =
              match Stream.peek_nth strm n  with
              [ Some (tok, _) when FanToken.match_keyword kwd tok ->
                (Stream.njunk (n+1) strm ; Action.mk tok)
              | _ -> raise Stream.Failure ] in
            fun strm ->
              let bp = get_cur_loc strm in
              match strm with parser
              [ [< a = ps; act = p1 bp a >] -> Action.getf act a]
          | _ ->
              let ps strm =
                match Stream.peek_nth strm n  with
                [ Some (tok, _) when FanToken.match_keyword kwd tok -> tok
                | _ -> raise Stream.Failure ] in
              let p1 = loop (n + 1) tokl in
              parser
                  [[< tok = ps; act=p1  >] -> Action.getf act tok ]]
      | _ -> invalid_arg "parser_of_terminals" ] in
  loop 0 tokl 

and parser_of_symbol entry s nlevn =
  let rec aux s = 
  match s with 
  [ `Smeta _ symbls act ->
    let act = Obj.magic act entry symbls in
    let pl = List.map aux symbls in
    Obj.magic (List.fold_left (fun act p -> Obj.magic act p) act pl)
  | `Slist0 s ->
    let ps = aux s in
    (* let rec loop al = parser *)
    (*   [ [< a = ps; 's >] -> loop [a :: al] s *)
    (*   | [< >] -> al ] in *)
    (* parser [< a = loop [] >] -> Action.mk (List.rev a) *)
    Comb.slist0 ps ~f:(fun l -> Action.mk (List.rev l))
   | `Slist0sep (symb, sep) ->
     let ps = aux symb  in
     let pt =  aux sep  in
     (* let rec kont al = parser *)
     (*   [ [< v = pt; a = ps ?? Failed.symb_failed entry v sep symb; 's >] -> *)
     (*     kont [a :: al] s *)
     (*   | [< >] -> al ] in *)
     (* parser *)
     (*   [ [< a = ps; 's >] -> Action.mk (List.rev (kont [a] s)) *)
     (*   | [< >] -> Action.mk [] ] *)
     Comb.slist0sep ps pt ~err:(fun v -> Failed.symb_failed entry v sep symb)
       ~f:(fun l -> Action.mk (List.rev l))
   | `Slist1 s ->
     let ps =  aux s  in
     Comb.slist1 ps ~f:(fun l -> Action.mk (List.rev l))
     (* let rec loop al = parser *)
     (*   [ [< a = ps; 's >] -> loop [a :: al] s *)
     (*   | [< >] -> al ] in *)
     (* parser [< a = ps; 's >] -> Action.mk (List.rev (loop [a] s)) *)
   | `Slist1sep (symb, sep) ->
     let ps = aux symb  in
     let pt = aux sep  in
     let rec kont al = parser
       [ [< v = pt; a = parser
         [ [< a = ps >] -> a
         | [< a = parse_top_symb entry symb >] -> a
         | [< >] ->
             raise (Stream.Error (Failed.symb_failed entry v sep symb)) ];
           's >] ->kont [a :: al] s
         | [< >] -> al ] in
     parser [< a = ps; 's >] -> Action.mk (List.rev (kont [a] s))
  | `Sopt s ->
      let ps = aux s  in
      Comb.opt ps ~f:Action.mk
      (* parser *)
      (*   [ [< a = ps >] -> Action.mk (Some a) *)
      (*   | [< >] -> Action.mk None ] *)
  | `Stry s ->
      let ps = aux s in Comb.tryp ps
  | `Stree t ->
      let pt = parser_of_tree entry 1 0 t in fun strm ->
        let bp = get_cur_loc strm in
        match strm with parser
        [ [< (act, loc) = add_loc bp pt >] ->  Action.getf act loc]
  | `Snterm e -> parser [< a = e.estart 0 >] -> a
        (* When a [SELF] or the current entry name is encountered in the middle of the rule(i.e, if it's not
           the last symbol), there is a call to the [start] function of the first level of the current entry.
         *)
  | `Snterml (e, l) -> parser [< a = e.estart (level_number e l) >] -> a
  | `Sself -> parser [< a = entry.estart 0 >] -> a
  | `Snext -> parser [< a = entry.estart nlevn >] -> a
  | `Skeyword kwd -> parser
        [ [< (tok, _) when FanToken.match_keyword kwd tok >] ->
          Action.mk tok ]
 | `Stoken (f, _) ->
     parser [ [< (tok,_) when f tok >] -> Action.mk tok ]] in
  aux s
and parse_top_symb entry symb strm =
  parser_of_symbol entry (top_symb entry symb) 0  strm;


(* entrance for the start
   [clevn] is the current level 
 *)  
let start_parser_of_levels entry =
  let rec aux clevn : list level -> int -> parse Action.t =  fun
    [ [] -> fun _ -> parser [] (* end *)
    | [lev :: levs] ->
        let hstart = aux  (clevn+1) levs in
        match lev.lprefix with
        [ DeadEnd -> hstart (* try the upper levels *)
        | tree ->
          let alevn =
            match lev.assoc with
            [ `LA | `NA ->  clevn + 1 | `RA -> clevn ] in
          let cstart = (* *)
            parser_of_tree entry (1 + clevn) alevn tree in
          (*
            （1 + clevn） was used by [parser_of_symbol]
             [alevn] was used by [entry.estart]
           *)
          match levs with
          [ [] ->
            fun levn strm ->
              let bp = get_cur_loc strm in
              match strm with parser
              [ [< (act, loc) = add_loc bp cstart; 'strm >] ->
                let a = Action.getf act loc in
                entry.econtinue levn loc a strm]
              (* the [start] function tires its associated tree. If it works
                 it calls the [continue] function of the same level, giving the
                 result of [start] as parameter.
                 If this continue fails, the parameter is simply returned.
                 If this [start] function fails, the start function of the
                 next level is tested, if there is no more levels, the parsing fails
               *)    
          | _ ->
             fun levn strm ->
               if levn > clevn then
                 hstart levn strm (* only higher level allowed here *)
               else
                 let bp = get_cur_loc strm in
                 match strm with parser
                 [ [< (act, loc) = add_loc bp cstart >] ->
                     let a = Action.getf act loc in
                     entry.econtinue levn loc a strm
                 | [< act = hstart levn >] -> act ] ] ] ] in
  aux 0;
  
let start_parser_of_entry entry =
  debug gram "start_parser_of_entry: @[<2>%a@]@." Print.text#entry entry in
  match entry.edesc with
  [ Dlevels [] -> Tools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry  elev
  | Dparser p -> fun _ -> p ] ;
    


let rec continue_parser_of_levels entry clevn = fun
  [ [] -> fun _ _ _ -> parser []
  | [lev :: levs] ->
      let hcontinue = continue_parser_of_levels entry  (clevn+1) levs in
      match lev.lsuffix with
      [ DeadEnd -> hcontinue
          (* the continue function first tries the [continue] function of the next level,
             if it fails or if it's the last level, it tries its associated tree, then
             call itself again, giving the result as parameter. If the associated tree
             fails, it returns its extra parameter
           *)
      | tree ->
        let alevn =
          match lev.assoc with
          [ `LA | `NA -> clevn+1
          | `RA -> clevn ] in
        let ccontinue = parser_of_tree entry (1+ clevn) alevn tree in
        fun levn bp a strm ->
          if levn > clevn then
            hcontinue levn bp a strm
          else
            match strm with parser
            [ [< act = hcontinue levn bp a >] -> act
            | [< (act, loc) = add_loc bp ccontinue >] ->
                let a = Action.getf2 act a loc in
                entry.econtinue levn loc a strm ] ] ];

(*
  {[Structure.internal_entry ->
    int ->
    FanLoc.t ->
    Action.t -> efun ]}*)  
let continue_parser_of_entry entry =
  debug gram "continue_parser_of_entry: @[<2>%a@]@." Print.text#entry entry in
  match entry.edesc with
  [ Dlevels elev ->
    let p = continue_parser_of_levels entry 0 elev in
    fun levn bp a -> parser
    [ [< a = p levn bp a >] -> a
    | [< >] -> a ]
  | Dparser _ -> fun _ _ _ -> parser [] ];


