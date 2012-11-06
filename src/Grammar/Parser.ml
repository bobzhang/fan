
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

let try_parser ps strm =
  let strm' = Stream.dup strm in
  let r =
    try ps strm'
    with
    [ Stream.Error _ | FanLoc.Exc_located _ (Stream.Error _) ->
        raise Stream.Failure
    | exc -> raise exc ] in begin 
        Stream.njunk (Stream.count strm') strm ;
        r;
    end;

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
  [< a = (entry_of_symb entry s).econtinue 0 loc a;
     act = p1 ?? Failed.tree_failed entry a s son >] ->
  Action.mk (fun _ -> Action.getf act a);

(* PR#4603, PR#4330, PR#4551:
   Here get_cur_loc replaced get_prev_loc to fix all these bugs.
   If you do change it again look at these bugs. *)
let skip_if_empty bp strm =
  if get_cur_loc strm = bp then
    Action.mk (fun _ -> raise Stream.Failure)
  else
    raise Stream.Failure ;

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

let rec parser_of_tree entry nlevn alevn = fun
  [ DeadEnd -> parser []

  | LocAct act _ -> parser [< >] -> act
        
  | Node {node = `Sself; son = LocAct act _; brother = DeadEnd}
    ->
      parser
      [ [< a = entry.estart alevn >] -> Action.getf act a] (* estart only use aleven *)
          
  | Node {node = `Sself; son = LocAct act _; brother = bro}
    ->
      let p2 = parser_of_tree entry nlevn alevn bro in
      parser
        [ [< a = entry.estart alevn >] -> Action.getf act a
        | [< a = p2 >] -> a ]
            
  | Node {node = s; son = son; brother = DeadEnd} ->
      let tokl =
        match s with
        [ `Stoken _ | `Skeyword _ ->
          Tools.get_token_list s son
        | _ -> None ] in
      match tokl with
      [ None ->
        let ps = parser_of_symbol entry nlevn s in (*only use nleven*)
        let p1 =
          parser_of_tree entry nlevn alevn son
          |> parser_cont  entry nlevn alevn s son in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [[< a = ps; act = p1 bp a >] -> Action.getf act a]
       | Some (tokl, last_tok, son) ->
           parser_of_tree entry nlevn alevn son
           |> parser_cont  entry nlevn alevn last_tok son
           |> parser_of_symbols tokl ]
  | Node {node = s; son = son; brother = bro} ->
      let tokl =  match s with
       [ `Stoken _ | `Skeyword _ ->
         Tools.get_token_list s son
       | _ -> None ] in
      match tokl with
      [ None ->
        let ps = parser_of_symbol entry nlevn s in
        let p1 =
          parser_of_tree entry nlevn alevn son
          |> parser_cont  entry nlevn alevn s son in
        let p2 = parser_of_tree entry nlevn alevn bro in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [ [< a = ps; act = p1 bp a >] -> Action.getf act a
          | [< a = p2 >] -> a ]
      | Some (tokl, last_tok, son) ->
          let p1 =
            parser_of_tree entry nlevn alevn son
            |> parser_cont  entry nlevn alevn last_tok son
            |>  parser_of_symbols tokl in 
          let p2 = parser_of_tree entry nlevn alevn bro in
            parser
            [ [< a = p1 >] -> a
            | [< a = p2 >] -> a ] ] ]

and parser_cont  entry nlevn alevn s son p1 loc a =  parser
  [ [< b = p1 >] -> b
  | [< b = recover parser_of_tree entry nlevn alevn  s son loc a >] -> b
  | [< >] -> raise (Stream.Error (Failed.tree_failed entry a s son)) ]
and parser_of_symbols tokl p1 =
  let rec loop n =  fun
    [ [`Stoken (tematch, _) :: tokl] ->
      match tokl with
      [ [] ->
        let ps strm =
          match Stream.peek_nth n strm  with
          [ Some (tok, _) when tematch tok ->
            (Stream.njunk n strm ; Action.mk tok)
          | _ -> raise Stream.Failure ] in
        fun strm ->
          let bp = get_cur_loc strm in
          match strm with parser
          [[< a = ps; act = p1 bp a >] -> Action.getf act a]
      | _ ->
          let ps strm =
            match Stream.peek_nth n strm with
            [ Some (tok, _) when tematch tok -> tok
            | _ -> raise Stream.Failure ] in
          let p1 = loop (n + 1) tokl in
          parser
              [[< tok = ps; 's >] ->
                let act = p1 s in Action.getf act tok ]]
    | [`Skeyword kwd :: tokl] ->
          match tokl with
          [ [] ->
            let ps strm =
              match Stream.peek_nth n strm with
              [ Some (tok, _) when FanToken.match_keyword kwd tok ->
                (Stream.njunk n strm ; Action.mk tok)
              | _ -> raise Stream.Failure ] in
            fun strm ->
              let bp = get_cur_loc strm in
              match strm with parser
              [ [< a = ps; act = p1 bp a >] -> Action.getf act a]
          | _ ->
              let ps strm =
                match Stream.peek_nth n strm with
                [ Some (tok, _) when FanToken.match_keyword kwd tok -> tok
                | _ -> raise Stream.Failure ] in
              let p1 = loop (n + 1) tokl in
              parser
                  [[< tok = ps; 's >] ->
                    let act = p1 s in Action.getf act tok ]]
      | _ -> invalid_arg "parser_of_symbols" ] in
  loop 1 tokl 

and parser_of_symbol entry nlevn = fun
  [ `Smeta _ symbl act ->
    let act = Obj.magic act entry symbl in
    let pl = List.map (parser_of_symbol entry nlevn) symbl in
    Obj.magic (List.fold_left (fun act p -> Obj.magic act p) act pl)
  | `Slist0 s ->
    let ps = parser_of_symbol entry nlevn s in
    let rec loop al = parser
      [ [< a = ps; 's >] -> loop [a :: al] s
      | [< >] -> al ] in
    parser [< a = loop [] >] -> Action.mk (List.rev a)
   | `Slist0sep (symb, sep) ->
     let ps = parser_of_symbol entry nlevn symb in
     let pt = parser_of_symbol entry nlevn sep in
     let rec kont al = parser
       [ [< v = pt; a = ps ?? Failed.symb_failed entry v sep symb; 's >] ->
         kont [a :: al] s
       | [< >] -> al ] in
     parser
       [ [< a = ps; 's >] -> Action.mk (List.rev (kont [a] s))
       | [< >] -> Action.mk [] ]
   | `Slist1 s ->
     let ps = parser_of_symbol entry nlevn s in
     let rec loop al = parser
       [ [< a = ps; 's >] -> loop [a :: al] s
       | [< >] -> al ] in
     parser [< a = ps; 's >] -> Action.mk (List.rev (loop [a] s))
   | `Slist1sep (symb, sep) ->
     let ps = parser_of_symbol entry nlevn symb in
     let pt = parser_of_symbol entry nlevn sep in
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
      let ps = parser_of_symbol entry nlevn s in parser
        [ [< a = ps >] -> Action.mk (Some a)
        | [< >] -> Action.mk None ]
  | `Stry s ->
      let ps = parser_of_symbol entry nlevn s in
      try_parser ps
  | `Stree t ->
      let pt = parser_of_tree entry 1 0 t in fun strm ->
        let bp = get_cur_loc strm in
        match strm with parser
        [ [< (act, loc) = add_loc bp pt >] ->  Action.getf act loc]
  | `Snterm e -> parser [< a = e.estart 0 >] -> a
  | `Snterml (e, l) -> parser [< a = e.estart (level_number e l) >] -> a
  | `Sself -> parser [< a = entry.estart 0 >] -> a
  | `Snext -> parser [< a = entry.estart nlevn >] -> a
  | `Skeyword kwd -> parser
        [ [< (tok, _) when FanToken.match_keyword kwd tok >] ->
          Action.mk tok ]
 | `Stoken (f, _) ->
     parser [ [< (tok,_) when f tok >] -> Action.mk tok ]]
and parse_top_symb entry symb strm =
  parser_of_symbol entry 0 (top_symb entry symb) strm;

let start_parser_of_levels entry =
  let rec aux clevn : list level -> int -> parse Action.t =  fun
    [ [] -> fun _ -> parser []
    | [lev :: levs] ->
        let p1 = aux  (clevn+1) levs in
        match lev.lprefix with
        [ DeadEnd -> p1 (* try the upper levels *)
        | tree ->
          let alevn =
            match lev.assoc with
            [ `LA | `NA ->  clevn + 1 | `RA -> clevn ] in
          let p2 = (* *)
            parser_of_tree entry (1 + clevn) alevn tree in
          match levs with
          [ [] ->
            fun levn strm ->
              let bp = get_cur_loc strm in
              match strm with parser
              [ [< (act, loc) = add_loc bp p2; 'strm >] ->
                let a = Action.getf act loc in
                entry.econtinue levn loc a strm]
          | _ ->
             fun levn strm ->
               if levn > clevn then
                 p1 levn strm (* only higher level allowed here *)
               else
                 let bp = get_cur_loc strm in
                 match strm with parser
                 [ [< (act, loc) = add_loc bp p2 >] ->
                     let a = Action.getf act loc in
                     entry.econtinue levn loc a strm
                 | [< act = p1 levn >] -> act ] ] ] ] in
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
      let p1 = continue_parser_of_levels entry  (clevn+1) levs in
      match lev.lsuffix with
      [ DeadEnd -> p1
      | tree ->
        let alevn =
          match lev.assoc with
          [ `LA | `NA -> succ clevn
          | `RA -> clevn ] in
        let p2 = parser_of_tree entry (1+ clevn) alevn tree in
        fun levn bp a strm ->
          if levn > clevn then
            p1 levn bp a strm
          else
            match strm with parser
            [ [< act = p1 levn bp a >] -> act
            | [< (act, loc) = add_loc bp p2 >] ->
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


