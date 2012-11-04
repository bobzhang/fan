open Structure
open LibUtil
open FanUtil
let get_cur_loc = Tools.get_cur_loc
let get_prev_loc = Tools.get_prev_loc
let add_loc bp parse_fun strm =
  let x = parse_fun strm in
  let ep = get_prev_loc strm in
  let loc =
    if (FanLoc.start_off bp) > (FanLoc.stop_off ep)
    then FanLoc.join bp
    else FanLoc.merge bp ep in
  (x, loc)
let try_parser ps strm =
  let strm' = Stream.dup strm in
  let r =
    try ps strm'
    with
    | Stream.Error _|FanLoc.Exc_located (_,Stream.Error _) ->
        raise Stream.Failure
    | exc -> raise exc in
  njunk (Stream.count strm') strm; r
let level_number entry lab =
  let rec lookup levn =
    function
    | [] -> failwith ("unknown level " ^ lab)
    | lev::levs ->
        if Tools.is_level_labelled lab lev
        then levn
        else lookup (succ levn) levs in
  match entry.edesc with
  | Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found
let strict_parsing = ref false
let strict_parsing_warning = ref false
let rec top_symb entry =
  function
  | `Sself|`Snext -> `Snterm entry
  | `Snterml (e,_) -> `Snterm e
  | `Slist1sep (s,sep) -> `Slist1sep ((top_symb entry s), sep)
  | _ -> raise Stream.Failure
let top_tree entry =
  function
  | Node { node = s; brother = bro; son } ->
      Node { node = (top_symb entry s); brother = bro; son }
  | LocAct (_,_)|DeadEnd  -> raise Stream.Failure
let entry_of_symb entry =
  function
  | `Sself|`Snext -> entry
  | `Snterm e -> e
  | `Snterml (e,_) -> e
  | _ -> raise Stream.Failure
let continue entry loc a s son p1 (__strm : _ Stream.t) =
  let a = (entry_of_symb entry s).econtinue 0 loc a __strm in
  let act =
    try p1 __strm
    with
    | Stream.Failure  ->
        raise (Stream.Error (Failed.tree_failed entry a s son)) in
  Action.mk (fun _  -> Action.getf act a)
let skip_if_empty bp strm =
  if (get_cur_loc strm) = bp
  then Action.mk (fun _  -> raise Stream.Failure)
  else raise Stream.Failure
let do_recover parser_of_tree entry nlevn alevn loc a s son
  (__strm : _ Stream.t) =
  try parser_of_tree entry nlevn alevn (top_tree entry son) __strm
  with
  | Stream.Failure  ->
      (try skip_if_empty loc __strm
       with
       | Stream.Failure  ->
           continue entry loc a s son (parser_of_tree entry nlevn alevn son)
             __strm)
let recover parser_of_tree entry nlevn alevn loc a s son strm =
  if strict_parsing.contents
  then raise (Stream.Error (Failed.tree_failed entry a s son))
  else
    (let _ =
       if strict_parsing_warning.contents
       then
         let msg = Failed.tree_failed entry a s son in
         (Format.eprintf "Warning: trying to recover from syntax error";
          if entry.ename <> ""
          then Format.eprintf " in [%s]" entry.ename
          else ();
          Format.eprintf "\n%s%a@." msg FanLoc.print loc)
       else () in
     do_recover parser_of_tree entry nlevn alevn loc a s son strm)
let rec parser_of_tree entry nlevn alevn =
          function
          | DeadEnd  -> (fun (__strm : _ Stream.t)  -> raise Stream.Failure)
          | LocAct (act,_) -> (fun (__strm : _ Stream.t)  -> act)
          | Node { node = `Sself; son = LocAct (act,_); brother = DeadEnd  }
              ->
              (fun (__strm : _ Stream.t)  ->
                 let a = entry.estart alevn __strm in Action.getf act a)
          | Node { node = `Sself; son = LocAct (act,_); brother = bro } ->
              let p2 = parser_of_tree entry nlevn alevn bro in
              (fun (__strm : _ Stream.t)  ->
                 match try Some (entry.estart alevn __strm)
                       with | Stream.Failure  -> None
                 with
                 | Some a -> Action.getf act a
                 | _ -> p2 __strm)
          | Node { node = s; son; brother = DeadEnd  } ->
              let tokl =
                match s with
                | `Stoken _|`Skeyword _ -> Tools.get_token_list s son
                | _ -> None in
              (match tokl with
               | None  ->
                   let ps = parser_of_symbol entry nlevn s in
                   let p1 = parser_of_tree entry nlevn alevn son in
                   let p1 = parser_cont p1 entry nlevn alevn s son in
                   (fun strm  ->
                      let bp = get_cur_loc strm in
                      let (__strm :_ Stream.t)= strm in
                      let a = ps __strm in
                      let act =
                        try p1 bp a __strm
                        with | Stream.Failure  -> raise (Stream.Error "") in
                      Action.getf act a)
               | Some (tokl,last_tok,son) ->
                   let p1 = parser_of_tree entry nlevn alevn son in
                   let p1 = parser_cont p1 entry nlevn alevn last_tok son in
                   parser_of_token_list p1 tokl)
          | Node { node = s; son; brother = bro } ->
              let tokl =
                match s with
                | `Stoken _|`Skeyword _ -> Tools.get_token_list s son
                | _ -> None in
              (match tokl with
               | None  ->
                   let ps = parser_of_symbol entry nlevn s in
                   let p1 = parser_of_tree entry nlevn alevn son in
                   let p1 = parser_cont p1 entry nlevn alevn s son in
                   let p2 = parser_of_tree entry nlevn alevn bro in
                   (fun strm  ->
                      let bp = get_cur_loc strm in
                      let (__strm :_ Stream.t)= strm in
                      match try Some (ps __strm)
                            with | Stream.Failure  -> None
                      with
                      | Some a ->
                          let act =
                            try p1 bp a __strm
                            with | Stream.Failure  -> raise (Stream.Error "") in
                          Action.getf act a
                      | _ -> p2 __strm)
               | Some (tokl,last_tok,son) ->
                   let p1 = parser_of_tree entry nlevn alevn son in
                   let p1 = parser_cont p1 entry nlevn alevn last_tok son in
                   let p1 = parser_of_token_list p1 tokl in
                   let p2 = parser_of_tree entry nlevn alevn bro in
                   (fun (__strm : _ Stream.t)  ->
                      try p1 __strm with | Stream.Failure  -> p2 __strm))
and parser_cont p1 entry nlevn alevn s son loc a (__strm : _ Stream.t) =
      try p1 __strm
      with
      | Stream.Failure  ->
          (try recover parser_of_tree entry nlevn alevn loc a s son __strm
           with
           | Stream.Failure  ->
               raise (Stream.Error (Failed.tree_failed entry a s son)))
and parser_of_token_list p1 tokl =
      let rec loop n =
        function
        | (`Stoken (tematch,_))::tokl ->
            (match tokl with
             | [] ->
                 let ps strm =
                   match stream_peek_nth n strm with
                   | Some (tok,_) when tematch tok ->
                       (njunk n strm; Action.mk tok)
                   | _ -> raise Stream.Failure in
                 (fun strm  ->
                    let bp = get_cur_loc strm in
                    let (__strm :_ Stream.t)= strm in
                    let a = ps __strm in
                    let act =
                      try p1 bp a __strm
                      with | Stream.Failure  -> raise (Stream.Error "") in
                    Action.getf act a)
             | _ ->
                 let ps strm =
                   match stream_peek_nth n strm with
                   | Some (tok,_) when tematch tok -> tok
                   | _ -> raise Stream.Failure in
                 let p1 = loop (n + 1) tokl in
                 (fun (__strm : _ Stream.t)  ->
                    let tok = ps __strm in
                    let s = __strm in let act = p1 s in Action.getf act tok))
        | (`Skeyword kwd)::tokl ->
            (match tokl with
             | [] ->
                 let ps strm =
                   match stream_peek_nth n strm with
                   | Some (tok,_) when FanToken.match_keyword kwd tok ->
                       (njunk n strm; Action.mk tok)
                   | _ -> raise Stream.Failure in
                 (fun strm  ->
                    let bp = get_cur_loc strm in
                    let (__strm :_ Stream.t)= strm in
                    let a = ps __strm in
                    let act =
                      try p1 bp a __strm
                      with | Stream.Failure  -> raise (Stream.Error "") in
                    Action.getf act a)
             | _ ->
                 let ps strm =
                   match stream_peek_nth n strm with
                   | Some (tok,_) when FanToken.match_keyword kwd tok -> tok
                   | _ -> raise Stream.Failure in
                 let p1 = loop (n + 1) tokl in
                 (fun (__strm : _ Stream.t)  ->
                    let tok = ps __strm in
                    let s = __strm in let act = p1 s in Action.getf act tok))
        | _ -> invalid_arg "parser_of_token_list" in
      loop 1 tokl
and parser_of_symbol entry nlevn =
      function
      | `Smeta (_,symbl,act) ->
          let act = Obj.magic act entry symbl in
          let pl = List.map (parser_of_symbol entry nlevn) symbl in
          Obj.magic (List.fold_left (fun act  p  -> Obj.magic act p) act pl)
      | `Slist0 s ->
          let ps = parser_of_symbol entry nlevn s in
          let rec loop al (__strm : _ Stream.t) =
            match try Some (ps __strm) with | Stream.Failure  -> None with
            | Some a -> loop (a :: al) __strm
            | _ -> al in
          (fun (__strm : _ Stream.t)  ->
             let a = loop [] __strm in Action.mk (List.rev a))
      | `Slist0sep (symb,sep) ->
          let ps = parser_of_symbol entry nlevn symb in
          let pt = parser_of_symbol entry nlevn sep in
          let rec kont al (__strm : _ Stream.t) =
            match try Some (pt __strm) with | Stream.Failure  -> None with
            | Some v ->
                let a =
                  try ps __strm
                  with
                  | Stream.Failure  ->
                      raise
                        (Stream.Error (Failed.symb_failed entry v sep symb)) in
                kont (a :: al) __strm
            | _ -> al in
          (fun (__strm : _ Stream.t)  ->
             match try Some (ps __strm) with | Stream.Failure  -> None with
             | Some a -> let s = __strm in Action.mk (List.rev (kont [a] s))
             | _ -> Action.mk [])
      | `Slist1 s ->
          let ps = parser_of_symbol entry nlevn s in
          let rec loop al (__strm : _ Stream.t) =
            match try Some (ps __strm) with | Stream.Failure  -> None with
            | Some a -> loop (a :: al) __strm
            | _ -> al in
          (fun (__strm : _ Stream.t)  ->
             let a = ps __strm in
             let s = __strm in Action.mk (List.rev (loop [a] s)))
      | `Slist1sep (symb,sep) ->
          let ps = parser_of_symbol entry nlevn symb in
          let pt = parser_of_symbol entry nlevn sep in
          let rec kont al (__strm : _ Stream.t) =
            match try Some (pt __strm) with | Stream.Failure  -> None with
            | Some v ->
                let a =
                  try ps __strm
                  with
                  | Stream.Failure  ->
                      (try parse_top_symb entry symb __strm
                       with
                       | Stream.Failure  ->
                           raise
                             (Stream.Error
                                (Failed.symb_failed entry v sep symb))) in
                kont (a :: al) __strm
            | _ -> al in
          (fun (__strm : _ Stream.t)  ->
             let a = ps __strm in
             let s = __strm in Action.mk (List.rev (kont [a] s)))
      | `Sopt s ->
          let ps = parser_of_symbol entry nlevn s in
          (fun (__strm : _ Stream.t)  ->
             match try Some (ps __strm) with | Stream.Failure  -> None with
             | Some a -> Action.mk (Some a)
             | _ -> Action.mk None)
      | `Stry s -> let ps = parser_of_symbol entry nlevn s in try_parser ps
      | `Stree t ->
          let pt = parser_of_tree entry 1 0 t in
          (fun strm  ->
             let bp = get_cur_loc strm in
             let (__strm :_ Stream.t)= strm in
             let (act,loc) = add_loc bp pt __strm in Action.getf act loc)
      | `Snterm e -> (fun (__strm : _ Stream.t)  -> e.estart 0 __strm)
      | `Snterml (e,l) ->
          (fun (__strm : _ Stream.t)  -> e.estart (level_number e l) __strm)
      | `Sself -> (fun (__strm : _ Stream.t)  -> entry.estart 0 __strm)
      | `Snext -> (fun (__strm : _ Stream.t)  -> entry.estart nlevn __strm)
      | `Skeyword kwd ->
          (fun (__strm : _ Stream.t)  ->
             match Stream.peek __strm with
             | Some (tok,_) when FanToken.match_keyword kwd tok ->
                 (Stream.junk __strm; Action.mk tok)
             | _ -> raise Stream.Failure)
      | `Stoken (f,_) ->
          (fun (__strm : _ Stream.t)  ->
             match Stream.peek __strm with
             | Some (tok,_) when f tok -> (Stream.junk __strm; Action.mk tok)
             | _ -> raise Stream.Failure)
and parse_top_symb entry symb strm =
      parser_of_symbol entry 0 (top_symb entry symb) strm
let rec start_parser_of_levels entry clevn =
  function
  | [] -> (fun _  (__strm : _ Stream.t)  -> raise Stream.Failure)
  | lev::levs ->
      let p1 = start_parser_of_levels entry (succ clevn) levs in
      (match lev.lprefix with
       | DeadEnd  -> p1
       | tree ->
           let alevn =
             match lev.assoc with | `LA|`NA -> succ clevn | `RA -> clevn in
           let p2 = parser_of_tree entry (succ clevn) alevn tree in
           (match levs with
            | [] ->
                (fun levn  strm  ->
                   let bp = get_cur_loc strm in
                   let (__strm :_ Stream.t)= strm in
                   let (act,loc) = add_loc bp p2 __strm in
                   let strm = __strm in
                   let a = Action.getf act loc in
                   entry.econtinue levn loc a strm)
            | _ ->
                (fun levn  strm  ->
                   if levn > clevn
                   then p1 levn strm
                   else
                     (let bp = get_cur_loc strm in
                      let (__strm :_ Stream.t)= strm in
                      match try Some (add_loc bp p2 __strm)
                            with | Stream.Failure  -> None
                      with
                      | Some (act,loc) ->
                          let a = Action.getf act loc in
                          entry.econtinue levn loc a strm
                      | _ -> p1 levn __strm))))
let start_parser_of_entry entry =
  match entry.edesc with
  | Dlevels [] -> Tools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry 0 elev
  | Dparser p -> (fun _  -> p)
let rec continue_parser_of_levels entry clevn =
  function
  | [] -> (fun _  _  _  (__strm : _ Stream.t)  -> raise Stream.Failure)
  | lev::levs ->
      let p1 = continue_parser_of_levels entry (succ clevn) levs in
      (match lev.lsuffix with
       | DeadEnd  -> p1
       | tree ->
           let alevn =
             match lev.assoc with | `LA|`NA -> succ clevn | `RA -> clevn in
           let p2 = parser_of_tree entry (succ clevn) alevn tree in
           (fun levn  bp  a  strm  ->
              if levn > clevn
              then p1 levn bp a strm
              else
                (let (__strm :_ Stream.t)= strm in
                 try p1 levn bp a __strm
                 with
                 | Stream.Failure  ->
                     let (act,loc) = add_loc bp p2 __strm in
                     let a = Action.getf2 act a loc in
                     entry.econtinue levn loc a strm)))
let continue_parser_of_entry entry =
  match entry.edesc with
  | Dlevels elev ->
      let p = continue_parser_of_levels entry 0 elev in
      (fun levn  bp  a  (__strm : _ Stream.t)  ->
         try p levn bp a __strm with | Stream.Failure  -> a)
  | Dparser _ ->
      (fun _  _  _  (__strm : _ Stream.t)  -> raise Stream.Failure)