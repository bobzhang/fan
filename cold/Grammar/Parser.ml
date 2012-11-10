open Structure
open LibUtil
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
let rec top_symb entry =
  function
  | `Sself|`Snext -> `Snterm entry
  | `Snterml (e,_) -> `Snterm e
  | `Slist1sep (s,sep) -> `Slist1sep ((top_symb entry s), sep)
  | _ -> raise Stream.Failure
let top_tree entry =
  function
  | Node ({ node = s;_} as x) -> Node { x with node = (top_symb entry s) }
  | LocAct (_,_)|DeadEnd  -> raise Stream.Failure
let entry_of_symb entry =
  function
  | `Sself|`Snext -> entry
  | `Snterm e -> e
  | `Snterml (e,_) -> e
  | _ -> raise Stream.Failure
let rec parser_of_tree entry (lev,assoc) x =
          let alevn = match assoc with | `LA|`NA -> lev + 1 | `RA -> lev in
          let rec from_tree =
            function
            | DeadEnd  -> raise Stream.Failure
            | LocAct (act,_) -> (fun (__strm : _ Stream.t)  -> act)
            | Node { node = `Sself; son = LocAct (act,_); brother = bro } ->
                (fun (__strm : _ Stream.t)  ->
                   match try Some (entry.estart alevn __strm)
                         with | Stream.Failure  -> None
                   with
                   | Some a -> Action.getf act a
                   | _ -> from_tree bro __strm)
            | Node ({ node; son; brother } as y) ->
                let parser_cont (node,son) loc a =
                  let pson = from_tree son in
                  let recover loc a strm =
                    if FanConfig.strict_parsing.contents
                    then
                      raise
                        (Stream.Error (Failed.tree_failed entry a node son))
                    else
                      (let _ =
                         if FanConfig.strict_parsing_warning.contents
                         then
                           let msg = Failed.tree_failed entry a node son in
                           (Format.eprintf
                              "Warning: trying to recover from syntax error";
                            if entry.ename <> ""
                            then Format.eprintf " in [%s]" entry.ename
                            else ();
                            Format.eprintf "\n%s%a@." msg FanLoc.print loc)
                         else () in
                       let continue loc a (__strm : _ Stream.t) =
                         let a =
                           (entry_of_symb entry node).econtinue 0 loc a
                             __strm in
                         let act =
                           try pson __strm
                           with
                           | Stream.Failure  ->
                               raise
                                 (Stream.Error
                                    (Failed.tree_failed entry a node son)) in
                         Action.mk (fun _  -> Action.getf act a) in
                       let skip_if_empty bp strm =
                         if (get_cur_loc strm) = bp
                         then Action.mk (fun _  -> raise Stream.Failure)
                         else raise Stream.Failure in
                       let do_recover loc a (__strm : _ Stream.t) =
                         try from_tree (top_tree entry son) __strm
                         with
                         | Stream.Failure  ->
                             (try skip_if_empty loc __strm
                              with | Stream.Failure  -> continue loc a __strm) in
                       do_recover loc a strm) in
                  fun (__strm : _ Stream.t)  ->
                    try pson __strm
                    with
                    | Stream.Failure  ->
                        (try recover loc a __strm
                         with
                         | Stream.Failure  ->
                             raise
                               (Stream.Error
                                  (Failed.tree_failed entry a node son))) in
                (match Tools.get_terminals y with
                 | None  ->
                     let ps = parser_of_symbol entry node lev in
                     (fun strm  ->
                        let bp = get_cur_loc strm in
                        let (__strm :_ Stream.t)= strm in
                        match try Some (ps __strm)
                              with | Stream.Failure  -> None
                        with
                        | Some a ->
                            let act =
                              try parser_cont (node, son) bp a __strm
                              with
                              | Stream.Failure  -> raise (Stream.Error "") in
                            Action.getf act a
                        | _ -> from_tree brother __strm)
                 | Some (tokl,node,son) ->
                     let p1 =
                       parser_of_terminals tokl (parser_cont (node, son)) in
                     (fun (__strm : _ Stream.t)  ->
                        try p1 __strm
                        with | Stream.Failure  -> from_tree brother __strm)) in
          from_tree x
and parser_of_terminals (terminals : terminal list)
      (cont : Action.t cont_parse) (strm : token_stream) =
      let bp = Tools.get_cur_loc strm in
      let rec p ?(first= true)  (acc : FanSig.token list)
        (ts : terminal list) =
        match ts with
        | [] -> acc
        | x::xs ->
            let (__strm :_ Stream.t)= strm in
            (match Stream.peek __strm with
             | Some (t,_) when
                 match x with
                 | `Stoken (f,_) -> f t
                 | `Skeyword kwd -> FanToken.match_keyword kwd t ->
                 (Stream.junk __strm; p ~first:false (t :: acc) xs)
             | _ ->
                 if first
                 then raise Stream.Failure
                 else raise (Stream.Error "")) in
      let (ts :FanSig.token list)= p [] terminals in
      match ts with
      | [] -> invalid_arg "parser_of_terminals"
      | x::_ ->
          let action = Obj.magic cont bp (Action.mk x) strm in
          List.fold_left (fun a  arg  -> Action.getf a arg) action ts
and parser_of_symbol entry s nlevn =
      let rec aux s =
        match s with
        | `Smeta (_,symbls,act) ->
            let act = Obj.magic act entry symbls and pl = List.map aux symbls in
            Obj.magic
              (List.fold_left (fun act  p  -> Obj.magic act p) act pl)
        | `Slist0 s ->
            let ps = aux s in
            Comb.slist0 ps ~f:(fun l  -> Action.mk (List.rev l))
        | `Slist0sep (symb,sep) ->
            let ps = aux symb and pt = aux sep in
            Comb.slist0sep ps pt
              ~err:(fun v  -> Failed.symb_failed entry v sep symb)
              ~f:(fun l  -> Action.mk (List.rev l))
        | `Slist1 s ->
            let ps = aux s in
            Comb.slist1 ps ~f:(fun l  -> Action.mk (List.rev l))
        | `Slist1sep (symb,sep) ->
            let ps = aux symb and pt = aux sep in
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
        | `Sopt s -> let ps = aux s in Comb.opt ps ~f:Action.mk
        | `Stry s -> let ps = aux s in Comb.tryp ps
        | `Stree t ->
            let pt = parser_of_tree entry (0, `RA) t in
            (fun strm  ->
               let bp = get_cur_loc strm in
               let (__strm :_ Stream.t)= strm in
               let (act,loc) = add_loc bp pt __strm in Action.getf act loc)
        | `Snterm e -> (fun (__strm : _ Stream.t)  -> e.estart 0 __strm)
        | `Snterml (e,l) ->
            (fun (__strm : _ Stream.t)  -> e.estart (level_number e l) __strm)
        | `Sself -> (fun (__strm : _ Stream.t)  -> entry.estart 0 __strm)
        | `Snext ->
            (fun (__strm : _ Stream.t)  -> entry.estart (nlevn + 1) __strm)
        | `Skeyword kwd ->
            (fun (__strm : _ Stream.t)  ->
               match Stream.peek __strm with
               | Some (tok,_) when FanToken.match_keyword kwd tok ->
                   (Stream.junk __strm; Action.mk tok)
               | _ -> raise Stream.Failure)
        | `Stoken (f,_) ->
            (fun (__strm : _ Stream.t)  ->
               match Stream.peek __strm with
               | Some (tok,_) when f tok ->
                   (Stream.junk __strm; Action.mk tok)
               | _ -> raise Stream.Failure) in
      aux s
and parse_top_symb entry symb =
      parser_of_symbol entry (top_symb entry symb) 0
let start_parser_of_levels entry =
  let rec aux clevn =
    (function
     | [] -> (fun _  (__strm : _ Stream.t)  -> raise Stream.Failure)
     | lev::levs ->
         let hstart = aux (clevn + 1) levs in
         (match lev.lprefix with
          | DeadEnd  -> hstart
          | tree ->
              let cstart = parser_of_tree entry (clevn, (lev.assoc)) tree in
              (fun levn  strm  ->
                 if (levn > clevn) && (not ([] = levs))
                 then hstart levn strm
                 else
                   (let bp = get_cur_loc strm in
                    let (__strm :_ Stream.t)= strm in
                    match try Some (add_loc bp cstart __strm)
                          with | Stream.Failure  -> None
                    with
                    | Some (act,loc) ->
                        let a = Action.getf act loc in
                        entry.econtinue levn loc a strm
                    | _ -> hstart levn __strm))) : level list ->
                                                     int -> Action.t parse ) in
  aux 0
let start_parser_of_entry entry =
  match entry.edesc with
  | Dlevels [] -> Tools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry elev
  | Dparser p -> (fun _  -> p)
let rec continue_parser_of_levels entry clevn =
  function
  | [] -> (fun _  _  _  (__strm : _ Stream.t)  -> raise Stream.Failure)
  | lev::levs ->
      let hcontinue = continue_parser_of_levels entry (clevn + 1) levs in
      (match lev.lsuffix with
       | DeadEnd  -> hcontinue
       | tree ->
           let ccontinue = parser_of_tree entry (clevn, (lev.assoc)) tree in
           (fun levn  bp  a  strm  ->
              if levn > clevn
              then hcontinue levn bp a strm
              else
                (let (__strm :_ Stream.t)= strm in
                 try hcontinue levn bp a __strm
                 with
                 | Stream.Failure  ->
                     let (act,loc) = add_loc bp ccontinue __strm in
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