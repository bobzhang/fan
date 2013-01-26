open Structure
open LibUtil
open FanToken
let add_loc bp (parse_fun : 'b parse) strm =
  let x = parse_fun strm in
  let ep = Tools.get_prev_loc strm in
  let loc =
    if (FanLoc.start_off bp) > (FanLoc.stop_off ep)
    then FanLoc.join bp
    else FanLoc.merge bp ep in
  (x, loc)
let level_number entry lab =
  let rec lookup levn =
    function
    | [] -> failwithf "unknown level %s" lab
    | lev::levs ->
        if Tools.is_level_labelled lab lev
        then levn
        else lookup (succ levn) levs in
  match entry.edesc with
  | Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found
let rec parser_of_tree entry (lev,assoc) x =
  let alevn = match assoc with | `LA|`NA -> lev + 1 | `RA -> lev in
  let rec from_tree tree =
    match tree with
    | DeadEnd  -> raise XStream.Failure
    | LocAct (act,_) -> (fun _  -> act)
    | Node { node = `Sself; son = LocAct (act,_); brother = bro } ->
        (fun (__strm : _ XStream.t)  ->
           match try Some (entry.estart alevn __strm)
                 with | XStream.Failure  -> None
           with
           | Some a -> Action.getf act a
           | _ -> from_tree bro __strm)
    | Node ({ node; son; brother } as y) ->
        (match Tools.get_terminals y with
         | None  ->
             let ps = parser_of_symbol entry node lev in
             (fun strm  ->
                let bp = Tools.get_cur_loc strm in
                (try
                   let a = ps strm in
                   fun ()  ->
                     let pson = from_tree son in
                     try Action.getf (pson strm) a
                     with
                     | XStream.Failure  ->
                         if (Tools.get_cur_loc strm) = bp
                         then raise XStream.Failure
                         else
                           raise
                             (XStream.Error
                                (Failed.tree_failed entry a node son))
                 with
                 | XStream.Failure  -> (fun ()  -> from_tree brother strm))
                  ())
         | Some (tokl,_node,son) ->
             (fun strm  ->
                try parser_of_terminals tokl (from_tree son) strm
                with | XStream.Failure  -> from_tree brother strm)) in
  from_tree x
and parser_of_terminals (terminals : terminal list) (cont : Action.t parse)
  strm =
  let n = List.length terminals in
  let acc = ref [] in
  (try
     List.iteri
       (fun i  terminal  ->
          let t =
            match XStream.peek_nth strm i with
            | Some (tok,_) -> tok
            | None  -> invalid_arg "parser_of_terminals" in
          acc := (t :: (acc.contents));
          if
            not
              (match terminal with
               | `Stoken (f,_) -> f t
               | `Skeyword kwd -> FanToken.match_keyword kwd t)
          then invalid_arg "parser_of_terminals") terminals
   with | Invalid_argument _ -> raise XStream.Failure);
  XStream.njunk n strm;
  (let action = cont strm in
   List.fold_left (fun a  arg  -> Action.getf a arg) action acc.contents)
and parser_of_symbol entry s nlevn =
  let rec aux s =
    match s with
    | `Smeta (_,symbls,act) ->
        let act = Obj.magic act entry symbls and pl = List.map aux symbls in
        Obj.magic (List.fold_left (fun act  p  -> Obj.magic act p) act pl)
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
        Comb.slist1sep ps pt
          ~err:(fun v  -> Failed.symb_failed entry v sep symb)
          ~f:(fun l  -> Action.mk (List.rev l))
    | `Sopt s -> let ps = aux s in Comb.opt ps ~f:Action.mk
    | `Stry s -> let ps = aux s in Comb.tryp ps
    | `Speek s -> let ps = aux s in Comb.peek ps
    | `Stree t ->
        let pt = parser_of_tree entry (0, `RA) t in
        (fun strm  ->
           let bp = Tools.get_cur_loc strm in
           let (act,loc) = add_loc bp pt strm in Action.getf act loc)
    | `Snterm e -> (fun (__strm : _ XStream.t)  -> e.estart 0 __strm)
    | `Snterml (e,l) -> (fun strm  -> e.estart (level_number e l) strm)
    | `Sself -> (fun strm  -> entry.estart 0 strm)
    | `Snext -> (fun strm  -> entry.estart (nlevn + 1) strm)
    | `Skeyword kwd ->
        (fun (__strm : _ XStream.t)  ->
           match XStream.peek __strm with
           | Some (tok,_) when FanToken.match_keyword kwd tok ->
               (XStream.junk __strm; Action.mk tok)
           | _ -> raise XStream.Failure)
    | `Stoken (f,_) ->
        (fun (__strm : _ XStream.t)  ->
           match XStream.peek __strm with
           | Some (tok,_) when f tok -> (XStream.junk __strm; Action.mk tok)
           | _ -> raise XStream.Failure) in
  aux s
let start_parser_of_levels entry =
  let rec aux clevn (xs : level list) =
    (match xs with
     | [] -> (fun _  (__strm : _ XStream.t)  -> raise XStream.Failure)
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
                   (let bp = Tools.get_cur_loc strm in
                    let (__strm :_ XStream.t)= strm in
                    match try Some (add_loc bp cstart __strm)
                          with | XStream.Failure  -> None
                    with
                    | Some (act,loc) ->
                        let a = Action.getf act loc in
                        entry.econtinue levn loc a strm
                    | _ -> hstart levn __strm))) : int -> Action.t parse ) in
  aux 0
let start_parser_of_entry entry =
  match entry.edesc with
  | Dlevels [] -> Tools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry elev
  | Dparser p -> (fun _  -> p)
let rec continue_parser_of_levels entry clevn =
  function
  | [] -> (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure)
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
                (let (__strm :_ XStream.t)= strm in
                 try hcontinue levn bp a __strm
                 with
                 | XStream.Failure  ->
                     let (act,loc) = add_loc bp ccontinue __strm in
                     let a = Action.getf2 act a loc in
                     entry.econtinue levn loc a strm)))
let continue_parser_of_entry entry =
  match entry.edesc with
  | Dlevels elev ->
      let p = continue_parser_of_levels entry 0 elev in
      (fun levn  bp  a  (__strm : _ XStream.t)  ->
         try p levn bp a __strm with | XStream.Failure  -> a)
  | Dparser _ ->
      (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure)