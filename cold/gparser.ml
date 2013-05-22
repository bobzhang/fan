open Gstructure

open LibUtil

open FanToken

let with_loc (parse_fun : 'b parse) strm =
  let bp = Gtools.get_cur_loc strm in
  let x = parse_fun strm in
  let ep = Gtools.get_prev_loc strm in
  let loc =
    let start_off_bp = FanLoc.start_off bp in
    let stop_off_ep = FanLoc.stop_off ep in
    if start_off_bp > stop_off_ep then FanLoc.join bp else FanLoc.merge bp ep in
  (x, loc)

let level_number entry lab =
  let rec lookup levn =
    function
    | [] -> failwithf "unknown level %s" lab
    | lev::levs ->
        if Gtools.is_level_labelled lab lev
        then levn
        else lookup (1 + levn) levs in
  match entry.edesc with
  | Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found

module ArgContainer = Stack

let rec parser_of_tree entry (lev,assoc)
  (q : (Gaction.t * FanLoc.t) ArgContainer.t) x =
  let alevn = match assoc with | `LA|`NA -> lev + 1 | `RA -> lev in
  let rec from_tree tree =
    match tree with
    | DeadEnd  -> raise XStream.Failure
    | LocAct (act,_) -> (fun _  -> act)
    | Node { node = `Sself; son = LocAct (act,_); brother = bro } ->
        (fun strm  ->
           (try
              let a = with_loc (entry.estart alevn) strm in
              fun ()  -> begin ArgContainer.push a q; act end
            with | XStream.Failure  -> (fun ()  -> from_tree bro strm)) ())
    | Node ({ node; son; brother } as y) ->
        (match Gtools.get_terminals y with
         | None  ->
             let ps = parser_of_symbol entry node lev in
             (fun strm  ->
                let bp = Gtools.get_cur_loc strm in
                (try
                   let a = ps strm in
                   fun ()  ->
                     begin
                       ArgContainer.push a q;
                       (let pson = from_tree son in
                        try pson strm
                        with
                        | e ->
                            begin
                              ignore (ArgContainer.pop q);
                              (match e with
                               | XStream.Failure  ->
                                   if (Gtools.get_cur_loc strm) = bp
                                   then raise XStream.Failure
                                   else
                                     raise
                                       (XStream.Error
                                          (Gfailed.tree_failed entry a node
                                             son))
                               | _ -> raise e)
                            end)
                     end
                 with
                 | XStream.Failure  -> (fun ()  -> from_tree brother strm))
                  ())
         | Some (tokl,_node,son) ->
             (fun strm  ->
                (try
                   let args = List.rev (parser_of_terminals tokl strm) in
                   fun ()  ->
                     begin
                       List.iter (fun a  -> ArgContainer.push a q) args;
                       (let len = List.length args in
                        let p = from_tree son in
                        try p strm
                        with
                        | e ->
                            begin
                              for _i = 1 to len do
                                ignore (ArgContainer.pop q)
                              done;
                              (match e with
                               | XStream.Failure  -> raise (XStream.Error "")
                               | _ -> raise e)
                            end)
                     end
                 with
                 | XStream.Failure  -> (fun ()  -> from_tree brother strm))
                  ())) in
  let parse = from_tree x in
  fun strm  ->
    let ((arity,_symbols,_,parse),loc) = with_loc parse strm in
    let ans = ref parse in
    begin
      for _i = 1 to arity do
        (let (v,_) = ArgContainer.pop q in
         ans := (Gaction.getf ans.contents v))
      done; ((ans.contents), loc)
    end
and parser_of_terminals (terminals : terminal list) strm =
  let n = List.length terminals in
  let acc = ref [] in
  begin
    (try
       List.iteri
         (fun i  terminal  ->
            let (t,loc) =
              match XStream.peek_nth strm i with
              | Some (t,loc) -> (t, loc)
              | None  -> invalid_arg "parser_of_terminals" in
            begin
              acc := (((Gaction.mk t), loc) :: (acc.contents));
              if
                not
                  (match terminal with
                   | `Stoken (f,_) -> f t
                   | `Skeyword kwd -> FanToken.match_keyword kwd t)
              then invalid_arg "parser_of_terminals"
            end) terminals
     with | Invalid_argument _ -> raise XStream.Failure);
    XStream.njunk n strm; acc.contents
  end
and parser_of_symbol entry s nlevn =
  let rec aux s =
    match s with
    | `Smeta (_,symbls,act) ->
        let act = Obj.magic act entry symbls and pl = List.map aux symbls in
        Obj.magic (List.fold_left (fun act  p  -> Obj.magic act p) act pl)
    | `Slist0 s ->
        let ps = aux s in
        Gcomb.slist0 ps ~f:(fun l  -> Gaction.mk (List.rev l))
    | `Slist0sep (symb,sep) ->
        let ps = aux symb and pt = aux sep in
        Gcomb.slist0sep ps pt
          ~err:(fun v  -> Gfailed.symb_failed entry v sep symb)
          ~f:(fun l  -> Gaction.mk (List.rev l))
    | `Slist1 s ->
        let ps = aux s in
        Gcomb.slist1 ps ~f:(fun l  -> Gaction.mk (List.rev l))
    | `Slist1sep (symb,sep) ->
        let ps = aux symb and pt = aux sep in
        Gcomb.slist1sep ps pt
          ~err:(fun v  -> Gfailed.symb_failed entry v sep symb)
          ~f:(fun l  -> Gaction.mk (List.rev l))
    | `Sopt s -> let ps = aux s in Gcomb.opt ps ~f:Gaction.mk
    | `Stry s -> let ps = aux s in Gcomb.tryp ps
    | `Speek s -> let ps = aux s in Gcomb.peek ps
    | `Stree t ->
        let pt = parser_of_tree entry (0, `RA) (ArgContainer.create ()) t in
        (fun strm  -> let (act,loc) = pt strm in Gaction.getf act loc)
    | `Snterml (e,l) -> (fun strm  -> e.estart (level_number e l) strm)
    | `Snterm e -> (fun strm  -> e.estart 0 strm)
    | `Sself -> (fun strm  -> entry.estart 0 strm)
    | `Snext -> (fun strm  -> entry.estart (nlevn + 1) strm)
    | `Skeyword kwd ->
        (fun strm  ->
           match XStream.peek strm with
           | Some (tok,_) when FanToken.match_keyword kwd tok ->
               begin XStream.junk strm; Gaction.mk tok end
           | _ -> raise XStream.Failure)
    | `Stoken (f,_) ->
        (fun strm  ->
           match XStream.peek strm with
           | Some (tok,_) when f tok ->
               begin XStream.junk strm; Gaction.mk tok end
           | _ -> raise XStream.Failure) in
  with_loc (aux s)

let start_parser_of_levels entry =
  let rec aux clevn (xs : level list) =
    (match xs with
     | [] -> (fun _  _  -> raise XStream.Failure)
     | lev::levs ->
         let hstart = aux (clevn + 1) levs in
         (match lev.lprefix with
          | DeadEnd  -> hstart
          | tree ->
              let cstart =
                parser_of_tree entry (clevn, (lev.assoc))
                  (ArgContainer.create ()) tree in
              (fun levn  strm  ->
                 if (levn > clevn) && (not ([] = levs))
                 then hstart levn strm
                 else
                   ((try
                       let (act,loc) = cstart strm in
                       fun ()  ->
                         let a = Gaction.getf act loc in
                         entry.econtinue levn loc a strm
                     with | XStream.Failure  -> (fun ()  -> hstart levn strm)))
                     ())) : int -> Gaction.t parse ) in
  aux 0

let start_parser_of_entry entry =
  match entry.edesc with
  | Dlevels [] -> Gtools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry elev
  | Dparser p -> (fun _  -> p)

let rec continue_parser_of_levels entry clevn =
  function
  | [] -> (fun _  _  _  _  -> raise XStream.Failure)
  | lev::levs ->
      let hcontinue = continue_parser_of_levels entry (clevn + 1) levs in
      (match lev.lsuffix with
       | DeadEnd  -> hcontinue
       | tree ->
           let ccontinue =
             parser_of_tree entry (clevn, (lev.assoc))
               (ArgContainer.create ()) tree in
           (fun levn  bp  a  strm  ->
              if levn > clevn
              then hcontinue levn bp a strm
              else
                (try hcontinue levn bp a strm
                 with
                 | XStream.Failure  ->
                     let (act,loc) = ccontinue strm in
                     let loc = FanLoc.merge bp loc in
                     let a = Gaction.getf2 act a loc in
                     entry.econtinue levn loc a strm)))

let continue_parser_of_entry entry =
  match entry.edesc with
  | Dlevels elev ->
      let p = continue_parser_of_levels entry 0 elev in
      (fun levn  bp  a  strm  ->
         try p levn bp a strm with | XStream.Failure  -> a)
  | Dparser _ -> (fun _  _  _  _  -> raise XStream.Failure)