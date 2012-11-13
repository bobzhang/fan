open Structure
open Format
let is_before s1 s2 =
  match (s1, s2) with
  | ((`Skeyword _|`Stoken _),(`Skeyword _|`Stoken _)) -> false
  | ((`Skeyword _|`Stoken _),_) -> true
  | _ -> false
let rec derive_eps: symbol -> bool =
  function
  | `Slist0 _|`Slist0sep (_,_)|`Sopt _ -> true
  | `Stry s -> derive_eps s
  | `Stree t -> tree_derive_eps t
  | `Slist1 _|`Slist1sep (_,_)|`Stoken _|`Skeyword _ -> false
  | `Smeta (_,_,_)|`Snterm _|`Snterml (_,_)|`Snext|`Sself -> false
and tree_derive_eps: tree -> bool =
  function
  | LocAct (_,_) -> true
  | Node { node = s; brother = bro; son } ->
      ((derive_eps s) && (tree_derive_eps son)) || (tree_derive_eps bro)
  | DeadEnd  -> false
let empty_lev lname assoc =
  let assoc = match assoc with | Some a -> a | None  -> `LA in
  { assoc; lname; lsuffix = DeadEnd; lprefix = DeadEnd }
let change_lev entry lev name lname assoc =
  let a =
    match assoc with
    | None  -> lev.assoc
    | Some a ->
        (if (a <> lev.assoc) && ((entry.egram).warning_verbose).contents
         then eprintf "<W> Changing associativity of level %S @." name
         else ();
         a) in
  (match lname with
   | Some n ->
       if (lname <> lev.lname) && ((entry.egram).warning_verbose).contents
       then eprintf "<W> Level label %S ignored@." n
       else ()
   | None  -> ());
  { lev with assoc = a }
let change_to_self entry =
  function | `Snterm e when e == entry -> `Sself | x -> x
let levels_of_entry e =
  match e.edesc with | Dlevels ls -> Some ls | _ -> None
let find_level ?position  entry levs =
  let find x n ls =
    let rec get =
      function
      | [] ->
          (eprintf "No level labelled %S in entry %S @." n entry.ename;
           failwith "find_level")
      | lev::levs ->
          if Tools.is_level_labelled n lev
          then
            (match x with
             | `Level _ -> ([], (change_lev entry lev n), levs)
             | `Before _ -> ([], empty_lev, (lev :: levs))
             | `After _ -> ([lev], empty_lev, levs))
          else
            (let (levs1,rlev,levs2) = get levs in
             ((lev :: levs1), rlev, levs2)) in
    get ls in
  match position with
  | Some `First -> ([], empty_lev, levs)
  | Some `Last -> (levs, empty_lev, [])
  | Some (`Level n|`Before n|`After n as x) -> find x n levs
  | None  ->
      (match levs with
       | lev::levs -> ([], (change_lev entry lev "<top>"), levs)
       | [] -> ([], empty_lev, []))
let rec check_gram entry =
  function
  | `Snterm e ->
      if e.egram != entry.egram
      then
        (eprintf
           "Error: entries %S and %S do not belong to the same grammar.@."
           entry.ename e.ename;
         failwith "Grammar.extend error")
      else ()
  | `Snterml (e,_) ->
      if e.egram != entry.egram
      then
        (eprintf
           "Error: entries %S and %S do not belong to the same grammar.@."
           entry.ename e.ename;
         failwith "Grammar.extend error")
      else ()
  | `Smeta (_,sl,_) -> List.iter (check_gram entry) sl
  | `Slist0sep (s,t) -> (check_gram entry t; check_gram entry s)
  | `Slist1sep (s,t) -> (check_gram entry t; check_gram entry s)
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s -> check_gram entry s
  | `Stree t -> tree_check_gram entry t
  | `Snext|`Sself|`Stoken _|`Skeyword _ -> ()
and tree_check_gram entry =
  function
  | Node { node; brother; son } ->
      (check_gram entry node;
       tree_check_gram entry brother;
       tree_check_gram entry son)
  | LocAct _|DeadEnd  -> ()
let get_initial =
  function | `Sself::symbols -> (true, symbols) | symbols -> (false, symbols)
let insert_tokens gram symbols =
  let rec insert =
    function
    | `Smeta (_,sl,_) -> List.iter insert sl
    | `Slist0 s|`Slist1 s|`Sopt s|`Stry s -> insert s
    | `Slist0sep (s,t) -> (insert s; insert t)
    | `Slist1sep (s,t) -> (insert s; insert t)
    | `Stree t -> tinsert t
    | `Skeyword kwd -> using gram kwd
    | `Snterm _|`Snterml (_,_)|`Snext|`Sself|`Stoken _ -> ()
  and tinsert =
    function
    | Node { node = s; brother = bro; son } ->
        (insert s; tinsert bro; tinsert son)
    | LocAct (_,_)|DeadEnd  -> () in
  List.iter insert symbols
let insert_production_in_tree entry (gsymbols,action) tree =
  let rec try_insert s sl tree =
    match tree with
    | Node ({ node; son; brother } as x) ->
        if Tools.eq_symbol s node
        then Some (Node { x with son = (insert sl son) })
        else
          (match try_insert s sl brother with
           | Some y -> Some (Node { x with brother = y })
           | None  ->
               if
                 (is_before node s) ||
                   ((derive_eps s) && (not (derive_eps node)))
               then
                 Some
                   (Node
                      {
                        x with
                        brother =
                          (Node
                             { x with node = s; son = (insert sl DeadEnd) })
                      })
               else None)
    | LocAct (_,_)|DeadEnd  -> None
  and insert_in_tree s sl tree =
    match try_insert s sl tree with
    | Some t -> t
    | None  -> Node { node = s; son = (insert sl DeadEnd); brother = tree }
  and insert symbols tree =
    match symbols with
    | s::sl -> insert_in_tree s sl tree
    | [] ->
        (match tree with
         | Node ({ brother;_} as x) ->
             Node { x with brother = (insert [] brother) }
         | LocAct (old_action,action_list) ->
             let () =
               if ((entry.egram).warning_verbose).contents
               then
                 eprintf
                   "<W> Grammar extension: in [%s] some rule has been masked@."
                   entry.ename
               else () in
             LocAct (action, (old_action :: action_list))
         | DeadEnd  -> LocAct (action, [])) in
  insert gsymbols tree
let insert_production_in_level entry e1 (symbols,action) slev =
  if e1
  then
    {
      slev with
      lsuffix =
        (insert_production_in_tree entry (symbols, action) slev.lsuffix)
    }
  else
    {
      slev with
      lprefix =
        (insert_production_in_tree entry (symbols, action) slev.lprefix)
    }
let insert_olevels_in_levels entry position rules =
  let elev =
    match entry.edesc with
    | Dlevels elev -> elev
    | Dparser _ ->
        (eprintf "Error: entry not extensible: %S@." entry.ename;
         failwith "Grammar.extend") in
  if rules = []
  then elev
  else
    (let (levs1,make_lev,levs2) = find_level ?position entry elev in
     let (levs,_) =
       List.fold_left
         (fun (levs,make_lev)  (lname,assoc,rules)  ->
            let lev = make_lev lname assoc in
            let lev =
              List.fold_left
                (fun lev  (symbols,action)  ->
                   let symbols = List.map (change_to_self entry) symbols in
                   let () = List.iter (check_gram entry) symbols in
                   let (e1,symbols) = get_initial symbols in
                   let () = insert_tokens entry.egram symbols in
                   insert_production_in_level entry e1 (symbols, action) lev)
                lev rules in
            ((lev :: levs), empty_lev)) ([], make_lev) rules in
     levs1 @ ((List.rev levs) @ levs2))
let extend entry (position,rules) =
  let elev = insert_olevels_in_levels entry position rules in
  entry.edesc <- Dlevels elev;
  entry.estart <-
    (fun lev  strm  ->
       let f = Parser.start_parser_of_entry entry in
       entry.estart <- f; f lev strm);
  entry.econtinue <-
    (fun lev  bp  a  strm  ->
       let f = Parser.continue_parser_of_entry entry in
       entry.econtinue <- f; f lev bp a strm)