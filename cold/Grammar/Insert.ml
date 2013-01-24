open Structure
open Format
open LibUtil
let higher s1 s2 =
  match (s1, s2) with
  | (#terminal,#terminal) -> false
  | (#terminal,_) -> true
  | _ -> false
let rec derive_eps: symbol -> bool =
  function
  | `Slist0 _|`Slist0sep (_,_)|`Sopt _|`Speek _ -> true
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
  { assoc; lname; lsuffix = DeadEnd; lprefix = DeadEnd; productions = [] }
let change_lev lev name lname assoc =
  if (assoc <> lev.assoc) && FanConfig.gram_warning_verbose.contents
  then eprintf "<W> Changing associativity of level %S aborted@." name;
  if
    (lname <> "") &&
      ((lname <> lev.lname) && FanConfig.gram_warning_verbose.contents)
  then eprintf "<W> Level label (%S: %S) ignored@." lname lev.lname;
  lev
let change_to_self entry =
  function | `Snterm e when e == entry -> `Sself | x -> x
let levels_of_entry e =
  match e.edesc with | Dlevels ls -> Some ls | _ -> None
let find_level ?position  entry levs =
  let find x n ls =
    let rec get =
      function
      | [] ->
          failwithf "Insert.find_level: No level labelled %S in entry %S @."
            n entry.ename
      | lev::levs ->
          if Tools.is_level_labelled n lev
          then
            (match x with
             | `Level _ -> ([], (Some (lev, n)), levs)
             | `Before _ -> ([], None, (lev :: levs))
             | `After _ -> ([lev], None, levs))
          else
            (let (levs1,rlev,levs2) = get levs in
             ((lev :: levs1), rlev, levs2)) in
    get ls in
  match position with
  | Some `First -> ([], None, levs)
  | Some `Last -> (levs, None, [])
  | Some (`Level n|`Before n|`After n as x) -> find x n levs
  | None  ->
      (match levs with
       | lev::levs -> ([], (Some (lev, "<top>")), levs)
       | [] -> ([], None, []))
let rec check_gram entry =
  function
  | `Snterm e ->
      if e.egram != entry.egram
      then
        failwithf
          "Gram.extend: entries %S and %S do not belong to the same grammar.@."
          entry.ename e.ename
  | `Snterml (e,_) ->
      if e.egram != entry.egram
      then
        failwithf
          "Gram.extend Error: entries %S and %S do not belong to the same grammar.@."
          entry.ename e.ename
  | `Smeta (_,sl,_) -> List.iter (check_gram entry) sl
  | `Slist0sep (s,t) -> (check_gram entry t; check_gram entry s)
  | `Slist1sep (s,t) -> (check_gram entry t; check_gram entry s)
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s|`Speek s -> check_gram entry s
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
let rec using_symbols gram symbols = List.iter (using_symbol gram) symbols
and using_symbol gram symbol =
  match symbol with
  | `Smeta (_,sl,_) -> List.iter (using_symbol gram) sl
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s|`Speek s -> using_symbol gram s
  | `Slist0sep (s,t) -> (using_symbol gram s; using_symbol gram t)
  | `Slist1sep (s,t) -> (using_symbol gram s; using_symbol gram t)
  | `Stree t -> using_node gram t
  | `Skeyword kwd -> using gram kwd
  | `Snterm _|`Snterml (_,_)|`Snext|`Sself|`Stoken _ -> ()
and using_node gram node =
  match node with
  | Node { node = s; brother = bro; son } ->
      (using_symbol gram s; using_node gram bro; using_node gram son)
  | LocAct (_,_)|DeadEnd  -> ()
let add_production (gsymbols,action) tree =
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
                 (higher node s) ||
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
               if FanConfig.gram_warning_verbose.contents
               then
                 eprintf
                   "<W> Grammar extension: in @[%a@] some rule has been masked@."
                   Print.dump#rule symbols
               else () in
             LocAct (action, (old_action :: action_list))
         | DeadEnd  -> LocAct (action, [])) in
  insert gsymbols tree
let add_production_in_level e1 (symbols,action) slev =
  if e1
  then
    { slev with lsuffix = (add_production (symbols, action) slev.lsuffix) }
  else
    { slev with lprefix = (add_production (symbols, action) slev.lprefix) }
let merge_level (la : level) (lb : olevel) =
  let (_lname1,_assoc1,rules1) = lb in
  List.fold_right
    (fun (symbols,action)  lev  ->
       let (e1,symbols) = get_initial symbols in
       add_production_in_level e1 (symbols, action) lev) rules1 la
let level_of_olevel (lb : olevel) =
  let (lname1,assoc1,_) = lb in
  let la = empty_lev lname1 assoc1 in merge_level la lb
let insert_olevels_in_levels entry position olevels =
  let elev =
    match entry.edesc with
    | Dlevels elev -> elev
    | Dparser _ ->
        failwithf "Grammar.extend: Error: entry not extensible: %S@."
          entry.ename in
  match olevels with
  | [] -> elev
  | x::xs ->
      let (levs1,make_lev,levs2) = find_level ?position entry elev in
      (match make_lev with
       | Some (lev,_n) ->
           let l1 = merge_level lev x in
           levs1 @ (l1 :: ((List.map level_of_olevel xs) @ levs2))
       | None  -> levs1 @ ((List.map level_of_olevel olevels) @ levs2))
let insert_olevel entry position olevel =
  let elev =
    match entry.edesc with
    | Dlevels elev -> elev
    | Dparser _ ->
        failwithf "Grammar.extend: Error: entry not extensible: %S@."
          entry.ename in
  let (levs1,v,levs2) = find_level ?position entry elev in
  let l1 =
    match v with
    | Some (lev,_n) -> merge_level lev olevel
    | None  -> level_of_olevel olevel in
  levs1 @ (l1 :: levs2)
let rec scan_olevels entry (levels : olevel list) =
  List.map (scan_olevel entry) levels
and scan_olevel entry (x,y,prods) =
  (x, y, (List.map (scan_product entry) prods))
and scan_product entry (symbols,x) =
  ((List.map
      (fun symbol  ->
         using_symbol entry.egram symbol;
         check_gram entry symbol;
         change_to_self entry symbol) symbols), x)
let extend entry (position,levels) =
  let levels = scan_olevels entry levels in
  let elev = insert_olevels_in_levels entry position levels in
  entry.edesc <- Dlevels elev;
  entry.estart <- Parser.start_parser_of_entry entry;
  entry.econtinue <- Parser.continue_parser_of_entry entry
let extend_single entry (position,level) =
  let level = scan_olevel entry level in
  let elev = insert_olevel entry position level in
  entry.edesc <- Dlevels elev;
  entry.estart <- Parser.start_parser_of_entry entry;
  entry.econtinue <- Parser.continue_parser_of_entry entry