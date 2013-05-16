open Gstructure

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
          if Gtools.is_level_labelled n lev
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

let rec using_symbols gram symbols acc =
  List.fold_left (fun acc  symbol  -> using_symbol gram symbol acc) acc
    symbols
and using_symbol gram symbol acc =
  match symbol with
  | `Smeta (_,sl,_) -> using_symbols gram sl acc
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s|`Speek s -> using_symbol gram s acc
  | `Slist0sep (s,t) -> using_symbol gram t (using_symbol gram s acc)
  | `Slist1sep (s,t) -> using_symbol gram t (using_symbol gram s acc)
  | `Stree t -> using_node gram t acc
  | `Skeyword kwd -> kwd :: acc
  | `Snterm _|`Snterml _|`Snext|`Sself|`Stoken _ -> acc
and using_node gram node acc =
  match node with
  | Node { node = s; brother = bro; son } ->
      using_node gram son (using_node gram bro (using_symbol gram s acc))
  | LocAct (_,_)|DeadEnd  -> acc

let add_production ((gsymbols,(annot,action)) : production) tree =
  let anno_action = ((List.length gsymbols), gsymbols, annot, action) in
  let rec try_insert s sl tree =
    match tree with
    | Node ({ node; son; brother } as x) ->
        if Gtools.eq_symbol s node
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
             (if FanConfig.gram_warning_verbose.contents
              then
                eprintf
                  "<W> Grammar extension: in @[%a@] some rule has been masked@."
                  Gprint.dump#rule symbols;
              LocAct (anno_action, (old_action :: action_list)))
         | DeadEnd  -> LocAct (anno_action, [])) in
  insert gsymbols tree

let add_production_in_level e1 (symbols,action) slev =
  if e1
  then
    { slev with lsuffix = (add_production (symbols, action) slev.lsuffix) }
  else
    { slev with lprefix = (add_production (symbols, action) slev.lprefix) }

let merge_level (la : level) (lb : olevel) =
  let rules1 =
    match lb with
    | (y,Some assoc,x) ->
        (if not ((la.lname = y) && (la.assoc = assoc))
         then
           eprintf
             "<W> Grammar level merging: merge_level does not agree (%a:%a) (%a:%a)@."
             (StdLib.pp_print_option pp_print_string) la.lname
             (StdLib.pp_print_option pp_print_string) y Gprint.dump#assoc
             la.assoc Gprint.dump#assoc assoc;
         x)
    | ((Some _ as y),_,x) ->
        (if not (la.lname = y)
         then
           eprintf
             "<W> Grammar level merging: merge_level does not agree (%a:%a)@."
             (StdLib.pp_print_option pp_print_string) la.lname
             (StdLib.pp_print_option pp_print_string) y;
         x)
    | (None ,None ,x) -> x in
  List.fold_right
    (fun (symbols,action)  lev  ->
       let (e1,symbols) = get_initial symbols in
       add_production_in_level e1 (symbols, action) lev) rules1 la

let level_of_olevel (lb : olevel) =
  let (lname1,assoc1,_) = lb in
  let la = empty_lev lname1 (Option.default `LA assoc1) in merge_level la lb

let insert_olevels_in_levels entry position olevels =
  let elev =
    match entry.edesc with
    | Dlevels elev -> elev
    | Dparser _ ->
        failwithf "Grammar.extend: Error: entry not extensible: %S@."
          entry.ename in
  match olevels with
  | [] -> elev
  | _::_ ->
      let (levs1,make_lev,levs2) = find_level ?position entry elev in
      (match make_lev with
       | Some (_lev,_n) ->
           failwithf "Insert group levels in to a specific lev:%s"
             entry.ename
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
         let keywords = using_symbol entry.egram symbol [] in
         let diff =
           let open SSet in
             elements &
               (diff (of_list keywords) ((entry.egram).gkeywords).contents) in
         if diff <> []
         then
           failwithf "in grammar %s: keywords introduced: [ %s ] "
             (entry.egram).annot (List.reduce_left ( ^ ) diff);
         check_gram entry symbol;
         (match symbol with
          | `Snterm e when e == entry -> `Sself
          | _ -> symbol)) symbols), x)