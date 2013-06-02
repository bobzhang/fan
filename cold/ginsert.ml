open Gstructure

open Format

open LibUtil

let higher s1 s2 =
  match (s1, s2) with
  | (#terminal,#terminal) -> false
  | (#terminal,_) -> true
  | _ -> false

let rec derive_eps (s : symbol) =
  match s with
  | `Slist0 _|`Slist0sep (_,_)|`Sopt _|`Speek _ -> true
  | `Stry s -> derive_eps s
  | `Slist1 _|`Slist1sep (_,_)|`Stoken _|`Skeyword _ -> false
  | `Snterm _|`Snterml (_,_)|`Snext|`Sself -> false

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
          "Fgram.extend: entries %S and %S do not belong to the same grammar.@."
          entry.ename e.ename
  | `Snterml (e,_) ->
      if e.egram != entry.egram
      then
        failwithf
          "Fgram.extend Error: entries %S and %S do not belong to the same grammar.@."
          entry.ename e.ename
  | `Slist0sep (s,t) -> begin check_gram entry t; check_gram entry s end
  | `Slist1sep (s,t) -> begin check_gram entry t; check_gram entry s end
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s|`Speek s -> check_gram entry s
  | `Snext|`Sself|`Stoken _|`Skeyword _ -> ()
and tree_check_gram entry =
  function
  | Node { node; brother; son } ->
      begin
        check_gram entry node; tree_check_gram entry brother;
        tree_check_gram entry son
      end
  | LocAct _|DeadEnd  -> ()

let get_initial =
  function | `Sself::symbols -> (true, symbols) | symbols -> (false, symbols)

let rec using_symbols symbols acc =
  List.fold_left (fun acc  symbol  -> using_symbol symbol acc) acc symbols
and using_symbol symbol acc =
  match symbol with
  | `Slist0 s|`Slist1 s|`Sopt s|`Stry s|`Speek s -> using_symbol s acc
  | `Slist0sep (s,t) -> using_symbol t (using_symbol s acc)
  | `Slist1sep (s,t) -> using_symbol t (using_symbol s acc)
  | `Skeyword kwd -> kwd :: acc
  | `Snterm _|`Snterml _|`Snext|`Sself|`Stoken _ -> acc
and using_node node acc =
  match node with
  | Node { node = s; brother = bro; son } ->
      using_node son (using_node bro (using_symbol s acc))
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
             begin
               if FConfig.gram_warning_verbose.contents
               then
                 eprintf
                   "<W> Grammar extension: in @[%a@] some rule has been masked@."
                   Gprint.dump#rule symbols;
               LocAct (anno_action, (old_action :: action_list))
             end
         | DeadEnd  -> LocAct (anno_action, [])) in
  insert gsymbols tree

let add_production_in_level ((symbols,action) as prod) slev =
  let (suffix,symbols1) = get_initial symbols in
  if suffix
  then
    {
      slev with
      lsuffix = (add_production (symbols1, action) slev.lsuffix);
      productions = (prod :: (slev.productions))
    }
  else
    {
      slev with
      lprefix = (add_production (symbols1, action) slev.lprefix);
      productions = (prod :: (slev.productions))
    }

let merge_level (la : level) (lb : olevel) =
  let rules1 =
    match lb with
    | (y,Some assoc,x) ->
        begin
          if not ((la.lname = y) && (la.assoc = assoc))
          then
            eprintf
              "<W> Grammar level merging: merge_level does not agree (%a:%a) (%a:%a)@."
              (StdFan.pp_print_option pp_print_string) la.lname
              (StdFan.pp_print_option pp_print_string) y Gprint.pp_assoc
              la.assoc Gprint.pp_assoc assoc;
          x
        end
    | ((Some _ as y),_,x) ->
        begin
          if not (la.lname = y)
          then
            eprintf
              "<W> Grammar level merging: merge_level does not agree (%a:%a)@."
              (StdFan.pp_print_option pp_print_string) la.lname
              (StdFan.pp_print_option pp_print_string) y;
          x
        end
    | (None ,None ,x) -> x in
  List.fold_right add_production_in_level rules1 la

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
  (((List.map
       (fun symbol  ->
          let keywords = using_symbol symbol [] in
          let diff =
            let open SSet in
              elements &
                (diff (of_list keywords) ((entry.egram).gkeywords).contents) in
          let () =
            if diff <> []
            then
              failwithf "in grammar %s: keywords introduced: [ %s ] "
                (entry.egram).annot (List.reduce_left ( ^ ) diff) in
          let () = check_gram entry symbol in
          match symbol with
          | `Snterm e when e == entry -> `Sself
          | _ -> symbol) symbols), x) : production )

let rec unsafe_scan_olevels entry (levels : olevel list) =
  List.map (unsafe_scan_olevel entry) levels
and unsafe_scan_olevel entry (x,y,prods) =
  (x, y, (List.map (unsafe_scan_product entry) prods))
and unsafe_scan_product entry (symbols,x) =
  (((List.map
       (fun symbol  ->
          let keywords = using_symbol symbol [] in
          let () =
            (entry.egram).gkeywords :=
              (SSet.add_list ((entry.egram).gkeywords).contents keywords) in
          let () = check_gram entry symbol in
          match symbol with
          | `Snterm e when e == entry -> `Sself
          | _ -> symbol) symbols), x) : production )

let unsafe_extend entry (position,levels) =
  let levels = unsafe_scan_olevels entry levels in
  let elev = insert_olevels_in_levels entry position levels in
  begin
    entry.edesc <- Dlevels elev;
    entry.estart <- Gparser.start_parser_of_entry entry;
    entry.econtinue <- Gparser.continue_parser_of_entry entry
  end

let unsafe_extend_single entry (position,olevel) =
  let olevel = unsafe_scan_olevel entry olevel in
  let elev = insert_olevel entry position olevel in
  begin
    entry.edesc <- Dlevels elev;
    entry.estart <- Gparser.start_parser_of_entry entry;
    entry.econtinue <- Gparser.continue_parser_of_entry entry
  end

let extend entry (position,levels) =
  let levels = scan_olevels entry levels in
  let elev = insert_olevels_in_levels entry position levels in
  begin
    entry.edesc <- Dlevels elev;
    entry.estart <- Gparser.start_parser_of_entry entry;
    entry.econtinue <- Gparser.continue_parser_of_entry entry
  end

let extend_single entry (position,olevel) =
  let olevel = scan_olevel entry olevel in
  let elev = insert_olevel entry position olevel in
  begin
    entry.edesc <- Dlevels elev;
    entry.estart <- Gparser.start_parser_of_entry entry;
    entry.econtinue <- Gparser.continue_parser_of_entry entry
  end

let copy (e : entry) =
  (let result =
     {
       e with
       estart = (fun _  -> assert false);
       econtinue = (fun _  -> assert false)
     } in
   begin
     result.estart <- Gparser.start_parser_of_entry result;
     result.econtinue <- Gparser.continue_parser_of_entry result; result
   end : entry )

let refresh_level ~f  { assoc; lname; productions;_} =
  level_of_olevel (lname, (Some assoc), (f productions))

let eoi_entry e =
  let eoi_level l =
    let aux (prods : production list) =
      List.map
        (fun (symbs,(annot,act))  ->
           let symbs =
             List.map
               (function
                | `Sself -> `Snterm e
                | `Snext -> assert false
                | x -> x) symbs in
           ((symbs @
               [`Stoken
                  (((function | `EOI -> true | _ -> false)),
                    (`Normal, "`EOI"))]),
             (annot, (Gaction.mk (fun _  -> act))))) prods in
    refresh_level ~f:aux l in
  let result =
    {
      e with
      estart = (fun _  -> assert false);
      econtinue = (fun _  -> assert false)
    } in
  match result.edesc with
  | Dlevels ls ->
      begin
        result.edesc <- Dlevels (List.map eoi_level ls);
        result.estart <- Gparser.start_parser_of_entry result;
        result.econtinue <- Gparser.continue_parser_of_entry result; result
      end
  | Dparser _ -> failwith "Ginsert.eoi_entry Dparser"