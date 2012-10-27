open Structure
let delete_rule_in_tree (entry) =
  let rec delete_in_tree (symbols) (tree) = begin match (symbols,tree) with
    | (s::sl,Node(n)) ->
        if (Tools.logically_eq_symbols entry s ( n.node )) then begin
          (delete_son sl n)
        end else begin
          begin match (delete_in_tree symbols ( n.brother )) with
          | Some(dsl,t) ->
              Some
                ((dsl,(
                  Node ({node = ( n.node );son = ( n.son );brother = t}) )))
          | None  ->   None end
        end
    | (_::_,_) ->   None
    | ([] ,Node(n)) ->
        begin match (delete_in_tree []  ( n.brother )) with
        | Some(dsl,t) ->
            Some
              ((dsl,( Node ({node = ( n.node );son = ( n.son );brother = t})
                )))
        | None  ->   None end
    | ([] ,DeadEnd ) ->   None
    | ([] ,LocAct(_,[] )) ->   Some ((( Some ([]) ),DeadEnd ))
    | ([] ,LocAct(_,action::list)) ->
        Some ((None ,( LocAct ((action,list)) )))
    end and delete_son (sl) (n) = begin match (delete_in_tree sl ( n.son ))
    with
    | Some(Some(dsl),DeadEnd ) ->
        Some ((( Some (n.node::dsl) ),( n.brother )))
    | Some(Some(dsl),t) ->
        let t = Node ({node = ( n.node );son = t;brother = ( n.brother )}) in
        Some ((( Some (n.node::dsl) ),t))
    | Some(None ,t) ->
        let t = Node ({node = ( n.node );son = t;brother = ( n.brother )}) in
        Some ((None ,t))
    | None  ->   None end in
  delete_in_tree
let rec decr_keyw_use (gram) =
  (function
  | (`Skeyword kwd) ->   (removing gram kwd)
  | (`Smeta (_,sl,_)) ->   (List.iter ( (decr_keyw_use gram) ) sl)
  | ((`Slist0 s)|(`Slist1 s)|(`Sopt s)|(`Stry s)) ->   (decr_keyw_use gram s)
  | (`Slist0sep (s1,s2)) ->
      begin
      (decr_keyw_use gram s1);
      (decr_keyw_use gram s2)
      end
  | (`Slist1sep (s1,s2)) ->
      begin
      (decr_keyw_use gram s1);
      (decr_keyw_use gram s2)
      end
  | (`Stree t) ->   (decr_keyw_use_in_tree gram t)
  | (`Sself|`Snext|(`Snterm _)|(`Snterml (_,_))|(`Stoken _)) ->   ()) and
  decr_keyw_use_in_tree (gram) =
  (function
  | (DeadEnd |LocAct(_,_)) ->   ()
  | Node(n) ->
      begin
      (decr_keyw_use gram ( n.node ));
      (decr_keyw_use_in_tree gram ( n.son ));
      (decr_keyw_use_in_tree gram ( n.brother ))
      end)
let rec delete_rule_in_suffix (entry) (symbols) =
  (function
  | lev::levs ->
      begin match (delete_rule_in_tree entry symbols ( lev.lsuffix )) with
      | Some(dsl,t) ->
          begin
          begin
          match
          dsl
          with
          | Some(dsl) ->
              (List.iter ( (decr_keyw_use ( entry.egram )) ) dsl)
          | None  ->   ()
          end;
          begin
          match
          t
          with
          | DeadEnd  when (( lev.lprefix ) == DeadEnd ) ->   levs
          | _ ->
              let lev =
                {assoc = ( lev.assoc );lname = ( lev.lname );lsuffix = t;
                  lprefix = ( lev.lprefix )} in
              lev::levs
          end
          end
      | None  ->
          let levs = (delete_rule_in_suffix entry symbols levs) in lev::levs
      end
  | []  ->   (raise Not_found ))
let rec delete_rule_in_prefix (entry) (symbols) =
  (function
  | lev::levs ->
      begin match (delete_rule_in_tree entry symbols ( lev.lprefix )) with
      | Some(dsl,t) ->
          begin
          begin
          match
          dsl
          with
          | Some(dsl) ->
              (List.iter ( (decr_keyw_use ( entry.egram )) ) dsl)
          | None  ->   ()
          end;
          begin
          match
          t
          with
          | DeadEnd  when (( lev.lsuffix ) == DeadEnd ) ->   levs
          | _ ->
              let lev =
                {assoc = ( lev.assoc );lname = ( lev.lname );lsuffix = (
                                                               lev.lsuffix );
                  lprefix = t} in
              lev::levs
          end
          end
      | None  ->
          let levs = (delete_rule_in_prefix entry symbols levs) in lev::levs
      end
  | []  ->   (raise Not_found ))
let delete_rule_in_level_list (entry) (symbols) (levs) = begin match symbols
  with
  | `Sself::symbols ->   (delete_rule_in_suffix entry symbols levs)
  | (`Snterm e)::symbols when (e == entry) ->
      (delete_rule_in_suffix entry symbols levs)
  | _ ->   (delete_rule_in_prefix entry symbols levs) end
let delete_rule (entry) (sl) = begin match entry.edesc with
  | Dlevels(levs) ->
      let levs = (delete_rule_in_level_list entry sl levs) in
      begin
        entry.edesc <- Dlevels (levs);
        entry.estart <-
          (fun (lev) ->
            (fun (strm) ->
              let f = (Parser.start_parser_of_entry entry) in
              begin
                entry.estart <- f;
                (f lev strm)
                end));
        entry.econtinue <-
          (fun (lev) ->
            (fun (bp) ->
              (fun (a) ->
                (fun (strm) ->
                  let f = (Parser.continue_parser_of_entry entry) in
                  begin
                    entry.econtinue <- f;
                    (f lev bp a strm)
                    end))))
        end
  | Dparser(_) ->   () end