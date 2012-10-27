open Structure
let tree_in_entry (prev_symb) (tree) =
  (function
  | Dlevels(levels) ->
      let rec search_levels =
        (function
        | []  ->   tree
        | level::levels ->
            begin match (search_level level) with
            | Some(tree) ->   tree
            | None  ->   (search_levels levels) end)
        and search_level (level) = begin match
        (search_tree ( level.lsuffix )) with
        | Some(t) ->
            Some (Node ({node = `Sself;son = t;brother = DeadEnd }))
        | None  ->   (search_tree ( level.lprefix )) end and search_tree (t)
        =
        if (( (tree <> DeadEnd ) ) && ( (t == tree) )) then begin
          Some (t)
        end else begin
          begin match t with
          | Node(n) ->
              begin match (search_symbol ( n.node )) with
              | Some(symb) ->
                  Some
                    (Node ({node = symb;son = ( n.son );brother = DeadEnd }))
              | None  ->
                  begin match (search_tree ( n.son )) with
                  | Some(t) ->
                      Some
                        (Node
                           ({node = ( n.node );son = t;brother = DeadEnd }))
                  | None  ->   (search_tree ( n.brother )) end
              end
          | (LocAct(_,_)|DeadEnd ) ->   None end
        end and search_symbol (symb) = begin match symb with
        | ((`Snterm _)|(`Snterml (_,_))|(`Slist0 _)|(`Slist0sep (_,_))|
          (`Slist1 _)|(`Slist1sep (_,_))|(`Sopt _)|(`Stry _)|(`Stoken _)|
          (`Stree _)|(`Skeyword _)) when (symb == prev_symb) ->   Some (symb)
        | (`Slist0 symb) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Slist0 (symb))
            | None  ->   None end
        | (`Slist0sep (symb,sep)) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Slist0sep ((symb,sep)))
            | None  ->
                begin match (search_symbol sep) with
                | Some(sep) ->   Some (`Slist0sep ((symb,sep)))
                | None  ->   None end
            end
        | (`Slist1 symb) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Slist1 (symb))
            | None  ->   None end
        | (`Slist1sep (symb,sep)) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Slist1sep ((symb,sep)))
            | None  ->
                begin match (search_symbol sep) with
                | Some(sep) ->   Some (`Slist1sep ((symb,sep)))
                | None  ->   None end
            end
        | (`Sopt symb) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Sopt (symb))
            | None  ->   None end
        | (`Stry symb) ->
            begin match (search_symbol symb) with
            | Some(symb) ->   Some (`Stry (symb))
            | None  ->   None end
        | (`Stree t) ->
            begin match (search_tree t) with
            | Some(t) ->   Some (`Stree (t))
            | None  ->   None end
        | _ ->   None end in
      (search_levels levels)
  | Dparser(_) ->   tree)