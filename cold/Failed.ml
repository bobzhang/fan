open Structure

open LibUtil

open Format

let pp = fprintf

let name_of_descr = function | (`Antiquot,s) -> "$" ^ s | (_,s) -> s

let name_of_symbol entry =
  (function
   | `Snterm e -> "[" ^ (e.ename ^ "]")
   | `Snterml (e,l) -> "[" ^ (e.ename ^ (" level " ^ (l ^ "]")))
   | `Sself|`Snext -> "[" ^ (entry.ename ^ "]")
   | `Stoken (_,descr) -> name_of_descr descr
   | `Skeyword kwd -> "\"" ^ (kwd ^ "\"")
   | _ -> "???" : [> symbol] -> string )

let tree_in_entry prev_symb tree =
  function
  | Dlevels levels ->
      let rec search_level level =
        match search_tree level.lsuffix with
        | Some t -> Some (Node { node = `Sself; son = t; brother = DeadEnd })
        | None  -> search_tree level.lprefix
      and search_tree t =
        if (tree <> DeadEnd) && (t == tree)
        then Some t
        else
          (match t with
           | Node n ->
               (match search_symbol n.node with
                | Some symb ->
                    Some
                      (Node { node = symb; son = (n.son); brother = DeadEnd })
                | None  ->
                    (match search_tree n.son with
                     | Some t ->
                         Some
                           (Node
                              { node = (n.node); son = t; brother = DeadEnd })
                     | None  -> search_tree n.brother))
           | LocAct (_,_)|DeadEnd  -> None)
      and search_symbol symb =
        match symb with
        | `Snterm _|`Snterml (_,_)|`Slist0 _|`Slist0sep (_,_)|`Slist1 _
          |`Slist1sep (_,_)|`Sopt _|`Stry _|`Stoken _|`Stree _|`Skeyword _
          |`Speek _ when symb == prev_symb -> Some symb
        | `Slist0 symb ->
            (match search_symbol symb with
             | Some symb -> Some (`Slist0 symb)
             | None  -> None)
        | `Slist0sep (symb,sep) ->
            (match search_symbol symb with
             | Some symb -> Some (`Slist0sep (symb, sep))
             | None  ->
                 (match search_symbol sep with
                  | Some sep -> Some (`Slist0sep (symb, sep))
                  | None  -> None))
        | `Slist1 symb ->
            (match search_symbol symb with
             | Some symb -> Some (`Slist1 symb)
             | None  -> None)
        | `Slist1sep (symb,sep) ->
            (match search_symbol symb with
             | Some symb -> Some (`Slist1sep (symb, sep))
             | None  ->
                 (match search_symbol sep with
                  | Some sep -> Some (`Slist1sep (symb, sep))
                  | None  -> None))
        | `Sopt symb ->
            (match search_symbol symb with
             | Some symb -> Some (`Sopt symb)
             | None  -> None)
        | `Stry symb ->
            (match search_symbol symb with
             | Some symb -> Some (`Stry symb)
             | None  -> None)
        | `Speek symb ->
            (match search_symbol symb with
             | Some symb -> Some (`Speek symb)
             | None  -> None)
        | `Stree t ->
            (match search_tree t with
             | Some t -> Some (`Stree t)
             | None  -> None)
        | _ -> None in
      (try List.find_map search_level levels with | Not_found  -> tree)
  | Dparser _ -> tree

let rec name_of_symbol_failed entry =
  function
  | `Slist0 s|`Slist0sep (s,_)|`Slist1 s|`Slist1sep (s,_)|`Sopt s|`Stry s
    |`Speek s -> name_of_symbol_failed entry s
  | `Stree t -> name_of_tree_failed entry t
  | s -> name_of_symbol entry s
and name_of_tree_failed entry x =
  match x with
  | Node ({ node; brother; son } as y) ->
      (match Tools.get_terminals y with
       | None  ->
           let txt = name_of_symbol_failed entry node in
           let txt =
             match (node, son) with
             | (`Sopt _,Node _) ->
                 txt ^ (" or " ^ (name_of_tree_failed entry son))
             | _ -> txt in
           let txt =
             match brother with
             | DeadEnd |LocAct (_,_) -> txt
             | Node _ -> txt ^ (" or " ^ (name_of_tree_failed entry brother)) in
           txt
       | Some (tokl,_,_) ->
           List.fold_left
             (fun s  tok  ->
                (if s = "" then "" else s ^ " then ") ^
                  (match tok with
                   | `Stoken (_,descr) -> name_of_descr descr
                   | `Skeyword kwd -> kwd)) "" tokl)
  | DeadEnd |LocAct (_,_) -> "???"

let magic _s x = Obj.magic x

let tree_failed entry prev_symb_result prev_symb tree =
  let txt = name_of_tree_failed entry tree in
  let txt =
    match prev_symb with
    | `Slist0 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ (" or " ^ (txt ^ " expected"))
    | `Slist1 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ (" or " ^ (txt ^ " expected"))
    | `Slist0sep (s,sep) ->
        (match magic "tree_failed: 'a -> list 'b" prev_symb_result with
         | [] ->
             let txt1 = name_of_symbol_failed entry s in
             txt1 ^ (" or " ^ (txt ^ " expected"))
         | _ ->
             let txt1 = name_of_symbol_failed entry sep in
             txt1 ^ (" or " ^ (txt ^ " expected")))
    | `Slist1sep (s,sep) ->
        (match magic "tree_failed: 'a -> list 'b" prev_symb_result with
         | [] ->
             let txt1 = name_of_symbol_failed entry s in
             txt1 ^ (" or " ^ (txt ^ " expected"))
         | _ ->
             let txt1 = name_of_symbol_failed entry sep in
             txt1 ^ (" or " ^ (txt ^ " expected")))
    | `Stry _|`Speek _|`Sopt _|`Stree _ -> txt ^ " expected"
    | _ -> txt ^ (" expected after " ^ (name_of_symbol entry prev_symb)) in
  if FanConfig.verbose.contents
  then
    (let tree = tree_in_entry prev_symb tree entry.edesc in
     let f = err_formatter in
     pp f
       ("@[<v 0>@,----------------------------------@," ^^
          ("Parse error in entry [%s], rule:@;<0 2>@[%a@]@," ^^
             "----------------------------------@,@]@.")) entry.ename
       Print.text#rules (flatten_tree tree))
  else ();
  txt ^ (" (in [" ^ (entry.ename ^ "])"))

let symb_failed entry prev_symb_result prev_symb symb =
  let tree = Node { node = symb; brother = DeadEnd; son = DeadEnd } in
  tree_failed entry prev_symb_result prev_symb tree

let symb_failed_txt e s1 s2 = symb_failed e 0 s1 s2