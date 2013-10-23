

open Gstructure



open Format

let pp = fprintf 

  
let  name_of_symbol entry : [> symbol] -> string  =  function
  | `Snterm e -> "[" ^ e.name ^ "]"
  | `Snterml (e, l) -> "[" ^ e.name ^ " level " ^ l ^ "]"
  | `Sself  -> "[" ^ entry.name ^ "]"
  | `Stoken (_, _,descr) -> descr
  | `Skeyword kwd -> "\"" ^ kwd ^ "\""
  | _ -> "???" 

let tree_in_entry prev_symb tree = function
  | Dlevels levels ->
      let rec search_level level =
        match search_tree level.lsuffix with
        | Some t -> Some (Node {node = `Sself; son = t; brother = DeadEnd})
        | None -> search_tree level.lprefix 
      and search_tree t =
        if tree <> DeadEnd && t == tree then (* physical equivality*)
          Some t
        else
          match t with
          | Node n ->
              begin match search_symbol n.node with
              |Some symb ->
                  Some (Node {node = symb; son = n.son; brother = DeadEnd})
              | None ->
                  begin match search_tree n.son with
                  | Some t ->
                      Some (Node {node = n.node; son = t; brother = DeadEnd})
                  | None -> search_tree n.brother
                  end
              end
          | LocAct (_, _) | DeadEnd -> None 
      and search_symbol symb =
        match symb with
        | `Snterm _ | `Snterml (_, _) | `Slist0 _ | `Slist0sep (_, _) | `Slist1 _ |
          `Slist1sep (_, _) | `Sopt _ | `Stry _ | `Stoken _ | `Skeyword _
        | `Speek _
          when symb == prev_symb ->
            Some symb
        | `Slist0 symb ->
            begin match search_symbol symb with
            | Some symb -> Some (`Slist0 symb)
            | None -> None
            end
        | `Slist0sep (symb, sep) ->
            begin match search_symbol symb with
            | Some symb -> Some (`Slist0sep (symb, sep))
            | None ->
                match search_symbol sep with
                | Some sep -> Some (`Slist0sep (symb, sep))
                | None -> None 
            end
        | `Slist1 symb ->
            begin match search_symbol symb with
            | Some symb -> Some (`Slist1 symb)
            | None -> None
            end
        | `Slist1sep (symb, sep) ->
            begin match search_symbol symb with
            | Some symb -> Some (`Slist1sep (symb, sep))
            | None ->
                match search_symbol sep with
                | Some sep -> Some (`Slist1sep (symb, sep))
                | None -> None 
            end
        | `Sopt symb ->
            begin match search_symbol symb with
            | Some symb -> Some (`Sopt symb)
            | None -> None
            end
        | `Stry symb ->
            begin match search_symbol symb with
            | Some symb -> Some (`Stry symb)
            | None -> None
            end
        | `Speek symb ->
            begin match search_symbol symb with
            | Some symb -> Some (`Speek symb)
            | None -> None
            end
        | _ -> None  in
      (match Listf.find_map search_level  levels with
      | Some x -> x
      | None -> tree)
  | Dparser _ -> tree 

        
(* error message entrance *)
let rec name_of_symbol_failed entry  = function
  | `Slist0 s | `Slist0sep (s, _) |
    `Slist1 s | `Slist1sep (s, _) |
    `Sopt s | `Stry s | `Speek s  ->
      name_of_symbol_failed entry s
  | s -> name_of_symbol entry s
and name_of_tree_failed entry x =
  match x with 
  | Node ({node ; brother; son = son} as y)->
      begin match Gtools.get_terminals y  with
      | None ->
          let txt = name_of_symbol_failed entry node in
          let txt =
            match (node, son) with (* when the current node is Opt *)
            | (`Sopt _, Node _) -> txt ^ " or " ^ name_of_tree_failed entry son
            | _ -> txt   in
          let txt =
            match brother with
            | DeadEnd | LocAct (_, _) -> txt
            | Node _ -> txt ^ " or " ^ name_of_tree_failed entry brother  in
          txt
      | Some (tokl, _, _) ->
          List.fold_left
            (fun s tok ->
              ((if s = "" then "" else s ^ " then ") ^
               (match tok with
               | `Stoken (_, _,descr) ->  descr
               | `Skeyword kwd -> kwd))) "" tokl 
      end
  | DeadEnd | LocAct (_, _) -> "???" 

let magic _s x = (* debug magic "Obj.magic: %s@." _s in *) Obj.magic x

(* [prev_symb_result] is cast by [Obj.magic] *)
let tree_failed ?(verbose=false) entry prev_symb_result prev_symb tree =
  let txt = name_of_tree_failed entry tree in
  let txt =
    match prev_symb with
    | `Slist0 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | `Slist1 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | `Slist0sep (s, sep) ->
        begin match magic "tree_failed: 'a -> 'b list" prev_symb_result with
        | [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected"
        end
    | `Slist1sep (s, sep) ->
        begin match magic "tree_failed: 'a -> 'b list" prev_symb_result with
        | [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected"
        end
    | `Stry _ | `Speek _ (*NP: not sure about this*) | `Sopt _  -> txt ^ " expected"
    | _ -> txt ^ " expected after " ^ name_of_symbol entry prev_symb  in
  begin
    (* it may not necessary fail when  we use try somewhere*)
    if verbose then
      let tree = tree_in_entry prev_symb tree entry.desc in
      pp err_formatter
        ("@[<v 0>@,----------------------------------@,"^^
         "Parse error in entry [%s], rule:@;<0 2>@[%a@]@," ^^
         "----------------------------------@,@]@.")
        entry.name
        (Gprint.text#rules ) (Gstru.flatten_tree tree);
    else ();
    txt ^ " (in [" ^ entry.name ^ "])"
  end
    
let symb_failed entry prev_symb_result prev_symb symb =
  let tree = Node {node = symb; brother = DeadEnd; son = DeadEnd} in
  tree_failed entry prev_symb_result prev_symb tree

let symb_failed_txt e s1 s2 =
  symb_failed e 0 s1 s2



(* local variables: *)
(* compile-command: "pmake gfailed.cmo" *)
(* end: *)
