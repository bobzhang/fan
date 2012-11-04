

open Structure;
open Format;
let pp = fprintf ;

let name_of_descr = fun
  [(`Antiquot,s) -> "$"^s
  |(_,s) -> s ];
  
let  name_of_symbol entry : [> symbol] -> string  =  fun
  [ `Snterm e -> "[" ^ e.ename ^ "]"
  | `Snterml (e, l) -> "[" ^ e.ename ^ " level " ^ l ^ "]"
  | `Sself | `Snext -> "[" ^ entry.ename ^ "]"
  | `Stoken (_, descr) -> name_of_descr descr
  | `Skeyword kwd -> "\"" ^ kwd ^ "\""
  | _ -> "???" ];

(* error message entrance *)
let rec name_of_symbol_failed entry : [>symbol] -> string = fun
  [ `Slist0 s | `Slist0sep (s, _) |
    `Slist1 s | `Slist1sep (s, _) |
    `Sopt s | `Stry s ->
      name_of_symbol_failed entry s
  | `Stree t -> name_of_tree_failed entry t
  | s -> name_of_symbol entry s ]
and name_of_tree_failed entry : tree -> string =  fun
  [ Node {node = s; brother = bro; son = son} ->
      let tokl =
        match s with
        [ `Stoken _ | `Skeyword _
          ->  Tools.get_token_list s son
        | _ -> None ]  in
      match tokl with
      [ None ->
          let txt = name_of_symbol_failed entry s in
          let txt =
            match (s, son) with (* when the current node is Opt *)
            [ (`Sopt _, Node _) -> txt ^ " or " ^ name_of_tree_failed entry son
            | _ -> txt ]  in
          let txt =
            match bro with
            [ DeadEnd | LocAct _ _ -> txt
            | Node _ -> txt ^ " or " ^ name_of_tree_failed entry bro ] in
          txt
      | Some (tokl, _, _) ->
          List.fold_left
            (fun s tok ->
               ((if s = "" then "" else s ^ " then ") ^
                match tok with
                  [ `Stoken (_, descr) -> name_of_descr descr
                  | `Skeyword kwd -> kwd
                  | _ -> assert false ])) "" tokl ]
  | DeadEnd | LocAct _ _ -> "???" ];

let magic _s x = debug magic "Obj.magic: %s@." _s in Obj.magic x;

(* [prev_symb_result] is cast by [Obj.magic] *)
let tree_failed entry prev_symb_result prev_symb tree =
  let txt = name_of_tree_failed entry tree in
  let txt = match prev_symb with
    [ `Slist0 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | `Slist1 s ->
        let txt1 = name_of_symbol_failed entry s in
        txt1 ^ " or " ^ txt ^ " expected"
    | `Slist0sep (s, sep) ->
        match magic "tree_failed: 'a -> list 'b" prev_symb_result with
        [ [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected" ]
    | `Slist1sep (s, sep) ->
        match magic "tree_failed: 'a -> list 'b" prev_symb_result with
        [ [] ->
            let txt1 = name_of_symbol_failed entry s in
            txt1 ^ " or " ^ txt ^ " expected"
        | _ ->
            let txt1 = name_of_symbol_failed entry sep in
            txt1 ^ " or " ^ txt ^ " expected" ]
    | `Stry _(*NP: not sure about this*) | `Sopt _ | `Stree _ -> txt ^ " expected"
    | _ -> txt ^ " expected after " ^ name_of_symbol entry prev_symb ] in begin
        if !(entry.egram.error_verbose) then 
          let tree = Search.tree_in_entry prev_symb tree entry.edesc in 
          let f = err_formatter in begin
            pp f ("@[<v 0>@,----------------------------------@,"^^
                  "Parse error in entry [%s], rule:@;<0 2>@[%a@]@," ^^
                  "----------------------------------@,@]@.")
              entry.ename
              (Print.text#level ~space:"@\n"   (* Format.pp_force_newline *)) (flatten_tree tree);
          end
        else ();
        txt ^ " (in [" ^ entry.ename ^ "])"
    end;
  
let symb_failed entry prev_symb_result prev_symb symb =
  let tree = Node {node = symb; brother = DeadEnd; son = DeadEnd} in
  tree_failed entry prev_symb_result prev_symb tree;

let symb_failed_txt e s1 s2 = symb_failed e 0 s1 s2;


