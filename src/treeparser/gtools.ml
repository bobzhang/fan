

let empty_entry ename _ =
  raise (Streamf.Error ("entry [" ^ ename ^ "] is empty"))





let is_level_labelled n l =
  match (l:Gdefs.level) with 
  | {lname=Some n1 ; _  } -> n = n1
  | _ -> false 

(*
  try to decouple the node [x] into
  (terminals,node,son) triple, the length of
  terminals should have at least length [2], otherwise,
  it does not make sense
  {[

  ]}
 *)  
let get_terminals x =
  let rec aux tokl last_tok  x =
    match (x:Gdefs.tree) with 
    | Node {node = `Token tok (* (#Tokenf.terminal as tok) *); son; brother = DeadEnd}
      ->  aux (last_tok :: tokl) tok son
    | tree ->
        if tokl = [] then None (* FIXME?*)
        else Some (List.rev (last_tok :: tokl), last_tok, tree)  in
  match (x:Gdefs.node) with
  | {node=`Token tok;son;_} ->
    (* first case we don't require anything on [brother] *)
     (aux [] tok son)
  | _ -> None 


(** used in [Delete], the delete API may be deprecated in the future *)        
let logically_eq_symbols (entry:Gdefs.entry) =
  let rec eq_symbol (s1:Gdefs.symbol) (s2:Gdefs.symbol) =
    match (s1, s2) with
    | (`Nterm e1, `Nterm e2) -> e1.name = e2.name
    | (`Nterm e1, `Self) -> e1.name = entry.name
    | (`Self, `Nterm e2) -> entry.name = e2.name
    (* | (`Self, `Self) -> true *)
    | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1.name = e2.name && l1 = l2
    | (`List0 s1, `List0 s2)
    | (`List1 s1, `List1 s2)
    | (`Peek s1, `Peek s2)
    | (`Try s1, `Try s2) -> eq_symbol s1 s2
    | (`List0sep (s1, sep1), `List0sep (s2, sep2))
    | (`List1sep (s1, sep1), `List1sep (s2, sep2)) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | `Token x , `Token  y  -> Tokenf.eq_pattern x y 
    | _ -> s1 = s2 in
  eq_symbol

(* used in [Insert] *)
let rec eq_symbol (s1:Gdefs.symbol) (s2:Gdefs.symbol) =
  match (s1, s2) with
  | (`Nterm e1, `Nterm e2) -> e1 == e2
  | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1 == e2 && l1 = l2
  | (`Self, `Self) -> true
  | (`List0 s1, `List0 s2)
  | (`List1 s1, `List1 s2)
  | (`Peek s1, `Peek s2)
  | (`Try s1, `Try s2) -> eq_symbol s1 s2
  | (`List0sep (s1, sep1), `List0sep (s2, sep2))
  | (`List1sep (s1, sep1), `List1sep (s2, sep2)) ->
      eq_symbol s1 s2 && eq_symbol sep1 sep2
  | `Token x, `Token y -> Tokenf.eq_pattern  x y
  | _ -> s1 = s2
      





let rec entry_first (v:Gdefs.entry) : string list =
 Listf.concat_map level_first v.levels

and level_first (x:Gdefs.level) : string list =
  tree_first x.lprefix

and tree_first (x:Gdefs.tree) : string list =
  match x with
  | Node {node;brother;_} ->
      symbol_first node @ tree_first brother
  | LocAct _ | DeadEnd -> []

and symbol_first (x:Gdefs.symbol) : string list  = 
  match x  with
  | `Nterm e -> entry_first e
  | `Snterml (e,_) -> entry_first e
        
  | `List0 s
  | `List1 s 
  | `List0sep (s,_) 
  | `List1sep (s,_) -> symbol_first s 
  | `Self -> assert false
  | `Try s 
  | `Peek s ->symbol_first s
  (* | `Keyword s -> [s ] (\* FIXME *\) *)
  | `Token _ -> []



        
let mk_action=Gaction.mk

(* tree processing *)  
let rec flatten_tree (x: Gdefs.tree ) =
  match x with 
  | DeadEnd -> []
  | LocAct (_, _) -> [[]]
  | Node {node = n; brother = b; son = s} ->
      List.map (fun l -> n::l) (flatten_tree s) @ flatten_tree b 

type brothers =
  | Bro of Gdefs.symbol * brothers list
  | End


  


let get_brothers x =
  let rec aux acc (x:Gdefs.tree) =
    match x with
    | DeadEnd -> List.rev acc 
    | LocAct _ -> List.rev (End:: acc)
    | Node {node = n; brother = b; son = s} ->
        aux  (Bro (n, aux [] s) :: acc) b  in aux [] x 
let get_children x = 
  let rec aux acc =  function
    | [] -> List.rev acc
    | [Bro (n, x)] -> aux (n::acc) x
    | _ -> raise Exit  in aux [] x 

(* level -> lprefix -> *)  
let get_first =
  let rec aux acc (x:Gdefs.tree) =
    match x with 
    |Node {node;brother;_} ->
        aux (node::acc) brother
    |LocAct (_,_) | DeadEnd -> acc  in
  aux []

let get_first_from levels set =
  levels |>
  List.iter
    (fun (level:Gdefs.level) ->
      level.lprefix |> get_first |> Hashset.add_list set)
    
        

(* let rec get_first = fun *)
(*   [ Node {node;brother} -> [node::get_first brother] *)
(*   | _ -> [] ]; *)
(*    
let rec append_tree (a:tree) (b:tree)  =
  match a with
  [Node {node;son;brother=DeadEnd} ->
    (* merge_tree *)
      Node {node; son = append_tree son b; brother = DeadEnd }
      (* (append_tree brother b) *)
  |Node {node;son;brother} ->
      merge_tree 
        (Node {node;son=append_tree son b; brother=DeadEnd})
        (append_tree brother b)
  | LocAct (anno_action,ls) ->
      LocActAppend(anno_action,ls,b)

  | DeadEnd -> assert false 

  | LocActAppend (anno_action,ls,la) ->
      LocActAppend (anno_action,ls, append_tree la b)
  ]
and merge_tree (a:tree) (b:tree) : tree =
  match (a,b) with
  [ (DeadEnd,_) -> b
  | (_,DeadEnd) -> a 
  | (Node {node=n1;son=s1;brother=b1}, Node{node=n2;son=s2;brother=b2}) ->
      if eq_symbol n1 n2 then
        merge_tree
          (Node {node=n1; son = merge_tree s1 s2;brother=DeadEnd })
          (merge_tree b1 b2)
      else
        Node {node=n1;son=s1;brother = merge_tree b1 b}
  | (Node {node;son;brother=b1},(LocAct _ as b2) ) ->
      Node {node;son;brother = merge_tree b1 b2}
  | (Node {node;son;brother=b1}, (LocActAppend (act,ls,n2))) ->
  | (LocAct (act,ls), LocActAppend (act2,ls2,n2)) ->
      LocActAppend (act2,ls2,)
  ]
;
*)

(* local variables: *)
(* compile-command: "cd .. && pmake treeparser/gtools.cmo" *)
(* end: *)
