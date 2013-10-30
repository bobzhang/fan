
open Gstructure


let empty_entry ename _ =
  raise (Streamf.Error ("entry [" ^ ename ^ "] is empty"))



(* get_cur_loc *must* be used first *)  
let get_cur_loc (strm:Tokenf.stream) =
  match Streamf.peek strm with
  | Some r -> Tokenf.get_loc r 
  | None -> Locf.ghost 


let get_prev_loc strm =
  match Streamf.get_last strm with
  |Some l -> Tokenf.get_loc l
  |None -> Locf.ghost

let is_level_labelled n =   function
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
  let rec aux tokl last_tok  = function
    | Node {node = (#terminal as tok); son; brother = DeadEnd}
      ->  aux (last_tok :: tokl) tok son
    | tree ->
        if tokl = [] then None (* FIXME?*)
        else Some (List.rev (last_tok :: tokl), last_tok, tree)  in
  match x with
  | {node=(#terminal as x);son;_} ->
    (* first case we don't require anything on [brother] *)
     (aux [] x son)
  | _ -> None 


let eq_token  (x : Gstructure.token_pattern)  (y:Gstructure.token_pattern) : bool =
  match x , y with 
  | (_,a,_) , (_,b,_) -> a  =  b

(** used in [Delete], the delete API may be deprecated in the future *)        
let logically_eq_symbols entry =
  let rec eq_symbol (s1:symbol) (s2:symbol) =
    match (s1, s2) with
    | (`Nterm e1, `Nterm e2) -> e1.name = e2.name
    | (`Nterm e1, `Self) -> e1.name = entry.name
    | (`Self, `Nterm e2) -> entry.name = e2.name
    (* | (`Self, `Self) -> true *)
    | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1.name = e2.name && l1 = l2
    | (`List0 s1, `List0 s2) |
      (`List1 s1, `List1 s2) |
      (* (`Opt s1, `Opt s2) | *)
      (`Peek s1, `Peek s2) |
      (`Try s1, `Try s2) -> eq_symbol s1 s2
    | (`List0sep (s1, sep1), `List0sep (s2, sep2)) |
      (`List1sep (s1, sep1), `List1sep (s2, sep2)) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | `Token x , `Token  y  -> eq_token x y 
    | _ -> s1 = s2 in
  eq_symbol

(* used in [Insert] *)
let rec eq_symbol (s1:symbol) (s2:symbol) =
  match (s1, s2) with
  | (`Nterm e1, `Nterm e2) -> e1 == e2
  | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1 == e2 && l1 = l2
  | (`Self, `Self) -> true
  | (`List0 s1, `List0 s2) |
    (`List1 s1, `List1 s2) |
    (* (`Opt s1, `Opt s2) | *)
    (`Peek s1, `Peek s2) |
    (`Try s1, `Try s2) -> eq_symbol s1 s2
  | (`List0sep (s1, sep1), `List0sep (s2, sep2)) |
    (`List1sep (s1, sep1), `List1sep (s2, sep2)) ->
      eq_symbol s1 s2 && eq_symbol sep1 sep2
  | `Token x, `Token y -> eq_token x y
  | _ -> s1 = s2
      





        

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
