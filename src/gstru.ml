open Gstructure
open LibUtil  


let mk_action=Gaction.mk
let string_of_token=FToken.extract_string 

(* tree processing *)  
let rec flatten_tree = function
  | DeadEnd -> []
  | LocAct (_, _) -> [[]]
  | Node {node = n; brother = b; son = s} ->
      List.map (fun l -> n::l) (flatten_tree s) @ flatten_tree b 

type brothers =
  | Bro of symbol * brothers list
  | End


  


let get_brothers x =
  let rec aux acc =  function
    | DeadEnd -> List.rev acc 
    | LocAct _ -> List.rev (End:: acc)
    | Node {node = n; brother = b; son = s} ->
        aux  (Bro n (aux [] s) :: acc) b  in aux [] x 
let get_children x = 
  let rec aux acc =  function
    | [] -> List.rev acc
    | [Bro (n, x)] -> aux (n::acc) x
    | _ -> raise Exit  in aux [] x 

(* level -> lprefix -> *)  
let get_first =
  let rec aux acc = function
    |Node {node;brother;_} ->
        aux (node::acc) brother
    |LocAct (_,_) | DeadEnd -> acc  in
  aux []

let get_first_from levels set =
  List.iter
    (fun level -> level.lprefix |> get_first |> Hashset.add_list set)
    levels

  
