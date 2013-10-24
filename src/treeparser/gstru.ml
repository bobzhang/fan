



let mk_action=Gaction.mk

(* tree processing *)  
let rec flatten_tree (x: Gstructure.tree ) =
  match x with 
  | DeadEnd -> []
  | LocAct (_, _) -> [[]]
  | Node {node = n; brother = b; son = s} ->
      List.map (fun l -> n::l) (flatten_tree s) @ flatten_tree b 

type brothers =
  | Bro of Gstructure.symbol * brothers list
  | End


  


let get_brothers x =
  let rec aux acc (x:Gstructure.tree) =
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
  let rec aux acc (x:Gstructure.tree) =
    match x with 
    |Node {node;brother;_} ->
        aux (node::acc) brother
    |LocAct (_,_) | DeadEnd -> acc  in
  aux []

let get_first_from levels set =
  levels |>
  List.iter
    (fun (level:Gstructure.level) ->
      level.lprefix |> get_first |> Hashset.add_list set)
    

  
