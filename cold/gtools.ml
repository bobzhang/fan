open LibUtil

open Gstructure

let empty_entry ename _ =
  raise (XStream.Error ("entry [" ^ (ename ^ "] is empty")))

let get_cur_loc strm =
  match XStream.peek strm with | Some (_,r) -> r | None  -> FLoc.ghost

let get_prev_loc strm =
  match XStream.get_last strm with | Some (_,l) -> l | None  -> FLoc.ghost

let is_level_labelled n =
  function | { lname = Some n1;_} -> n = n1 | _ -> false

let get_terminals x =
  let rec aux tokl last_tok =
    function
    | Node { node = (#terminal as tok); son; brother = DeadEnd  } ->
        aux (last_tok :: tokl) tok son
    | tree ->
        if tokl = []
        then None
        else Some ((List.rev (last_tok :: tokl)), last_tok, tree) in
  match x with
  | { node = (#terminal as x); son;_} -> aux [] x son
  | _ -> None

let eq_Stoken_ids s1 s2 =
  match (s1, s2) with
  | ((`Antiquot,_),_) -> false
  | (_,(`Antiquot,_)) -> false
  | ((_,s1),(_,s2)) -> s1 = s2

let logically_eq_symbols entry =
  let rec eq_symbol s1 s2 =
    match (s1, s2) with
    | (`Snterm e1,`Snterm e2) -> e1.ename = e2.ename
    | (`Snterm e1,`Sself) -> e1.ename = entry.ename
    | (`Sself,`Snterm e2) -> entry.ename = e2.ename
    | (`Snterml (e1,l1),`Snterml (e2,l2)) ->
        (e1.ename = e2.ename) && (l1 = l2)
    | (`Slist0 s1,`Slist0 s2)|(`Slist1 s1,`Slist1 s2)|(`Sopt s1,`Sopt s2)
      |(`Speek s1,`Speek s2)|(`Stry s1,`Stry s2) -> eq_symbol s1 s2
    | (`Slist0sep (s1,sep1),`Slist0sep (s2,sep2))
      |(`Slist1sep (s1,sep1),`Slist1sep (s2,sep2)) ->
        (eq_symbol s1 s2) && (eq_symbol sep1 sep2)
    | (`Stoken (_,s1),`Stoken (_,s2)) -> eq_Stoken_ids s1 s2
    | _ -> s1 = s2 in
  eq_symbol

let rec eq_symbol (s1 : symbol) (s2 : symbol) =
  match (s1, s2) with
  | (`Snterm e1,`Snterm e2) -> e1 == e2
  | (`Snterml (e1,l1),`Snterml (e2,l2)) -> (e1 == e2) && (l1 = l2)
  | (`Sself,`Sself) -> true
  | (`Slist0 s1,`Slist0 s2)|(`Slist1 s1,`Slist1 s2)|(`Sopt s1,`Sopt s2)
    |(`Speek s1,`Speek s2)|(`Stry s1,`Stry s2) -> eq_symbol s1 s2
  | (`Slist0sep (s1,sep1),`Slist0sep (s2,sep2))
    |(`Slist1sep (s1,sep1),`Slist1sep (s2,sep2)) ->
      (eq_symbol s1 s2) && (eq_symbol sep1 sep2)
  | (`Stoken (_,s1),`Stoken (_,s2)) -> eq_Stoken_ids s1 s2
  | _ -> s1 = s2