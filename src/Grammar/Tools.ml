open LibUtil;
open Structure;

let get_prev_loc_only = ref false;

let empty_entry ename _ =
  raise (XStream.Error ("entry [" ^ ename ^ "] is empty"));

(* Make sure [get_prev_loc] will not peek *)  
let keep_prev_loc (strm: XStream.t ('c * FanLoc.t) ) :  XStream.t ('c * token_info) =
  match XStream.peek strm with
  [ None -> [< >]
  | Some (tok0,init_loc) ->
      let rec go prev_loc strm1 =
        if !get_prev_loc_only then
          [<  (tok0,{prev_loc;cur_loc=prev_loc;prev_loc_only=true});
              'go prev_loc strm1 >]
        else match strm1 with parser
        [ [< (tok,cur_loc); 'strm >] ->
            [< (tok, {prev_loc; cur_loc; prev_loc_only = false});
               'go cur_loc strm >]
        | [< >] -> [< >] ] in
        go init_loc strm ];

(* not used *)    
let drop_prev_loc strm = XStream.map (fun (tok,r) -> (tok,r.cur_loc)) strm;

(* get_cur_loc *must* be used first *)  
let get_cur_loc strm =
  match XStream.peek strm with
  [ Some (_,r) -> r.cur_loc
  | None -> FanLoc.ghost ];

(* (\* Make sure [get_prev_loc] will not peek *\)     *)
(* let get_prev_loc strm = begin *)
(*   get_prev_loc_only:=true; *)
(*   let result = *)
(*     match XStream.peek strm with *)
(*     [ Some (_, {prev_loc; prev_loc_only = true; _}) -> *)
(*       begin XStream.junk strm; prev_loc end *)
(*     | Some (_, {prev_loc; prev_loc_only = false;_}) -> prev_loc *)
(*     | None -> FanLoc.ghost ] in *)
(*   (get_prev_loc_only:=false; result) *)
(* end; *)
let get_prev_loc strm =
  match XStream.get_last strm with
  [Some (_,{cur_loc=l;_}) -> l
  |None -> begin
      FanLoc.ghost end];
      
let is_level_labelled n = fun [ {lname=Some n1; _  } ->  n = n1 | _ -> false ];
  
let warning_verbose = ref true;
  
let get_terminals x =
  let rec aux tokl last_tok  = fun 
    [ Node {node = (#terminal as tok); son; brother = DeadEnd}
      ->  aux [last_tok :: tokl] tok son
    | tree ->
        if tokl = [] then None (* FIXME?*)
        else Some (List.rev [last_tok :: tokl], last_tok, tree) ] in
  match x with
  [{node=(#terminal as x);son;_} ->
    (* first case we don't require anything on [brother] *)
     (aux [] x son)
  | _ -> None ];

let eq_Stoken_ids s1 s2 =
  match (s1,s2) with
  [ ((`Antiquot,_),_) -> false
  | (_,(`Antiquot,_)) -> false
  | ((_,s1),(_,s2)) -> s1 = s2];

let logically_eq_symbols entry =
  let rec eq_symbol s1 s2 =
    match (s1, s2) with
    [ (`Snterm e1, `Snterm e2) -> e1.ename = e2.ename
    | (`Snterm e1, `Sself) -> e1.ename = entry.ename
    | (`Sself, `Snterm e2) -> entry.ename = e2.ename
    | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1.ename = e2.ename && l1 = l2
    | (`Slist0 s1, `Slist0 s2) |
      (`Slist1 s1, `Slist1 s2) |
      (`Sopt s1, `Sopt s2) |
      (`Speek s1, `Speek s2) |
      (`Stry s1, `Stry s2) -> eq_symbol s1 s2
    | (`Slist0sep (s1, sep1), `Slist0sep (s2, sep2)) |
      (`Slist1sep (s1, sep1), `Slist1sep (s2, sep2)) ->
        eq_symbol s1 s2 && eq_symbol sep1 sep2
    | (`Stree t1, `Stree t2) -> eq_tree t1 t2
    | (`Stoken (_, s1), `Stoken (_, s2)) -> eq_Stoken_ids s1 s2
    | _ -> s1 = s2 ]
  and eq_tree t1 t2 = match (t1, t2) with
    [ (Node n1, Node n2) ->
      eq_symbol n1.node n2.node && eq_tree n1.son n2.son &&  eq_tree n1.brother n2.brother
    | (LocAct (_, _) | DeadEnd, LocAct (_, _) | DeadEnd) -> true
    | _ -> false ] in eq_symbol;

let rec eq_symbol s1 s2 =
  match (s1, s2) with
  [ (`Snterm e1, `Snterm e2) -> e1 == e2
  | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1 == e2 && l1 = l2
  | (`Slist0 s1, `Slist0 s2) |
    (`Slist1 s1, `Slist1 s2) |
    (`Sopt s1, `Sopt s2) |
    (`Speek s1, `Speek s2) |
    (`Stry s1, `Stry s2) -> eq_symbol s1 s2
  | (`Slist0sep (s1, sep1), `Slist0sep (s2, sep2)) |
    (`Slist1sep (s1, sep1), `Slist1sep (s2, sep2)) ->
      eq_symbol s1 s2 && eq_symbol sep1 sep2
  | (`Stree _, `Stree _) -> false
  | (`Stoken (_, s1), `Stoken (_, s2)) -> eq_Stoken_ids s1 s2
  | _ -> s1 = s2 ];
    

(* let rec get_first = fun *)
(*   [ Node {node;brother} -> [node::get_first brother] *)
(*   | _ -> [] ]; *)
    
