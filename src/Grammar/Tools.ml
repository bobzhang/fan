open LibUtil;
open Structure;

let get_prev_loc_only = ref False;
module Make (U:sig end) = struct
  let empty_entry ename _ =
      raise (Stream.Error ("entry [" ^ ename ^ "] is empty"));
  
  let keep_prev_loc strm =  match Stream.peek strm with
    [ None -> [< >]
    | Some (tok0,init_loc) ->
        let rec go prev_loc strm1 =
          if !get_prev_loc_only then
            [< (tok0,{prev_loc;cur_loc=prev_loc;prev_loc_only=True});
               'go prev_loc strm1 >]
          else match strm1 with parser
            [ [< (tok,cur_loc); 'strm >] ->
              [< (tok, {prev_loc; cur_loc; prev_loc_only = False});
                 'go cur_loc strm >]
            | [< >] -> [< >] ] in
        go init_loc strm ];
  
  let drop_prev_loc strm = Stream.map (fun (tok,r) -> (tok,r.cur_loc)) strm;
  
  let get_cur_loc strm =  match Stream.peek strm with
    [ Some (_,r) -> r.cur_loc
    | None -> FanLoc.ghost ];
  
  let get_prev_loc strm = begin
    get_prev_loc_only:=True;
    let result = match Stream.peek strm with
      [ Some (_, {prev_loc; prev_loc_only = True; _}) ->
        begin Stream.junk strm; prev_loc end
      | Some (_, {prev_loc; prev_loc_only = False;_}) -> prev_loc
      | None -> FanLoc.ghost ] in
    (get_prev_loc_only:=False; result)
  end;
  
  let is_level_labelled n = fun [ {lname=Some n1; _  } ->  n = n1 | _ -> False ];
  
  let warning_verbose = ref True;
  
  let rec get_token_list entry tokl last_tok  = fun 
    [ Node {node = (`Stoken _ | `Skeyword _ as tok); son = son; brother = DeadEnd} ->
      get_token_list entry [last_tok :: tokl] tok son
    | tree ->
        if tokl = [] then None
        else Some (List.rev [last_tok :: tokl], last_tok, tree) ];
  
  
  let eq_Stoken_ids s1 s2 = match (s1,s2) with
    [ ((`Antiquot,_),_) -> False
    | (_,(`Antiquot,_)) -> False
    | ((_,s1),(_,s2)) -> s1 = s2];
  
  let logically_eq_symbols entry =
    let rec eq_symbols s1 s2 =  match (s1, s2) with
      [ (`Snterm e1, `Snterm e2) -> e1.ename = e2.ename
      | (`Snterm e1, `Sself) -> e1.ename = entry.ename
      | (`Sself, `Snterm e2) -> entry.ename = e2.ename
      | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1.ename = e2.ename && l1 = l2
      | (`Slist0 s1, `Slist0 s2) |
        (`Slist1 s1, `Slist1 s2) |
        (`Sopt s1, `Sopt s2) |
        (`Stry s1, `Stry s2) -> eq_symbols s1 s2
      | (`Slist0sep (s1, sep1), `Slist0sep (s2, sep2)) |
        (`Slist1sep (s1, sep1), `Slist1sep (s2, sep2)) ->
          eq_symbols s1 s2 && eq_symbols sep1 sep2
      | (`Stree t1, `Stree t2) -> eq_trees t1 t2
      | (`Stoken (_, s1), `Stoken (_, s2)) -> eq_Stoken_ids s1 s2
      | _ -> s1 = s2 ]
  and eq_trees t1 t2 = match (t1, t2) with
    [ (Node n1, Node n2) ->
      eq_symbols n1.node n2.node && eq_trees n1.son n2.son &&
      eq_trees n1.brother n2.brother
    | (LocAct _ _ | DeadEnd, LocAct _ _ | DeadEnd) -> True
    | _ -> False ] in eq_symbols;
  
  let rec eq_symbol s1 s2 =  match (s1, s2) with
      [ (`Snterm e1, `Snterm e2) -> e1 == e2
      | (`Snterml (e1, l1), `Snterml (e2, l2)) -> e1 == e2 && l1 = l2
      | (`Slist0 s1, `Slist0 s2) |
        (`Slist1 s1, `Slist1 s2) |
        (`Sopt s1, `Sopt s2) |
        (`Stry s1, `Stry s2) -> eq_symbol s1 s2
      | (`Slist0sep (s1, sep1), `Slist0sep (s2, sep2)) |
        (`Slist1sep (s1, sep1), `Slist1sep (s2, sep2)) ->
          eq_symbol s1 s2 && eq_symbol sep1 sep2
      | (`Stree _, `Stree _) -> False
      | (`Stoken (_, s1), `Stoken (_, s2)) -> eq_Stoken_ids s1 s2
      | _ -> s1 = s2 ];
  
  end;
