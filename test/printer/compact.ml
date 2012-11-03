let compact_tables state_v =
  for i = 0 to n - 1 do
    match state_v.(i) with
    | Perform (n,c) ->
        base.(i) <- -(n+1) ;
        base_code.(i) <- emit_tag_code c
    | Shift(trans, move) ->
        begin match trans with
        | No_remember -> ()
        | Remember (n,c) ->
            backtrk.(i) <- n ;
            backtrk_code.(i) <- emit_tag_code c
        end;
        let (b_trans, d_trans),(b_moves,d_moves) = pack_moves i move in
        base.(i) <- b_trans; default.(i) <- d_trans ;
        base_code.(i) <- b_moves; default_code.(i) <- d_moves ;
  done;
  reset_compact trans ;
  reset_compact moves ;
  tables
