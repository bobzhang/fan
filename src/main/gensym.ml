


let fresh =
  let cnt = ref 0 in
  fun ?(prefix = "_fan") () -> begin 
    incr cnt;
    Printf.sprintf "%s__%03i_" prefix !cnt
  end

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gensym.cmo" *)
(* end: *)
