
let fold_left ?(start = 0) ~until ~acc f =
  let v = ref acc in
  for x = start to until do
    v := f !v x
  done;
  !v

    

  

(* local variables: *)
(* compile-command: "pmake int.cmo" *)
(* end: *)
