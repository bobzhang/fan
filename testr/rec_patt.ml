
let f = fun ?s () -> s;
let g ?s  =  f ?s ();
(* let g  ?(s=3) = 3 ;   *)
