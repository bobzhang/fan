


(*
  
  *)
let rec fa al =fun
  [ <:ctyp< $f $a >>  -> fa [a :: al] f
  | f -> (f, al) ];

let rec to_var_list =  fun
  [ <:ctyp< $t1 $t2 >> ->
    to_var_list t1 @ to_var_list t2
  | <:ctyp< '$s >> -> [s]
  | _ -> assert False ];
