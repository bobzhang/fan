(* open Format; *)
(* open List ; *)

let _loc = FanLoc.ghost ;
let rec member v lst = with "expr"
  match lst with
  [ {|[]|} -> {|false|}
  | {| [ $x :: $xs] |} ->
      {| ($v = $x) || $(member v xs)  |}
  ];  

(*
  list expr
  expr list

  lift

  code -> data
    how to get list int  
  data -> code

  $(member  x [1;2;3;4;5])

  how to turn the ast into code ?
 *)        

with "expr"
  member {|x|} {|[1;2;3;4]|};    
