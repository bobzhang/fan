

let make_infix ?(left=true) exp f i =
  %extend{
  exp: ${f i} $bool:left
  [ S  as e1 ; Inf@xloc ($i,op); S as e2 %{
    let op = %exp@xloc{$lid:op} in %exp{$op $e1 $e2}}]}

let make_key ?(left=true) exp i op =
  %extend{
  exp: $i $bool:left
  [ S as e1 ; $key:op @xloc; S as e2 %{
    let op = %exp@xloc{$lid:op} in %exp{$op $e1 $e2}}]  
}
    
(* let simple = Gramf.mk "simple" in *)
(* begin  *)
(*   List.iter (make_op f simple) [1;2;3;4]; *)
(*   List.iter (make_key exp 80) ["+";"-" ;"-."]; *)
(*   List.iter (make_key exp 70) ["::"]; *)
(*   List.iter (make_key exp 50) ["==";"=";"<";">"]; *)
(*   List.iter (make_key exp 40) ["&";"&&"]; *)
(*   List.iter (make_key exp 30) ["or"; "||"]; *)
(*   List.iter (make_key exp 20) [":="] *)
(* end *)


(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../treeparser -I ../../cold -pp ../../fan -c a8.ml" *)
(* end: *)
