

let make_op exp f i =
  %extend{
  exp: $int'{f i}
  [ S  as x ; Inf@xloc ($infix:i,op); S as y %{
    lookup op x y} ]} in
let make_key exp pre k =
  %extend{
  exp: $int':pre
  [ S as x ; $key:k @xloc; S as y %{
    loop xloc k x y}]  
} in
let simple = Gramf.mk "simple" in
begin 
  List.iter (make_op f simple) [1;2;3;4];
  List.iter (make_key exp 80) ["+";"-" ;"-."];
  List.iter (make_key exp 70) ["::"];
  List.iter (make_key exp 50) ["==";"=";"<";">"];
  List.iter (make_key exp 40) ["&";"&&"];
  List.iter (make_key exp 30) ["or"; "||"];
  List.iter (make_key exp 20) [":="]
end
