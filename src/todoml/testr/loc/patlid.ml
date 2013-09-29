


let u : Ast.patt -> string= with patt
  fun [{|$(lid:x)|} -> x |_ -> "x"];
