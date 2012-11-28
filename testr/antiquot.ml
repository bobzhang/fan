open Fan.Syntax;
let define x =
  {:extend| Gram
    expr: Level "apply"
    [ `UID y; S{param} -> {:expr| 0 |}] |};

let u =
  3   + 5 +
    2 + 1
    + 3;
  
