let define x =
  {:extend| Gram
    expr: Level "apply"
    [ `UID (y,$x,z); S{param} -> y] |};
  
