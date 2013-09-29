{:extend|Gram
  expr :
  ["sum"; LIST0 [ `INT(x,_) -> x ]{xs}; "end" ->
    let result = List.fold_left (+) 0 xs  in
    {:expr| $`int:result |} ] |};
