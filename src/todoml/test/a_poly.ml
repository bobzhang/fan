

type u =
    [ `a | `b | `c]

let f (x:u) =
  match x with
  | `aa -> 3
  | `bb -> 2
  | `cc -> 3
  | _ -> 1
