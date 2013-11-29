
type t = { foo: int; bar: string } with compare
type t = { foo : int; bar : string }

let _ = fun (_ : t) -> ()
  
let compare : t -> t -> int =
  fun a__001_ b__002_ ->
    if Pervasives.( == ) a__001_ b__002_
    then 0
    else
      (let ret =
         (Pervasives.compare : int -> int -> int) a__001_.foo b__002_.foo
       in
         if Pervasives.( <> ) ret 0
         then ret
         else
           (Pervasives.compare : string -> string -> int) a__001_.bar
             b__002_.bar)
  
let _ = compare
  
let compare_t = compare
  
let _ = compare_t
  

