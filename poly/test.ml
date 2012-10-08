open Format
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

let _ = print_int (fib 20 )       


















