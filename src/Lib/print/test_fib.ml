open Format

let rec y f x = f (y f) x

let fib' fib n =
  if n < 2 then
    n
  else
    fib (n - 1) + fib (n - 2)


    
let log f' f n =
  let () = printf "@[<v 2>@[<v 2>-->%d@," n in
  let res = f' f n in 
  let () = printf "@]@;<0 -2><--%d@,@]" res in
  res
    
let fib = y (log  fib')

let a = fib 8    




(* "@[<--%d@]" n *)














