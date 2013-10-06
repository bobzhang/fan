
type 'a t  = 'a -> exn

(* The outer raise is only to make type checker happy, since
   in general, [label v] would cause an exceptioin to be thrown *)    
let k label v =
  raise (label v)

let cc (type u) (f : u t ->  u) : u =
  let module M = struct exception Return of u end in
  try f (fun x -> M.Return x)
  with  M.Return u -> u




    

(* local variables: *)
(* compile-command: "pmake return.cmo" *)
(* end: *)
