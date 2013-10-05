
type 'a t  = 'a -> exn

let return label v =
  raise (label v)

let label (type u) (f : u t ->  u) : u =
  let module M = struct exception Return of u end in
  try f (fun x -> M.Return x)
  with  M.Return u -> u

let with_label = label


    

(* local variables: *)
(* compile-command: "pmake return.cmo" *)
(* end: *)
