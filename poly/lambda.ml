(* open Format *)
type var = [`Var of string]  
type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a ]
let gensym =
  let n = ref 0 in
  fun () -> incr n ; "_" ^ string_of_int !n

let eval_var subst (`Var s as v:var) =
  try List.assoc s subst with Not_found -> v 
let eval1 eval subst =  function
  | #var as v -> eval_var subst v
  | `App(l1,l2) ->
      let l2' = eval subst l2 in begin
        match eval subst l1 with
        | `Abs (s,body) ->
            eval [s,l2'] body
        | l1' ->
            `App(l1',l2')
      end
  | `Abs(s,l1) ->
      let s'= gensym () in
      `Abs(s',eval ((s,`Var s')::subst) l1)

type 'a expr = [`Var of string | `Add of 'a * 'a | `Mult of 'a * 'a | `Num of int]


let map_expr (f:_->'a) : 'a expr -> 'a  = function
  | #var as v -> v
  | `Num _ as n -> n
  | `Add(e1,e2) -> `Add (f e1, f e2)
  | `Mult (e1,e2) -> `Mult(f e1, f e2)


let eval2 eval subst (e : 'a expr) : 'a = match map_expr (eval subst) e with
| #var as v -> eval_var subst v
| `Add(`Num (m:int), `Num (n:int)) -> `Num (m+n)
| `Mult(`Num (m:int),`Num (n:int)) -> `Num (m*n)
| e -> e

type 'a lexpr = ['a lambda | 'a expr]

let eval3 eval subst : 'a lexpr -> 'a = function
  | #lambda as x -> eval1 eval subst x
  | #expr as x -> eval2 eval subst x 
      



















