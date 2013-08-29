
let rec list_of_and x acc =
  match x with
  |`And(_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x::acc

let rec list_of_com x acc =
  match x with
  |`Com(_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x::acc
    
let rec list_of_star x acc =
  match x with
  | `Sta(_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x::acc

let rec list_of_bar x acc =
  match x with
  |`Bar(_,x,y) -> list_of_bar x (list_of_bar y acc)
  | _ -> x::acc

let rec list_of_or x acc =
  match x with
  |`Bar(_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x::acc

    
let rec list_of_sem x acc =
  match x with
  |`Sem(_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x::acc

let rec list_of_dot x acc =
  match x with
  |`Dot(_,x,y) -> list_of_dot x (list_of_dot y acc)
  |x -> x::acc

let rec list_of_app  x acc =
  match x with
  |`App(_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  |x -> x :: acc


(*
  t1 -> t2 -> t3 =>
  [t1::t2::t3::acc]
 *)    
let rec list_of_arrow_r x acc =
  match x with
  |`Arrow(_,t1,t2) -> list_of_arrow_r t1 (list_of_arrow_r t2 acc)
  | x -> x::acc

(*************************************************************************)
(*************************************************************************)
  
let rec view_app acc = function
  |`App (_,f,a) -> view_app (a::acc) f
  | f -> (f,acc)
        
