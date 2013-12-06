

(** Ast processing library with minimial dependency while not
    complex type signature
 *)  


(** connecting a list of nodes from right to the left
  It assumes that the associativity is right.

  For example,
  {[
   com_of_list [ `Int(_loc,2 ); `Int (_loc,3); `Int (_loc,4) ]
   `Com (, `Int (, 2), `Com (, `Int (, 3), `Int (, 4)))
   ]}
  
  Note that [com_of_list] is making use of [of_listr]

  *)
let rec of_listr f xs =
  match xs with
  | [] -> invalid_arg "of_listr empty"
  | [t] -> t
  | t :: ts -> f t (of_listr f ts)

(**
   connecting a list of nodes from left to right.
   It assumes that the associativity is to the left.
   {[
   Ast_gen.appl_of_list [`Int(_loc,2); `Int (_loc,3); `Int (_loc,4) ];;
   `App (, `App (, `Int (, 2), `Int (, 3)), `Int (, 4))
   ]}
 *)
let rec of_listl f xs =
  match xs with
  | [] -> invalid_arg "of_listl empty"
  | [t] -> t
  | x::y::xs -> of_listl f (f x y :: xs)

  
                                    
(** collapse the intemediate nodes, it does not care about the associativity
 *)
let rec list_of a acc =
  match a with
  | `And(_,x,y)|`Com(_,x,y)|`Sta(_,x,y)|`Bar(_,x,y)
  | `Sem(_,x,y)|`Dot(_,x,y)|`App(_,x,y) ->
      list_of x (list_of y acc)
  | _ -> a :: acc

let rec list_of_and x acc =
  match x with
  |`And(_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x::acc

(* Inline [list_of_and] and [fold_right] *)              
let rec fold_and_right f x acc  =
  match x with
  | `And(_,x,y) -> fold_and_right f x (fold_and_right f y acc)
  | e -> f e acc

        
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
        
(**
  (t1 -> (t2 -> t3)) =>
  [t1::t2::t3::acc]
 *)    
let rec listr_of_arrow x acc =
  match x with
  |`Arrow(_,t1,t2) -> listr_of_arrow t1 (listr_of_arrow t2 acc)
  | x -> x::acc

(** destruct app
    for example
    {[
    view_app {| f a b c d|}
    
    ]}
 *)
let rec view_app acc = function
  |`App (_,f,a) -> view_app (a::acc) f
  | f -> (f,acc)


module N = struct
  let rec list_of_and x acc =
    match x with
    |`And(x,y) -> list_of_and x (list_of_and y acc)
    | _ -> x::acc

  let rec list_of_com x acc =
    match x with
    |`Com(x,y) -> list_of_com x (list_of_com y acc)
    | _ -> x::acc
                
  let rec list_of_star x acc =
    match x with
    | `Sta(x,y) -> list_of_star x (list_of_star y acc)
    | _ -> x::acc

  let rec list_of_bar x acc =
    match x with
    |`Bar(x,y) -> list_of_bar x (list_of_bar y acc)
    | _ -> x::acc

  let rec list_of_or x acc =
    match x with
    |`Bar(x,y) -> list_of_or x (list_of_or y acc)
    | _ -> x::acc

                
  let rec list_of_sem x acc =
    match x with
    |`Sem(x,y) -> list_of_sem x (list_of_sem y acc)
    | _ -> x::acc

  let rec list_of_dot x acc =
    match x with
    |`Dot(x,y) -> list_of_dot x (list_of_dot y acc)
    |x -> x::acc

  let rec list_of_app  x acc =
    match x with
    |`App(t1,t2) -> list_of_app t1 (list_of_app t2 acc)
    |x -> x :: acc


  let rec listr_of_arrow x acc =
    match x with
    |`Arrow(t1,t2) -> listr_of_arrow t1 (listr_of_arrow t2 acc)
    | x -> x::acc

  let rec view_app acc = function
    |`App (f,a) -> view_app (a::acc) f
    | f -> (f,acc)

          
          
end
    
(* local variables: *)
(* compile-command: "pmake ast_basic.cmo" *)
(* end: *)
