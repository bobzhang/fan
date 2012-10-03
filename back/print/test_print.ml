open Format
  
type op = Plus | Times
type expr = Op of op * expr list | I of int
let v = Op (Plus,
            [I 3;
             Op (Times,
                 [I 5; I 7; Op (Plus, [I 4; I 2])]);
             I 2])

let op2s = function
  | Plus -> '+'
  | Times -> '*'

let rec pp_list sep pp fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" pp x
  | x::xs -> fprintf fmt "%a%c%a" pp x sep (pp_list sep pp) xs

let rec y f x = f (y f) x

let precedence = function 
  | Op (Plus, _::_::_) -> 1
  | Op (Times, _::_::_) -> 2
  | _ -> 0

let pp' pp fmt = function
  | Op (o, es) -> pp_list (op2s o) pp fmt es
  | I x -> fprintf fmt "%d@," x

let paren f f_par fmt (x, up) =
  let down = precedence x in
  let lp, rp = 
    if down <= up &&
      up <> 0 && down <> 0 then ("(", ")") else ("","") in
  let f' fmt' x' = f_par fmt' (x', down) in
  fprintf fmt "@[%s%a%s@]@," lp (f f') x rp

let pp_par = y (paren pp')

let pp ppf x = pp_par ppf (x, 0)

let _ = pp std_formatter v     
(*  
    let rec y1 f fmt x =
    fprintf fmt "@[(%a)@]@," (f (y1 f)) x
    let pp1 = y1 pp
    let _ = pp1 std_formatter v

    let rec y2 up f fmt x =
    let down = precedence x in
    let lp, rp = 
    if down <= up && up <> 0 && down <> 0 then ("(", ")") else ("","") in
    fprintf fmt "@[%s%a%s@]@," lp (f (y2 down f)) x rp

    let pp2 = y2 0 pp

    let _ = pp2 std_formatter v

 *)
    
(*
  let rec pp fmt = function
  | Op (o, es) ->
  fprintf fmt "@[(%a)@]@," (pp_list (op2s o) pp) es
  | I x -> fprintf fmt "%d@," x
 *)
(*
let pp r fmt = function
  | Op (o, es) -> pp_list (op2s o) r fmt es
  | I x -> fprintf fmt "%d" x
*)
