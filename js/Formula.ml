
type formula =
  | Var of string
  | Not of formula
  | Or of formula * formula
  | And of formula * formula

let rec str = function
  | Var s -> s
  | Not f -> "(not " ^ (str f) ^ ")"
  | Or (f, g) -> "(" ^ (str f) ^ " or " ^ (str g) ^ ")"
  | And (f, g) -> "(" ^ (str f) ^ " and " ^ (str g) ^ ")"
    
let rec nnf ?(negate=false) = function
  | Var s -> if not negate then Var s else Not (Var s)
  | Not f -> nnf ~negate:(not negate) f
  | Or (f, g) -> if not negate then Or (nnf f, nnf g) else
      And (nnf ~negate:true f, nnf ~negate:true g)
  | And (f, g) -> if not negate then And (nnf f, nnf g) else
      Or (nnf ~negate:true f, nnf ~negate:true g)

let _ =
  print_endline (str (nnf (Not (And (Var "X", Not (Var "Y"))))))
    



















