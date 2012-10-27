



type u = v:(int->int) -> int
    
type u = ?v:(int->int) -> int
    
type u = ?v:int option list -> int
    
type u = ?v:int option  -> int
    
type u = (int -> (int -> int) -> int)-> int ->int -> int
    
type u  = int option
      
type u = (int,bool) option list (* type u = int (option  , list  ) *)

type u = ?v:int -> ?l:int -> m:int -> string

type u = (a*b) -> ?v:int -> ?l:int -> m:int -> string


type 'a u = [< `a | `b] as 'a 
type 'a u = [< `a | `b > `a] as 'a
type u = [`a | `b]


type 'a u = [< `a | `b > `a `b]

type 'a c =  < draw:int; .. >  as 'a

type 'a u = (#a as 'a)    



type 'c u = ('a,'b)#a as 'c     


let f ~v:v0 ~u = u + v0;;
let f ~v:(v0:int) y = v0 + y;;


type ('a,'b,'c) u  = ('a,'b)#f [> `c `a] as 'c;;
(* empty should not print*)
 

let sum (lst : _ #iterator) = lst#fold (fun x y -> x+y) 0    
    
let f y ?(x=3) z = x + y + z 
let f ~y:y0 = 3
let f ?x:x0 y = x+y0

let f ?x:(Some x0) y = x0+y;;

let f ?x:x y = match x with Some x -> x +y0;;

let f ?x y = match x with Some x -> x +y|None -> 0;;

let f x = function
  | Some y -> 1
  | None -> 0 

let a f = f

let f = fun (Some x) -> x

let u = function
  | Some x when x > 0 -> x
  | None -> 3
;;


let g () =
  let f = fun (Some x) when x > 0 -> x in f ;;
let f = fun (Some x ) when x > 0 -> x in f ;;

let u = [1;2;3;4;4];;
let f x xs ys =
  (x+y)::xs::ys;;
    
type u = [ `a of int * bool | `b of bool ]
type u = { f : int; g : bool; }
let fg = function {f;_} -> f;;


let f = function
  |x::xs -> x
  | [] -> 0;;



let _ = begin
  (!a, !a.b, !(a.b))
end


external f : int -> int = "hah"
    





type ('a,'b,'c)u = ('a,'b) #f as 'c    



module type S = sig
  type u 
end

module  X (U:S) = struct end

module rec X:sig
  end = struct
  end
and Y:sig end = struct
end
module type S = sig 
  module rec X:sig end
  and Y:sig end
end





    
include Ast
external loc_of_ctyp :
  (ctyp -> FanLoc.t)  = "%field0"
external loc_of_patt :
    (patt -> FanLoc.t)  = "%field0"
external loc_of_expr :
    (expr -> FanLoc.t)  = "%field0"
external loc_of_module_type :
    (module_type -> FanLoc.t)  = "%field0"
external loc_of_module_expr :
    (module_expr -> FanLoc.t)  = "%field0"

let f = function
  | 'a'..'z' -> 1
  | _ -> 2
let rec f : 'a 'b . 'a list = []

module type S = sig
  module U : S with type u = x and type ('a,'b) m = ('a,'b)x  and type 'c h = M.h
end

let _ = begin
  print_int 4;
  print_int 10;
end
    
let _ = begin
  begin
    print_int 3;
    print_int 2;
  end
    begin
      print_int 3;
      print_int 2;
    end
end
and g f =
  g ; g
let f ()  =
  let g = a
  and g =  3 in g ;g

;;
let _ = begin
  ignore (a.[0],b.(1));
  a.[0] <- 3;
  b.(1) <- 4; 
end

let _ = object
  method x: string -> string = print_int 3 
end

(* let  mk_set = fun (type s) -> *)
(*   fun ~cmp -> *)
(*     let module M = struct  type t =   s   let  compare = cmp end in *)
(*       ((module (Set.Make) (M)) :(module Set.S with type elt =  s ) ) *)

let mk_set (type s) ~cmp =
  let module M = struct type t = s let compare = cmp end in
  (module Set.Make(M) :Set.S with type elt = s)
let mk s (type s) (type u)  = s ;;
let mk (type s) s (type u)  = s ;;

