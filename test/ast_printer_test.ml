



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

type 'a ab = [< `a|`b] as 'a 
type 'a ac = 'a constraint 'a = [< `a | `c ]
type ('a,'b) m = [< `m of 'a ab & 'a ac ] as 'b

type 'a u = [< `a | `b > `a `b]

type 'a c =  < draw:int; .. >  as 'a

class a = object end
type 'a u = (#a as 'a)    

class type a = object method v : int end

type 'c u = ('a,'b)#a as 'c     

module type S = sig 
  class ['a, 'b] f : 'a -> 'b -> object method x : 'a method y : 'b end
end
module type S = sig
  class ['a, 'b] f : f:'a -> ?g:'b -> object method x : 'a method y : 'b end
end;;      

class ['a,'b] f (v:'a) (u:'b) = object method x = v method y = u end

type ('a,'b,'c)u = ('a,'b) #f as 'c    

let f ~v:v0 ~u = u + v0;;

let f ~v:(v0:int) y = v0 + y;;

class ['a,'b] f ~v:(v:'a) ~u:(u:'b) = object method x = v method y = u end;;
(* class ['a, 'b] f : v:'a -> u:'b -> object method x : 'a method y : 'b end;; *)

type ('a,'b,'c) u  = ('a,'b)#f [> `c `a] as 'c;;
(* empty should not print*)
 
class ['a] circle (c : 'a) = object
  constraint 'a = #point
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = center#move
end

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
and g f =
  g ; g

let f ()  =
  let g = a
  and g =  3 in g ;g

;;
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
  ignore (a.[0],b.(1));
  a.[0] <- 3;
  b.(1) <- 4; 
end


let _ = begin
  (!a, !a.b, !(a.b))
end

let f x = object
    method x = print_int x 
end

    
