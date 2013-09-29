
type u = (int * float * string)
      
type a = string


type b =
    [ `Atr of (string * a )]


type c = [  `a of int ]


type d = (b * float*c)


type e = A of int * a | C of float 


type f =float


type g  = e  = A of int * a | C of float 


type h = [ `a | `b]


type i =  B of g | E of int 



type 'a phan   = ('a * int)



type 'a m_list  =
   M_nil
  | M_cons of 'a * 'a m_list  


type 'a test  =
  T0 of (int * float * 'a)


type 'a tree   =
   Leaf
  | Node of 'a tree m_list  *  'a 



type 'a j  = { mutable u:  int  ;
             v : int m_list  ; x: 'a phan  }


type k = i tree j


(*
type m
;

type n = []
;



value a :e = A 2 "xx"
;

*)
