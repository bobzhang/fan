
type u = (int * float * string)
;      
type a = string
;

type b =
    [ = `Atr of (string * a )]
;

type c = [=  `a of int ]
;

type d = (b * float*c)
;

type e = [A of int and  a | C of float ]
;

type f =float
;

type g  = e  == [A of int and a | C of float ]
;

type h = [= `a | `b]
;

type i = [ B of g | E of int ]
;


type phan 'a  = ('a * int)
;


type m_list 'a =
  [ M_nil
  | M_cons of 'a and m_list 'a ]
;

type test 'a =
  [T0 of (int * float * 'a)]
;

type tree 'a  =
  [ Leaf
  | Node of m_list (tree 'a) and  'a ]
;


type j 'a = {u:mutable int;
             v : m_list int ; x:phan 'a }
;

type k = j (tree i)
;

(*
type m
;

type n = []
;



value a :e = A 2 "xx"
;

*)
