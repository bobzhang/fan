open Format;
type u = int == private [A of int];

type u =  private  [A of int]

type u = [A of int ]

type u = private int
type u = v == private [A of int]



(*
  type v = private u

  type v = u = private A of int

  they have the same fields in

    ptype_manifest
    ptype_private

  different in
    ptype_kind
  
 *)      


















