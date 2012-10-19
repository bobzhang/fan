



type u = v:(int->int) -> int
    
type u = ?v:(int->int) -> int
    
type u = ?v:int option list -> int
    
type u = ?v:int option  -> int
    
type u = (int -> (int -> int) -> int)-> int ->int -> int
    
type u  = int option
      
type u = (int,bool) option list (* type u = int (option  , list  ) *)

type u = ?v:int -> ?l:int -> m:int -> string

type u = (a*b) -> ?v:int -> ?l:int -> m:int -> string

