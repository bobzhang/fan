
open Astf 


(* Ugly meta explosion, should be improved after  polishing  deriving *)  
class primitive = object
  method string _loc (i:string) :ep = %ep{$`str:i}
  method int _loc (i:int) : ep = %ep{$`int:i}
  method unknown (_ : Locf.t) (_ : Lexing.position): ep = assert false 
end

type position = Lexing.position =  {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} with ("MetaObj")

  

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/loc_meta.cmo" *)
(* end: *)
