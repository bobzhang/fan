
open Genbase
<:fan< lang "fan" ; >>;;
module Camlp4Types = struct
  << types_of_mli "test_types.mli" ;  >> 
end 

module  Test_types = struct 
  include Test_types;;
  << gen
    ( |<- "Print")
    ( |<- "Eq")
    ( |<- "Map")
    ( |<- "Map2")
    ( |<- "Fold")
    ( |<- "Fold2")
    ( |<- "OPrint");>>  ;;  
end 
