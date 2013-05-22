
open FAstN

(** this is caused by #ant problem, which requires [fill_loc_ant] to be
    specifiied *)
let fill_loc_ant _loc x = x;;
{:fans|keep off; derive (  Fill MetaObj(* OEq OPrint *)(* MetaExpr *));|};;


class primitive =  object
  method int _loc (i:int)  =  {:ep|$`int:i|}
  method int32 _loc (i:int32) = {:ep|$`int32:i|}
  method int64 _loc  (i:int64) = {:ep|$`int64:i|}
  method nativeint _loc (i:nativeint) = {:ep|$`nativeint:i|}
  method float _loc (i:float) = {:ep|$`flo:i|}
  method string _loc (i:string) = {:ep|$`str:i|}
  method char _loc (i:char) = {:ep|$`chr:i|}
  method unit _loc (_:unit) = {:ep|()|}
  (*default use [meta_loc] for expession*)   
  (* method loc _loc (_l:loc) : ep= `Lid (_loc, !FanLoc.name) *)
  method ant (_loc:loc) (x:ant)  = (x:> FAst.ep)
  (* FIXME bool antiquot *)
  method bool _loc x  =
    match x with
    |true -> {:ep|true|}
    | false -> {:ep| false |} 
end;;

{:ocaml| {:include| "src/fAstN.ml" |} |};;















