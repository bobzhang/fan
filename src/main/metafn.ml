
open Astfn


%fans{keep off;
      derive
        (MetaObj
(* OEq OPrint *)(* MetaExpr *));};;


class primitive =  object
  method int _loc (i:int)  = %ep{$int':i}
  method int32 _loc (i:int32) = %ep{$int32':i}
  method int64 _loc  (i:int64) = %ep{$int64':i}
  method nativeint _loc (i:nativeint) = %ep{$nativeint':i}
  method float _loc (i:float) = %ep{$flo':i}
  method string _loc (i:string) = %ep{$str':i}
  method char _loc (i:char) = %ep{$chr':i}
  method unit _loc (_:unit) = (`Unit _loc : Astf.ep) 
  (*default use [meta_loc] for expession*)   
  (* method loc _loc (_l:loc) : ep= `Lid (_loc, !Locf.name) *)
  method ant (_loc:loc) (x:ant)  = (x:> Astf.ep)
  method bool _loc x  = (`Bool (_loc,x) : Astf.ep)
end;;

(* FIXME -- nested include the error message is confusing *)
%ocaml{ %include{ "astfn.ml" };; };;



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/metafn.cmo" *)
(* end: *)

