

open Astf

class primitive =  object
  method int _loc (i:int) : ep =  %ep{$int':i}
  method int32 _loc (i:int32) : ep = %ep{$int32':i}
  method int64 _loc  (i:int64) : ep = %ep{$int64':i}
  method nativeint _loc (i:nativeint) : ep = %ep{$nativeint':i}
  method float _loc (i:float) : ep= %ep{$flo':i}
  method string _loc (i:string): ep = %ep{$str':i}
  method char _loc (i:char) : ep = %ep{$chr':i}
  method unit _loc (_:unit) : ep = (`Unit _loc : ep)
  (*default use [meta_loc] for expession*)   
  method loc _loc (_l:loc) : ep=
    let n  = !Locf.name in %ep{$lid:n}
  method ant (_loc:loc) (x:ant) : ep = (x:>ep)
  (* FIXME bool antiquot *)
  method bool _loc x  : ep= `Bool (_loc, x ) (* BOOTSTRAPING *)
  (* method unknown (_loc:loc) : ! 'a . 'a -> ep  = assert false;
     method unknown (_loc : loc) = (assert false : 'a . 'a -> ep )
     a bug to be FIXED
   *)
end;;


%fans{keep off; derive (MetaObj); };;
%ocaml{%include{ "astf.ml"};; };;

  

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/metaf.cmo" *)
(* end: *)
