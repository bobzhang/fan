
open Ast;
#default_quotation "expr";;
class primitive =  object
  method int _loc (i:int) : ep =  {|$`int:i|};
  method int32 _loc (i:int32) : ep = {|$`int32:i|};
  method int64 _loc  (i:int64) : ep = {|$`int64:i|};
  method nativeint _loc (i:nativeint) : ep = {|$`nativeint:i|};
  method float _loc (i:float) : ep= {|$`flo:i|};
  method string _loc (i:string): ep = {|$`str:i|};
  method char _loc (i:char) : ep = {|$`chr:i|};
  method unit _loc (_:unit) : ep = {|()|};
  (* FIXME bool antiquot *)
  method bool _loc x  : ep=
    match x with
    [true -> {|true|} | false -> {| false |} ];
end;

{:fans|keep off; derive (MetaObj); |};

{:ocaml|{:include| "src/Ast.ml"|}; |};
  
