open Parsetree;
open Asttypes;
open Longident;

class printer = object(self:'self)
    method longident _loc i = 
      with "ident" match i with
      [ Lident s -> {|$lid:s|}
      | Ldot(y,s) -> {| $(id:self#longident _loc y).$lid:s|}
      | Lapply(a,b) -> {| $(id:self#longident _loc a) $(id:self#longident _loc b) |}]  ;
    method constant_expr _loc i=
      with "expr" match i with 
      [Const_int32 i -> {|$`int32:i|}
      |Const_int i -> {|$`int:i|}
      |Const_int64 i -> {|$`int64:i|}
      |Const_float i -> {|$flo:i|}
      |Const_nativeint i -> {|$`nativeint:i|}
      |Const_char i -> {|$`chr:i|}
      |Const_string i -> {|$`str:i|} ];
end;




