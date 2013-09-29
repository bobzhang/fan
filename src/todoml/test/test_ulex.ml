#directory "_build/ulex";
#load "fan.cma";
#load "gen_ulex.cma";

<:fan< lang "regexps"; >>;
open Ulex;
open Gen_ulex;

(* code_of_t _loc [| <<  "utf8" >>; << "latin1" >> |] [||] |> opr#expr fmt ; *)

value p_expr = opr#expr ;
#install_printer p_expr;
value p_str_item = opr#str_item;
#install_printer p_str_item;
  
let (tables,partitions,code) =
  code_of_t _loc  <<
      "<utf8>"
    ; "<latin1>"
    ; xml_letter+
    ; eof
    ; [1234-1246]
    ;  "("
    ;  _
    >>
 [||] in
 opr#expr fmt code;
