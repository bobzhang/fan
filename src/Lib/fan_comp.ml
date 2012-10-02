open Format;

#directory "+compiler-libs";
#load "ocamlcommon.cma";
#load "fan_common.cmo";
#load "fan_ocaml_asttypes_o.cma";
#load "fan_ocaml_parsetree_o.cma";

module Parsetree = Fan_ocaml_parsetree_o.Parsetree;
value asttypes= new Fan_ocaml_asttypes_o.Asttypes.print;
    
value p = object
  inherit Parsetree.print;
  method! asttypes_loc mf_a fmt _ = fprintf fmt "<loc>";
  method! asttypes_constant fmt v = asttypes#constant fmt v;
end ;
  
value structure_of_string s =
  s
  |> Lexing.from_string
  |> Parse.implementation
  |> printf "@[%a@]@."
     p#structure;
    
structure_of_string "let a = 3";


















