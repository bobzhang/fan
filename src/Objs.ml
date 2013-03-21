open LibUtil;
open StdLib;
open Ast;


let strip_loc_list f lst =
  List.map f lst ;
let strip_loc_ant ant = ant ;
  
{:fans|keep off;
derive(Map2 Fold2 OIter Map Fold  OPrint OEq Strip Print);
|};

{:ocaml|{:include| "src/Ast.ml"|} ; |};


  
