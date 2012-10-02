open Format;
open Camlp4.PreCast;
open Syntax;
open Fan_basic;
open Lib_common;
open Fan_config;
open Fan_dynamic_plugins;


(* every language should make their module,
   otherwise there will be potential conflict for the lexer
 *)
module MGram = MakeGram Lexer;
module M = Make MGram;
value fan_include_ml = MGram.Entry.mk "fan_include_ml";
value main = MGram.Entry.mk "main";
EXTEND MGram GLOBAL: main fan_include_ml ;
    main:
    [ "top"
     [strs = LIST0 [x=fan_include_ml; ";" -> x ] ->
       <:str_item< .$list:strs$. >> 
     ]];
    fan_include_ml:
    [ "top"
        [ file=STRING  -> do{ 
        let p = Syntax.str_items in 
         try
           Gram.parse_file_with ~rule:p file
         with [ Loc.Exc_located(loc,e) -> begin
           prerr_endline (Loc.to_string loc);
           failwithf "include_ml %s parse error@."  file;
         end]}   ] ];
END;
  
let open M in begin 
  add_quotation_of_str_item ~name:"include_ml" ~entry:main;
end;
  
