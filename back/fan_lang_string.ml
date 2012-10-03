open Format;
open Camlp4.PreCast;
open Syntax;
open Fan_basic;
open Lib_common;
open Fan_config;
open Fan_dynamic_plugins;

module MGram = MakeGram Lexer;
module M = Make MGram;
value fan_string = MGram.Entry.mk "fan_string";


EXTEND MGram GLOBAL: fan_string:
  fan_string:
  ["top"
    [anti=ANTIQUOT("",s) ->
      
   ]  
   ];
END;

let open M in begin
  add_quotation_of_str_item ~name:"str" ~entry:main;
end 
