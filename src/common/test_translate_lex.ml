
#load "./_build/common/translate_lex.cma";;
#directory "_build/common";;
open Translate_lex;;
let print_cset fmt set =
  Format.fprintf fmt "%s" @@ Fcset.to_string set;;

#install_printer print_cset;; 
regexp_for_string "abcd"  
