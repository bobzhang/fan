open Format;
#load "fan_camlp4_generate_origin.cma";
open Fan_camlp4_generate_origin;
value p_expr = fprintf fmt   "@[<hov 1>%a@]" Print.pp_print_expr ;

( << 3 + 4 + 4 + 2 + ( 32  * 4 ) |> 3 >> |> p_expr );

















