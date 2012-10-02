open Format
module String_map = Map.Make(String)
module String_set = Set.Make(String)
open Lexing
open Parse_tree
open Format
open Dyp


type string_position = string * position
type dyp_position = int * int * int * string
type dyp_production  =
    (literal_desc *
       (string * pat_type * position)) list
type dyp_lhs =
    (string * (string * pat_type * position) list) 

type dyp_rhs =
    (literal_desc *
       (string * pat_type * position)) list
type dyp_action_code = string       
module type Params = sig
  val parser_param_info: parser_param_info
  val lexer:
      ((string * Dyp.regexp) list *
         (string list *
            (Dyp.regexp * (string * (position * bool))) list) list
         * (Dyp.regexp * string * code_desc) list) option
  val grammar:
      (dyp_lhs
         * (string * dyp_position)
         * dyp_production
         * (dyp_action_code *
              (position * bool))
         * (bool * bool))  list
end
  



















