
<:fan<
lang "ocaml";
keep on;      (* don't keep the type definitions *)
show_code off; (* don't print the derived code *)
plugins_add "Print", "Eq",  "Map", "Map2",
"Fold", "Fold2", "OPrint", "MetaExpr", "MetaPatt";
>> ;;
open Camlp4.PreCast
open Common_base
open Meta_base
open Lib_common
module Loc = struct
  include Loc
  let pp_print_t fmt v = ()
  let eq_t a b = false
end 
<<
type ml_unop =
  | Ml_fst
  | Ml_snd
and ml_binop =
  | Ml_add
  | Ml_sub
  | Ml_mult
  | Ml_eq
  | Ml_less
  | Ml_gt
and ml_exp =
  | Ml_int_const of  int
  | Ml_bool_const of bool
  | Ml_pair of ml_exp * ml_exp
  | Ml_unop of ml_unop * ml_exp 
  | Ml_binop of ml_binop * ml_exp * ml_exp
  | Ml_var of string
  | Ml_if of ml_exp * ml_exp * ml_exp
  | Ml_fun of ml_patt * ml_exp
  | Ml_app of ml_exp * ml_exp
  | Ml_let of ml_patt * ml_exp * ml_exp
  | Ml_letrec of ml_patt * ml_exp * ml_exp
  | Ml_Ant of Loc.t * string  (** meta filter special treatment *)
and ml_type =
  | Int_type
  | Bool_type
  | Pair_type of ml_type * ml_type
  | Arrow_type of ml_type * ml_type
  | Var_type of string
  | Type_Ant of Loc.t * string
and ml_patt =
  | Ml_pat_id of string
  | Ml_patAnt of Loc.t * string 
>> ;;












