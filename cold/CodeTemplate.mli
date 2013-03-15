open Ast

type ty_meta = {
  str : string;
  print : [ `Custom of stru | `Exist | `Fmt of string ];
  eq : [ `Custom of stru | `Def ];
}
val base1_types :
  (string * [> `Custom of stru | `Exist | `Fmt of string ] *
   [> `Custom of stru | `Def ])
  list
val ty_metas : ty_meta list

val map_cstru_base_1 : cstru
val map_cstru_base_2 : cstru
val fold_cstru_base_1 : cstru
val fold_cstru_base_2 : cstru
val print_cstru_base : cstru
val eq_base1 : stru
val print_base1 : stru
