
(** internal usage *)
open Astf

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

val map_clfield_base_1 : clfield

val map_clfield_base_2 : clfield

val fold_clfield_base_1 : clfield

val fold_clfield_base_2 : clfield

val print_clfield_base : clfield

val eq_base1 : stru

val print_base1 : stru
