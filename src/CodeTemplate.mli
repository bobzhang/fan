open Ast

type ty_meta = {
  str : string;
  print : [ `Custom of str_item | `Exist | `Fmt of string ];
  eq : [ `Custom of str_item | `Def ];
}
val base1_types :
  (string * [> `Custom of str_item | `Exist | `Fmt of string ] *
   [> `Custom of str_item | `Def ])
  list
val ty_metas : ty_meta list

val map_class_str_item_base_1 : class_str_item
val map_class_str_item_base_2 : class_str_item
val fold_class_str_item_base_1 : class_str_item
val fold_class_str_item_base_2 : class_str_item
val print_class_str_item_base : class_str_item
val eq_base1 : str_item
val print_base1 : str_item
