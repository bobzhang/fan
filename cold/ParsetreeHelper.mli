

val with_loc : 'a -> Location.t -> 'a Location.loc
(* val ( +> ) : 'a -> Location.t -> 'a Location.loc *)
val lident : string -> Longident.t
val lident_with_loc : string -> Location.t -> Longident.t Location.loc
val ldot : Longident.t -> string -> Longident.t
val lapply : Longident.t -> Longident.t -> Longident.t
val mkghloc : FanLoc.t -> FanLoc.t
val error : FanLoc.t -> string -> 'a
val mksig :
  Location.t -> Parsetree.signature_item_desc -> Parsetree.signature_item
val mkmod : Location.t -> Parsetree.module_expr_desc -> Parsetree.module_expr
val mkexp : Location.t -> Parsetree.expression_desc -> Parsetree.expression
val mkstr :
  Location.t -> Parsetree.structure_item_desc -> Parsetree.structure_item
val mkfield :
  Location.t -> Parsetree.core_field_desc -> Parsetree.core_field_type
val mkcty : Location.t -> Parsetree.class_type_desc -> Parsetree.class_type
val mkcl : Location.t -> Parsetree.class_expr_desc -> Parsetree.class_expr
val mkcf : Location.t -> Parsetree.class_field_desc -> Parsetree.class_field
val mkctf :
  Location.t -> Parsetree.class_type_field_desc -> Parsetree.class_type_field
val mktyp : Location.t -> Parsetree.core_type_desc -> Parsetree.core_type
val mkpat : Location.t -> Parsetree.pattern_desc -> Parsetree.pattern
val mkghpat : FanLoc.t -> Parsetree.pattern_desc -> Parsetree.pattern
val mkmty : Location.t -> Parsetree.module_type_desc -> Parsetree.module_type
val mkpolytype : Parsetree.core_type -> Parsetree.core_type
val array_function_no_loc : string -> string -> Longident.t
val array_function :
  Location.t -> string -> string -> Longident.t Location.loc
val mkli : Location.t -> string -> string list -> Longident.t Location.loc
val varify_constructors :
  string list -> Parsetree.core_type -> Parsetree.core_type
