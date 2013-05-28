open Parsetree

val with_loc : 'a -> Location.t -> 'a Location.loc

val lident : string -> Longident.t

val lident_with_loc : string -> Location.t -> Longident.t Location.loc

val ldot : Longident.t -> string -> Longident.t

val lapply : Longident.t -> Longident.t -> Longident.t

val mkghloc : FLoc.t -> FLoc.t

val error : FLoc.t -> string -> 'a

val mksig :
  Location.t -> signature_item_desc -> signature_item
val mkmod : Location.t -> module_expr_desc -> module_expr

val mkexp : Location.t -> expression_desc -> expression

val mkstr :
  Location.t -> structure_item_desc -> structure_item

val mkfield :
    Location.t -> core_field_desc -> core_field_type
val mkcty : Location.t -> class_type_desc -> class_type

val mkcl : Location.t -> class_expr_desc -> class_expr

val mkcf : Location.t -> class_field_desc -> class_field

val mkctf :
  Location.t -> class_type_field_desc -> class_type_field
val mktyp : Location.t -> core_type_desc -> core_type

val mkpat : Location.t -> pattern_desc -> pattern

val mkghpat : FLoc.t -> pattern_desc -> pattern

val mkmty : Location.t -> module_type_desc -> module_type

val mkpolytype : core_type -> core_type

val array_function_no_loc : string -> string -> Longident.t

val array_function :
  Location.t -> string -> string -> Longident.t Location.loc

val mkli : Location.t -> string -> string list -> Longident.t Location.loc

val varify_constructors :  string list -> core_type -> core_type
