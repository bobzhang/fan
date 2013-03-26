open Objs
open LibUtil
let dump = new print
let dump_type_parameters = to_string_of_printer dump#type_parameters
let dump_row_field = to_string_of_printer dump#row_field
let dump_or_ctyp = to_string_of_printer dump#or_ctyp
let dump_type_repr = to_string_of_printer dump#type_repr
let dump_type_info = to_string_of_printer dump#type_info
let dump_typedecl = to_string_of_printer dump#typedecl
let dump_ctyp = to_string_of_printer dump#ctyp
let dump_name_ctyp = to_string_of_printer dump#name_ctyp
let dump_with_constr = to_string_of_printer dump#with_constr
let dump_module_type = to_string_of_printer dump#module_type
let dump_exp = to_string_of_printer dump#exp
let dump_pat = to_string_of_printer dump#pat
let dump_class_type = to_string_of_printer dump#class_type
let dump_class_exp = to_string_of_printer dump#class_exp
let dump_ident = to_string_of_printer dump#ident
let dump_type_constr = to_string_of_printer dump#type_constr
let dump_case = to_string_of_printer dump#case
let dump_rec_exp = to_string_of_printer dump#rec_exp
let dump_stru = to_string_of_printer dump#stru
let dump_sig_item = to_string_of_printer dump#sig_item
let dump_module_binding = to_string_of_printer dump#module_binding
let dump_module_exp = to_string_of_printer dump#module_exp
let dump_class_sig_item = to_string_of_printer dump#class_sig_item
let dump_cstru = to_string_of_printer dump#cstru
let dump_decl_param = to_string_of_printer dump#decl_param
let dump_decl_params = to_string_of_printer dump#decl_params
class reloc _loc = object  inherit  Objs.map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  Objs.map as super
    method! pat =
      function
      | `Id (_loc,`Lid (_,_)) -> `Any _loc
      | `Alias (_loc,p,_) -> self#pat p
      | p -> super#pat p
  end