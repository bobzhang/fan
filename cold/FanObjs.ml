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
let dump_expr = to_string_of_printer dump#expr
let dump_patt = to_string_of_printer dump#patt
let dump_class_type = to_string_of_printer dump#class_type
let dump_class_expr = to_string_of_printer dump#class_expr
let dump_ident = to_string_of_printer dump#ident
let dump_match_case = to_string_of_printer dump#match_case
let dump_rec_expr = to_string_of_printer dump#rec_expr
let dump_str_item = to_string_of_printer dump#str_item
let dump_sig_item = to_string_of_printer dump#sig_item
let dump_module_binding = to_string_of_printer dump#module_binding
let dump_module_expr = to_string_of_printer dump#module_expr
let dump_class_sig_item = to_string_of_printer dump#class_sig_item
let dump_class_str_item = to_string_of_printer dump#class_str_item
let map_expr f =
  object  inherit  Objs.map as super method! expr x = f (super#expr x) end
let map_patt f =
  object  inherit  Objs.map as super method! patt x = f (super#patt x) end
let map_ctyp f =
  object  inherit  Objs.map as super method! ctyp x = f (super#ctyp x) end
let map_str_item f =
  object 
    inherit  Objs.map as super
    method! str_item x = f (super#str_item x)
  end
let map_sig_item f =
  object 
    inherit  Objs.map as super
    method! sig_item x = f (super#sig_item x)
  end
let map_ctyp f =
  object  inherit  Objs.map as super method! ctyp x = f (super#ctyp x) end
let map_loc f =
  object  inherit  Objs.map as super method! loc x = f (super#loc x) end
class clean_ast =
  object 
    inherit  Objs.map as super
    method! ctyp t =
      match super#ctyp t with
      | `TyPol (_loc,`Nil _l,t)|`Arrow (_loc,t,`Nil _l)
        |`Arrow (_loc,`Nil _l,t)|`Sta (_loc,`Nil _l,t)|`Sta (_loc,t,`Nil _l)
          -> t
      | t -> t
    method! type_parameters t =
      match super#type_parameters t with
      | `Com (_,t,`Nil _) -> t
      | `Com (_,`Nil _,t) -> t
      | t -> t
    method! or_ctyp t =
      match super#or_ctyp t with
      | `Or (_,t,`Nil _) -> t
      | `Or (_,`Nil _,t) -> t
      | t -> t
    method! typedecl t =
      match super#typedecl t with
      | `And (_,t,`Nil _)|`And (_,`Nil _,t) -> t
      | t -> t
    method! name_ctyp t =
      match super#name_ctyp t with
      | `Sem (_,t,`Nil _)|`Sem (_,`Nil _,t) -> t
      | t -> t
    method! class_expr ce =
      match super#class_expr ce with
      | `And (_loc,`Nil _l,ce)|`And (_loc,ce,`Nil _l) -> ce
      | ce -> ce
    method! class_type ct =
      match super#class_type ct with
      | `And (_loc,`Nil _l,ct)|`And (_loc,ct,`Nil _l) -> ct
      | ct -> ct
    method! class_str_item cst =
      match super#class_str_item cst with
      | `Sem (_loc,`Nil _l,cst)|`Sem (_loc,cst,`Nil _l) -> cst
      | cst -> cst
  end
class reloc _loc = object  inherit  Objs.map method! loc _ = _loc end
let wildcarder =
  object (self)
    inherit  Objs.map as super
    method! patt =
      function
      | `Id (_loc,`Lid (_,_)) -> `Any _loc
      | `Alias (_loc,p,_) -> self#patt p
      | p -> super#patt p
  end