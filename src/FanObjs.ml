open Objs;
open LibUtil;
let dump = new print;

let dump_type_parameters = to_string_of_printer dump#type_parameters;  
let dump_row_field = to_string_of_printer dump#row_field;
let dump_or_ctyp = to_string_of_printer dump#or_ctyp;  
let dump_type_repr = to_string_of_printer dump#type_repr;
let dump_type_info = to_string_of_printer dump#type_info;  
let dump_typedecl = to_string_of_printer dump#typedecl;
let dump_ctyp = to_string_of_printer dump#ctyp;
let dump_name_ctyp = to_string_of_printer dump#name_ctyp;  
let dump_with_constr = to_string_of_printer dump#with_constr;
let dump_module_type = to_string_of_printer dump#module_type;
let dump_expr = to_string_of_printer dump#expr;
let dump_patt = to_string_of_printer dump#patt;
let dump_class_type = to_string_of_printer dump#class_type;
let dump_class_expr = to_string_of_printer dump#class_expr;
let dump_ident = to_string_of_printer dump#ident;
let dump_match_case = to_string_of_printer dump#match_case;
let dump_rec_expr = to_string_of_printer dump#rec_expr;  
let dump_str_item = to_string_of_printer dump#str_item;
let dump_sig_item = to_string_of_printer dump#sig_item;
let dump_module_binding  = to_string_of_printer dump#module_binding;
let dump_module_expr = to_string_of_printer dump#module_expr;  
let dump_class_sig_item = to_string_of_printer dump#class_sig_item;
let dump_class_str_item = to_string_of_printer dump#class_str_item;  



let map_expr f = object
  inherit Objs.map as super;
  method! expr x = f (super#expr x);
end;
let map_patt f = object
  inherit Objs.map as super;
  method! patt x = f (super#patt x);
end;
let map_ctyp f = object
  inherit Objs.map as super;
  method! ctyp x = f (super#ctyp x);
end;
let map_str_item f = object
  inherit Objs.map as super;
  method! str_item x = f (super#str_item x);
end;
let map_sig_item f = object
  inherit Objs.map as super;
  method! sig_item x = f (super#sig_item x);
end;
let map_ctyp f = object
  inherit Objs.map as super;
  method! ctyp x = f (super#ctyp x);
end;
let map_loc f = object
  inherit Objs.map as super;
  method! loc x = f (super#loc x);
end;


class clean_ast = object
  inherit Objs.map as super;
  (* method! with_constr wc = *)
  (*   with with_constr *)
  (*   match super#with_constr wc with *)
  (*   [ {| $({@_l||})  and $wc |} | *)
  (*     {| $wc and $({@_l||} ) |} -> wc *)
  (*   | wc -> wc ]; *)
  (* method! expr e = *)
  (*   with expr *)
  (*   match super#expr e with *)
  (*   [ {| let $rec:_ $({:binding@_l||}) in $e |} | *)
  (*     (\* {| { ($e) with $({:rec_expr@_l||})  } |} | *\) *)
  (*     {| $({@_l||} ), $e |} | *)
  (*     {| $e, $({@_l||} ) |} | *)
  (*     {| $({@_l||}); $e |} | *)
  (*     {| $e; $({@_l||} ) |} -> e *)
  (*   | e -> e ]; *)
  (* method! patt p = *)

  (*   match super#patt p with *)
  (*   [ (\* {| ( $p as $({@_l||} ) ) |} | *\) *)
  (*     {| $({@_l||}) | $p |} | *)
  (*     {| $p | $({@_l||} ) |} | *)
  (*     {| $({@_l||} ), $p |} | *)
  (*     {| $p, $({@_l||} ) |} | *)
  (*     {| $({@_l||} ); $p |} | *)
  (*     {| $p; $({@_l||} ) |} -> p *)
  (*   | p -> p ]; *)
  (* method! match_case mc = *)
  (*   with match_case *)
  (*   match super#match_case mc with *)
  (*   [ {| $({@_l||} ) | $mc |} | *)
  (*     {| $mc | $({@_l||} ) |} -> mc *)
  (*   | mc -> mc ]; *)
  (* method! binding bi = *)
  (*   with binding *)
  (*   match super#binding bi with *)
  (*   [ {| $({@_l||} ) and $bi |} | *)
  (*     {| $bi and $({@_l||} ) |} -> bi *)
  (*   | bi -> bi ]; *)
  (* method! rec_expr rb = *)
  (*   with rec_expr *)
  (*   match super#rec_expr rb with *)
  (*   [ {| $({@_l||} ) ; $bi |} | {| $bi ; $({@_l||} ) |} -> bi *)
  (*   | bi -> bi ]; *)

  (* method! module_binding mb = *)
  (*   with module_binding *)
  (*   match super#module_binding mb with *)
  (*   [ {| $({@_l||} ) and $mb |} | *)
  (*     {| $mb and $({@_l||} ) |} -> mb *)
  (*   | mb -> mb ]; *)

  method! ctyp t =
    with ctyp
    match super#ctyp t with
    [
     {| ! $({@_l||} ) . $t |} |
      (* {| $({@_l||} ) as $t |} | *)
      (* `Alias(_loc,`Nil _l, t) | *)
      (* {| $t as $({@_l||} ) |} | *)
      {| $t -> $({@_l||} ) |} |
      {| $({@_l||} ) -> $t |} |
      (* {| $({@_l||} ) | $t |} | *)
      (* `Or(_loc,`Nil _l ,t) | *)
      (* `Or(_loc,t,`Nil _l) | *)
      (* {| $t | $({@_l||} ) |} | *)
      (* {| $t of $({@_l||} ) |} | *)

      (* {| $({@_l||} ) and $t |} | *)
      (* {| $t and $({@_l||} ) |} | *)
      (* `Com(_loc,`Nil _loc,t) | *)
      (* `Com(_loc,t, `Nil _loc)| *)
      (* {| $({@_l||}), $t |} | *)
      (* {| $t, $({@_l||} ) |} | *)
      (* {| $t & $({@_l||} ) |} | *)
      (* {| $({@_l||} ) & $t |} | *)
      `Sta(_loc,`Nil _l,t) |
      `Sta(_loc,t, `Nil _l) 
      (* {| $({@_l||} ) * $t |} | *)
      (* {| $t * $({@_l||} ) |} *)
      -> t
    | t -> t ];
  method! type_parameters t =
    match super#type_parameters t with
      [`Com(_,t, `Nil _ ) -> t | `Com (_,`Nil _, t) -> t | t -> t];
  method! or_ctyp t =
    match super#or_ctyp t with [ `Or(_,t,`Nil _) -> t | `Or(_,`Nil _,t) -> t| t -> t];
  method! typedecl t =
     match super#typedecl t with [`And(_,t,`Nil _) | `And(_,`Nil _,t) -> t | t -> t];
  (* method! poly_ctyp t = *)
  (*   match super#poly_ctyp t with *)
  (*   [`TyPol(_,`Nil _,t) -> t | t ->t ]; *)
  method! name_ctyp t =
    match super#name_ctyp t with
    [`Sem(_,t,`Nil _)
    |`Sem(_,`Nil _,t) -> t | t -> t ]  ;
  (* method! sig_item sg = *)
  (*   with sig_item *)
  (*   match super#sig_item sg with *)
  (*   [ {| $({@_l||}); $sg |} | {| $sg; $({@_l||} ) |} -> sg *)
  (*   | {| type $({:ctyp@_l||} ) |} -> {||} *)
  (*   | sg -> sg ]; *)

  (* method! str_item st = *)
  (*   with str_item *)
  (*   match super#str_item st with *)
  (*   [(\*  {| $({@_l||} ); $st |} | {| $st; $({@_l||} ) |} -> st *\) *)
  (*   (\* |  *\){| type $({:ctyp@_l||} ) |} -> {||} *)
  (*   | {| let $rec:_ $({:binding@_l||} ) |} -> {||} *)
  (*   | st -> st ]; *)

  (* method! module_type mt = *)
  (*   match super#module_type mt with *)
  (*   [ {:module_type| $mt with $({:with_constr@_l||} ) |} -> mt *)
  (*   | mt -> mt ]; *)

  method! class_expr ce =
    with class_expr
    match super#class_expr ce with
    [ {| $({@_l||} ) and $ce |} | {| $ce and $({@_l||} ) |} -> ce
    | ce -> ce ];

  method! class_type ct =
    with class_type
    match super#class_type ct with
    [ {| $({@_l||} ) and $ct |} | {| $ct and $({@_l||} ) |} -> ct
    | ct -> ct ];

  (* method! class_sig_item csg = *)
  (*   with class_sig_item *)
  (*   match super#class_sig_item csg with *)
  (*   [ {| $({@_l||} ); $csg |} | {| $csg; $({@_l||} ) |} -> csg *)
  (*   | csg -> csg ]; *)

  method! class_str_item cst =
    with class_str_item
    match super#class_str_item cst with
    [ {| $({@_l||} ); $cst |} | {| $cst; $({@_l||} ) |} -> cst
    | cst -> cst ];
end;

(* change all the [loc] to [ghost] *)    
class reloc _loc = object
  inherit Objs.map ;
  method! loc _ = _loc;
end;

(*
  {[]}
 *)  
let wildcarder = object (self)
  inherit Objs.map as super;
  method! patt = fun
  [ {:patt| $lid:_ |} -> {:patt| _ |}
  | {:patt| ($p as $_) |} -> self#patt p
  | p -> super#patt p ];
end;

