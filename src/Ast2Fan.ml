open Parsetree;
open Asttypes;
open Longident;

module Ast = Camlp4Ast;
  
class printer = object(self:'self)
    method longident _loc i = 
      with "ident" match i with
      [ Lident s -> {|$lid:s|}
      | Ldot(y,s) -> {| $(id:self#longident _loc y).$lid:s|}
      | Lapply(a,b) -> {| $(id:self#longident _loc a) $(id:self#longident _loc b) |}]  ;
    method longident_loc i =
      self#longident i.loc i.txt;
    method constant_expr _loc i=
      with "expr" match i with 
      [Const_int32 i -> {|$`int32:i|}
      |Const_int i -> {|$`int:i|}
      |Const_int64 i -> {|$`int64:i|}
      |Const_float i -> {|$flo:i|}
      |Const_nativeint i -> {|$`nativeint:i|}
      |Const_char i -> {|$`chr:i|}
      |Const_string i -> {|$`str:i|} ];
     method constant_patt _loc i =
      with "patt" match i with 
      [Const_int32 i -> {|$`int32:i|}
      |Const_int i -> {|$`int:i|}
      |Const_int64 i -> {|$`int64:i|}
      |Const_float i -> {|$flo:i|}
      |Const_nativeint i -> {|$`nativeint:i|}
      |Const_char i -> {|$`chr:i|}
      |Const_string i -> {|$`str:i|} ];
     method mutable_flag =
       with "mutable_flag" fun 
      [ Immutable -> {||}
      | Mutable -> {|mutable|}];
     method virtual_flag =
       with "virtual_flag" fun
       [Concrete-> {||}
       |Virtual -> {| virtual |}];
     method rec_flag =
       with "rec_flag" fun
       [ Nonrecursive -> {||}
       | Recursive | Default -> {| rec |}] ;
     method direction_flag =
       with "direction_flag" fun
       [ Upto -> {|to|}
       | Downto -> {|downto|}];
     method private_flag =
       with "private_flag" fun
       [Public -> {||}
       |Private -> {|private|}];
     method core_type {ptyp_desc=ty;ptyp_loc=_loc} =
       with "ctyp" match ty with
       [Ptyp_any -> {| _ |}
       |Ptyp_var s -> {| $lid:s |}
       |Ptyp_arrow (label,t1,t2) ->
           match label with
           ["" -> {| $(self#core_type t1) -> $(self#core_type t2) |}
           | s ->
               if s.[0] = '?' then
                 {| ? $label : $(self#core_type t1)-> $(self#core_type t2) |}
               else
                 {| ~ $label : $(self#core_type t1) -> $(self#core_type t2) |} ]
       |Ptyp_tuple ([x::xs]) ->
           {| ( $(self#core_type x)  * $(list: List.map self#core_type xs )) |}
       | Ptyp_tuple [] -> assert false 
       |Ptyp_alias (ty,s) ->
           {| $(self#core_type ty) as $lid:s |}
       | Ptyp_variant (rfs,b,labels) -> assert false
       | Ptyp_poly (ls,ty) ->
           {| ! $(list:(_loc,ls)) . $(self#core_type ty) |}
       | Ptyp_package (lid, ls) ->
           let with_constrs =
             List.map
               (fun (lid,ty) ->
                 {:with_constr| type $(id:self#longident_loc lid) = $(self#core_type ty) |} ) ls in 
           {|(module $(id:self#longident_loc lid) with $(list:with_constrs) )|}
      ];

         (* {:ctyp| ! $list:a . $b |} *)
         (*   {:ctyp| ! 'a 'b 'c . int |} *)
end;




