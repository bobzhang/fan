open Parsetree;
open Asttypes;
open Longident;
open LibUtil;
module Ast = Camlp4Ast;


  
class printer = object(self:'self)
    method longident _loc i = 
      with "ident" match i with
      [ Lident s -> {|$lid:s|}
      | Ldot(y,s) -> {| $(id:self#longident _loc y).$lid:s|}
      | Lapply(a,b) -> {| $(id:self#longident _loc a) $(id:self#longident _loc b) |}]  ;
    method longident_loc i =
      self#longident i.loc i.txt;
    method gen_cases  _loc (lst: list (pattern*expression)) =
     with "match_case" 
     List.map
     (fun (p,e) ->
       match e.pexp_desc with
       [Pexp_when (e1,e2) ->
           {| $(pat:self#pattern p) when $(self#expr e1) -> $(self#expr e2) |}
       | _ ->
          {|$(pat:self#pattern p) -> $(self#expr (e:expression)) |} ]) lst ;

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
       [ Ptyp_any -> {| _ |}
       | Ptyp_var s -> {| $lid:s |}
       | Ptyp_arrow (label,t1,t2) ->
           match label with
           ["" -> {| $(self#core_type t1) -> $(self#core_type t2) |}
           | s ->
               if s.[0] = '?' then
                 {| ? $label : $(self#core_type t1)-> $(self#core_type t2) |}
               else
                 {| ~ $label : $(self#core_type t1) -> $(self#core_type t2) |} ]
       | Ptyp_tuple ([x::xs]) ->
           {| ( $(self#core_type x)  * $(list: List.map self#core_type xs )) |}
       | Ptyp_tuple [] -> assert false 
       | Ptyp_alias (ty,s) ->
           {| $(self#core_type ty) as $lid:s |}

       | Ptyp_variant (rfs,closed,labels) ->
           let ls =
             List.map
               (fun
                 [Rinherit t -> self#core_type t
                 |Rtag (label,_b,[] ) (* FIXME the second b *)
                     -> {| `$lid:label |}
                 |Rtag (label,_b,ls)
                     ->
                       {| `$lid:label of & $(list:List.map self#core_type ls) |}
                 ]) rfs in
             match (closed,labels) with
             [(true,None) ->
               {:ctyp| [= $list:ls ] |}
             |(true,Some x) ->
                 let u = List.map (fun x -> {| `$lid:x |}) x |> Camlp4Ast.tyApp_of_list in
                {:ctyp| [< $list:ls > $u ]|} 
             | (false,_ ) ->
                 {:ctyp| [> $list:ls ] |} ]  
               
       | Ptyp_constr (lid_loc,ts) ->
           Camlp4Ast.tyApp_of_list
             [ {| $(id:self#longident_loc lid_loc) |} ::
               List.map self#core_type ts
             ]

       | Ptyp_object cfs ->
           let row_var = ref false in 
           let res = List.fold_left
             (fun  acc {pfield_loc=_loc;pfield_desc=cf} ->
               match cf  with
               [ Pfield(lab,ty) ->
                 let t = {| $lid:lab : $(self#core_type ty) |} in
                 {| $acc ; $t |} (* FIXME location*)
               | Pfield_var  -> acc 
               ]) {||} cfs   in 
           if !row_var then
             {| < $res; .. > |}
           else
             {| < $res > |}
               
           (*Ptyp_class lows not supported yet
             type u = (int,float)#a [> `a `b]
             --> type u = #a int float ...
            *)
       | Ptyp_class (lid_loc,cts,_lows) -> (* FIXME *)
           (* {|class |} *)
           Ast.tyApp_of_list
             [ {| # $(self#longident_loc lid_loc) |} :: 
               List.map self#core_type cts]
             (* {:ctyp| #a int float|} *)
       | Ptyp_poly (ls,ty) ->
           {| ! $(list:(_loc,ls)) . $(self#core_type ty) |}


       | Ptyp_package (lid, ls) ->
           let with_constrs =
             List.map
               (fun (lid,ty) ->
                 {:with_constr| type $(id:self#longident_loc lid) = $(self#core_type ty) |} ) ls in 
           {|(module $(id:self#longident_loc lid) with $(list:with_constrs) )|}
      ];
     method pattern {ppat_desc=x;ppat_loc=_loc} =
       with "patt" match x with
       [Ppat_any -> {| _ |}
       |Ppat_var {txt;_} -> {| $lid:txt|}
       |Ppat_alias (p,{txt;_}) ->
           {| ($(self#pattern p)  as $lid:txt )|}
       |Ppat_constant c ->
           self#constant_patt _loc c
       |Ppat_tuple [] -> assert false
       |Ppat_tuple [x::xs] -> 
           {| ($(self#pattern x), $(list:(List.map self#pattern xs ))) |}

       |Ppat_construct (lid_loc,opt,_b) -> (* FIXME the third field alwasy false *)
           match opt with
           [None ->
             {| $(id:self#longident_loc lid_loc) |}
           |Some x ->
             {| $(id:self#longident_loc lid_loc)  $(self#pattern x) |} ]

       | Ppat_variant (label,opt) ->
            match opt with
            [Some o -> {| `$label $(self#pattern o) |}
            |None -> {| `$label |} ]
       | Ppat_record (lst,closed) ->
            let ls =
              List.map
                (fun (lid_loc,p) ->
                  {| $(id:self#longident_loc lid_loc) = $(self#pattern p) |}
                              ) lst in
            match closed with
            [ Closed ->   
              {| { $list:ls }|}
            | Open  ->  {| { $list:ls ; _ }|} ]
        | Ppat_array ls ->
            let ls = List.map self#pattern ls in
            {| [| $list:ls |]|}
        | Ppat_or (p1,p2) ->
            {| $(self#pattern p1) | $(self#pattern p2) |}
        | Ppat_constraint (p1,ty) ->
            {| ( $(self#pattern p1) : $(self#core_type ty) ) |}
        | Ppat_type  lid_loc -> 
              {| # $(id:self#longident_loc lid_loc ) |}
        | Ppat_lazy p -> 
           {| lazy $(self#pattern p ) |}
        | Ppat_unpack {txt;_} ->
            {| (module $txt )|}
        ];
     method expr {pexp_desc=x;pexp_loc=_loc} =
       with "expr" match x with
       [Pexp_ident (lid_loc) ->
         {| $(id: self#longident_loc lid_loc) |}
       | Pexp_constant c ->
           self#constant_expr _loc c
       | Pexp_let (recf,lst,e) ->
           let recf = self#rec_flag recf in
           let bindings =
             List.map (fun (p,e) -> {:binding| $(self#pattern p) = $(self#expr e) |}) lst in 
           {|let $rec:recf $list:bindings in $(self#expr e) |}
       | Pexp_function (label,eo,lst) ->
           match label with
           ["" -> let cases = self#gen_cases _loc lst in {|fun [ $list:cases ] |}
           | _ ->
               match lst with
               [ [(p,e)] ->
                 if label.[0] = '?' then
                   match eo with
                   [ Some o ->
                     {| fun ? $label:($(self#pattern p) = $(self#expr o))
                       -> $(self#expr e) |} (* FIXME ?$ =$ illegal *)
                   | None ->
                     {| fun ? $label -> $(self#expr e ) |} ]
                 else
                   {| fun ~ $label -> $(self#expr e) |}
               | _ -> assert false ] ]
       | Pexp_apply (e,lst) ->
           let args =
             List.map
               (fun (label,e) ->
                 let v = self#expr e in
                 if label = "" then
                   v 
                 else
                   {| ~ $label : $v |}
               ) lst in
           Camlp4Ast.exApp_of_list [ self#expr e :: args]
       | Pexp_match (e, lst) ->
           let cases = self#gen_cases _loc lst in
           {| match $(self#expr e) with
              [$list:cases] |}
       | Pexp_try (e,lst) ->
           let cases = self#gen_cases _loc lst in
           {| try $(self#expr e) with [$list:cases] |}
       | Pexp_tuple [] ->
           assert false
       | Pexp_tuple [x::xs] ->
           {| ($(self#expr x), $(list:(List.map self#expr xs )) )|}
       | Pexp_construct (lid_loc,eo,_) (* FIXME*)
           ->
             match eo with
             [ None -> {| $(id:self#longident_loc lid_loc) |}
             | Some v ->
                 {|$(id:self#longident_loc lid_loc) $(self#expr v) |} (* FIXME *)
             ]
        | Pexp_variant (label,eo) ->
            match eo with
            [Some e -> {| `$label $(self#expr e) |}
            |None -> {| `$label |} ]
        | Pexp_record (lst,eo) ->
            let bindings =
              List.map
                (fun (lid,e) -> {:rec_binding| $(id:self#longident_loc lid) = $(self#expr e) |}) lst in 
            match eo with
            [ Some e  -> {| { ($(self#expr e)) with $list:bindings } |}
            | None -> {| { $list:bindings } |}]
        | Pexp_field (e,lid_loc) ->
            {| $(self#expr e).$(id:self#longident_loc lid_loc) |}
        | Pexp_setfield (e1,lid_loc,e2) ->
            {| $(self#expr e1).$(id:self#longident_loc lid_loc) <- $(self#expr e2) |}
        | Pexp_array lst ->
            {| [| $(list: List.map self#expr lst ) |] |}
        | Pexp_ifthenelse (e1,e2,e3) ->
           match e3 with
           [Some e3 ->
             {| if $(self#expr e1) then $(self#expr e2 ) else $(self#expr e3) |}
           | None ->
               {| if $(self#expr e1) then $(self#expr e2 ) else () |} ]  
        | Pexp_sequence (e1,e2) ->
            {| begin $(self#expr e1) $(self#expr e2) end |}
        | Pexp_while (e1,e2) ->
            {| while $(self#expr e1) do $(self#expr e2) done |}
        | Pexp_for ({txt;_},e1,e2,df,e3) ->
            (* FIXME non-terminal expected after ... more friendly error message *)
            {| for $txt = $(self#expr e1) $(to:self#direction_flag df) $(self#expr e2) do
                    $(self#expr e3)
                done
            |}
        | Pexp_constraint (e1,ot1,ot2) ->
            match (ot1,ot2) with
            [(None,None) ->
              self#expr e1
            | (Some t1,Some t2) ->
                {| ($(self#expr e1) : $(self#core_type t1) :> $(self#core_type t2) ) |}
            | (Some t1,None) ->
                {| ($(self#expr e1) : $(self#core_type t1)  ) |}
            | (None,Some t2) ->
                {| ($(self#expr e1) :> $(self#core_type t2) ) |}
           ]  
        | Pexp_when _ -> assert false
        | Pexp_send (e,txt) -> 
            {| $(self#expr e)# $txt |}
        | Pexp_new lid_loc ->
            {| new $(id:self#longident_loc lid_loc) |}
        | Pexp_setinstvar ({txt;_},e) ->
            {| $lid:txt <- $(self#expr e) |}
        | Pexp_override lst ->
            let lst = List.map (fun ({txt;_},e) -> {:rec_binding| $lid:txt = $(self#expr e)|}) lst in
            {| {< $list:lst >}|}
        | Pexp_letmodule ({txt;_},me,e) ->
            {| let module $txt = $(self#module_expr me) in $(self#expr e) |}
        | Pexp_assert e ->
            {| assert $(self#expr e) |}
        | Pexp_assertfalse -> {| assert false |}
        | Pexp_lazy e -> {| lazy $(self#expr e) |}
        | Pexp_poly _ -> assert false (* appears only in Pcf_meth *)
          (* {:expr| object (self:'self) method x = 3 ; end |} *)    
        | Pexp_object {pcstr_pat=pat;pcstr_fields=fs}  -> (* assert false *)
            {|object ($(self#pattern pat)) $(self#class_fields fs) end |}
              
        | Pexp_newtype (str,e) ->
            {| fun (type $str) ->  $(self#expr e) |}
        | Pexp_pack me ->
            {| (module $(self#module_expr me) ) |}
        | Pexp_open (lid_loc,e) ->
            {| $(id:self#longident_loc lid_loc).($(self#expr e) ) |}
        ];

     method module_expr {pmod_desc=x;pmod_loc = _loc} : Ast.module_expr =
       with "module_expr" match x with
       [ Pmod_ident lid_loc ->
         {| $(id:self#longident_loc lid_loc) |}
       | Pmod_structure s ->
           {| struct $(self#structure s) end|}
       | Pmod_functor ({txt;_},mty,me) ->
           {| functor ($txt : $(self#module_type mty)) -> $(self#module_expr me) |}
       | Pmod_apply (me1,me2) ->
           {| $(self#module_expr me1) $(self#module_expr me2) |}
       | Pmod_constraint (me,mty) ->
           {| ( $(self#module_expr me) : $(self#module_type mty) ) |}
       | Pmod_unpack e ->
           {| (val $(self#expr e)) |}
       ];
     method lhs_type_declaration (params,variance,({loc;_} as lid_loc)) =
       with "ctyp"
       let u = List.map2
         (fun p v ->
           match (p,v) with
           [((false,false),Some {txt;loc=_loc}) ->
             {| '$txt |}
           | ((false,false),None) ->
               let _loc = FanLoc.ghost in {| _ |}
           | ((true,false),Some{txt;loc=_loc}) ->
               {| +' $txt|}
           | ((true,false),None) ->
               let _loc = FanLoc.ghost in Ast.TyAnP _loc 
           | ((false,true),Some {txt;loc=_loc} ) ->
               {| - ' $txt |}
           | ((false,true),None) ->
               let _loc = FanLoc.ghost in Ast.TyAnM _loc
           | _ -> assert false ]) variance params  in
       Camlp4Ast.tyApp_of_list
         [
          {@loc| $(id:self#longident_loc lid_loc) |}::
          u] ;
     method with_constraint  (({loc=_loc;_} as lid1),w)  =
       with "with_constr" match w with
       [ Pwith_type ({ptype_params=ls;ptype_manifest=Some ty;ptype_variance;_} ) -> 
           {| type $(self#lhs_type_declaration (ls, ptype_variance,lid1))
                 = $(self#core_type ty)|}
       | Pwith_typesubst ({ptype_params=ls;ptype_manifest=Some ty;ptype_variance;_} ) -> 
           {| type $(self#lhs_type_declaration (ls, ptype_variance,lid1))
                 := $(self#core_type ty)|}
       | Pwith_type _ | Pwith_typesubst _ -> assert false 
       | Pwith_module lid2 ->
           {| module $(id:self#longident_loc lid1) = $(id:self#longident_loc lid2) |}
       | Pwith_modsubst lid2 ->
           {| module $(id:self#longident_loc lid1) := $(id:self#longident_loc lid2) |}
       ] ;
     
     method module_type {pmty_desc=x;pmty_loc=_loc}:Ast.module_type =
       with "module_type" match x with
       [Pmty_ident lid_loc -> {| $(id:self#longident_loc lid_loc ) |}
       |Pmty_signature s ->
           {| sig $(self#signature s) end |}
       | Pmty_functor ({txt;_},mty1,mty2 ) ->
           {| functor ($txt : $(self#module_type mty1)) -> $(self#module_type mty2) |}
       | Pmty_with (mt1,lst) ->
           let lst = List.map self#with_constraint lst in
           {| $(self#module_type mt1) with $list:lst |}
       | Pmty_typeof me ->
           {| module type of $(self#module_expr me) |}
       ];  

     method structure_item {pstr_desc=x;pstr_loc=_loc} : Ast.str_item =
       with "str_item" match x with
       [Pstr_eval e -> {| $(exp:self#expr e) |}
       |Pstr_value (rf,lst) ->
           let bindings =
             List.map (fun (p,e) -> {:binding| $(self#pattern p) = $(self#expr e) |}) lst in 
           {|let $(rec:self#rec_flag rf) $list:bindings |}
       | Pstr_module ({txt;_},me) ->
           {| module $txt = $(self#module_expr me) |}
       | Pstr_modtype ({txt;_},mty) ->
           {| module type $txt = $(self#module_type mty) |}
       | Pstr_open lid ->
           {| open $(id:self#longident_loc lid) |}
       | Pstr_include me ->
           {| include $(self#module_expr me) |}
       | Pstr_class_type _ 
       | Pstr_class _
       | Pstr_recmodule _
       | Pstr_exn_rebind _
       | Pstr_exception _
       | Pstr_primitive _
       | Pstr_type _ ->  assert false

       ];  

     method structure (ls:structure) : Ast.str_item =
       assert false; 
     method signature (ls:signature)  : Ast.sig_item =
       assert false;
     method signature_item {psig_desc=x;psig_loc=_loc} : Ast.sig_item =
       raise Not_found;
     method class_fields (ls:list class_field) : Ast.class_str_item =
       assert false;
     method class_field {pcf_desc=x;pcf_loc = _loc} : Ast.class_str_item =
       assert false;
     method class_expr {pcl_desc=x;pcl_loc=_loc} : Ast.class_expr = assert false;
     method class_type ({pci_expr;_}: class_infos class_type)  : Ast.class_type = assert false;
  (*    method class_types ls = *)
  (*      with "class_type_declaration" *)
  (*      {| $(list:List.map self#class_type ls ) |} ; *)
  (* {:class_type| object end |} *)
  (*   {:class_type| $a and $b |} *)
       (* {:str_item| class type a = object end and b = object end|} *)
end;




