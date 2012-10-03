open Format;
open Fan_dynamic_plugins;
open Fan_basic;
<:fan<
lang "str_item";
>>;

<:include_ml<
"open_template.ml";  
>>;

value file = Filename.chop_extension __FILE__;


type ty_meta = {
    ty_str:string;
    ty_print: [= `Exist | `Custom of Ast.str_item | `Fmt of string];
    ty_eq: [= `Def | `Custom of Ast.str_item]
  };

value base1_types =
  [ ("int",
     `Exist ,
     `Def) ;
    
    ("int32",
     `Fmt "%ld",
     `Def);
    
    ("int64",
     `Fmt "%Ld",
     `Def );
    
    ("nativeint",
     `Fmt "%nd",
     `Def);
    
    ("float",
     `Exist,
     `Def);
    
    ("string",
     (* `Custom (
      * <<
      * value pp_print_string fmt str=
      *   Format.fprintf fmt "@[\"%s\"@]"(String.escaped str)
      * >> ) , *)
     `Fmt "%S",
     `Def );
    
    ("bool",
     `Exist,
     `Def);
    
    ("char",
     `Exist,
     `Def);
    
    ("unit",
     `Custom (
     << value pp_print_unit : Format.formatter -> unit -> unit = fun fmt _ ->
          Format.fprintf fmt "()"
     >>  
    ),
     `Custom (
     << 
     value eq_unit : unit -> unit -> bool = fun _ _ -> True
       >>  ))  
  ]
;

value ty_metas =
  base1_types |> map (fun
    [(ty_str,ty_print,ty_eq) ->
      {ty_str;ty_print;ty_eq}])
;
  
value print_base1 =
  let items = ty_metas |> map (fun [
  {ty_str;ty_print;_} ->
    let ty = <:ctyp< Format.formatter -> .$lid:ty_str$. -> unit >> in
    let name = "pp_print_"^ty_str in
    match ty_print with
    [`Exist -> << value .$lid:name$. = .$lid:name$. >>
    |`Custom s -> s
    |`Fmt c ->
      <<value .$lid:name$. : .$ty$. =
        fun fmt a -> Format.fprintf fmt .$str:c$. a  >> ]]) in
  << .$list:items$. >> ;
  
  
value (map_class_str_item_base_1, map_class_str_item_base_2,
       fold_class_str_item_base_1, fold_class_str_item_base_2,
      print_class_str_item_base)=
  let ty_names = ty_metas |> map (fun [{ty_str;_} -> ty_str]) in
  let v1 = ty_names |> map (fun x ->
              let ty = <:ctyp< .$lid:x$. -> .$lid:x$. >> in
              <:class_str_item< method .$lid:x$. : .$ty$. =
                fun x -> x >> ) in 
  let v2 = ty_names |> map (fun x ->
              let ty = <:ctyp< .$lid:x$. -> .$lid:x$. -> .$lid:x$. >> in
              <:class_str_item< method .$lid:x$. : .$ty$. =
                fun x _ -> x >> ) in
  let v3 = ty_names |> map (fun x ->
              let ty = <:ctyp< .$lid:x$. -> 'self_type >> in
              <:class_str_item< method .$lid:x$. : .$ty$. =
                fun _ -> self >> ) in 
  let v4 = ty_names |> map (fun x ->
              let ty = <:ctyp< .$lid:x$. -> .$lid:x$. -> 'self_type >> in
              <:class_str_item< method .$lid:x$. : .$ty$. = fun _ _ -> self >> ) in
  let v5 = ty_names |> map (fun x ->
    <:class_str_item< method .$lid:x$. = .$lid:"pp_print_"^x$. >> ) in 
  ( <:class_str_item< .$list:v1$. >>,
    <:class_str_item< .$list:v2$. >>,
    <:class_str_item< .$list:v3$. >>,
    <:class_str_item< .$list:v4$. >>,
    <:class_str_item< .$list:v5$. >> )
;

value eq_base1 =   
  let items = ty_metas |> map (fun [
  {ty_str;ty_eq;_} ->
    let ty =  <:ctyp< .$lid:ty_str$. -> .$lid:ty_str$. -> bool >>  in
    let name = "eq_" ^ ty_str in 
    match ty_eq with
    [`Def -> << value .$lid:name$. : .$ty$. = (=) >>
    |`Custom s -> s ]]) in 
  << .$list:items$. >> 
;

update (file,"eq_base1") str_items
    (module struct value code = eq_base1 ; end);

update (file,"print_base1") str_items
    (module struct value code = print_base1; end);
  
value register_class_str_items = List.iter (fun (name,code) -> 
  update (file,name) class_str_items
    (module struct value code = code; end));

register_class_str_items
    [ ("map_class_str_item_base_1",map_class_str_item_base_1);
      ("map_class_str_item_base_2",map_class_str_item_base_2);
      ("fold_class_str_item_base_1",fold_class_str_item_base_1);
      ("fold_class_str_item_base_2",fold_class_str_item_base_2);
      ("print_class_str_item_base",print_class_str_item_base); ]
;








