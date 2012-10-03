
<:fan<
lang "expr"; >> ;

<:include_ml<
"open_template.ml";
>> ;
open Fan_easy;
open Fan_expr;
value (gen_map,gen_map2) =
  let mk_variant cons params =
    params |> map (fun [ {ty_expr;_} -> ty_expr]) |> apply (of_str cons) in
  let mk_tuple params =
    params |> map (fun [{ty_expr; _ } -> ty_expr]) |> tuple_of_list in 
  let mk_record cols =
    cols |> map (fun [ {record_label; record_info={ty_expr;_ } ; _ }  ->
          (record_label,ty_expr) ] )  |> mk_record 
  in
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase" ~class_name:"map"
     mk_variant ~names:[],
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"mapbase2" ~class_name:"map2" mk_variant ~names:[]
     ~arity:2 ~trail: <<  invalid_arg "map2 failure" >> )
;

begin
  [("Map",gen_map);
   ("Map2",gen_map2);]
  |> iter Fan_lang_asthook.register;
end;
















