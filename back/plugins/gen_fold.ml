<:fan<
lang "expr";
>>;
<:include_ml<
"open_template.ml";
>>;

open Fan_easy;
open Fan_expr;


value (gen_fold,gen_fold2) = 
  let mk_variant cons params =
    params
    |> map (fun [{ty_expr;_} -> ty_expr])
    |> (fun
        [ [] -> << self >>
        | ls -> reduce_right
              (fun v acc -> << let self = .$v$. in .$acc$. >>)
              ls ]) in
  let mk_tuple  = mk_variant ""  in 
  let mk_record cols =
    cols |> map (fun [ {record_label; record_info ; _ } -> record_info ] )
         |> mk_variant "" in 
  (gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase" ~class_name:"fold" mk_variant ~names:[],
   gen_object ~kind:Fold ~mk_tuple ~mk_record
     ~base:"foldbase2" ~class_name:"fold2"
     mk_variant ~names:[]
     ~arity:2 ~trail: <<invalid_arg "fold2 failure" >> ) ;
begin  
   [("Fold",gen_fold);
    ("Fold2",gen_fold2);] |> iter Fan_lang_asthook.register;
end;
