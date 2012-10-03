<:fan<
lang_at "expr" "expr";
>>;

<:include_ml<
"open_template.ml";
>> ;
open Fan_easy;
open Fan_expr;
value mk_variant_meta_expr cons params =
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ n when n > 1 -> of_ident_number <:ident< Ast.ExAnt >> len
      | 1 ->  <:expr< Ast.ExAnt _loc  .$id:xid 0$.  >>
      | _ ->  failwithf "%s can not be handled" cons]
    else
      params
      |> map (fun [ {ty_expr;_} -> ty_expr ])
      |> fold_left mee_app (mee_of_str cons)  ;
        
value mk_record_meta_expr cols = cols |> map
      (fun [ {record_label; record_info={ty_expr;_};_} -> (record_label, ty_expr)])
      |> mk_record_ee ;

value mk_tuple_meta_expr params =
    params |> map (fun [{ty_expr;_} -> ty_expr]) |> mk_tuple_ee ;

value gen_meta_expr = 
  gen_str_item  ~id:(`Pre "meta_")  ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_expr
    ~mk_record:mk_record_meta_expr mk_variant_meta_expr
    ~module_name:"MetaExpr"
;    

value mk_variant_meta_patt cons params =
    let len = List.length params in 
    if String.ends_with cons "Ant" then
      match len with
      [ n when n > 1 -> of_ident_number <:ident< Ast.PaAnt >> len
      | 1 -> <:expr< Ast.PaAnt _loc  .$id:xid 0$.  >>
      | _ -> failwithf "%s can not be handled" cons ]
    else
      params
      |> map (fun [ {ty_expr;_} -> ty_expr ])
      |> fold_left mep_app (mep_of_str cons);
        
value mk_record_meta_patt cols = cols |> map
      (fun [ {record_label; record_info={ty_expr;_};_}
             -> (record_label, ty_expr)])
         |> mk_record_ep ;

value mk_tuple_meta_patt params = params |> map
      (fun [{ty_expr;_} -> ty_expr]) |> mk_tuple_ep;

value gen_meta_patt =
  gen_str_item  ~id:(`Pre "meta_")
    ~names:["_loc"]
    ~mk_tuple:mk_tuple_meta_patt
    ~mk_record:mk_record_meta_patt mk_variant_meta_patt
    ~module_name:"MetaPatt"
;
open Fan_lang_asthook;  
begin  [
   ("MetaExpr",gen_meta_expr) ;
   ("MetaPatt",gen_meta_patt) ;] |> iter register;
end ;
