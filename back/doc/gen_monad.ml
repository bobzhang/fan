open Format;
open Camlp4.PreCast;
open Format;
open Fan_easy;
open List;
open Fan_expr;
open Fan_asthook;  
<:fan< lang "expr";>>;

value bind = of_str ">>="; (* overcome the parsing problem*)
  
value (gen_fold,gen_fold2) =
  let mk_variant cons params  =
    let ids = params   |> map (fun [{ty_id_exprs;_} -> List.hd ty_id_exprs]) in
    let patts = params |> map (fun [{ty_id_patts;_} -> List.hd ty_id_patts]) in 
    let exprs = params |> map (fun [{ty_expr;_} -> ty_expr]) in 
    List.fold_right2
      (fun v p acc ->
        app_of_list
          [bind; v ; << fun [$p$ -> $acc$] >> ] ) exprs patts
       << return  .$(apply (of_str cons) ids)$.  >> in 
  let mk_tuple params =
    let ids = params   |> map (fun [{ty_id_exprs;_} -> List.hd ty_id_exprs]) in
    let patts = params |> map (fun [{ty_id_patts;_} -> List.hd ty_id_patts]) in 
    let exprs = params |> map (fun [{ty_expr;_} -> ty_expr]) in 
    List.fold_right2
      (fun v p acc ->
        app_of_list
          [bind; v ; << fun [$p$ -> $acc$] >> ] ) exprs patts
       << return  .$(tuple_of_list ids)$.  >> in 
  let mk_record cols =
    let params =
      cols |> map
        (fun [ {record_label;
                record_info =
                {ty_expr;ty_id_exprs;ty_id_patts;_}
                  ; _
              } ->
                (record_label,ty_expr, List.hd ty_id_exprs, List.hd ty_id_patts) ] ) in
    let labels = params |> map(fun (i,_,_,_) -> i ) in
    let exprs = params |> map (fun (_,i,_,_) -> i) in
    let ids = params |> map   (fun (_,_,i,_) -> i)  in
    let patts = params |> map (fun (_,_,_,i) -> i) in
    List.fold_right2
      (fun v p acc ->
        app_of_list [bind; v ; << fun [$p$ -> $acc$ ] >> ] )
      exprs patts << return  .$mk_record (List.combine labels ids) $.  >> in 
  (gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"monadbase" ~class_name:"monad" mk_variant ~names:[],
   gen_object ~kind:Map ~mk_tuple ~mk_record
     ~base:"monadbase2" ~class_name:"monad2"
     mk_variant ~names:[]
     ~arity:2 ~trail:(fun
     [(_,1) -> <:match_case< >>
     |(_,_) ->  <:match_case< (_,_) -> invalid_arg "monad fold2 failure" >>
     ] ))
;
begin  
   [("MFold",gen_fold);
    ("MFold2",gen_fold2);] |> iter register;
end;
