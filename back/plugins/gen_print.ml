<:fan<
lang_at "expr" "expr"; >> ;

<:include_ml<
"open_template.ml";
>> ;
open Fan_easy;
open Fan_expr;
value extract info = info
    |> map (fun [{ty_name_expr;ty_id_expr;_} -> [ty_name_expr;ty_id_expr] ])
    |> concat ;

value mkfmt pre sep post fields =
    << Format.fprintf fmt  .$str: pre^ String.concat sep fields ^ post $. >> ;
  
value mk_variant_print cons params =
    let len = List.length params in
    let pre =
        if len >= 1 then
          mkfmt ("@[<1>("^cons^"@ ")
            "@ " ")@]" (init len (fun _ -> "%a"))
        else
          mkfmt cons "" "" [] in
    params |> extract |> apply pre ;
    
value mk_tuple_print params =
    let len = List.length params in
    let pre = mkfmt "@[<1>(" ",@," ")@]" (init len (fun _ -> "%a")) in 
    params |> extract |> apply pre  ;
    
value mk_record_print cols = 
    let pre = cols
       |> map (fun [ {record_label;_} -> record_label^":%a" ])
       |>  mkfmt "@[<hv 1>{" ";@," "}@]" in 
    cols |> map(fun [ {record_info;_} -> record_info ])
         |> extract |> apply pre  ;
  
value gen_print =
  gen_str_item  ~id:(`Pre "pp_print_")  ~names:["fmt"] 
    ~mk_tuple:mk_tuple_print  ~mk_record:mk_record_print   mk_variant_print;    

value gen_print_obj =
  gen_object ~kind:Iter ~mk_tuple:mk_tuple_print
    ~base:"printbase" ~class_name:"print"
    ~names:["fmt"]  ~mk_record:mk_record_print mk_variant_print;

[("Print",gen_print);
 ("OPrint",gen_print_obj)] |> iter Fan_lang_asthook.register;

















