<:fan<
lang "expr" ;
>> ;
<:include_ml<
"open_template.ml";
>> ;
open Fan_easy;
open Fan_expr;
value mk_variant_eq _cons  = fun 
  [ [] -> << True >>
  | ls -> reduce_left_with
        ~compose:(fun x y -> << .$x$. && .$y$. >> )
        ~map:(fun [{ty_expr;_} -> ty_expr]) ls ];
value mk_tuple_eq exprs = mk_variant_eq "" exprs ;
value mk_record_eq cols =
    cols |> map (fun [ {record_info;} -> record_info])
         |> mk_variant_eq "" ;
value gen_eq =
  gen_str_item ~id:(`Pre "eq_")  ~names:[]
    ~arity:2   ~mk_tuple:mk_tuple_eq ~mk_record:mk_record_eq mk_variant_eq
    ~trail: << False >> ;
[ ("Eq",gen_eq) ; ] |> iter Fan_lang_asthook.register;
