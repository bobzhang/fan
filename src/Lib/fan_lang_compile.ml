<:fan<
lang "str_item" ;
>>;

<:include_ml<
"open_template.ml";
>>;

Fan_camlp4syntax.add_quotation_of_str_item_with_filter
  ~name:"compile" ~entry:Syntax.str_items ~filter:(fun s -> begin
    prerr_endline "eval at compile time...";
    Eval.eval_ast err_formatter s ;
    prerr_endline "finished...";
    << >> ;
  end );



















