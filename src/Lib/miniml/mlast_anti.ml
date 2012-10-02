<:fan< include_ml "open_template.ml"; >> ;

value aq_expander = object 
  inherit Ast.map as super ;
  method expr = fun 
    [ Ast.ExAnt(_loc,s) -> begin
      <:fan< lang "expr"; >> ;
      handle_antiquot_in_string s
        ~term:(failwithf "%s is an invalid antiquot")
        ~parse:Syntax.AntiquotSyntax.parse_expr
        ~loc:_loc
        ~decorate:(fun name e
          -> match name with
          [ "int" ->
	    <:expr< Ml_int_const  .$e$.  >>
	  | "bool" ->
	    <:expr< Ml_bool_const  .$e$.  >>
	  | "lid" ->
	    <:expr< Ml_var  .$e$.  >>
	  | "patt" ->
	    <:expr< Ml_pat_id  .$e$.  >> 
	  | "" -> e 
          | _ -> failwithf "unsupported expander %s in expr context" name ])
    end
    | e -> super#expr e];
  method patt = fun
    [Ast.PaAnt(_loc,s) -> begin
      handle_antiquot_in_string s
        ~term:(failwithf "%s is an invalid antiquot")
        ~parse:Syntax.AntiquotSyntax.parse_patt
        ~loc:_loc
        ~decorate:(fun name e
          -> match name with
          [ "int" ->
	    <:patt< Ml_int_const  .$e$.  >>
	  | "bool" ->
	    <:patt< Ml_bool_const  .$e$.  >>
	  | "lid" ->
	    <:patt< Ml_var  .$e$.  >>
	  | "patt" ->
	    <:patt< Ml_pat_id  .$e$.  >> 
	  | "" -> e 
          | _ -> failwithf "unsupported expander %s in expr context" name ])
    end
    | e -> super#patt e ];
end ;




















