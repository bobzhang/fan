(* Parser.start_parser_of_levels (Obj.magic expr) [ Gram.find_level expr ~position:(`Level "simple") ] 0;; *)

(* Parser.parser_of_symbol (Obj.magic expr)  (`Snterm (Obj.magic ident)) 0 (Gram.filter (Gram.lex _loc (Stream.of_string "A.B.c")));; *)

let level = Gram.find_level expr ~position:(`Level "simple");;
let u =
  Parser.parser_of_tree (Obj.magic expr) (0,`LA) level.lprefix (Gram.filter (Gram.lex _loc (Stream.of_string "A.B.c")));;

(Obj.magic u _loc : Ast.expr);;
(* - : Lib.Meta.Ast.expr = ExId (, IdUid (, "A")) *)


(*
  {:delete|Gram expr:
      ["`"; a_ident{s} 
      | "[|"; "|]" 
      | "[|"; sem_expr{el}; "|]" 
      | "{"; label_expr_list{el}; "}" 
      | "{"; "("; S{e}; ")"; "with"; label_expr_list{el}; "}" 
      | "{<"; ">}" 
      | "{<"; field_expr_list{fel}; ">}" 
      | "("; ")"
      | "("; S{e}; ":"; ctyp{t}; ")" 
      | "("; S{e}; ","; comma_expr{el}; ")" 
      | "("; S{e}; ";"; sequence{seq}; ")" 
      | "("; S{e}; ";"; ")" 
      | "("; S{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" 
      | "("; S{e}; ":>"; ctyp{t}; ")" 
      | "("; S{e}; ")" 
      | "begin"; sequence{seq}; "end" 
      | "begin"; "end" 
      | "("; "module"; module_expr{me}; ")" 
      | "("; "module"; module_expr{me}; ":"; package_type{pt}; ")"
      | `QUOTATION x
      | `ANT (("exp"|""|"anti"|"`bool" |"tup"|"seq"|"int"|"`int"
               |"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"
               |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s) 
      | `INT(_,s) | `INT32(_,s) | `INT64(_,s) | `FLO(_,s) | `CHAR(_,s) 
      | `STR(_,s) | `NATIVEINT(_,s)
      | TRY module_longident_dot_lparen{i};S{e}; ")" ] |}

  {:extend|Gram a:[TRY module_longident_dot_lparen -> raise Not_found | ident{s} -> s] |};
  t a "A.B.c";
 *)  
