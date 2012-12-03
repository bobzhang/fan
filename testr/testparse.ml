Parser.start_parser_of_levels (Obj.magic expr) [ Gram.find_level expr ~position:(`Level "simple") ] 0;;
let level = Gram.find_level expr ~position:(`Level "simple");;
Parser.parser_of_symbol (Obj.magic expr)  (`Snterm (Obj.magic ident)) 0 (Gram.filter (Gram.lex _loc (Stream.of_string "A.B.c")));;

let u =
  Parser.parser_of_tree (Obj.magic expr) (0,`LA) level.lprefix (Gram.filter (Gram.lex _loc (Stream.of_string "A.B.c")));;

(Obj.magic u _loc : Ast.expr);;
- : Lib.Meta.Ast.expr = ExId (, IdUid (, "A"))
