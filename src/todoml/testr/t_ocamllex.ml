{:lexer| | ' ' '\t' '\n'  as x -> print_string x | _ as c -> print_char c  | ! -> print_string "end" |};;
