
let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_comment %{token lexbuf}
 | @ocaml_lid("default"|"import"|"filter"|"lang_clear"|"require")
 | @ocaml_string
 | @ocaml_uid
 | @kwd_symbol("."|";")
 | @ocaml_eof
 | @default
};;

%create{item dot_namespace items };;

%extend{
  item:
  ["default"; Str s %{ (* FIXME*)
    begin 
      match Ast_quotation.resolve_name  (`Sub[],s)
      with
      | None ->
          Locf.failf _loc "DDSL `%s' can not be resolved" s
      | Some x -> 
          Ast_quotation.set_default x
    end}
  |"import"; dot_namespace as xs %{Ast_quotation.paths := `Absolute  xs :: !Ast_quotation.paths}
  | "filter"; Str s %{Ast_filters.use_implem_filter s}
  | "lang_clear" %{(Ast_quotation.clear_map(); Ast_quotation.clear_default())}
  ]
  dot_namespace:
  [ Uid i; "."; S as xs %{ i::xs}
  | Uid i %{ [i]}
  ]
  items:
  [item; ";" %{ ()}
  |item; ";"; S %{ ()}
  | %{ ()}
  ]
};;

let lexer = Lexing_util.adapt_to_stream token 
let () =
  Ast_quotation.register_unit_parser ~lexer
    (Tokenf.name_of_string "control", items)
    (* (Tokenf.name_of_string "control", *)
    (*  (fun loc _ c -> *)
    (*    Gramlib.parse_string ~loc  items ~lexer c )) *);;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/control.cmo " *)
(* end: *)
