

let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_comment %{token lexbuf}
 | @ocaml_string
 | @ocaml_eof
 | @default
}

let lexer = Lexing_util.adapt_to_stream token ;;


%create{ include_quot };;

%extend{ 
include_quot:
  [Str s %{ (* FIXME *)
    let (keep,cf) = State.((keep,current_filters)) in
    %save{ keep cf %{begin
      State.reset ();
      Gramlib.parse_include_file Syntaxf.strus s;
    end
  }}}
 ]
};;

let _ = begin
  Ast_quotation.of_stru
    ~lexer
    ~name:{domains = Ns.lang; name =  "include"} ~entry:include_quot ()
end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_include.cmo " *)
(* end: *)
