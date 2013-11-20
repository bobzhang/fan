

let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_comment %{token lexbuf}
 | @ocaml_string
 | @ocaml_eof
 | @default
}

let lexer = Lexing_util.adapt_to_stream token
    
(* let g = *)
(*   Gramf.create_lexer ~annot:"include" ~keywords:[] ();; *)
;;    
%create{ (* (g:Gramf.t) *) include_quot };;

%extend{ (* (g:Gramf.t) *)
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
    ~name:(Ns.lang, "include") ~entry:include_quot ()
end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_include.cmo " *)
(* end: *)
