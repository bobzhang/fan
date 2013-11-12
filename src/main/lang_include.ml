


%new{ (g:Gramf.t) include_quot };;

%unsafe_extend{ (g:Gramf.t)
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
  Ast_quotation.of_stru ~name:(Ns.lang, "include") ~entry:include_quot ()
end
(* local variables: *)
(* compile-command: "cd ../main_annot && pmake lang_include.cmo " *)
(* end: *)
