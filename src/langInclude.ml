
let g = Gram.create_lexer ~annot:"include" ~keywords:[] ();;

{:create| (g:Gram.t) include_quot |};;

{:extend|
include_quot:
  [`STR(_,s) ->
    let (keep,cf) = FState.((keep,current_filters)) in
    {:save| keep cf ->  begin
      FState.reset ();
      Gram.parse_include_file Fsyntax.strus s;
    end
  |}
 ]
|};;
