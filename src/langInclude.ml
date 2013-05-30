
let g = Fgram.create_lexer ~annot:"include" ~keywords:[] ();;

{:create| (g:Fgram.t) include_quot |};;

{:extend|
include_quot:
  [`STR(_,s) ->
    let (keep,cf) = FState.((keep,current_filters)) in
    {:save| keep cf ->  begin
      FState.reset ();
      Fgram.parse_include_file Fsyntax.strus s;
    end
  |}
 ]
|};;
