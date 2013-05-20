
let g = Gram.create_lexer ~annot:"include" ~keywords:[] ();;

{:create| (g:Gram.t) include_quot |};;

{:extend|
include_quot:
  [`STR(_,s) ->
    let (keep,cf) = FanState.((keep,current_filters)) in
    {:save| keep cf ->  begin
      FanState.reset ();
      FanBasic.parse_include_file Syntax.strus s;
    end
  |}
 ]
|};;
