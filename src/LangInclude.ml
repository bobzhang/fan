open Ast 
let g = Gram.create_lexer ~annot:"include" ~keywords:[] ();;

{:create| (g:Gram.t) include_quot |};;

{:extend|
include_quot:
  [`STR(_,s) ->
    let keep = FanState.keep and cf = FanState.current_filters in
    {:save| keep cf ->  begin
      FanState.reset ();
      FanBasic.parse_include_file PreCast.Syntax.strus s;
    end
  |}
 ]
|};;
