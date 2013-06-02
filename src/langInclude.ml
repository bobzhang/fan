


{:new| (g:Fgram.t) include_quot |};;

{:unsafe_extend| (g:Fgram.t)
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
