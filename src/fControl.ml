

{:new|(g:Fgram.t) item dot_namespace items |};;


{:unsafe_extend| (g:Fgram.t)
  item:
  ["default"; `STR(_,s) ->
    AstQuotation.set_default (FToken.resolve_name _loc (`Sub[],s))
  |"import"; dot_namespace{xs} ->
      FToken.paths := `Absolute  xs :: !FToken.paths
  | "filter"; `STR(_,s) ->
      AstFilters.use_implem_filter s
  | "lang_clear" ->
      (AstQuotation.clear_map();
       AstQuotation.clear_default())
  ]
  dot_namespace:
  [ `Uid i; "."; S{xs} -> i::xs
  | `Uid i -> [i]]
  items:
  [item; ";" -> ()
  |item; ";"; S -> ()
  | -> ()]
|};;

Fdir.register
("control",(fun loc c -> Fgram.parse_string ~loc  items c ));;
