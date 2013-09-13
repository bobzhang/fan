

{:new|(g:Fgram.t) item dot_namespace items |};;

{:unsafe_extend| (g:Fgram.t)
  item:
  ["default"; `Str s -> (* FIXME*)
    AstQuotation.set_default (AstQuotation.resolve_name _loc (`Sub[],s))
  |"import"; dot_namespace{xs} ->
      AstQuotation.paths := `Absolute  xs :: !AstQuotation.paths
  | "filter"; `Str s ->
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
