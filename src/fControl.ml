

{:new|(g:Fgram.t) item dot_namespace items |};;

{:unsafe_extend| (g:Fgram.t)
  item:
  ["default"; `Str s -> (* FIXME*)
    Ast_quotation.set_default (Ast_quotation.resolve_name _loc (`Sub[],s))
  |"import"; dot_namespace{xs} ->
      Ast_quotation.paths := `Absolute  xs :: !Ast_quotation.paths
  | "filter"; `Str s ->
      Ast_filters.use_implem_filter s
  | "lang_clear" ->
      (Ast_quotation.clear_map();
       Ast_quotation.clear_default())
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
