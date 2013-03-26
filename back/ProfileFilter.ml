let decorate_binding decorate_fun = object
  inherit Objs.map as super;
  method! binding = fun
    [ `Bind (_loc,(`Id (_,`Lid (_,id)) as x),(`Fun (_,_) as e)) ->
      `Bind (_loc, x, (decorate_fun id e))
    | b -> super#binding b];
  end#binding;

let decorate decorate_fun = object (o)
  inherit Objs.map as super;
  method! stru = fun
    [ {:stru@_loc| let $rec:r $b |} ->
      {:stru| let $rec:r $(decorate_binding decorate_fun b) |}
    | st -> super#stru st ];
  method! exp = fun
    [ {:exp@_loc| let $rec:r $b in $e |} ->
      {:exp| let $rec:r $(decorate_binding decorate_fun b) in $(o#exp e) |}
    | {:exp@_loc| fun [ $_ ] |} as e -> decorate_fun "<fun>" e
    | e -> super#exp e ];
end;

let decorate_this_exp e id =
  let buf = Buffer.create 42 in
  let _loc = loc_of e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  {:exp| let () = Camlp4prof.count $`str:s in $e |};

let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_exp = decorate#exp in
  let decorate_case = decorate#case in
  fun
  [ {:exp@_loc| fun $p -> $e |} ->
      {:exp| fun $p -> $(decorate_fun id e) |}
  | {:exp@_loc| fun [ $m ] |} ->
      decorate_this_exp {:exp| fun [ $(decorate_case m) ] |} id
  | e -> decorate_this_exp (decorate_exp e) id ];

AstFilters.register_stru_filter("profile", (decorate decorate_fun)#stru);
