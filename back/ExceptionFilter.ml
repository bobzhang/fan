let add_debug_exp (e:exp) : exp =
  let _loc = loc_of e in
  let msg = "camlp4-debug: exc: %s at " ^ FanLoc.to_string _loc ^ "@." in
  {:exp|
      try $e  with
      [ XStream.Failure | Exit as exc -> raise exc (* FIXME *)
      | exc -> begin
          if Debug.mode "exc" then
            Format.eprintf $`str:msg (Printexc.to_string exc) else ();
          raise exc
        end ] |};

let rec map_case : case -> case  = with case 
  fun
  [ {| $m1 | $m2 |} ->
      {| $(map_case m1) | $(map_case m2) |}
  | {| $pat:p -> $e |} -> {|$pat:p -> $(add_debug_exp e)|}
  | {| $pat:p when $w -> $e |} ->
      {| $pat:p when $w -> $(add_debug_exp e) |}
  | m -> m ];


AstFilters.register_stru_filter ("exception",object
  inherit Objs.map as super;
  method! exp = fun
  [ {:exp@_loc| fun [ $m ] |}  -> {:exp| fun [ $(map_case m) ] |}
  | x -> super#exp x ];
  method! stru = fun
  [ {:stru| module Debug = $_ |} as st -> st
  | st -> super#stru st ];
end#stru);
