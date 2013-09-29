Objs.map_row_field begin fun 
  [ {:row_field| $vrn:x of loc |} -> {:row_field| $vrn:x |}
  | {:row_field| $vrn:x of (loc * $y ) |}->
      match y with
      [ {:ctyp| $_ * $_ |} -> {:row_field| $vrn:x of $tup:y |}
      | _ -> {:row_field| $vrn:x of $y |}]
      | x -> x ]
end
