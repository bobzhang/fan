with "match_case" fun
   [ {@_loc| $pat:p -> $e |} -> {| $pat:p -> fun () -> $e |}
   | {@_loc| $pat:p when $e -> $e1 |} -> {| $pat:p when $e -> fun () -> $e1 |}
   | {@_loc| $a1 | $a2 |} -> {| $(self#match_case a1) | $(self#match_case a2) |}
   | {@_loc| |} -> {| |}
   | {@_loc| $anti:x |} -> {| $(anti: add_context x "lettry" ) |} ];

with "patt" fun
  [ {@loc|$anti:x|} -> x];



















