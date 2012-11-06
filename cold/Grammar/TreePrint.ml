open Format
let pp = fprintf
let rec print_node decomp pref f t =
          let (s,sons) = decomp t in
          pp f "%s" s;
          if sons <> []
          then
            (let w = String.length s in
             let pref' = pref ^ (String.make (w + 1) ' ') in
             match sons with
             | t'::[] -> pp f "---%a" (print_node decomp (pref' ^ "  ")) t'
             | _ -> pp f "-%a" (print_sons "+-" decomp pref') sons)
          else ()
and print_sons start decomp pref f =
      function
      | [] -> ()
      | s::[] -> pp f "`-%a" (print_node decomp (pref ^ "  ")) s
      | s::sons ->
          pp f "%s%a@\n%s%a" start (print_node decomp (pref ^ "| ")) s pref
            (print_sons "|-" decomp pref) sons