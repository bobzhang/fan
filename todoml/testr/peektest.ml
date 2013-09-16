
let a = Gram.mk "a";

  {:extend|Gram 
   a:[ PEEK "`"; "`";"["; "]" -> 1
     | "("; ")" -> 2]
  |};

 try
   ignore (Gram.parse_string a FanLoc.string_loc "`  []")
 with
   [e -> print_endline (Printexc.to_string e) ];
