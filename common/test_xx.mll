


{
}
rule token = parse
| (('l'|'L' as x)?  )  "a"
    { print_string "a\n";
      print_string x ;
      print_string "b\n";
      "ghosgho"}

{
let f =  token (Lexing.from_string "a")
}  
