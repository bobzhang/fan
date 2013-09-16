let a = {:pred|('a'|'b' as x) when x='a'|} 'a';;

if a then print_string "true" else print_string "false";;
(*true *)
