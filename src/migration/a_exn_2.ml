# exception Good_message of string list with sexp;;
exception Good_message of string list
# Exn.to_string (Good_message ["1";"2";"3"]) ;;
- : string = "(//toplevel//.Good_message (1 2 3))"
