let print_money currency_fmt sum = 
    Printf.printf "ammount due: %(%i%)@." currency_fmt sum;;
let print_dollar sum = print_money "$ %i" sum ;;

print_dollar 32;;

let print_money currency_fmt sum = 
    Format.fprintf Format.std_formatter "ammount due: %(%i%)" currency_fmt sum;;
let print_dollar sum = print_money "$ %i" sum ;;
print_dollar 32;
