open TreePrint;
type t = [Node of int and list t];
let decomp = fun
    [Node i ls -> (string_of_int i , ls)];
let f = Format.std_formatter;

print_node  decomp ""
    f (Node 3 [Node 4 [Node 10 []]; Node 5 []; Node 6 [Node 3 []]])  ;


  
pp f "@."  ;
print_sons
    "|-"
    decomp
    "" 
    f
    [Node 4 [Node 10 []]; Node 5 []; Node 6 [Node 3 []]]  ;

pp f "@."  ;
