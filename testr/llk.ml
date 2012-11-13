

let a = Gram.mk "a" ; 
{:extend|Gram LOCAL:e;
  a:
  ["a";"b";"c";"d" -> 1
  | e -> 2]
  e:
  ["a";"b" -> 2]
|};

print_int (Gram.parse_string a FanLoc.string_loc "a b c d");
print_int (Gram.parse_string a FanLoc.string_loc "a b ");  
