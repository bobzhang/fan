for a = 3 to 10 do
  let u = 4;
  print_int a;
  print_int u;
done;

let a = ref 3;

while !a >0  do
  let u = 3;
  decr a;
  print_int u;
  print_newline ();
done;
