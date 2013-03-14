type f1 = [ `App of (f1* f1) | `U of string] 
type 'a u = [> `App of (int* int) | 'a] -> 'a 