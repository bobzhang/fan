


(* loc is lost *)
let binding_table (n,t) = {:binding| let $lid:n = $(output_byte_array t) |};
