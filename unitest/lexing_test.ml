

open OUnit

let test_empty_string _ =
  assert_equal
    (List.map fst
       (Flex_lib.list_of_string ~verbose:false
          {:str|""|}))
    [`Str ""; `EOI]  
