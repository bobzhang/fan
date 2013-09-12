

open OUnit
let (===) = assert_equal



let get_tokens s =
  List.map fst
    (Flex_lib.list_of_string ~verbose:false s )
    
let test_empty_string _ =
  get_tokens {:str|""|}
    ===
  [`Str ""; `EOI]  

let test_escaped_string _ =
  get_tokens {:str|"a\n"|}
    ===
  [`Str "a\n"; `EOI]
    
let test_comment_string _ =
  get_tokens {:str|(*(**)*)|}
    ===
  [`COMMENT"(*(**)*)";`EOI]
