(* open LibUtil; *)
open LibUtil.Stream;
let  u = (Gram.token_stream_of_string "A.B.C.d") ;
let  v = dup u;


{:extend.create|Gram c|}  ;

  with "ident"{:extend|Gram local:d;
 c:[  d {x}; "(" -> {| $uid:x |}   ]
                 d:[`UID x -> begin prerr_endline "d"; x end ]
|};
  
let p =  (fun (t,i) ->
         Format.eprintf "(%a:@\n%a)@." FanToken.print t  Grammar.Structure.pp_token_info i);

let f = Grammar.Comb.tryp (Gram.parse_origin_tokens c );  
(* Stream.iter (fun (t,_) -> print_string (FanToken.to_string t)) (Stream.take 2  u); *)
(* peek_nth  u 10; *)
(* f u; *)
(* dump p u; *)

f u;















