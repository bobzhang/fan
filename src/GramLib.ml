open LibUtil;
open FanUtil;

(* test wheter revised or not hack*)  
let test_patt_lessminus =
    Gram.of_parser "test_patt_lessminus"
      (fun strm ->
        let rec skip_patt n =
          match stream_peek_nth n strm with
          [ Some (`KEYWORD "<-") -> n
          | Some (`KEYWORD ("[" | "[<")) ->
              skip_patt (ignore_upto "]" (n + 1) + 1)
          | Some (`KEYWORD "(") ->
              skip_patt (ignore_upto ")" (n + 1) + 1)
          | Some (`KEYWORD "{") ->
              skip_patt (ignore_upto "}" (n + 1) + 1)
          | Some (`KEYWORD ("as" | "::" | "," | "_"))
          | Some (`LIDENT _ | `UIDENT _) -> skip_patt (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (`KEYWORD prm) when prm = end_kwd -> n
          | Some (`KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (`KEYWORD "(") ->
              ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some (`KEYWORD "{") ->
              ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1)
          | Some _ -> ignore_upto end_kwd (n + 1)
          | None -> raise Stream.Failure ]
        in
        skip_patt 1);

(* very ad-hoc trick*)  
let is_revised ~expr ~sem_expr_for_list =
  try begin
      DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
      True
  end with [ Not_found -> False ];
    
let setup_op_parser entry p =
  Gram.setup_parser entry
    (parser
        [< (`KEYWORD x | `SYMBOL x, ti) when p x >] ->
          let _loc = Gram.token_location ti in
          <:expr< $lid:x >>);

let rec infix_kwds_filter = parser
  [ [< ((`KEYWORD "(", _) as tok); 'xs >] ->
    match xs with parser
      [ [< (`KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i), _loc);
             (`KEYWORD ")", _); 'xs >] ->
                [< (`LIDENT i, _loc); '(infix_kwds_filter xs) >]
        | [< 'xs >] ->
                [< tok; '(infix_kwds_filter xs) >] ]
    | [< x; 'xs >] -> [< x; '(infix_kwds_filter xs) >] ];
  
