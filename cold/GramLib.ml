open LibUtil
open FanUtil
module Ast = Camlp4Ast
let test_patt_lessminus =
  Gram.of_parser "test_patt_lessminus"
    (fun strm  ->
       let rec skip_patt n =
                 match stream_peek_nth n strm with
                 | Some (`KEYWORD "<-",_) -> n
                 | Some (`KEYWORD ("["|"[<"),_) ->
                     skip_patt ((ignore_upto "]" (n + 1)) + 1)
                 | Some (`KEYWORD "(",_) ->
                     skip_patt ((ignore_upto ")" (n + 1)) + 1)
                 | Some (`KEYWORD "{",_) ->
                     skip_patt ((ignore_upto "}" (n + 1)) + 1)
                 | Some (`KEYWORD ("as"|"::"|","|"_"),_)|Some
                     ((`LIDENT _|`UIDENT _),_) -> skip_patt (n + 1)
                 | Some _|None  -> raise Stream.Failure
       and ignore_upto end_kwd n =
             match stream_peek_nth n strm with
             | Some (`KEYWORD prm,_) when prm = end_kwd -> n
             | Some (`KEYWORD ("["|"[<"),_) ->
                 ignore_upto end_kwd ((ignore_upto "]" (n + 1)) + 1)
             | Some (`KEYWORD "(",_) ->
                 ignore_upto end_kwd ((ignore_upto ")" (n + 1)) + 1)
             | Some (`KEYWORD "{",_) ->
                 ignore_upto end_kwd ((ignore_upto "}" (n + 1)) + 1)
             | Some _ -> ignore_upto end_kwd (n + 1)
             | None  -> raise Stream.Failure in
       skip_patt 1)
let is_revised ~expr  ~sem_expr_for_list:(x : _ Gram.t)  =
  try
    Gram.delete_rule expr
      [`Skeyword "[";
      `Snterm (Gram.obj (x : 'x Gram.t ));
      `Skeyword "::";
      `Snterm (Gram.obj (expr : 'expr Gram.t ));
      `Skeyword "]"];
    true
  with | Not_found  -> false
let setup_op_parser entry p =
  Gram.setup_parser entry
    (fun (__strm : _ Stream.t)  ->
       match Stream.peek __strm with
       | Some ((`KEYWORD x|`SYMBOL x),ti) when p x ->
           (Stream.junk __strm;
            (let _loc = Gram.token_location ti in
             Ast.ExId (_loc, (Ast.IdLid (_loc, x)))))
       | _ -> raise Stream.Failure)
let rec infix_kwds_filter (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some ((`KEYWORD "(",_) as tok) ->
      (Stream.junk __strm;
       (let xs = __strm in
        let (__strm :_ Stream.t)= xs in
        match Stream.peek __strm with
        | Some
            (`KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i),_loc)
            ->
            (Stream.junk __strm;
             (match Stream.peek __strm with
              | Some (`KEYWORD ")",_) ->
                  (Stream.junk __strm;
                   (let xs = __strm in
                    Stream.lcons (fun _  -> ((`LIDENT i), _loc))
                      (Stream.slazy (fun _  -> infix_kwds_filter xs))))
              | _ -> raise (Stream.Error "")))
        | _ ->
            let xs = __strm in
            Stream.icons tok (Stream.slazy (fun _  -> infix_kwds_filter xs))))
  | Some x ->
      (Stream.junk __strm;
       (let xs = __strm in
        Stream.icons x (Stream.slazy (fun _  -> infix_kwds_filter xs))))
  | _ -> Stream.sempty