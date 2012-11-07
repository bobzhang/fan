open LibUtil
module Ast = Camlp4Ast
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