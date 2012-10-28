open LibUtil
open FanUtil
module Ast = Camlp4Ast
let test_patt_lessminus =
  (Gram.of_parser "test_patt_lessminus" (
    (fun strm ->
      let rec skip_patt n = begin match (stream_peek_nth n strm) with
        | Some (`KEYWORD "<-",_) ->   n
        | Some (`KEYWORD ("["|"[<"),_) ->
            (skip_patt ( (( (ignore_upto "]" ( (n + 1) )) ) + 1) ))
        | Some (`KEYWORD "(",_) ->
            (skip_patt ( (( (ignore_upto ")" ( (n + 1) )) ) + 1) ))
        | Some (`KEYWORD "{",_) ->
            (skip_patt ( (( (ignore_upto "}" ( (n + 1) )) ) + 1) ))
        | Some (`KEYWORD ("as"|"::"|","|"_"),_)|Some
          ((`LIDENT _|`UIDENT _),_) ->   (skip_patt ( (n + 1) ))
        | Some _|None  ->   (raise Stream.Failure ) end and ignore_upto
        end_kwd n = begin match (stream_peek_nth n strm) with
        | Some (`KEYWORD prm,_) when (prm = end_kwd) ->   n
        | Some (`KEYWORD ("["|"[<"),_) ->
            (ignore_upto end_kwd ( (( (ignore_upto "]" ( (n + 1) )) ) + 1) ))
        | Some (`KEYWORD "(",_) ->
            (ignore_upto end_kwd ( (( (ignore_upto ")" ( (n + 1) )) ) + 1) ))
        | Some (`KEYWORD "{",_) ->
            (ignore_upto end_kwd ( (( (ignore_upto "}" ( (n + 1) )) ) + 1) ))
        | Some _ ->   (ignore_upto end_kwd ( (n + 1) ))
        | None  ->   (raise Stream.Failure ) end in
      (skip_patt 1)) ))
let is_revised ~expr  ~sem_expr_for_list  = begin try
  begin
    (Gram.delete_rule expr ( [`Skeyword ("[");`Snterm
      ((Gram.obj ( (sem_expr_for_list :'sem_expr_for_list Gram.t  ) )));`Skeyword
      ("::");`Snterm ((Gram.obj ( (expr :'expr Gram.t  ) )));`Skeyword ("]")]
      ));
    true
    end
  with
  | Not_found  ->   false end
let setup_op_parser entry p =
  (Gram.setup_parser entry (
    (fun (__strm : _ Stream.t ) -> begin match (Stream.peek __strm) with
      | Some ((`KEYWORD x|`SYMBOL x),ti) when (p x) ->
          begin
          (Stream.junk __strm);
          let _loc = (Gram.token_location ti) in
          Ast.ExId ((_loc,( Ast.IdLid ((_loc,x)) )))
          end
      | _ ->   (raise Stream.Failure ) end) ))
let rec infix_kwds_filter (__strm : _ Stream.t ) = begin match
  (Stream.peek __strm) with
  | Some ((`KEYWORD "(",_) as tok) ->
      begin
      (Stream.junk __strm);
      let xs = __strm in
      let (__strm : _ Stream.t ) = xs in begin match (Stream.peek __strm)
        with
        | Some
          (`KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i),_loc)
          ->
            begin
            (Stream.junk __strm);
            begin
            match
            (Stream.peek __strm)
            with
            | Some (`KEYWORD ")",_) ->
                begin
                (Stream.junk __strm);
                let xs = __strm in
                (Stream.lcons ( (fun _ -> (`LIDENT (i),_loc)) ) (
                  (Stream.slazy ( (fun _ -> (infix_kwds_filter xs)) )) ))
                end
            | _ ->   (raise ( Stream.Error ("") ))
            end
            end
        | _ ->
            let xs = __strm in
            (Stream.icons tok (
              (Stream.slazy ( (fun _ -> (infix_kwds_filter xs)) )) ))
        end
      end
  | Some x ->
      begin
      (Stream.junk __strm);
      let xs = __strm in
      (Stream.icons x ( (Stream.slazy ( (fun _ -> (infix_kwds_filter xs)) ))
        ))
      end
  | _ ->   (raise Stream.Failure ) end