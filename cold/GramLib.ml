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
    (fun (__strm : _ XStream.t)  ->
       match XStream.peek __strm with
       | Some ((`KEYWORD x|`SYMBOL x),ti) when p x ->
           (XStream.junk __strm;
            (let _loc = Gram.token_location ti in
             Ast.ExId (_loc, (Ast.IdLid (_loc, x)))))
       | _ -> raise XStream.Failure)
let rec infix_kwds_filter (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some ((`KEYWORD "(",_) as tok) ->
      (XStream.junk __strm;
       (let xs = __strm in
        let (__strm :_ XStream.t)= xs in
        match XStream.peek __strm with
        | Some
            (`KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i),_loc)
            ->
            (XStream.junk __strm;
             (match XStream.peek __strm with
              | Some (`KEYWORD ")",_) ->
                  (XStream.junk __strm;
                   (let xs = __strm in
                    XStream.lcons (fun _  -> ((`LID i), _loc))
                      (XStream.slazy (fun _  -> infix_kwds_filter xs))))
              | _ -> raise (XStream.Error "")))
        | _ ->
            let xs = __strm in
            XStream.icons tok
              (XStream.slazy (fun _  -> infix_kwds_filter xs))))
  | Some x ->
      (XStream.junk __strm;
       (let xs = __strm in
        XStream.icons x (XStream.slazy (fun _  -> infix_kwds_filter xs))))
  | _ -> XStream.sempty
let parse_include_file rule file =
  if Sys.file_exists file
  then
    let ch = open_in file in
    let st = XStream.of_channel ch in Gram.parse rule (FanLoc.mk file) st
  else failwithf "@[file: %s not found@]@." file