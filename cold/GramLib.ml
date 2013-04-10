open LibUtil

let setup_op_parser entry p =
  Gram.setup_parser entry
    (fun (__strm : _ XStream.t)  ->
       match XStream.peek __strm with
       | Some ((`KEYWORD x|`SYMBOL x),_loc) when p x ->
           (XStream.junk __strm; `Lid (_loc, x))
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
                    XStream.lcons (fun _  -> ((`Lid i), _loc))
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