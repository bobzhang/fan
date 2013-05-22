open LibUtil

let setup_op_parser entry p =
  Gram.setup_parser entry
    (fun (__strm : _ XStream.t)  ->
       match XStream.peek __strm with
       | Some ((`KEYWORD x|`SYMBOL x),_loc) when p x ->
           begin XStream.junk __strm; (`Lid (_loc, x) : FAst.exp ) end
       | _ -> raise XStream.Failure)

let rec infix_kwds_filter (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some ((`KEYWORD "(",_) as tok) ->
      begin
        XStream.junk __strm;
        (let xs = __strm in
         let (__strm :_ XStream.t)= xs in
         match XStream.peek __strm with
         | Some
             (`KEYWORD
                ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i),_loc)
             ->
             begin
               XStream.junk __strm;
               (match XStream.peek __strm with
                | Some (`KEYWORD ")",_) ->
                    begin
                      XStream.junk __strm;
                      (let xs = __strm in
                       XStream.lcons (fun _  -> ((`Lid i), _loc))
                         (XStream.slazy (fun _  -> infix_kwds_filter xs)))
                    end
                | _ -> raise (XStream.Error ""))
             end
         | _ ->
             let xs = __strm in
             XStream.icons tok
               (XStream.slazy (fun _  -> infix_kwds_filter xs)))
      end
  | Some x ->
      begin
        XStream.junk __strm;
        (let xs = __strm in
         XStream.icons x (XStream.slazy (fun _  -> infix_kwds_filter xs)))
      end
  | _ -> XStream.sempty