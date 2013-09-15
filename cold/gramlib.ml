open LibUtil
let setup_op_parser entry p =
  Fgram.setup_parser entry
    (fun (__strm : _ XStream.t)  ->
       match XStream.peek __strm with
       | Some ((`Key x|`Sym x),_loc) when p x ->
           (XStream.junk __strm; (`Lid (_loc, x) : FAst.exp ))
       | _ -> raise XStream.NotConsumed)
let symbolchars =
  ['$';
  '!';
  '%';
  '&';
  '*';
  '+';
  '-';
  '.';
  '/';
  ':';
  '<';
  '=';
  '>';
  '?';
  '@';
  '^';
  '|';
  '~';
  '\\']
let symbolchar s i =
  let len = String.length s in
  try
    for j = i to len - 1 do
      if not (List.mem (s.[j]) symbolchars) then raise Not_found
    done;
    true
  with | Not_found  -> false