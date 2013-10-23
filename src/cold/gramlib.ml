let setup_op_parser entry p =
  Gramf.setup_parser entry
    (fun (__strm : _ Streamf.t)  ->
       match Streamf.peek __strm with
       | Some ((`Key (_,x)|`Sym (_,x)),_loc) when p x ->
           (Streamf.junk __strm; (`Lid (_loc, x) : FAst.exp ))
       | _ -> raise Streamf.NotConsumed)
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