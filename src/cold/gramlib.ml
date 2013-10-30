let setup_op_parser entry p =
  Gramf.setup_parser entry
    (fun (__strm : _ Streamf.t)  ->
       match Streamf.peek __strm with
       | Some (`Key x|`Sym x) when p x.txt ->
           (Streamf.junk __strm;
            (let _loc = x.loc in (`Lid (_loc, (x.txt)) : FAst.exp )))
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
let eoi_entry entry =
  let open! Gstru in
    let g = Gramf.gram_of_entry entry in
    let entry_eoi = Gramf.mk_dynamic g ((Gramf.name entry) ^ "_eoi") in
    Gramf.extend_single (entry_eoi : 'entry_eoi Gramf.t )
      (None,
        ((None, None,
           [([`Nterm (Gramf.obj (entry : 'entry Gramf.t ));
             `Token
               (((function | `EOI _ -> true | _ -> false)),
                 (3448991, `Empty), "`EOI")],
              ("x\n",
                (Gramf.mk_action
                   (fun ~__fan_1:_  ~__fan_0:(x : 'entry)  (_loc : Locf.t) 
                      -> (x : 'entry_eoi )))))]) : Gramf.olevel ));
    entry_eoi
