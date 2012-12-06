open Format;

(** The type of locations.  Note that, as for OCaml locations,
    character numbers in locations refer to character numbers in the
    parsed character stream, while line numbers refer to line
    numbers in the source file. The source file and the parsed
    character stream differ, for instance, when the parsed character
    stream contains a line number directive. The line number
    directive will only update the file-name field and the
    line-number field of the position. It makes therefore no sense
    to use character numbers with the source file if the sources
    contain line number directives. *)



type position = Lexing.position == {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
};

type t = Location.t == {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
};

(* Debug section *)
let dump_sel f x =
  let s =
    match x with
    [ `start -> "`start"
    | `stop  -> "`stop"
    | `both  -> "`both"
    | _      -> "<not-printable>" ] in pp_print_string f s;
  
let dump_pos f x =
  fprintf f "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
          x.pos_lnum x.pos_bol x.pos_cnum;
  
let dump_long f x =
  fprintf f
    "@[<hov 2>{ file_name = %s ;@ start = %a (%d-%d);@ stop = %a (%d);@ ghost = %b@ } : FanLoc.t@]"
    x.loc_start.pos_fname dump_pos x.loc_start (x.loc_start.pos_cnum - x.loc_start.pos_bol)
    (x.loc_end.pos_cnum - x.loc_start.pos_bol)  (* FIXME here*)
    dump_pos
    x.loc_end
    (x.loc_end.pos_cnum - x.loc_end.pos_bol) x.loc_ghost;

(** Print the location in a short format useful for debugging. *)  
let dump f x =
  fprintf f "[%S: %d:%d-%d %d:%d%t]"
    x.loc_start.pos_fname x.loc_start.pos_lnum
    (x.loc_start.pos_cnum - x.loc_start.pos_bol)
    (x.loc_end.pos_cnum - x.loc_start.pos_bol) x.loc_end.pos_lnum (x.loc_end.pos_cnum - x.loc_end.pos_bol)
    (fun o -> if x.loc_ghost then fprintf o " (ghost)" else ());

  
let start_pos name =
  { pos_fname=name ;
    pos_lnum = 1 ;
    pos_bol = 0 ;
    pos_cnum = 0 };

let ghost_name = "ghost-location";
(** The [ghost] location can be used when no location
        information is available. *)
let ghost =
  { 
    loc_start = start_pos ghost_name;
    loc_end   = start_pos ghost_name;
    loc_ghost     = true     };

(** Return a start location for the given file name.
    This location starts at the begining of the file. *)
let mk file_name =
  debug loc "mk %s@\n" file_name in
  { 
    loc_start     = start_pos file_name;
    loc_end      = start_pos file_name;
    loc_ghost     = false    };


(** Return a location from [(file_name, start_line, start_bol, start_off,
            stop_line,  stop_bol,  stop_off, ghost)]. *)
let of_tuple (file_name, start_line, start_bol, start_off,
                          stop_line,  stop_bol,  stop_off, ghost) =
  {
    loc_start     = {
      pos_fname=file_name;
      pos_lnum = start_line ;
      pos_bol = start_bol ;
      pos_cnum = start_off };
    loc_end      = {
      pos_fname = file_name;
      pos_lnum = stop_line;
      pos_bol = stop_bol;
      pos_cnum = stop_off};

    loc_ghost     = ghost };

(** Return [(file_name, start_line, start_bol, start_off,
            stop_line,  stop_bol,  stop_off, ghost)]. *)
let to_tuple = 
  fun [{loc_start=
        { pos_fname;
          pos_lnum = start_line ;
          pos_bol = start_bol ;
          pos_cnum  = start_off };
        loc_end=
        { pos_lnum = stop_line  ;
          pos_bol = stop_bol  ;
          pos_cnum = stop_off ;
          _};
        loc_ghost     = ghost } -> 
          (pos_fname, start_line, start_bol, start_off,
           stop_line,  stop_bol,  stop_off, ghost) ];


let better_file_name a b =
  match (a, b) with
  [ ("", "") -> a
  | ("", x)  -> x
  | (x, "")  -> x
  | ("-", x) -> x
  | (x, "-") -> x
  | (x, _)   -> x ];
    
(** Return a location from ocamllex buffer. *)
let of_lexbuf lb =
  let loc_start = Lexing.lexeme_start_p lb
  and loc_end  = Lexing.lexeme_end_p lb in
  let loc =
  { loc_start  ;
    loc_end   ;
    loc_ghost  = false } in
  debug loc "of_lexbuf: %a@\n" dump loc in
  loc;

let of_positions s e = {loc_start = s; loc_end = e ; loc_ghost = false};
let dummy_pos = Lexing.dummy_pos;  
(** Return a location where both positions are set the given position. *)
(* let of_lexing_position pos = *)
(*   let loc = *)
(*   { loc_start =  pos; *)
(*     loc_end =  pos; *)
(*     loc_ghost     = false } in *)
(*   debug loc "of_lexing_position: %a@\n" dump loc in *)
(*   loc; *)


(** Return the start position as a Lexing.position. *)
let start_pos x =  x.loc_start;

(** Return the stop position as a Lexing.position. *)  
let stop_pos x =  x.loc_end;

(** [merge loc1 loc2] Return a location that starts at [loc1] and end at
            [loc2]. *)  
let merge a b =
  if a == b then
    debug loc "trivial merge@\n" in
    a
  else
    let r =
      match (a.loc_ghost, b.loc_ghost) with
      [ (false, false) ->
        (* FIXME if a.file_name <> b.file_name then
          raise (Invalid_argument
            (sprintf "Loc.merge: Filenames must be equal: %s <> %s"
                    a.file_name b.file_name))                          *)
        (* else *)
          { (a) with loc_end = b.loc_end }
      | (true, true) -> { (a) with loc_end = b.loc_end }
      | (true, _) -> { (a) with loc_end = b.loc_end }
      | (_, true) -> { (b) with loc_start = a.loc_start } ]
    in debug loc "@[<hov 6>merge %a@ %a@ %a@]@\n" dump a dump b dump r in r;

(** The stop pos becomes equal to the start pos. *)
let join x = { (x) with loc_end = x.loc_start };
let join_end x = {(x) with loc_start = x.loc_end};
let map f start_stop_both x =
  match start_stop_both with
  [ `start -> { (x) with loc_start = f x.loc_start }
  | `stop  -> { (x) with loc_end  = f x.loc_end }
  | `both  -> { (x) with loc_start = f x.loc_start; loc_end  = f x.loc_end } ];

let move_pos chars x = { (x) with pos_cnum = x.pos_cnum + chars };
  
(** [move selector n loc]
    Return the location where positions are moved.
    Affected positions are chosen with [selector].
    Returned positions have their character offset plus [n]. *)
let move s chars x =
  debug loc "move %a %d %a@\n" dump_sel s chars dump x in
  map (move_pos chars) s x;

(** [move_line n loc] Return the location with the old line count plus [n].
            The "begin of line" of both positions become the current offset. *)
let move_line lines x =
  debug loc "move_line %d %a@\n" lines dump x in
  let move_line_pos x =
    { (x) with pos_lnum = x.pos_lnum + lines ; pos_bol = x.pos_cnum }
  in map move_line_pos `both x;
  

(** [shift n loc] Return the location where the new start position is the old
            stop position, and where the new stop position character offset is the
            old one plus [n]. *)  
let shift width x =
  { (x) with loc_start = x.loc_end ; loc_end = move_pos width x.loc_end };

(** Return the file name *)
let file_name  x = x.loc_start.pos_fname;
  
(** Return the line number of the begining of this location. *)
let start_line x = x.loc_start.pos_lnum;

(** Return the line number of the ending of this location. *)  
let stop_line  x = x.loc_end.pos_lnum;

(** Returns the number of characters from the begining of the stream
    to the begining of the line of location's begining. *)
 let start_bol  x = x.loc_start.pos_bol;
   
(** Returns the number of characters from the begining of the stream
            to the begining of the line of location's ending. *)
let stop_bol   x = x.loc_end.pos_bol;

(** Returns the number of characters from the begining of the stream
            of the begining of this location. *)  
let start_off  x = x.loc_start.pos_cnum;

(** Return the number of characters from the begining of the stream
            of the ending of this location. *)  
let stop_off   x = x.loc_end.pos_cnum;

(** Generally, return true if this location does not come
    from an input stream. *)
let is_ghost   x = x.loc_ghost;

(** Return the location with the give file name *)
let set_file_name s x =
  debug loc "set_file_name: %a@\n" dump x in
  { (x) with
    loc_start = {(x.loc_start) with pos_fname = s };
    loc_end = {(x.loc_end) with pos_fname = s }
  };

(** Return the associated ghost location. *)
let ghostify x =
  debug loc "ghostify: %a@\n" dump x in
  { (x) with loc_ghost = true };

(** Return the location with an absolute file name. *)
let make_absolute x =
  debug loc "make_absolute: %a@\n" dump x in
  let pwd = Sys.getcwd () in
  let old_name = x.loc_start.pos_fname in 
  if Filename.is_relative old_name then
    let new_name = Filename.concat pwd old_name in  
    { (x) with
      loc_start = {(x.loc_start) with pos_fname = new_name};
      loc_end = {(x.loc_end) with pos_fname = new_name}
    }
  else x;

(** [strictly_before loc1 loc2] true if the stop position of [loc1] is
            strictly_before the start position of [loc2].
 *)
let strictly_before x y =
  let b = x.loc_end.pos_cnum < y.loc_start.pos_cnum && x.loc_end.pos_fname = y.loc_start.pos_fname in
  debug loc "%a [strictly_before] %a => %b@\n" dump x dump y b in
  b;

(** Same as {!print} but return a string instead of printting it. *)
let to_string x = begin
  let (a, b) = (x.loc_start, x.loc_end) in
  let res = sprintf "File \"%s\", line %d, characters %d-%d"
      a.pos_fname a.pos_lnum
      (a.pos_cnum - a.pos_bol)
      (b.pos_cnum - a.pos_bol) in
  if x.loc_start.pos_lnum  <> x.loc_end.pos_lnum then
    sprintf "%s (end at line %d, character %d)"
            res x.loc_end.pos_lnum (b.pos_cnum - b.pos_bol)
  else res
end;

(** Print the location into the formatter in a format suitable for error
    reporting. *)
let print out x = pp_print_string out (to_string x);

let check x msg =
  if ((start_line x) > (stop_line x) ||
      (start_bol x) > (stop_bol x) ||
      (start_off x) > (stop_off x) ||
      (start_line x) < 0 || (stop_line x) < 0 ||
      (start_bol x) < 0 || (stop_bol x) < 0 ||
      (start_off x) < 0 ||  (stop_off x) < 0)
      (* Here, we don't check
        (start_off x) < (start_bol x) || (stop_off x) < (start_bol x)
        since the lexer is called on antiquotations, with off=0, but line and bolpos
        have "correct" lets *)
  then begin
    eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
    false
  end
  else true;

  
(** [Exc_located loc e] is an encapsulation of the exception [e] with
            the input location [loc]. To be used in quotation expanders
            and in grammars to specify some input location for an error.
            Do not raise this exception directly: rather use the following
            function [Loc.raise]. *)
exception Exc_located of t and exn;

let _ = begin
  Printexc.register_printer (fun
  [Exc_located (t, exn) ->
    Some (sprintf "Exc_located(%s,%s)" (to_string t ) (Printexc.to_string exn))
  |_ -> None ])
  end;
(** The name of the location variable used in grammars and in
    the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
let name = ref "_loc";

(** [raise loc e], if [e] is already an [Exc_located] exception,
            re-raise it, else raise the exception [Exc_located loc e]. *)
let raise loc exc =
  match exc with
  [ Exc_located (_, _) -> raise exc
  | _ -> raise (Exc_located loc exc) ];


let error_report (loc,s) = begin
  prerr_endline (to_string loc);
  let (start_bol,stop_bol,
         start_off, stop_off) =
    ( (start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc)
           ) in
  let abs_start_off = start_bol + start_off in
  let abs_stop_off = stop_bol + stop_off in
  let err_location = String.sub s abs_start_off
      (abs_stop_off - abs_start_off + 1) in
  prerr_endline (sprintf "err: ^%s^" err_location);
end ;

let string_loc = mk "<string>";
    
