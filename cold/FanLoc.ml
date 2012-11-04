open Format
open Location
open Lexing
type t = Location.t 
let dump_sel f x =
  let s =
    match x with
    | `start -> "`start"
    | `stop -> "`stop"
    | `both -> "`both"
    | _ -> "<not-printable>" in
  pp_print_string f s
let dump_pos f x =
  fprintf f "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
    x.pos_lnum x.pos_bol x.pos_cnum
let dump_long f x =
  fprintf f
    "@[<hov 2>{ file_name = %s ;@ start = %a (%d-%d);@ stop = %a (%d);@ ghost = %b@ } : FanLoc.t@]"
    (x.loc_start).pos_fname dump_pos x.loc_start
    ((x.loc_start).pos_cnum - (x.loc_start).pos_bol)
    ((x.loc_end).pos_cnum - (x.loc_start).pos_bol) dump_pos x.loc_end
    ((x.loc_end).pos_cnum - (x.loc_end).pos_bol) x.loc_ghost
let dump f x =
  fprintf f "[%S: %d:%d-%d %d:%d%t]" (x.loc_start).pos_fname
    (x.loc_start).pos_lnum ((x.loc_start).pos_cnum - (x.loc_start).pos_bol)
    ((x.loc_end).pos_cnum - (x.loc_start).pos_bol) (x.loc_end).pos_lnum
    ((x.loc_end).pos_cnum - (x.loc_end).pos_bol)
    (fun o  -> if x.loc_ghost then fprintf o " (ghost)" else ())
let start_pos name =
  { pos_fname = name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
let ghost_name = "ghost-location"
let ghost =
  {
    loc_start = (start_pos ghost_name);
    loc_end = (start_pos ghost_name);
    loc_ghost = true
  }
let mk file_name =
  {
    loc_start = (start_pos file_name);
    loc_end = (start_pos file_name);
    loc_ghost = false
  }
let of_tuple
  (file_name,start_line,start_bol,start_off,stop_line,stop_bol,stop_off,ghost)
  =
  {
    loc_start =
      {
        pos_fname = file_name;
        pos_lnum = start_line;
        pos_bol = start_bol;
        pos_cnum = start_off
      };
    loc_end =
      {
        pos_fname = file_name;
        pos_lnum = stop_line;
        pos_bol = stop_bol;
        pos_cnum = stop_off
      };
    loc_ghost = ghost
  }
let to_tuple
  {
    loc_start =
      { pos_fname; pos_lnum = start_line; pos_bol = start_bol;
        pos_cnum = start_off };
    loc_end =
      { pos_lnum = stop_line; pos_bol = stop_bol; pos_cnum = stop_off;_};
    loc_ghost = ghost }
  = (pos_fname, start_line, start_bol, start_off, stop_line, stop_bol,
  stop_off, ghost)
let better_file_name a b =
  match (a, b) with
  | ("","") -> a
  | ("",x) -> x
  | (x,"") -> x
  | ("-",x) -> x
  | (x,"-") -> x
  | (x,_) -> x
let of_lexbuf lb =
  let start = Lexing.lexeme_start_p lb and stop = Lexing.lexeme_end_p lb in
  let loc = { loc_start = start; loc_end = stop; loc_ghost = false } in loc
let of_lexing_position pos =
  let loc = { loc_start = pos; loc_end = pos; loc_ghost = false } in loc
let start_pos x = x.loc_start
let stop_pos x = x.loc_end
let merge a b =
  if a == b
  then a
  else
    (let r =
       match ((a.loc_ghost), (b.loc_ghost)) with
       | (false ,false ) -> { a with loc_end = (b.loc_end) }
       | (true ,true ) -> { a with loc_end = (b.loc_end) }
       | (true ,_) -> { a with loc_end = (b.loc_end) }
       | (_,true ) -> { b with loc_start = (a.loc_start) } in
     r)
let join x = { x with loc_end = (x.loc_start) }
let map f start_stop_both x =
  match start_stop_both with
  | `start -> { x with loc_start = (f x.loc_start) }
  | `stop -> { x with loc_end = (f x.loc_end) }
  | `both -> { x with loc_start = (f x.loc_start); loc_end = (f x.loc_end) }
let move_pos chars x = { x with pos_cnum = (x.pos_cnum + chars) }
let move s chars x = map (move_pos chars) s x
let move_line lines x =
  let move_line_pos x =
    { x with pos_lnum = (x.pos_lnum + lines); pos_bol = (x.pos_cnum) } in
  map move_line_pos `both x
let shift width x =
  { x with loc_start = (x.loc_end); loc_end = (move_pos width x.loc_end) }
let file_name x = (x.loc_start).pos_fname
let start_line x = (x.loc_start).pos_lnum
let stop_line x = (x.loc_end).pos_lnum
let start_bol x = (x.loc_start).pos_bol
let stop_bol x = (x.loc_end).pos_bol
let start_off x = (x.loc_start).pos_cnum
let stop_off x = (x.loc_end).pos_cnum
let is_ghost x = x.loc_ghost
let set_file_name s x =
  {
    x with
    loc_start = { (x.loc_start) with pos_fname = s };
    loc_end = { (x.loc_end) with pos_fname = s }
  }
let ghostify x = { x with loc_ghost = true }
let make_absolute x =
  let pwd = Sys.getcwd () in
  let old_name = (x.loc_start).pos_fname in
  if Filename.is_relative old_name
  then
    let new_name = Filename.concat pwd old_name in
    {
      x with
      loc_start = { (x.loc_start) with pos_fname = new_name };
      loc_end = { (x.loc_end) with pos_fname = new_name }
    }
  else x
let strictly_before x y =
  let b =
    ((x.loc_end).pos_cnum < (y.loc_start).pos_cnum) &&
      ((x.loc_end).pos_fname = (y.loc_start).pos_fname) in
  b
let to_string x =
  let (a,b) = ((x.loc_start), (x.loc_end)) in
  let res =
    sprintf "File \"%s\", line %d, characters %d-%d" a.pos_fname a.pos_lnum
      (a.pos_cnum - a.pos_bol) (b.pos_cnum - a.pos_bol) in
  if (x.loc_start).pos_lnum <> (x.loc_end).pos_lnum
  then
    sprintf "%s (end at line %d, character %d)" res (x.loc_end).pos_lnum
      (b.pos_cnum - b.pos_bol)
  else res
let print out x = pp_print_string out (to_string x)
let check x msg =
  if
    ((start_line x) > (stop_line x)) ||
      (((start_bol x) > (stop_bol x)) ||
         (((start_off x) > (stop_off x)) ||
            (((start_line x) < 0) ||
               (((stop_line x) < 0) ||
                  (((start_bol x) < 0) ||
                     (((stop_bol x) < 0) ||
                        (((start_off x) < 0) || ((stop_off x) < 0))))))))
  then
    (eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
     false)
  else true
exception Exc_located of t*exn
let _ =
  Printexc.register_printer
    (function
     | Exc_located (t,exn) ->
         Some
           (sprintf "Exc_located(%s,%s)" (to_string t)
              (Printexc.to_string exn))
     | _ -> None)
let name = ref "_loc"
let raise loc exc =
  match exc with
  | Exc_located (_,_) -> raise exc
  | _ -> raise (Exc_located (loc, exc))
let error_report (loc,s) =
  prerr_endline (to_string loc);
  (let (start_bol,stop_bol,start_off,stop_off) = ((start_bol loc),
     (stop_bol loc), (start_off loc), (stop_off loc)) in
   let abs_start_off = start_bol + start_off in
   let abs_stop_off = stop_bol + stop_off in
   let err_location =
     String.sub s abs_start_off ((abs_stop_off - abs_start_off) + 1) in
   prerr_endline (sprintf "err: ^%s^" err_location))
let string_loc = mk "<string>"