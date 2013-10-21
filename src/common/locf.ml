
open Format

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


(**Warning: see [Ast2pt.unsafe_loc_of] before you make any change *)
type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type t = Location.t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}


let pp_print_position f x =
  fprintf f "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
          x.pos_lnum x.pos_bol x.pos_cnum
  
let start_pos name = {
  pos_fname=name ;
  pos_lnum = 1 ;
  pos_bol = 0 ;
  pos_cnum = 0 }

let ghost_name = "ghost-location"
(** The [ghost] location can be used when no location
        information is available. *)
let ghost = { 
  loc_start = start_pos ghost_name;
  loc_end   = start_pos ghost_name;
  loc_ghost     = true     }

(** Return a start location for the given file name.
    This location starts at the begining of the file. *)
let mk file_name = { 
  loc_start     = start_pos file_name;
  loc_end      = start_pos file_name;
  loc_ghost     = false    }



(** Note that the filename was not taken into account *)    
let max_pos (x:position) (y:position) =
  if x.pos_cnum > y.pos_cnum then
    x
  else y
let min_pos (x:position) (y:position) =
  if x.pos_cnum < y.pos_cnum then
    x
  else y
      
(** [merge loc1 loc2] Return a location that starts at [loc1] and end at
            [loc2]. *)  
let merge a b =
  if a == b then a
  else
    match (a,b) with
    |{loc_ghost = false; loc_start = a0; loc_end = a1 },
      {loc_ghost = false; loc_start = b0; loc_end = b1 } ->
        {loc_ghost = false;
         loc_start= min_pos a0 b0;
         loc_end = max_pos a1 b1
       }
    | {loc_ghost = true; _},
        {loc_ghost = true; _}  
    | {loc_ghost = true; _}, _ -> {a with loc_end = b.loc_end}
    | {loc_ghost = _; _},{loc_ghost= true;_} ->
        {b with loc_start = a.loc_start  }

(** Return the file name *)
let file_name  x = x.loc_start.pos_fname


(** Generally, return true if this location does not come
    from an input stream. *)
let is_ghost   x = x.loc_ghost

(** Return the location with the give file name *)
let set_file_name s x =
  {x with
   loc_start = {x.loc_start with pos_fname = s };
   loc_end = {x.loc_end with pos_fname = s }
  }

(** Return the associated ghost location. *)
let ghostify x =
  { x with loc_ghost = true }

(** Return the location with an absolute file name. *)
let make_absolute x =
  let pwd = Sys.getcwd () in
  let old_name = x.loc_start.pos_fname in 
  if Filename.is_relative old_name then
    let new_name = Filename.concat pwd old_name in  
    { x with
      loc_start = {(x.loc_start) with pos_fname = new_name};
      loc_end = {(x.loc_end) with pos_fname = new_name}
    }
  else x

(** [strictly_before loc1 loc2] true if the stop position of [loc1] is
            strictly_before the start position of [loc2].
 *)
let strictly_before x y =
   x.loc_end.pos_cnum < y.loc_start.pos_cnum
    && x.loc_end.pos_fname = y.loc_start.pos_fname 


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
end

(** Print the location into the formatter in a format suitable for error
    reporting. *)
let print out x = pp_print_string out (to_string x)
    
let pp_print_t = print




    
let check (x:t) msg =
  if  x.loc_start.pos_lnum > x.loc_end.pos_lnum  ||
      x.loc_start.pos_bol > x.loc_end.pos_bol  ||
      x.loc_start.pos_cnum > x.loc_end.pos_cnum ||
      x.loc_start.pos_lnum < 0 || x.loc_end.pos_lnum < 0  ||
      x.loc_start.pos_bol < 0 || x.loc_end.pos_bol < 0 ||
      x.loc_start.pos_cnum < 0 || x.loc_end.pos_cnum < 0 
      (* Here, we don't check
        (start_off x) < (start_bol x) || (stop_off x) < (start_bol x)
        since the lexer is called on antiquotations, with off=0, but line and bolpos
        have "correct" lets *)
  then begin
    eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
    false
  end
  else true

  
(** [Exc_located loc e] is an encapsulation of the exception [e] with
    the input location [loc]. To be used in quotation expanders
    and in grammars to specify some input location for an error.
    Do not raise this exception directly: rather use the following
    function [Locf.raise]. *)
exception Exc_located of t * exn


(** [raise loc e], if [e] is already an [Exc_located] exception,
    re-raise it, else raise the exception [Exc_located loc e]. *)
let raise loc exc =
  match exc with
  | Exc_located (_, _) -> raise exc
  | _ -> raise (Exc_located (loc, exc)) 

    
(** The name of the location variable used in grammars and in
    the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
let name = ref "_loc"


(* error reporting for string parsing *)
let error_report (loc,s) = begin
  prerr_endline (to_string loc);
  let (start_bol,stop_bol,
       start_off, stop_off) =
    (loc.loc_start.pos_bol,
     loc.loc_end.pos_bol,
     loc.loc_start.pos_cnum,
     loc.loc_end.pos_cnum) in
  let abs_start_off = start_bol + start_off in
  let abs_stop_off = stop_bol + stop_off in
  let err_location = String.sub s abs_start_off
      (abs_stop_off - abs_start_off + 1) in
  prerr_endline (sprintf "err: ^%s^" err_location);
end 

let string_loc = mk "<string>"
    
let failf loc fmt =
  Format.ksprintf (fun s -> raise loc (Failure s))   fmt 

module Ops = struct 
  let (<+>) = merge
end

let () =     
  Printexc.register_printer @@
    function
      | Exc_located (loc, exn) ->
          Some (Format.sprintf "%s:@\n%s" (to_string loc) (Printexc.to_string exn))
      | _ -> None 



(* local variables: *)
(* compile-command: "cd .. && pmake common/locf.cmo" *)
(* end: *)
