open Format

type pos = {line:int; bol:int; off:int}

type t = {
                                                                 file_name:
                                                                  string;
                                                                 start:pos;
                                                                 stop:pos;
                                                                 ghost:bool}


let dump_sel =
 fun f ->
  fun x ->
   let s =
    (match x with
     | `start -> "`start"
     | `stop -> "`stop"
     | `both -> "`both"
     | _ -> "<not-printable>") in
   (pp_print_string f s)

let dump_pos =
                           fun f ->
                            fun x ->
                             (fprintf f
                               "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
                               ( x.line ) ( x.bol ) ( x.off ))

let dump_long =
                                                                 fun f ->
                                                                  fun x ->
                                                                   (fprintf f
                                                                    "@[<hov 2>{ file_name = %s ;@ start = %a (%d-%d);@ stop = %a (%d);@ ghost = %b@ } : Loc.t@]"
                                                                    (
                                                                    x.file_name
                                                                    )
                                                                    dump_pos
                                                                    ( 
                                                                    x.start )
                                                                    (
                                                                    ((
                                                                    (x.start).off
                                                                    ) - (
                                                                    (x.start).bol
                                                                    )) ) (
                                                                    ((
                                                                    (x.stop).off
                                                                    ) - (
                                                                    (x.start).bol
                                                                    )) )
                                                                    dump_pos
                                                                    ( 
                                                                    x.stop )
                                                                    (
                                                                    ((
                                                                    (x.stop).off
                                                                    ) - (
                                                                    (x.stop).bol
                                                                    )) ) (
                                                                    x.ghost
                                                                    ))


let dump =
 fun f ->
  fun x ->
   (fprintf f "[%S: %d:%d-%d %d:%d%t]" ( x.file_name ) ( (x.start).line ) (
     (( (x.start).off ) - ( (x.start).bol )) ) (
     (( (x.stop).off ) - ( (x.start).bol )) ) ( (x.stop).line ) (
     (( (x.stop).off ) - ( (x.stop).bol )) ) (
     fun o -> if x.ghost then ( (fprintf o " (ghost)") ) else () ))


let start_pos = {line = 1; bol = 0; off = 0}

let ghost =
                                               {file_name = "ghost-location";
                                                start = start_pos;
                                                stop = start_pos;
                                                ghost = true }

let mk =
                                                                 fun file_name ->
                                                                  {file_name =
                                                                    file_name;
                                                                   start =
                                                                    start_pos;
                                                                   stop =
                                                                    start_pos;
                                                                   ghost =
                                                                    false }


let of_tuple =
 fun (file_name, start_line, start_bol, start_off, stop_line, stop_bol,
      stop_off, ghost) ->
  {file_name = file_name;
   start = {line = start_line; bol = start_bol; off = start_off};
   stop = {line = stop_line; bol = stop_bol; off = stop_off}; ghost = ghost}


let to_tuple =
 fun {file_name = file_name;
  start = {line = start_line; bol = start_bol; off = start_off};
  stop = {line = stop_line; bol = stop_bol; off = stop_off};
  ghost = ghost} ->
  (file_name, start_line, start_bol, start_off, stop_line, stop_bol,
   stop_off, ghost)

let pos_of_lexing_position =
                      fun p ->
                       let pos =
                        {line = ( p.Lexing.pos_lnum );
                         bol = ( p.Lexing.pos_bol );
                         off = ( p.Lexing.pos_cnum )} in
                       pos

let pos_to_lexing_position =
                             fun p ->
                              fun file_name ->
                               {Lexing.pos_fname = file_name;
                                pos_lnum = ( p.line ); pos_bol = ( p.bol );
                                pos_cnum = ( p.off )}

let better_file_name =
                                                        fun a ->
                                                         fun b ->
                                                          (match (a, b) with
                                                           | ("", "") -> a
                                                           | ("", x) -> x
                                                           | (x, "") -> x
                                                           | ("-", x) -> x
                                                           | (x, "-") -> x
                                                           | (x, _) -> x)


let of_lexbuf =
 fun lb ->
  let start = (Lexing.lexeme_start_p lb)
  and stop = (Lexing.lexeme_end_p lb) in
  let loc =
   {file_name = (
     (better_file_name ( start.Lexing.pos_fname ) ( stop.Lexing.pos_fname ))
     ); start = ( (pos_of_lexing_position start) );
    stop = ( (pos_of_lexing_position stop) ); ghost = false } in
  loc

let of_lexing_position =
        fun pos ->
         let loc =
          {file_name = ( pos.Lexing.pos_fname );
           start = ( (pos_of_lexing_position pos) );
           stop = ( (pos_of_lexing_position pos) ); ghost = false } in
         loc

let to_ocaml_location =
               fun x ->
                {Location.loc_start = (
                  (pos_to_lexing_position ( x.start ) ( x.file_name )) );
                 loc_end = (
                  (pos_to_lexing_position ( x.stop ) ( x.file_name )) );
                 loc_ghost = ( x.ghost )}

let of_ocaml_location =
                                            fun {Location.loc_start = a;
                                             loc_end = b;
                                             loc_ghost = g} ->
                                             let res =
                                              {file_name = (
                                                (better_file_name (
                                                  a.Lexing.pos_fname ) (
                                                  b.Lexing.pos_fname )) );
                                               start = (
                                                (pos_of_lexing_position a) );
                                               stop = (
                                                (pos_of_lexing_position b) );
                                               ghost = g} in
                                             res

let start_pos =
                                                   fun x ->
                                                    (pos_to_lexing_position (
                                                      x.start ) ( x.file_name
                                                      ))

let stop_pos =
                                                           fun x ->
                                                            (pos_to_lexing_position
                                                              ( x.stop ) (
                                                              x.file_name ))


let merge =
 fun a ->
  fun b ->
   if (a == b) then a
   else
    let r =
     (match (( a.ghost ), ( b.ghost )) with
      | (false, false) -> {a with stop = ( b.stop )}
      | (true, true) -> {a with stop = ( b.stop )}
      | (true, _) -> {a with stop = ( b.stop )}
      | (_, true) -> {b with start = ( a.start )}) in
    r

let join = fun x -> {x with stop = ( x.start )}

let map =
                                                         fun f ->
                                                          fun start_stop_both ->
                                                           fun x ->
                                                            (match
                                                               start_stop_both with
                                                             | `start ->
                                                                {x with
                                                                 start = (
                                                                  (f (
                                                                    x.start
                                                                    )) )}
                                                             | `stop ->
                                                                {x with
                                                                 stop = (
                                                                  (f ( 
                                                                    x.stop ))
                                                                  )}
                                                             | `both ->
                                                                {x with
                                                                 start = (
                                                                  (f (
                                                                    x.start
                                                                    )) );
                                                                 stop = (
                                                                  (f ( 
                                                                    x.stop ))
                                                                  )})


let move_pos = fun chars -> fun x -> {x with off = ( (( x.off ) + chars) )}


let move = fun s -> fun chars -> fun x -> (map ( (move_pos chars) ) s x)


let move_line =
 fun lines ->
  fun x ->
   let move_line_pos =
    fun x -> {x with line = ( (( x.line ) + lines) ); bol = ( x.off )} in
   (map move_line_pos `both x)

let shift =
                                 fun width ->
                                  fun x ->
                                   {x with start = ( x.stop );
                                    stop = ( (move_pos width ( x.stop )) )}


let file_name = fun x -> x.file_name

let start_line =
                                       fun x -> (x.start).line

let stop_line =
                                                                 fun x ->
                                                                  (x.stop).line


let start_bol = fun x -> (x.start).bol

let stop_bol = fun x -> (x.stop).bol


let start_off = fun x -> (x.start).off

let stop_off = fun x -> (x.stop).off


let is_ghost = fun x -> x.ghost

let set_file_name =
                                  fun s -> fun x -> {x with file_name = s}


let ghostify = fun x -> {x with ghost = true }

let make_absolute =
                                                 fun x ->
                                                  let pwd = (Sys.getcwd () ) in
                                                  if (Filename.is_relative (
                                                       x.file_name )) then
                                                   {x with
                                                    file_name = (
                                                     (Filename.concat pwd (
                                                       x.file_name )) )}
                                                  else x

let strictly_before =
                                                           fun x ->
                                                            fun y ->
                                                             let b =
                                                              ((
                                                                ((
                                                                  (x.stop).off
                                                                  ) < (
                                                                  (y.start).off
                                                                  )) ) && (
                                                                ((
                                                                  x.file_name
                                                                  ) = (
                                                                  y.file_name
                                                                  )) )) in
                                                             b

let to_string =
                                                                 fun x ->
                                                                  let 
                                                                   (a, b) =
                                                                   (( 
                                                                    x.start
                                                                    ), (
                                                                    x.stop )) in
                                                                  let res =
                                                                   (sprintf
                                                                    "File \"%s\", line %d, characters %d-%d"
                                                                    (
                                                                    x.file_name
                                                                    ) (
                                                                    a.line )
                                                                    (
                                                                    (( 
                                                                    a.off ) -
                                                                    ( a.bol
                                                                    )) ) (
                                                                    (( 
                                                                    b.off ) -
                                                                    ( a.bol
                                                                    )) )) in
                                                                  if 
                                                                   ((
                                                                    (x.start).line
                                                                    ) <> (
                                                                    (x.stop).line
                                                                    )) then
                                                                   (
                                                                   (sprintf
                                                                    "%s (end at line %d, character %d)"
                                                                    res (
                                                                    (x.stop).line
                                                                    ) (
                                                                    (( 
                                                                    b.off ) -
                                                                    ( b.bol
                                                                    )) ))
                                                                   )
                                                                  else res


let print = fun out -> fun x -> (pp_print_string out ( (to_string x) ))


let check =
 fun x ->
  fun msg ->
   if (( (( (start_line x) ) > ( (stop_line x) )) ) || (
        (( (( (start_bol x) ) > ( (stop_bol x) )) ) || (
          (( (( (start_off x) ) > ( (stop_off x) )) ) || (
            (( (( (start_line x) ) < 0) ) || (
              (( (( (stop_line x) ) < 0) ) || (
                (( (( (start_bol x) ) < 0) ) || (
                  (( (( (stop_bol x) ) < 0) ) || (
                    (( (( (start_off x) ) < 0) ) || ( (( (stop_off x) ) < 0)
                      )) )) )) )) )) )) )) ))
   then
    begin
    (
    (eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x)
    );
    (false)
   end else (true)

exception Exc_located of t * exn

let _ = (ErrorHandler.register
                                                                (
                                                                fun ppf ->
                                                                 function
                                                                 | Exc_located
                                                                    (loc, exn) ->
                                                                    (fprintf
                                                                    ppf
                                                                    "%a:@\n%a"
                                                                    print loc
                                                                    ErrorHandler.print
                                                                    exn)
                                                                 | exn ->
                                                                    (raise
                                                                    exn) ))


let name = (ref "_loc")

let raise =
                          fun loc ->
                           fun exc ->
                            (match exc with
                             | Exc_located (_, _) -> (raise exc)
                             | _ -> (raise ( (Exc_located (loc, exc)) )))
