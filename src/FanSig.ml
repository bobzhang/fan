(** Signature for errors modules, an Error modules can be registred with
    the {!ErrorHandler.Register} functor in order to be well printed. *)
module type Error = sig
  type t
  exception E of t
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end


(** A signature for locations. *)
module type Loc =
  sig
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
    type t
    
    (** Return a start location for the given file name.
      This location starts at the begining of the file. *)
    val mk : string -> t
      
    (** The [ghost] location can be used when no location
      information is available. *)
    val ghost : t
      
    (** {6 Conversion functions} *)
    (** Return a location where both positions are set the given position. *)
    val of_lexing_position : Lexing.position -> t
      
    (** Return an OCaml location. *)
    val to_ocaml_location : t -> Location.t
      
    (** Return a location from an OCaml location. *)
    val of_ocaml_location : Location.t -> t
      
    (** Return a location from ocamllex buffer. *)
    val of_lexbuf : Lexing.lexbuf -> t
      
    (** Return a location from [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
    val of_tuple : (string * int * int * int * int * int * int * bool) -> t
      
    (** Return [(file_name, start_line, start_bol, start_off,
      stop_line,  stop_bol,  stop_off, ghost)]. *)
    val to_tuple : t -> (string * int * int * int * int * int * int * bool)
      
    (** [merge loc1 loc2] Return a location that starts at [loc1] and end at
      [loc2]. *)
    val merge : t -> t -> t
      
    (** The stop pos becomes equal to the start pos. *)
    val join : t -> t
      
    (** [move selector n loc]
      Return the location where positions are moved.
      Affected positions are chosen with [selector].
      Returned positions have their character offset plus [n]. *)
    val move : [ | `start | `stop | `both ] -> int -> t -> t
      
    (** [shift n loc] Return the location where the new start position is the old
      stop position, and where the new stop position character offset is the
      old one plus [n]. *)
    val shift : int -> t -> t
      
    (** [move_line n loc] Return the location with the old line count plus [n].
      The "begin of line" of both positions become the current offset. *)
    val move_line : int -> t -> t
      
    (** {6 Accessors} *)
    (** Return the file name *)
    val file_name : t -> string
      
    (** Return the line number of the begining of this location. *)
    val start_line : t -> int
      
    (** Return the line number of the ending of this location. *)
    val stop_line : t -> int
      
    (** Returns the number of characters from the begining of the stream
      to the begining of the line of location's begining. *)
    val start_bol : t -> int
      
    (** Returns the number of characters from the begining of the stream
      to the begining of the line of location's ending. *)
    val stop_bol : t -> int
      
    (** Returns the number of characters from the begining of the stream
      of the begining of this location. *)
    val start_off : t -> int
      
    (** Return the number of characters from the begining of the stream
      of the ending of this location. *)
    val stop_off : t -> int
      
    (** Return the start position as a Lexing.position. *)
    val start_pos : t -> Lexing.position
      
    (** Return the stop position as a Lexing.position. *)
    val stop_pos : t -> Lexing.position
      
    (** Generally, return true if this location does not come
      from an input stream. *)
    val is_ghost : t -> bool
      
    (** Return the associated ghost location. *)
    val ghostify : t -> t
      
    (** Return the location with the give file name *)
    val set_file_name : string -> t -> t
      
    (** [strictly_before loc1 loc2] True if the stop position of [loc1] is
      strictly_before the start position of [loc2]. *)
    val strictly_before : t -> t -> bool
      
    (** Return the location with an absolute file name. *)
    val make_absolute : t -> t
      
    (** Print the location into the formatter in a format suitable for error
      reporting. *)
    val print : Format.formatter -> t -> unit
      
    (** Print the location in a short format useful for debugging. *)
    val dump : Format.formatter -> t -> unit
      
    (** Same as {!print} but return a string instead of printting it. *)
    val to_string : t -> string
      
    (** [Exc_located loc e] is an encapsulation of the exception [e] with
      the input location [loc]. To be used in quotation expanders
      and in grammars to specify some input location for an error.
      Do not raise this exception directly: rather use the following
      function [Loc.raise]. *)
    exception Exc_located of t * exn
      
    (** [raise loc e], if [e] is already an [Exc_located] exception,
      re-raise it, else raise the exception [Exc_located loc e]. *)
    val raise : t -> exn -> 'a
      
    (** The name of the location variable used in grammars and in
      the predefined quotations for OCaml syntax trees. Default: [_loc]. *)
    val name : string ref
      
  end
  

      



















