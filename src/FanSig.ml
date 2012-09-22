
(** An Error modules can be registred with
    the {!ErrorHandler.Register}
    functor in order to be well printed. *)
module type Error = sig
  type t
  exception E of t
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end



(** A signature for locations. *)
module type Loc =  sig

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


(** The generic quotation type.
    To see how fields are used here is an example:
    <:q_name@q_loc<q_contents>>
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
type quotation ={
    q_name : string;
    q_loc : string;
    q_shift : int;
    q_contents : string
  }

      
(** A type for stream filters. *)
type ('a, 'loc) stream_filter = ('a * 'loc) Stream.t -> ('a * 'loc) Stream.t


(** A signature for tokens. *)
module type Token = sig
  module Loc : Loc
  type t
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
  val match_keyword : string -> t -> bool
  val extract_string : t -> string
  module Filter : sig
    type token_filter = (t, Loc.t) stream_filter
    (** The type for this filter chain.
        A basic implementation just store the [is_keyword] function given
        by [mk] and use it in the [filter] function. *)
  type t
  (** The given predicate function returns true if the given string
      is a keyword. This function can be used in filters to translate
      identifier tokens to keyword tokens. *)
  val mk : (string -> bool) -> t
  (** This function allows to register a new filter to the token filter chain.
     You can choose to not support these and raise an exception. *)
  val define_filter : t -> (token_filter -> token_filter) -> unit
              
  (** This function filter the given stream and return a filtered stream.
      A basic implementation just match identifiers against the [is_keyword]
      function to produce token keywords instead. *)
  val filter : t -> token_filter
              
  (** Called by the grammar system when a keyword is used.
      The boolean argument is True when it's the first time that keyword
      is used. If you do not care about this information just return [()]. *)
  val keyword_added : t -> string -> bool -> unit
              
  (** Called by the grammar system when a keyword is no longer used.
      If you do not care about this information just return [()]. *)
  val keyword_removed : t -> string -> unit
  end
  module Error : Error
end
      
(** This signature describes tokens for the OCaml and the Revised
    syntax lexing rules. For some tokens the data constructor holds two
    representations with the evaluated one and the source one. For example
    the INT data constructor holds an integer and a string, this string can
    contains more information that's needed for a good pretty-printing
    ("42", "4_2", "0000042", "0b0101010"...).

    The meaning of the tokens are:
    -      [KEYWORD s] is the keyword [s].
    -      [LIDENT s] is the ident [s] starting with a lowercase letter.
    -      [UIDENT s] is the ident [s] starting with an uppercase letter.
    -      [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
    the integer constant [i] whose string source is [s].
    -      [FLOAT f s] is the float constant [f] whose string source is [s].
    -      [STRING s s'] is the string constant [s] whose string source is [s'].
    -      [CHAR c s] is the character constant [c] whose string source is [s].
    -      [QUOTATION q] is a quotation [q], see {!Quotation.t} for more information.
    -      [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
    -      [EOI] is the end of input.

    Warning: the second string associated with the constructor [STRING] is
    the string found in the source without any interpretation. In particular,
    the backslashes are not interpreted. For example, if the input is ["\n"]
    the string is *not* a string with one element containing the character
    "return", but a string of two elements: the backslash and the character
    ["n"]. To interpret a string use the first string of the [STRING]
    constructor (or if you need to compute it use the module
    {!Camlp4.Struct.Token.Eval}. Same thing for the constructor [CHAR]. *)

type camlp4_token =
  | KEYWORD of string
  | SYMBOL of string
  | LIDENT of string
  | UIDENT of string
  | ESCAPED_IDENT of string
  | INT of int * string
  | INT32 of int32 * string
  | INT64 of int64 * string
  | NATIVEINT of nativeint * string
  | FLOAT of float * string
  | CHAR of char * string
  | STRING of string * string
  | LABEL of string
  | OPTLABEL of string
  | QUOTATION of quotation
  | ANTIQUOT of string * string
  | COMMENT of string
  | BLANKS of string
  | NEWLINE
  | LINE_DIRECTIVE of int * string option
  | EOI

(** A signature for specialized tokens. *)
module type Camlp4Token = Token with type t = camlp4_token
      

      



(** A signature for dynamic loaders. *)
module type DynLoader =
  sig
    type t
    
    exception Error of string * string
      
    (** [mk ?ocaml_stdlib ?camlp4_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:False] *)
    val mk : ?ocaml_stdlib: bool -> ?camlp4_stdlib: bool -> unit -> t
      
    (** Fold over the current load path list. *)
    val fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a
      
    (** [load f] Load the file [f]. If [f] is not an absolute path name,
      the load path list used to find the directory of [f]. *)
    val load : t -> string -> unit
      
    (** [include_dir d] Add the directory [d] in the current load path
      list (like the common -I option). *)
    val include_dir : t -> string -> unit
      
    (** [find_in_path f] Returns the full path of the file [f] if
      [f] is in the current load path, raises [Not_found] otherwise. *)
    val find_in_path : t -> string -> string
      
    (** [is_native] [True] if we are in native code, [False] for bytecode. *)
    val is_native : bool
      
  end
  




(** A signature for grammars. *)
module Grammar =
  struct
    (** Internal signature for sematantic actions of grammars,
      not for the casual user. These functions are unsafe. *)
    module type Action =
      sig
        type t
        
        val mk : 'a -> t
          
        val get : t -> 'a
          
        val getf : t -> 'a -> 'b
          
        val getf2 : t -> 'a -> 'b -> 'c
          
      end
      
    type assoc = | NA | RA | LA
    
    type position =
      | First | Last | Before of string | After of string | Level of string
    
    (** Common signature for {!Sig.Grammar.Static} and {!Sig.Grammar.Dynamic}. *)
    module type Structure =
      sig
        module Loc : Loc
          
        module Action : Action
          
        module Token : Token with module Loc = Loc
          
        type gram =
          { gfilter : Token.Filter.t;
            gkeywords : (string, int ref) Hashtbl.t;
            glexer : Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t;
            warning_verbose : bool ref; error_verbose : bool ref
          }
        
        type internal_entry
        
        type tree
        
        type token_pattern = ((Token.t -> bool) * string)
        
        type token_info
        
        type token_stream = (Token.t * token_info) Stream.t
        
        val token_location : token_info -> Loc.t
          
        type symbol =
          | Smeta of string * symbol list * Action.t
          | Snterm of internal_entry
          | Snterml of internal_entry * string
          | Slist0 of symbol
          | Slist0sep of symbol * symbol
          | Slist1 of symbol
          | Slist1sep of symbol * symbol
          | Sopt of symbol
          | Stry of symbol
          | Sself
          | Snext
          | Stoken of token_pattern
          | Skeyword of string
          | Stree of tree
        
        type production_rule = ((symbol list) * Action.t)
        
        type single_extend_statment =
          ((string option) * (assoc option) * (production_rule list))
        
        type extend_statment =
          ((position option) * (single_extend_statment list))
        
        type delete_statment = symbol list
        
        type ('a, 'b, 'c) fold =
          internal_entry ->
            symbol list -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'c
        
        type ('a, 'b, 'c) foldsep =
          internal_entry ->
            symbol list ->
              ('a Stream.t -> 'b) ->
                ('a Stream.t -> unit) -> 'a Stream.t -> 'c
        
      end
      
    (** Signature for Camlp4 grammars. Here the dynamic means that you can produce as
      many grammar values as needed with a single grammar module.
      If you do not need many grammar values it's preferable to use a static one. *)
    module type Dynamic =
      sig
        include Structure
          
        (** Make a new grammar. *)
        val mk : unit -> gram
          
        module Entry :
          sig
            (** The abstract type of grammar entries. The type parameter is the type
          of the semantic actions that are associated with this entry. *)
            type 'a t
            
            (** Make a new entry from the given name. *)
            val mk : gram -> string -> 'a t
              
            (** Make a new entry from a name and an hand made token parser. *)
            val of_parser : gram -> string -> (token_stream -> 'a) -> 'a t
              
            (** Clear the entry and setup this parser instead. *)
            val setup_parser : 'a t -> (token_stream -> 'a) -> unit
              
            (** Get the entry name. *)
            val name : 'a t -> string
              
            (** Print the given entry into the given formatter. *)
            val print : Format.formatter -> 'a t -> unit
              
            (** Same as {!print} but show the left-factorization. *)
            val dump : Format.formatter -> 'a t -> unit
              
            (**/**)
            val obj : 'a t -> internal_entry
              
            val clear : 'a t -> unit
              
          end
          
        (**/**)
        (** [get_filter g] Get the {!Token.Filter} associated to the [g]. *)
        val get_filter : gram -> Token.Filter.t
          
        type 'a not_filtered
        
        (** This function is called by the EXTEND ... END syntax. *)
        val extend : 'a Entry.t -> extend_statment -> unit
          
        (** The delete rule. *)
        val delete_rule : 'a Entry.t -> delete_statment -> unit
          
        val srules : 'a Entry.t -> ((symbol list) * Action.t) list -> symbol
          
        val sfold0 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold1 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold0sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) foldsep
          
        (* value sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)
        (** Use the lexer to produce a non filtered token stream from a char stream. *)
        val lex :
          gram ->
            Loc.t ->
              char Stream.t -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        (** Token stream from string. *)
        val lex_string :
          gram ->
            Loc.t -> string -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        (** Filter a token stream using the {!Token.Filter} module *)
        val filter :
          gram -> ((Token.t * Loc.t) Stream.t) not_filtered -> token_stream
          
        (** Lex, filter and parse a stream of character. *)
        val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
          
        (** Same as {!parse} but from a string. *)
        val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
          
        (** Parse a token stream that is not filtered yet. *)
        val parse_tokens_before_filter :
          'a Entry.t -> ((Token.t * Loc.t) Stream.t) not_filtered -> 'a
          
        (** Parse a token stream that is already filtered. *)
        val parse_tokens_after_filter : 'a Entry.t -> token_stream -> 'a
          
      end
      
    (** Signature for Camlp4 grammars. Here the static means that there is only
      one grammar value by grammar module. If you do not need to store the grammar
      value it's preferable to use a static one. *)
    module type Static =
      sig
        include Structure
          
        (** Whether trace parser or not *)
        val trace_parser : bool ref
          
        val gram : gram
          
        module Entry :
          sig
            (** The abstract type of grammar entries. The type parameter is the type
          of the semantic actions that are associated with this entry. *)
            type 'a t
            
            (** Make a new entry from the given name. *)
            val mk : string -> 'a t
              
            (** Make a new entry from a name and an hand made token parser. *)
            val of_parser : string -> (token_stream -> 'a) -> 'a t
              
            (** Clear the entry and setup this parser instead. *)
            val setup_parser : 'a t -> (token_stream -> 'a) -> unit
              
            (** Get the entry name. *)
            val name : 'a t -> string
              
            (** Print the given entry into the given formatter. *)
            val print : Format.formatter -> 'a t -> unit
              
            (** Same as {!print} but show the left-factorization. *)
            val dump : Format.formatter -> 'a t -> unit
              
            (**/**)
            val obj : 'a t -> internal_entry
              
            val clear : 'a t -> unit
              
          end
          
        (**/**)
        (** Get the {!Token.Filter} associated to the grammar module. *)
        val get_filter : unit -> Token.Filter.t
          
        type 'a not_filtered
        
        (** This function is called by the EXTEND ... END syntax. *)
        val extend : 'a Entry.t -> extend_statment -> unit
          
        (** The delete rule. *)
        val delete_rule : 'a Entry.t -> delete_statment -> unit
          
        val srules : 'a Entry.t -> ((symbol list) * Action.t) list -> symbol
          
        val sfold0 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold1 : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) fold
          
        val sfold0sep : ('a -> 'b -> 'b) -> 'b -> (_, 'a, 'b) foldsep
          
        (* value sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)
        (** Use the lexer to produce a non filtered token stream from a char stream. *)
        val lex :
          Loc.t -> char Stream.t -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        (** Token stream from string. *)
        val lex_string :
          Loc.t -> string -> ((Token.t * Loc.t) Stream.t) not_filtered
          
        (** Filter a token stream using the {!Token.Filter} module *)
        val filter :
          ((Token.t * Loc.t) Stream.t) not_filtered -> token_stream
          
        (** Lex, filter and parse a stream of character. *)
        val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
          
        (** Same as {!parse} but from a string. *)
        val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
          
        (** Parse a token stream that is not filtered yet. *)
        val parse_tokens_before_filter :
          'a Entry.t -> ((Token.t * Loc.t) Stream.t) not_filtered -> 'a
          
        (** Parse a token stream that is already filtered. *)
        val parse_tokens_after_filter : 'a Entry.t -> token_stream -> 'a
          
      end
      
  end
  






(** A signature for lexers. *)
module type Lexer =
  sig
    module Loc : Loc
      
    module Token : Token with module Loc = Loc
      
    module Error : Error
      
    (** The constructor for a lexing function. The character stream is the input
      stream to be lexed. The result is a stream of pairs of a token and
      a location.
      The lexer do not use global (mutable) variables: instantiations
      of [Lexer.mk ()] do not perturb each other. *)
    val mk : unit -> Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t
      
  end
  





