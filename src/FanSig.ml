



      
(** A type for stream filters. *)
type  stream_filter 'a 'loc =
     Stream.t ('a *  'loc)-> Stream.t ('a*  'loc);




(** The generic quotation type.
    To see how fields are used here is an example:
    <:q_name@q_loc<q_contents>>
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
type quotation ={
    q_name : string;
    q_loc : string;
    q_shift : int;
    q_contents : string
  };


      
(* (\** This signature describes tokens for the OCaml and the Revised *)
(*     syntax lexing rules. For some tokens the data constructor holds two *)
(*     representations with the evaluated one and the source one. For example *)
(*     the INT data constructor holds an integer and a string, this string can *)
(*     contains more information that's needed for a good pretty-printing *)
(*     ("42", "4_2", "0000042", "0b0101010"...). *)

(*     The meaning of the tokens are: *)
(*     -      [KEYWORD s] is the keyword [s]. *)
(*     -      [LIDENT s] is the ident [s] starting with a lowercase letter. *)
(*     -      [UIDENT s] is the ident [s] starting with an uppercase letter. *)
(*     -      [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s]) *)
(*     the integer constant [i] whose string source is [s]. *)
(*     -      [FLOAT f s] is the float constant [f] whose string source is [s]. *)
(*     -      [STRING s s'] is the string constant [s] whose string source is [s']. *)
(*     -      [CHAR c s] is the character constant [c] whose string source is [s]. *)
(*     -      [QUOTATION q] is a quotation [q], see {!Quotation.t} for more information. *)
(*     -      [ANTIQUOT n s] is an antiquotation [n] holding the string [s]. *)
(*     -      [EOI] is the end of input. *)

(*     Warning: the second string associated with the constructor [STRING] is *)
(*     the string found in the source without any interpretation. In particular, *)
(*     the backslashes are not interpreted. For example, if the input is ["\n"] *)
(*     the string is *not* a string with one element containing the character *)
(*     "return", but a string of two elements: the backslash and the character *)
(*     ["n"]. To interpret a string use the first string of the [STRING] *)
(*     constructor (or if you need to compute it use the module *)
(*     {!Camlp4.Struct.Token.Eval}. Same thing for the constructor [CHAR]. *\) *)
type token  =
  [ = `KEYWORD of string
  | `SYMBOL of string
  | `LIDENT of string
  | `UIDENT of string
  | `ESCAPED_IDENT of string
  | `INT of (int * string )
  | `INT32 of (int32 * string )
  | `INT64 of (int64 * string )
  | `NATIVEINT of (nativeint * string )
  | `FLOAT of (float * string )
  | `CHAR of (char * string )
  | `STRING of (string * string )
  | `LABEL of string
  | `OPTLABEL of string
  | `QUOTATION of quotation
  | `ANTIQUOT of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int * option string )
  | `EOI];

 (** The type for this filter chain.
     A basic implementation just store the [is_keyword] function given
     by [mk] and use it in the [filter] function. *)
type token_filter = stream_filter token FanLoc.t;

type filter ={
    is_kwd : string -> bool;
    filter : mutable token_filter };


module type SEntry = sig
  (** The abstract type of grammar entries. The type parameter is the type
      of the semantic actions that are associated with this entry. *)
  type t 'a;
  type token_stream;   (* := *)
  type internal_entry; (* := *)
  type extend_statment; (* := *)
  type delete_statment; (* := *)              
  type action; (* := *)
  type symbol; (* := *)
  type  fold 'a 'b 'c; (* := *)
  type  foldsep 'a 'b 'c; (* := *)
  type token;       (* = *)
  val mk : string -> t 'a;
 (** Make a new entry from a name and an hand made token parser. *)
  val of_parser : string -> (token_stream -> 'a) ->  t 'a;
 (** Clear the entry and setup this parser instead. *)
  val setup_parser :  t 'a -> (token_stream -> 'a) -> unit;
  val name : t 'a -> string;
  (** Print the given entry into the given formatter. *)
  val print : Format.formatter ->  t 'a -> unit;
  (** Same as {!print} but show the left-factorization. *)
  val dump : Format.formatter ->  t 'a -> unit;
  val obj : t 'a -> internal_entry;
  val clear : t 'a -> unit;
  (** This function is called by the EXTEND ... END syntax. *)
  val extend : t 'a -> extend_statment -> unit;
  (** The delete rule. *)
  val delete_rule : t 'a -> delete_statment -> unit;
  val srules : t 'a ->  list ((list symbol )*  action) -> symbol;
  val sfold0 : ('a -> 'b -> 'b) -> 'b ->  fold _  'a 'b;
  val sfold1 : ('a -> 'b -> 'b) -> 'b ->  fold _  'a 'b;
  val sfold0sep : ('a -> 'b -> 'b) -> 'b -> foldsep _  'a 'b;
 (* let sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)
      (** Lex, filter and parse a stream of character. *)
  val parse : t 'a -> FanLoc.t ->  Stream.t char -> 'a;
      (** Same as {!parse} but from a string. *)
  val parse_string : t 'a -> FanLoc.t -> string -> 'a;
      (** Parse a token stream that is not filtered yet. *)
  val filter_and_parse_tokens :
      t 'a ->  Stream.t (token*  FanLoc.t)  -> 'a;
          (** Parse a token stream that is already filtered. *)
  val parse_origin_tokens : t 'a -> token_stream -> 'a;
  val mk_action: 'a->action;  (*Action.mk*)
  val string_of_token:token -> string;     (* Token.extract_string*)
  val debug_origin_token_stream: t 'a ->   Stream.t token  -> 'a;
  val debug_filtered_token_stream: t 'a ->   Stream.t token  -> 'a;
end;
module type DEntry = sig
  include SEntry;
  type gram ;
  (** Make a new entry from the given name. *)
  val mk : gram -> string -> t 'a;
  (** Make a new entry from a name and an hand made token parser. *)
  val of_parser : gram -> string -> (token_stream -> 'a) -> t 'a;
end;



type assoc =
    [= `NA|`RA|`LA];
type position =
    [= `First | `Last | `Before of string | `After of string | `Level of string];
module Grammar = struct
    (** Internal signature for sematantic actions of grammars,
      not for the casual user. These functions are unsafe. *)
    module type Action = sig
        type t;
        val mk : 'a -> t;
        val get : t -> 'a;
        val getf : t -> 'a -> 'b;
        val getf2 : t -> 'a -> 'b -> 'c;
      end;
    module type Structure =  sig
        module Action : Action;
        type gram =
          { gfilter : filter;
            gkeywords :  Hashtbl.t string  (ref int);
            glexer : FanLoc.t ->  Stream.t char ->
              Stream.t (token * FanLoc.t);
            warning_verbose :  ref bool;
            error_verbose :  ref bool
          };
        type internal_entry;
        type tree;
        type token_pattern = ((token -> bool) * string);
        type token_info;
        type token_stream =  Stream.t (token * token_info);
        val token_location : token_info -> FanLoc.t;
        type symbol =
          [= `Smeta of (string *  list symbol * Action.t)
          | `Snterm of internal_entry
          | `Snterml of (internal_entry * string)
          | `Slist0 of symbol
          | `Slist0sep of (symbol * symbol)
          | `Slist1 of symbol
          | `Slist1sep of (symbol * symbol)
          | `Sopt of symbol
          | `Stry of symbol
          | `Sself
          | `Snext
          | `Stoken of token_pattern
          | `Skeyword of string
          | `Stree of tree];
        type production_rule = ((list symbol ) * Action.t);
        type single_extend_statment =
          ((option string ) * (option assoc ) * ( list production_rule));
        type extend_statment =
          (( option position) * ( list single_extend_statment));
        type delete_statment =  list symbol;
        type fold 'a 'b 'c=
          internal_entry ->
             list symbol -> (Stream.t 'a -> 'b) -> Stream.t 'a -> 'c;
        type foldsep 'a 'b 'c=
          internal_entry ->
             list symbol ->
              (Stream.t 'a -> 'b) ->
                (Stream.t 'a -> unit) -> Stream.t 'a -> 'c;
      end;
    module type Dynamic =
      sig
        include Structure;
        val mk : unit -> gram;
        include DEntry  with
        type token_stream:=token_stream
        and  type internal_entry := internal_entry
        and type gram := gram
        and type action := Action.t
        and type extend_statment := extend_statment
        and type delete_statment := delete_statment
        and type symbol := symbol
        and type fold 'a 'b 'c := fold 'a 'b 'c 
        and type foldsep 'a 'b 'c := foldsep 'a 'b 'c 
        and type token = token;

        val get_filter : gram -> token;
        val lex : gram -> FanLoc.t ->
               Stream.t char ->  Stream.t (token * FanLoc.t);
        val lex_string : gram -> FanLoc.t ->
          string -> Stream.t (token * FanLoc.t);
        val filter :   gram -> Stream.t (token * FanLoc.t) -> token_stream;
          
      end;
    module type Static =
      sig
        include Structure;
        val trace_parser :  ref bool;
        val gram : gram;
        include SEntry with
        type token_stream := token_stream
        and type internal_entry := internal_entry
        and type action := Action.t
        and type extend_statment := extend_statment
        and type delete_statment := delete_statment
        and type symbol := symbol
        and type fold 'a 'b 'c := fold 'a 'b 'c 
        and type foldsep 'a 'b 'c := foldsep 'a 'b 'c 
        and type token =token;
        val get_filter : unit -> filter;
        val lex :
          FanLoc.t ->  Stream.t char -> Stream.t (token * FanLoc.t);
        val lex_string :
          FanLoc.t -> string -> Stream.t (token * FanLoc.t);
        val filter :
          Stream.t (token * FanLoc.t)  -> token_stream;
      end;
  end;


    
