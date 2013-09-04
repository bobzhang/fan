open LibUtil
type tag_info = { id : string; start : bool; action : int; }
type regexp =
  | Empty
  | Chars of int * bool
  | Action of int
  | Tag of tag_info
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp
module Id : sig type t = Automata_def.ident val compare : t -> t -> int end    
val encode_regexp : SSet.t -> int -> LexSyntax.regular_expression -> regexp
val find_chars : LexSyntax.regular_expression -> SSet.t
val find_optional : LexSyntax.regular_expression -> SSet.t
val find_double : LexSyntax.regular_expression -> SSet.t * SSet.t
val remove_nested_as :
  LexSyntax.regular_expression -> LexSyntax.regular_expression

val chars : Fcset.t list ref
val chars_count : int ref


(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3
   Extension to tagged automata.
   Confer
   Ville Larikari
   ``NFAs with Tagged Transitions, their Conversion to Deterministic
   Automata and Application to Regular Expressions''.
   Symposium on String Processing and Information Retrieval (SPIRE 2000),
   http://kouli.iki.fi/~vlaurika/spire2000-tnfa.ps
   (See also)
   http://kouli.iki.fi/~vlaurika/regex-submatch.ps.gz
 *)

type t_transition =
  | OnChars of int
  | ToAction of int

