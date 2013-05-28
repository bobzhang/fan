
(** domain is the namespace all begins with capital letters *)
type domains = [ `Absolute of string list | `Sub of string list]
type name = domains * string


(* FIXME how to put it in ocamldoc?
   The generic quotation type . To see how fields are used here is an example:
    "{:q_name@q_loc|q_contents|}"
    The last one, q_shift is equal to the length of "{:q_name@q_loc|"*)      
type quotation ={
    q_name : name;
    q_loc : string;
    q_shift : int;
    q_contents : string
  }

(**
  For some tokens the data constructor holds two representations with the
  evaluated one and the source one. For example
  the INT data constructor holds an integer and a string, this string can
  contains more information that's needed for a good pretty-printing
  ("42", "4_2", "0000042", "0b0101010"...).

  [KEYWORD s] is the keyword [s].
  [LidENT s] is the ident [s] starting with a lowercase letter.
  [UidENT s] is the ident [s] starting with an uppercase letter.
  [INT i s]
   (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
   the integer constant [i] whose string source is [s].
  [FLOAT f s] is the float constant [f] whose string source is [s].
  [STRING s s'] is the string constant [s] whose string source is [s'].
  [CHAR c s] is the character constant [c] whose string source is [s].
  [QUOTATION q] is a quotation [q], see {!AstQuotation.t} for more information.
  [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
  [EOI] is the end of input.

  Warning: the second string associated with the constructor [STRING] is
  the string found in the source without any interpretation. In particular,
  the backslashes are not interpreted. For example, if the input is ["\n"]
  the string is *not* a string with one element containing the character
  "return", but a string of two elements: the backslash and the character
  ["n"]. To interpret a string use the first string of the [STRING]
  constructor (or if you need to compute it use the module
  {!TokenEval}. Same thing for the constructor [CHAR].

 *)
type t =
  [  `KEYWORD of string
  | `SYMBOL of string
  | `Lid of string
  | `Uid of string
  | `ESCAPED_IDENT of string (* (+)*)
  | `INT of (int * string )
  | `INT32 of (int32 * string )
  | `INT64 of (int64 * string )
  | `NATIVEINT of (nativeint * string )
  | `Flo of (float * string )
  | `CHAR of (char * string )
  | `STR of (string * string )
  | `LABEL of string
  | `OPTLABEL of string
        (* . *)


  | `QUOTATION of quotation
  | `Ant of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int *  string option )
  | `EOI]
      
