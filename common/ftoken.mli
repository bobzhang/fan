
(** domain is the namespace all begins with capital letters *)
type domains = [ `Absolute of string list | `Sub of string list]
type name = domains * string



(* FIXME how to put it in ocamldoc?
   The generic quotation type . To see how fields are used here is an example:
    "{:q_name@q_loc|q_contents|}"
    The last one, q_shift is equal to the length of "{:q_name@q_loc|"

   (name,loc,shift,contents)
 *)
type quot = {
    name:name;
    loc:string option;
    shift:int;
    content:string;
  }
type quotation = [ `Quot of quot ]      

(** (name,contents) *)
type dir_quotation = [`DirQuotation of (int * string * string) ] 
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
   (resp. [INT32 i s], [INT64 i s] and [Nativeint i s])
   the integer constant [i] whose string source is [s].
  [FLOAT f s] is the float constant [f] whose string source is [s].
  [STRING s s'] is the string constant [s] whose string source is [s'].
  [CHAR c s] is the character constant [c] whose string source is [s].
  [Quot q] is a quotation [q], see {!AstQuotation.t} for more information.
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

   [Eident] is the identifier which function the same as Lid but does not expect it to
   be lifted to keyworkds
   example:
   {[
   let f = (=)
   ]}
   the first =  is a keyword, while the latter is not

   [Sym] works specially in grammars [infixop*] and [prefixop]. Some of them will be lifted to
   keyword. for example [/] is not a keyword.
   
 *)
type space_token =
   [ `Comment        of string
   | `Blank         of string
   | `NEWLINE
   | `LINE_DIRECTIVE of (int * string option) ]
      
type t =
  [ `KEYWORD        of string
  | `Sym            of string
  | `Lid            of string
  | `Uid            of string
  | `Eident         of string (* (+)*)
  | `Int            of string 
  | `Int32          of string 
  | `Int64          of string 
  | `Nativeint      of string 
  | `Flo            of string 
  | `Chr            of string 
  | `Str            of string 
  | `Label          of string
  | `Optlabel       of string
  | space_token
   (* . *)
  | quotation
  | dir_quotation
  | `Ant            of (string * string )        
  | `EOI]
      
type stream = (t * FLoc.t) XStream.t 

type 'a token  = [> t] as 'a
      
type 'a estream  = ('a token * FLoc.t) XStream.t
      
type 'a parse = stream -> 'a

type filter = stream -> stream

val token_to_string : t  -> string

val to_string : [> t]  -> string

val print : Format.formatter -> [> t ] -> unit


(**  {[x=STRING -> extract_string x  ]} *)  
val extract_string : [> t ] -> string



val string_of_name : name -> string

val empty_name : name

val name_of_string : string -> name

