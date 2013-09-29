# 15 "parsing/lexer.mll"
 
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
;;

exception Error of error * Location.t;;

(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

(* let store_string_char c = *)
(*   if !string_index >= String.length (!string_buff) then begin *)
(*     let new_buff = String.create (String.length (!string_buff) * 2) in *)
(*       String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff)); *)
(*       string_buff := new_buff *)
(*   end; *)
(*   String.unsafe_set (!string_buff) (!string_index) c; *)
(*   incr string_index *)

(* let store_lexeme lexbuf = *)
(*   let s = Lexing.lexeme lexbuf in *)
(*   for i = 0 to String.length s - 1 do *)
(*     store_string_char s.[i]; *)
(*   done *)

(* let get_stored_string () = *)
(*   let s = String.sub (!string_buff) 0 (!string_index) in *)
(*   string_buff := initial_string_buffer; *)
(*   s *)

(* (\* To store the position of the beginning of a string and comment *\) *)
(* let string_start_loc = ref Location.none;; *)
(* let comment_start_loc = ref [];; *)
(* let in_comment () = !comment_start_loc <> [];; *)
(* let is_in_string = ref false *)
(* let in_string () = !is_in_string *)
(* let print_warnings = ref true *)

(* To translate escape sequences *)

(* let char_for_backslash = function *)
(*   | 'n' -> '\010' *)
(*   | 'r' -> '\013' *)
(*   | 'b' -> '\008' *)
(*   | 't' -> '\009' *)
(*   | c   -> c *)

