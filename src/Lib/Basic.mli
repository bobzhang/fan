

(**
   {!Fan_basic}   module contains utility functions to manipulate Camlp4Ast
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.
 *)
  
(* exception Unhandled of Ast.ctyp ; *)
(* exception Finished of Ast.expr ; *)


val unit_literal: Ast.expr ;

(**
   {[
   x ~off:20 3
   string = "u3"
   ]}
 *)  
val x : ?off:int -> int -> string;

    
(**
   {[
   xid ~off:20 3 |> opr#ident fmt;
   u3
   ]}
 *)
val xid : ?off:int -> int -> Ast.ident;


(** mainly generate universal quantifiers
    @see 'allxid' for the same reason
 *)    
val allx : ?off:int -> int -> string;

val allxid : ?off:int -> int -> Ast.ident;

val check_valid : string -> unit;

(** revised printer for current synatx  *)    
(* module RPrinters :  sig *)
(*   class printer : *)
(*       ?curry_constr:bool -> ?comments:bool ->   unit -> *)
(*             object ('a) *)
(*               inherit Camlp4.Printers.OCaml.Make(Syntax).printer *)
(*             end *)
(*   val with_outfile : *)
(*       string option -> (Format.formatter -> 'a -> unit) -> 'a -> unit *)
(*   val print : *)
(*       string option -> *)
(*         (printer -> Format.formatter -> 'a -> unit) -> 'a -> unit           *)
(* end *)

(** revised printer for current synatx  *)    
(* module OPrinters : sig *)
(*   class printer : *)
(*       ?curry_constr:bool -> *)
(*         ?comments:bool -> *)
(*           unit -> *)
(*             object ('a) *)
(*               inherit Camlp4.Printers.OCaml.Make(Syntax).printer    *)
(*             end *)
(*   val with_outfile : *)
(*       string option -> (Format.formatter -> 'a -> unit) -> 'a -> unit *)
(*   val print : *)
(*       string option -> *)
(*         (printer -> Format.formatter -> 'a -> unit) -> 'a -> unit *)
(* end *)
    
(**
   Usage:
   {[
   string_of_formatter_fn opr#expr  <:expr< let a = 3 in a >> ;
   "let a = 3 in a"
   ]}
*)  
(* val opr : RPrinters.printer *)
(* val opo : OPrinters.printer *)
(* val p_expr : Ast.expr -> unit *)
(* val p_patt : Ast.patt -> unit *)
(* val p_str_item : Ast.str_item -> unit *)
(* val p_ident : Ast.ident -> unit *)
(* val p_ctyp : Ast.ctyp -> unit *)


(** When the input is string we can pinponit the content of the string,
   which means we can provide clearer error message
 *)
val error_report : (FanLoc.t * string) -> unit;

(** a robust wrapper over camlp4 parser  mainly give a
    good error message since when input is {b string},
    we could give  better error report message *)    
(* val parse_string_of_entry :  ?_loc:FanLoc.t -> *)
(*   'a Gram.Entry.t -> string -> 'a; *)

(** wrap the exception [Loc.Ex_located]
 *)      
(* val wrap_stream_parser :  ?_loc:Loc.t ->  (Loc.t -> 'a -> 'b) -> 'a -> 'b *)

    
(* (\**  read a file as a channel. @warning Syntax related *\)     *)
(* val parse_include_file : 'a Gram.Entry.t -> string -> 'a *)


(* (\** parse Ast.module_expr to get an identifier *)
(*   A.Make(S) ==> get an identifier <:ident< A.Make(S) >>  *)
(*   @Warning Syntax related  *\) *)
(* val parse_module_type : string -> Ast.ident *)


(* (\** *)
(*    guess use [Syntax.str_items] or [Syntax.sig_items] *\)     *)

(* val parse_include_file_smart : *)
(*   string -> *)
(*   [> `Sig of Syntax.Ast.sig_item *)
(*    | `Str of Syntax.Ast.str_item ] *)
    
(* module Make : *)
(*   functor (Gram : Camlp4.Sig.Grammar.Static with *)
(*            module Loc = Loc and module Token = Token ) -> *)
(*              Fan_sig.Grammar with *)
(*                type 'a t = 'a Gram.Entry.t *)
(*                and type loc = Gram.Loc.t *)
                     
(* module Fan_camlp4syntax : Fan_sig.Grammar with *)
(*                type 'a t = 'a Gram.Entry.t *)
(*                and type loc = Gram.Loc.t *)
                     
(* val anti_str_item: *)
(*   Syntax.Ast.str_item Gram.Entry.t *)
(* val anti_expr: Syntax.Ast.expr Gram.Entry.t *)

(* val handle_antiquot_in_string:string -> *)
(*   term:(string -> 'a) -> *)
(*   parse:('b -> string -> 'c) -> loc:'b -> decorate:(string -> 'c -> 'a) -> 'a *)

(* val token_of_string:string -> *)
(*   (Token.t * Loc.t) Stream.t *)

(* val tokens_of_string:string -> unit *)


    
    
