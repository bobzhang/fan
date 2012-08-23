module Id = struct
  value name = "Camlp4.MakePreCast";
  value version = Sys.ocaml_version;
end ;

module type S = sig
  type camlp4_token = Sig.camlp4_token ==
  [ KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of Sig.quotation
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

(* module Id         : Sig.Id; *)
module Loc        : Sig.Loc;
module Ast        : Sig.Camlp4Ast with module Loc = Loc;
module Token      : Sig.Token
                      with module Loc = Loc
                       and type t = camlp4_token;
module Lexer      : Sig.Lexer
                      with module Loc = Loc
                       and module Token = Token;
module Gram       : Sig.Grammar.Static
                      with module Loc = Loc
                       and module Token = Token;
module Quotation  : Sig.Quotation with module Ast = Sig.Camlp4AstToAst Ast;
module DynLoader  : Sig.DynLoader;
module AstFilters : Sig.AstFilters with module Ast = Ast;
module Syntax     : Sig.Camlp4Syntax
                      with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;

module Printers : sig
  module OCaml         : (Sig.Printer Ast).S;
  module OCamlr        : (Sig.Printer Ast).S;
  module DumpOCamlAst  : (Sig.Printer Ast).S;
  module DumpCamlp4Ast : (Sig.Printer Ast).S;
  module Null          : (Sig.Printer Ast).S;
end;

module MakeGram (Lexer : Sig.Lexer with module Loc = Loc)
  : Sig.Grammar.Static with module Loc = Loc and module Token = Lexer.Token;

module MakeSyntax (U : sig end) : Sig.Syntax;

module FilterSyntax: Sig.FilterSyntax
                      with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;


end ;
module Make (Loc: Sig.Loc)
    (Lexer:
     functor (Token:Sig.Camlp4Token) ->
       Sig.Lexer with module Loc = Token.Loc and module Token = Token
    ) : S with module Loc = Loc  = struct
  (* open Struct; *)
  type camlp4_token = Sig.camlp4_token ==
    [ KEYWORD       of string
    | SYMBOL        of string
    | LIDENT        of string
    | UIDENT        of string
    | ESCAPED_IDENT of string
    | INT           of int and string
    | INT32         of int32 and string
    | INT64         of int64 and string
    | NATIVEINT     of nativeint and string
    | FLOAT         of float and string
    | CHAR          of char and string
    | STRING        of string and string
    | LABEL         of string
    | OPTLABEL      of string
    | QUOTATION     of Sig.quotation
    | ANTIQUOT      of string and string
    | COMMENT       of string
    | BLANKS        of string
    | NEWLINE
    | LINE_DIRECTIVE of int and option string
    | EOI ];

  module Loc = Loc;      
  module Ast = Struct.Camlp4Ast.Make Loc;
  module Token = Struct.Token.Make Loc;
  module Lexer = Lexer Token;
  module Gram = Struct.Grammar.Static.Make Lexer;
  module DynLoader = Struct.DynLoader;
  module Quotation = Struct.Quotation.Make Ast;
  module MakeSyntax (U : sig end) = OCamlInitSyntax.Make Ast Gram Quotation;
  module Syntax = MakeSyntax (struct end);
  module AstFilters = Struct.AstFilters.Make Ast;
  module MakeGram = Struct.Grammar.Static.Make;
  module Printers = struct
    module OCaml = Printers.OCaml.Make Syntax;
    module OCamlr = Printers.OCamlr.Make Syntax;
    (* module OCamlrr = Printers.OCamlrr.Make Syntax; *)
    module DumpOCamlAst = Printers.DumpOCamlAst.Make Syntax;
    module DumpCamlp4Ast = Printers.DumpCamlp4Ast.Make Syntax;
    module Null = Printers.Null.Make Syntax;
  end;
  module FilterSyntax = struct
    include Syntax;
    module AstFilters = AstFilters;
  end ;
  (* module Register = Register.Make FilterSyntax; *)
end; 
