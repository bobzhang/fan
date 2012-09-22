open FanSig;
module Id : Camlp4.Sig.Id = struct
  value name = "Camlp4OCamlParserParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  open Camlp4.Sig;
  include Syntax;

  module M = RStreamParser.Make  Syntax;
  open M;

  Gram.Entry.clear stream_expr;
  Gram.Entry.clear stream_begin;
  Gram.Entry.clear stream_end;
  Gram.Entry.clear stream_quot;
  Gram.Entry.clear parser_case_list;

  EXTEND Gram
    stream_expr:
      [ [ e = expr Level "top" -> e ] ]
    ;
    stream_begin:
      [ [ "[<" -> () ] ]
    ;
    stream_end:
      [ [ ">]" -> () ] ]
    ;
    stream_quot:
      [ [ "'" -> () ] ]
    ;
    parser_case_list:
      [ [ OPT "|"; pcl = LIST1 parser_case SEP "|" -> pcl ] ]
    ;
  END;
end;
