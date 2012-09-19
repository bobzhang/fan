module Id = struct
  value name = "Camlp4QuotationExpander";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Camlp4.Sig.Camlp4Syntax)
= struct
  module M = QuotationCommon.Make Syntax Syntax.AntiquotSyntax;
  include M;
end;
