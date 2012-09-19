module Id =
 struct
  let name = "Camlp4QuotationExpander"

  let version = Sys.ocaml_version

 end

module Make =
       functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
        struct
         module M = ((QuotationCommon.Make)(Syntax))(Syntax.AntiquotSyntax)

         include M

        end
