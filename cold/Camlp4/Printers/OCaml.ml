open Format

module Id =
              struct
               let name = "Camlp4.Printers.OCaml"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Sig.Camlp4Syntax) ->
                     (struct
                       let print_implem =
                        fun ?input_file:_ ->
                         fun ?output_file ->
                          fun ast ->
                           let pt = (Syntax.Ast2pt.str_item ast) in
                           (FanUtil.with_open_out_file output_file (
                             fun oc ->
                              let fmt = (Format.formatter_of_out_channel oc) in
                              let () = (Pprintast.print_structure fmt pt) in
                              (pp_print_flush fmt () ) ))

                       let print_interf =
                        fun ?input_file:_ ->
                         fun ?output_file ->
                          fun ast ->
                           let pt = (Syntax.Ast2pt.sig_item ast) in
                           (FanUtil.with_open_out_file output_file (
                             fun oc ->
                              let fmt = (Format.formatter_of_out_channel oc) in
                              let () = (Pprintast.print_signature fmt pt) in
                              (pp_print_flush fmt () ) ))

                      end : Sig.Printer(Syntax.Ast).S)
