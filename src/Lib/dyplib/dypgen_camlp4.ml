open Parse_tree;
open Format;
open Dyp;
open Dypgen_sig;
open Parse_tree;
open Lexing;
open Argument;
open Camlp4.PreCast;
<:fan< lang "str_item" ; >>;
module Make (S:Params) =struct
  value (
    token_list,
    relation,
    non_terminal_start_list,
    generic_merge,
    cons_list,
    nt_type_list,
    single_nt_list,
    add_cons_list,
    ppi_layout) =
    let s = S.parser_param_info in
    (s.token_list,s.relation, s.start,
     s.generic_merge, s.cons, s.nt_type,
     s.single_nt, s.additional_cons, s.layout);
  value grammar = S.grammar;
  value (topheader_main,topheader_main_pos) = S.optional_mltop;
  value (header_main,header_main_pos) = S.optional_code;
  value (trailer_main,trailer_main_pos) = S.optional_trailer;
  value (topmli_code,topmli_code_pos) = S.optional_mlitop;
  value (midmli_code,midmli_code_pos) = S.optional_mlimid;
  value (mli_code,mli_code_pos) = S.optional_mli ;
  value lexer = S.lexer;

  (** code generator below *)
  value test_version_match = <<
    if $str:dypgen_version$ <> Dyp.version then begin 
      Format.eprintf
        $str:"version mismatch, dypgen version"^dypgen_version
      ^" and dyplib version %s\n"$ Dyp.version;
      exit 2;
    end 
   else ();
   >>;
  value (regexp_decl, aux_lexer, main_lexer) =
    match lexer with [ None -> ([], [], []) | Some a b c -> (a, b, c) ];

  value main_lexer =
    List.fold_left (fun ml (re, c) -> [ (re, "__dypgen_layout", c) :: ml ])
      main_lexer ppi_layout;
end;

















