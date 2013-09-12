
open OUnit

let test_ident_location  _ =
  let () = Parse_fan.fill_parsers () in
  assert_equal 
    (Ast2pt.exp (Fgram.parse_string Fsyntax.exp {:str| X.x|})).pexp_desc

    (Parsetree.Pexp_ident
       {Asttypes.txt = Longident.Ldot (Longident.Lident "X", "x");
        loc =
        {Location.loc_start =
         {Lexing.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = 1};
         loc_end =
         {Lexing.pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = 4};
         loc_ghost = false}});;  
