


let module P =
  Camlp4.MakePreCast.Make Camlp4.Struct.Loc Camlp4.Struct.Lexer.Make in
let module M =
  MakeCamlp4Bin.Camlp4Bin Camlp4.Struct.Loc P P.Register in
();
      

