



let module P =
  Camlp4.MakePreCast.Make  FanLexer.Make in 
let module M =MakeCamlp4Bin.Camlp4Bin  P in
();

