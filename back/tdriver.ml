


open Camlp4
module MyPreCast = MakePreCast.Make (Struct.Loc)  (Lexer.Make)
module MyRegister = MakeRegister.Make (MyPreCast.FilterSyntax)    
let () =
  let module M = MakeCamlp4Bin.Camlp4Bin (Struct.Loc) (MyPreCast) (MyRegister) in
  ()
