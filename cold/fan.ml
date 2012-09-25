let _ = let module P = (MakePreCast.Make)(FanLexer.Make) in
        let module M = (MakeCamlp4Bin.Camlp4Bin)(P) in ()
