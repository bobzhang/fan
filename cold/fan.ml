let _ = let module P = ((Camlp4.MakePreCast.Make)(FanLoc))(FanLexer.Make) in
        let module M = ((MakeCamlp4Bin.Camlp4Bin)(FanLoc))(P) in ()
