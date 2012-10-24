let _=
  let module P = (MakePreCast.Make) (struct  end) in
    let module M = (MakeCamlp4Bin.Camlp4Bin) (P) in ()
