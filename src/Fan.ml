



let module P =
  MakePreCast.Make (struct end) in 
let module M =MakeBin.Camlp4Bin  P in
();



