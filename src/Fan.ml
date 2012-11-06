



let module P =
  PreCast.Make (struct end) in 
let module M =MakeBin.Camlp4Bin  P in
();



