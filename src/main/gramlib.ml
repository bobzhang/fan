
(* [eoi_entry] could be improved   *)
let eoi_entry entry =
  let g = Gramf.gram_of_entry entry in
  let entry_eoi = (Gramf.mk_dynamic g (Gramf.name entry ^ "_eoi")) in
  begin
    %extend{ entry_eoi: [  entry as x; EOI %{x} ] } ;
    entry_eoi
  end



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramlib.cmo" *)
(* end: *)
