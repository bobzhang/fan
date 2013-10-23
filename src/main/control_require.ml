

(**
  extend the control langauge, it's split here,
  because we want to split the dependency on dynlink
  *)

let loaded_modules = ref Setf.String.empty

let add_to_loaded_modules name =
  loaded_modules := Setf.String.add name !loaded_modules;;
  
let add name = 
  if not @@ Setf.String.mem name !loaded_modules  then begin
    add_to_loaded_modules name;
    Dyn_load.load  (name ^ Dyn_load.libext)
  end
;;
  
let () =
  let open Control in
  %unsafe_extend{
  (* (g:Gramf.t) *)
    item:
    [ "require"; Str s %{ add s} ]}


(* local variables: *)
(* compile-command: "cd ../main_annot && pmake control_require.cmo" *)
(* end: *)
