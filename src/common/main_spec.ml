[@@@ocaml.warning "-44"]
open Cmdliner

let plugins  : string list Term.t = 
  Arg.(value & opt_all  string [] & info ["plugin"] ~docv:"PLUGIN" ~doc:{|Dynamic loading $(docv)
For native code, it's plugins like `cmxs'
For byte code', it's plugins like `cma'
|})


let printer : string option Term.t = 
  Arg.(value & opt (some string) None & info ["printer"]  ~docv:"PRINTER")

let version="0.8.0.0" 

let info = Term.info "fan" ~version

let file : string Term.t 
    = Arg.(required & (pos 0 (some non_dir_file) None ) & info ~doc:"FILE" [])

let (++) ta tb = 
  Term.(pure (fun _ _ -> ()) $ ta $ tb)

(* let driver (plugins : string list) (printer : string option)  : unit =  *)
(*   let f = Format.std_formatter in  *)
(*   snd (Arg.list Arg.string) f plugins ;  *)
(*   Format.fprintf f "\n"; *)
(*   match printer with  *)
(*   | None -> Format.fprintf f "None" *)
(*   | Some x -> Format.pp_print_string f x  *)
type compile_info = 
    {
     printer : string option;
     plugins : string list ;
     file : string
   }

(* let compile_command  (info : compile_info) =  *)
let compile_info_arg : compile_info Term.t = 
  let compile_info printer plugins file = { printer; plugins; file} in
  Term.(pure compile_info $ printer $ plugins $ file)

let compile_command : compile_info Term.t * Term.info= (compile_info_arg, info)

let default_command : 'a Term.t * Term.info = 
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ (pure ()))), 
  Term.info "fan" ~version


(* let _ =  *)
(*   Term.eval  *)
(*     (( plugins ++ printer ++ file), info) *)
(*     ~argv:[| "fan"; "--help=plain"|] *)
(*
let () = 

  match Term.eval_choice default_command [compile_command] with 
  | `Error _ -> exit 1 
  |  _ -> exit 0
*)
    (* ~argv:[|"fan" ; "--plugin" ; "x.cma"; "--plugin"; "u.cma"; "--printer"; "o"|] *)
