[@@@ocaml.warning "-44"]
open Cmdliner

let plugins  : string list Term.t = 
  Arg.(value & opt_all  string [] & info ["plugin"] ~docv:"PLUGIN" ~doc:{|Dynamic loading $(docv)
Load plugin cma or cmxs files
For native code, it's plugins like `cmxs'
For byte code', it's plugins like `cma'
|})


(* a  bug in cmdliner, when no doc for printer, help==plain will be weird *)
let printer : string option Term.t = 
  Arg.(value & opt (some string) None & info ["printer","p"]  ~docv:"PRINTER" 
         ~doc:"Set printer of Fan")

let version="0.8.0.0" 

let info = Term.info "fan" ~version

let file : string Term.t 
    = Arg.(value & (pos 0 ( non_dir_file) "" ) & info ~docv:"FILE" [])

let show_where : bool Term.t 
    = Arg.(value & flag & info ["w";"where"] ~doc:"Print location of fan's standard library and exit")

let show_printers : bool Term.t 
    = Arg.(value & flag & info ["list-printers"]
             ~doc:" List the backends available, and exit")
let include_dirs : string list Term.t
    = Arg.(value & opt_all dir [] & info ["I"]
             ~docv:"DIR"
             ~doc:" Add $(docv) in search patch for object files.")
(* let show_parsers : bool Term.t *)
(*        = Arg.(value & flag & info ["list-printers"] *)
(*              ~doc:" List the backends available, and exit") *)


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
     file : string;
     include_dirs : string list;
     show_where : bool;
     show_printers : bool;
   }

(* let compile_command  (info : compile_info) =  *)
let compile_info_arg : compile_info Term.t = 
  let compile_info printer plugins 
      file
      include_dirs
      show_where
      show_printers 
      = { printer; plugins; file; show_where; show_printers; include_dirs} in
  Term.(pure compile_info $ printer $ plugins $ file $ include_dirs $ show_where $ show_printers)

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
