type compile_info = 
    {
     printer : string option;
     plugins : string list ;
     file : string ;
     include_dirs : string list;
     show_where : bool;
     show_printers : bool;
   }

val compile_info_arg : compile_info Cmdliner.Term.t 
val info : Cmdliner.Term.info
