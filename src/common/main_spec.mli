type compile_info = 
    {
     printer : string option;
     plugins : string list ;
     file : string
   }

val compile_info_arg : compile_info Cmdliner.Term.t 
val info : Cmdliner.Term.info
