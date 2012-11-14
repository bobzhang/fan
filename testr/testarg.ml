let initial_spec_list =
        Arg.align
        [
         ("-unsafe", Arg.Set FanConfig.unsafe,
          " Generate unsafe accesses to array and strings.");
         ("-unsaf", Arg.Set FanConfig.unsafe,
          " Generate unsafe accesses to array and strings.");
         ("-verbose", Arg.Set FanConfig.verbose,
          " More verbose in parsing errors.");
         ("-loc", Arg.Set_string FanLoc.name,
          "<name>   Name of the location variable (default: " ^ !FanLoc.name ^ ").");
         ("-no_quot", Arg.Clear FanConfig.quotations,
          " Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
         ("-parsing-strict",Arg.Set FanConfig.strict_parsing, " strict parsing");
         (* FIXME the command line parsing sucks, it can not handle prefix problem*)
         ("-ignore", Arg.String ignore, " ignore the next argument");
         ("--", Arg.Unit ignore, " Deprecated, does nothing")];

let usage_message = "Usage: fan <options> <files>\nOptions are:"; 
(*  
Arg.parse(* _argv Sys.argv *)
    initial_spec_list
    print_endline
    "Usage: fan <options> <files>\nOptions are:";
*)

print_endline  (Arg.usage_string initial_spec_list usage_message );
