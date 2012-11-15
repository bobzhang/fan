let initial_spec_list =
        Arg.align
        [
         ("-loc", Arg.Set_string FanLoc.name,
          "<name>   Name of the location variable (default: " ^ !FanLoc.name ^ ").");
         ("-no_quot", Arg.Clear FanConfig.quotations,
          " Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
         ("-parsing-strict",Arg.Set FanConfig.strict_parsing, " strict parsing");
         ("-parsing-strict-warning",Arg.Set FanConfig.strict_parsing_warning," strict parsing warning");
         (* FIXME the command line parsing sucks, it can not handle prefix problem*)
         ("-ignore", Arg.String ignore, " ignore the next argument");

         ("-tuple", Arg.Tuple [(Arg.Set_string FanLoc.name);(Arg.Set_string FanLoc.name)],"tuple");
         (* Feed -tuple a b *)
         ("-options", Arg.Symbol (["a";"b";"c"], (fun x -> prerr_endline x)), "options");
         (* Feed -options a *)
         ("--", Arg.Rest (fun s -> prerr_endline (Format.sprintf "processing file %s" s)),
          " Files")];

let usage_message = "Usage: fan <options> <files>\nOptions are:"; 
 
Arg.parse(* _argv Sys.argv *)
    initial_spec_list
    print_endline
    "Usage: fan <options> <files>\nOptions are:";

(* print_endline  (Arg.usage_string initial_spec_list usage_message ); *)
