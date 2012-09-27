(*
   Basic module contains utility functions to manipulate Camlp4Ast
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.(Should be FIXED later)
 *)

open LibUtil;
exception Unhandled of Ast.ctyp ;
exception Finished of Ast.expr;
value _loc = FanLoc.ghost;
value unit_literal = <:expr< () >> ;

(* generate name *)  
value x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    String.of_char base ^ string_of_int i;
    
value xid ?(off=0) (i:int) : Ast.ident  =
  <:ident< $(lid:x ~off i) >> ;
  
value allx ?(off=0) i =  "all_" ^x ~off i ;
  
value allxid ?(off=0) i = <:ident< $(lid:allx ~off i) >>;

(* check whether the introduced name is valid or not *)  
value check_valid str =
  let len = String.length str in
  if
    not
      (len > 1 &&
       (not (Char.is_digit str.[1]))
         && (not (String.starts_with str "all_"))) then begin
           eprintf "%s is not a valid name" str;
           eprintf "For valid name its length should be more than 1\n\
             can not be a-[digit], can not start with [all_]";
           exit 2;
         end 
  else ();    
(* module RPrinters = Camlp4.Printers.OCamlr.Make Camlp4.PreCast.Syntax; *)
(* module OPrinters = Camlp4.Printers.OCaml.Make Camlp4.PreCast.Syntax; *)

(* value opr= (new RPrinters.printer ()); *)
(* value opo= (new OPrinters.printer ()); *)

(* FIXME will Ast2pt do the check, and then some partial Ast node will not be able
   to be dumped
 *)  
value p_expr fmt  e =
  eprintf "@[%a@]@." Pprintast.print_expression (Ast2pt.expr e);
(* value p_ident = eprintf "@[%a@]@." opr#ident ;     *)
value p_patt fmt e =
  eprintf "@[%a@]@." Pprintast.print_pattern (Ast2pt.pattern e);
  
value p_str_item fmt e =
  eprintf "@[%a@]@." Pprintast.print_structure (Ast2pt.str_item e);
value p_ident fmt e =
  eprintf "@[%a@]@." Pprintast.fmt_longident (Ast2pt.ident e) ;    
value p_ctyp fmt e =
  eprintf "@[%a@]@." Pprintast.core_type (Ast2pt.ctyp e) ;
  

(* value error_report (loc,s) = begin *)
(*   prerr_endline (Loc.to_string loc); *)
(*   let (start_bol,stop_bol, *)
(*          start_off, stop_off) = *)
(*     Loc.( (start_bol loc, *)
(*              stop_bol loc, *)
(*              start_off loc, *)
(*              stop_off loc) *)
(*            ) in *)
(*   let abs_start_off = start_bol + start_off in *)
(*   let abs_stop_off = stop_bol + stop_off in *)
(*   let err_location = String.sub s abs_start_off *)
(*       (abs_stop_off - abs_start_off + 1) in *)
(*   prerr_endline (sprintf "err: ^%s^" err_location); *)
(* end ; *)


(* value parse_string_of_entry ?(_loc=Loc.mk "<string>") entry  s = *)
(*   try *)
(*     Gram.parse_string entry  _loc s *)
(*   with *)
(*     [Loc.Exc_located(loc, e) -> begin *)
(*       eprintf "%s" (Printexc.to_string e); *)
(*       error_report (loc,s); *)
(*       Loc.raise loc e ; *)
(*     end ]; *)

(* value wrap_stream_parser ?(_loc=Loc.mk "<stream>") p s = *)
(*   try p _loc s *)
(*   with *)
(*     [Loc.Exc_located(loc,e) -> begin *)
(*       eprintf "error: %s" (Loc.to_string loc) ; *)
(*       Loc.raise loc e; *)
(*     end  *)
(*    ]; *)

(* value parse_include_file rule file  = *)
(*   if Sys.file_exists file then *)
(*     let ch = open_in file in *)
(*     let st = Stream.of_channel ch in  *)
(*     Gram.parse rule (Loc.mk file) st *)
(*   else  failwithf "@[file: %s not found@]@." file; *)



(* value parse_module_type str = *)
(*   try *)
(*      match  Gram.parse_string Syntax.module_type _loc str with *)
(*      [ <:module_type< .$id:i$. >>  -> i *)
(*      | _ -> begin *)
(*          eprintf "the module type %s is not a simple module type" str; *)
(*          exit 2; *)
(*      end ] *)
(*   with *)
(*     [ e -> begin *)
(*       eprintf "%s is not a valid module_type" str; *)
(*       exit 2; *)
(*     end]; *)

(* value parse_include_file_smart file = let open Filename in  *)
(*   if check_suffix file ".ml" then  *)
(*       `Str (parse_include_file Syntax.str_items file) *)
(*   else if check_suffix file ".mli" then *)
(*     `Sig (parse_include_file Syntax.sig_items file) *)
(*   else begin  *)
(*     eprintf "file input should ends with either .ml or .mli"; *)
(*     invalid_arg ("parse_include_file_smart: " ^ file ) *)
(*   end  *)
(* ; *)


(* (\** [Fan_quot] is a library which helps to design DSLs   *)
(*     This module provides  utlities to *)
(*     transform parser to quotation expander *\) *)
(* module MetaLocHere = Ast.Meta.MetaLoc; *)
(* module MetaLoc = struct *)
(*   module Ast = Ast; *)
(*   value loc_name = ref None; *)
(*   value meta_loc_expr _loc loc = *)
(*     match loc_name.val with *)
(*     [ None -> <:expr< .$lid:Loc.name.val$. >> *)
(*     | Some "here" -> MetaLocHere.meta_loc_expr _loc loc *)
(*     | Some x -> <:expr< .$lid:x$. >> ]; *)
(*   value meta_loc_patt _loc _ = <:patt< _ >>; *)
(* end; *)
(* module MetaAst = Ast.Meta.Make MetaLoc; *)

(* open Camlp4.PreCast; *)
(* module Make (MGram:Camlp4.Sig.Grammar.Static *)
(*             with module Loc = Loc *)
(*             and  module Token = Token)  : Fan_sig.Grammar with *)
(*                      type t 'a = MGram.Entry.t 'a *)
(*                      and type loc = MGram.Loc.t = struct *)
(*   type t 'a = MGram.Entry.t 'a; *)
(*   type loc =  MGram.Loc.t ; *)
(*   (\* add an end marker for each parser *\) *)
(*   value eoi_entry entry = do{ *)
(*     let entry_eoi = MGram.Entry.(mk (name entry)) ; *)
(*     EXTEND MGram entry_eoi: *)
(*       [ [ x =entry ; `EOI -> x ]]; *)
(*     END; *)
(*     entry_eoi   *)
(*    }; *)


(*   value parse_quot_string_with_filter entry f loc loc_name_opt s  = do{ *)
(*     let q = Camlp4_config.antiquotations.val ; *)
(*     Camlp4_config.antiquotations.val := True; *)
(*     let res = MGram.parse_string entry loc s ;   *)
(*     Camlp4_config.antiquotations.val := q; *)
(*     MetaLoc.loc_name.val := loc_name_opt ; *)
(*     (\* not sure whether this refactoring is correct *\) *)
(*     f res *)
(*   }; *)

(*   (\* given an string input, apply the parser, return the result *\) *)
(*   value parse_quot_string entry  loc  loc_name_opt s = *)
(*     parse_quot_string_with_filter entry (fun x -> x) loc *)
(*       loc_name_opt s ; *)

(*   (\* here entry [does not need] to handle eoi case, *)
(*     we will add eoi automatically. *)
(*     This add quotation utility is for normal *)
(*     It's tailored for ADT DSL paradigm  *\) *)
(*   value add_quotation ?antiquot_expander name   ~entry *)
(*      ~mexpr ~mpatt  = ( *)
(*     let anti_expr = match antiquot_expander with *)
(*       [ None -> fun x -> x | Some obj -> obj#expr] in *)
(*     let anti_patt = match antiquot_expander with *)
(*       [ None -> fun x -> x | Some obj -> obj#patt] in *)
(*     let entry_eoi = eoi_entry entry in  *)
(*     let expand_expr loc loc_name_opt s =  *)
(*       parse_quot_string entry_eoi loc  loc_name_opt s *)
(*       |> mexpr loc *)
(*       |> anti_expr  in *)
(*     let expand_str_item loc loc_name_opt s = *)
(*       let exp_ast = expand_expr loc loc_name_opt s  in  *)
(*       <:str_item@loc< .$exp:exp_ast$. >> in *)
(*     let expand_patt _loc loc_name_opt s = *)
(*       let exp_ast = *)
(*         parse_quot_string entry_eoi _loc  loc_name_opt s *)
(*         |> mpatt _loc *)
(*         |> anti_patt in  *)
(*       match loc_name_opt with *)
(*       [ None -> exp_ast *)
(*       | Some name -> *)
(*           let rec subst_first_loc = *)
(*           fun *)
(*           [ <:patt@_loc< Ast. .$uid:u$. .$_$. >> *)
(*             -> <:patt< Ast. .$uid:u$. .$lid:name$. >> *)
(*           | <:patt@_loc< .$a$. .$b$. >> *)
(*             -> <:patt< .$subst_first_loc a$. .$b$. >> *)
(*           | p -> p ] in *)
(*         subst_first_loc exp_ast ] in *)
(*     let open Quotation in  *)
(*     do { *)
(*       add name DynAst.expr_tag expand_expr; *)
(*       add name DynAst.patt_tag expand_patt; *)
(*       add name DynAst.str_item_tag expand_str_item; *)
(*     } *)
(* ); *)

(*     value add = Quotation.add; *)
(*     value add_quotation_of_str_item ~name ~entry = *)
(*       add name Quotation.DynAst.str_item_tag *)
(*         (parse_quot_string (eoi_entry entry)); *)
(*     value add_quotation_of_str_item_with_filter ~name ~entry ~filter = *)
(*       add name Quotation.DynAst.str_item_tag *)
(*         (parse_quot_string_with_filter (eoi_entry entry) filter); *)
(*     (\* will register str_item as well  *\)  *)
(*     value add_quotation_of_expr ~name ~entry = begin *)
(*       let expand_fun = parse_quot_string & eoi_entry entry in  *)
(*       let mk_fun loc loc_name_opt s = *)
(*         <:str_item< .$exp:expand_fun loc loc_name_opt s$. >> in  *)
(*       let () = add name Quotation.DynAst.expr_tag expand_fun in  *)
(*       let () = add name Quotation.DynAst.str_item_tag mk_fun in *)
(*       () *)
(*     end ; *)
(*     value add_quotation_of_patt ~name ~entry = *)
(*       add name Quotation.DynAst.patt_tag (parse_quot_string (eoi_entry entry)); *)
(*     value add_quotation_of_class_str_item ~name ~entry = *)
(*      add name Quotation.DynAst.class_str_item_tag (parse_quot_string (eoi_entry entry)); *)
(*     value add_quotation_of_match_case ~name ~entry = *)
(*       add name Quotation.DynAst.match_case_tag *)
(*         (parse_quot_string (eoi_entry entry)); *)
(* end; *)

(* (\** Built in MGram  utilities *\) *)
(* module Fan_camlp4syntax = Make(Gram); *)
(* value (anti_str_item, anti_expr) *)
(*     =  Fan_camlp4syntax.( *)
(*   (eoi_entry Syntax.str_item, *)
(*   eoi_entry Syntax.expr) *)
(*  ) *)
(* ; *)


(* value is_antiquot_data_ctor s = String.ends_with s "Ant"; *)
    
(* (\** *)
(*    c means [context] here  *)
(*    {[ *)
(*    mk_anti ~c:"binding" "list" "code" ; *)
   
(*    string = "\\$listbinding:code" *)
(*    ]} *)
(*  *\) *)
(* value mk_anti ?(c = "") n s = "\\$" ^ (n ^ (c ^ (":" ^ s))); *)
                                      
(* (\** \\$expr;:code *\) *)
(* value is_antiquot s = *)
(*   let len = String.length s in (len > 2) *)
(*     && ((s.[0] = '\\') && (s.[1] = '$')); *)
    
(* value handle_antiquot_in_string s ~term ~parse ~loc ~decorate = *)
(*   if is_antiquot s *)
(*   then *)
(*     (let pos = String.index s ':' in *)
(*     let name = String.sub s 2 (pos - 2) *)
(*     and code = String.sub s (pos + 1) (((String.length s) - pos) - 1) *)
(*     in decorate name (parse loc code)) *)
(*   else term s; *)

(* value token_of_string str = *)
(*   let lex = Lexer.mk () (Loc.mk "<string>") in *)
(*   lex (Stream.of_string str); *)

(* (\* debugging purpose *)
(*  *\) *)
(* value tokens_of_string str = begin  *)
(*   let stream = token_of_string str in *)
(*   let tok = ref (fst (Stream.next stream)) in  *)
(*   while tok.val <> EOI do *)
(*     fprintf err_formatter "%a\n" Token.print  tok.val; *)
(*     tok.val := fst (Stream.next stream); *)
(*   done ; *)
(*   fprintf err_formatter "@."; *)
(* end ; *)
(* (\** *)
(*    For functor [Camlp4.Struct.Grammar.Structre.Make] *)
(*    Given [Lexer] module, it will generate more type defintions *)
(*    and utillities *)
(*  *\) *)
(* (\* module ExGram = struct *)
(*  *   include Gram; *)
(*  *   include Camlp4.Struct.Grammar.Structure.Make Camlp4.PreCast.Lexer; *)
(*  * end ; *)
(*  * module MPrint = *)
(*  *   Camlp4.Struct.Grammar.Print.Make ExGram; *\) *)


(* (\** *)
(*    triggered by fan_asthook *)
(*    FIXME be more safe to guarantee its uniqueness. Magic number *)
(*  *\) *)
(* (\* value fan_generate_name = "GENERATE_by_fan"; *\) *)

(* (\* *)
(*   Wait for fix *)
(*   this will mutate the Syntax. Wait to be fixed *)
(* module OSyntax = *)
(*   (Camlp4OCamlParser.Make *)
(*   (Camlp4OCamlRevisedParser.Make *)
(*   (Camlp4.OCamlInitSyntax.Make Ast Gram Quotation))) *)
(* ;     *)
(* *\) *)
