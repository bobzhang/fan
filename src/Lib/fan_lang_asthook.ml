(* <:fan< include_ml "open_template.ml" ; >> ; *)
<:include_ml< "open_template.ml"; >> ;
(** A Hook To Ast Filters *)
value keep = ref False;
type plugin = {
    plugin_transform:(module_types -> Ast.str_item);
    plugin_activate: mutable bool;
  };
type plugin_name = string ;
value filters : Hashtbl.t plugin_name plugin = Hashtbl.create 30;
value show_code =  ref False;
value register  (name,filter) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else do {
   eprintf "%s filter registered@." name ;
   Hashtbl.add filters name {plugin_transform=filter; plugin_activate=False} ;
  };

value show_modules () = do{ 
  Hashtbl.iter
    (fun key _  ->
      Format.printf  "%s@ " key )
    filters;
  print_newline()
};    
  
(*
  Get all definitions from mli file
  Entrance: sig_item
 *)


value plugin_add plugin =
  try
    let v = Hashtbl.find filters plugin in begin 
    v.plugin_activate := True
  end 
  with
    [Not_found -> begin
      show_modules ();
      failwithf "plugins %s not found " plugin ;
    end];
value plugin_remove plugin =
  try
    let v = Hashtbl.find filters plugin in
    v.plugin_activate := False
  with
    [Not_found -> begin 
      show_modules ();
      eprintf "plugin %s not found, removing operation ignored" plugin;
      end ];
  
<:fan< lang "sig_item"; >> ;


(* Filter type definitions from mli file
   for simplicity, we only care about `toplevel type definitions'
   nested type definitions in module are not considered.
 *)  
value filter_type_defs ?qualified () = object (self:'self_type)
  inherit Ast.map as super;
  value mutable type_defs = <:str_item< >> ;
  method! sig_item = fun
    [
     ( << value .$_$. : .$_$. >> | << include .$_$. >>
     | << external .$_$. : .$_$. = .$_$. >>
     | <<exception .$_$. >>
     | << class .$_$. >>
     | << class type .$_$. >>
     | << # .$_$. >>
     | << module .$_$.:.$_$. >>
     | << module type .$_$. = .$_$. >>
     | << module rec .$_$. >> 
     | << open .$_$. >> ) -> << >> (* For sig_item, keep does not make sense. *)
     | << type .$
           (Ast.TyDcl _loc name vars ctyp constraints as x)$. >>   -> begin
             let x = 
               match (Fan_ctyp.qualified_app_list ctyp,
                      qualified)with
              [(Some (<:ident< .$i$. . .$_$. >>,
                        ls),Some q) when
                          (Fan_ident.eq i q && Fan_ctyp.eq_list ls vars )->
                   (* type u 'a = Loc.u 'a *)       
                   Ast.TyDcl _loc name vars <:ctyp< >> constraints
               |(_,_) -> super#ctyp x ] in 
             let y = <:str_item< type .$ x $. >> in
             let () =  type_defs :=
               <:str_item< .$type_defs$. ; .$y$. >> in      
             << type .$x$. >>  
     end
     | << type .$ty$. >> -> (* TyAnd case *) begin
         let x = super#ctyp ty in
         let () = type_defs := <:str_item< .$type_defs$. ;
           .$ <:str_item< type .$x$. >> $. >> in
         << type .$x$. >> 
         end
     | x -> super#sig_item x];
  method! ident = fun
    [ <:ident< .$x$. . .$y$. >> as i ->
      match qualified with
      [Some q when Fan_ident.eq q  x -> super#ident y
      |_ -> super#ident i]
    | i -> super#ident i];
  method! ctyp = fun
    [ <:ctyp< .$_$. == .$ctyp$. >> ->
      super#ctyp ctyp
    | ty -> super#ctyp ty];
  method get_type_defs = type_defs;
end;


<:fan< lang "str_item"; >> ;


(*
  Entrance is  [module_expr]
  Choose [module_expr] as an entrace point to make the traversal
  more modular
 *)  
value traversal ()  = object (self:'self_type)
  inherit Ast.map as super;
  value module_types_stack : Stack.t module_types = Stack.create ();
  method get_cur_module_types : module_types =
    Stack.top module_types_stack;
  method update_cur_module_types f =
    Stack.(push (f (pop module_types_stack)) module_types_stack);
  method in_module =  Stack.push [] module_types_stack ;
  method out_module = Stack.pop module_types_stack |> ignore;
  value mutable cur_and_types : and_types= [];
  value mutable and_group = False;
  method in_and_types = do {and_group := True; cur_and_types := []};
  method out_and_types = do {and_group := False; cur_and_types := []};
  method is_in_and_types = and_group;
  method get_cur_and_types = cur_and_types;
  method update_cur_and_types f = 
    cur_and_types :=  f cur_and_types;
  (* entrance *)  
  method! module_expr = fun
    [ <:module_expr< struct .$u$. end >>  -> 
      let () = self#in_module in 
      let res = self#str_item u in 
      let module_types = List.rev (self#get_cur_module_types) in
      let result = Hashtbl.fold (fun _ v acc
        -> if v.plugin_activate then
             << .$acc$. ;
                .$v.plugin_transform module_types$. >>
         else  acc) filters
          (if keep.val then res else << >> ) in 
      (* let items = <<
       *   .$res$.;
       *   .$gen (List.rev (self#get_cur_module_types))$. >> ; *)
      let () = self#out_module in 
      <:module_expr< struct .$result$. end >>  

    | x -> super#module_expr x ];

  method! str_item  = fun
    [ << type .$_$. and .$_$. >> as x -> do{
      self#in_and_types;
      let _ = super#str_item x ;
      self#update_cur_module_types (
        fun lst -> [Mutual (List.rev self#get_cur_and_types) :: lst] );
      self#out_and_types;
      (if keep.val then x else << >> )
    }
    | << type .$(Ast.TyDcl _ name _ _ _ as t) $. >> as x -> do{
        self#update_cur_module_types (fun lst -> [Single (name,t) :: lst]);
       (* if keep.val then x else << >> *)
       x (* always keep *)
     }          
    | ( << value .$_$. >>  | << module type .$_$. = .$_$. >>  | << include .$_$. >>
    | << external .$_$. : .$_$. = .$_$. >>
    | << .$exp:_$. >>   | << exception .$_$. >> 
    | << # .$_$. .$_$. >>  as x)  ->  x (* always keep *)
    |  x -> do {     super#str_item x ; } ];
  method! ctyp = fun
    [ Ast.TyDcl _ name _ _ _ as t -> do{
      if self#is_in_and_types then
        self#update_cur_and_types (fun lst -> [ (name,t) :: lst] )
      else ();
      t
     }
    | t -> super#ctyp t ];
end;


<:fan<
lang "expr";
lang_at "patt" "module_expr";
>>;

  
let open Fan_lang_meta in   
EXTEND MGram fan_quot: LEVEL "top" 
  [ [  "plugin_add" ; plugin = STRING -> begin
         plugin_add plugin; << >> 
     end 
     | "plugins_add"; plugins = LIST1 [x = STRING -> x] SEP "," -> begin
         List.iter plugin_add plugins; << >>
     end 
     | "plugins_clear" -> begin 
         Hashtbl.iter (fun _  v -> v.plugin_activate := False) filters;
         << >>   
     end 
     | "plugin_remove"; plugin = STRING -> begin 
         plugin_remove plugin;
         << >>
     end 
     | "plugins_remove"; plugins =LIST1 [x=STRING -> x] SEP "," -> begin
         List.iter plugin_remove plugins ; << >> 
     end 

     | "keep" ; "on" -> begin
         keep.val := True; << >> 
     end
     | "keep" ; "off" -> begin
         keep.val := False; << >>
     end
     | "show_code"; "on" -> begin
         show_code.val := True; << >> 
     end
     | "show_code"; "off" -> begin
         show_code.val := False; << >> 
     end 
    ] ];
END;

let open Fan_lang_include in EXTEND MGram fan_include_ml: LEVEL "top"
  [ [ "mli"; file=STRING;
          include_mod = OPT [ x= STRING -> x ] -> 
          let sig_item = (Gram.parse_file_with ~rule:Syntax.sig_items file) in
          let qualified = match include_mod with
            [Some v -> Some (parse_module_type v)
            |None -> None] in 
          let obj = (filter_type_defs ?qualified ()) in 
          let _ = obj#sig_item sig_item in
          obj#get_type_defs
            
    ] ];
END;


(* AstFilters.register_str_item_filter  filter ; *)
Fan_camlp4syntax.add_quotation_of_str_item_with_filter
  ~name:"ocaml" ~entry:Syntax.str_items ~filter:(fun s ->
    let v =  <:module_expr< struct .$s$. end >> in 
    let module_expr = 
      (traversal ())#module_expr v in
    let code = match module_expr with
    [ <:module_expr< struct .$item$. end >>  -> item 
    | _ -> failwith "can not find items back " ]  in
    begin
      if show_code.val then
        try
          p_str_item code;
        with
          [e -> begin
            prerr_endline &
            "There is a printer bug\
             Our code generator may still work when \
             Printer is broken\
             Plz send bug report to " ^ bug_main_address;
          end]
      else ();
      code 
    end);

begin
  prerr_endline "fan_asthook linking!"; (* it should appear only once *)
end ;

Fan_camlp4syntax.add_quotation_of_match_case
      ~name:"pattern" ~entry:Syntax.match_case;
    
begin
  (* Camlp4.Options.add "-trash" (Arg.Set_string trash)
   *   "Trash  for your defined type and will be removed default: Camlp4Types"; *)
  Camlp4.Options.add "-debug" (Arg.Set debug)
    "Turn on debug option";
  Camlp4.Options.add "-keep" (Arg.Set keep)
    "Keep the included type definitions" ;
  Camlp4.Options.add "-loaded-plugins"
    (Arg.Unit show_modules) "Show plugins";
end
;
