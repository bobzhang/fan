open LibUtil;

open FSig;
open Format;
open Lib;
module Ast = Camlp4Ast;
  
(** A Hook To Ast Filters *)
let keep = ref false;
type plugin = {
    plugin_transform:(module_types -> Ast.str_item);
    plugin_activate: mutable bool;
  };
type plugin_name = string ;
let filters : Hashtbl.t plugin_name plugin = Hashtbl.create 30;
let show_code =  ref false;
let register  (name,filter) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else begin
   (* eprintf "%s filter registered@." name ; *)
   Hashtbl.add filters name {plugin_transform=filter; plugin_activate=false} ;
  end;

let show_modules () =
  begin
    Hashtbl.iter
      (fun key _  ->
        Format.printf  "%s@ " key ) filters;
    print_newline()
  end;    
  
(*
  Get all definitions from mli file
  Entrance: sig_item
 *)


let plugin_add plugin =
  try
    let v = Hashtbl.find filters plugin in begin 
    v.plugin_activate <- true
  end 
  with
    [Not_found -> begin
      show_modules ();
      failwithf "plugins %s not found " plugin ;
    end];
let plugin_remove plugin =
  try
    let v = Hashtbl.find filters plugin in
    v.plugin_activate <- false
  with
    [Not_found -> begin 
      show_modules ();
      eprintf "plugin %s not found, removing operation ignored" plugin;
      end ];
  



(* Filter type definitions from mli file
   for simplicity, we only care about `toplevel type definitions'
   nested type definitions in module are not considered.
   {:sig_item| type $x |}
 *)  
let filter_type_defs ?qualified () = object (* (self:'self_type) *)
  inherit Ast.map as super;
  val mutable type_defs = let _loc = FanLoc.ghost in {:str_item||} ;
  method! sig_item = with "sig_item" fun
    [
     ( {| val $_ : $_ |} | {| include $_ |} | {| external $_ : $_ = $_ |}
     | {|exception $_ |}  | {| class $_ |}  | {| class type $_ |}
     | {| # $_ |}  | {| module $_:$_ |}    | {| module type $_ = $_ |}
     | {| module rec $_ |}  | {| open $_ |} ) -> {| |} (* For sig_item, keep does not make sense. *)
     | {@_| type $((Ast.TyDcl (_loc,name,vars, ctyp, constraints) as x)) |} -> begin
             let x = 
               match (Ctyp.qualified_app_list ctyp, qualified)with
              [(Some ({:ident|$i.$_ |},ls),Some q) when
                (Ident.eq i q && Ctyp.eq_list ls vars )->
                   (* type u 'a = Loc.u 'a *)       
                  Ast.TyDcl _loc name vars {:ctyp||} constraints
               |(_,_) -> super#ctyp x ] in 
             let y = {:str_item| type $x  |} in
             let () =  type_defs <- {:str_item| $type_defs ; $y |} in      
             {| type $x |}  
     end
     | {| type $ty |} -> (* TyAnd case *) begin
         let x = super#ctyp ty in
         let () = type_defs <- {:str_item| $type_defs ; $({:str_item|type $x |}) |} in
         {|type $x |} 
         end
     | x -> super#sig_item x];
  method! ident = fun
    [ {:ident| $x.$y |} as i ->
      match qualified with
      [Some q when Ident.eq q  x -> super#ident y
      |_ -> super#ident i]
    | i -> super#ident i];
  method! ctyp = fun
    [ {:ctyp| $_ == $ctyp |} ->
      super#ctyp ctyp
    | ty -> super#ctyp ty];
  method get_type_defs = type_defs;
end;



(*
  Entrance is  [module_expr]
  Choose [module_expr] as an entrace point to make the traversal
  more modular
 *)  
let traversal ()  = object (self:'self_type)
  inherit Ast.map as super;
  val module_types_stack : Stack.t module_types = Stack.create ();
  method get_cur_module_types : module_types =
    Stack.top module_types_stack;
  method update_cur_module_types f =
    Stack.(push (f (pop module_types_stack)) module_types_stack);
  method in_module =  Stack.push [] module_types_stack ;
  method out_module = Stack.pop module_types_stack |> ignore;
  val mutable cur_and_types : and_types= [];
  val mutable and_group = false;
  method in_and_types = begin and_group <- true; cur_and_types <- [] end;
  method out_and_types = begin and_group <- false; cur_and_types <- [] end;
  method is_in_and_types = and_group;
  method get_cur_and_types = cur_and_types;
  method update_cur_and_types f = 
    cur_and_types <-  f cur_and_types;
  (* entrance *)  
  method! module_expr = with "str_item" fun
    [ {:module_expr| struct $u end |}  -> 
      let () = self#in_module in 
      let res = self#str_item u in 
      let module_types = List.rev (self#get_cur_module_types) in
      let result =
        Hashtbl.fold
          (fun _ v acc
            -> if v.plugin_activate then
              {|$acc; $(v.plugin_transform module_types) |}
            else  acc) filters
          (if !keep then res else {| |} ) in 
      (* let items = <<
       *   .$res$.;
       *   .$gen (List.rev (self#get_cur_module_types))$. >> ; *)
      let () = self#out_module in 
      {:module_expr| struct $result end |}  

    | x -> super#module_expr x ];

  method! str_item  = with "str_item" fun
    [ {| type $_ and $_ |} as x -> begin
      self#in_and_types;
      let _ = super#str_item x ;
      self#update_cur_module_types (
        fun lst -> [Mutual (List.rev self#get_cur_and_types) :: lst] );
      self#out_and_types;
      (if !keep then x else {| |} )
    end
    | {| type $((Ast.TyDcl (_, name, _, _, _) as t)) |} as x -> begin
        self#update_cur_module_types (fun lst -> [Single (name,t) :: lst]);
       (* if keep.val then x else {| |} *)
       x (* always keep *)
    end
    | ( {| let $_ |}  | {| module type $_ = $_ |}  | {| include $_ |}
    | {| external $_ : $_ = $_ |} | {| $exp:_ |}   | {| exception $_ |} 
    | {| # $_ $_ |}  as x)  ->  x (* always keep *)
    |  x ->  super#str_item x  ];
  method! ctyp = fun
    [ Ast.TyDcl (_, name, _, _, _) as t -> begin
      if self#is_in_and_types then
        self#update_cur_and_types (fun lst -> [ (name,t) :: lst] )
      else ();
      t
    end
    | t -> super#ctyp t ];
end;



#default_quotation "expr"  ;;
#lang_at "patt" "module_expr";;

let g = Gram.create_gram ();

{:extend.create|(g:Gram.t)
  fan_quot fan_quots
|};

with "expr"
    {:extend|Gram
      fan_quot:
      [ "<+" ; `STR(_,plugin) -> begin plugin_add plugin; {| |} end
      | "<++"; L1 [`STR(_,x) -> x] SEP ","{plugins} ->
          begin List.iter plugin_add plugins; {| |}  end
      | "clear" -> begin Hashtbl.iter (fun _  v -> v.plugin_activate <- false) filters; {| |} end
      | "<--"; L1 [`STR(_,x) -> x] SEP ","{plugins} -> begin List.iter plugin_remove plugins ; {| |} end
      | "keep" ; "on" -> begin keep := true; {| |} end
      | "keep" ; "off" -> begin keep := false; {| |} end
      | "show_code"; "on" -> begin show_code := true; {| |} end
      | "show_code"; "off" -> begin show_code := false; {| |} end]
      fan_quots:
      [L0[fan_quot{x};";" -> x]{xs} -> {| begin $list:xs end|}]
|};  

begin 
  PreCast.Syntax.Options.add ("-keep", (FanArg.Set keep), "Keep the included type definitions") ;
  PreCast.Syntax.Options.add ("-loaded-plugins", (FanArg.Unit show_modules), "Show plugins");
end;
