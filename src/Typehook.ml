open LibUtil;
open Ast;
open FSig;
open Format;
open Lib;

  
(** A Hook To Ast Filters *)
let keep = ref false;
type plugin = {
    transform:(module_types -> str_item);
    activate: mutable bool;
    position: option string;
    filter: option (string->bool);
  };

let apply_filter f (m:module_types) : module_types = begin 
  (* eprintf "applying filter@."; *)
  let f  = (fun
    [ (`Single (s,_) as x) ->
      if f s then Some  x else None
    | `Mutual ls ->
       let x = List.filter_map (fun ((s,_) as x) -> if f s then Some x  else None) ls in
       match x with
       [ [] -> None
       | [x] -> Some (`Single  x)
       |  y -> Some (`Mutual y)]]) in
  List.filter_map  f m ;
end;
  
type plugin_name = string ;
  
let filters : Hashtbl.t plugin_name plugin = Hashtbl.create 30;
  
let show_code =  ref false;
let print_collect_module_types = ref false;
  
let register  ?filter ?position (name,f) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else begin
   (* eprintf "%s filter registered@." name ; *)
   Hashtbl.add filters name {transform=f; activate=false;position;filter} ;
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
  let try v = Hashtbl.find filters plugin in
  v.activate <- true
  with
  [Not_found -> begin
    show_modules ();
    failwithf "plugins %s not found " plugin ;
  end];
let plugin_remove plugin =
  let try v = Hashtbl.find filters plugin in
  v.activate <- false
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
  inherit FanAst.map as super;
  val mutable type_defs = let _loc = FanLoc.ghost in {:str_item||} ;
  method! sig_item = with sig_item fun
    [
     ( {| val $_ : $_ |} | {| include $_ |} | {| external $_ : $_ = $_ |}
     | {|exception $_ |}  | {| class $_ |}  | {| class type $_ |}
     | {| # $_ |}  | {| module $_:$_ |}    | {| module type $_ = $_ |}
     | {| module rec $_ |}  | {| open $_ |} ) -> {| |} (* For sig_item, keep does not make sense. *)
     | {@_| type $((`TyDcl (_loc,name,vars, ctyp, constraints) as x)) |} -> begin
             let x = 
               match (Ctyp.qualified_app_list ctyp, qualified)with
              [(Some ({:ident|$i.$_ |},ls),Some q) when
                (Ident.eq i q && Ctyp.eq_list ls vars )->
                   (* type u 'a = Loc.u 'a *)       
                  `TyDcl _loc name vars {:ctyp||} constraints
               |(_,_) -> super#ctyp x ] in 
             let y = {:str_item| type $x  |} in
             let () =  type_defs <- {:str_item| $type_defs ; $y |} in      
             {| type $x |}  
     end
     | {| type $ty |} -> (* `And case *) begin
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

class type traversal = object
  inherit FanAst.map;
  method get_cur_module_types: FSig.module_types;
  method get_cur_and_types: FSig.and_types;
  (* method in_and_types: *)
  method update_cur_and_types:
      (FSig.and_types -> FSig.and_types) -> unit;
  method update_cur_module_types:
      (FSig.module_types -> FSig.module_types) -> unit;

end;

(*
  Entrance is  [module_expr]
  Choose [module_expr] as an entrace point to make the traversal
  more modular.
  Usage
  {[
  let v =  {:module_expr| struct $s end |} in
  let module_expr =
  (Typehook.traversal ())#module_expr v 
  ]}
 *)  
let traversal () : traversal  = object (self:'self_type)
  inherit FanAst.map as super;
  val module_types_stack : Stack.t module_types = Stack.create ();
  val mutable cur_and_types : and_types= [];
  val mutable and_group = false;
  method get_cur_module_types : module_types =
    Stack.top module_types_stack;
  method update_cur_module_types f =
    Stack.(push (f (pop module_types_stack)) module_types_stack);
  method private in_module =  Stack.push [] module_types_stack ;
  method private out_module = Stack.pop module_types_stack |> ignore;
    
  method private in_and_types = begin and_group <- true; cur_and_types <- [] end;
  method private out_and_types = begin and_group <- false; cur_and_types <- [] end;
  method private is_in_and_types = and_group;
  method get_cur_and_types = cur_and_types;
  method update_cur_and_types f = 
    cur_and_types <-  f cur_and_types;
  (* entrance *)  
  method! module_expr = with str_item fun
    [ {:module_expr| struct $u end |}  ->  begin 
      self#in_module ;
      let res = self#str_item u ;
      let module_types = List.rev (self#get_cur_module_types) ;
      if !print_collect_module_types then
        eprintf "@[%a@]@." FSig.pp_print_module_types module_types ;
      let result =
        Hashtbl.fold
          (fun _ {activate;position;transform;filter} acc
            ->
              let module_types =
                match filter with
                [Some x -> apply_filter x module_types
                |None -> module_types] in
              if activate then
                let code = transform module_types in 
                match position with
                [Some x ->
                  let (name,f) = Filters.make_filter (x,code) in begin 
                    AstFilters.register_str_item_filter (name,f);
                    AstFilters.use_implem_filter name ;
                    acc
                  end
                |None -> 
                  {| $acc; $code |} ]
            else  acc) filters
          (if !keep then res else {| |} );
      self#out_module ;
      {:module_expr| struct $result end |}  
    end
    | x -> super#module_expr x ];

  method! str_item  = with str_item fun
    [ {| type $_ and $_ |} as x -> begin
      self#in_and_types;
      let _ = super#str_item x ;
      self#update_cur_module_types
          (fun lst -> [`Mutual (List.rev self#get_cur_and_types) :: lst] );
      self#out_and_types;
      (if !keep then x else {| |} )
    end
    | {| type $((`TyDcl (_,`Lid(_, name), _, _, _) as t)) |} as x -> begin
        let item =  `Single (name,t) ;
        if !print_collect_module_types then eprintf "Came across @[%a@]@." FSig.pp_print_types  item ;
        self#update_cur_module_types (fun lst -> [ item :: lst]);
       (* if !keep then x else {| |} *)
       x (* always keep *)
    end
    | ( {| let $_ |}  | {| module type $_ = $_ |}  | {| include $_ |}
    | {| external $_ : $_ = $_ |} | {| $exp:_ |}   | `Exception (_loc,_)
(* {| exception $_ |} *) 
    | {| # $_ $_ |}  as x)  ->  x (* always keep *)
    |  x ->  super#str_item x  ];
  method! ctyp = fun
    [ `TyDcl (_, `Lid(_,name), _, _, _) as t -> begin
      if self#is_in_and_types then
        self#update_cur_and_types (fun lst -> [ (name,t) :: lst] )
      else ();
      t
    end
    | t -> super#ctyp t ];
end;



#default_quotation "expr"  ;;
(* #lang_at "patt" "module_expr";; *)

let g = Gram.create_gram ();

{:create|(g:Gram.t)
  fan_quot fan_quots
|};

with expr
    {:extend|Gram
      fan_quot:
      ["derive";"("; L1 [`Lid x -> x | `Uid x  -> x]{plugins}; ")" ->
          begin List.iter plugin_add plugins; {| |}  end
      | "unload"; L1 [`Lid x  -> x | `Uid x -> x ] SEP ","{plugins} ->
          begin List.iter plugin_remove plugins ; {| |} end
      | "clear" ->
          begin Hashtbl.iter (fun _  v -> v.activate <- false) filters; {| |} end
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
