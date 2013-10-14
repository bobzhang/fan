
open FAst
open Ast_gen
open Automata_def 
open Lexgen

  
(** FIXME *)
let _loc = Locf.mk "x"

let auto_binds =
  [ %bind{
  __ocaml_lex_init_lexbuf lexbuf mem_size =
    let pos = lexbuf.Lexing.lex_curr_pos in
    (lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ;
     lexbuf.Lexing.lex_start_pos <- pos ;
     lexbuf.Lexing.lex_last_pos <- pos ;
     lexbuf.Lexing.lex_last_action <- (-1))
  };
   %bind{
   __ocaml_lex_next_char lexbuf =
    if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then
      begin
        if lexbuf.Lexing.lex_eof_reached then
          256
        else begin
          lexbuf.Lexing.refill_buff lexbuf ;
          __ocaml_lex_next_char lexbuf
        end
      end else begin
        let i = lexbuf.Lexing.lex_curr_pos in
        let c = lexbuf.Lexing.lex_buffer.[i] in
        (lexbuf.Lexing.lex_curr_pos <- i+1 ;
        Char.code c)
      end
 }
 ]
(* let auto_defs = {:stru| *)
(*   let __ocaml_lex_init_lexbuf lexbuf mem_size = *)
(*     let pos = lexbuf.Lexing.lex_curr_pos in *)
(*     (lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ; *)
(*      lexbuf.Lexing.lex_start_pos <- pos ; *)
(*      lexbuf.Lexing.lex_last_pos <- pos ; *)
(*      lexbuf.Lexing.lex_last_action <- (-1)) *)

(*   let rec __ocaml_lex_next_char lexbuf = *)
(*     if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then *)
(*       begin *)
(*         if lexbuf.Lexing.lex_eof_reached then *)
(*           256 *)
(*         else begin *)
(*           lexbuf.Lexing.refill_buff lexbuf ; *)
(*           __ocaml_lex_next_char lexbuf *)
(*         end *)
(*       end else begin *)
(*         let i = lexbuf.Lexing.lex_curr_pos in *)
(*         let c = lexbuf.Lexing.lex_buffer.[i] in *)
(*         lexbuf.Lexing.lex_curr_pos <- i+1 ; *)
(*         Char.code c *)
(*       end *)
(* |} *)

let output_pats (pats:int list) =
  bar_of_list (List.map (fun x -> %pat{$`int:x}) pats)


let output_mem_access (i:int) = %exp{lexbuf.Lexing.lex_mem.($`int:i)}

let (curr_pos,
     last_pos,
     last_action,
     start_pos
    )  =
  ( %exp{lexbuf.Lexing.lex_curr_pos },
    %exp{lexbuf.Lexing.lex_last_pos },
    %exp{lexbuf.Lexing.lex_last_action },
    %exp{ lexbuf.Lexing.lex_start_pos }
   )
    
let lex_state i =
  let state = "__ocaml_lex_state"^string_of_int i in
  %exp{ $lid:state lexbuf }
    
let output_memory_actions (mvs:memory_action list) : exp list =
  List.map
    (fun x ->
      match x with
      | Copy(tgt,src) ->
          let u = output_mem_access tgt in
          let v = output_mem_access src in 
          %exp{ $u <- $v }
      | Set tgt ->
          let u = output_mem_access tgt in
          %exp{ $u <- $curr_pos }) mvs
    

(* length at least  one*)    
let output_action (mems:memory_action list) (r:automata_move) : exp list  =
   output_memory_actions mems @
  (match r with
  | Backtrack ->
      [ %exp{$curr_pos <- $last_pos };
        last_action]
  | Goto n -> [lex_state n])
let output_clause
    (pats:int list)
    (mems:memory_action list)
    (r:automata_move) =
  let pat = output_pats pats in
  let action = seq_sem (output_action mems r) in 
  %case{ $pat:pat -> $action }

let output_default_clause mems r =
  let action = seq_sem (output_action mems r) in
  %case{ _ -> $action }

(* output at least one case *)    
let output_moves (moves: (automata_move * memory_action list) array) : case list =
  let t = Hashtbl.create 17 in
  let add_move i (m,mems) =
    let (mems,r) = try Hashtbl.find t m with Not_found -> (mems,[]) in
    Hashtbl.replace t m (mems,(i::r)) in
  begin
    for i = 0 to 256 do
      add_move i moves.(i)
    done ;
    let most_frequent = ref Backtrack
    and most_mems = ref []
    and size = ref 0 in
    (Hashtbl.iter
       (fun m (mems,pats) ->
         let size_m = List.length pats in
         if size_m > !size then begin
           most_frequent := m ;
           most_mems := mems;
           size := size_m
         end
       )
       t;
     (Hashtbl.fold 
        (fun m (mems,pats) acc ->
          if m <> !most_frequent then
            output_clause (List.rev pats) mems m  :: acc
          else acc
        ) t []) @ [output_default_clause !most_mems !most_frequent])
  end
    
let output_tag_actions (mvs:tag_action list) : exp list =
  List.map
    (function
      | SetTag(t,m) ->
          let u = output_mem_access t in
          let v = output_mem_access m in 
          %exp{ $u <- $v }
      | EraseTag(t) ->
          let u = output_mem_access t in 
          %exp{ $u <- -1 }) mvs

    
let output_trans (i:int) (trans:automata)=
  let state = "__ocaml_lex_state"^(string_of_int i) in
  let e =
    match trans with
  | Perform(n,mvs) ->
      let es = output_tag_actions mvs in 
      seq_sem (es @ [ %exp{ $`int:n}]) 

  | Shift(trans,move) ->
      let moves = bar_of_list (output_moves move) in
      seq_sem
          (match trans with
          | Remember(n,mvs) ->
              let es = output_tag_actions mvs in
              (es@
               [ %exp{ $last_pos  <- $curr_pos };
                 %exp{ $last_action <- $`int:n };
                 %exp{ match __ocaml_lex_next_char lexbuf with | $moves }
              ])
          | No_remember ->
              [ %exp{ match __ocaml_lex_next_char lexbuf with | $moves }]) in
  %bind{ $lid:state lexbuf = $e  }
      
let output_args (args:string list) e =
  List.fold_right (fun a b -> %exp{ fun $lid:a -> $b }) args e
    
let output_automata (transitions:automata array) : bind list = 
  (Array.to_list (Array.mapi (fun i auto -> output_trans i auto) transitions))

  
    
let output_env (env:t_env) : bind list =
  let env =
    List.sort
      (function x y ->
        match (x,y) with
        | (((p1,_),_), ((p2,_),_)) ->
            if Locf.strictly_before p1 p2 then -1 else 1) env in
  let output_tag_access = function
    |(Mem i,d) ->
        %exp{ ($(output_mem_access i) + $`int:d) }
    |(Start,d) ->
        %exp{ ($start_pos+ $`int:d) }
    | (End,d) ->
        %exp{ ($curr_pos + $`int:d) } in
  List.map
    (fun (id,v) ->
      let (id : pat) = `Lid id  in
      match v with
      | Ident_string (o,nstart,nend) ->
          let sub =
            if o then %exp{Lexing.sub_lexeme_opt}
            else %exp{Lexing.sub_lexeme} in
          let nstart = output_tag_access nstart in
          let nend = output_tag_access nend in
          %bind{ $id = $sub lexbuf  $nstart $nend}
      | Ident_char (o,nstart) ->
          (* break the invariant ....
             "Lexing.sub_lexeme_char_opt" can not be an lident...
             it can only be an ident
           *)
          let sub =
            if o then %exp{ Lexing.sub_lexeme_char_opt}
            else  %exp{Lexing.sub_lexeme_char} in
          let nstart = output_tag_access nstart in
          let _loc = loc_of id in (* location *)
          %bind{ $id = $sub lexbuf $nstart }
    ) env
     
let output_entry
    ({auto_mem_size;
    auto_initial_state=(init_num,init_moves);
    auto_actions; },
     (transitions:automata array)) : FAst.exp  =
  let actions = seq_sem
      (%exp{ __ocaml_lex_init_lexbuf lexbuf $`int:auto_mem_size } ::
       output_memory_actions init_moves) in  
  let state = "__ocaml_lex_state" ^string_of_int init_num in
  let binds =
    and_of_list (auto_binds @ output_automata transitions) in 
  let cases =
    bar_of_list
     ((auto_actions |> List.map
      (function (num,env,act) ->
        let n = string_of_int num in
        match output_env env with
        | [] -> %case{ $int:n -> $act }
        | xs ->
            let bind = and_of_list xs in
            %case{ $int:n ->
              let $bind in
              $act })) @
       [ %case{ _ -> failwith   "lexing: empty token" }])  in
    %exp{
    function (lexbuf:Lexing.lexbuf) ->
      let rec $binds in
      begin
      $actions;
      let __ocaml_lex_result = $lid:state  lexbuf in
      begin
        lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p ;
        lexbuf.Lexing.lex_curr_p <-
          { (lexbuf.Lexing.lex_curr_p) with
            Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos };
        match __ocaml_lex_result with
        | $cases
      end
    end
  }


    

(* local variables: *)
(* compile-command: "cd ../main_annot && pmake compile_lex.cmo" *)
(* end: *)
