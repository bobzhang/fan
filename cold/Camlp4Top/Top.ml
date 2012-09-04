module Toploop :
 sig
  val print_location : (Format.formatter -> (Location.t -> unit))

  val print_warning :
   (Location.t -> (Format.formatter -> (Warnings.t -> unit)))

  val parse_toplevel_phrase :
   (Lexing.lexbuf -> Parsetree.toplevel_phrase) ref

  val parse_use_file : (Lexing.lexbuf -> Parsetree.toplevel_phrase list) ref

 end =
 struct
  let print_location =
   fun fmt -> fun loc -> (Toploop.print_location fmt ( (Obj.magic loc) ))

  let parse_toplevel_phrase = (Obj.magic Toploop.parse_toplevel_phrase)

  let parse_use_file = (Obj.magic Toploop.parse_use_file)

  let print_warning =
   fun loc ->
    fun fmt ->
     fun w ->
      (Toploop.print_warning ( (Obj.magic loc) ) fmt ( (Obj.magic w) ))

 end

open Parsetree

open Lexing

open Camlp4

open PreCast

open Syntax

open Camlp4.Sig


module Ast2pt =
 (Camlp4.Struct.Camlp4Ast2OCamlAst.Make)(Ast)

module Lexer =
                                                (Camlp4.Struct.Lexer.Make)
                                                 (Token)

external
                                                           not_filtered :
                                                           ('a ->
                                                            'a Gram.not_filtered) =
                                                            "%identity"


let initialization =
 lazy (
  if !Sys.interactive then
   (
   (Format.printf "\tCamlp4 Parsing version %s\n@." Camlp4_config.version)
   )
  else () )

let wrap =
              fun parse_fun ->
               fun lb ->
                let () = (Lazy.force initialization) in
                let () =
                 (Register.iter_and_take_callbacks ( fun (_, f) -> (f () ) )) in
                let not_filtered_token_stream = (Lexer.from_lexbuf lb) in
                let token_stream =
                 (Gram.filter ( (not_filtered not_filtered_token_stream) )) in
                (try
                  let (__strm : _ Stream.t) = token_stream in
                  (match (Stream.peek __strm) with
                   | Some (EOI, _) ->
                      ( (Stream.junk __strm) ); (raise End_of_file )
                   | _ -> (parse_fun token_stream))
                 with
                 | (((End_of_file | Sys.Break)
                     | Loc.Exc_located (_, (End_of_file | Sys.Break))) as x) ->
                    (raise x)
                 | x ->
                    let x =
                     (match x with
                      | Loc.Exc_located (loc, x) ->
                         (
                         (Toploop.print_location Format.err_formatter (
                           (Loc.to_ocaml_location loc) ))
                         );
                         x
                      | x -> x) in
                    (
                    (Format.eprintf "@[<0>%a@]@." Camlp4.ErrorHandler.print
                      x)
                    );
                    (raise Exit ))

let toplevel_phrase =
                                     fun token_stream ->
                                      (match
                                         (Gram.parse_tokens_after_filter
                                           Syntax.top_phrase token_stream) with
                                       | Some (str_item) ->
                                          let str_item =
                                           (AstFilters.fold_topphrase_filters
                                             (
                                             fun t ->
                                              fun filter -> (filter t) )
                                             str_item) in
                                          (Ast2pt.phrase str_item)
                                       | None -> (raise End_of_file ))


let use_file =
 fun token_stream ->
  let (pl0, eoi) =
   let rec loop =
    fun ()
      ->
     let (pl, stopped_at_directive) =
      (Gram.parse_tokens_after_filter Syntax.use_file token_stream) in
     if (stopped_at_directive <> None ) then
      (
      (match pl with
       | (Ast.StDir (_, "load", Ast.ExStr (_, s)) :: []) ->
          ( (Topdirs.dir_load Format.std_formatter s) ); (loop () )
       | (Ast.StDir (_, "directory", Ast.ExStr (_, s)) :: []) ->
          ( (Topdirs.dir_directory s) ); (loop () )
       | _ -> (pl, false ))
      )
     else (pl, true ) in
   (loop () ) in
  let pl =
   if eoi then [] 
   else
    let rec loop =
     fun ()
       ->
      let (pl, stopped_at_directive) =
       (Gram.parse_tokens_after_filter Syntax.use_file token_stream) in
      if (stopped_at_directive <> None ) then ( (pl @ ( (loop () ) )) )
      else pl in
    (loop () ) in
  (List.map Ast2pt.phrase ( (pl0 @ pl) ))

let _ = (Toploop.parse_toplevel_phrase
                                                     := (
                                                     (wrap toplevel_phrase)
                                                     ))

let _ = (Toploop.parse_use_file
                                                                   := (
                                                                   (wrap
                                                                    use_file)
                                                                   ))


let _ = (current_warning := (
          fun loc ->
           fun txt ->
            (Toploop.print_warning ( (Loc.to_ocaml_location loc) )
              Format.err_formatter ( (Warnings.Camlp4 (txt)) )) ))

let _ = 
                                                                    (Register.iter_and_take_callbacks
                                                                    (
                                                                    fun 
                                                                    (_, f) ->
                                                                    (f () )
                                                                    ))
