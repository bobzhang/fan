module IdDebugParser =
 struct let name = "Camlp4DebugParser"
 let version = Sys.ocaml_version
 end

open FanSig


module MakeDebugParser =
 functor (Syntax : Sig.Camlp4Syntax) ->
  struct
   open Sig

   include Syntax

   module StringSet = (Set.Make)(String)

   let debug_mode =
    (try
      let str = (Sys.getenv "STATIC_CAMLP4_DEBUG") in
      let rec loop =
       fun acc ->
        fun i ->
         (try
           let pos = (String.index_from str i ':') in
           (loop ( (StringSet.add ( (String.sub str i ( (pos - i) )) ) acc) )
             ( (pos + 1) ))
          with
          Not_found ->
           (StringSet.add (
             (String.sub str i ( (( (String.length str) ) - i) )) ) acc)) in
      let sections = (loop StringSet.empty 0) in
      if (StringSet.mem "*" sections) then ( fun _ -> (true) )
      else fun x -> (StringSet.mem x sections)
     with
     Not_found -> fun _ -> (false))

   let rec apply =
    fun accu ->
     function
     | [] -> accu
     | (x :: xs) ->
        let _loc = (Ast.loc_of_expr x) in
        (apply ( (Ast.ExApp (_loc, accu, x)) ) xs)

   let mk_debug_mode =
    fun _loc ->
     function
     | None ->
        (Ast.ExId
          (_loc, (
           (Ast.IdAcc
             (_loc, ( (Ast.IdUid (_loc, "Debug")) ), (
              (Ast.IdLid (_loc, "mode")) ))) )))
     | Some (m) ->
        (Ast.ExId
          (_loc, (
           (Ast.IdAcc
             (_loc, ( (Ast.IdUid (_loc, m)) ), (
              (Ast.IdAcc
                (_loc, ( (Ast.IdUid (_loc, "Debug")) ), (
                 (Ast.IdLid (_loc, "mode")) ))) ))) )))

   let mk_debug =
    fun _loc ->
     fun m ->
      fun fmt ->
       fun section ->
        fun args ->
         let call =
          (apply (
            (Ast.ExApp
              (_loc, (
               (Ast.ExApp
                 (_loc, (
                  (Ast.ExId
                    (_loc, (
                     (Ast.IdAcc
                       (_loc, ( (Ast.IdUid (_loc, "Debug")) ), (
                        (Ast.IdLid (_loc, "printf")) ))) ))) ), (
                  (Ast.ExStr (_loc, section)) ))) ), (
               (Ast.ExStr (_loc, fmt)) ))) ) args) in
         (Ast.ExIfe
           (_loc, (
            (Ast.ExApp
              (_loc, ( (mk_debug_mode _loc m) ), (
               (Ast.ExStr (_loc, section)) ))) ), call, (
            (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) ))) )))

   let _ = let _ = (expr : 'expr Gram.Entry.t) in
           let grammar_entry_create = Gram.Entry.mk in
           let end_or_in =
            ((grammar_entry_create "end_or_in") : 'end_or_in Gram.Entry.t)
           and start_debug =
            ((grammar_entry_create "start_debug") :
              'start_debug Gram.Entry.t) in
           (
           (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
             ((fun ()
                 ->
                (None , (
                 [(None , None , (
                   [((
                     [(
                      (Gram.Snterm
                        (Gram.Entry.obj (
                          (start_debug : 'start_debug Gram.Entry.t) ))) ); (
                      (Gram.Stoken
                        (( function | LIDENT (_) -> (true) | _ -> (false) ),
                         "LIDENT _")) ); (
                      (Gram.Stoken
                        (( function | STRING (_) -> (true) | _ -> (false) ),
                         "STRING _")) ); (
                      (Gram.Slist0
                        ((Gram.Snterml
                           ((
                            (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) ))
                            ), ".")))) ); (
                      (Gram.Snterm
                        (Gram.Entry.obj (
                          (end_or_in : 'end_or_in Gram.Entry.t) ))) )] ), (
                     (Gram.Action.mk (
                       fun (x :
                         'end_or_in) ->
                        fun (args :
                          'expr list) ->
                         fun (fmt :
                           Gram.Token.t) ->
                          fun (section :
                            Gram.Token.t) ->
                           fun (m :
                             'start_debug) ->
                            fun (_loc :
                              FanLoc.t) ->
                             (let fmt = (Gram.Token.extract_string fmt) in
                              let section =
                               (Gram.Token.extract_string section) in
                              (match (x, ( (debug_mode section) )) with
                               | (None, false) ->
                                  (Ast.ExId
                                    (_loc, ( (Ast.IdUid (_loc, "()")) )))
                               | (Some (e), false) -> e
                               | (None, _) ->
                                  (mk_debug _loc m fmt section args)
                               | (Some (e), _) ->
                                  (Ast.ExLet
                                    (_loc, Ast.ReNil , (
                                     (Ast.BiEq
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, ( (Ast.IdUid (_loc, "()"))
                                           ))) ), (
                                        (mk_debug _loc m fmt section args) )))
                                     ), e))) : 'expr) )) ))] ))] ))) () ) ))
           );
           (
           (Gram.extend ( (end_or_in : 'end_or_in Gram.Entry.t) ) (
             ((fun ()
                 ->
                (None , (
                 [(None , None , (
                   [((
                     [( (Gram.Skeyword ("in")) ); (
                      (Gram.Snterm
                        (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) ))) )]
                     ), (
                     (Gram.Action.mk (
                       fun (e :
                         'expr) ->
                        fun _ ->
                         fun (_loc : FanLoc.t) -> ((Some (e)) : 'end_or_in)
                       )) ));
                    (( [( (Gram.Skeyword ("end")) )] ), (
                     (Gram.Action.mk (
                       fun _ ->
                        fun (_loc : FanLoc.t) -> ((None) : 'end_or_in) )) ))]
                   ))] ))) () ) ))
           );
           (Gram.extend ( (start_debug : 'start_debug Gram.Entry.t) ) (
             ((fun ()
                 ->
                (None , (
                 [(None , None , (
                   [((
                     [(
                      (Gram.Stoken
                        ((
                         function
                         | LIDENT ("camlp4_debug") -> (true)
                         | _ -> (false) ), "LIDENT (\"camlp4_debug\")")) )]
                     ), (
                     (Gram.Action.mk (
                       fun (__camlp4_0 :
                         Gram.Token.t) ->
                        fun (_loc :
                          FanLoc.t) ->
                         (match __camlp4_0 with
                          | LIDENT ("camlp4_debug") ->
                             ((Some ("Camlp4")) : 'start_debug)
                          | _ -> assert false) )) ));
                    ((
                     [(
                      (Gram.Stoken
                        ((
                         function | LIDENT ("debug") -> (true) | _ -> (false)
                         ), "LIDENT (\"debug\")")) )] ), (
                     (Gram.Action.mk (
                       fun (__camlp4_0 :
                         Gram.Token.t) ->
                        fun (_loc :
                          FanLoc.t) ->
                         (match __camlp4_0 with
                          | LIDENT ("debug") -> ((None) : 'start_debug)
                          | _ -> assert false) )) ))] ))] ))) () ) ))

  end
