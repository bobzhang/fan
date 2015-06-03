let sfold0 =
  function
  | f ->
      (function
       | e ->
           (function
            | _entry ->
                (function
                 | _symbl ->
                     (function
                      | psymb ->
                          let rec fold =
                            function
                            | accu ->
                                (function
                                 | (__strm : _ Streamf.t) ->
                                     (match try Some (psymb __strm)
                                            with
                                            | Streamf.NotConsumed  -> None
                                      with
                                      | Some a -> fold (f a accu) __strm
                                      | _ -> accu)) in
                          (function | (__strm : _ Streamf.t) -> fold e __strm)))))
let sfold1 =
  function
  | f ->
      (function
       | e ->
           (function
            | _entry ->
                (function
                 | _symbl ->
                     (function
                      | psymb ->
                          let rec fold =
                            function
                            | accu ->
                                (function
                                 | (__strm : _ Streamf.t) ->
                                     (match try Some (psymb __strm)
                                            with
                                            | Streamf.NotConsumed  -> None
                                      with
                                      | Some a -> fold (f a accu) __strm
                                      | _ -> accu)) in
                          (function
                           | (__strm : _ Streamf.t) ->
                               let a = psymb __strm in
                               (try fold (f a e) __strm
                                with
                                | Streamf.NotConsumed  ->
                                    raise (Streamf.Error "")))))))
let sfold0sep =
  function
  | f ->
      (function
       | e ->
           (function
            | entry ->
                (function
                 | symbl ->
                     (function
                      | psymb ->
                          (function
                           | psep ->
                               let failed =
                                 function
                                 | symb::sep::[] ->
                                     Gentry.symb_failed_txt entry sep symb
                                 | _ -> assert false in
                               let rec kont =
                                 function
                                 | accu ->
                                     (function
                                      | (__strm : _ Streamf.t) ->
                                          (match try Some (psep __strm)
                                                 with
                                                 | Streamf.NotConsumed  ->
                                                     None
                                           with
                                           | Some () ->
                                               let a =
                                                 try psymb __strm
                                                 with
                                                 | Streamf.NotConsumed  ->
                                                     raise
                                                       (Streamf.Error
                                                          (failed symbl)) in
                                               kont (f a accu) __strm
                                           | _ -> accu)) in
                               (function
                                | (__strm : _ Streamf.t) ->
                                    (match try Some (psymb __strm)
                                           with
                                           | Streamf.NotConsumed  -> None
                                     with
                                     | Some a -> kont (f a e) __strm
                                     | _ -> e)))))))
let sfold1sep =
  function
  | f ->
      (function
       | e ->
           (function
            | entry ->
                (function
                 | symbl ->
                     (function
                      | psymb ->
                          (function
                           | psep ->
                               let failed =
                                 function
                                 | symb::sep::[] ->
                                     Gentry.symb_failed_txt entry sep symb
                                 | _ -> assert false in
                               let parse_top =
                                 function
                                 | symb::_::[] ->
                                     Gentry.parser_of_symbol entry symb
                                 | _ -> raise Streamf.NotConsumed in
                               let rec kont =
                                 function
                                 | accu ->
                                     (function
                                      | (__strm : _ Streamf.t) ->
                                          (match try Some (psep __strm)
                                                 with
                                                 | Streamf.NotConsumed  ->
                                                     None
                                           with
                                           | Some () ->
                                               let a =
                                                 try
                                                   try psymb __strm
                                                   with
                                                   | Streamf.NotConsumed  ->
                                                       let a =
                                                         try
                                                           parse_top symbl
                                                             __strm
                                                         with
                                                         | Streamf.NotConsumed
                                                              ->
                                                             raise
                                                               (Streamf.Error
                                                                  (failed
                                                                    symbl)) in
                                                       Obj.magic a
                                                 with
                                                 | Streamf.NotConsumed  ->
                                                     raise (Streamf.Error "") in
                                               kont (f a accu) __strm
                                           | _ -> accu)) in
                               (function
                                | (__strm : _ Streamf.t) ->
                                    let a = psymb __strm in
                                    kont (f a e) __strm))))))
