module Make =
 functor (Structure : Structure.S) ->
  struct
   open Structure

   open Format

   module Parse = (Parser.Make)(Structure)

   module Fail = (Failed.Make)(Structure)

   open FanSig.Grammar

   module Stream =
    struct
     type 'a t = 'a Stream.t

     exception Failure = Stream.Failure

     exception Error = Stream.Error

    end

   let sfold0 =
    fun f ->
     fun e ->
      fun _entry ->
       fun _symbl ->
        fun psymb ->
         let rec fold =
          fun accu ->
           fun (__strm :
             _ Stream.t) ->
            (match
               (try (Some (psymb __strm)) with
                Stream.Failure -> (None)) with
             | Some (a) -> (fold ( (f a accu) ) __strm)
             | _ -> accu) in
         fun (__strm : _ Stream.t) -> (fold e __strm)

   let sfold1 =
    fun f ->
     fun e ->
      fun _entry ->
       fun _symbl ->
        fun psymb ->
         let rec fold =
          fun accu ->
           fun (__strm :
             _ Stream.t) ->
            (match
               (try (Some (psymb __strm)) with
                Stream.Failure -> (None)) with
             | Some (a) -> (fold ( (f a accu) ) __strm)
             | _ -> accu) in
         fun (__strm :
           _ Stream.t) ->
          let a = (psymb __strm) in
          (try (fold ( (f a e) ) __strm) with
           Stream.Failure -> (raise ( (Stream.Error ("")) )))

   let sfold0sep =
    fun f ->
     fun e ->
      fun entry ->
       fun symbl ->
        fun psymb ->
         fun psep ->
          let failed =
           function
           | (symb :: sep :: []) -> (Fail.symb_failed_txt entry sep symb)
           | _ -> "failed" in
          let rec kont =
           fun accu ->
            fun (__strm :
              _ Stream.t) ->
             (match
                (try (Some (psep __strm)) with
                 Stream.Failure -> (None)) with
              | Some (()) ->
                 let a =
                  (try (psymb __strm) with
                   Stream.Failure ->
                    (raise ( (Stream.Error (failed symbl)) ))) in
                 (kont ( (f a accu) ) __strm)
              | _ -> accu) in
          fun (__strm :
            _ Stream.t) ->
           (match
              (try (Some (psymb __strm)) with
               Stream.Failure -> (None)) with
            | Some (a) -> (kont ( (f a e) ) __strm)
            | _ -> e)

   let sfold1sep =
    fun f ->
     fun e ->
      fun entry ->
       fun symbl ->
        fun psymb ->
         fun psep ->
          let failed =
           function
           | (symb :: sep :: []) -> (Fail.symb_failed_txt entry sep symb)
           | _ -> "failed" in
          let parse_top =
           function
           | (symb :: _ :: []) -> (Parse.parse_top_symb entry symb)
           | _ -> (raise Stream.Failure ) in
          let rec kont =
           fun accu ->
            fun (__strm :
              _ Stream.t) ->
             (match
                (try (Some (psep __strm)) with
                 Stream.Failure -> (None)) with
              | Some (()) ->
                 let a =
                  (try
                    (try (psymb __strm) with
                     Stream.Failure ->
                      let a =
                       (try (parse_top symbl __strm) with
                        Stream.Failure ->
                         (raise ( (Stream.Error (failed symbl)) ))) in
                      (Obj.magic a))
                   with
                   Stream.Failure -> (raise ( (Stream.Error ("")) ))) in
                 (kont ( (f a accu) ) __strm)
              | _ -> accu) in
          fun (__strm :
            _ Stream.t) ->
           let a = (psymb __strm) in (kont ( (f a e) ) __strm)

  end
