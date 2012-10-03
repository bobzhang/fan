open Format

open FanUtil

module Debug =
                            struct let mode = fun _ -> (false)
 end


type section = string

let out_channel =
                        (try
                          let f = (Sys.getenv "CAMLP4_DEBUG_FILE") in
                          (open_out_gen (
                            [Open_wronly ; Open_creat ; Open_append ;
                             Open_text ] ) 438 f)
                         with
                         Not_found -> Pervasives.stderr)

let mode =
                                                           (try
                                                             let str =
                                                              (Sys.getenv
                                                                "CAMLP4_DEBUG") in
                                                             let rec loop =
                                                              fun acc ->
                                                               fun i ->
                                                                (try
                                                                  let pos =
                                                                   (String.index_from
                                                                    str i
                                                                    ':') in
                                                                  (loop (
                                                                    (SSet.add
                                                                    (
                                                                    (String.sub
                                                                    str i (
                                                                    (pos - i)
                                                                    )) ) acc)
                                                                    ) (
                                                                    (pos + 1)
                                                                    ))
                                                                 with
                                                                 Not_found ->
                                                                  (SSet.add (
                                                                    (String.sub
                                                                    str i (
                                                                    ((
                                                                    (String.length
                                                                    str) ) -
                                                                    i) )) )
                                                                    acc)) in
                                                             let sections =
                                                              (loop
                                                                SSet.empty 0) in
                                                             if (SSet.mem "*"
                                                                  sections) then
                                                              (
                                                              fun _ -> (true)
                                                              )
                                                             else
                                                              fun x ->
                                                               (SSet.mem x
                                                                 sections)
                                                            with
                                                            Not_found ->
                                                             fun _ -> (false))


let formatter =
 let header = "camlp4-debug: " in
 let at_bol = (ref true ) in
 (make_formatter (
   fun buf ->
    fun pos ->
     fun len ->
      for i = pos to (( (pos + len) ) - 1) do
       (
      if at_bol.contents then ( (output_string out_channel header) ) else ()
      );
       let ch = (String.get buf i) in
       (
       (output_char out_channel ch)
       );
       (( at_bol.contents ) := ( (ch = '\010') ))
      done ) ( fun ()  -> (flush out_channel) ))

let printf =
                                                   fun section ->
                                                    fun fmt ->
                                                     (fprintf formatter (
                                                       ("%s: " ^^ fmt) )
                                                       section)
