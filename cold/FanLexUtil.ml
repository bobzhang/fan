open Format
open FanLexer
open LibUtil
let rec clean (__strm : _ Stream.t ) =
  match Stream.peek __strm with
  | Some (`EOI,loc) ->
      (Stream.junk __strm; Stream.lsing (fun _ -> (`EOI, loc)))
  | Some x ->
      (Stream.junk __strm;
       let xs = __strm in Stream.icons x (Stream.slazy (fun _ -> clean xs)))
  | _ -> Stream.sempty
let debug_from_string str =
  let loc = FanLoc.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Stream.iter
       (fun (t,loc) ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))
let debug_from_file file =
  let loc = FanLoc.mk file in
  let chan = open_in file in
  let stream = Stream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (Stream.iter
       (fun (t,loc) ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))