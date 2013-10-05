include Format

let pp_print_list mf_a  fmt  lst =
  fprintf fmt "@[<1>[%a]@]"
    (fun fmt  -> List.iter (fun x ->
      fprintf fmt "%a@ " mf_a x )) lst

let pp_print_option mf_a fmt v =
  match v with
  | None -> fprintf fmt "None"
  | Some v -> fprintf fmt "Some @[%a@]" mf_a v 
let pp_print_int32: formatter -> int32 -> unit =
  fun fmt  a  -> fprintf fmt "%ld" a
let pp_print_int64: formatter -> int64 -> unit =
  fun fmt  a  -> fprintf fmt "%Ld" a
let pp_print_nativeint: formatter -> nativeint -> unit =
  fun fmt  a  -> fprintf fmt "%nd" a
let pp_print_float = pp_print_float
let pp_print_string: formatter -> string -> unit =
  fun fmt  a  -> fprintf fmt "%S" a
let pp_print_bool = pp_print_bool
let pp_print_char = pp_print_char
let pp_print_unit: formatter -> unit -> unit =
  fun fmt  _  -> fprintf fmt "()"

type space_formatter =  (unit, Format.formatter, unit )format     
(** duplicated *)      
let pp_list ?sep ?first  ?last fu f xs = 

      let first = Option.default ("":space_formatter) first in
      let last = Option.default ("":space_formatter) last in
      let sep = Option.default ("@ ":space_formatter) sep in
      let aux f = function
        | [] -> ()
        | [x] -> fu f x
        | xs ->
            let rec loop  f = function
              | [x] -> fu f x
              | x::xs ->  fprintf f "%a%(%)%a" fu x sep loop xs 
              | _ -> assert false  in 
            fprintf f "%(%)%a%(%)" first loop xs last in
      aux f xs
let pp_option :
    ?first:space_formatter -> ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a option -> unit
        = fun  ?first  ?last fu f a ->
     let first =
       match first with
       | Some x -> x
       | None -> ""
     and last =
       match last with
       | Some x -> x
       | None -> ""  in
     match a with
     | None -> ()
     | Some x -> fprintf f "%(%)%a%(%)" first fu x last
  
      

(* local variables: *)
(* compile-command: "pmake formatf.cmo" *)
(* end: *)
