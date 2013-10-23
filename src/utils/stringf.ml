(* module String = struct *)
include String
    (* include BatString; *)
let init len f = 
  let s = create len in begin
    for i = 0 to len - 1 do
      unsafe_set s i (f i)
    done;
    s
  end

let is_empty s = s = ""
    
let not_empty s = s <> ""
    (*$T starts_with
      starts_with "foobarbaz" "foob"
      starts_with "foobarbaz" ""
      starts_with "" ""
      not (starts_with "bar" "foobar")
      not (starts_with "" "foo")
      starts_with "Jon \"Maddog\" Orwant" "Jon"
      not (starts_with "Jon \"Maddog\" Orwant" "Jon \"Maddog\" Orwants")
      not (starts_with "Jon \"Maddog\" Orwant" "Orwants")
     *) 
let starts_with str p =
  let len = length p in
  if length str < len then false
  else
    Return.cc
      (fun id -> begin 
        for i = 0 to len - 1 do
          if unsafe_get str i <> unsafe_get p i then
            Return.k id false
        done;
        true end)

      (*$T ends_with
        ends_with "foobarbaz" "rbaz"
        ends_with "foobarbaz" ""
        ends_with "" ""
        not (ends_with "foo" "foobar")
        not (ends_with "" "foo")
        ends_with "Jon \"Maddog\" Orwant" "want"
        not (ends_with "Jon \"Maddog\" Orwant" "I'm Jon \"Maddog\" Orwant")
        not (ends_with "Jon \"Maddog\" Orwant" "Jon")
       *)
let ends_with str p =
  let el = length p
  and sl = length str in
  let diff = sl - el in
  if diff < 0 then false (*string is too short*)
  else
    Return.cc @@ fun id ->
      begin 
        for i = 0 to el - 1 do
          if get str (diff + i) <> get p i then
            Return.k id false
        done;
        true
      end
      
let of_char = make 1

let drop_while f s =
  let len = length s in
  let found = ref false in
  let i = ref 0 in begin 
    while !i < len && not !found do
      if not (f s.[!i]) then 
        found:=true
      else incr i
    done ;
    String.sub s !i (len - !i)
  end

(**
   [neg_string "ab" ] = ["-ab"]
   [neg_string ""] = ["-"]
 *)
let neg n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n

let map f s =
  let l = length s in
  if l = 0 then s else 
  let r = create l in
  (for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
   r)

    

let lowercase s = map Char.lowercase s


let find_from str ofs sub = 
  let sublen = length sub in
  if sublen = 0 then ofs
      (*If [sub] is the empty string, by convention,
        it may be found wherever we started searching.*)
  else
    let len = length str in
    if len = 0 then raise Not_found else
    if 0 > ofs || ofs >= len then raise (Invalid_argument "index out of bounds")
    else
      Return.cc (fun id  -> begin
  	for i = ofs to len - sublen do
	  let j = ref 0 in
	  while unsafe_get str (i + !j) = unsafe_get sub !j do
	    incr j;
	    if !j = sublen then Return.k id i
	  done;
	done;
	raise Not_found
      end)
        
let find str sub = find_from str 0 sub

let split str sep =
  let p = find str sep in
  let len = length sep in
  let slen = length str in
  (sub str 0 p, sub str (p + len) (slen - p - len))


let rfind_from str suf sub = 
  let sublen = length sub 
  and len    = length str in
  if sublen = 0 then len
  else
    if len = 0 then raise Not_found else
    if 0 > suf || suf >= len then raise (Invalid_argument "index out of bounds")
    else
      Return.cc (fun id -> begin
  	for i = suf - sublen + 1 downto 0 do
	  (*Printf.printf "i:%i/suf:%i/sublen:%i/len:%i\n" i suf sublen len;*)
	  let j = ref 0 in
	  while unsafe_get str ( i + !j ) = unsafe_get sub !j do
	    incr j;
	    if !j = sublen then Return.k id i
	  done;
	done;
	raise Not_found
      end)

let rfind str sub = rfind_from str (String.length str - 1) sub

    (*
      LibUtil.String.nsplit ".a.b.c..d" ".";;
      - : string list = [""; "a"; "b"; "c"; ""; "d"]
     *)  
let nsplit str sep =
  if str = "" then []
  else if sep = "" then invalid_arg "nsplit: empty sep not allowed"
  else
    (* str is non empty *)
    let seplen = String.length sep in
    let rec aux acc ofs =
      if ofs >= 0 then (
        match
          (try Some (rfind_from str ofs sep)
          with Not_found -> None)
        with
        | Some idx -> (* sep found *)
            let end_of_sep = idx + seplen - 1 in
            if end_of_sep = ofs (* sep at end of str *)
            then aux (""::acc) (idx - 1)
            else
              let token = sub str (end_of_sep + 1) (ofs - end_of_sep) in
              aux (token::acc) (idx - 1)
        | None     -> (* sep NOT found *)
            (sub str 0 (ofs + 1))::acc
       )
      else
        (* Negative ofs: the last sep started at the beginning of str *)
        ""::acc in
    aux [] (length str - 1 )
      
      (* let filter_map f a = *)
      (*   let u = Array.filter *)
(* end *)

(* local variables: *)
(* compile-command: "pmake fstring.cmo" *)
(* end: *)
