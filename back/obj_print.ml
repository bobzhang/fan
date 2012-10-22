open Format
class _indent= object(self:'self)
  method longident ppf (x:Longident.t) = match x with
    | Lident s -> fprintf ppf "%s" s
    | Ldot(y,s) -> (match s.[0] with
          | 'a'..'z' | 'A' .. 'Z' ->
              fprintf ppf "%a.%s" self#longident y s
          | _ ->
              fprintf ppf "%a.(@ %s@ )@ " self#longident y s)
    | Longident.Lapply (y,s)->
        fprintf ppf "%a(%a)" self#longident y self#longident s
  method longident_loc ppf ({txt;_}:Longident.t Location.loc) =
    fprintf ppf "%a" self#longident txt
  method constant ppf x = match x with
  | Const_int i -> fprintf ppf "%d" i
  | Const_char i -> fprintf ppf "%C"  i 
  | Const_string i -> fprintf ppf "%S" i
  | Const_float  i -> fprintf ppf "%s" i 
  | Const_int32 i -> fprintf ppf "%ldl" i
  | Const_int64 i -> fprintf ppf "%LdL" i
  | Const_nativeint i -> fprintf ppf "%ndn" i
  method mutable_flag ppf   = function
    | Immutable -> ()
    | Mutable -> fprintf ppf "mutable@ "
  method virtual_flag ppf  = function
    | Concrete -> ()
    | Virtual -> fprintf ppf "virtual@ "
  method rec_flag ppf = function
    | Nonrecursive -> ()
    | Recursive | Default -> fprintf ppf "rec@ "
  method direction_flag ppf = function
    | Upto -> fprintf ppf "to@ "
    | Downto -> fprintf ppf "downto@ "
  method private_flag ppf = function
    | Public -> ()
    | Private -> fprintf ppf "private@ "
end;;



  



















