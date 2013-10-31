(** Names and name functions **)

(** Builtin predicates and compound constructors **)

let nil = "nil" and cons = "cons"
let add = "add" and sub = "sub" and neg = "neg"

let same = "same" and diff = "diff"
let is = "is"
let eq = "eq" and ne = "ne"
let lt = "lt" and lte = "lte"
let gt = "gt" and gte = "gte"

let notp = "not"
let cut = "cut"

let truep = "true" and fail = "fail"
let repeat = "repeat"
let write = "write" and nl = "nl"

let builtin_preds = [
	(same,2); (diff,2);
	(is,2);
	(eq,2); (ne,2);
	(lt,2); (lte,2);
	(gt,2); (gte,2);
	(notp,1); (cut,0);
	(truep,0); (fail,0);
	(repeat,0);
	(write,1); (nl,0)
]

(** Code generation names **)

let comp n = String.capitalize n
let pred n v = n ^ "_" ^ (PlVersion.to_string v)
let pred_var n = "_arg" ^ (string_of_int n)

let f = "_f"

let found_exc = "Found"
let cut_exc = "Cut"
let notalist_exc = "Not_a_list"
let notanint_exc = "Not_an_int"

let val_type = "plval"

let int_cons = "Int"

let cut_id = "_cutid"
let my_cut_id = "_mycutid"
let list_of = "list_of_" ^ val_type
let int_of = "int_of_" ^ val_type
let string_of = "string_of_" ^ val_type
