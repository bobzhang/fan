DEFINE AGE(field) = fun (state,age) ->
  let (s,a) = age_stat state.field age.field in
  ({(state) with field=s}, {(age) with field=a});

let u = AGE(x);

(* also works with my fix, but not before *)
DEFINE NEW(class_name) = new class_name;
let test_NEW = NEW(foo);

(* still doesn't work with my fix, a stronger fix is still needed *)
DEFINE HAS_FIELD(field) = {field=field};
  
let test_HAS_FIELD = fun [HAS_FIELD(foo) -> foo];  

















