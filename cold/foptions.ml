type spec_list = (string* Arg.spec* string) list 
let init_spec_list = ref []
let init spec_list = init_spec_list := spec_list
let add (name,spec,descr) =
  init_spec_list := (init_spec_list.contents @ [(name, spec, descr)])
let adds ls = init_spec_list := (init_spec_list.contents @ ls)