
let var = ref 0

let incr_var v =
  if not Guile.(v = undefined) then
    failwith "expected nullary argument";
  incr var;
  Guile.eol

let get_var v =
  if not Guile.(v = undefined) then
    failwith "expected nullary argument";
  let v = !var in
  Guile.Number.int_to_raw v

let () =
  Guile.init ();
  ignore @@ Guile.Functions.register_fun1 "incr-var"
    ~no_opt:1 incr_var;
  ignore @@ Guile.Functions.register_fun1 "get-var"
    ~no_opt:1 get_var;
  Guile.shell ()
