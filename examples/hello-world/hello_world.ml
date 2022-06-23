
let my_fun s =
  if not @@ Guile.String.is_string s then
    failwith "expected string input";
  let s = Guile.String.from_raw s in
  Format.printf "swipl -> OCaml: %s\n%!" s;
  Guile.String.to_raw (s ^ " world\n")


let () =
  Guile.init ();
  ignore @@ Guile.Functions.register_fun1 "my-fun" my_fun;

  let s = 
  Guile.eval_string {|
    (let ((x "hello"))
        (set! x (my-fun x))
        (display x)
    x)
  |} in
  let result = Guile.to_string s in
  Printf.printf "OCaml -> swipl: %s\n%!" @@ result

