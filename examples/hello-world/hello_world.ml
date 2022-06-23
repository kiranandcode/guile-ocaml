let () =
  Guile.init ();
  ignore @@ Guile.Functions.register_fun1 "kiran" (fun n ->
    if Guile.Number.is_integer n then begin
      let n = Guile.Number.int_from_raw n in
      print_endline @@ Printf.sprintf "in OCAML!: %d"  n;
    end else begin
      print_endline @@ Printf.sprintf "recieved %s"  (Guile.Sexp.from_raw n |> Sexplib.Sexp.to_string_hum)
    end
    ;
    Guile.Bool.t
  );
  Printf.printf "result was %s\n%!" @@ Guile.to_string @@ Guile.eval Guile.Sexp.(to_raw [%sexp ["+"; 1; 3]]);
  Guile.shell ()
