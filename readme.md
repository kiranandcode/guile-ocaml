# Guile-OCaml

Documentation available at: https://gopiandcode.github.io/guile-ocaml/

Guile-ocaml is a Free Software library that provides high-level OCaml
bindings to the FFI interface for GNU Guile Scheme. The aim of these
bindings are to provide an easy way for OCaml developers to extend
their OCaml applications with GNU Guile scheme scripting capabilities,
providing simple combinators to translate terms and send queries
between the two languages.

```ocaml
(* initialise GNU Guile *)
let () = Guile.init () in
(* expose OCaml functions to Guile scheme *)
let _ = Guile.Functions.register_fun1 "my-fun" ~no_opt:1
    (fun _ -> print_endline "hello world!"; Guile.eol) in
(* start guile repl *)
Guile.shell ()
```
