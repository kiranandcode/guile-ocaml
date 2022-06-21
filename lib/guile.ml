let init_with f =
  ignore @@ Raw.scm_with_guile (fun v -> f (); v) Ctypes.null

let init () =
  Raw.scm_init_guile ()

let shell () =
  Raw.scm_shell Sys.argv
