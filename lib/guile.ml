let scm_init f =
  ignore @@ Raw.scm_with_guile (fun v -> f (); v) Ctypes.null


