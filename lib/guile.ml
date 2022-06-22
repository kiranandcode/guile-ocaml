let init_with f =
  ignore @@ Raw.scm_with_guile (fun v -> f (); v) Ctypes.null

let init () =
  Raw.scm_init_guile ()

let shell () =
  Raw.scm_shell Sys.argv

module Bool = struct
  let booleanp v = Raw.scm_boolean_p v
  let is_bool v = Raw.scm_is_bool

  let not v = Raw.scm_not v

  let from_bool v = Raw.scm_from_bool v
  let to_bool v = Raw.scm_to_bool v

end

module Pair = struct

  let cons hd tl = Raw.scm_cons hd tl

  let car pair = Raw.scm_car pair
  let cdr pair = Raw.scm_cdr pair

  let caar pair = Raw.scm_caar pair
  let cadr pair = Raw.scm_cadr pair
  let cdar pair = Raw.scm_cdar pair

  let hd pair = car pair
  let tl pair = cdr pair


  let consp x = Raw.scm_is_pair x

  let nconsp x = not (consp x)

end
