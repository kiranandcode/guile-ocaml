module Bindings = Bindings.Stubs(Bindings_stubs)
open Ctypes


type scm = unit ptr
(** SCM is the user level abstract C type that is used to represent
   all of Guile’s Scheme objects, no matter what the Scheme object
   type is. No C operation except assignment is guaranteed to work
   with variables of type SCM, so you should only use macros and
   functions to work with SCM values. Values are converted between C
   data types and the SCM type with utility functions and macros.  *)


module InitCallback = (val Foreign.(dynamic_funptr
                                    ~thread_registration:true
                                    (ptr void @-> returning (ptr void))))

let scm_with_guile =
  Foreign.foreign "scm_with_guile"
    (InitCallback.t @-> ptr void @-> returning (ptr void))
let scm_with_guile f  v =
  scm_with_guile (InitCallback.of_fun f) v

let scm_init_guile =
  Foreign.foreign "scm_init_guile"
    (void @-> returning void)

let scm_shell =
  Foreign.foreign "scm_shell"
    (int @-> ptr string @-> returning void)

let scm_shell argv =
  let argc = Array.length argv in
  let argv =  Ctypes.CArray.of_list string (Array.to_list argv) in
  scm_shell argc (CArray.start argv)
  
