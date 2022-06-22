module Bindings = Bindings.Stubs(Bindings_stubs)
open Ctypes
external linkme : unit -> int = "linkme"

let scm = ptr void
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

(* ====================================================================  *)
(* Boolean  *)
(* ====================================================================  *)

let scm_not : scm -> scm = Foreign.foreign "scm_not" (scm @-> returning scm)

let scm_boolean_p : scm -> scm = Foreign.foreign "scm_boolean_p" (scm @-> returning scm)

(* let scm_is_true : scm -> bool = Foreign.foreign "scm_is_true" (scm @-> returning bool) *)

(* let scm_is_false : scm -> bool = Foreign.foreign "scm_is_false" (scm @-> returning bool) *)

let scm_is_bool : scm -> bool = Foreign.foreign "scm_is_bool" (scm @-> returning bool)

let scm_from_bool : bool -> scm = Foreign.foreign "scm_from_bool_" (bool @-> returning scm)

let scm_to_bool : scm -> bool = Foreign.foreign "scm_to_bool" (scm @-> returning bool)

(* ====================================================================  *)
(* Numbers  *)
(* ====================================================================  *)

let scm_number_p : scm -> scm = Foreign.foreign "scm_number_p" (scm @-> returning scm)
let scm_is_number : scm -> bool = Foreign.foreign "scm_is_number" (scm @-> returning bool)

let scm_integer_p : scm -> scm = Foreign.foreign "scm_integer_p" (scm @-> returning scm)
let scm_is_integer : scm -> bool = Foreign.foreign "scm_is_integer" (scm @-> returning bool)

let scm_exact_integer_p : scm -> scm = Foreign.foreign "scm_exact_integer_p" (scm @-> returning scm)
let scm_is_exact_integer : scm -> bool = Foreign.foreign "scm_is_exact_integer" (scm @-> returning bool)

(* let scm_to_char : scm -> char = Foreign.foreign "scm_to_char" (scm @-> returning char) *)
(* let scm_to_schar : scm -> int = Foreign.foreign "scm_to_schar" (scm @-> returning schar) *)
(* let scm_to_uchar : scm -> Unsigned.UChar.t = Foreign.foreign "scm_to_uchar" (scm @-> returning uchar) *)
(* let scm_to_short : scm -> int = Foreign.foreign "scm_to_short" (scm @-> returning short) *)
(* let scm_to_ushort : scm -> Unsigned.UShort.t = Foreign.foreign "scm_to_ushort" (scm @-> returning ushort) *)
(* let scm_to_int : scm -> int = Foreign.foreign "scm_to_int" (scm @-> returning int) *)
(* let scm_to_uint : scm -> Unsigned.UInt.t = Foreign.foreign "scm_to_uint" (scm @-> returning uint) *)
(* let scm_to_long : scm -> Signed.Long.t = Foreign.foreign "scm_to_long" (scm @-> returning long) *)
(* let scm_to_ulong : scm -> Unsigned.ULong.t = Foreign.foreign "scm_to_ulong" (scm @-> returning ulong) *)
(* let scm_to_long_long : scm -> Signed.LLong.t = Foreign.foreign "scm_to_long_long" (scm @-> returning llong) *)
(* let scm_to_ulong_long : scm -> Unsigned.ULLong.t = Foreign.foreign "scm_to_ulong_long" (scm @-> returning ullong) *)
(* let scm_to_size_t : scm -> Unsigned.Size_t.t = Foreign.foreign "scm_to_size_t" (scm @-> returning size_t) *)

(* let scm_from_char : char -> scm = Foreign.foreign "scm_from_char" ( char @-> returning scm) *)
(* let scm_from_schar : int -> scm = Foreign.foreign "scm_from_schar" ( schar @-> returning scm) *)
(* let scm_from_uchar : Unsigned.UChar.t -> scm = Foreign.foreign "scm_from_uchar" ( uchar @-> returning scm) *)
(* let scm_from_short : int -> scm = Foreign.foreign "scm_from_short" ( short @-> returning scm) *)
(* let scm_from_ushort : Unsigned.UShort.t -> scm = Foreign.foreign "scm_from_ushort" ( ushort @-> returning scm) *)
(* let scm_from_int : int -> scm = Foreign.foreign "scm_from_int" ( int @-> returning scm) *)
(* let scm_from_uint : Unsigned.UInt.t -> scm = Foreign.foreign "scm_from_uint" ( uint @-> returning scm) *)
(* let scm_from_long : Signed.Long.t -> scm = Foreign.foreign "scm_from_long" ( long @-> returning scm) *)
(* let scm_from_ulong : Unsigned.ULong.t -> scm = Foreign.foreign "scm_from_ulong" ( ulong @-> returning scm) *)
(* let scm_from_long_long : Signed.LLong.t -> scm = Foreign.foreign "scm_from_long_long" ( llong @-> returning scm) *)
(* let scm_from_ulong_long : Unsigned.ULLong.t -> scm = Foreign.foreign "scm_from_ulong_long" ( ullong @-> returning scm) *)
(* let scm_from_size_t : Unsigned.Size_t.t -> scm = Foreign.foreign "scm_from_size_t" ( size_t @-> returning scm) *)


(* ====================================================================  *)
(* Pair  *)
(* ====================================================================  *)

let scm_is_pair : scm -> bool =
  Foreign.foreign "scm_is_pair" (scm @-> returning bool)

let scm_cons : scm -> scm -> scm =
  Foreign.foreign "scm_cons" (scm @-> scm @-> returning scm)

let scm_car : scm -> scm =
  Foreign.foreign "scm_car" (scm @-> returning scm)

let scm_cdr : scm -> scm =
  Foreign.foreign "scm_cdr" (scm @-> returning scm)

let scm_setcar : scm -> scm -> unit =
  Foreign.foreign "scm_setcar" (scm @-> scm @-> returning void)

let scm_setcdr : scm -> scm -> unit =
  Foreign.foreign "scm_setcdr" (scm @-> scm @-> returning void)

let scm_caar : scm -> scm =
  Foreign.foreign "scm_caar" (scm @-> returning scm)

let scm_cadr : scm -> scm =
  Foreign.foreign "scm_cadr" (scm @-> returning scm)

let scm_cdar : scm -> scm =
  Foreign.foreign "scm_cdar" (scm @-> returning scm)
