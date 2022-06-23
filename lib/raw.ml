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

let guile_void_callback =
  Foreign.funptr ~thread_registration:true
    (ptr void @-> returning (ptr void))
let guile_handler_callback =
  Foreign.funptr ~thread_registration:true
    (ptr void @-> scm @-> scm @-> returning scm)

module GuileCallback1 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> returning (scm)))
module GuileCallback2 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> returning (scm)))
module GuileCallback3 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback4 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback5 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback6 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback7 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback8 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback9 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))
module GuileCallback10 = (val Foreign.dynamic_funptr ~thread_registration:true (scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> scm @-> returning (scm)))

let scm_with_guile =
  Foreign.foreign "scm_with_guile"
    (guile_void_callback @-> ptr void @-> returning (ptr void))
let scm_with_guile f v =
  scm_with_guile f v

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

let scm_primitive_load = Foreign.foreign "scm_c_primitive_load" (string @-> returning scm)

(* ====================================================================  *)
(* Modules  *)
(* ====================================================================  *)
let scm_resolve_module = Foreign.foreign "scm_c_resolve_module" (string @-> returning scm)
let scm_use_module = Foreign.foreign "scm_c_use_module" (string @-> returning scm)
let scm_define_module = Foreign.foreign "scm_c_define_module" (string @-> guile_void_callback @-> ptr void @-> returning scm)
let scm_export = Foreign.foreign "scm_c_export" (string @-> ptr void @-> returning void)
let scm_variable = Foreign.foreign "scm_c_public_variable" (string @-> string @-> returning scm)
let scm_variable_ref = Foreign.foreign "scm_c_public_ref" (string @-> string @-> returning scm)
let scm_call_with_current_module =
  Foreign.foreign "scm_c_call_with_current_module" (scm @-> guile_void_callback @-> ptr void @-> returning scm)

(* ====================================================================  *)
(* Bindings  *)
(* ====================================================================  *)

let scm_define = Foreign.foreign "scm_c_define" (string @-> scm @-> returning void)
let scm_defined_p = Foreign.foreign "scm_defined_p" (scm @-> scm @-> returning scm)

(* ====================================================================  *)
(* Control flow  *)
(* ====================================================================  *)

let scm_with_continuation_barrier =
  Foreign.foreign "scm_c_with_continuation_barrier"
    (guile_void_callback @-> ptr void @-> returning (ptr void))
let scm_with_continuation_barrier f v =
  scm_with_continuation_barrier f v

let scm_error = Foreign.foreign "scm_error" (scm @-> string_opt @-> string_opt @-> scm @-> scm @-> returning scm)
  
let scm_c_catch = Foreign.foreign "scm_internal_catch" (scm @-> guile_void_callback @-> ptr void @-> guile_handler_callback @-> ptr void @-> returning scm)

(* ====================================================================  *)
(* FFI  *)
(* ====================================================================  *)

let scm_pointer_to_procedure =
  Foreign.foreign "scm_pointer_to_procedure" (scm @-> scm @-> scm @-> returning scm)

let scm_define_gsubr_1 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback1.t @-> returning scm)
let scm_define_gsubr_1 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 1 - no_opt - rst in
  scm_define_gsubr_1 name no_required no_opt rst (GuileCallback1.of_fun f)

let scm_define_gsubr_2 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback2.t @-> returning scm)
let scm_define_gsubr_2 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 2 - no_opt - rst in
  scm_define_gsubr_2 name no_required no_opt rst (GuileCallback2.of_fun f)

let scm_define_gsubr_3 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback3.t @-> returning scm)
let scm_define_gsubr_3 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 3 - no_opt - rst in
  scm_define_gsubr_3 name no_required no_opt rst (GuileCallback3.of_fun f)

let scm_define_gsubr_4 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback4.t @-> returning scm)
let scm_define_gsubr_4 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 4 - no_opt - rst in
  scm_define_gsubr_4 name no_required no_opt rst (GuileCallback4.of_fun f)

let scm_define_gsubr_5 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback5.t @-> returning scm)
let scm_define_gsubr_5 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 5 - no_opt - rst in
  scm_define_gsubr_5 name no_required no_opt rst (GuileCallback5.of_fun f)

let scm_define_gsubr_6 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback6.t @-> returning scm)
let scm_define_gsubr_6 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 6 - no_opt - rst in
  scm_define_gsubr_6 name no_required no_opt rst (GuileCallback6.of_fun f)

let scm_define_gsubr_7 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback7.t @-> returning scm)
let scm_define_gsubr_7 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 7 - no_opt - rst in
  scm_define_gsubr_7 name no_required no_opt rst (GuileCallback7.of_fun f)

let scm_define_gsubr_8 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback8.t @-> returning scm)
let scm_define_gsubr_8 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 8 - no_opt - rst in
  scm_define_gsubr_8 name no_required no_opt rst (GuileCallback8.of_fun f)

let scm_define_gsubr_9 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback9.t @-> returning scm)
let scm_define_gsubr_9 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 9 - no_opt - rst in
  scm_define_gsubr_9 name no_required no_opt rst (GuileCallback9.of_fun f)

let scm_define_gsubr_10 =
  Foreign.foreign "scm_c_define_gsubr" (string @-> int @-> int @-> int @-> GuileCallback10.t @-> returning scm)
let scm_define_gsubr_10 name ?(no_opt=0) ?(rst=false) f =
  let rst = Bool.to_int rst in
  let no_required = 10 - no_opt - rst in
  scm_define_gsubr_10 name no_required no_opt rst (GuileCallback10.of_fun f)

(* ====================================================================  *)
(* Equality  *)
(* ====================================================================  *)
let scm_eq_p : scm -> scm -> scm = Foreign.foreign "scm_eq_p" (scm @-> scm @-> returning scm)
let scm_is_eq : scm -> scm -> bool = Foreign.foreign "scm_is_eq_" (scm @-> scm @-> returning bool)

let scm_eqv_p : scm -> scm -> scm = Foreign.foreign "scm_eqv_p" (scm @-> scm @-> returning scm)

let scm_equal_p : scm -> scm -> scm = Foreign.foreign "scm_equal_p" (scm @-> scm @-> returning scm)

let scm_object_to_string: scm -> scm -> scm = Foreign.foreign "scm_object_to_string" (scm @-> scm @-> returning scm)


(* ====================================================================  *)
(* Evaluation  *)
(* ====================================================================  *)

let scm_eval: scm -> scm -> scm = Foreign.foreign "scm_eval" (scm @-> scm @-> returning scm)

let scm_interaction_environment: unit -> scm = Foreign.foreign "scm_interaction_environment" (void @-> returning scm)

let scm_eval_string : scm -> scm = Foreign.foreign "scm_eval_string" (scm @-> returning scm)

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

let scm_to_char : scm -> char = Foreign.foreign "scm_to_char_" (scm @-> returning char)
let scm_to_schar : scm -> int = Foreign.foreign "scm_to_schar_" (scm @-> returning schar)
let scm_to_uchar : scm -> Unsigned.UChar.t = Foreign.foreign "scm_to_uchar_" (scm @-> returning uchar)
let scm_to_short : scm -> int = Foreign.foreign "scm_to_short_" (scm @-> returning short)
let scm_to_ushort : scm -> Unsigned.UShort.t = Foreign.foreign "scm_to_ushort_" (scm @-> returning ushort)
let scm_to_int : scm -> int = Foreign.foreign "scm_to_int_" (scm @-> returning int)
let scm_to_uint : scm -> Unsigned.UInt.t = Foreign.foreign "scm_to_uint_" (scm @-> returning uint)
let scm_to_long : scm -> Signed.Long.t = Foreign.foreign "scm_to_long_" (scm @-> returning long)
let scm_to_ulong : scm -> Unsigned.ULong.t = Foreign.foreign "scm_to_ulong_" (scm @-> returning ulong)
let scm_to_long_long : scm -> Signed.LLong.t = Foreign.foreign "scm_to_long_long_" (scm @-> returning llong)
let scm_to_ulong_long : scm -> Unsigned.ULLong.t = Foreign.foreign "scm_to_ulong_long_" (scm @-> returning ullong)
let scm_to_size_t : scm -> Unsigned.Size_t.t = Foreign.foreign "scm_to_size_t_" (scm @-> returning size_t)

let scm_from_char : char -> scm = Foreign.foreign "scm_from_char_" ( char @-> returning scm)
let scm_from_schar : int -> scm = Foreign.foreign "scm_from_schar_" ( schar @-> returning scm)
let scm_from_uchar : Unsigned.UChar.t -> scm = Foreign.foreign "scm_from_uchar_" ( uchar @-> returning scm)
let scm_from_short : int -> scm = Foreign.foreign "scm_from_short_" ( short @-> returning scm)
let scm_from_ushort : Unsigned.UShort.t -> scm = Foreign.foreign "scm_from_ushort_" ( ushort @-> returning scm)
let scm_from_int : int -> scm = Foreign.foreign "scm_from_int_" ( int @-> returning scm)
let scm_from_uint : Unsigned.UInt.t -> scm = Foreign.foreign "scm_from_uint_" ( uint @-> returning scm)
let scm_from_long : Signed.Long.t -> scm = Foreign.foreign "scm_from_long_" ( long @-> returning scm)
let scm_from_ulong : Unsigned.ULong.t -> scm = Foreign.foreign "scm_from_ulong_" ( ulong @-> returning scm)
let scm_from_long_long : Signed.LLong.t -> scm = Foreign.foreign "scm_from_long_long_" ( llong @-> returning scm)
let scm_from_ulong_long : Unsigned.ULLong.t -> scm = Foreign.foreign "scm_from_ulong_long_" ( ullong @-> returning scm)
let scm_from_size_t : Unsigned.Size_t.t -> scm = Foreign.foreign "scm_from_size_t_" ( size_t @-> returning scm)

(* ====================================================================  *)
(* Real  *)
(* ====================================================================  *)

let scm_real_p : scm -> scm = Foreign.foreign "scm_real_p" (scm @-> returning scm)
let scm_is_real : scm -> bool = Foreign.foreign "scm_is_real" (scm @-> returning bool)

let scm_rational_p : scm -> scm = Foreign.foreign "scm_rational_p" (scm @-> returning scm)
let scm_is_rational : scm -> bool = Foreign.foreign "scm_is_rational" (scm @-> returning bool)

let scm_rationalize : scm -> scm -> scm = Foreign.foreign "scm_rationalize" (scm @-> scm @-> returning scm)

let scm_inf_p : scm -> scm = Foreign.foreign "scm_inf_p" (scm @-> returning scm)
let scm_nan_p : scm -> scm = Foreign.foreign "scm_nan_p" (scm @-> returning scm)
let scm_finite_p : scm -> scm = Foreign.foreign "scm_finite_p" (scm @-> returning scm)

let scm_nan : unit -> scm = Foreign.foreign "scm_nan" (void @-> returning scm)
let scm_inf : unit -> scm = Foreign.foreign "scm_inf" (void @-> returning scm)

let scm_numerator : scm -> scm = Foreign.foreign "scm_numerator" (scm @-> returning scm)
let scm_denominator : scm -> scm = Foreign.foreign "scm_denominator" (scm @-> returning scm)

let scm_to_double : scm -> float = Foreign.foreign "scm_to_double" (scm @-> returning double)
let scm_from_double : float -> scm = Foreign.foreign "scm_from_double" (double @-> returning scm)

(* ====================================================================  *)
(* Complex  *)
(* ====================================================================  *)

let scm_complex_p : scm -> scm = Foreign.foreign "scm_complex_p" (scm @-> returning scm)
let scm_is_complex : scm -> bool = Foreign.foreign "scm_is_complex" (scm @-> returning bool)

(* ====================================================================  *)
(* Exact  *)
(* ====================================================================  *)

let scm_exact_p : scm -> scm = Foreign.foreign "scm_exact_p" (scm @-> returning scm)
let scm_is_exact : scm -> bool = Foreign.foreign "scm_is_exact" (scm @-> returning bool)

let scm_inexact_p : scm -> scm = Foreign.foreign "scm_inexact_p" (scm @-> returning scm)
let scm_is_inexact : scm -> bool = Foreign.foreign "scm_is_inexact" (scm @-> returning bool)

let scm_inexact_to_exact : scm -> scm = Foreign.foreign "scm_inexact_to_exact" (scm @-> returning scm)
let scm_exact_to_inexact : scm -> scm = Foreign.foreign "scm_exact_to_inexact" (scm @-> returning scm)

let scm_odd_p : scm -> scm = Foreign.foreign "scm_odd_p" (scm @-> returning scm)
let scm_even_p : scm -> scm = Foreign.foreign "scm_even_p" (scm @-> returning scm)

let scm_quotient : scm -> scm -> scm = Foreign.foreign "scm_quotient" (scm @-> scm @-> returning scm)
let scm_remainder : scm -> scm -> scm = Foreign.foreign "scm_remainder" (scm @-> scm @-> returning scm)
let scm_modulo : scm -> scm -> scm = Foreign.foreign "scm_modulo" (scm @-> scm @-> returning scm)
let scm_gcd : scm -> scm -> scm = Foreign.foreign "scm_gcd" (scm @-> scm @-> returning scm)
let scm_lcm : scm -> scm -> scm = Foreign.foreign "scm_lcm" (scm @-> scm @-> returning scm)

let scm_modulo_expt : scm -> scm -> scm -> scm = Foreign.foreign "scm_modulo_expt" (scm @-> scm @-> scm @-> returning scm)
let scm_exact_integer_sqrt : scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_exact_integer_sqrt" (scm @-> ptr scm @-> ptr scm @-> returning void)

let scm_num_eq_p : scm -> scm -> scm = Foreign.foreign "scm_num_eq_p" (scm @-> scm @-> returning scm)
let scm_less_p : scm -> scm -> scm = Foreign.foreign "scm_less_p" (scm @-> scm @-> returning scm)
let scm_gr_p : scm -> scm -> scm = Foreign.foreign "scm_gr_p" (scm @-> scm @-> returning scm)
let scm_leq_p : scm -> scm -> scm = Foreign.foreign "scm_leq_p" (scm @-> scm @-> returning scm)
let scm_geq_p : scm -> scm -> scm = Foreign.foreign "scm_geq_p" (scm @-> scm @-> returning scm)

let scm_zero_p : scm -> scm = Foreign.foreign "scm_zero_p" (scm @-> returning scm)
let scm_positive_p : scm -> scm = Foreign.foreign "scm_positive_p" (scm @-> returning scm)
let scm_negative_p : scm -> scm = Foreign.foreign "scm_negative_p" (scm @-> returning scm)

let scm_number_to_string : scm -> scm -> scm = Foreign.foreign "scm_number_to_string" (scm @-> scm @-> returning scm)
let scm_string_to_number : scm -> scm -> scm = Foreign.foreign "scm_string_to_number" (scm @-> scm @-> returning scm)

let scm_make_rectangular : scm -> scm -> scm = Foreign.foreign "scm_make_rectangular" (scm @-> scm @-> returning scm)
let scm_make_poloar : scm -> scm -> scm = Foreign.foreign "scm_make_polar" (scm @-> scm @-> returning scm)

let scm_real_part : scm -> scm = Foreign.foreign "scm_real_part" (scm @-> returning scm)
let scm_imag_part : scm -> scm = Foreign.foreign "scm_imag_part" (scm @-> returning scm)

let scm_magnitude : scm -> scm = Foreign.foreign "scm_magnitude" (scm @-> returning scm)
let scm_angle : scm -> scm = Foreign.foreign "scm_angle" (scm @-> returning scm)

let scm_sum : scm -> scm -> scm = Foreign.foreign "scm_sum" (scm @-> scm @-> returning scm)
let scm_difference : scm -> scm -> scm = Foreign.foreign "scm_difference" (scm @-> scm @-> returning scm)
let scm_product : scm -> scm -> scm = Foreign.foreign "scm_product" (scm @-> scm @-> returning scm)
let scm_divide : scm -> scm -> scm = Foreign.foreign "scm_divide" (scm @-> scm @-> returning scm)
let scm_oneplus : scm -> scm -> scm = Foreign.foreign "scm_oneplus" (scm @-> scm @-> returning scm)
let scm_oneminus : scm -> scm -> scm = Foreign.foreign "scm_oneminus" (scm @-> scm @-> returning scm)
let scm_abs : scm -> scm -> scm = Foreign.foreign "scm_abs" (scm @-> scm @-> returning scm)
let scm_max : scm -> scm -> scm = Foreign.foreign "scm_max" (scm @-> scm @-> returning scm)
let scm_min : scm -> scm -> scm = Foreign.foreign "scm_min" (scm @-> scm @-> returning scm)
let scm_truncate : scm -> scm -> scm = Foreign.foreign "scm_truncate_number" (scm @-> scm @-> returning scm)
let scm_round : scm -> scm -> scm = Foreign.foreign "scm_round_number" (scm @-> scm @-> returning scm)
let scm_floor : scm -> scm -> scm = Foreign.foreign "scm_floor" (scm @-> scm @-> returning scm)
let scm_ceiling : scm -> scm -> scm = Foreign.foreign "scm_ceiling" (scm @-> scm @-> returning scm)

let scm_euclidean_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_euclidean_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_euclidean_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_euclidean_quotient" (scm @-> scm @-> returning scm)
let scm_euclidean_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_euclidean_remainder" (scm @-> scm @-> returning scm)

let scm_floor_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_floor_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_floor_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_floor_quotient" (scm @-> scm @-> returning scm)
let scm_floor_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_floor_remainder" (scm @-> scm @-> returning scm)

let scm_ceiling_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_ceiling_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_ceiling_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_ceiling_quotient" (scm @-> scm @-> returning scm)
let scm_ceiling_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_ceiling_remainder" (scm @-> scm @-> returning scm)

let scm_truncate_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_truncate_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_truncate_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_truncate_quotient" (scm @-> scm @-> returning scm)
let scm_truncate_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_truncate_remainder" (scm @-> scm @-> returning scm)

let scm_centered_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_centered_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_centered_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_centered_quotient" (scm @-> scm @-> returning scm)
let scm_centered_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_centered_remainder" (scm @-> scm @-> returning scm)

let scm_round_divide : scm -> scm -> scm ptr -> scm ptr -> unit =
  Foreign.foreign "scm_round_divide" (scm @-> scm @-> ptr scm @-> ptr scm @-> returning void)
let scm_round_quotient : scm -> scm -> scm =
  Foreign.foreign "scm_round_quotient" (scm @-> scm @-> returning scm)
let scm_round_remainder : scm -> scm -> scm =
  Foreign.foreign "scm_round_remainder" (scm @-> scm @-> returning scm)

let scm_logand : scm -> scm -> scm = Foreign.foreign "scm_logand" (scm @-> scm @-> returning scm)
let scm_logior : scm -> scm -> scm = Foreign.foreign "scm_logior" (scm @-> scm @-> returning scm)
let scm_logxor : scm -> scm -> scm = Foreign.foreign "scm_logxor" (scm @-> scm @-> returning scm)
let scm_lognot : scm -> scm = Foreign.foreign "scm_lognot" (scm @-> returning scm)
let scm_logtest : scm -> scm -> scm = Foreign.foreign "scm_logtest" (scm @-> scm @-> returning scm)
let scm_logbit_p : scm -> scm -> scm = Foreign.foreign "scm_logbit_p" (scm @-> scm @-> returning scm)
let scm_ash : scm -> scm -> scm = Foreign.foreign "scm_ash" (scm @-> scm @-> returning scm)
let scm_round_ash : scm -> scm -> scm = Foreign.foreign "scm_round_ash" (scm @-> scm @-> returning scm)
let scm_logcount : scm -> scm = Foreign.foreign "scm_logcount" (scm @-> returning scm)
let scm_integer_length : scm -> scm = Foreign.foreign "scm_integer_length" (scm @-> returning scm)
let scm_integer_expt : scm -> scm -> scm = Foreign.foreign "scm_integer_expt" (scm @-> scm @-> returning scm)
let scm_bit_extract : scm -> scm -> scm -> scm = Foreign.foreign "scm_bit_extract" (scm @-> scm @-> scm @-> returning scm)

let scm_copy_random_state : scm -> scm = Foreign.foreign "scm_copy_random_state" (scm @-> returning scm)
let scm_random : scm -> scm -> scm = Foreign.foreign "scm_random" (scm @-> scm @-> returning scm)
let scm_random_exp : scm -> scm = Foreign.foreign "scm_random_exp" (scm @-> returning scm)
let scm_random_hollow_sphere_x : scm -> scm -> scm = Foreign.foreign "scm_random_hollow_sphere_x" (scm @-> scm @-> returning scm)
let scm_random_normal : scm -> scm = Foreign.foreign "scm_random_normal" (scm @-> returning scm)
let scm_random_normal_vector_x : scm -> scm -> scm = Foreign.foreign "scm_random_normal_vector_x" (scm @-> scm @-> returning scm)
let scm_random_solid_sphere_x : scm -> scm -> scm = Foreign.foreign "scm_random_solid_sphere_x" (scm @-> scm @-> returning scm)
let scm_random_uniform : scm -> scm = Foreign.foreign "scm_random_uniform" (scm @-> returning scm)
let scm_seed_to_random_state : scm -> scm = Foreign.foreign "scm_seed_to_random_state" (scm @-> returning scm)
let scm_datum_to_random_state : scm -> scm = Foreign.foreign "scm_datum_to_random_state" (scm @-> returning scm)
let scm_random_state_to_datum : scm -> scm = Foreign.foreign "scm_random_state_to_datum" (scm @-> returning scm)
let scm_random_state_from_platform : scm -> scm = Foreign.foreign "scm_random_state_from_platform" (scm @-> returning scm)

let scm_char_p : scm -> scm = Foreign.foreign "scm_char_p" (scm @-> returning scm)
let scm_char_alphabetic_p : scm -> scm = Foreign.foreign "scm_char_alphabetic_p" (scm @-> returning scm)
let scm_char_numeric_p : scm -> scm = Foreign.foreign "scm_char_numeric_p" (scm @-> returning scm)
let scm_char_whitespace_p : scm -> scm = Foreign.foreign "scm_char_whitespace_p" (scm @-> returning scm)
let scm_char_upper_case_p : scm -> scm = Foreign.foreign "scm_char_upper_case_p" (scm @-> returning scm)
let scm_char_lower_case_p : scm -> scm = Foreign.foreign "scm_char_lower_case_p" (scm @-> returning scm)
let scm_char_is_both_p : scm -> scm = Foreign.foreign "scm_char_is_both_p" (scm @-> returning scm)
let scm_char_general_category : scm -> scm = Foreign.foreign "scm_char_general_category" (scm @-> returning scm)
let scm_char_to_integer : scm -> scm = Foreign.foreign "scm_char_to_integer" (scm @-> returning scm)
let scm_integer_to_char : scm -> scm = Foreign.foreign "scm_integer_to_char" (scm @-> returning scm)
let scm_char_upcase : scm -> scm = Foreign.foreign "scm_char_upcase" (scm @-> returning scm)
let scm_char_downcase : scm -> scm = Foreign.foreign "scm_char_downcase" (scm @-> returning scm)
let scm_char_titlecase : scm -> scm = Foreign.foreign "scm_char_titlecase" (scm @-> returning scm)


(* ====================================================================  *)
(* String  *)
(* ====================================================================  *)
let scm_string_p : scm -> scm = Foreign.foreign "scm_string_p" (scm @-> returning scm)
let scm_is_string : scm -> bool = Foreign.foreign "scm_is_string" (scm @-> returning bool)

let scm_string_null_p : scm -> scm = Foreign.foreign "scm_string_null_p" (scm @-> returning scm)

let scm_string : scm -> scm = Foreign.foreign "scm_string" (scm @-> returning scm)

let scm_reverse_list_to_string : scm -> scm = Foreign.foreign "scm_reverse_list_to_string" (scm @-> returning scm)

let scm_make_string : scm -> scm -> scm = Foreign.foreign "scm_make_string" (scm @-> scm @-> returning scm)

let scm_string_join : scm -> scm -> scm -> scm = Foreign.foreign "scm_string_join" (scm @-> scm @-> scm @-> returning scm)

let scm_substring_to_list : scm -> scm -> scm -> scm = Foreign.foreign "scm_substring_to_list" (scm @-> scm @-> scm @-> returning scm)

let scm_string_to_list : scm -> scm = Foreign.foreign "scm_string_to_list" (scm @-> returning scm)

let scm_string_split : scm -> scm -> scm = Foreign.foreign "scm_string_split" (scm @-> scm @-> returning scm)

let scm_string_length : scm -> scm = Foreign.foreign "scm_string_length" (scm @-> returning scm)
let scm_c_string_length : scm -> Unsigned.size_t = Foreign.foreign "scm_c_string_length" (scm @-> returning size_t)

let scm_substring_copy : scm -> scm -> scm -> scm = Foreign.foreign "scm_substring_copy" (scm @-> scm @-> scm @-> returning scm)

let scm_string_copy : scm -> scm = Foreign.foreign "scm_string_copy" (scm @-> returning scm)

let scm_substring : scm -> scm -> scm -> scm = Foreign.foreign "scm_string_copy" (scm @-> scm @-> scm @-> returning scm)

let scm_string_eq : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_eq" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_neq : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_neq" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_lt : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_lt" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_gt : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_gt" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_le : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_le" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ge : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ge" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_eq : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_eq" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_neq : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_neq" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_lt : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_lt" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_gt : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_gt" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_ge : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_ge" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_string_ci_le : scm -> scm -> scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_string_ci_le" (scm @-> scm  @-> scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_substring_hash : scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_substring_hash" (scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_substring_hash_ci : scm -> scm -> scm -> scm -> scm =
  Foreign.foreign "scm_substring_hash_ci" (scm  @-> scm  @-> scm  @-> scm  @-> returning scm)

let scm_from_locale_string : string -> scm =
  Foreign.foreign "scm_from_locale_string" (string @-> returning scm)

let scm_to_locale_stringbuf :
  scm -> char Ctypes_static.ptr -> Unsigned.size_t -> Unsigned.size_t =
  Foreign.foreign "scm_to_locale_stringbuf" (scm @-> ptr char @-> size_t @-> returning size_t)

(* ====================================================================  *)
(* Symbol  *)
(* ====================================================================  *)

let scm_symbol_p : scm -> scm = Foreign.foreign "scm_symbol_p" (scm @-> returning scm)

let scm_symbol_to_string : scm -> scm = Foreign.foreign "scm_symbol_to_string" (scm @-> returning scm)
let scm_string_to_symbol : scm -> scm = Foreign.foreign "scm_string_to_symbol" (scm @-> returning scm)

let scm_string_from_latin1_symbol : string -> scm = Foreign.foreign "scm_from_latin1_symbol" (string @-> returning scm)
let scm_string_from_utf8_symbol : string -> scm = Foreign.foreign "scm_from_utf8_symbol" (string @-> returning scm)

let scm_gensym: scm -> scm = Foreign.foreign "scm_gensym" (scm @-> returning scm)

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

(* ====================================================================  *)
(* List  *)
(* ====================================================================  *)

let scm_list_p : scm -> scm = Foreign.foreign "scm_list_p" (scm @-> returning scm)

let scm_null_p : scm -> scm = Foreign.foreign "scm_null_p" (scm @-> returning scm)

let scm_is_null : scm -> bool = Foreign.foreign "scm_is_null_" (scm @-> returning bool)

let scm_list_1 : scm -> scm = Foreign.foreign "scm_list_1" (scm @-> returning scm)
let scm_list_2 : scm -> scm -> scm = Foreign.foreign "scm_list_2" (scm @-> scm @-> returning scm)
let scm_list_3 : scm -> scm -> scm -> scm = Foreign.foreign "scm_list_3" (scm @-> scm @-> scm @-> returning scm)
let scm_list_4 : scm -> scm -> scm -> scm -> scm = Foreign.foreign "scm_list_4" (scm @-> scm @-> scm @-> scm @-> returning scm)
let scm_list_5 : scm -> scm -> scm -> scm -> scm -> scm = Foreign.foreign "scm_list_5" (scm @-> scm @-> scm @-> scm @-> scm @-> returning scm)
