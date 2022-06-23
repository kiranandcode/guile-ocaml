(*
GNU Guile OCaml Bindings

Copyright (C) 2021  Kiran Gopinathan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type scm = Raw.scm

let init_with f =
  ignore @@ Raw.scm_with_guile (fun v -> f (); v) Ctypes.null

let with_continuation_barrier f =
  ignore @@ Raw.scm_with_continuation_barrier (fun v -> f (); v) Ctypes.null

let init () =
  Raw.scm_init_guile ()

let shell () =
  Raw.scm_shell Sys.argv

let load filename = Raw.scm_primitive_load filename

let eol: scm = Ctypes.ptr_of_raw_address (Ctypes.Intptr.to_nativeint Raw.Bindings.scm_eol)
let undefined: scm = Ctypes.ptr_of_raw_address (Ctypes.Intptr.to_nativeint Raw.Bindings.scm_undefined)

let (=) l r = Raw.scm_is_eq l r

module Bool = struct

  let t: scm = Ctypes.ptr_of_raw_address (Ctypes.Intptr.to_nativeint Raw.Bindings.scml_bool_t)
  let f: scm = Ctypes.ptr_of_raw_address (Ctypes.Intptr.to_nativeint Raw.Bindings.scml_bool_f)

  let boolean_p v = Raw.scm_boolean_p v
  let is_bool v = Raw.scm_is_bool v

  let not v = Raw.scm_not v

  let to_raw v = Raw.scm_from_bool v
  let from_raw v = Raw.scm_to_bool v

end

module Number = struct
  let number_p v = Raw.scm_number_p v
  let is_number v = Raw.scm_is_number v

  let integer_p v = Raw.scm_integer_p v
  let is_integer v = Raw.scm_is_integer v

  let exact_integer_p v = Raw.scm_exact_integer_p v
  let is_exact_integer v = Raw.scm_is_exact_integer v

  let char_from_raw v = Raw.scm_to_char v
  let schar_from_raw v = Raw.scm_to_schar v
  let uchar_from_raw v = Raw.scm_to_uchar v
  let short_from_raw v = Raw.scm_to_short v
  let ushort_from_raw v = Raw.scm_to_ushort v
  let int_from_raw v = Raw.scm_to_int v
  let uint_from_raw v = Raw.scm_to_uint v
  let long_from_raw v = Raw.scm_to_long v
  let ulong_from_raw v = Raw.scm_to_ulong v
  let long_long_from_raw v = Raw.scm_to_long_long v
  let ulong_long_from_raw v = Raw.scm_to_ulong_long v
  let size_t_from_raw v = Raw.scm_to_size_t v

  let char_to_raw v = Raw.scm_from_char v
  let schar_to_raw v = Raw.scm_from_schar v
  let uchar_to_raw v = Raw.scm_from_uchar v
  let short_to_raw v = Raw.scm_from_short v
  let ushort_to_raw v = Raw.scm_from_ushort v
  let int_to_raw v = Raw.scm_from_int v
  let uint_to_raw v = Raw.scm_from_uint v
  let long_to_raw v = Raw.scm_from_long v
  let ulong_to_raw v = Raw.scm_from_ulong v
  let long_long_to_raw v = Raw.scm_from_long_long v
  let ulong_long_to_raw v = Raw.scm_from_ulong_long v
  let size_t_to_raw v = Raw.scm_from_size_t v
  module Float = struct

    let real_p v = Raw.scm_real_p v

    let is_real v = Raw.scm_is_real v

    let rationalp v = Raw.scm_rational_p v
    let is_rational v = Raw.scm_is_rational v

    let rationalize v = Raw.scm_rationalize v

    let inf_p v = Raw.scm_inf_p v
    let nan_p v = Raw.scm_nan_p v

    let finite_p v = Raw.scm_finite_p v

    let nan v = Raw.scm_nan v
    let inf v = Raw.scm_inf v

    let numerator v = Raw.scm_numerator v
    let denominator v = Raw.scm_denominator v

    let from_raw v = Raw.scm_to_double v
    let to_raw v = Raw.scm_from_double v

  end

  module Complex = struct

    let complex_p v = Raw.scm_complex_p v

    let is_complex v = Raw.scm_is_complex v

  end

  let exact_p v = Raw.scm_exact_p v
  let is_exact v = Raw.scm_is_exact v

  let inexact_p v = Raw.scm_inexact_p v
  let is_inexact v = Raw.scm_is_inexact v

  let inexact_to_exact v = Raw.scm_inexact_to_exact v
  let exact_to_inexact v = Raw.scm_exact_to_inexact v

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

  let set_car pair vl = Raw.scm_setcar pair vl
  let set_cdr pair vl = Raw.scm_setcdr pair vl


  let is_cons x = Raw.scm_is_pair x

  let is_ncons x = not (is_cons x)

end

module List = struct

  let is_null = Raw.scm_is_null

  let of_raw f scm = 
    let rec of_list acc f scm =
      if is_null scm
      then List.rev acc
      else begin
        if not @@ Pair.is_cons scm then
          failwith "found non-list construction";
        let hd = Pair.car scm in
        let tl = Pair.cdr scm in
        of_list (f hd :: acc) f tl
      end in
    of_list [] f scm

  let rec to_raw f = function
    | [] -> eol
    | [x] -> Raw.scm_list_1 (f x)
    | [x1;x2] -> Raw.scm_list_2 (f x1) (f x2)
    | [x1;x2;x3] -> Raw.scm_list_3 (f x1) (f x2) (f x3)
    | [x1;x2;x3;x4] -> Raw.scm_list_4 (f x1) (f x2) (f x3) (f x4)
    | [x1;x2;x3;x4;x5] -> Raw.scm_list_5 (f x1) (f x2) (f x3) (f x4) (f x5)
    | hd :: tl -> Raw.scm_cons (f hd) (to_raw f tl)

end

module Char = struct

  let char_p v = Raw.scm_char_p v

  let is_char v = char_p v |> Bool.from_raw

  let alphabetic_p v = Raw.scm_char_alphabetic_p v
  let is_alphabetic v = alphabetic_p v |> Bool.from_raw

  let numeric_p v = Raw.scm_char_numeric_p v
  let is_numeric v = numeric_p v |> Bool.from_raw

  let whitespace_p v = Raw.scm_char_whitespace_p v
  let is_whitespace v = whitespace_p v |> Bool.from_raw

  let upper_case_p v = Raw.scm_char_upper_case_p v
  let is_upper_case v = upper_case_p v |> Bool.from_raw

  let lower_case_p v = Raw.scm_char_lower_case_p v
  let is_lower_case v = lower_case_p v |> Bool.from_raw

  let is_both_p v = Raw.scm_char_is_both_p v
  let is_both v = is_both_p v |> Bool.from_raw

  let general_category_p v = Raw.scm_char_general_category v
  let is_general_category v = general_category_p v |> Bool.from_raw

  let from_raw = Number.char_from_raw
  let to_raw = Number.char_to_raw

end

module String = struct

  let string_p v = Raw.scm_string_p v
  let is_string v = Raw.scm_is_string v
  let is_empty v = Raw.scm_string_null_p v

  let string ls = Raw.scm_string (List.to_raw Char.to_raw ls)

  let len s = Raw.scm_string_length s |> Number.int_from_raw

  let to_raw s = Raw.scm_from_locale_string s
  let from_raw s =
    let len = (len s) in
    let buf = Ctypes.CArray.make Ctypes.char len in
    let _ = Raw.scm_to_locale_stringbuf s (Ctypes.CArray.start buf) (Unsigned.Size_t.of_int len) in
    Ctypes.string_from_ptr (Ctypes.CArray.start buf) ~length:len

end

module Symbol = struct

  let symbol_p v = Raw.scm_symbol_p v
  let is_symbol v = symbol_p v |> Bool.from_raw

  let to_raw s = Raw.scm_string_from_utf8_symbol s
  let from_raw s = Raw.scm_symbol_to_string s |> String.from_raw

  let gensym s = Raw.scm_gensym (to_raw s)

end

module Error = struct

  let error ?key ?fn_name message =
    let key = match key with None -> Symbol.to_raw "ocaml-guile" | Some key -> key in
    Raw.scm_error key fn_name (Some message) eol Bool.f

  let catch ~tag f on_catch =
    ignore @@ Raw.scm_c_catch
      tag (fun null -> f (); null) Ctypes.null
      (fun null key args -> on_catch key args; null) Ctypes.null

end

module Functions = struct

  let safe_fun1 name f v =
    try f v with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun2 name f v1 v2 =
    try f v1 v2 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun3 name f v1 v2 v3 =
    try f v1 v2 v3 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun4 name f v1 v2 v3 v4 =
    try f v1 v2 v3 v4 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun5 name f v1 v2 v3 v4 v5 =
    try f v1 v2 v3 v4 v5 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun6 name f v1 v2 v3 v4 v5 v6 =
    try f v1 v2 v3 v4 v5 v6 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun7 name f v1 v2 v3 v4 v5 v6 v7 =
    try f v1 v2 v3 v4 v5 v6 v7 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun8 name f v1 v2 v3 v4 v5 v6 v7 v8 =
    try f v1 v2 v3 v4 v5 v6 v7 v8 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun9 name f v1 v2 v3 v4 v5 v6 v7 v8 v9 =
    try f v1 v2 v3 v4 v5 v6 v7 v8 v9 with e -> Error.error ~fn_name:name (Printexc.to_string e)
  let safe_fun10 name f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 =
    try f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 with e -> Error.error ~fn_name:name (Printexc.to_string e)

  let register_fun1 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_1 fname ?no_opt ?rst (safe_fun1 fname f)
  let register_fun2 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_2 fname ?no_opt ?rst (safe_fun2 fname f)
  let register_fun3 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm) -> scm = 
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_3 fname ?no_opt ?rst (safe_fun3 fname f)
  let register_fun4 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_4 fname ?no_opt ?rst (safe_fun4 fname f)
  let register_fun5 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_5 fname ?no_opt ?rst (safe_fun5 fname f)
  let register_fun6 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_6 fname ?no_opt ?rst (safe_fun6 fname f)
  let register_fun7 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_7 fname ?no_opt ?rst (safe_fun7 fname f)
  let register_fun8 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_8 fname ?no_opt ?rst (safe_fun8 fname f)
  let register_fun9 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_9 fname ?no_opt ?rst (safe_fun9 fname f)
  let register_fun10 : string -> ?no_opt:int -> ?rst: bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm =
    fun fname ?no_opt ?rst f -> Raw.scm_define_gsubr_10 fname ?no_opt ?rst (safe_fun10 fname f)

end

let eval ?state s =
  let state = match state with Some state -> state | None -> Raw.scm_interaction_environment () in
  Raw.scm_eval s state

let eval_string s = Raw.scm_eval_string (String.to_raw s)

let to_string ?printer v =
  let printer = Option.value ~default:undefined printer in
  Raw.scm_object_to_string v printer
  |> String.from_raw

module Sexp = struct

  let rec to_raw : Sexplib.Sexp.t -> scm =
    function
    | Atom a when Stdlib.(String.get a 0 = '"') ->
      String.to_raw Stdlib.(String.sub a 1 (String.length a - 2))
    | Atom a ->
      begin match int_of_string_opt a with
      | Some n -> Number.int_to_raw n
      | None -> match float_of_string_opt a with
          Some f -> Number.Float.to_raw f
        | None -> Symbol.to_raw a
      end
    | List elts ->
      List.to_raw to_raw elts

  let rec from_raw : scm -> Sexplib.Sexp.t = fun s ->
    if Pair.is_cons s
    then loop [] s
    else Sexplib.Sexp.Atom (to_string s)
  and loop acc s =
    if Pair.is_cons s
    then
      let hd = Pair.hd s in
      let tl = Pair.tl s in
      loop (from_raw hd :: acc) tl
    else if List.is_null s
    then Sexplib.Sexp.List (Stdlib.List.rev acc)
    else Sexplib.Sexp.List (Stdlib.List.rev (from_raw s :: acc))

end

module Module = struct

  let resolve v = Raw.scm_resolve_module v

  let with_current_module ~modl f =
    ignore @@ Raw.scm_call_with_current_module modl (fun null -> f (); null) Ctypes.null

  let lookup_variable ~modl name = Raw.scm_variable modl name

  let lookup ~modl name = Raw.scm_variable_ref modl name

  let is_defined ?modl name =
    let modl = Option.value modl ~default:undefined in
    Raw.scm_defined_p (Symbol.to_raw name) modl |> Bool.from_raw

  let define_module name f = Raw.scm_define_module name (fun null -> f (); null) Ctypes.null

  let define name vl = Raw.scm_define name vl

  let use v = Raw.scm_use_module v

  let export name  = Raw.scm_export name Ctypes.null
  
end
