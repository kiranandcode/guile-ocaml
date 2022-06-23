type scm
(** opaque type representing Guile scheme values.  *)

val init_with : (unit -> unit) -> unit
(** [init_with f] calls [f] within a fresh Guile context. *)

val with_continuation_barrier : (unit -> unit) -> unit
(** [with_continuation_barrier f] runs the function [f] preventing any
   non-local control flow beyond the current calling context. *)

val init : unit -> unit
(** [init ()] initialises the Guile context for the current thread of
   execution. *)

val shell : unit -> unit
(** [shell ()] starts execution of a Guile repl.

    Note: assumes [Guile.init] has been called.  *)

val load: string -> scm
(** [load filename] loads the file at [filename] and evaluates it as a Guile scheme object. *)

val eol : scm
(** [eol] represents an empty list in Guile. *)

val undefined : scm
(** [undefined] represents a nullary value in Guile, can be passed in as none values to functions with optional arguments. *)

val ( = ) : scm -> scm -> bool
(** [(=) x y] tests for equality between two Guile entities. *)

module Bool : sig

  val t : scm
  (** [t] is the Guile value encoding true. *)

  val f : scm
  (** [t] is the Guile value encoding false. *)

  val boolean_p : scm -> scm
  (** [boolean_p b] returns #t if [b] is a boolean and #f otherwise. *)

  val is_bool : 'a -> scm -> bool
  (** [is_bool b] returns true if [b] is a boolean and false otherwise. *)

  val not : scm -> scm
  (** [not b] negates the boolean [b]. *)

  val to_raw : bool -> scm
  (** [to_raw b] converts the OCaml boolean [b] to a Guile boolean. *)

  val from_raw : scm -> bool
  (** [from_raw b] converts the Guile boolean [b] to an OCaml boolean. *)

end

module Number : sig

  val number_p : scm -> scm
  (** [number_p v] returns #t if [v] is a number and #f otherwise. *)

  val is_number : scm -> bool
  (** [is_number v] returns true if [v] is a number and false otherwise. *)

  val integer_p : scm -> scm
  (** [integer_p v] returns #t if [v] is an integer and #f otherwise. *)

  val is_integer : scm -> bool
  (** [is_integer v] returns true if [v] is an integer and false otherwise. *)

  val exact_integer_p : scm -> scm
  (** [exact_integer_p v] returns #t if [v] is an exact integer and #f otherwise. *)

  val is_exact_integer : scm -> bool
  (** [is_exact_integer v] returns true if [v] is an exact integer and false otherwise. *)

  val char_from_raw : scm -> char
  (** [char_from_raw v] extracts an OCaml char from a Guile value [v]. *)

  val schar_from_raw : scm -> int
  (** [schar_from_raw v] extracts an OCaml signed char from a Guile value [v]. *)

  val uchar_from_raw : scm -> Unsigned.uchar
  (** [uchar_from_raw v] extracts an OCaml unsigned char from a Guile value [v]. *)

  val short_from_raw : scm -> int
  (** [short_from_raw v] extracts an OCaml short from a Guile value [v]. *)

  val ushort_from_raw : scm -> Unsigned.ushort
  (** [ushort_from_raw v] extracts an OCaml unsigned short from a Guile value [v]. *)

  val int_from_raw : scm -> int
  (** [int_from_raw v] extracts an OCaml int from a Guile value [v]. *)

  val uint_from_raw : scm -> Unsigned.uint
  (** [uint_from_raw v] extracts an OCaml unsigned int from a Guile value [v]. *)

  val long_from_raw : scm -> Signed.long
  (** [long_from_raw v] extracts an OCaml long from a Guile value [v]. *)

  val ulong_from_raw : scm -> Unsigned.ulong
  (** [long_from_raw v] extracts an OCaml unsigned long from a Guile value [v]. *)

  val long_long_from_raw : scm -> Signed.llong
  (** [long_long_from_raw v] extracts an OCaml long long from a Guile value [v]. *)

  val ulong_long_from_raw : scm -> Unsigned.ullong
  (** [ulong_long_from_raw v] extracts an OCaml unsigned long long from a Guile value [v]. *)

  val size_t_from_raw : scm -> Unsigned.size_t
  (** [size_t_from_raw v] extracts an OCaml size_t from a Guile value [v]. *)

  val char_to_raw : char -> scm
  (** [char_to_raw c] converts an OCaml char [c] into a Guile value. *)

  val schar_to_raw : int -> scm
  (** [schar_to_raw c] converts an OCaml signed char [c] into a Guile value. *)

  val uchar_to_raw : Unsigned.uchar -> scm
  (** [uchar_to_raw c] converts an OCaml unsigned char [c] into a Guile value. *)

  val short_to_raw : int -> scm
  (** [short_to_raw c] converts an OCaml short [c] into a Guile value. *)

  val ushort_to_raw : Unsigned.ushort -> scm
  (** [ushort_to_raw c] converts an OCaml unsigned short [c] into a Guile value. *)

  val int_to_raw : int -> scm
  (** [int_to_raw i] converts an OCaml int [i] into a Guile value. *)

  val uint_to_raw : Unsigned.uint -> scm
  (** [uint_to_raw i] converts an OCaml unsigned int [i] into a Guile value. *)

  val long_to_raw : Signed.long -> scm
  (** [long_to_raw l] converts an OCaml long [l] into a Guile value. *)

  val ulong_to_raw : Unsigned.ulong -> scm
  (** [ulong_to_raw l] converts an OCaml unsigned long [l] into a Guile value. *)

  val long_long_to_raw : Signed.llong -> scm
  (** [long_long_to_raw l] converts an OCaml long long [l] into a Guile value. *)

  val ulong_long_to_raw : Unsigned.ullong -> scm
  (** [ulong_long_to_raw l] converts an OCaml unsigned long long [l] into a Guile value. *)

  val size_t_to_raw : Unsigned.size_t -> scm
  (** [size_t_to_raw l] converts an OCaml size_t [l] into a Guile value. *)

  module Float : sig

    val real_p : scm -> scm
    (** [real_p v] returns #t if [v] is a real value and #f otherwise. *)

    val is_real : scm -> bool
    (** [is_real v] returns true if [v] is a real value and false otherwise. *)

    val rationalp : scm -> scm
    (** [rational_p v] returns #t if [v] is a rational value and #f otherwise. *)

    val is_rational : scm -> bool
    (** [is_rational v] returns true if [v] is a rational value and false otherwise. *)

    val rationalize : scm -> scm -> scm

    val inf_p : scm -> scm
    (** [inf_p v] returns #t if [v] is a inf value and #f otherwise. *)

    val nan_p : scm -> scm
    (** [nan_p v] returns #t if [v] is a nan value and #f otherwise. *)

    val finite_p : scm -> scm
    (** [finite_p v] returns #t if [v] is a finite value and #f otherwise. *)

    val nan : unit -> scm
    (** [nan ()] returns the Guile value representing nan. *)

    val inf : unit -> scm
    (** [inf ()] returns the Guile value representing inf. *)

    val numerator : scm -> scm
    (** [numerator v] returns the numerator of a rational value [v]. *)

    val denominator : scm -> scm
    (** [denominator v] returns the denominator of a rational value [v]. *)

    val from_raw : scm -> float
    (** [from_raw v] extracts an OCaml float from a Guile value [v]. *)

    val to_raw : float -> scm
    (** [to_raw f] converts an OCaml float [f] into a Guile value. *)

  end

  module Complex : sig

    val complex_p : scm -> scm
    (** [complex_p v] returns #t if [v] is a complex number and #f otherwise. *)

    val is_complex : scm -> bool
    (** [is_complex v] returns true if [v] is a complex number and false otherwise. *)

  end

  val exact_p : scm -> scm
  (** [exact_p v] returns #t if [v] is an exact number and #f otherwise. *)

  val is_exact : scm -> bool
  (** [is_exact v] returns true if [v] is an exact number and false otherwise. *)

  val inexact_p : scm -> scm
  (** [inexact_p v] returns #t if [v] is an exact number and #f otherwise. *)

  val is_inexact : scm -> bool
  (** [is_inexact v] returns true if [v] is an exact number and false otherwise. *)

  val inexact_to_exact : scm -> scm
  (** [inexact_to_exact v] converts an inexact value [v] to its nearest exact counterpart. *)

  val exact_to_inexact : scm -> scm
  (** [exact_to_inexact v] converts an exact value [v] to an inexact representation. *)

end

module Pair : sig

  val cons : scm -> scm -> scm
  (** [cons hd tl] returns a cons cell with head [hd] and tail [tl]. *)

  val car : scm -> scm
  (** [car cell] returns the head of the cons cell [cell]. *)

  val cdr : scm -> scm
  (** [cdr cell] returns the tail of the cons cell [cell]. *)

  val caar : scm -> scm

  val cadr : scm -> scm

  val cdar : scm -> scm

  val hd : scm -> scm
  (** [hd cell] returns the head of the cons cell [cell]. *)

  val tl : scm -> scm
  (** [tl cell] returns the tail of the cons cell [cell]. *)

  val set_car : scm -> scm -> unit
  (** [set_car cell vl] updates the car of cell [cell] with value [vl]. *)

  val set_cdr : scm -> scm -> unit
  (** [set_cdr cell vl] updates the cdr of cell [cell] with value [vl]. *)

  val is_cons : scm -> bool
  (** [is_cons cell] returns true if [cell] is a cons cell and facelle otherwise. *)

  val is_ncons : scm -> bool
  (** [is_cons cell] returns false if [cell] is a cons cell and true otherwise. *)

end

module List : sig

  val is_null : scm -> bool
  (** [is_null ls] returns true if [ls] is an empty list. *)

  val of_raw : (scm -> 'a) -> scm -> 'a list
  (** [of_raw f ls] extracts a list from a Guile list [ls] using [f] to extract individual elements. *)

  val to_raw : ('a -> scm) -> 'a list -> scm
  (** [to_raw f ls] converts an OCaml list [ls] to a Guile list using [f] to encode individual elements. *)

end

module Char : sig

  val char_p : scm -> scm
  (** [char_p v] returns #t if [v] is a char and #f otherwise. *)

  val is_char : scm -> bool
  (** [is_char v] returns true if [v] is a char and false otherwise. *)

  val alphabetic_p : scm -> scm
  (** [alphabetic_p v] returns #t if [v] is a char and #f otherwise. *)

  val is_alphabetic : scm -> bool
  (** [is_alphabetic v] returns true if [v] is a char and false otherwise. *)

  val numeric_p : scm -> scm
  (** [numeric_p v] returns #t if [v] is a number and #f otherwise. *)

  val is_numeric : scm -> bool
  (** [is_numeric v] returns true if [v] is a number and false otherwise. *)

  val whitespace_p : scm -> scm
  (** [whitespace_p v] returns #t if [v] is whitespace and #f otherwise. *)

  val is_whitespace : scm -> bool
  (** [is_whitespace v] returns true if [v] is whitespace and false otherwise. *)

  val upper_case_p : scm -> scm
  (** [upper_case_p v] returns #t if [v] is upper case and #f otherwise. *)

  val is_upper_case : scm -> bool
  (** [is_upper_case v] returns true if [v] is upper case and false otherwise. *)

  val lower_case_p : scm -> scm
  (** [lower_case_p v] returns #t if [v] is lower case and #f otherwise. *)

  val is_lower_case : scm -> bool
  (** [is_lower_case v] returns true if [v] is lower case and false otherwise. *)

  val is_both_p : scm -> scm
  (** [is_both_p v] returns #t if [v] is either lower or upper case and #f otherwise. *)

  val is_both : scm -> bool
  (** [is_both v] returns true if [v] is either lower or upper case and false otherwise. *)

  val general_category_p : scm -> scm
  (** [general_category_p v] returns #t if [v] is a general category unicode char and #f otherwise. *)

  val is_general_category : scm -> bool
  (** [is_general_category v] returns true if [v] is a general category unicode char and false otherwise. *)

  val from_raw : scm -> char
  (** [from_raw c] converts a Guile scheme char [c] to an OCaml char. *)

  val to_raw : char -> scm
  (** [to_raw c] converts an OCaml char [c] to an Guile char. *)

end

module String : sig

  val string_p : scm -> scm
  (** [string_p v] returns #t if [v] is a string and #f otherwise. *)

  val is_string : scm -> bool
  (** [is_string v] returns true if [v] is a string and false otherwise. *)

  val is_empty : scm -> scm
  (** [is_empty v] returns true if [v] is an empty string and false otherwise. *)

  val string : char list -> scm
  (** [string cs] constructs a fresh Guile string from the list of characters [cs]. *)

  val len : scm -> int
  (** [len s] returns the length of the Guile string [s]. *)

  val to_raw : string -> scm
  (** [to_raw s] encodes an OCaml string [s] as a Guile string. *)

  val from_raw : scm -> string
  (** [from_raw s] extracts an OCaml string from a Guile string [s]. *)

end

module Symbol : sig

  val symbol_p : scm -> scm
  (** [symbol_p v] returns #t if [v] is a symbol and #f otherwise. *)

  val is_symbol : scm -> bool
  (** [is_symbol v] returns true if [v] is a symbol and false otherwise. *)

  val to_raw : string -> scm
  (** [to_raw s] converts a string [s] into a Guile symbol. *)

  val from_raw : scm -> string
  (** [from_raw s] converts a Guile symbol [s] to an OCaml string. *)

  val gensym : string -> scm
  (** [gensym s] constructs a fresh symbol based on string [s]. *)

end

module Error : sig

  val error : ?key:scm -> ?fn_name:string -> string -> scm
  (** [error ?key ?fn_name msg] throws a Guile scheme error with tag
     [key] (defaults to the symbol ocaml-guile) with message [msg],
     originating while executing [fn_name].

      Returns a dummy Guile scheme value as it does not return. *)

  val catch : tag:scm -> (unit -> unit) -> (scm -> scm -> unit) -> unit
  (** [catch ~tag f handler] runs [f] while catching any exceptions of
     with tag [tag]. If an exception is caught, the handler [handler]
     is called as [handler key args].  *)

end

module Functions : sig

  val register_fun1 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm) -> scm
  (** [register_fun1 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun2 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm) -> scm
  (** [register_fun2 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun3 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm) -> scm
  (** [register_fun3 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun4 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun4 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun5 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun5 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun6 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun6 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun7 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun7 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun8 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun8 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun9 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun9 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

  val register_fun10 : string -> ?no_opt:int -> ?rst:bool -> (scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm -> scm) -> scm
  (** [register_fun10 fname ?no_opt ?rst f] exposes the OCaml function
     [f] to the Guile scheme context, under the name [fname].

      [no_opt] encodes the number of trailing arguments that are
     optional, and [rst] encodes whether the last argument should
     capture all extraneous arguments. *)

end

val eval : ?state:scm -> scm -> scm
(** [eval ?state s] evaluates a Guile scheme s-expression [s] in execution state [state]. *)

val eval_string : string -> scm
(** [eval_string s] evaluates a string [s] as a Guile scheme s-expression  *)

val to_string : ?printer:scm -> scm -> string
(** [to_string ?printer v] returns a string representation of a Guile scheme value [v]. *)

module Sexp : sig

  val to_raw : Sexplib.Sexp.t -> scm
  (** [to_raw s] converts an s-expression [s] to a Guile scheme value. *)

  val from_raw : scm -> Sexplib.Sexp.t
  (** [from_raw s] extracts a Guile scheme value [s] into an OCaml
     s-expression.  *)

end

module Module : sig

  val resolve : string -> scm
  (** [resolve name] finds the module named [name] and returns
     it. When it has not already been defined, try to auto-load
     it. When it can’t be found that way either, create an empty
     module. *)

  val with_current_module : modl:scm -> (unit -> unit) -> unit
  (** [with_current_module ~modl f] calls [f] and makes module [modl]
     the current module during the call. *)

  val lookup_variable : modl:string -> string -> scm
  (** [lookup_variable ~modl name] finds the variable bound to the
     symbol [name] in the public interface of the module [modl].

      [modl] should be a space separated string of module names *)

  val lookup : modl:string -> string -> scm
  (** [lookup ~modl name] finds value of the variable bound to the
     symbol [name] in the public interface of the module [modl].

      Throws a Guile exception if not found.

      [modl] should be a space separated string of module names *)

  val is_defined: ?modl:scm -> string -> bool
  (** [is_defined ~modl name] returns true if [name] is defined in the
     module [modl] or the current module when module is not specified;
     otherwise return false.  *)

  val define_module : string -> (unit -> unit) -> scm
  (** [define_module modl f] defines a new module named [modl] and
     makes it current while [f] is called. Returns the module [modl].  *)

  val define : string -> scm -> unit
  (** [define name vl] binds the symbol indicated by [name] to a
     variable in the current module and set that variable to
     [vl]. When [name] is already bound to a variable, update
     that. Else create a new variable.  *)

  val use : string -> scm
  (** [use modl] add the module [modl] to the uses list of the current
     module.  *)

  val export : string -> unit
  (** [export name] adds the bindings designated by [name] to the
     public interface of the current module.  *)

end
