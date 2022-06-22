(*
gnu Guile OCaml Bindings

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
[@@@warning "-50"]

module Stubs = functor (T: Cstubs_structs.TYPE) -> struct

  let scml_bool_f = T.constant "SCM_BOOL_F" T.(intptr_t)
  let scml_bool_t = T.constant "SCM_BOOL_T" T.(intptr_t)
  let scm_eol = T.constant "SCM_EOL" T.(intptr_t)

end

