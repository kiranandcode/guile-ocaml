/*
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
*/

#include "libguile.h"

int linkme() {
  return 42;
}

void scm_setcar(SCM pair, SCM x) {
  SCM_SETCAR(pair, x);
}

void scm_setcdr(SCM pair, SCM x) {
  SCM_SETCDR(pair, x);
}

SCM scm_from_bool_(int v) {
  return scm_from_bool(v);
}

char scm_to_char_(SCM v) {
  return scm_to_char(v);
}

signed char scm_to_schar_(SCM v) {
  return scm_to_schar(v);
}

unsigned char scm_to_uchar_(SCM v) {
  return scm_to_uchar(v);
}

short scm_to_short_(SCM v) {
  return scm_to_short(v);
}

unsigned short scm_to_ushort_(SCM v) {
  return scm_to_ushort(v);
}

int scm_to_int_(SCM v) {
  return scm_to_int(v);
}

unsigned int scm_to_uint_(SCM v) {
  return scm_to_uint(v);
}

long scm_to_long_(SCM v) {
  return scm_to_long(v);
}

unsigned long scm_to_ulong_(SCM v) {
  return scm_to_ulong(v);
}

long long scm_to_long_long_(SCM v) {
  return scm_to_long_long(v);
}

unsigned long long scm_to_ulong_long_(SCM v) {
  return scm_to_ulong_long(v);
}

size_t scm_to_size_t_(SCM v) {
  return scm_to_size_t(v);
}

SCM scm_from_char_(char v) {
  return scm_from_char(v);
}

SCM scm_from_schar_(signed char v) {
  return scm_from_schar(v);
}

SCM scm_from_uchar_(unsigned char v) {
  return scm_from_uchar(v);
}

SCM scm_from_short_(short v) {
  return scm_from_short(v);
}

SCM scm_from_ushort_(unsigned short v) {
  return scm_from_ushort(v);
}

SCM scm_from_int_(int v) {
  return scm_from_int(v);
}

SCM scm_from_uint_(unsigned int v) {
  return scm_from_uint(v);
}

SCM scm_from_long_(long v) {
  return scm_from_long(v);
}

SCM scm_from_ulong_(unsigned long v) {
  return scm_from_ulong(v);
}

SCM scm_from_long_long_(long long v) {
  return scm_from_long_long(v);
}

SCM scm_from_ulong_long_(unsigned long long v) {
  return scm_from_ulong_long(v);
}

SCM scm_from_size_t_(size_t v) {
  return scm_from_size_t(v);
}

int scm_is_eq_(SCM x, SCM y) {
  return scm_is_eq(x,y);
}

int scm_is_null_(SCM x) {
  return scm_is_null(x);
}

