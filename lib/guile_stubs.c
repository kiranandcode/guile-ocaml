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
