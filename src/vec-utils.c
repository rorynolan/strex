#include <Rinternals.h>

SEXP C_int_vec_all_value(SEXP int_vec, SEXP int_scalar) {
  R_xlen_t n = Rf_xlength(int_vec);
  int *int_vec_int = INTEGER(int_vec);
  int *int_scalar_int = INTEGER(int_scalar);
  for (R_xlen_t i = 0; i != n; ++i) {
    if (int_vec_int[i] != *int_scalar_int) {
      SEXP out = PROTECT(Rf_ScalarLogical(0));
      UNPROTECT(1);
      return out;
    }
  }
  SEXP out = PROTECT(Rf_ScalarLogical(1));
  UNPROTECT(1);
  return out;
}
