#include <Rinternals.h>

SEXP C_int_mat_row_maxs(SEXP int_mat) {
  int nr = Rf_nrows(int_mat), nc = Rf_ncols(int_mat);
  int *int_mat_int = INTEGER(int_mat);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nr));
  int *out_int = INTEGER(out);
  for (int r = 0; r != nr; ++r) {
    int max = int_mat_int[r];
    for (int c = 1; c < nc; ++c) {
      if (int_mat_int[c * nr + r] > max) max = int_mat_int[c * nr + r];
    }
    out_int[r] = max;
  }
  UNPROTECT(1);
  return out;
}
