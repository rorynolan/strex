#include <Rinternals.h>

SEXP C_int_prlst_cbind(SEXP int_prlst, int int_prlst_len) {
  int nr = Rf_xlength(CAR(int_prlst));
  SEXP out = PROTECT(Rf_allocMatrix(INTSXP, nr, int_prlst_len));
  int *out_int = INTEGER(out);
  for (int i = 0; i != int_prlst_len; ++i) {
    SEXP int_prlst_i_car = CAR(int_prlst);
    INTEGER_GET_REGION(int_prlst_i_car, 0, nr, out_int + i * nr);
    int_prlst = CDR(int_prlst);
  }
  UNPROTECT(1);
  return out;
}
SEXP C_SXP_int_prlst_cbind(SEXP int_prlst, SEXP prlst_len) {
  int int_prlst_len = *INTEGER(prlst_len);
  return C_int_prlst_cbind(int_prlst, int_prlst_len);
}

SEXP C_int_prlst_rbind(SEXP int_prlst, int int_prlst_len) {
  SEXP int_prlst_tail = int_prlst;
  int nc = Rf_xlength(CAR(int_prlst));
  SEXP out = PROTECT(Rf_allocMatrix(INTSXP, int_prlst_len, nc));
  int *out_int = INTEGER(out);
  for (int i = 0; i != int_prlst_len; ++i) {
    SEXP int_prlst_i_car = CAR(int_prlst_tail);
    int *int_prlst_i_car_int = INTEGER(int_prlst_i_car);
    for (int j = 0; j != nc; ++j) {
      out_int[j * int_prlst_len + i] = int_prlst_i_car_int[j];
    }
    int_prlst_tail = CDR(int_prlst_tail);
  }
  UNPROTECT(1);
  return out;
}
SEXP C_SXP_int_prlst_rbind(SEXP int_prlst, SEXP prlst_len) {
  int int_prlst_len = *INTEGER(prlst_len);
  return C_int_prlst_rbind(int_prlst, int_prlst_len);
}
