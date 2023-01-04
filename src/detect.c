#include "stringi-imports.h"

SEXP C_stringi_detect_coll(SEXP string, SEXP pattern) {
  static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP))
    R_GetCCallable("stringi", "C_stri_detect_coll");
  }
  SEXP falsesxp = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP minusonesxp = PROTECT(ScalarInteger(-1));
  SEXP out = PROTECT(
    fun(string, pattern, falsesxp, minusonesxp, R_NilValue)
  );
  UNPROTECT(3);
  return out;
}

SEXP C_stringi_detect_fixed(SEXP string, SEXP pattern) {
  static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP))
    R_GetCCallable("stringi", "C_stri_detect_fixed");
  }
  SEXP falsesxp = PROTECT(Rf_ScalarLogical(FALSE));
  SEXP minusonesxp = PROTECT(ScalarInteger(-1));
  SEXP out = PROTECT(
    fun(string, pattern, falsesxp, minusonesxp, R_NilValue)
  );
  UNPROTECT(3);
  return out;
}

SEXP C_str_detect_many_coll(SEXP string, SEXP pattern) {
  R_xlen_t pl = Rf_xlength(pattern);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, pl));
  for (R_xlen_t i = 0; i != pl; ++i) {
    SEXP pattern_i = PROTECT(ScalarString(STRING_ELT(pattern, i)));
    SEXP out_i = PROTECT(C_stringi_detect_coll(string, pattern_i));
    SET_VECTOR_ELT(out, i, out_i);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return out;
}

SEXP C_str_detect_many_fixed(SEXP string, SEXP pattern) {
  R_xlen_t pl = Rf_xlength(pattern);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, pl));
  for (R_xlen_t i = 0; i != pl; ++i) {
    SEXP pattern_i = PROTECT(ScalarString(STRING_ELT(pattern, i)));
    SEXP out_i = PROTECT(C_stringi_detect_fixed(string, pattern_i));
    SET_VECTOR_ELT(out, i, out_i);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return out;
}
