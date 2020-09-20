#ifndef STRINGI_IMPORTS_H_
#define STRINGI_IMPORTS_H_

#include <Rinternals.h>

SEXP C_stringi_replace_all(SEXP str, SEXP pattern, SEXP replacement) {
  static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP))
    R_GetCCallable("stringi", "C_stri_replace_all_coll");
  }
  SEXP truesxp = PROTECT(Rf_ScalarLogical(1));
  SEXP out = PROTECT(fun(str, pattern, replacement, truesxp, R_NilValue));
  UNPROTECT(2);
  return out;
}


#endif  // STRINGI_IMPORTS_H_
