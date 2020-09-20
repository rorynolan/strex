#ifndef PAIRLIST_UTILS_H_
#define PAIRLIST_UTILS_H_

#include <Rinternals.h>

SEXP C_int_prlst_cbind(SEXP int_prlst, int int_prlst_len);
SEXP C_SXP_int_prlst_cbind(SEXP int_prlst, SEXP prlst_len);
SEXP C_int_prlst_rbind(SEXP int_prlst, int int_prlst_len);
SEXP C_SXP_int_prlst_rbind(SEXP int_prlst, SEXP prlst_len);

#endif  // PAIRLIST_UTILS_H_
