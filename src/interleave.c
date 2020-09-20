#include <Rinternals.h>


//' Interleave two vectors of strings.
//'
//' Make a vector of strings where the first element is from `x`, the
//' second is from `y`, the third is from `x`, the fourth is from
//' `y`, and so on.
//'
//' `x` and `y` must be the same length or differ in length only
//' by 1. If `y` is longer, it goes first.
//'
//' @param x,y Character vectors.
//'
//' @return A character vector.
//'
//' @examples
//' interleave_chr_vecs(c("a", "c", "e"), c("b", "d"))
//'
//' @noRd
SEXP C_interleave_chr_vecs(SEXP x, SEXP y) {
  R_xlen_t x_len = Rf_xlength(x), y_len = Rf_xlength(y);
  long long length_diff = x_len - y_len;
  R_xlen_t l = x_len + y_len;
  SEXP interleaved = PROTECT(Rf_allocVector(STRSXP, l));
  R_xlen_t i = 0;
  if (length_diff >= 0) {
    while (i < l) {
      if (i % 2 == 0) {
        SET_STRING_ELT(interleaved, i, STRING_ELT(x, i / 2));
        ++i;
      } else if (i < l) {
        SET_STRING_ELT(interleaved, i, STRING_ELT(y, i / 2));
        ++i;
      }
    }
  } else {
    while (i < l) {
      if (i % 2 == 0) {
        SET_STRING_ELT(interleaved, i, STRING_ELT(y, i / 2));
        ++i;
      } else if (i < l) {
        SET_STRING_ELT(interleaved, i, STRING_ELT(x, i / 2));
        ++i;
      }
    }
  }
  UNPROTECT(1);
  return(interleaved);
}

SEXP C_interleave_chr_lsts(SEXP x, SEXP y) {
  R_xlen_t l = Rf_xlength(x);
  SEXP interleaved = PROTECT(Rf_allocVector(VECSXP, l));
  for (R_xlen_t i = 0; i != l; ++i) {
    SEXP interleaved_i = PROTECT(
      C_interleave_chr_vecs(VECTOR_ELT(x, i), VECTOR_ELT(y, i))
    );
    SET_VECTOR_ELT(interleaved, i, interleaved_i);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(interleaved);
}
