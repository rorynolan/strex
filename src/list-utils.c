#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <Rinternals.h>

#include "stringi-imports.h"


SEXP C_stringi_replace_all_coll(SEXP string, SEXP pattern, SEXP replacement) {
  static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
  if (fun == NULL) {
    fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP))
    R_GetCCallable("stringi", "C_stri_replace_all_coll");
  }
  SEXP truesxp = PROTECT(Rf_ScalarLogical(1));
  SEXP out = PROTECT(fun(string, pattern, replacement, truesxp, R_NilValue));
  UNPROTECT(2);
  return out;
}

SEXP C_lst_elems_common_length(SEXP lst, SEXP l) {
  R_xlen_t lst_len = *REAL(l);
  R_xlen_t len0 = Rf_xlength(VECTOR_ELT(lst, 0));
  SEXP out;
  for (R_xlen_t i = 1; i != lst_len; ++i) {  // Assumes lst_len >= 2
    if (Rf_xlength(VECTOR_ELT(lst, i)) != len0) {
      out = PROTECT(Rf_ScalarLogical(0));
      UNPROTECT(1);
      return out;
    }
  }
  out = PROTECT(Rf_ScalarLogical(1));
  UNPROTECT(1);
  return out;
}

SEXP C_chr_to_dbl(SEXP x, int commas) {  // this int is treated like a bool
  SEXP y = x;
  unsigned char to_unprotect = 0;
  if (commas) {
    SEXP comma = PROTECT(Rf_mkString(",")), empty = PROTECT(Rf_mkString(""));
    y = PROTECT(C_stringi_replace_all_coll(x, comma, empty));
    to_unprotect += 3;
  }
  SEXP out = PROTECT(Rf_coerceVector(y, REALSXP));
  UNPROTECT(++to_unprotect);
  return out;
}

SEXP C_lst_chr_to_dbl(SEXP x, SEXP commas) {
  R_xlen_t commas_len = Rf_xlength(commas);
  int *commas_int = INTEGER(commas);
  R_xlen_t n = Rf_xlength(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  if (commas_len == 1) {
    for (R_xlen_t i = 0; i != n; ++i) {
      SEXP out_i = PROTECT(C_chr_to_dbl(VECTOR_ELT(x, i), *commas_int));
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  } else {
    for (R_xlen_t i = 0; i != n; ++i) {
      SEXP out_i = PROTECT(C_chr_to_dbl(VECTOR_ELT(x, i), commas_int[i]));
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_chr_vec_remove_empties(SEXP x) {
  R_xlen_t n = Rf_xlength(x);
  bool *empty = (bool *) malloc(n * sizeof(bool));
  R_xlen_t out_len = 0;
  for (R_xlen_t i = 0; i != n; ++i) {
    if (strlen(CHAR(STRING_ELT(x, i)))) {
      empty[i] = false;
      ++out_len;
    } else {
      empty[i] = true;
    }
  }
  SEXP out = PROTECT(Rf_allocVector(STRSXP, out_len));
  R_xlen_t out_filled = 0, i = 0;
  while(out_filled != out_len) {
    if (!empty[i]) {
      SET_STRING_ELT(out, out_filled, STRING_ELT(x, i));
      ++out_filled;
    }
    ++i;
  }
  free(empty);
  UNPROTECT(1);
  return out;
}

SEXP C_chr_lst_remove_empties(SEXP chr_lst) {
  R_xlen_t n = Rf_xlength(chr_lst);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i != n; ++i) {
    SEXP out_i = PROTECT(C_chr_vec_remove_empties(VECTOR_ELT(chr_lst, i)));
    SET_VECTOR_ELT(out, i, out_i);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return(out);
}

SEXP C_chr_lst_nth_elems(SEXP chr_lst, SEXP n) {
  R_xlen_t cl_len = Rf_xlength(chr_lst), n_len = Rf_xlength(n);
  int *n_int = INTEGER(n);
  SEXP out;
  if (cl_len == 1) {
    out = PROTECT(Rf_allocVector(STRSXP, n_len));
    SEXP strings = VECTOR_ELT(chr_lst, 0);
    for (R_xlen_t i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += Rf_xlength(strings) + 1;
      SET_STRING_ELT(out, i,
                     (((n_i > Rf_xlength(strings)) | (n_i <= 0)) ?
                       NA_STRING : STRING_ELT(strings, n_i - 1)));
    }
  } else {
    out = PROTECT(Rf_allocVector(STRSXP, cl_len));
    if (n_len == 1) {
      for (R_xlen_t i = 0; i != cl_len; ++i) {
        SEXP strings = VECTOR_ELT(chr_lst, i);
        int n_i = n_int[0];
        if (n_i < 0) n_i += Rf_xlength(strings) + 1;
        SET_STRING_ELT(out, i,
                       (((n_i > Rf_xlength(strings)) | (n_i <= 0)) ?
                         NA_STRING : STRING_ELT(strings, n_i - 1)));
      }
    } else {
      for (R_xlen_t i = 0; i < cl_len; i++) {
        SEXP strings = VECTOR_ELT(chr_lst, i);
        int n_i = n_int[i];
        if (n_i < 0) n_i += Rf_xlength(strings) + 1;
        SET_STRING_ELT(out, i,
                       (((n_i > Rf_xlength(strings)) | (n_i <= 0)) ?
                         NA_STRING : STRING_ELT(strings, n_i - 1)));
      }
    }
  }
  UNPROTECT(1);
  return(out);
}

SEXP C_dbl_lst_nth_elems(SEXP dbl_lst, SEXP n) {
  R_xlen_t nl_len = Rf_xlength(dbl_lst), n_len = Rf_xlength(n);
  int *n_int = INTEGER(n);
  SEXP out;
  if (nl_len == 1) {
    out = PROTECT(Rf_allocVector(REALSXP, n_len));
    double *out_dbl = REAL(out);
    SEXP nums = VECTOR_ELT(dbl_lst, 0);
    double *nums_dbl = REAL(nums);
    for (R_xlen_t i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += Rf_xlength(nums) + 1;
      out_dbl[i] = (((n_i > Rf_xlength(nums)) | (n_i <= 0)) ?
                      NA_REAL : nums_dbl[n_i - 1]);
    }
  } else {
    out = PROTECT(Rf_allocVector(REALSXP, nl_len));
    double *out_dbl = REAL(out);
    if (n_len == 1) {
      for (R_xlen_t i = 0; i != nl_len; ++i) {
        SEXP nums = VECTOR_ELT(dbl_lst, i);
        double *nums_dbl = REAL(nums);
        int n_i = n_int[0];
        if (n_i < 0) n_i += Rf_xlength(nums) + 1;
        out_dbl[i] = (((n_i > Rf_xlength(nums)) | (n_i <= 0)) ?
                        NA_REAL : nums_dbl[n_i - 1]);
      }
    } else {
      for (R_xlen_t i = 0; i < nl_len; i++) {
        SEXP nums = VECTOR_ELT(dbl_lst, i);
        double *nums_dbl = REAL(nums);
        int n_i = n_int[i];
        if (n_i < 0) n_i += Rf_xlength(nums) + 1;
        out_dbl[i] = (((n_i > Rf_xlength(nums)) | (n_i <= 0)) ?
                          NA_REAL : nums_dbl[n_i - 1]);
      }
    }
  }
  UNPROTECT(1);
  return(out);
}

SEXP C_int_mat_nth_col(SEXP int_mat, int n) {  // 1-indexed
  int nr = Rf_nrows(int_mat), nc = Rf_ncols(int_mat);
  if (n < 0) n += nc;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nr));
  int *out_int = INTEGER(out);
  if ((n <= 0) | (n > nc)) {
    for (int i = 0; i != nr; ++i) {
      out_int[i] = NA_INTEGER;
    }
  } else {
    INTEGER_GET_REGION(int_mat, (n - 1) * nr, nr, out_int);
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_nth_col_nrnc(int *int_mat_int, int nr, int nc, int n) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nr));
  int *out_int = INTEGER(out);
  if ((n <= 0) | (n > nc)) {
    for (int i = 0; i != nr; ++i) {
      out_int[i] = NA_INTEGER;
    }
  } else {
    for (int i = 0; i != nr; ++i) {
      out_int[i] = int_mat_int[(n - 1) * nr + i];
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_nth_cols(SEXP int_mat_lst, SEXP n) {
  R_xlen_t im_len = Rf_xlength(int_mat_lst), n_len = Rf_xlength(n);
  int *n_int = INTEGER(n);
  SEXP out;
  if (im_len == 1) {
    out = PROTECT(Rf_allocVector(VECSXP, n_len));
    SEXP int_mat = VECTOR_ELT(int_mat_lst, 0);
    int *int_mat_int = INTEGER(int_mat);
    int nr = Rf_nrows(int_mat), nc = Rf_ncols(int_mat);
    for (int i = 0; i != n_len; ++i) {
      SET_VECTOR_ELT(out, i,
                     C_int_mat_nth_col_nrnc(int_mat_int, nr, nc, n_int[i]));
    }
  } else {
    out = PROTECT(Rf_allocVector(VECSXP, im_len));
    if (n_len == 1) {
      for (R_xlen_t i = 0; i != im_len; ++i) {
        SET_VECTOR_ELT(out, i,
                       C_int_mat_nth_col(VECTOR_ELT(int_mat_lst, i), *n_int));
      }
    } else {
      for (R_xlen_t i = 0; i != im_len; ++i) {
        SET_VECTOR_ELT(out, i,
                       C_int_mat_nth_col(VECTOR_ELT(int_mat_lst, i), n_int[i]));
      }
    }
  }
  UNPROTECT(1);
  return(out);
}

SEXP C_int_mat_nth_row(SEXP int_mat, int n) {
  int nr = Rf_nrows(int_mat), nc = Rf_ncols(int_mat);
  if (n < 0) n += nr;
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nc));
  int *out_int = INTEGER(out);
  if ((n <= 0) | (n > nr)) {
    for (int i = 0; i != nc; ++i) {
      out_int[i] = NA_INTEGER;
    }
  } else {
    int *int_mat_int = INTEGER(int_mat);
    for (int i = 0; i != nc; ++i) {
      out_int[i] = int_mat_int[n - 1 + i * nr];
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_nth_row_nrnc(int *int_mat_int, int nr, int nc, int n) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, nc));
  int *out_int = INTEGER(out);
  if ((n <= 0) | (n > nr)) {
    for (int i = 0; i != nc; ++i) {
      out_int[i] = NA_INTEGER;
    }
  } else {
    for (int i = 0; i != nc; ++i) {
      out_int[i] = int_mat_int[n - 1 + i * nr];
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_nth_rows(SEXP int_mat_lst, SEXP n) {
  R_xlen_t im_len = Rf_xlength(int_mat_lst), n_len = Rf_xlength(n);
  int *n_int = INTEGER(n);
  SEXP out;
  if (im_len == 1) {
    out = PROTECT(Rf_allocVector(VECSXP, n_len));
    SEXP int_mat = VECTOR_ELT(int_mat_lst, 0);
    int *int_mat_int = INTEGER(int_mat);
    int nr = Rf_nrows(int_mat), nc = Rf_ncols(int_mat);
    for (int i = 0; i != n_len; ++i) {
      SET_VECTOR_ELT(out, i,
                     C_int_mat_nth_row_nrnc(int_mat_int, nr, nc, n_int[i]));
    }
  } else {
    out = PROTECT(Rf_allocVector(VECSXP, im_len));
    if (n_len == 1) {
      for (R_xlen_t i = 0; i != im_len; ++i) {
        SET_VECTOR_ELT(out, i,
                       C_int_mat_nth_row(VECTOR_ELT(int_mat_lst, i), *n_int));
      }
    } else {
      for (R_xlen_t i = 0; i != im_len; ++i) {
        SET_VECTOR_ELT(out, i,
                       C_int_mat_nth_row(VECTOR_ELT(int_mat_lst, i), n_int[i]));
      }
    }
  }
  UNPROTECT(1);
  return(out);
}

SEXP C_int_lst_cbind(SEXP int_lst) {
  int nc = Rf_xlength(int_lst), nr = Rf_xlength(VECTOR_ELT(int_lst, 0));
  SEXP out = PROTECT(Rf_allocMatrix(INTSXP, nr, nc));
  int *out_int = INTEGER(out);
  for (int i = 0; i != nc; ++i) {
    INTEGER_GET_REGION(VECTOR_ELT(int_lst, i), 0, nr, out_int + i * nr);
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_lst_rbind(SEXP int_lst) {
  int nr = Rf_xlength(int_lst), nc = Rf_xlength(VECTOR_ELT(int_lst, 0));
  SEXP out = PROTECT(Rf_allocMatrix(INTSXP, nr, nc));
  int *out_int = INTEGER(out);
  for (int i = 0; i != nr; ++i) {
    int *int_lst_i_int = INTEGER(VECTOR_ELT(int_lst, i));
    for (int j = 0; j != nc; ++j) {
      out_int[j * nr + i] = int_lst_i_int[j];
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_cbind_nth_cols(SEXP int_mat_lst, SEXP n) {
  int int_mat_lst_len = Rf_xlength(int_mat_lst);
  int n_len = Rf_xlength(n), *n_int = INTEGER(n);
  SEXP int_mat_lst_0 = VECTOR_ELT(int_mat_lst, 0);
  int out_nr = Rf_nrows(int_mat_lst_0);
  SEXP out;
  if (int_mat_lst_len == 1) {
    out = PROTECT(Rf_allocMatrix(INTSXP, out_nr, n_len));
    int int_mat_lst_0_nc = Rf_ncols(int_mat_lst_0);
    int *int_mat_lst_0_int = INTEGER(int_mat_lst_0), *out_int = INTEGER(out);
    for (int i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += int_mat_lst_0_nc + 1;
      if ((n_i > int_mat_lst_0_nc) | (n_i < 1)) {
        for (int j = 0; j != out_nr; ++j) {
          out_int[i * out_nr + j] = NA_INTEGER;
        }
      } else {
        for (int j = 0; j != out_nr; ++j) {
          out_int[i * out_nr + j] = int_mat_lst_0_int[(n_i - 1) * out_nr + j];
        }
      }
    }
  } else {
    out = PROTECT(Rf_allocMatrix(INTSXP, out_nr, int_mat_lst_len));
    int *out_int = INTEGER(out);
    if (n_len == 1) {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nc = Rf_ncols(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        if (*n_int < 0) *n_int += int_mat_lst_i_nc + 1;
        if ((*n_int > int_mat_lst_i_nc) | (*n_int < 1)) {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] =
              int_mat_lst_i_int[(*n_int - 1) * out_nr + j];
          }
        }
      }
    } else {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nc = Rf_ncols(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        int n_i = n_int[i];
        if (n_i < 0) n_i += int_mat_lst_i_nc + 1;
        if ((n_i > int_mat_lst_i_nc) | (n_i < 1)) {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] = int_mat_lst_i_int[(n_i - 1) * out_nr + j];
          }
        }
      }
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_cbind_nth_rows(SEXP int_mat_lst, SEXP n) {
  int int_mat_lst_len = Rf_xlength(int_mat_lst);
  int n_len = Rf_xlength(n), *n_int = INTEGER(n);
  SEXP int_mat_lst_0 = VECTOR_ELT(int_mat_lst, 0);
  int out_nr = Rf_ncols(int_mat_lst_0);
  SEXP out;
  if (int_mat_lst_len == 1) {
    out = PROTECT(Rf_allocMatrix(INTSXP, out_nr, n_len));
    int int_mat_lst_0_nr = Rf_nrows(int_mat_lst_0);
    int *int_mat_lst_0_int = INTEGER(int_mat_lst_0), *out_int = INTEGER(out);
    for (int i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += int_mat_lst_0_nr + 1;
      if ((n_i > int_mat_lst_0_nr) | (n_i < 1)) {
        for (int j = 0; j != out_nr; ++j) {
          out_int[i * out_nr + j] = NA_INTEGER;
        }
      } else {
        for (int j = 0; j != out_nr; ++j) {
          out_int[i * out_nr + j] =
            int_mat_lst_0_int[n_i - 1 + j * int_mat_lst_0_nr];
        }
      }
    }
  } else {
    out = PROTECT(Rf_allocMatrix(INTSXP, out_nr, int_mat_lst_len));
    int *out_int = INTEGER(out);
    if (n_len == 1) {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nr = Rf_nrows(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        if (*n_int < 0) *n_int += int_mat_lst_i_nr + 1;
        if ((*n_int > int_mat_lst_i_nr) | (*n_int < 1)) {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] =
              int_mat_lst_i_int[*n_int - 1 + j * int_mat_lst_i_nr];
          }
        }
      }
    } else {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nr = Rf_nrows(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        int n_i = n_int[i];
        if (n_i < 0) n_i += int_mat_lst_i_nr + 1;
        if ((n_i > int_mat_lst_i_nr) | (n_i < 1)) {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nr; ++j) {
            out_int[i * out_nr + j] =
              int_mat_lst_i_int[n_i - 1 + j * int_mat_lst_i_nr];
          }
        }
      }
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_rbind_nth_cols(SEXP int_mat_lst, SEXP n) {
  int int_mat_lst_len = Rf_xlength(int_mat_lst);
  int n_len = Rf_xlength(n), *n_int = INTEGER(n);
  SEXP int_mat_lst_0 = VECTOR_ELT(int_mat_lst, 0);
  int out_nc = Rf_nrows(int_mat_lst_0);
  SEXP out;
  if (int_mat_lst_len == 1) {
    out = PROTECT(Rf_allocMatrix(INTSXP, n_len, out_nc));
    int int_mat_lst_0_nc = Rf_ncols(int_mat_lst_0);
    int *int_mat_lst_0_int = INTEGER(int_mat_lst_0), *out_int = INTEGER(out);
    for (int i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += int_mat_lst_0_nc + 1;
      if ((n_i > int_mat_lst_0_nc) | (n_i < 1)) {
        for (int j = 0; j != out_nc; ++j) {
          out_int[j * n_len + i] = NA_INTEGER;
        }
      } else {
        for (int j = 0; j != out_nc; ++j) {
          out_int[j * n_len + i] = int_mat_lst_0_int[(n_i - 1) * out_nc + j];
        }
      }
    }
  } else {
    out = PROTECT(Rf_allocMatrix(INTSXP, int_mat_lst_len, out_nc));
    int *out_int = INTEGER(out);
    if (n_len == 1) {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nc = Rf_ncols(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        if (*n_int < 0) *n_int += int_mat_lst_i_nc + 1;
        if ((*n_int > int_mat_lst_i_nc) | (*n_int < 1)) {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * int_mat_lst_len + i] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * int_mat_lst_len + i] =
              int_mat_lst_i_int[(*n_int - 1) * out_nc + j];
          }
        }
      }
    } else {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nc = Rf_ncols(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        int n_i = n_int[i];
        if (n_i < 0) n_i += int_mat_lst_i_nc + 1;
        if ((n_i > int_mat_lst_i_nc) | (n_i < 1)) {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * n_len + i] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * n_len + i] = int_mat_lst_i_int[(n_i - 1) * out_nc + j];
          }
        }
      }
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP C_int_mat_lst_rbind_nth_rows(SEXP int_mat_lst, SEXP n) {
  int int_mat_lst_len = Rf_xlength(int_mat_lst);
  int n_len = Rf_xlength(n), *n_int = INTEGER(n);
  SEXP int_mat_lst_0 = VECTOR_ELT(int_mat_lst, 0);
  int out_nc = Rf_ncols(int_mat_lst_0);
  SEXP out;
  if (int_mat_lst_len == 1) {
    out = PROTECT(Rf_allocMatrix(INTSXP, n_len, out_nc));
    int int_mat_lst_0_nr = Rf_nrows(int_mat_lst_0);
    int *int_mat_lst_0_int = INTEGER(int_mat_lst_0), *out_int = INTEGER(out);
    for (int i = 0; i != n_len; ++i) {
      int n_i = n_int[i];
      if (n_i < 0) n_i += int_mat_lst_0_nr + 1;
      if ((n_i > int_mat_lst_0_nr) | (n_i < 1)) {
        for (int j = 0; j != out_nc; ++j) {
          out_int[j * n_len + i] = NA_INTEGER;
        }
      } else {
        for (int j = 0; j != out_nc; ++j) {
          out_int[j * n_len + i] =
            int_mat_lst_0_int[n_i - 1 + j * int_mat_lst_0_nr];
        }
      }
    }
  } else {
    out = PROTECT(Rf_allocMatrix(INTSXP, int_mat_lst_len, out_nc));
    int *out_int = INTEGER(out);
    if (n_len == 1) {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nr = Rf_nrows(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        if (*n_int < 0) *n_int += int_mat_lst_i_nr + 1;
        if ((*n_int > int_mat_lst_i_nr) | (*n_int < 1)) {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * int_mat_lst_len + i] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * int_mat_lst_len + i] =
              int_mat_lst_i_int[*n_int - 1 + j * int_mat_lst_i_nr];
          }
        }
      }
    } else {
      for (int i = 0; i != int_mat_lst_len; ++i) {
        SEXP int_mat_lst_i = VECTOR_ELT(int_mat_lst, i);
        int int_mat_lst_i_nr = Rf_nrows(int_mat_lst_i);
        int *int_mat_lst_i_int = INTEGER(int_mat_lst_i);
        int n_i = n_int[i];
        if (n_i < 0) n_i += int_mat_lst_i_nr + 1;
        if ((n_i > int_mat_lst_i_nr) | (n_i < 1)) {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * n_len + i] = NA_INTEGER;
          }
        } else {
          for (int j = 0; j != out_nc; ++j) {
            out_int[j * n_len + i] =
              int_mat_lst_i_int[n_i - 1 + j * int_mat_lst_i_nr];
          }
        }
      }
    }
  }
  UNPROTECT(1);
  return out;
}
