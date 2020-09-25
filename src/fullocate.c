#include <Rinternals.h>

#include "list-utils.h"
#include "pairlist-utils.h"

SEXP C_make_len2_int_vec(int first, int second) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, 2));
  int *out_int = INTEGER(out);
  out_int[0] = first;
  out_int[1] = second;
  UNPROTECT(1);
  return out;
}

SEXP C_fullocate(SEXP int_mat, int start, int end) {
  int nr = Rf_nrows(int_mat), *int_mat_int = INTEGER(int_mat);
  int last, row_num;  // row_num will be 1-indexed
  SEXP prlst0car;
  if (start >= int_mat_int[0]) {
    prlst0car = PROTECT(C_int_mat_nth_row_nrnc(int_mat_int, nr, 2, 1));
    last = int_mat_int[nr];
    row_num = 2;
  } else {
    prlst0car = PROTECT(C_make_len2_int_vec(start, int_mat_int[0] - 1));
    last = int_mat_int[0] - 1;
    row_num = 1;
  }
  SEXP prlst = PROTECT(Rf_list1(prlst0car));
  SEXP prlst_tail = prlst;
  int prlst_len = 1;
  while (row_num <= nr) {
    SEXP row = PROTECT(C_int_mat_nth_row_nrnc(int_mat_int, nr, 2, row_num));
    int *row_int = INTEGER(row);
    if (row_int[0] == last + 1) {
      SEXP next = PROTECT(Rf_list1(row));
      prlst_tail = SETCDR(prlst_tail, next);
      last = row_int[1];
      UNPROTECT(1);
      ++row_num;
    } else {
      SEXP next_car = PROTECT(C_make_len2_int_vec(last + 1, row_int[0] - 1));
      SEXP next = PROTECT(Rf_list1(next_car));
      prlst_tail = SETCDR(prlst_tail, next);
      last = row_int[0] - 1;
      UNPROTECT(2);
    }
    UNPROTECT(1);
    ++prlst_len;
  }
  if (INTEGER(CAR(prlst_tail))[1] < end) {
    SEXP next_car = PROTECT(C_make_len2_int_vec(last + 1, end));
    SEXP next = PROTECT(Rf_list1(next_car));
    SETCDR(prlst_tail, next);
    UNPROTECT(2);
    ++prlst_len;
  }
  SEXP out = PROTECT(C_int_prlst_rbind(prlst, prlst_len));
  UNPROTECT(3);
  return out;
}

SEXP C_lst_fullocate(SEXP int_mat_lst, SEXP start, SEXP end) {
  int *start_int = INTEGER(start), *end_int = INTEGER(end);
  R_xlen_t int_mat_lst_len = Rf_xlength(int_mat_lst);
  R_xlen_t start_len = Rf_xlength(start), end_len = Rf_xlength(end);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, int_mat_lst_len));
  if (start_len == 1 && end_len == 1) {
    for (R_xlen_t i = 0; i != int_mat_lst_len; ++i) {
      SEXP out_i = PROTECT(
        C_fullocate(VECTOR_ELT(int_mat_lst, i), *start_int, *end_int)
      );
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  } else if (start_len == 1 && end_len != 1) {
    for (R_xlen_t i = 0; i != int_mat_lst_len; ++i) {
      SEXP out_i = PROTECT(
        C_fullocate(VECTOR_ELT(int_mat_lst, i), *start_int, end_int[i])
      );
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  } else if (start_len != 1 && end_len == 1) {
    for (R_xlen_t i = 0; i != int_mat_lst_len; ++i) {
      SEXP out_i = PROTECT(
        C_fullocate(VECTOR_ELT(int_mat_lst, i), start_int[i], *end_int)
      );
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  } else {
    for (R_xlen_t i = 0; i != int_mat_lst_len; ++i) {
      SEXP out_i = PROTECT(
        C_fullocate(VECTOR_ELT(int_mat_lst, i), start_int[i], end_int[i])
      );
      SET_VECTOR_ELT(out, i, out_i);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return out;
}
