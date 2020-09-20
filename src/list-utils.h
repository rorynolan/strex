#ifndef LIST_UTILS_H_
#define LIST_UTILS_H_

#include <Rinternals.h>


SEXP C_lst_elems_common_length(SEXP lst, SEXP l);
SEXP C_chr_to_dbl(SEXP x, int commas);
SEXP C_lst_chr_to_dbl(SEXP x, SEXP commas);
SEXP C_chr_vec_remove_empties(SEXP x);
SEXP C_chr_lst_nth_elems(SEXP chr_lst, SEXP n);
SEXP C_dbl_lst_nth_elems(SEXP dbl_lst, SEXP n);
SEXP C_int_mat_nth_col(SEXP int_mat, int n);
SEXP C_int_mat_nth_col_nrnc(int *int_mat_int, int nr, int nc, int n);
SEXP C_int_mat_lst_nth_cols(SEXP int_mat_lst, SEXP n);
SEXP C_int_mat_nth_row(SEXP int_mat, int n);
SEXP C_int_mat_nth_row_nrnc(int *int_mat_int, int nr, int nc, int n);
SEXP C_int_mat_lst_nth_rows(SEXP int_mat_lst, SEXP n);
SEXP C_int_lst_cbind(SEXP int_lst);
SEXP C_int_lst_rbind(SEXP int_lst);
SEXP C_int_prlst_cbind(SEXP int_prlst, int int_prlst_len);
SEXP C_int_prlst_rbind(SEXP int_prlst, int int_prlst_len);
SEXP C_int_mat_lst_cbind_nth_cols(SEXP int_mat_lst, SEXP n);
SEXP C_int_mat_lst_cbind_nth_rows(SEXP int_mat_lst, SEXP n);
SEXP C_int_mat_lst_rbind_nth_cols(SEXP int_mat_lst, SEXP n);
SEXP C_int_mat_lst_rbind_nth_rows(SEXP int_mat_lst, SEXP n);


#endif  // LIST_UTILS_H_
