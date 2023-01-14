#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP C_chr_lst_nth_elems(SEXP, SEXP);
extern SEXP C_chr_lst_remove_empties(SEXP);
extern SEXP C_chr_vec_remove_empties(SEXP);
extern SEXP C_dbl_lst_nth_elems(SEXP, SEXP);
extern SEXP C_int_lst_cbind(SEXP);
extern SEXP C_int_lst_rbind(SEXP);
extern SEXP C_int_mat_lst_cbind_nth_cols(SEXP, SEXP);
extern SEXP C_int_mat_lst_cbind_nth_rows(SEXP, SEXP);
extern SEXP C_int_mat_lst_nth_cols(SEXP, SEXP);
extern SEXP C_int_mat_lst_nth_rows(SEXP, SEXP);
extern SEXP C_int_mat_lst_rbind_nth_cols(SEXP, SEXP);
extern SEXP C_int_mat_lst_rbind_nth_rows(SEXP, SEXP);
extern SEXP C_int_mat_row_maxs(SEXP);
extern SEXP C_int_vec_all_value(SEXP, SEXP);
extern SEXP C_interleave_chr_lsts(SEXP, SEXP);
extern SEXP C_interleave_chr_vecs(SEXP, SEXP);
extern SEXP C_lst_chr_to_dbl(SEXP, SEXP);
extern SEXP C_lst_elems_common_length(SEXP, SEXP);
extern SEXP C_lst_fullocate(SEXP, SEXP, SEXP);
extern SEXP C_match_arg_index(SEXP, SEXP);
extern SEXP C_str_detect_many_coll(SEXP, SEXP);
extern SEXP C_str_detect_many_fixed(SEXP, SEXP);
extern SEXP C_SXP_int_prlst_cbind(SEXP, SEXP);
extern SEXP C_SXP_int_prlst_rbind(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_chr_lst_nth_elems",          (DL_FUNC) &C_chr_lst_nth_elems,          2},
    {"C_chr_lst_remove_empties",     (DL_FUNC) &C_chr_lst_remove_empties,     1},
    {"C_chr_vec_remove_empties",     (DL_FUNC) &C_chr_vec_remove_empties,     1},
    {"C_dbl_lst_nth_elems",          (DL_FUNC) &C_dbl_lst_nth_elems,          2},
    {"C_int_lst_cbind",              (DL_FUNC) &C_int_lst_cbind,              1},
    {"C_int_lst_rbind",              (DL_FUNC) &C_int_lst_rbind,              1},
    {"C_int_mat_lst_cbind_nth_cols", (DL_FUNC) &C_int_mat_lst_cbind_nth_cols, 2},
    {"C_int_mat_lst_cbind_nth_rows", (DL_FUNC) &C_int_mat_lst_cbind_nth_rows, 2},
    {"C_int_mat_lst_nth_cols",       (DL_FUNC) &C_int_mat_lst_nth_cols,       2},
    {"C_int_mat_lst_nth_rows",       (DL_FUNC) &C_int_mat_lst_nth_rows,       2},
    {"C_int_mat_lst_rbind_nth_cols", (DL_FUNC) &C_int_mat_lst_rbind_nth_cols, 2},
    {"C_int_mat_lst_rbind_nth_rows", (DL_FUNC) &C_int_mat_lst_rbind_nth_rows, 2},
    {"C_int_mat_row_maxs",           (DL_FUNC) &C_int_mat_row_maxs,           1},
    {"C_int_vec_all_value",          (DL_FUNC) &C_int_vec_all_value,          2},
    {"C_interleave_chr_lsts",        (DL_FUNC) &C_interleave_chr_lsts,        2},
    {"C_interleave_chr_vecs",        (DL_FUNC) &C_interleave_chr_vecs,        2},
    {"C_lst_chr_to_dbl",             (DL_FUNC) &C_lst_chr_to_dbl,             2},
    {"C_lst_elems_common_length",    (DL_FUNC) &C_lst_elems_common_length,    2},
    {"C_lst_fullocate",              (DL_FUNC) &C_lst_fullocate,              3},
    {"C_match_arg_index",            (DL_FUNC) &C_match_arg_index,            2},
    {"C_str_detect_many_coll",       (DL_FUNC) &C_str_detect_many_coll,       2},
    {"C_str_detect_many_fixed",      (DL_FUNC) &C_str_detect_many_fixed,      2},
    {"C_SXP_int_prlst_cbind",        (DL_FUNC) &C_SXP_int_prlst_cbind,        2},
    {"C_SXP_int_prlst_rbind",        (DL_FUNC) &C_SXP_int_prlst_rbind,        2},
    {NULL, NULL, 0}
};

void R_init_strex(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
