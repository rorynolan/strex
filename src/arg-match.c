#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <Rinternals.h>


bool C_is_prefix(const char *full, const char *pre) {
  return strncmp(pre, full, strlen(pre)) == 0;
}

R_xlen_t C_count_if(bool* x, R_xlen_t x_len, R_xlen_t* first_true) {
  R_xlen_t counter = 0;
  *first_true = -1;
  for (R_xlen_t i = 0; i != x_len; ++i) {
    if (x[i]) {
      if (counter == 0) *first_true = i;
      counter++;
    }
  }
  return counter;
}

int C_match_arg_index1(const char *arg, SEXP choices) {
  R_xlen_t n_choices = Rf_xlength(choices);
  bool *is_pre = (bool*) malloc(n_choices * sizeof(bool));
  R_xlen_t first_true = -1;
  for (R_xlen_t i = 0; i != n_choices; ++i)
    is_pre[i] = C_is_prefix(CHAR(STRING_ELT(choices, i)), arg);
  int n_matches = C_count_if(is_pre, n_choices, &first_true);
  free(is_pre);
  if (n_matches == 0) {  // no match
    return -1;
  } else if (n_matches > 1) {  // ambiguity
    for (R_xlen_t i = 0; i != n_choices; ++i) {
      if (strcmp(arg, CHAR(STRING_ELT(choices, i))) == 0) {
          return i + 1;
      }
    }
    return -2;
  } else {
    return first_true + 1;
  }
}

SEXP C_match_arg_index(SEXP arg, SEXP choices) {
  R_xlen_t arg_len = Rf_xlength(arg);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, arg_len));
  int *out_int = INTEGER(out);
  for (R_xlen_t i = 0; i != arg_len; ++i)
    out_int[i] = C_match_arg_index1(CHAR(STRING_ELT(arg, i)), choices);
  UNPROTECT(1);
  return out;
}
