#ifndef STRINGI_IMPORTS_H_
#define STRINGI_IMPORTS_H_

#include <Rinternals.h>


SEXP C_stringi_replace_all_coll(SEXP string, SEXP pattern, SEXP replacement);
SEXP C_stringi_detect_coll(SEXP string, SEXP pattern);
SEXP C_stringi_detect_fixed(SEXP string, SEXP pattern);


#endif  // STRINGI_IMPORTS_H_
