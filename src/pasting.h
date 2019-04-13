#ifndef STREX_PASTING_
#define STREX_PASTING_


#include <Rcpp.h>
using namespace Rcpp;

std::string paste_collapse(CharacterVector, std::string);

CharacterVector paste_collapse_list_elems(List, std::string);


#endif  // STREX_PASTING_
