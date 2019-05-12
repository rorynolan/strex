#ifndef STREX_SUBSTRS_
#define STREX_SUBSTRS_


#include <Rcpp.h>
using namespace Rcpp;


CharacterVector substrs(const std::string&,
                        const std::vector<int>&);


#endif  // STREX_SUBSTRS_
