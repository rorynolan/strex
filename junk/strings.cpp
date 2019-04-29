#include <stdexcept>
#include <string>

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
List str_elems(StringVector strings, List locations) {
  std::size_t n = strings.size();
  if (locations.size() != n) {
    throw std::invalid_argument("`strings` and `locations` must have the "
                                "same length.");
  }
  List out(n);
  for (std::size_t i = 0; i != n; ++i) {
    IntegerVector locations_i = locations[i];
    std::size_t m = locations_i.size();
    CharacterVector out_i(m);
    for (std::size_t j = 0; j != m; ++j) {
      out_i[j] = std::string(1, strings[i][locations_i[j] - 1]);
    }
    out[i] = out_i;
  }
  return out;
}




