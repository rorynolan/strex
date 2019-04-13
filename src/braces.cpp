#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List lst_df_pos_brace(List positions, List braces) {
  std::size_t n = positions.size();
  if (braces.size() != n) {
    throw std::invalid_argument("`positions` and `braces` must have the "
                                  "same length.");
  }
  List out(n);
  for (std::size_t i = 0; i != n; ++i) {
    out[i] = DataFrame::create(_["position"] = positions[i],
                               _["brace"] = braces[i]);
  }
  return out;
}
