#include <Rcpp.h>
using namespace Rcpp;

#include <regex>

//' Convert a character vector to a numeric vector.
//'
//' This is my cpp best shot at R's `as.numeric()`.
//'
//' @param x A character vector.
//' @param commas Allow comma-separated numbers like 1,000?
//'
//' @return A numeric vector.
//'
//' @noRd
// [[Rcpp::export]]
NumericVector char_to_num(CharacterVector x, bool commas) {
  std::size_t n = x.size();
  if (n == 0) return NumericVector(0);
  NumericVector out(n);
  std::regex comma_regex;
  if (commas) comma_regex = std::regex(",", std::regex::extended);
  LogicalVector nas = is_na(x);
  for (std::size_t i = 0; i != n; ++i) {
    double number = NA_REAL;
    if (!nas[i]) {
      std::string x_i = as<std::string>(x[i]);
      try {
        std::size_t pos;
        if (commas) {
          std::string x_i_nocommas = std::regex_replace(x_i, comma_regex, "");
          number = std::stod(x_i_nocommas, &pos);
          if (pos != x_i_nocommas.size()) {
            throw std::invalid_argument("Could not convert '" + x_i +
                                        "' to numeric.");
          }
        } else {
          number = std::stod(x_i, &pos);
          if (pos != x_i.size()) {
            throw std::invalid_argument("Could not convert '" + x_i +
                                        "' to numeric.");
          }
        }
      } catch (const std::invalid_argument& e) {
        if (!x_i.size()) {
          throw std::invalid_argument("Empty string passed to "
                                      "`char_to_num()`.");
        } else {
          throw std::invalid_argument("Could not convert '" + x_i +
                                      "' to numeric.");
        }
      } catch (const std::out_of_range& e) {
        number = ((x_i[0] == '-') ? R_NegInf : R_PosInf);
      }
    }
    out[i] = number;
  }
  return out;
}

