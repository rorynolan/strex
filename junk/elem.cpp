#include <Rcpp.h>
using namespace Rcpp;

//' Get the indices that exist in a string of length `str_len`.
//'
//' @param str_len The length of a string.
//' @param indices A sorted, unique vector of integers with no zero elements.
std::vector<int32_t> get_good_indices(R_xlen_t str_len,
                                      IntegerVector indices) {
  std::vector<int32_t> out;
  for (R_xlen_t i = 0, n = indices.length(); i != n; ++i)
    if (abs(indices[i]) <= n) out.push_back(indices[i]);
  return out;
}

//' Vectorized `get_good_indices`
std::vector<std::vector<int32_t>> get_good_indices_list(IntegerVector str_lens,
                                                        IntegerVector indices) {
  R_xlen_t n = str_lens.length();
  std::vector<std::vector<int32_t>> out(n);
  for (R_xlen_t i = 0; i != n; ++i)
    out[i] = get_good_indices(str_lens[i], indices);
  return out;
}

//' Get the regular expression to match the `n`th next character.
std::string nth_next_char_regex(R_xlen_t n) {
  if (n == 1) {
    return "(.)";
  }
  return std::string(".{") + (n - 1) + "}(.)";
}

//' Get the regular expression to match this character with `n - 1` unmatched
//' after.
std::string next_char_regex_nm1_after(R_xlen_t n) {
  if (n == 1) {
    return "(.)";
  }
  return std::string("(.).{") + (n - 1) + "}";
}

//' Get the regular expression to match positive indices in a string
std::string get_pos_indices_regex(const std::vector<int32_t>& indices) {
  R_xlen_t i = 0, n = indices.size();
  while (i != n && indices[i] < 0) ++i;
  std::string out;
  if (i == n) return out;
  out.reserve(7 * (n - i));
  out += "^";
  if (indices[i] > 1) out += nth_next_char_regex(indices[i]);
  while(i != n - 1) {
    out += nth_next_char_regex(indices[i + 1] - indices[i]);
    ++i;
  }
}

