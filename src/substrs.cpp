#include <Rcpp.h>
using namespace Rcpp;


//' Get a series of substrings using `string::substr()`.
//'
//' @param s A `std::string`.
//' @param locs An even length `std::vector<size_t>`. Pairs of numbers
//' indicating the locations (0-indexed, start position and size) of the
//' substrings to extract.
//'
//' @return A character vector.
//'
//' @noRd
CharacterVector substrs(const std::string& s,
                        const std::vector<int>& locs) {
  std::size_t ls = locs.size(), i = 0;
  CharacterVector out(ls / 2);
  while (i != ls) {
    out[i / 2] = s.substr(locs[i], locs[i + 1]);
    i += 2;
  }
  return out;
}

