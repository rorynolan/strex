#include <Rcpp.h>
using namespace Rcpp;

//' Paste a vector of strings into a single string.
//'
//' Paste a vector of strings together with a specified separator.
//'
//' @param strings A character vector of strings.
//' @param collapse A string.
//'
//' @return A string.
//'
//' @examples
//' paste_collapse(c("abc", "def"), collapse = "_")
//'
//' @noRd
// [[Rcpp::export]]
std::string paste_collapse(CharacterVector strings, std::string collapse) {
  std::string out = as<std::string>(strings[0]);
  for (int i = 1; i < strings.size(); i++) {
    out += collapse;
    out += strings[i];
  }
  return out;
}

//' Apply paste collapse to each element of a list.
//'
//' This is the same as doing
//' `sapply(char.list, paste, collapse = collapse)`, it's just faster.
//'
//' @param char_list A list of character vectors.
//' @param collapse A string.
//'
//' @return A list of character vectors.
//'
//' @examples
//' paste_collapse_list_elems(list(1:3, c("a", 5, "rory")), collapse = "R")
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector paste_collapse_list_elems(List char_list,
                                          std::string collapse = "") {
  int list_len = char_list.size();
  CharacterVector pasted(list_len);
  for (int i = 0; i < list_len; i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    pasted[i] = paste_collapse(strings, collapse);
  }
  return(pasted);
}
