#include <Rcpp.h>
using namespace Rcpp;


bool is_prefix(const std::string& full, const std::string& prefix) {
  if (prefix.length() > full.length()) {
    return false;
  } else {
    return std::equal(prefix.begin(), prefix.end(), full.begin());
  }
}

int count_if(LogicalVector x, int& first_true) {
  int counter = 0;
  first_true = -1;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      if (counter == 0) first_true = i;
      counter++;
    }
  }
  return counter;
}

int match_arg_index1(std::string arg, CharacterVector choices) {
  std::size_t n_choices = choices.size();
  LogicalVector is_pre(n_choices);
  int first_true = -1;
  for (std::size_t i = 0; i != n_choices; ++i)
    is_pre[i] = is_prefix(std::string(choices[i]), arg);
  int n_matches = count_if(is_pre, first_true);
  if (n_matches == 0) {  // no match
    return -1;
  } else if (n_matches > 1) {  // ambiguity
    std::size_t arg_len = arg.length();
    for (std::size_t i = 0; i != n_choices; ++i) {
      std::string choice(choices[i]);
      if (arg_len == choice.length()) {
        if (std::equal(arg.begin(), arg.end(), choice.begin()))
          return i;
      }
    }
    return -2;
  } else {
    return first_true;
  }
}

// [[Rcpp::export]]
IntegerVector match_arg_index(CharacterVector arg, CharacterVector choices) {
  int arg_sz = arg.size();
  IntegerVector out(arg_sz);
  for (int i = 0; i != arg_sz; ++i)
    out[i] = match_arg_index1(std::string(arg[i]), choices);
  return out;
}
