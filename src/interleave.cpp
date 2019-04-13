#include <Rcpp.h>
using namespace Rcpp;

#include "pasting.h"

//' Interleave two vectors of strings.
//'
//' Make a vector of strings where the first element is from `strings1`, the
//' second is from `strings2`, the third is from `strings1`, the fourth is from
//' `strings2`, and so on.
//'
//' `strings1` and `strings2` must be the same length or differ in length only
//' by 1. If `strings2` is longer, it goes first.
//'
//' @param strings1,strings2 Character vectors.
//'
//' @return A character vector.
//'
//' @examples
//' interleave_strings(c("a", "c", "e"), c("b", "d"))
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector interleave_strings(CharacterVector strings1,
                                   CharacterVector strings2) {
  int s1l = strings1.size();
  int s2l = strings2.size();
  int length_diff = s1l - s2l;
  if (abs(length_diff) > 1) {
    return(NA_STRING);
  } else {
    int l = s1l + s2l;
    CharacterVector interleaved(l);
    int i = 0;
    if (length_diff >= 0) {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings1[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings2[i / 2];
          i++;
        }
      }
    } else {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings2[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings1[i / 2];
          i++;
        }
      }
    }
    return(interleaved);
  }
}


// [[Rcpp::export]]
CharacterVector interleave_correctly_vec(std::string orig,
                                         CharacterVector strings1,
                                         CharacterVector strings2) {
  CharacterVector interleave = NA_STRING;
  if (strings1.size() == 0)
    interleave = strings2;
  else if (strings2.size() == 0)
    interleave = strings1;
  else {
    CharacterVector onetwo = interleave_strings(strings1, strings2);
    if (paste_collapse(onetwo, "") == orig)
      interleave = onetwo;
    else {
      CharacterVector twoone = interleave_strings(strings2, strings1);
      if (paste_collapse(twoone, "") == orig)
        interleave = twoone;
    }
  }
  return(interleave);
}

// [[Rcpp::export]]
List interleave_correctly(CharacterVector orig, List strings1, List strings2) {
  int l = orig.size();
  List interleaved(l);
  if (strings1.size() != l || strings2.size() != l) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = interleave_correctly_vec(as<std::string>(orig[i]),
                                                strings1[i], strings2[i]);
    }
  }
  return(interleaved);
}

// [[Rcpp::export]]
List interleave_char_lists(List strings1, List strings2) {
  int l = strings1.size();
  List interleaved(l);
  if (l != strings2.size()) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = interleave_strings(as<CharacterVector>(strings1[i]),
                                          as<CharacterVector>(strings2[i]));
    }
  }
  return(interleaved);
}
