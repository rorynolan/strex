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

//' Get a series of substrings using `string::substr()`.
//'
//' @param s A `std::string`.
//' @param locs Output of `stringr::str_locate()` (1-indexed).
//'
//' @return A character vector.
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector substrs2(const std::string& s,
                         const IntegerMatrix locs) {
  std::size_t n_locs = locs.nrow();
  CharacterVector out(n_locs);
  for (std::size_t i = 0; i != n_locs; ++i) {
    out[i] = s.substr(locs(i, 0) - 1, locs(i, 1) - locs(i, 0) + 1);
  }
  return out;
}
ı
