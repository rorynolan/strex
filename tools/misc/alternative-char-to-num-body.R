  if (any(nchar(stats::na.omit(x)) == 0)) {
    rlang::abort("Empty string passed to `char_to_num()`.",
                 .subclass = "std::invalid_argument")
  }
  if (commas) {
    y <- str_replace_all(x, ',', '')
  } else {
    y <- x
  }
  out <- suppressWarnings(as.numeric(y))
  if (sum(is.na(out)) > sum(is.na(x))) {
    first_offending_index <- match(
      TRUE,
      is.na(out) & !is.na(x)
    )
    err_msg <- paste0("Could not convert '",
                      x[first_offending_index],
                      "' to numeric.")
    rlang::abort(err_msg, .subclass = "std::invalid_argument")
  }
  out
