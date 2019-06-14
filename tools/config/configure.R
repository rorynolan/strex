# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.

gcc_version <- function() {
  out <- tryCatch(processx::run("gcc", "-v"),
                  error = list(stdout = "", stderr = ""))
  out <- tolower(paste(out$stdout, out$stderr))
  out <- stringr::str_match(out, "gcc version (\\d+(?:\\.\\d+)*)")[1, 2]
  if (!is.na(out)) out <- numeric_version(out)
  out
}

replace_R_fun <- function(orig_lines, fun_name, new_fun_body) {
  fun_pattern <- stringr::coll(paste0(fun_name, " <- function("))
  fun_def_start_line <- match(TRUE,
                              stringr::str_detect(orig_lines, fun_pattern))
  fun_def_end_line <- fun_def_start_line +
    match(TRUE,
          stringr::str_detect(orig_lines[-seq_len(fun_def_start_line)],
                              "^\\}\\s*$"))
  c(orig_lines[seq_len(fun_def_start_line)],
    new_fun_body,
    orig_lines[seq(fun_def_end_line, length(orig_lines))])
}
file_replace_R_fun <- function(path, fun_name, new_fun_body) {
  orig_lines <- readLines(path)
  new_lines <- replace_R_fun(orig_lines, fun_name, new_fun_body)
  writeLines(new_lines, path)
}
file_replace_R_funs <- function(path, fun_names, new_fun_bodies) {
  for (i in seq_along(fun_names)) {
    file_replace_R_fun(fun_names[[i]], new_fun_bodies[[i]])
  }
}

remove_C_fun <- function(orig_lines, fun_sig) {
  fun_def_start_line <- match(
    TRUE,
    stringr::str_detect(orig_lines, stringr::coll(fun_sig))
  )
  while (fun_def_start_line > 1) {
    if (startsWith(orig_lines[fun_def_start_line - 1], "//")) {
      fun_def_start_line <- fun_def_start_line - 1
    } else {
      break
    }
  }
  fun_def_end_line <- fun_def_start_line +
    match(TRUE,
          stringr::str_detect(orig_lines[-seq_len(fun_def_start_line)],
                              "^\\}\\s*$"))
  c(orig_lines[-seq(fun_def_start_line, fun_def_end_line)])
}
file_remove_C_fun <- function(path, fun_sig) {
  orig_lines <- readLines(path)
  new_lines <- remove_C_fun(orig_lines, fun_sig)
  writeLines(new_lines, path)
}
file_remove_C_funs <- function(path, fun_sigs) {
  for (i in seq_along(fun_sigs)) {
    file_remove_C_fun(path, fun_sigs[[i]])
  }
}

remove_matching_lines <- function(orig_lines, patterns) {
  matching_lines <- lapply(
    patterns,
    function(pattern) {
      stringr::str_which(orig_lines, stringr::coll(pattern))
    }
  )
  matching_lines <- unique(unlist(matching_lines))
  orig_lines[-matching_lines]
}
file_remove_matching_lines <- function(path, patterns) {
  orig_lines <- readLines(path)
  new_lines <- remove_matching_lines(orig_lines, patterns)
  writeLines(new_lines, path)
}


if (!is.na(gcc_version()) && gcc_version() < "4.9") {
  cat("Making allowances for GCC < 4.9.\n")
  cat("Replacing R fun.\n")
  file_replace_R_fun("R/RcppExports.R", "lst_char_to_num",
                     c("  if (commas) x %<>% lapply(str_replace_all, ',', '')",
                       "  lapply(x, as.numeric)"))
  cat("Removing C fun.\n")
  file_remove_C_fun("src/list-utils.cpp",
                    "List lst_char_to_num(List x, bool commas)")
  cat("Removing stod.cpp.\n")
  file.remove("src/stod.cpp")
  cat("Removing matching lines.\n")
  file_remove_matching_lines(
    "src/RcppExports.cpp",
    c("List lst_char_to_num(List x, bool commas);",
      "(DL_FUNC) &_strex_lst_char_to_num",
      "NumericVector char_to_num(CharacterVector x, bool commas);",
      "(DL_FUNC) &_strex_char_to_num"))
  cat("Printing RcppExports.cpp.\n")
  cat(readLines("src/RcppExports.cpp"), sep = "\n")
  cat("\nRemoving C funs.\n")
  file_remove_C_funs(
    "src/RcppExports.cpp",
    c("RcppExport SEXP _strex_lst_char_to_num(SEXP xSEXP, SEXP commasSEXP)",
      "RcppExport SEXP _strex_char_to_num(SEXP xSEXP, SEXP commasSEXP)")
  )

  cat("Finished making allowances for GCC < 4.9.")
  cat("RcppExports.R")
  cat(readLines("R/RcppExports.R"), sep = "\n")
  cat("RcppExports.cpp")
  cat(readLines("src/RcppExports.cpp"), sep = "\n")
}
