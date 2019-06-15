# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.

gcc_version <- function() {
  out <- tryCatch(processx::run("gcc", "-v", stderr_to_stdout = TRUE),
                  error = function(cnd) list(stdout = ""))
  out <- stringr::str_match(out$stdout, "gcc version (\\d+(?:\\.\\d+)*)")[1, 2]
  if (!is.na(out)) out <- numeric_version(out)
  out
}

replace_R_fun <- function(orig_lines, fun_name, new_fun_body) {
  fun_pattern <- stringr::coll(paste0(fun_name, " <- function("))
  fun_def_start_lines <- which(
    stringr::str_starts(stringr::str_trim(orig_lines), fun_pattern)
  )
  if (!length(fun_def_start_lines)) {
    if (Sys.getenv("TRAVIS") == "true" || interactive())
      cat(paste0("Didn't find function '", fun_sig, "'.\n"))
    return(orig_lines)
  }
  fun_def_start_line <- fun_def_start_lines[[1]]
  fun_def_end_line <- fun_def_start_line +
    stringr::str_which(orig_lines[-seq_len(fun_def_start_line)],
                       "^\\}\\s*$")[[1]]
  c(orig_lines[seq_len(fun_def_start_line)],
    new_fun_body,
    orig_lines[seq(fun_def_end_line, length(orig_lines))])
}
file_replace_R_fun <- function(path, fun_name, new_fun_body) {
  if (Sys.getenv("TRAVIS") == "true" || interactive())
    cat(paste0("File: ", path, ".\n"))
  orig_lines <- readLines(path)
  new_lines <- replace_R_fun(orig_lines, fun_name, new_fun_body)
  writeLines(new_lines, path)
}
file_replace_R_funs <- function(path, fun_names, new_fun_bodies) {
  for (i in seq_along(fun_names)) {
    file_replace_R_fun(path, fun_names[[i]], new_fun_bodies[[i]])
  }
}

remove_C_fun <- function(orig_lines, fun_sig) {
  fun_sig <- stringr::coll(fun_sig)
  fun_def_start_lines <- fun_def_start_lines <- which(
    stringr::str_starts(stringr::str_trim(orig_lines), fun_sig)
  )
  if (!length(fun_def_start_lines)) {
    if (Sys.getenv("TRAVIS") == "true" || interactive())
      cat(paste0("Didn't find function '", fun_sig, "'.\n"))
    return(orig_lines)
  }
  fun_def_start_line <- fun_def_start_lines[[1]]
  while (fun_def_start_line > 1) {
    if (stringr::str_starts(orig_lines[fun_def_start_line - 1],
                            stringr::coll("//"))) {
      fun_def_start_line <- fun_def_start_line - 1
    } else {
      break
    }
  }
  fun_def_end_line <- fun_def_start_line +
    stringr::str_which(orig_lines[-seq_len(fun_def_start_line)],
                       "^\\}\\s*$")[[1]]
  c(orig_lines[-seq(fun_def_start_line, fun_def_end_line)])
}
file_remove_C_fun <- function(path, fun_sig) {
  if (Sys.getenv("TRAVIS") == "true" || interactive())
    cat(paste0("File: ", path, ".\n"))
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
  if (length(matching_lines)) {
    orig_lines[-matching_lines]
  } else {
    orig_lines
  }
}
file_remove_matching_lines <- function(path, patterns) {
  if (Sys.getenv("TRAVIS") == "true" || interactive())
    cat(paste0("File: ", path, ".\n"))
  orig_lines <- readLines(path)
  new_lines <- remove_matching_lines(orig_lines, patterns)
  writeLines(new_lines, path)
}


if (!is.na(gcc_version()) && gcc_version() < "4.9") {
  cat("Making allowances for GCC < 4.9.\n")
  cat("Replacing R fun.\n")
  file_replace_R_funs(
    "R/RcppExports.R",
    c("char_to_num", "lst_char_to_num"),
    lapply(paste0("tools/misc/alternative-",
                  c("char-to-num", "lst-char-to-num"),
                  "-body.R"),
           readLines)
  )
  cat("Removing C fun.\n")
  file_remove_C_fun("src/list-utils.cpp",
                    "List lst_char_to_num(List x, bool commas)")
  cat("Removing stod.cpp.\n")
  if (file.exists("src/stod.cpp")) file.remove("src/stod.cpp")
  cat("Removing matching lines.\n")
  file_remove_matching_lines(
    "src/RcppExports.cpp",
    c("List lst_char_to_num(List x, bool commas);",
      "(DL_FUNC) &_strex_lst_char_to_num",
      "NumericVector char_to_num(CharacterVector x, bool commas);",
      "(DL_FUNC) &_strex_char_to_num"))
  cat("\nRemoving C funs.\n")
  file_remove_C_funs(
    "src/RcppExports.cpp",
    c("RcppExport SEXP _strex_lst_char_to_num(SEXP xSEXP, SEXP commasSEXP)",
      "RcppExport SEXP _strex_char_to_num(SEXP xSEXP, SEXP commasSEXP)")
  )
  cat("Finished making allowances for GCC < 4.9.\n")
}
