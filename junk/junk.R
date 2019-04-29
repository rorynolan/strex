#' Group together close adjacent elements of a vector.
#'
#' Given a strictly increasing vector (each element is bigger than the last),
#' group together stretches of the vector where *adjacent* elements are
#' separated by at most some specified distance. Hence, each element in each
#' group has at least one other element in that group that is *close* to it. See
#' the examples.
#' @param x A strictly increasing numeric vector.
#' @param max_gap The biggest allowable gap between adjacent elements for them
#'   to be considered part of the same *group*.
#' @param check Check inputs for validity? Can be turned off for speed if you're
#'   sure your inputs are valid.
#' @return A where each element is one group, as a numeric vector.
#' @examples
#' group_close(1:10, 1)
#' group_close(1:10, 0.5)
#' group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3)
#' @noRd
group_close <- function(x, max_gap = 1, check = TRUE) {
  dva <- diff(x)
  if (check) {
    checkmate::assert_numeric(x, min.len = 1)
    test <- dva > 0
    if (anyNA(test) || (!all(test))) {
      bad_index <- match(F, test)
      custom_stop(
        "`vec_ascending` must be strictly increasing.",
        "
                  Indices {bad_index} and {bad_index + 1} of `vec_ascending`
                  are respectively {vec_ascending[bad_index]} and
                  {vec_ascending[bad_index + 1]}, therefore `vec_ascending`
                  is not strictly increasing.
                  "
      )
    }
  }
  lva <- length(x)
  if (lva == 1) return(list(x))
  gaps <- dva
  big_gaps <- which(gaps > max_gap)
  nbgaps <- length(big_gaps) # number of big gaps
  if (!nbgaps) return(list(x))
  big_gaps %>% {
    split(x, rep(seq_len(nbgaps + 1), times = c(.[1], diff(c(., lva)))))
  }
}

test_that("group_close works", {
  expect_equal(unname(group_close(1:10, 1)), list(1:10))
  expect_equal(unname(group_close(1:10, 0.5)), as.list(1:10))
  expect_equal(
    unname(group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3)),
    list(c(1, 2, 4), c(10, 11, 14), 20, c(25, 27))
  )
  expect_error(group_close(integer(0)))
  expect_error(group_close(rep(1, 2)))
  expect_equal(unname(group_close(0)), list(0))
  expect_equal(unname(group_close(c(0, 2))), list(0, 2))
})

#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A list of data frames, one for each member of the string character
#'   vector. Each data frame has a "position" and "brace" column which give the
#'   positions and types of braces in the given string.
#'
#' @examples
#' str_locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @export
str_locate_braces <- function(string) {
  locations <- str_locate_all(string, "[\\(\\)\\[\\]\\{\\}]") %>%
    int_lst_first_col()
  braces <- str_elems(string, locations)
  lst_df_pos_brace(locations, braces)
}

get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "mac"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "mac"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}
