context("Location")


test_that("`str_locate_nth()` works", {
  expect_equal(
    str_locate_first(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(1, 3), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_last(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(5, 7, 4, 6), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth("abc1def2abc", "abc", 3),
    matrix(NA_integer_, ncol = 2, nrow = 1) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth(
      c(
        "This old thing.",
        "That beautiful thing there."
      ),
      "\\w+", c(2, -2)
    ),
    matrix(c(
      6, 8,
      16, 20
    ), ncol = 2, byrow = 2) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_error(str_locate_first(c("a", "b"), c("c", "d", "e")),
    paste(
      "When `string` has length greater than 1,",
      "`pattern` must either\nbe length 1 or have the",
      "same length as `string`.\n    * Your `string` has",
      "length 2.\n    * Your `pattern` has length 3."
    ),
    fixed = TRUE
  )
  expect_error(str_locate_nth(c("a", "b"), c("a", "b"), 1:5),
    paste(
      "When `string` has length greater than 1, `n` must",
      "either be\nlength 1 or have the same length as",
      "`string`.\n    * Your `string` has length 2.\n    *",
      "Your `n` has length 5."
    ),
    fixed = TRUE
  )
  expect_equal(
    str_locate_nth("abc", "b", c(0, 1, 1, 2)),
    matrix(c(rep(NA, 2), rep(2, 4), rep(NA, 2)),
      ncol = 2, byrow = TRUE
    ) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth(character(0), "b", 4),
    matrix(character(0), ncol = 2) %>%
      magrittr::set_colnames(c("start", "end"))
  )
})

test_that("str_locate_braces() works", {
  string <- c("a{](kkj)})", "ab(]c{}")
  out <- str_locate_braces(string)
  expect_equal(
    as.data.frame(out),
    data.frame(
      string_num = as.integer(rep(1:2, c(6, 4))),
      string = rep(string, c(6, 4)),
      position = as.integer(c(
        2, 3, 4, 8, 9, 10,
        3, 4, 6, 7
      )),
      brace = c(
        "{", "]", "(", ")", "}", ")", "(",
        "]", "{", "}"
      ),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_locate_braces(character())),
    as.data.frame(out[0, ])
  )
})
