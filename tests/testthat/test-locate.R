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
    structure(c(NA, NA), .Dim = 1:2, .Dimnames = list(NULL, c(
      "start",
      "end"
    )))
  )
  expect_equal(
    str_locate_braces(c("a{](kkj)})", "ab(]c{}")),
    structure(list(string_num = c(
      1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
      2L, 2L
    ), string = c(
      "a{](kkj)})", "a{](kkj)})", "a{](kkj)})",
      "a{](kkj)})", "a{](kkj)})", "a{](kkj)})", "ab(]c{}", "ab(]c{}",
      "ab(]c{}", "ab(]c{}"
    ), position = c(
      2L, 3L, 4L, 8L, 9L, 10L,
      3L, 4L, 6L, 7L
    ), brace = c(
      "{", "]", "(", ")", "}", ")", "(",
      "]", "{", "}"
    )), row.names = c(NA, -10L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ))
  )
  expect_equal(str_locate_nth(c("This old thing.",
                                "That beautiful thing there."),
                              "\\w+", c(2, -2)),
               matrix(c(6, 8,
                        16, 20), ncol = 2, byrow = 2) %>%
                 magrittr::set_colnames(c("start", "end")))
  expect_error(str_locate_first(c("a", "b"), c("c", "d", "e")),
               paste("`pattern` must either be length 1 or have the",
                     "same length as\n`string`\n    * Your `pattern` has",
                     "length 3.\n    * Your `string` has length 2."),
               fixed = TRUE)
  expect_error(str_locate_nth(c("a", "b"), c("a", "b"), 1:5),
               paste("`n` must either be length 1 or have the same",
                     "length as\n`string`\n    * Your `n` has length 5.\n ",
                     "  * Your `string` has length 2."),
               fixed = TRUE)
})
