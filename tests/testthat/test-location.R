context("Location")

test_that("str_locate_braces works", {
  expect_equal(
    str_locate_braces(c("a{](kkj)})", "ab(]c{}")),
    list(
      data.frame(
        position = as.integer(c(2, 3, 4, 8, 9, 10)),
        brace = c("{", "]", "(", ")", "}", ")")
      ),
      data.frame(
        position = as.integer(c(3, 4, 6, 7)),
        brace = c("(", "]", "{", "}")
      )
    )
  )
})

test_that("`str_locate_first/last()` work", {
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
})

test_that("`str_locate_nth()` errors correctly", {
  expect_error(
    str_locate_nth("abc1def2abc", "abc", 3),
    "There aren.* 3 instances of.*`pattern`.* in one.*of.*strings"
  )
})
