context("Alphordering")
test_that("str_alphord_nums works", {
  strings <- paste0("abc", 1:12)
  expect_equal(str_alphord_nums(strings),
               str_c("abc", c(paste0(0, 1:9), 10:12)))
  expect_equal(
    str_alphord_nums(c("01abc9def55", "5abc10def777", "99abc4def4")),
    c("01abc09def055", "05abc10def777", "99abc04def004")
  )
  expect_equal(
    str_alphord_nums(c("abc9def55", "abc10def7")),
    c("abc09def55", "abc10def07")
  )
  expect_equal(
    str_alphord_nums(c("abc9def55", "abc10def777", "abc4def4")),
    c("abc09def055", "abc10def777", "abc04def004")
  )
  expect_error(
    str_alphord_nums(c("abc9def55", "abc9def5", "abc10xyz7")),
    "The non-number bits of every string must be the same."
  )
  expect_error(
    str_alphord_nums(c("abc9def55", "9abc10def7")),
    "The `strings` must all have the same number of numbers."
  )
  expect_error(
    str_alphord_nums(c("0abc9def55g", "abc10def7g0")),
    "Either all strings or none should start with numbers."
  )
  expect_error(
    str_alphord_nums("abc"),
    "Some of the input strings have no numbers in them."
  )
  expect_equal(str_alphord_nums(1:10), c(paste0(0, 1:9), 10))
})
