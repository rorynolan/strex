test_that("str_alphord_nums works", {
  strings <- paste0("abc", 1:12)
  expect_equal(
    str_alphord_nums(strings),
    str_c("abc", c(paste0(0, 1:9), 10:12))
  )
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
    paste(
      "The non-number bits of every string must be the",
      "same.\n    * The first pair of strings with",
      "different non-number bits\n      are strings 1 and",
      "3.\n    * They are \"abc9def55\" and \"abc10xyz7\""
    ),
    fixed = TRUE
  )
  expect_error(
    str_alphord_nums(c("abc9def55", "9abc10def7")),
    "The strings must all have the same number of numbers."
  )
  expect_error(
    str_alphord_nums(c("0abc9def55g", "abc10def7g0")),
    paste0(
      "It should either be the case that all strings start\\s?",
      "with.+numbers or that none of them do.+String number 1\\s?",
      "\"0abc9def55g\" does\\s?",
      "start with a number.+whereas string number 2\\s?",
      "\"abc10def7g0\" does\\s?",
      "not, start.+with a number."
    )
  )
  expect_error(
    str_alphord_nums("abc"),
    "Some of the input strings have no numbers in them."
  )
  expect_equal(str_alphord_nums(1:10), c(paste0(0, 1:9), 10))
  expect_equal(str_alphord_nums(character()), character())
})
