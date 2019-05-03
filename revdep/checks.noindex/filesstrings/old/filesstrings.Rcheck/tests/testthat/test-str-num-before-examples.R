context("Str num before")

test_that("`nth_number_before_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4def5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(nth_number_before_mth(string, "def", 1, 1), c(1, 1))
  expect_equal(nth_number_before_mth(string, "abc", 2, 3), c(2, 2))
  expect_equal(nth_number_before_first(string, "def", 2), c(2, NA))
  expect_equal(nth_number_before_last(string, "def", -1), c(4, 7))
  expect_equal(first_number_before_mth(string, "abc", 2), c(1, 1))
  expect_equal(last_number_before_mth(string, "def", 1), c(4, 1))
  expect_equal(first_number_before_first(string, "def"), c(1, 1))
  expect_equal(first_number_before_last(string, "def"), c(1, 1))
  expect_equal(last_number_before_first(string, "def"), c(4, 1))
  expect_equal(last_number_before_last(string, "def"), c(4, 7))
})
