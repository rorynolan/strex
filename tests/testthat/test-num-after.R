test_that("`nth_number_after_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4abc5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(str_nth_number_after_mth(string, "abc", 1, 3), c(3, 7))
  expect_equal(str_nth_number_after_mth(string, "abc", 2, 3), c(4, 8))
  expect_equal(str_nth_number_after_first(string, "abc", 2), c(2, 2))
  expect_equal(str_nth_number_after_last(string, "abc", -1), c(9, 9))
  expect_equal(str_first_number_after_mth(string, "abc", 2), c(2, 4))
  expect_equal(str_last_number_after_mth(string, "abc", 1), c(9, 9))
  expect_equal(str_first_number_after_first(string, "abc"), c(1, 1))
  expect_equal(str_first_number_after_last(string, "abc"), c(9, 7))
  expect_equal(str_last_number_after_first(string, "abc"), c(9, 9))
  expect_equal(str_last_number_after_last(string, "abc"), c(9, 9))
  expect_equal(str_last_number_after_last(character(), "abc"), numeric())
  expect_equal(
    str_last_number_after_last(character(), "abc",
      leave_as_string = TRUE
    ),
    character()
  )
  expect_snapshot_error(str_nth_number_after_mth("abc", "123", 1:2, 1:3))
})
