context("Number extraction")
test_that("str_extract_numbers works", {
  expect_equal(
    str_extract_numbers(c("abc123abc456", "abc1.23abc456")),
    list(c(123, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers(c("abc1.23abc456", "abc1..23abc456"),
      decimals = TRUE
    ),
    list(c(1.23, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers("abc1..23abc456", decimals = TRUE),
    list(c(1, 23, 456))
  )
  expect_equal(str_extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c(1, .23, 456)))
  expect_equal(str_extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE,
    leave_as_string = TRUE
  ), list((c("1", ".23", "456"))))
  expect_equal(str_extract_numbers("-123abc456"), list(c(123, 456)))
  expect_equal(str_extract_numbers("-123abc456", negs = TRUE),
               list(c(-123, 456)))
  expect_equal(str_extract_numbers("--123abc456", negs = TRUE),
               list(c(-123, 456)))
  expect_equal(str_extract_non_numerics("abc123abc456"), list(rep("abc", 2)))
  expect_equal(
    str_extract_non_numerics("abc1.23abc456"),
    list(c("abc", ".", "abc"))
  )
  expect_equal(
    str_extract_non_numerics("abc1.23abc456", decimals = TRUE),
    list(c("abc", "abc"))
  )
  expect_equal(
    str_extract_non_numerics("abc1..23abc456", decimals = TRUE),
    list(c("abc", "..", "abc"))
  )
  expect_equal(str_extract_non_numerics("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c("abc", ".", "abc")))
  expect_equal(
    str_extract_non_numerics(c("-123abc456", "ab1c")),
    list(c("-", "abc"), c("ab", "c"))
  )
  expect_equal(str_extract_non_numerics("-123abc456", negs = TRUE), list("abc"))
  expect_equal(
    str_extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(str_extract_numbers("abc1.2.3", decimals = TRUE), list(NA_real_))
  expect_equal(str_extract_numbers("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(NA_real_))
  expect_equal(
    str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
      decimals = TRUE
    ),
    c(as.list(rep(NA_real_, 3)), list(c(5, 6)))
  )
  expect_equal(str_nth_number("abc1.23abc456", 2), 23)
  expect_equal(str_first_number("abc1a2"), 1)
  expect_equal(str_last_number("akd50lkdjf0qukwjfj8"), 8)
  expect_equal(str_nth_number("abc1.23abc456", 2, leave_as_string = TRUE), "23")
  expect_equal(str_nth_number("abc1.23abc456", 2, decimals = TRUE), 456)
  expect_equal(str_nth_number("-123abc456", -2, negs = TRUE), -123)
  expect_equal(
    str_extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(str_first_non_numeric("--123abc456"), "--")
  expect_equal(str_last_non_numeric("--123abc456"), "abc")
  expect_equal(str_nth_non_numeric("--123abc456", -2), "--")
  expect_error(str_extract_numbers("a.23", leading_decimals = T))
  expect_error(str_extract_non_numerics("a.23", leading_decimals = T))
  expect_equal(str_first_number("abc"), NA_integer_)
  expect_equal(str_first_non_numeric("1"), NA_character_)
  expect_equal(str_last_non_numeric(c("abc", "def")), c("abc", "def"))
  expect_equal(
    str_nth_non_numeric(c("ab12bd23", "wx56yz89"), c(3, -1)),
    c(NA, "yz")
  )
  expect_equal(
    strex:::num_list_nth_elems(list(c(1, 2)), c(-1, 3)),
    c(2, NA)
  )
  expect_equal(strex:::num_list_nth_elems(list(1:2, 3:4), -1), c(2, 4))
  expect_equal(strex:::num_list_nth_elems(list(1:2, 3:4), c(-1, 1)), 2:3)
})
