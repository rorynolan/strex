test_that("str_extract_numbers works", {
  expect_equal(
    str_extract_numbers(c("abc123abc456", "abc1.23abc456")),
    list(c(123, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers(c("abc1.23abc456", "abc1..23abc456"),
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(1.23, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers("abc1..23abc456",
      decimals = TRUE, leading_decimals = FALSE
    ),
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
  ), list(c("1", ".23", "456")))
  expect_equal(str_extract_numbers("-123abc456"), list(c(123, 456)))
  expect_equal(
    str_extract_numbers("-123abc456", negs = TRUE),
    list(c(-123, 456))
  )
  expect_equal(
    str_extract_numbers("--123abc456", negs = TRUE),
    list(c(-123, 456))
  )
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
    str_extract_non_numerics("abc1..23abc456",
      decimals = TRUE,
      leading_decimals = FALSE
    ),
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
  expect_snapshot_warning(
    str_extract_numbers("abc1.2.3", decimals = TRUE)
  )
  expect_equal(suppressWarnings(
    str_extract_numbers("abc1.2.3", decimals = TRUE)
  ), list(NA_real_))
  expect_snapshot_warning(
    str_extract_numbers("ab.1.2",
      decimals = TRUE,
      leading_decimals = TRUE
    )
  )
  expect_snapshot_warning(
    str_extract_numbers("ab.1.2",
      decimals = TRUE,
      leading_decimals = TRUE
    )
  )
  expect_equal(suppressWarnings(str_extract_numbers("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  )), list(NA_real_))
  expect_snapshot_warning(
    str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
                        decimals = TRUE)
  )
  expect_equal(
    suppressWarnings(
      str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
        decimals = TRUE
      )
    ),
    c(as.list(rep(NA_real_, 3)), list(c(5, 6)))
  )
  expect_equal(str_nth_number("abc1.23abc456", 2), 23)
  expect_equal(str_first_number("abc1a2"), 1)
  expect_equal(str_last_number("akd50lkdjf0qukwjfj8"), 8)
  expect_equal(str_nth_number("abc1.23abc456", 2, leave_as_string = TRUE), "23")
  expect_equal(str_nth_number("abc1.23abc456", 2, decimals = TRUE), 456)
  expect_equal(str_nth_number("-123abc456", -2, negs = TRUE), -123)
  expect_equal(str_first_number("abc1e5"), 1)
  expect_equal(str_first_number("abc1e5", sci = TRUE), 1e5)
  expect_equal(str_first_number("abc1.4e5", sci = TRUE), 1)
  expect_equal(str_first_number("abc1.4e5", sci = TRUE, decimals = TRUE), 1.4e5)
  expect_equal(
    str_first_number("abc-1.4e5", sci = TRUE, decimals = TRUE),
    1.4e5
  )
  expect_equal(
    str_first_number("abc-1.4e5",
      sci = TRUE, decimals = TRUE,
      negs = TRUE
    ),
    -1.4e5
  )
  expect_snapshot_warning(
    expect_equal(
      str_first_number("ab.1.2",
        decimals = TRUE, leading_decimals = TRUE
      ),
      NA_real_
    )
  )
  expect_equal(
    suppressWarnings(str_first_number("ab.1.2",
      decimals = TRUE, leading_decimals = TRUE
    )),
    NA_real_
  )
  expect_equal(suppressWarnings(str_last_number("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  )), NA_real_)
  expect_snapshot_error(str_extract_numbers("a.23", leading_decimals = T))
  expect_equal(str_first_number("abc"), NA_integer_)
  expect_equal(
    strex:::dbl_lst_nth_elems(list(c(1, 2)), c(-1, 3)),
    c(2, NA)
  )
  expect_equal(strex:::dbl_lst_nth_elems(list(c(1, 2), c(3, 4)), -1), c(2, 4))
  expect_equal(strex:::dbl_lst_nth_elems(list(c(1, 2), c(3, 4)), c(-1, 1)), 2:3)
  strings <- c(
    "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
    "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
  )
  expect_equal(
    str_nth_number(strings, n = 2),
    c(456, 12, 4, 100, 100)
  )
  expect_equal(
    str_nth_number(strings, n = -2, decimals = TRUE),
    c(123, 0.12, 34.5, 1, 1)
  )
  expect_equal(
    str_first_number(strings,
      decimals = TRUE, leading_decimals = TRUE
    ),
    c(123, 0.12, 0.12, 1, 1)
  )
  expect_equal(
    str_last_number(strings, commas = TRUE),
    c(456, 345, 9, 5, 1000)
  )
  expect_equal(str_nth_number(strings,
    n = 1, decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), c(123, 0.12, 1200, 1, 1))
  expect_equal(str_first_number(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, commas = TRUE, negs = TRUE
  ), c(123, -0.12, 1200, 1100, Inf))
  expect_equal(str_last_number(strings,
    decimals = TRUE, leading_decimals = FALSE,
    sci = FALSE, commas = TRUE, leave_as_string = TRUE
  ), c("456", "345", "9", "1,230.5", "1,000"))
  expect_equal(
    str_extract_numbers(strings),
    list(c(123, 456), c(0, 12, 345), c(12, 4, 34, 5, 9), c(
      1, 100,
      1, 230, 5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings,
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(123, 456), c(0.12, 345), c(12, 4, 34.5, 9), c(
      1, 100,
      1, 230.5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings,
      decimals = TRUE, leading_decimals = TRUE
    ),
    list(c(123, 456), c(0.12, 0.345), c(0.12, 4, 34.5, 9), c(
      1, 100,
      1, 230.5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings, commas = TRUE),
    list(c(123, 456), c(0, 12, 345), c(12, 4, 34, 5, 9), c(
      1100,
      1230, 5
    ), c(1100, 3215, 4, 1000))
  )
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, sci = TRUE
  ), list(c(123, 456), c(0.12, 0.345), c(1200, 3.45e+10), c(
    1, 100,
    1, 230.5
  ), c(1, 1e+05, 215, 40, 0)))
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, sci = TRUE, commas = TRUE, negs = TRUE
  ), list(c(123, 456), c(-0.12, 0.345), c(1200, 3.45e+10), c(
    1100,
    1230.5
  ), c(Inf, Inf)))
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, leading_decimals = FALSE,
    sci = FALSE, commas = TRUE, leave_as_string = TRUE
  ), list(c("123", "456"), c("0.12", "345"), c(
    "12", "4", "34.5",
    "9"
  ), c("1,100", "1,230.5"), c("1,100", "3,215", "4", "1,000")))
  expect_equal(str_extract_numbers(character()), list())
  expect_equal(str_first_number(character()), numeric())
  expect_equal(
    str_last_number(character(), leave_as_string = TRUE),
    character()
  )
})
