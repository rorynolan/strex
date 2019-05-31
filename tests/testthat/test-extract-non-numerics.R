test_that("str_extract_non_numerics() works", {
  expect_equal(
    str_extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(str_first_non_numeric("--123abc456"), "--")
  expect_equal(str_last_non_numeric("--123abc456"), "abc")
  expect_equal(str_nth_non_numeric("--123abc456", -2), "--")
  expect_error(str_extract_non_numerics("a.23", leading_decimals = T))
  expect_equal(str_first_non_numeric("1"), NA_character_)
  expect_equal(str_last_non_numeric(c("abc", "def")), c("abc", "def"))
  expect_equal(
    str_nth_non_numeric(c("ab12bd23", "wx56yz89"), c(3, -1)),
    c(NA, "yz")
  )
  strings <- c(
    "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
    "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
  )
  expect_equal(
    str_extract_non_numerics(strings),
    list(c("abc", "def"), c("abc-", ".", "def."), c(
      "abc.", "e",
      "def", ".", "e"
    ), c("abc", ",", "def", ",", "."), c(
      "abc", ",",
      "e", ",", "def", "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings,
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c("abc", "def"), c("abc-", "def."), c(
      "abc.", "e", "def",
      "e"
    ), c("abc", ",", "def", ","), c(
      "abc", ",", "e", ",", "def",
      "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings, decimals = TRUE),
    list(c("abc", "def"), c("abc-", "def"), c(
      "abc", "e", "def",
      "e"
    ), c("abc", ",", "def", ","), c(
      "abc", ",", "e", ",", "def",
      "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings, commas = TRUE),
    list(c("abc", "def"), c("abc-", ".", "def."), c(
      "abc.", "e",
      "def", ".", "e"
    ), c("abc", "def", "."), c(
      "abc", "e", "def",
      "e"
    ))
  )
  expect_equal(str_extract_non_numerics(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), list(c("abc", "def"), c("abc-", "def"), c("abc", "def"), c(
    "abc",
    ",", "def", ","
  ), c("abc", ",", ",", "def", ",")))
  expect_equal(str_extract_non_numerics(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, commas = TRUE, negs = TRUE
  ), list(c("abc", "def"), c("abc", "def"), c("abc", "def"), c(
    "abc",
    "def"
  ), c("abc", "def")))
  expect_equal(
    str_nth_non_numeric(strings, n = 2),
    c("def", ".", "e", ",", ",")
  )
  expect_equal(
    str_nth_non_numeric(strings, n = -2, decimals = TRUE),
    c("abc", "abc-", "def", "def", "e")
  )
  expect_equal(str_first_non_numeric(strings,
    decimals = TRUE,
    leading_decimals = FALSE
  ), c("abc", "abc-", "abc.", "abc", "abc"))
  expect_equal(
    str_last_non_numeric(strings, commas = TRUE),
    c("def", "def.", "e", ".", "e")
  )
  expect_equal(str_nth_non_numeric(strings,
    n = 1, decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), c("abc", "abc-", "abc", "abc", "abc"))
  expect_equal(str_first_non_numeric(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, commas = TRUE, negs = TRUE
  ), c("abc", "abc", "abc", "abc", "abc"))
  expect_equal(
    suppressWarnings(str_extract_non_numerics("abc25.25.25def",
      decimals = TRUE
    )),
    list(NA_character_)
  )
  expect_equal(
    suppressWarnings(str_last_non_numeric("abc25.25.25def",
      decimals = TRUE
    )),
    NA_character_
  )
  expect_equal(str_extract_non_numerics(character()), list())
  expect_equal(str_last_non_numeric(character()), character())
})
