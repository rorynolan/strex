test_that("str_split_by_numbers works", {
  expect_equal(
    str_split_by_numbers(c("abc123def456.789gh", "a1b2c344")),
    list(
      c("abc", "123", "def", "456", ".", "789", "gh"),
      c("a", 1, "b", 2, "c", 344)
    )
  )
  expect_equal(
    str_split_by_numbers("abc123def456.789gh", decimals = TRUE),
    list(c("abc", "123", "def", "456.789", "gh"))
  )
  expect_equal(str_split_by_numbers("22"), list("22"))
  expect_equal(
    suppressWarnings(str_split_by_numbers("abc25.25.25def", decimals = TRUE)),
    list(NA_character_)
  )
  expect_equal(str_split_by_numbers(character()), list())
})
