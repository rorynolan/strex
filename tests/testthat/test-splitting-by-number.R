context("Splitting by number")
test_that("str_split_by_nums works", {
  expect_equal(
    str_split_by_nums(c("abc123def456.789gh", "a1b2c344")),
    list(
      c("abc", "123", "def", "456", ".", "789", "gh"),
      c("a", 1, "b", 2, "c", 344)
    )
  )
  expect_equal(
    str_split_by_nums("abc123def456.789gh", decimals = TRUE),
    list(c("abc", "123", "def", "456.789", "gh"))
  )
  expect_equal(str_split_by_nums("22"), list("22"))
})
