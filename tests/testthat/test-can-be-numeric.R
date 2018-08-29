context("Can be numeric")
test_that("`str_can_be_numeric()` works", {
  expect_true(str_can_be_numeric("3"))
  expect_true(str_can_be_numeric("5 "))
  expect_equal(str_can_be_numeric(c("1a", "abc")), rep(FALSE, 2))
})
