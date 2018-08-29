context("Converting strings to vectors")
test_that("str_to_vec works", {
  expect_equal(str_to_vec("abcdef"), c("a", "b", "c", "d", "e", "f"))
})
