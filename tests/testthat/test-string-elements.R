context("String elements")
test_that("str_elem works", {
  expect_equal(str_elem("abcd", 3), "c")
  expect_equal(str_elem("abcd", -2), "c")
})
test_that("str_paste_elems works", {
  expect_equal(str_paste_elems("abcdef", c(2, 5:6)), "bef")
})
