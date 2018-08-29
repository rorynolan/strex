context("Trimming")
test_that("str_trim_anything works", {
  expect_equal(str_trim_anything("..abcd.", ".", "left"), "abcd.")
  expect_equal(str_trim_anything("-ghi--", "-"), "ghi")
  expect_equal(str_trim_anything("-ghi--", "--"), "-ghi")
  expect_equal(str_trim_anything("-ghi--", "--", "right"), "-ghi")
})
