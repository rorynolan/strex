context("File extensions")
test_that("`str_give_ext()` works", {
  expect_equal(str_give_ext("abc.csv", "csv"), "abc.csv")
  expect_equal(str_give_ext("abc", "csv"), "abc.csv")
  expect_equal(str_give_ext("abc.csv", "pdf"), "abc.csv.pdf")
  expect_equal(str_give_ext("abc.csv", "pdf", replace = TRUE), "abc.pdf")
})
