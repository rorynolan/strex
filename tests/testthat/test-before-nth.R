context("Before pattern")
test_that("str_before_last_dot works", {
  expect_equal(str_before_last_dot(c("spreadsheet1.csv", "doc2.doc")),
    c("spreadsheet1", "doc2"),
    check.attributes = FALSE
  )
})
