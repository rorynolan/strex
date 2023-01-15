test_that("str_before_last_dot works", {
  expect_equal(str_before_last_dot(c("spreadsheet1.csv", "doc2.doc")),
    c("spreadsheet1", "doc2"),
    ignore_attr = TRUE
  )
})

test_that("`str_before_nth()` works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(str_before_nth(string, "\\.", -3), "ab..cd..de.",
    ignore_attr = TRUE
  )
  expect_equal(str_before_nth(string, ".", -3), "ab..cd..de..fg",
    ignore_attr = TRUE
  )
  expect_equal(str_before_nth(rep(string, 2), fixed("."), -3),
    rep("ab..cd..de.", 2),
    ignore_attr = TRUE
  )
  expect_equal(str_before_last(rep(string, 2), fixed(".")),
    rep("ab..cd..de..fg.", 2),
    ignore_attr = TRUE
  )
  expect_equal(str_before_last(character(), 1:3), character())
  string <- "abxxcdxxdexxfgxxh"
  expect_equal(str_before_nth(string, "e", 1:2), c("abxxcdxxd", NA))
})
