context("After nth pattern")
test_that("str_after_nth works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(str_after_nth(string, "\\.\\.", 3), "fg..h",
    check.attributes = FALSE
  )
  expect_equal(str_after_first(string, "\\.\\."), "cd..de..fg..h",
    check.attributes = FALSE
  )
  expect_equal(str_after_last(string, "\\.\\."), "h",
    check.attributes = FALSE
  )
  expect_equal(str_before_first(string, "e"), "ab..cd..d",
    check.attributes = FALSE
  )
  expect_equal(str_before_nth(string, "\\.", -3), "ab..cd..de.",
    check.attributes = FALSE
  )
  expect_equal(str_before_nth(string, ".", -3), "ab..cd..de..fg",
    check.attributes = FALSE
  )
  expect_equal(str_before_nth(rep(string, 2), fixed("."), -3),
    rep("ab..cd..de.", 2),
    check.attributes = FALSE
  )
  expect_equal(str_before_last(rep(string, 2), fixed(".")),
    rep("ab..cd..de..fg.", 2),
    check.attributes = FALSE
  )
})
