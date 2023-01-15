test_that("str_after_nth works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(str_after_nth(string, "\\.\\.", 3), "fg..h",
    ignore_attr = TRUE
  )
  expect_equal(str_after_first(string, "\\.\\."), "cd..de..fg..h",
    ignore_attr = TRUE
  )
  expect_equal(str_after_last(string, "\\.\\."), "h",
    ignore_attr = TRUE
  )
  expect_equal(str_before_first(string, "e"), "ab..cd..d",
    ignore_attr = TRUE
  )
  string <- c("abc", "xyz.zyx")
  expect_equal(str_after_first(string, "."), str_sub(string, 2))
  expect_equal(str_after_first(string, coll(".")), c(NA, "zyx"))
  expect_equal(str_after_first(character(), 1:3), character())
  expect_equal(str_after_nth("abc", "b", c(0, 1)), c(NA, "c"))
  string <- "abxxcdxxdexxfgxxh"
  expect_equal(str_after_nth(string, "e", 1:2), c("xxfgxxh", NA))
  expect_snapshot_error(str_after_nth(c("a"), c("a", "b"), 1:3))
})
