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
  string <- c("abc", "xyz.zyx")
  expect_equal(str_after_first(string, "."), str_sub(string, 2))
  expect_equal(str_after_first(string, coll(".")), c(NA, "zyx"))
  expect_equal(str_after_first(character(), 1:3), character())
  expect_equal(str_after_nth("abc", "b", c(0, 1)), c(NA, "c"))
  string <- "abxxcdxxdexxfgxxh"
  expect_equal(str_after_nth(string, "e", 1:2), c("xxfgxxh", NA))
  expect_error(str_after_nth(c("a"), c("a", "b"), 1:3),
    paste0(
      "If `pattern` and `n` both have length greater than",
      " 1, their\nlengths must be equal.\n    * Your `patte",
      "rn` has length 2.\n    * Your `n` has length 3."
    ),
    fixed = TRUE
  )
})
