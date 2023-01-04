test_that("`str_detect_all()` works", {
  expect_equal(str_detect_all("quick brown fox", c("x", "y", "z")), FALSE)
  expect_equal(str_detect_all(c(".", "-"), "."), c(TRUE, TRUE))
  expect_equal(str_detect_all(c(".", "-"), coll(".")), c(TRUE, FALSE))
  expect_equal(str_detect_all(c(".", "-"), coll("."), negate = TRUE),
               c(FALSE, TRUE))
  expect_equal(str_detect_all(c(".", "-"), fixed("."), negate = TRUE),
               c(FALSE, TRUE))
  expect_equal(str_detect_all(c(".", "-"), c(".", ":")), c(FALSE, FALSE))
  expect_equal(str_detect_all(c(".", "-"), coll(c(".", ":"))), c(FALSE, FALSE))
  expect_equal(str_detect_any("quick brown fox", c("x", "y", "z")), TRUE)
  expect_equal(str_detect_any(c(".", "-"), "."), c(TRUE, TRUE))
  expect_equal(str_detect_any(c(".", "-"), coll(".")), c(TRUE, FALSE))
  expect_equal(str_detect_any(c(".", "-"), fixed(".")), c(TRUE, FALSE))
  expect_equal(str_detect_any(c(".", "-"), coll("."), negate = TRUE),
               c(FALSE, TRUE))
  expect_equal(str_detect_any(c(".", "-"), c(".", ":")), c(TRUE, TRUE))
  expect_equal(str_detect_any(c(".", "-"), coll(c(".", ":"))), c(TRUE, FALSE))
  expect_error(str_detect_all("quick brown fox", boundary()),
               "cannot handle.+pattern.+of type.+boundary")
  expect_error(str_detect_any("quick brown fox", boundary()),
               "cannot handle.+pattern.+of type.+boundary")
  expect_equal(str_detect_any(c("xyzabc", "abcxyz"), c(".b", "^x")),
               c(TRUE, TRUE))
  expect_equal(str_detect_all(c("xyzabc", "abcxyz"), c(".b", "^x")),
               c(TRUE, FALSE))
  expect_equal(str_detect_all("xyzabc", c("a", "c", "z")), TRUE)
})
