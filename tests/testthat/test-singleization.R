context("Singleization")
test_that("`str_singleize()` works", {
  expect_equal(str_singleize("abc//def", "/"), "abc/def")
  expect_equal(str_singleize("abababcabab", "ab"), "abcab")
  expect_equal(str_singleize(c("abab", "cdcd"), "cd"), c("abab", "cd"))
  expect_equal(
    str_singleize(c("abab", "cdcd"), c("ab", "cd")),
    c("ab", "cd")
  )
})
