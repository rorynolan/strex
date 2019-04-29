test_that("str_elem() works", {
  expect_equal(str_elem(c("abcd", "xyz"), 3), c("c", "z"))
  expect_equal(str_elem("abcd", -2), "c")
  expect_equal(str_elem("abcd", 3), "c")
})

test_that("str_elems() works", {
  string <- c("abc", "def", "ghi", "vwxyz")
  ans <- matrix(c(
    "a", "b",
    "d", "e",
    "g", "h",
    "v", "w"
  ),
  ncol = 2, byrow = TRUE
  )
  expect_equal(str_elems(string, 1:2), ans)
  expect_equal(str_elems(string, 1:2, byrow = FALSE), t(ans))
  expect_equal(
    str_elems(string, c(1, 2, 3, 4, -1)),
    matrix(c(
      "a", "b", "c", "", "c",
      "d", "e", "f", "", "f",
      "g", "h", "i", "", "i",
      "v", "w", "x", "y", "z"
    ),
    nrow = length(string), byrow = TRUE
    )
  )
})

test_that("str_paste_elems() works", {
  string <- c("abc", "def", "ghi", "vwxyz")
  expect_equal(str_paste_elems(string, 1:2), c("ab", "de", "gh", "vw"))
  expect_equal(str_paste_elems(string, c(1, 2, 3, 4, -1)),
               c("abcc", "deff", "ghii", "vwxyz"))
  expect_equal(str_paste_elems("abc", c(1, 5, 55, 43, 3)), "ac")
})
