test_that("match_arg() works", {
  expect_equal(match_arg("ab", c("abcdef", "defgh")), "abcdef")
  expect_error(match_arg("abcdefg", c("Abcdef", "defg")), "not a prefix of any")
  expect_equal(
    match_arg("ab", c("Abcdef", "defgh"), ignore_case = TRUE),
    "Abcdef"
  )
  expect_equal(match_arg("ab", c("xyz", "Abcdef", "defgh"),
    ignore_case = TRUE, index = TRUE
  ), 2)
  choices <- c("Apples", "Pears", "Bananas", "Oranges")
  expect_equal(match_arg("A", choices), "Apples")
  expect_equal(match_arg("B", choices, index = TRUE), 3)
  expect_equal(
    match_arg(c("b", "a"), choices,
      several_ok = TRUE,
      ignore_case = TRUE
    ),
    c("Bananas", "Apples")
  )
  expect_equal(
    match_arg(c("b", "a"), choices,
      ignore_case = TRUE, index = TRUE,
      several_ok = TRUE
    ),
    c(3, 1)
  )
  choices <- c(choices, "Avocados", "Apricots")
  expect_snapshot_error(match_arg("A", choices, ignore_case = FALSE))
  x <- "a"
  expect_snapshot_error(match_arg(x, choices, ignore_case = TRUE))
  expect_error(
    match_arg(c("A", "a"), choices),
    str_c(
      "`arg` must have length 1.+",
      ". Your `arg` has length 2.+",
      ". To use an `arg` with length greater than one, use.+",
      "`several_ok = TRUE`."
    )
  )
  choices <- c(choices, "bananas")
  expect_snapshot_error(match_arg("p", choices, ignore_case = TRUE))
  choices <- c(choices, "Pears")
  expect_snapshot_error(match_arg("p", choices, ignore_case = TRUE))
  expect_equal(match_arg("ab", c("ab", "abc")), "ab")
  y <- "a"
  expect_snapshot_error(match_arg(y, as.character(1:51)))
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(w)
  }
  expect_equal(word("b"), "baseball")
  expect_equal(word(), "abacus")
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(w, several_ok = TRUE)
  }
  expect_equal(word("c"), "candy")
  expect_equal(word(), c("abacus", "baseball", "candy"))
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(as.character(w), several_ok = TRUE)
  }
  expect_snapshot_error(word())
  word <- function(w = 1:3) {
    match_arg(w, several_ok = TRUE)
  }
  expect_snapshot_error(word())
  word <- function(w = c("abacus", "baseball", "candy")) {
    x <- "a"
    match_arg(x, several_ok = TRUE)
  }
  expect_snapshot_error(word())
})
