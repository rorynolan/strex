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
  choices %<>% c("Avocados", "Apricots")
  expect_error(
    match_arg("A", choices, ignore_case = FALSE),
    str_c(
      "`arg` must be a prefix.+of exactly one element of `choices`.+",
      ". Your `arg` \\\"A\\\" is a prefix of two or more elements.+",
      "`choices`.+",
      ". The first two of these are \\\"Apples\\\" and \\\"Avocados\\\"."
    )
  )
  expect_error(
    match_arg("a", choices, ignore_case = TRUE),
    str_c(
      "`arg` must be a prefix of exactly one element of `choices`.+",
      ". Your `arg` \\\"a\\\" is a prefix of two or more elements of.+",
      "`choices`.+",
      ". The first two of these are \\\"Apples\\\" and \\\"Avocados\\\"."
    )
  )
  expect_error(
    match_arg(c("A", "a"), choices),
    str_c(
      "`arg` must have length 1.+",
      ". Your `arg` has length 2.+",
      ". To use an `arg` with length greater than one, use.+",
      "`several_ok = TRUE`."
    )
  )
  choices %<>% c("bananas")
  expect_error(
    match_arg("p", choices, ignore_case = TRUE),
    str_c(
      "`choices` must not have duplicate elements.+",
      ". Since you have set `ignore_case = TRUE`, elements 3 and.+",
      "7 of your `choices` \\(\\\"Bananas\\\" and \\\"bananas\\\"\\) are.+",
      "effectively duplicates."
    )
  )
  choices %<>% c("Pears")
  expect_error(
    match_arg("p", choices, ignore_case = TRUE),
    str_c(
      "`choices` must not have duplicate elements.+",
      ". Element 8, of your `choices` \\(\\\"Pears\\\"\\) is a duplicate."
    )
  )
  expect_equal(match_arg("ab", c("ab", "abc")), "ab")
  expect_error(
    match_arg("a", as.character(1:51)),
    str_c(
      "`arg` must be a prefix of exactly one element of `choices`.+",
      ". Your first 50 `choices` are \\\"1\\\", \\\"2\\\", \\\"3\\\", .+",
      "47\\\", \\\"48\\\", \\\"49\\\" and \\\"50\\\".+",
      ". Your `arg` \\\"a\\\" is not a prefix of any of your `choices`."
    )
  )
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
  word_err_msg <- paste(
    "You have used `match_arg()` without specifying a",
    "`choices`\nargument.\n    * The only way to do this",
    "is from another function where\n      `arg` has a",
    "default setting. This is the same as\n     ",
    "`base::match.arg()`.\n    * See the man page for",
    "`match_arg()`, particularly the\n      examples:",
    "enter `help(\"match_arg\", package = \"strex\")`\n   ",
    "  at the R console.\n    * See also the vignette on",
    "argument matching: enter\n     ",
    "`vignette(\"argument-matching\", package =",
    "\"strex\")` at\n      the R console."
  )
  expect_error(word(), word_err_msg, fixed = TRUE)
  word <- function(w = 1:3) {
    match_arg(w, several_ok = TRUE)
  }
  expect_error(word(), word_err_msg, fixed = TRUE)
  word <- function(w = c("abacus", "baseball", "candy")) {
    x <- "a"
    match_arg(x, several_ok = TRUE)
  }
  expect_error(word(), word_err_msg, fixed = TRUE)
})
