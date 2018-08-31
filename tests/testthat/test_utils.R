context("Utils")

test_that("all_equal works", {
  expect_true(all_equal(1, rep(1, 3)))
  expect_true(all_equal(rep(1, 3), 1))
  expect_false(all_equal(2, 1:3))
  expect_true(all_equal(1:4, 1:4))
  expect_false(all_equal(1:4, c(1, 2, 3, 3)))
  expect_true(all_equal(rep(1, 10)))
  expect_false(all_equal(c(1, 88)))
  expect_false(all_equal(character(0), NA))
  expect_false(all_equal(NA, character(0)))
  expect_false(all_equal(NULL, NA))
  expect_true(all_equal(matrix(1:4, nrow = 2), matrix(1:4, nrow = 2)))
  expect_false(all_equal(array(1, dim = c(2, 2, 2)), 99))
  expect_false(all_equal(99, array(1, dim = c(2, 2, 2))))
  expect_false(all_equal(
    array(1, dim = c(2, 2, 2)),
    array(1, dim = c(3, 3, 3))
  ))
  expect_false(all_equal(matrix(1:4, nrow = 2), 1:3))
  expect_false(all_equal(1:3, matrix(1:4, nrow = 2)))
})

test_that("group_close works", {
  expect_equal(group_close(1:10, 1), list(1:10))
  expect_equal(group_close(1:10, 0.5), as.list(1:10))
  expect_equal(
    group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3),
    list(c(1, 2, 4), c(10, 11, 14), 20, c(25, 27))
  )
  expect_error(group_close(integer(0)))
  expect_error(group_close(rep(1, 2)))
  expect_equal(group_close(0), list(0))
  expect_equal(group_close(c(0, 2)), list(0, 2))
})

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
  expect_equal(match_arg(NULL, choices), "Apples")
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
})

test_that("`*_list_nth_elems()` error correctly", {
  expect_error(
    str_list_nth_elems(list("a", "b"), 1:3),
    str_c(
      "If both `char_list` and `n` .* lengths greater than 1.*",
      "then.+their lengths must be equal.*",
      "Your `char_list` has length 2 and your `n` has length 3."
    )
  )
  expect_error(
    num_list_nth_elems(list(1, 2), 1:3),
    str_c(
      "If both `num_list` and `n` have lengths greater than 1.+",
      "then.+their lengths must be equal.+",
      "Your `num_list` has length 2 and your `n` has length 3."
    )
  )
})

test_that("`custom_stop()` errors correctly", {
  expect_error(
    custom_stop("a", 1),
    "The arguments in ... must all be of character type."
  )
})
