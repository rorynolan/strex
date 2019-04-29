test_that("str_list_remove_empties works", {
  expect_equal(
    str_list_remove_empties(list(
      c("a", "", "b"), "gg",
      c("", 1, "")
    )),
    list(c("a", "b"), "gg", "1")
  )
})

test_that("str_list_nth_elems works", {
  expect_equal(
    str_list_nth_elems(list(
      c("a", "b", "c"),
      c("d", "f", "a")
    ), 2),
    c("b", "f")
  )
  expect_error(
    str_list_nth_elems(
      list(
        c("a", "b", "c"),
        c("d", "f", "a")
      ),
      2:5
    ),
    str_c(
      "If both `char_list` and `n` have lengths greater than 1,",
      " then.+their lengths must be equal."
    )
  )
  expect_equal(str_list_nth_elems_helper(list("abc"), -7), NA_character_)
})

test_that("num_list_nth_elems works", {
  expect_equal(num_list_nth_elems(list(1:5, 0:2), 4), c(4, NA))
  expect_equal(num_list_nth_elems(list(1:5, 0:2), 4:5), c(4, NA))
  expect_error(
    num_list_nth_elems(list(1:3, 7:8), 2:5),
    str_c(
      "If both `num_list` and `n` have lengths greater than 1,",
      " then.+their lengths must be equal."
    )
  )
})

test_that("char_to_num() errors correctly", {
  skip_on_cran()
  expect_error(char_to_num("1,000a", TRUE),
               "Could not convert '1,000a' to numeric.", fixed = TRUE,
               class = "std::invalid_argument")
  expect_error(char_to_num("a1,000", TRUE),
               "Could not convert 'a1,000' to numeric.", fixed = TRUE,
               class = "std::invalid_argument")
  expect_error(char_to_num("1000a", FALSE),
               "Could not convert '1000a' to numeric.", fixed = TRUE,
               class = "std::invalid_argument")
  expect_error(char_to_num("", FALSE),
               "Empty string passed to `char_to_num()`.", fixed = TRUE,
               class = "std::invalid_argument")
})

test_that("interleave_strings() works", {
  expect_equal(interleave_strings("a", as.character(1:3)), NA_character_)
  expect_equal(interleave_strings(as.character(1:3), as.character(1:4)),
               interleave_strings(as.character(1:4), as.character(1:3)))
})

test_that("interleave_char_lists() works", {
  skip_on_cran()
  expect_error(interleave_char_lists(as.list(1:2), list(1)),
               paste("`interleave_char_lists()` expects two lists of",
                     "the same length. You have passed arguments of",
                     "length 2 and 1."),
               fixed = TRUE, class = "std::invalid_argument")
})
