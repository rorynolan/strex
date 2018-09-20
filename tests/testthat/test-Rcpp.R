context("C++ stuff")

test_that("paste_collapse_list_elems works", {
  expect_equal(
    paste_collapse_list_elems(list(1:3, c("a", 5, "rory")),
      collapse = "R"
    ),
    c("1R2R3", "aR5Rrory")
  )
})

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

test_that("Random Rcpp stuff works", {
  expect_equal(
    interleave_strings(c("a", "b", "v"), "a"),
    NA_character_
  )
  expect_equal(
    interleave_correctly_vec("a", character(0), "a"),
    "a"
  )
  expect_equal(
    interleave_correctly_vec("a", "a", character(0)),
    "a"
  )
  expect_equal(
    interleave_correctly_vec("ab", "b", "a"),
    c("a", "b")
  )
  expect_equal(
    interleave_correctly("a", list(), list()),
    list(NA_character_)
  )
  expect_equal(
    interleave_char_lists(list("a"), list()),
    list(NA_character_)
  )
  if (get_os() == "mac") {
    expect_error(
      str_elems(c("abc", "def"), 1:3),
      paste0("(",
             "`strings` and `locations` must have the same length.",
             "|",
             ore::ore.escape("c++ exception (unknown reason)"),
             ")")
    )
    expect_error(
      lst_df_pos_brace(list(1, 1:2, 1:3), list(")", c("[", "]"))),
      paste0("(",
             "`positions` and `braces` must have the same length.",
             "|",
             ore::ore.escape("c++ exception (unknown reason)"),
             ")")
    )
  } else {
    expect_error(
      str_elems(c("abc", "def"), 1:3),
      "`strings` and `locations` must have the same length."
    )
    expect_error(
      lst_df_pos_brace(list(1, 1:2, 1:3), list(")", c("[", "]"))),
      "`positions` and `braces` must have the same length."
    )
  }
})
