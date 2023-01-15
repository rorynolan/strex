test_that("`str_extract_currencies()` works", {
  string <- "35.00 $1.14 abc5 $3.8 77"
  expect_equal(
    str_extract_currencies(string),
    data.frame(
      string_num = 1, string = string,
      curr_sym = c("", "$", "c", "$", " "),
      amount = c(35, 1.14, 5, 3.8, 77),
      stringsAsFactors = FALSE
    )
  )
  string <- c(
    "35.00 $1.14", "abc5 $3.8 77", "-$1.5e6",
    "over £1,000"
  )
  reps <- c(2, 3, 1, 1)
  expect_equal(
    str_extract_currencies(string),
    data.frame(
      string_num = rep(seq_along(string), reps),
      string = rep(string, reps),
      curr_sym = c("", "$", "c", "$", " ", "$", "£"),
      amount = c(35, 1.14, 5, 3.8, 77, -1.5e6, 1000),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_extract_currencies(character())),
    data.frame(
      string_num = integer(), string = character(),
      curr_sym = character(), amount = numeric(),
      stringsAsFactors = FALSE
    )
  )
})
test_that("`str_nth_currency()` works", {
  string <- c("ab3 13", "$1")
  expect_equal(
    str_nth_currency(string, n = 2),
    data.frame(
      string_num = seq_along(string), string = string,
      curr_sym = c(" ", NA), amount = c(13, NA),
      stringsAsFactors = FALSE
    )
  )
  string <- c("35.00 $1.14", "abc5 $3.8", "stuff")
  expect_equal(str_nth_currency(string, c(
    1,
    2, 1
  )), data.frame(
    string_num = seq_along(string), string = string,
    curr_sym = c("", "$", NA), amount = c(35, 3.8, NA),
    stringsAsFactors = FALSE
  ))
  string <- c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff")
  expect_equal(
    str_nth_currency(string, n = 2),
    data.frame(
      string_num = 1:5,
      string = c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff"),
      curr_sym = c(" ", NA, "$", "$", NA),
      amount = c(13, NA, 1.14, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    str_nth_currency(string, c(1, 2, 1, 2, 1)),
    data.frame(
      string_num = 1:5,
      string = c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff"),
      curr_sym = c("b", NA, "", "$", NA),
      amount = c(3, NA, 35, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    str_first_currency(string),
    data.frame(
      string_num = 1:5,
      string = c(
      "ab3 13", "$1", "35.00 $1.14",
      "abc5 $3.8", "stuff"
    ),
    curr_sym = c("b", "$", "", "c", NA),
    amount = c(
      3,
      1, 35, 5, NA
    ), stringsAsFactors = FALSE)
  )
  expect_equal(
    str_last_currency(string),
    data.frame(
      string_num = 1:5, string = c(
        "ab3 13", "$1", "35.00 $1.14",
        "abc5 $3.8", "stuff"
      ), curr_sym = c(" ", "$", "$", "$", NA),
      amount = c(13, 1, 1.14, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_snapshot_error(str_nth_currency(as.character(1:3), 1:7))
  expect_equal(as.data.frame(str_nth_currency(string, n = -2)),
    data.frame(
      string_num = seq_along(string), string,
      curr_sym = c("b", NA, "", "c", NA),
      amount = c(3, NA, 35, 5, NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(str_nth_currency(string, c(1, -2, 1, 2, -1))),
    data.frame(
      string_num = seq_along(string), string,
      curr_sym = c("b", NA, "", "$", NA),
      amount = c(3, NA, 35, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_nth_currency(character(), 1)),
    data.frame(
      string_num = integer(), string = character(),
      curr_sym = character(), amount = numeric(),
      stringsAsFactors = FALSE
    )
  )
})
