context("Strings")

test_that("can_be_numeric works", {
  expect_true(can_be_numeric("3"))
  expect_true(can_be_numeric("5 "))
  expect_equal(can_be_numeric(c("1a", "abc")), rep(FALSE, 2))
})

test_that("extract_currencies works", {
  expect_equal(
    extract_currencies("35.00 $1.14 abc5 $3.8 77")[c("curr_sym", "amount")],
    tibble::tibble(
      curr_sym = c("", "$", "c", "$", " "),
      amount = c(35, 1.14, 5, 3.8, 77)
    )
  )
  expect_equal(first_currency(c("ab3 13", "$1"))$curr_sym, c("b", "$"))
})

test_that("`nth_number_before_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4def5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(nth_number_before_mth(string, "def", 1, 1), c(1, 1))
  expect_equal(nth_number_before_mth(string, "abc", 2, 3), c(2, 2))
  expect_equal(nth_number_before_first(string, "def", 2), c(2, NA))
  expect_equal(nth_number_before_last(string, "def", -1), c(4, 7))
  expect_equal(first_number_before_mth(string, "abc", 2), c(1, 1))
  expect_equal(last_number_before_mth(string, "def", 1), c(4, 1))
  expect_equal(first_number_before_first(string, "def"), c(1, 1))
  expect_equal(first_number_before_last(string, "def"), c(1, 1))
  expect_equal(last_number_before_first(string, "def"), c(4, 1))
  expect_equal(last_number_before_last(string, "def"), c(4, 7))
})

test_that("singleize works", {
  expect_equal(singleize("abc//def", "/"), "abc/def")
  expect_equal(singleize("abababcabab", "ab"), "abcab")
  expect_equal(singleize(c("abab", "cdcd"), "cd"), c("abab", "cd"))
  expect_equal(
    singleize(c("abab", "cdcd"), c("ab", "cd")),
    c("ab", "cd")
  )
})

test_that("nice_nums works", {
  strings <- paste0("abc", 1:12)
  expect_equal(nice_nums(strings), paste0("abc", c(paste0(0, 1:9), 10:12)))
  expect_equal(
    nice_nums(c("01abc9def55", "5abc10def777", "99abc4def4")),
    c("01abc09def055", "05abc10def777", "99abc04def004")
  )
  expect_equal(
    nice_nums(c("abc9def55", "abc10def7")),
    c("abc09def55", "abc10def07")
  )
  expect_equal(
    nice_nums(c("abc9def55", "abc10def777", "abc4def4")),
    c("abc09def055", "abc10def777", "abc04def004")
  )
  expect_equal(nice_nums(1:10), c(paste0(0, 1:9), 10))
})

test_that("extract_numbers works", {
  expect_equal(
    extract_numbers(c("abc123abc456", "abc1.23abc456")),
    list(c(123, 456), c(1, 23, 456))
  )
  expect_equal(
    extract_numbers(c("abc1.23abc456", "abc1..23abc456"),
      decimals = TRUE, leading_decimals = FALSE,
    ),
    list(c(1.23, 456), c(1, 23, 456))
  )
  expect_equal(
    extract_numbers("abc1..23abc456",
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(1, 23, 456))
  )
  expect_equal(extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c(1, .23, 456)))
  expect_equal(extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE,
    leave_as_string = TRUE
  ), list((c("1", ".23", "456"))))
  expect_equal(extract_numbers("-123abc456"), list(c(123, 456)))
  expect_equal(extract_numbers("-123abc456", negs = TRUE), list(c(-123, 456)))
  expect_equal(extract_numbers("--123abc456", negs = TRUE), list(c(-123, 456)))
  expect_equal(extract_non_numerics("abc123abc456"), list(rep("abc", 2)))
  expect_equal(
    extract_non_numerics("abc1.23abc456"),
    list(c("abc", ".", "abc"))
  )
  expect_equal(
    extract_non_numerics("abc1.23abc456",
      decimals = TRUE, leading_decimals = TRUE
    ),
    list(c("abc", "abc"))
  )
  expect_equal(
    extract_non_numerics("abc1..23abc456",
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c("abc", "..", "abc"))
  )
  expect_equal(extract_non_numerics("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c("abc", ".", "abc")))
  expect_equal(
    extract_non_numerics(c("-123abc456", "ab1c")),
    list(c("-", "abc"), c("ab", "c"))
  )
  expect_equal(extract_non_numerics("-123abc456", negs = TRUE), list("abc"))
  expect_equal(
    extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(
    suppressWarnings(extract_numbers("abc1.2.3",
      decimals = TRUE, leading_decimals = TRUE
    )),
    list(NA_real_)
  )
  expect_equal(suppressWarnings(extract_numbers("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  )), list(NA_real_))
  expect_equal(
    suppressWarnings(extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
      decimals = TRUE, leading_decimals = TRUE
    )),
    c(as.list(rep(NA_real_, 3)), list(c(5, 6)))
  )
  expect_equal(nth_number("abc1.23abc456", 2), 23)
  expect_equal(first_number("abc1a2"), 1)
  expect_equal(last_number("akd50lkdjf0qukwjfj8"), 8)
  expect_equal(nth_number("abc1.23abc456", 2, leave_as_string = TRUE), "23")
  expect_equal(nth_number("abc1.23abc456", 2,
    decimals = TRUE, leading_decimals = TRUE
  ), 456)
  expect_equal(nth_number("-123abc456", -2, negs = TRUE), -123)
  expect_equal(
    extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(first_non_numeric("--123abc456"), "--")
  expect_equal(last_non_numeric("--123abc456"), "abc")
  expect_equal(nth_non_numeric("--123abc456", -2), "--")
  expect_error(extract_numbers("a.23", leading_decimals = TRUE))
  expect_error(extract_non_numerics("a.23", leading_decimals = TRUE))
  expect_equal(first_number("abc"), NA_integer_)
  expect_equal(first_non_numeric("1"), NA_character_)
  expect_equal(last_non_numeric(c("abc", "def")), c("abc", "def"))
  expect_equal(
    nth_non_numeric(c("ab12bd23", "wx56yz89"), c(3, -1)),
    c(NA, "yz")
  )
})

test_that("str_split_by_nums works", {
  expect_equal(
    str_split_by_nums(c("abc123def456.789gh", "a1b2c344")),
    list(
      c("abc", "123", "def", "456", ".", "789", "gh"),
      c("a", 1, "b", 2, "c", 344)
    )
  )
  expect_equal(
    str_split_by_nums("abc123def456.789gh",
      decimals = TRUE, leading_decimals = TRUE
    ),
    list(c("abc", "123", "def", "456.789", "gh"))
  )
  expect_equal(str_split_by_nums("22"), list("22"))
})

test_that("str_elem works", {
  expect_equal(str_elem("abcd", 3), "c")
  expect_equal(str_elem("abcd", -2), "c")
})

test_that("str_paste_elems works", {
  expect_equal(str_paste_elems("abcdef", c(2, 5:6)), "bef")
})

test_that("str_to_vec works", {
  expect_equal(str_to_vec("abcdef"), c("a", "b", "c", "d", "e", "f"))
})

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
  expect_equal(str_before_nth(string, "\\.", -3), "ab..cd..de.",
    check.attributes = FALSE
  )
  expect_equal(str_before_nth(string, ".", -3), "ab..cd..de..fg",
    check.attributes = FALSE
  )
  expect_equal(str_before_nth(rep(string, 2), fixed("."), -3),
    rep("ab..cd..de.", 2),
    check.attributes = FALSE
  )
  expect_equal(str_before_last(rep(string, 2), fixed(".")),
    rep("ab..cd..de..fg.", 2),
    check.attributes = FALSE
  )
})

test_that("before_last_dot works", {
  expect_equal(before_last_dot(c("spreadsheet1.csv", "doc2.doc")),
    c("spreadsheet1", "doc2"),
    check.attributes = FALSE
  )
})

test_that("extend_char_vec works", {
  expect_equal(extend_char_vec(1:5, extend_by = 2), c(1:5, "", ""))
  expect_equal(
    extend_char_vec(c("a", "b"), length_out = 10),
    c("a", "b", rep("", 8))
  )
  expect_error(extend_char_vec("0"))
  expect_error(extend_char_vec(c("0", 3)))
  expect_error(extend_char_vec(c("0", "1"), length_out = 1))
})

test_that("put_in_pos works", {
  expect_equal(put_in_pos(1:3, c(1, 8, 9)), c(1, rep("", 6), 2, 3))
  expect_equal(
    put_in_pos(c("Apple", "Orange", "County"), c(5, 7, 8)),
    c(rep("", 4), "Apple", "", "Orange", "County")
  )
  expect_equal(put_in_pos(1:2, 5), c(rep("", 4), 1:2))
})

test_that("trim_anything works", {
  expect_equal(trim_anything("..abcd.", ".", "left"), "abcd.")
  expect_equal(trim_anything("-ghi--", "-"), "ghi")
  expect_equal(trim_anything("-ghi--", "--"), "-ghi")
  expect_equal(trim_anything("-ghi--", "--", "right"), "-ghi")
})

test_that("locate_braces works", {
  skip_if_not_installed("dplyr")
  expect_equal(
    as.data.frame(locate_braces(c("a{](kkj)})", "ab(]c{}")),
      stringsAsFactors = FALSE
    )[c("position", "brace")],
    list(
      data.frame(
        position = as.integer(c(2, 3, 4, 8, 9, 10)),
        brace = c("{", "]", "(", ")", "}", ")"),
        stringsAsFactors = FALSE
      ),
      data.frame(
        position = as.integer(c(3, 4, 6, 7)),
        brace = c("(", "]", "{", "}"),
        stringsAsFactors = FALSE
      )
    ) %>%
      dplyr::bind_rows()
  )
})

test_that("remove_quoted works", {
  string <- "\"abc\"67a\'dk\'f"
  expect_equal(remove_quoted(string), "67af")
})

test_that("give_ext works", {
  expect_equal(give_ext("abc.csv", "csv"), "abc.csv")
  expect_equal(give_ext("abc", "csv"), "abc.csv")
  expect_equal(give_ext("abc.csv", "pdf"), "abc.csv.pdf")
  expect_equal(give_ext("abc.csv", "pdf", replace = TRUE), "abc.pdf")
})

test_that("str_split_camel_case works", {
  expect_equal(
    str_split_camel_case(c(
      "RoryNolan", "NaomiFlagg",
      "DepartmentOfSillyHats"
    )),
    list(
      c("Rory", "Nolan"), c("Naomi", "Flagg"),
      c("Department", "Of", "Silly", "Hats")
    )
  )
  expect_equal(
    str_split_camel_case(c(
      "RoryNolan", "NaomiFlagg",
      "DepartmentOfSillyHats"
    ),
    lower = TRUE
    ),
    list(
      c("Rory", "Nolan"), c("Naomi", "Flagg"),
      c("Department", "Of", "Silly", "Hats")
    ) %>%
      lapply(str_to_lower)
  )
})

test_that("str_locate_nth() works", {
  expect_equal(
    str_locate_nth("aba", "a", 9),
    matrix(NA_integer_, ncol = 2, nrow = 1) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_first(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(1, 3), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_last(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(5, 7, 4, 6), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
})

test_that("`nth_number_after_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4abc5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(nth_number_after_mth(string, "abc", 1, 3), c(3, 7))
  expect_equal(nth_number_after_mth(string, "abc", 2, 3), c(4, 8))
  expect_equal(nth_number_after_first(string, "abc", 2), c(2, 2))
  expect_equal(nth_number_after_last(string, "abc", -1), c(9, 9))
  expect_equal(first_number_after_mth(string, "abc", 2), c(2, 4))
  expect_equal(last_number_after_mth(string, "abc", 1), c(9, 9))
  expect_equal(first_number_after_first(string, "abc"), c(1, 1))
  expect_equal(first_number_after_last(string, "abc"), c(9, 7))
  expect_equal(last_number_after_first(string, "abc"), c(9, 9))
  expect_equal(last_number_after_last(string, "abc"), c(9, 9))
})
