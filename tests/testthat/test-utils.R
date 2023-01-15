test_that("`*_list_nth_elems()` error correctly", {
  expect_error(
    chr_lst_nth_elems(list("a", "b"), 1:3),
    str_c(
      "If both.+chr_lst.+n.+lengths greater than 1.+",
      "then.+their lengths must be equal.+",
      "chr_lst.+length 2.+n.+length 3."
    )
  )
  expect_error(
    dbl_lst_nth_elems(list(1, 2), 1:3),
    str_c(
      "If both.+dbl_lst.+n.+lengths greater than 1.+",
      "then.+lengths must be equal.+",
      "dbl_lst.+length 2.+n.+length 3."
    )
  )
})

test_that("assert_lst_elems_common_length() works", {
  lst <- list(1)
  expect_true(assert_lst_elems_common_length(lst))
  lst <- list(1, 1:2)
  expect_error(
    assert_lst_elems_common_length(lst),
    "Elements.+do not have a common length"
  )
})

test_that("verify_string_pattern() edge cases are OK", {
  expect_true(verify_string_pattern("a", boundary()))
})
