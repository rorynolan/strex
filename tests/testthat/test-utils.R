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
