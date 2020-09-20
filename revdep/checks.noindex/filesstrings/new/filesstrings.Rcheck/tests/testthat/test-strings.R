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
