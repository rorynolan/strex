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
  expect_true(all_equal(list(1, 1)))
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
