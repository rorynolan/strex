test_that("`chr_lst_nth_elems()` works", {
  expect_equal(
    chr_lst_nth_elems(list(c("a", "b"), c("x", "y", "z")), 2:3),
    c("b", "z")
  )
})

test_that("`chr_lst_remove_empties()` works", {
  expect_equal(
    chr_lst_remove_empties(
      list(
        c("", "a", "", "", "b", "c", ""),
        c("", ""), c("xy")
      )
    ),
    list(c("a", "b", "c"), character(0), "xy")
  )
})

test_that("`chr_vec_remove_empties()` works", {
  expect_equal(
    chr_vec_remove_empties(c("", "a", "", "", "b", "c", "")), c("a", "b", "c")
  )
})

test_that("`dbl_lst_nth_elems()` works", {
  expect_equal(
    dbl_lst_nth_elems(list(c(1.2, 2.3, 3.4), c(6.7, 8.9)), 2),
    c(2.3, 8.9)
  )
})

test_that("`int_lst_cbind()` works", {
  expect_equal(
    int_lst_cbind(list(1:4, 6:9, c(3L, 1L, 5L, 5L))),
    cbind(1:4, 6:9, c(3L, 1L, 5L, 5L))
  )
})

test_that("`int_lst_rbind()` works", {
  expect_equal(
    int_lst_rbind(list(1:4, 6:9, c(3L, 1L, 5L, 5L))),
    rbind(1:4, 6:9, c(3L, 1L, 5L, 5L))
  )
})

test_that("`int_mat_lst_cbind_nth_cols()` works", {
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    cbind(1:3, 5:7, NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, nrow = 3)
      ),
      2
    ),
    cbind(4:6, 5:7, 4:6 + 20L)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      3
    ),
    cbind(7:9, 8:10, NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    cbind(1:3, 4:6, NA_integer_)
  )
})

test_that("`int_mat_lst_cbind_nth_rows()` works", {
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    cbind(c(1L, 4L, 7L, 10L), c(3L, 6L, 9L, 12L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      2
    ),
    cbind(c(2L, 5L, 8L, 11L), c(3L, 6L, 9L, 12L), c(22L, 24L, 26L, 28L))
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      3
    ),
    cbind(c(3L, 6L, 9L, 12L), c(4L, 7L, 10L, 13L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    cbind(c(1L, 4L, 7L, 10L), c(2L, 5L, 8L, 11L), NA_integer_)
  )
})

test_that("`int_mat_lst_nth_cols()` works", {
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(3:4, 3:1)
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(6:4, 3:1)
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3)),
      c(2, -9)
    ),
    list(6:4, rep(NA_integer_, 3))
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3), matrix(9:1, nrow = 3)),
      c(2, -9)
    ),
    list(6:4, rep(NA_integer_, 3))
  )
})

test_that("`int_mat_lst_nth_rows()` works", {
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(c(2L, 4L), c(7L, 4L, 1L))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 9)
    ),
    list(c(2L, 4L), rep(NA_integer_, 3))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2)),
      c(2, 9)
    ),
    list(c(2L, 4L), rep(NA_integer_, 2))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)), 2
    ),
    list(c(2L, 4L), c(8L, 5L, 2L))
  )
})

test_that("`int_mat_lst_rbind_nth_cols()` works", {
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    rbind(1:3, 5:7, NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, nrow = 3)
      ),
      2
    ),
    rbind(4:6, 5:7, 4:6 + 20L)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      3
    ),
    rbind(7:9, 8:10, NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    rbind(1:3, 4:6, NA_integer_)
  )
})

test_that("`int_mat_lst_rbind_nth_rows()` works", {
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    rbind(c(1L, 4L, 7L, 10L), c(3L, 6L, 9L, 12L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      2
    ),
    rbind(c(2L, 5L, 8L, 11L), c(3L, 6L, 9L, 12L), c(22L, 24L, 26L, 28L))
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      3
    ),
    rbind(c(3L, 6L, 9L, 12L), c(4L, 7L, 10L, 13L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    rbind(c(1L, 4L, 7L, 10L), c(2L, 5L, 8L, 11L), NA_integer_)
  )
})

test_that("`interleave_chr_lsts()` works", {
  expect_equal(
    interleave_chr_lsts(
      list(c("a", "b"), c("a", "b", "c"), c("a", "b")),
      list(c("x", "y"), c("x", "y"), c("x", "y", "z"))
    ),
    list(
      c("a", "x", "b", "y"),
      c("a", "x", "b", "y", "c"),
      c("x", "a", "y", "b", "z")
    )
  )
})

test_that("`interleave_chr_vecs()` works", {
  expect_equal(
    interleave_chr_vecs(c("a", "b"), c("x", "y")),
    c("a", "x", "b", "y")
  )
  expect_equal(
    interleave_chr_vecs(c("a", "b", "c"), c("x", "y")),
    c("a", "x", "b", "y", "c")
  )
  expect_equal(
    interleave_chr_vecs(c("a", "b"), c("x", "y", "z")),
    c("x", "a", "y", "b", "z")
  )
  expect_error(
    interleave_chr_vecs(as.character(1:7), as.character(1:3)),
    str_c(
      "lengths.+differ by at most 1.+",
      "x.+length 7.+y.+length 3"
    )
  )
})

test_that("`lst_chr_to_dbl()` works", {
  expect_equal(
    lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")),
      commas = TRUE
    ),
    lapply(list(c("1", "2000"), c("1.3", "2.2", "5.9")), as.double)
  )
  expect_equal(
    lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")),
      commas = c(TRUE, FALSE)
    ),
    lapply(list(c("1", "2000"), c("1.3", "2.2", "5.9")), as.double)
  )
})

test_that("`lst_fullocate()` works", {
  int_mat_lst <- list(
    rbind(c(2L, 5L), 7:8),
    rbind(c(5L, 6L), c(7L, 10L), c(20L, 30L))
  )
  expect_equal(
    lst_fullocate(int_mat_lst, start = c(1, 5), end = c(10, 50)),
    list(
      rbind(1L, c(2L, 5L), 6L, 7:8, 9:10),
      rbind(5:6, c(7L, 10L), c(11L, 19L), c(20L, 30L), c(31L, 50L))
    )
  )
  expect_equal(
    lst_fullocate(int_mat_lst, start = c(1, 5), end = 50),
    list(
      rbind(1L, c(2L, 5L), 6L, 7:8, c(9L, 50L)),
      rbind(5:6, c(7L, 10L), c(11L, 19L), c(20L, 30L), c(31L, 50L))
    )
  )
})

test_that("`match_arg_index()` works", {
  expect_equal(match_arg_index("ab", c("book", "abacus", "pencil")), 2)
})

test_that("int_prlst_rbind() and int_prlst_cbind() work", {
  expect_equal(
    int_prlst_rbind(pairlist(1:2, 8:9, 5:4)),
    rbind(1:2, 8:9, 5:4)
  )
  expect_equal(
    int_prlst_cbind(pairlist(1:2, 8:9, 5:4)),
    cbind(1:2, 8:9, 5:4)
  )
})
