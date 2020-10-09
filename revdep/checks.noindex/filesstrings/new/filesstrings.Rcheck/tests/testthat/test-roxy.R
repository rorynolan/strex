test_that("roxy_files_vignette() works", {
  expect_equal(
    roxy_files_vignette(),
    paste0(
      "[vignette]",
      "(https://cran.r-project.org/package=filesstrings/vignettes/files.html)."
    )
  )
})
