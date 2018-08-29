context("Currency")
test_that("`str_get_currencies()` works", {
  expect_equal(
    str_get_currencies("35.00 $1.14 abc5 $3.8 77"),
    tibble::tibble(
      currency = c("", "$", "c", "$", " "),
      amount = c(35, 1.14, 5, 3.8, 77)
    )
  )
})
test_that("`str_get_currency()` works", {
  expect_equal(str_get_currency(c("ab3 13", "$1")), c("b", "$"))
})
