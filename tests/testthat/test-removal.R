context("Removal")
test_that("`str_remove_quoted()` works", {
  string <- "\"abc\"67a\'dk\'f"
  expect_equal(str_remove_quoted(string), "67af")
})
