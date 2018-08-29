context("CamelCase")
test_that("str_split_camel_case()` works", {
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
