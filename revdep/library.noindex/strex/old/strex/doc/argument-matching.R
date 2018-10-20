## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----base-ignore-case, error=TRUE----------------------------------------
base::match.arg("Y", c("yes", "no"))

## ----strex-ignore-case, error=TRUE---------------------------------------
strex::match_arg("Y", c("yes", "no"))
strex::match_arg("Y", c("yes", "no"), ignore_case = TRUE)

## ----no-matches, error=TRUE----------------------------------------------
choices <- c("Apples", "Pears", "Bananas", "Oranges", "Avocados", "Apricots")
match.arg("Q", choices)
strex::match_arg("Q", choices)

## ----multiple-matches, error=TRUE----------------------------------------
match.arg("A", choices)
strex::match_arg("A", choices)

## ----arg-too-long, error=TRUE--------------------------------------------
match.arg(c("A", "a"), choices)
strex::match_arg(c("A", "a"), choices)

## ----choices-duplicate, error=TRUE---------------------------------------
choices <- c(choices, "Pears")
match.arg("P", choices)
strex::match_arg("P", choices)

