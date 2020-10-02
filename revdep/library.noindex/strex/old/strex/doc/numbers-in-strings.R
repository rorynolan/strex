## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = ifelse(utils::packageVersion("knitr") >= "1.20.15", "styler", TRUE)
)
library(stringr)

## ----make-img_names, echo=FALSE-----------------------------------------------
img_names <- expand.grid(1:2, 1:3, c(0, 2.5)) %>%
  apply(1, function(x) {
    str_c("patient", x[1], "-cell", x[2], "-", x[3], "hours-after-biopsy.tif")
  }) %>%
  sort()

## ----print-img_names----------------------------------------------------------
img_names

## ----extract-all-numbers------------------------------------------------------
library(strex)
str_extract_numbers(img_names)

## ----extract-all-numbers-use-decimals-----------------------------------------
str_extract_numbers(img_names, decimals = TRUE)

## ----extract-non-numbers------------------------------------------------------
str_extract_non_numerics(img_names, decimals = TRUE)

## ----nth-number-n2------------------------------------------------------------
str_nth_number(img_names, n = 2)

## ----nth-number-after-mth-----------------------------------------------------
str_nth_number_after_mth(img_names, "cell", n = 1, m = 1)

## ----first-number-after-first-------------------------------------------------
str_first_number_after_first(img_names, "cell")

## ----las-number-before-first--------------------------------------------------
str_last_number_before_first(img_names, "biopsy", decimals = TRUE)

## ----dataframe----------------------------------------------------------------
data.frame(img_names,
  patient = str_first_number_after_first(img_names, "patient"),
  cell = str_first_number_after_first(img_names, "cell"),
  hrs_after_biop = str_last_number_before_first(img_names, "biop", 
                                                decimals = TRUE)
)

## ----scicom-------------------------------------------------------------------
string <- c("$1,000", "$1e6")
str_first_number(string, commas = TRUE, sci = TRUE)

## ----all-number-functions-----------------------------------------------------
str_subset(ls("package:strex"), "number")

## ----regex--------------------------------------------------------------------
data.frame(img_names,
  patient = str_match(img_names, "patient(\\d+)")[, 2],
  cell = str_match(img_names, "cell(\\d+)")[, 2],
  hrs_after_biop = str_match(img_names, "(\\d*\\.*\\d+)hour")[, 2]
)

