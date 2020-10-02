## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(1)
library(magrittr)
poorly_ordered <- expand.grid(98:103, 9:11) %>% 
  apply(1, function(x) paste0("patient", x[1], "-day", x[2], ".png")) %>% 
  sample(size = length(.))

## ----poorly-ordered-----------------------------------------------------------
poorly_ordered

## ----sort-attempt-------------------------------------------------------------
sort(poorly_ordered)

## ----alphordering-------------------------------------------------------------
strex::str_alphord_nums(poorly_ordered)

## ----good-sort----------------------------------------------------------------
sort(strex::str_alphord_nums(poorly_ordered))

