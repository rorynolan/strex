## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load---------------------------------------------------------------------
library(strex)

## ----before-------------------------------------------------------------------
string <- "ab..cd..de..fg..h"
str_before_first(string, "e")
str_before_nth(string, "\\.", 3)
str_before_last(string, "\\.")
str_before_nth(string, ".", -3)
str_before_nth(rep(string, 2), fixed("."), -3)

## ----after--------------------------------------------------------------------
string <- "ab..cd..de..fg..h"
str_after_first(string, "e")
str_after_nth(string, "\\.", 3)
str_after_last(string, "\\.")
str_after_nth(string, ".", -3)
str_after_nth(rep(string, 2), fixed("."), -3)

## ----james-harry--------------------------------------------------------------
string <- "James did the cooking, Harry did the cleaning."

## ----get-task-----------------------------------------------------------------
library(magrittr)
get_task <- function(string, name) {
  str_c(name, " did the ") %>% 
    str_after_first(string, .) %>% 
    str_before_first("[\\.,]")
}
get_task(string, "James")
get_task(string, "Harry")

