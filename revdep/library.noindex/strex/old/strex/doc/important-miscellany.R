## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load---------------------------------------------------------------------
library(strex)

## ----can-be-numeric-----------------------------------------------------------
str_can_be_numeric(c("1a", "abc", "5", "2e7", "seven"))

## ---- error=TRUE--------------------------------------------------------------
string <- c("Alan paid £5", "Joe paid $7")
str_first_currency(string)
string <- c("€1 is $1.17", "£1 is $1.29")
str_nth_currency(string, n = c(1, 2))
str_last_currency(string)  # only gets the first mentioned
str_extract_currencies(string)

## ----str-elem-----------------------------------------------------------------
string = "abcdefg"
str_sub(string, 3, 3)
str_elem(string, 3)  # simpler and more exressive

## ----extract-num-non-num------------------------------------------------------
string <- c("aa1bbb2ccc3", "xyz7ayc8jzk99elephant")
str_extract_numbers(string)
str_extract_non_numerics(string)

## ----split-by-numbers---------------------------------------------------------
string <- c("aa1bbb2ccc3", "xyz7ayc8jzk99elephant")
str_split_by_numbers(string)

## ----giv-ext------------------------------------------------------------------
string <- c("spreadsheet1.csv", "spreadsheet2")
str_give_ext(string, "csv")

## ----give-ext-replace---------------------------------------------------------
str_give_ext(string, "xls")  # append
str_give_ext(string, "csv", replace = TRUE)  # replace

## ----before-last-dot----------------------------------------------------------
string <- c("spreadsheet1.csv", "spreadsheet2")
str_before_last_dot(string)

## ----str-remove-quoted--------------------------------------------------------
string <- "I hate having these \"quotes\" in the middle of my strings."
cat(string)
str_remove_quoted(string)

## ----camel--------------------------------------------------------------------
string <- c("CamelVar1", c("CamelVar2"))
str_split_camel_case(string)

## ----to-vec-------------------------------------------------------------------
string <- "R is good."
str_to_vec(string)

## ----trim-anything------------------------------------------------------------
string <- "(((Why all the parentheses?)))"
string %>% 
  str_trim_anything(coll("("), side = "left") %>% 
  str_trim_anything(coll(")"), side = "r")

## ----singleize----------------------------------------------------------------
string <- c("I often write the word *my* twice in a row in my my sentences.")
str_singleize(string, " my")

