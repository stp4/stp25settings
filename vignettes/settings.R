## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rmarkdown)

## ----use-set_opt--------------------------------------------------------------
require(stp25settings)
 set_opt(
   p = list(digits = 2, mark.sig = TRUE),
   prozent = list(digits = 0, null_percent_sign =  ' . ')
 )
 get_opt("prozent")
 
 get_opt()$p$digits


