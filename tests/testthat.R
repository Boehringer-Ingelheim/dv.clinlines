pkg_name <- "dv.clinlines"

library(testthat)
library(pkg_name, character.only = TRUE)

test_check(pkg_name)
