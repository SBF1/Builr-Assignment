library(testthat)

# function to test if make_file returns the right name for an input file name
test_that("make_filename_test", {
  generatedName <- make_filename(2017)
  expect_that(generatedName, is_equivalent_to("accident_2017.csv.bz2"))
})

