library(testthat)
source("R047_PARL_functions.R")

test_that("the US Senate registry includes Hawaii from its admission day", {
  segs <- parse_parliament_size_series(
    "US_NT-SE_1959", as.Date("1959-01-03"), as.Date("1961-01-02"), "98;100")
  expect_identical(segs$size, c(98L, 100L))
  expect_identical(segs$seg_start, as.Date(c("1959-01-03", "1959-08-21")))
  expect_identical(segs$seg_end, as.Date(c("1959-08-20", "1961-01-02")))
  expect_identical(segs$seg_end[1] + 1L, segs$seg_start[2])
  sizes <- vapply(as.Date(c("1959-01-03", "1959-08-20", "1959-08-21", "1961-01-02")), function(day) {
    parliament_size_for_snapshot(
      "US_NT-SE_1959", as.Date("1959-01-03"), as.Date("1961-01-02"), "98;100", day)
  }, integer(1))
  expect_identical(sizes, c(98L, 98L, 100L, 100L))
})
