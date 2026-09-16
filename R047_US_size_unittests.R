library(testthat)
source("R047_PARL_functions.R")

test_that("US voting capacity increases at Tennessee statehood", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1795", as.Date("1795-03-04"), as.Date("1797-03-03"), "105;106")
  expect_equal(segs$size, c(105L, 106L))
  expect_equal(segs$seg_end[1], as.Date("1796-05-31"))
  expect_equal(segs$seg_start[2], as.Date("1796-06-01"))
  sizes <- vapply(as.Date(c("1796-05-31", "1796-06-01")), function(day) {
    parliament_size_for_snapshot("US_NT-HR_1795", as.Date("1795-03-04"),
      as.Date("1797-03-03"), "105;106", day)
  }, integer(1))
  expect_equal(sizes, c(105L, 106L))
})

test_that("US voting capacity increases only at Oklahoma statehood in 1907", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1907", as.Date("1907-03-04"), as.Date("1909-03-03"), "386;391")
  expect_equal(segs$size, c(386L, 391L))
  expect_equal(segs$seg_start, as.Date(c("1907-03-04", "1907-11-16")))
  expect_equal(segs$seg_end, as.Date(c("1907-11-15", "1909-03-03")))
})

test_that("nonvoting-only changes do not alter US voting capacity", {
  ids <- c("US_NT-HR_1793", "US_NT-HR_1871", "US_NT-HR_1899", "US_NT-HR_1905",
           "US_NT-HR_1935", "US_NT-HR_1945", "US_NT-HR_1971")
  sizes <- c(105L, 243L, 357L, 386L, 435L, 435L, 435L)
  for (i in seq_along(ids)) {
    year <- as.integer(substr(ids[i], 10, 13))
    segs <- parse_parliament_size_series(ids[i], as.Date(paste0(year, "-03-04")),
      as.Date(paste0(year + 2L, "-03-03")), as.character(sizes[i]))
    expect_equal(segs$size, sizes[i])
    expect_null(SIZE_CHANGE_DATES[[ids[i]]])
  }
})

test_that("old all-seat PARL sequences fail with a coordinated-import hint", {
  expect_error(parse_parliament_size_series("US_NT-HR_1945",
    as.Date("1945-01-03"), as.Date("1947-01-02"), "439;438"),
    "first import the coordinated voting-seat PARL corrections")
  expect_error(parse_parliament_size_series("DE_NT-BT_1987",
    as.Date("1987-02-18"), as.Date("1990-12-19"), "519;663"),
    "first import the coordinated voting-seat PARL corrections")
})

test_that("dashboard voting counts ignore a delegate-only Philippine transition", {
  env <- new.env(parent = environment())
  env$Sys.Date <- function() as.Date("1947-01-02")
  for (expr in parse("Dashboard/app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("build_daily_counts"))) eval(expr, env)
  }
  env$assembly_map <- c(US = "HR")
  env$RESE <- data.frame(
    country_abb = "US", political_function = c("NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_11"),
    start_date = as.Date("1945-01-03"), end_date = as.Date(c("1947-01-02", "1946-07-03")))
  env$PARL <- data.frame(
    parliament_id = "US_NT-HR_1945", country_abb = "US", level = "NT",
    assembly_abb = "HR", parliament_size = "435",
    leg_period_start_date = as.Date("1945-01-03"), leg_period_end_date = as.Date("1947-01-02"))
  daily <- env$build_daily_counts("US")
  boundary <- daily[daily$date %in% as.Date(c("1946-07-03", "1946-07-04")), ]
  expect_equal(boundary$parliament_size, c(435L, 435L))
  expect_equal(boundary$n_seated, c(1L, 1L))
})
