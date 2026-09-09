library(testthat)
source("R047_PARL_functions.R")

# Explicit-registry fixture: verifies date handling independently of the
# production registry. Reviewed production dates are covered separately.
test_that("US House size changes at midnight without overlapping segments", {
  registry <- list("US_NT-HR_1945" = as.Date("1946-07-04"))
  segs <- parse_parliament_size_series(
    "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
    "439;438", registry = registry)
  expect_equal(segs$size, c(439L, 438L))
  expect_equal(segs$seg_start, as.Date(c("1945-01-03", "1946-07-04")))
  expect_equal(segs$seg_end, as.Date(c("1946-07-03", "1947-01-02")))
  days <- as.Date(c("1946-07-03", "1946-07-04", "1946-07-05"))
  sizes <- vapply(days, function(day) {
    selected <- segs[day >= segs$seg_start & day <= segs$seg_end, ]
    expect_equal(nrow(selected), 1L)
    selected$size
  }, integer(1))
  expect_equal(sizes, c(439L, 438L, 438L))
})

test_that("US size expansion stops when transition dates are unknown or empty", {
  for (registry in list(list(), list("US_NT-HR_1945" = as.Date(character())),
                       list("US_NT-HR_1945" = as.Date(NA)))) {
    expect_error(parse_parliament_size_series(
      "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
      "439;438", registry = registry), "changes mid-term")
  }
})

test_that("US cohort snapshots use the correct side of a size transition", {
  registry <- list("US_NT-HR_1945" = as.Date("1946-07-04"))
  sizes <- vapply(as.Date(c("1946-07-03", "1946-07-04")), function(day) {
    parliament_size_for_snapshot(
      "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
      "439;438", day, registry = registry)
  }, integer(1))
  expect_equal(sizes, c(439L, 438L))
})

test_that("dashboard daily counts expand the US registered size decrease", {
  env <- new.env(parent = environment())
  sys.source("R047_PARL_functions.R", envir = env)
  env$SIZE_CHANGE_DATES[["US_NT-HR_1945"]] <- as.Date("1946-07-04")
  env$Sys.Date <- function() as.Date("1947-01-02")
  for (expr in parse("Dashboard/app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("build_daily_counts"))) eval(expr, env)
  }
  env$assembly_map <- c(US = "HR")
  env$RESE <- data.frame(
    country_abb = "US", political_function = "NT_LE-LH_T3_NA_11",
    start_date = as.Date("1945-01-03"), end_date = as.Date("1946-07-03"))
  env$PARL <- data.frame(
    parliament_id = "US_NT-HR_1945", country_abb = "US", level = "NT",
    assembly_abb = "HR", parliament_size = "439;438",
    leg_period_start_date = as.Date("1945-01-03"),
    leg_period_end_date = as.Date("1947-01-02"))
  daily <- env$build_daily_counts("US")
  boundary <- daily[daily$date %in% as.Date(c("1946-07-03", "1946-07-04")), ]
  expect_equal(boundary$parliament_size, c(439L, 438L))
  expect_equal(boundary$n_seated, c(1L, 0L))
})

# Reviewed production boundaries: the default registry must support the actual
# exported sequences without any caller-supplied replacement registry.
test_that("production US registry resolves Philippine independence", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"), "439;438")
  expect_equal(segs$size, c(439L, 438L))
  expect_equal(segs$seg_end[1L], as.Date("1946-07-03"))
  expect_equal(segs$seg_start[2L], as.Date("1946-07-04"))
})

test_that("production US registry handles both lasting changes in the 60th Congress", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1907", as.Date("1907-03-04"), as.Date("1909-03-03"), "392;396;398")
  expect_equal(segs$size, c(392L, 396L, 398L))
  expect_equal(segs$seg_start, as.Date(c("1907-03-04", "1907-11-16", "1908-02-04")))
  expect_equal(segs$seg_end, as.Date(c("1907-11-15", "1908-02-03", "1909-03-03")))
})

test_that("the 1795 US one-for-one status conversion keeps a constant total", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1795", as.Date("1795-03-04"), as.Date("1797-03-03"), "106")
  expect_equal(nrow(segs), 1L)
  expect_equal(segs$size, 106L)
  expect_null(SIZE_CHANGE_DATES[["US_NT-HR_1795"]])
})
