# R047_PARL_unittests.R
# ------------------------------------------------------------
# Unit tests for:
# - preprocess_PARLdates()
# - check_anyNAinPARLdates()
# ------------------------------------------------------------

library(testthat)

# Make sure functions are available (adjust path if needed)
source("R047_PARL_functions.R")

# Tiny helper to build a PARL-like df
mk_parl <- function(start, end) {
  data.frame(
    leg_period_start = start,
    leg_period_end   = end,
    stringsAsFactors = FALSE
  )
}

test_that("preprocess_PARLdates adds parsed POSIXct columns", {
  df <- mk_parl(c("01Jan2020","15Feb2021"), c("31Dec2020","28Feb2021"))
  out <- preprocess_PARLdates(df)

  expect_true(all(c("leg_period_start_posoxctformat","leg_period_end_posoxctformat") %in% names(out)))
  expect_s3_class(out$leg_period_start_posoxctformat, "POSIXct")
  expect_s3_class(out$leg_period_end_posoxctformat,   "POSIXct")

  expect_false(any(is.na(out$leg_period_start_posoxctformat)))
  expect_false(any(is.na(out$leg_period_end_posoxctformat)))
  expect_false(check_anyNAinPARLdates(out))
})

test_that("preprocess_PARLdates strips [[lcen]]/[[rcen]] before parsing", {
  df <- mk_parl(c("[[lcen]]01Jan2020", "[[rcen]]15Feb2021"),
                c("31Dec2020[[rcen]]", "[[lcen]]28Feb2021"))
  out <- preprocess_PARLdates(df)

  expect_false(any(is.na(out$leg_period_start_posoxctformat)))
  expect_false(any(is.na(out$leg_period_end_posoxctformat)))

  expect_equal(
    as.Date(out$leg_period_start_posoxctformat),
    as.Date(c("2020-01-01","2021-02-15"))
  )
  expect_equal(
    as.Date(out$leg_period_end_posoxctformat),
    as.Date(c("2020-12-31","2021-02-28"))
  )
})

test_that("empty strings parse to NA and trigger a message", {
  # second row: blanks -> as.POSIXct("") is NA -> should message
  df <- mk_parl(c("01Jan2020", "   "), c("31Dec2020", ""))
  expect_message(
    out <- preprocess_PARLdates(df),
    regexp = "not all PARL dates parsed",
    fixed  = FALSE
  )
  expect_true(is.na(out$leg_period_start_posoxctformat[2]))
  expect_true(is.na(out$leg_period_end_posoxctformat[2]))
  expect_true(check_anyNAinPARLdates(out))
})

test_that("invalid date strings become NA and are detected", {
  df <- mk_parl(c("BADDATE", "15Feb2021"), c("31Dec2020", "NOTADATE"))
  out <- preprocess_PARLdates(df)

  expect_equal(sum(is.na(out$leg_period_start_posoxctformat)), 1L)
  expect_equal(sum(is.na(out$leg_period_end_posoxctformat)),   1L)
  expect_true(check_anyNAinPARLdates(out))
})

test_that("idempotent if run twice", {
  df <- mk_parl(c("[[lcen]]01Jan2020", "15Feb2021"),
                c("31Dec2020", "[[rcen]]28Feb2021"))
  out1 <- preprocess_PARLdates(df)
  out2 <- preprocess_PARLdates(out1)

  expect_equal(out1$leg_period_start_posoxctformat, out2$leg_period_start_posoxctformat)
  expect_equal(out1$leg_period_end_posoxctformat,   out2$leg_period_end_posoxctformat)
})

test_that("check_anyNAinPARLdates TRUE iff any parsed date is NA", {
  good <- preprocess_PARLdates(mk_parl(c("01Jan2020","02Jan2020"),
                                       c("03Jan2020","04Jan2020")))
  bad  <- preprocess_PARLdates(mk_parl(c("01Jan2020", NA),
                                       c("03Jan2020","04Jan2020")))

  expect_false(check_anyNAinPARLdates(good))
  expect_true(check_anyNAinPARLdates(bad))
})
