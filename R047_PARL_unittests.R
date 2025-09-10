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
mk_parl <- function(start, end, level = NULL, parliament_size = NULL) {
  df <- data.frame(
    leg_period_start = start,
    leg_period_end   = end,
    stringsAsFactors = FALSE
  )
  if (!is.null(level)) {
    df$level <- level
  }
  if (!is.null(parliament_size)) {
    df$parliament_size <- parliament_size
  }
  df
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

test_that("check_anyNAinPARLdates filters by level parameter", {
  # Mixed data: NT level has good dates, regional level has bad dates
  df <- mk_parl(c("01Jan2020", "BADDATE", "15Feb2021"), 
                c("31Dec2020", "BADDATE", "28Feb2021"),
                level = c("NT", "REGIONAL", "NT"))
  processed <- preprocess_PARLdates(df)
  
  # Without level filter: should detect NA (from REGIONAL level)
  expect_true(check_anyNAinPARLdates(processed))
  
  # With NT level filter: should be clean
  expect_false(check_anyNAinPARLdates(processed, level = "NT"))
  
  # With REGIONAL level filter: should detect NA
  expect_true(check_anyNAinPARLdates(processed, level = "REGIONAL"))
})

test_that("check_anyNAinPARLdates_details filters by level parameter", {
  # Mixed data: NT level has good dates, regional level has bad dates
  df <- mk_parl(c("01Jan2020", "BADDATE", "15Feb2021"), 
                c("31Dec2020", "BADDATE", "28Feb2021"),
                level = c("NT", "REGIONAL", "NT"))
  processed <- preprocess_PARLdates(df)
  
  # Without level filter: should find NAs
  all_details <- check_anyNAinPARLdates_details(processed)
  expect_false(all_details$check_passed)
  expect_equal(all_details$na_start_count, 1)
  expect_equal(all_details$na_end_count, 1)
  
  # With NT level filter: should pass
  nt_details <- check_anyNAinPARLdates_details(processed, level = "NT")
  expect_true(nt_details$check_passed)
  expect_equal(nt_details$na_start_count, 0)
  expect_equal(nt_details$na_end_count, 0)
  
  # With REGIONAL level filter: should fail
  reg_details <- check_anyNAinPARLdates_details(processed, level = "REGIONAL")
  expect_false(reg_details$check_passed)
  expect_equal(reg_details$na_start_count, 1)
  expect_equal(reg_details$na_end_count, 1)
})

test_that("level parameter requires level column", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), c("31Dec2020", "28Feb2021"))
  processed <- preprocess_PARLdates(df)
  
  expect_error(
    check_anyNAinPARLdates(processed, level = "NT"),
    "PARLLOC is missing 'level' column needed for filtering"
  )
  
  expect_error(
    check_anyNAinPARLdates_details(processed, level = "NT"),
    "PARLLOC is missing 'level' column needed for filtering"
  )
})

# Unit tests for parliament_size validation functions
test_that("check_PARL_parliament_size_meaningful works with valid data", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), 
                c("31Dec2020", "28Feb2021"),
                parliament_size = c(120, 150))
  
  expect_true(check_PARL_parliament_size_meaningful(df))
})

test_that("check_PARL_parliament_size_meaningful detects NA values", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), 
                c("31Dec2020", "28Feb2021"),
                parliament_size = c(120, NA))
  
  expect_false(check_PARL_parliament_size_meaningful(df))
})

test_that("check_PARL_parliament_size_meaningful detects non-numeric values", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), 
                c("31Dec2020", "28Feb2021"),
                parliament_size = c("120", "INVALID"))
  
  expect_false(check_PARL_parliament_size_meaningful(df))
})

test_that("check_PARL_parliament_size_meaningful detects non-positive values", {
  df <- mk_parl(c("01Jan2020", "15Feb2021", "01Mar2021"), 
                c("31Dec2020", "28Feb2021", "31Mar2021"),
                parliament_size = c(120, 0, -5))
  
  expect_false(check_PARL_parliament_size_meaningful(df))
})

test_that("check_PARL_parliament_size_meaningful detects non-integer values", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), 
                c("31Dec2020", "28Feb2021"),
                parliament_size = c(120.5, 150))
  
  expect_false(check_PARL_parliament_size_meaningful(df))
})

test_that("check_PARL_parliament_size_meaningful filters by level", {
  df <- mk_parl(c("01Jan2020", "15Feb2021", "01Mar2021"), 
                c("31Dec2020", "28Feb2021", "31Mar2021"),
                level = c("NT", "REGIONAL", "NT"),
                parliament_size = c(120, NA, 150))
  
  # Without level filter: should detect NA
  expect_false(check_PARL_parliament_size_meaningful(df))
  
  # With NT level filter: should be clean
  expect_true(check_PARL_parliament_size_meaningful(df, level = "NT"))
  
  # With REGIONAL level filter: should detect NA
  expect_false(check_PARL_parliament_size_meaningful(df, level = "REGIONAL"))
})

test_that("check_PARL_parliament_size_meaningful_details provides comprehensive results", {
  df <- mk_parl(c("01Jan2020", "15Feb2021", "01Mar2021", "01Apr2021", "01May2021"), 
                c("31Dec2020", "28Feb2021", "31Mar2021", "30Apr2021", "31May2021"),
                parliament_size = c(120, NA, "INVALID", 0, 150.5))
  
  result <- check_PARL_parliament_size_meaningful_details(df)
  
  expect_false(result$check_passed)
  expect_equal(result$total_rows, 5)
  expect_equal(result$na_count, 1)
  expect_equal(result$non_numeric_count, 1)
  expect_equal(result$non_positive_count, 1)
  expect_equal(result$non_integer_count, 1)
  
  # Check row indices
  expect_equal(result$na_rows, 2)
  expect_equal(result$non_numeric_rows, 3)
  expect_equal(result$non_positive_rows, 4)
  expect_equal(result$non_integer_rows, 5)
  expect_equal(result$problem_rows, c(2, 3, 4, 5))
  
  # Check that problematic rows data frame is returned
  expect_s3_class(result$full_rows_with_problems, "data.frame")
  expect_equal(nrow(result$full_rows_with_problems), 4)
})

test_that("check_PARL_parliament_size_meaningful_details filters by level", {
  df <- mk_parl(c("01Jan2020", "15Feb2021", "01Mar2021"), 
                c("31Dec2020", "28Feb2021", "31Mar2021"),
                level = c("NT", "REGIONAL", "NT"),
                parliament_size = c(120, NA, 150))
  
  # Without level filter: should find NA
  all_details <- check_PARL_parliament_size_meaningful_details(df)
  expect_false(all_details$check_passed)
  expect_equal(all_details$na_count, 1)
  expect_equal(all_details$total_rows, 3)
  
  # With NT level filter: should pass
  nt_details <- check_PARL_parliament_size_meaningful_details(df, level = "NT")
  expect_true(nt_details$check_passed)
  expect_equal(nt_details$na_count, 0)
  expect_equal(nt_details$total_rows, 2)
  
  # With REGIONAL level filter: should fail
  reg_details <- check_PARL_parliament_size_meaningful_details(df, level = "REGIONAL")
  expect_false(reg_details$check_passed)
  expect_equal(reg_details$na_count, 1)
  expect_equal(reg_details$total_rows, 1)
})

test_that("parliament_size functions require parliament_size column", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), c("31Dec2020", "28Feb2021"))
  
  expect_error(
    check_PARL_parliament_size_meaningful(df),
    "PARLLOC is missing 'parliament_size' column"
  )
  
  expect_error(
    check_PARL_parliament_size_meaningful_details(df),
    "PARLLOC is missing 'parliament_size' column"
  )
})

test_that("parliament_size level parameter requires level column", {
  df <- mk_parl(c("01Jan2020", "15Feb2021"), 
                c("31Dec2020", "28Feb2021"),
                parliament_size = c(120, 150))
  
  expect_error(
    check_PARL_parliament_size_meaningful(df, level = "NT"),
    "PARLLOC is missing 'level' column needed for filtering"
  )
  
  expect_error(
    check_PARL_parliament_size_meaningful_details(df, level = "NT"),
    "PARLLOC is missing 'level' column needed for filtering"
  )
})
