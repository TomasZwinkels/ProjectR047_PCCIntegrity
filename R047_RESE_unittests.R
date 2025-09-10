# /R047_RESE_unittests.R
# ==================================================================
# Global test setup for RESE-related unit tests
# - Loads packages we rely on across tests
# - Sources the RESE functions under test
# - Defines common helpers used in multiple blocks
# ==================================================================

# Packages
library(testthat)
suppressPackageStartupMessages({
  library(dplyr)  # used by near-overlap checker
})

# Make sure the functions under test are available
# (adjust path if your functions live elsewhere)
source("R047_RESE_functions.R")

# ------------------------------------------------------------------
# Common helpers
# ------------------------------------------------------------------

# Minimal helper to build a tiny RESE-like df (string date inputs)
mk_rese <- function(start, end) {
  data.frame(
    res_entry_start = start,
    res_entry_end   = end,
    stringsAsFactors = FALSE
  )
}

# Helper to build a RESE-like df with parsed columns + required fields
mk_rese_overlap <- function(political_function, pers_id, start_dates, end_dates) {
  n <- length(political_function)
  data.frame(
    res_entry_id = if (n > 0) paste0("entry_", seq_len(n)) else character(0),
    political_function = political_function,
    pers_id = pers_id,
    res_entry_start = start_dates,
    res_entry_start_posoxctformat = as.POSIXct(start_dates, tz = "UTC"),
    res_entry_end = end_dates,
    res_entry_end_posoxctformat   = as.POSIXct(end_dates,   tz = "UTC"),
    res_entry_raw = if (n > 0) paste0("raw_", seq_len(n)) else character(0),
    stringsAsFactors = FALSE
  )
}

# ==================================================================
# Block: tests for referential integrity RESE -> POLI
#   Function under test: check_RESE_persid_in_POLI()
# ==================================================================

# ------------------------------------------------------------------
# Additional tests for: check_RESE_persid_in_POLI()
# ------------------------------------------------------------------

test_that("returns TRUE when all RESE pers_id are present in POLI", {
  RESE <- data.frame(pers_id = c(1,2,3,3), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = 1:5, stringsAsFactors = FALSE)
  expect_true(check_RESE_persid_in_POLI(RESE, POLI))
})

test_that("returns FALSE when some RESE pers_id are missing in POLI", {
  RESE <- data.frame(pers_id = c(1,2,6), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = 1:5, stringsAsFactors = FALSE)
  expect_false(check_RESE_persid_in_POLI(RESE, POLI))
})

test_that("works with empty RESE (trivially TRUE)", {
  RESE <- data.frame(pers_id = integer(0))
  POLI <- data.frame(pers_id = 1:5)
  expect_true(check_RESE_persid_in_POLI(RESE, POLI))
})

test_that("errors when pers_id column is missing in either RESE or POLI", {
  RESE <- data.frame(id = 1:3)
  POLI <- data.frame(pers_id = 1:3)
  expect_error(check_RESE_persid_in_POLI(RESE, POLI))

  RESE <- data.frame(pers_id = 1:3)
  POLI <- data.frame(id = 1:3)
  expect_error(check_RESE_persid_in_POLI(RESE, POLI))
})

# ==================================================================
# Block: check_RESE_resentryid_unique()
# ==================================================================

# ------------------------------------------------------------------
# Additional tests for: check_RESE_resentryid_unique()
# ------------------------------------------------------------------

test_that("returns TRUE when all res_entry_id are unique", {
  RESE <- data.frame(res_entry_id = 101:105, stringsAsFactors = FALSE)
  expect_true(check_RESE_resentryid_unique(RESE))
})

test_that("returns FALSE when there is any duplicate res_entry_id", {
  RESE <- data.frame(res_entry_id = c(101, 102, 103, 102), stringsAsFactors = FALSE)
  expect_false(check_RESE_resentryid_unique(RESE))
})

test_that("single NA is OK (still unique), two NA are not", {
  RESE1 <- data.frame(res_entry_id = c(101, NA, 103), stringsAsFactors = FALSE)
  RESE2 <- data.frame(res_entry_id = c(101, NA, 103, NA), stringsAsFactors = FALSE)
  expect_true(check_RESE_resentryid_unique(RESE1))   # only one NA -> unique
  expect_false(check_RESE_resentryid_unique(RESE2))  # two NA -> duplicate NA
})

test_that("empty RESE returns TRUE", {
  RESE <- data.frame(res_entry_id = integer(0))
  expect_true(check_RESE_resentryid_unique(RESE))
})

test_that("errors when res_entry_id column is missing", {
  RESE <- data.frame(id = 1:3)
  expect_error(check_RESE_resentryid_unique(RESE))
})



# ==================================================================
# Core block: preprocess_RESEdates() and check_anyNAinRESEdates()
# ==================================================================

# ------------------------------------------------------------
# Unit tests for:
# - preprocess_RESEdates()
# - check_anyNAinRESEdates()
# ------------------------------------------------------------

test_that("preprocess_RESEdates adds parsed POSIXct columns", {
  df <- mk_rese(c("01Jan2020","15Feb2021"), c("31Dec2020","28Feb2021"))
  out <- preprocess_RESEdates(df)

  expect_true(all(c("res_entry_start_posoxctformat","res_entry_end_posoxctformat") %in% names(out)))
  expect_s3_class(out$res_entry_start_posoxctformat, "POSIXct")
  expect_s3_class(out$res_entry_end_posoxctformat,   "POSIXct")

  # No NAs when inputs are valid
  expect_false(any(is.na(out$res_entry_start_posoxctformat)))
  expect_false(any(is.na(out$res_entry_end_posoxctformat)))
  expect_false(check_anyNAinRESEdates(out))
})

test_that("preprocess_RESEdates strips [[lcen]]/[[rcen]] before parsing", {
  df <- mk_rese(c("[[lcen]]01Jan2020", "[[rcen]]15Feb2021"),
                c("31Dec2020[[rcen]]", "[[lcen]]28Feb2021"))
  out <- preprocess_RESEdates(df)

  expect_false(any(is.na(out$res_entry_start_posoxctformat)))
  expect_false(any(is.na(out$res_entry_end_posoxctformat)))

  # Check exact parsed values
  expect_equal(
    as.Date(out$res_entry_start_posoxctformat),
    as.Date(c("2020-01-01","2021-02-15"))
  )
  expect_equal(
    as.Date(out$res_entry_end_posoxctformat),
    as.Date(c("2020-12-31","2021-02-28"))
  )
})

test_that("empty strings (after stripping) become NA and trigger a warning message", {
  # Second row has empty start and end after stripping -> NA -> should warn
  df <- mk_rese(c("01Jan2020", "   "), c("31Dec2020", ""))
  expect_message(
    out <- preprocess_RESEdates(df),
    regexp = "not all dates could be converted successfully",
    fixed  = FALSE
  )

  expect_true(is.na(out$res_entry_start_posoxctformat[2]))
  expect_true(is.na(out$res_entry_end_posoxctformat[2]))
  expect_true(check_anyNAinRESEdates(out))
})

test_that("invalid date strings become NA and are detected", {
  df <- mk_rese(c("BADDATE", "15Feb2021"), c("31Dec2020", "NOTADATE"))
  out <- preprocess_RESEdates(df)

  # one start NA, one end NA
  expect_equal(sum(is.na(out$res_entry_start_posoxctformat)), 1L)
  expect_equal(sum(is.na(out$res_entry_end_posoxctformat)),   1L)
  expect_true(check_anyNAinRESEdates(out))
})

test_that("function is idempotent (running twice yields identical result)", {
  df <- mk_rese(c("[[lcen]]01Jan2020", "15Feb2021"), c("31Dec2020", "[[rcen]]28Feb2021"))
  out1 <- preprocess_RESEdates(df)
  out2 <- preprocess_RESEdates(out1)  # run again

  expect_equal(out1$res_entry_start_posoxctformat, out2$res_entry_start_posoxctformat)
  expect_equal(out1$res_entry_end_posoxctformat,   out2$res_entry_end_posoxctformat)
})

test_that("check_anyNAinRESEdates returns FALSE when all parsed dates are present", {
  df <- mk_rese(c("01Jan2020","02Jan2020"), c("03Jan2020","04Jan2020"))
  out <- preprocess_RESEdates(df)
  expect_false(check_anyNAinRESEdates(out))
})

test_that("check_anyNAinRESEdates returns TRUE when any parsed date is NA", {
  df <- mk_rese(c("01Jan2020", NA), c("03Jan2020","04Jan2020"))
  out <- preprocess_RESEdates(df)
  expect_true(check_anyNAinRESEdates(out))
})

# ==================================================================
# Block: check_RESE_parlmemeppisodes_anyfulloverlap()
# ==================================================================

# ------------------------------------------------------------------
# Additional tests for: check_RESE_parlmemeppisodes_anyfulloverlap()
# ------------------------------------------------------------------

test_that("full-overlap duplicate among parliamentary episodes returns TRUE", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE-LH_T3_NA_01","OTHER"),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31","2020-12-31")
  )
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(df))
})

test_that("full-overlap duplicate with alternative political function code returns TRUE", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE_T3_NA_01","NT_LE_T3_NA_01","OTHER"),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31","2020-12-31")
  )
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(df))
})

test_that("mixed political function codes both detected", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE_T3_NA_01","OTHER"),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31","2020-12-31")
  )
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(df))
})

test_that("no full-overlap duplicates among parliamentary episodes returns FALSE", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE-LH_T3_NA_01"),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-02-01"),  # different start -> not a full duplicate
    end_dates   = c("2020-12-31","2020-12-31")
  )
  expect_false(check_RESE_parlmemeppisodes_anyfulloverlap(df))
})

test_that("non-parliamentary rows only: warns and returns TRUE", {
  df <- mk_rese_overlap(
    political_function = c("OTHER","OTHER"),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31")
  )
  expect_warning(
    res <- check_RESE_parlmemeppisodes_anyfulloverlap(df),
    regexp = "No parliamentary membership episodes found",
    fixed  = TRUE
  )
  expect_true(res)
})

test_that("empty after filter: warns and returns TRUE", {
  df <- mk_rese_overlap(
    political_function = character(0),
    pers_id     = integer(0),
    start_dates = as.POSIXct(character(0)),
    end_dates   = as.POSIXct(character(0))
  )
  expect_warning(
    res <- check_RESE_parlmemeppisodes_anyfulloverlap(df),
    regexp = "No parliamentary membership episodes found",
    fixed  = TRUE
  )
  expect_true(res)
})

test_that("NA-in-both dates duplicates are treated as duplicates (returns TRUE)", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE-LH_T3_NA_01"),
    pers_id     = c(99,99),
    start_dates = c(NA, NA),
    end_dates   = c(NA, NA)
  )
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(df))
})

# ==================================================================
# Block: check_RESE_anynear_fulloverlap()
# ==================================================================

# ------------------------------------------------------------------
# Additional tests for: check_RESE_anynear_fulloverlap()
# ------------------------------------------------------------------

test_that("returns TRUE when a near-full-overlap pair exists", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 3),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-02","2020-01-01"),
    end_dates   = c("2020-12-31","2021-01-01","2020-12-31")
  )
  expect_true(check_RESE_anynear_fulloverlap(df, tolerance_days = 2))
})

test_that("returns FALSE when pairs are outside tolerance", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-05"),
    end_dates   = c("2020-12-31","2021-01-06")
  )
  expect_false(check_RESE_anynear_fulloverlap(df, tolerance_days = 2))
})

test_that("tolerance parameter expands matches correctly", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-04"),
    end_dates   = c("2020-12-31","2021-01-03")
  )
  expect_false(check_RESE_anynear_fulloverlap(df, tolerance_days = 2))
  expect_true(check_RESE_anynear_fulloverlap(df, tolerance_days = 3))
})

test_that("returns FALSE when pers_id differs", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,2),
    start_dates = c("2020-01-01","2020-01-02"),
    end_dates   = c("2020-12-31","2021-01-01")
  )
  expect_false(check_RESE_anynear_fulloverlap(df))
})

test_that("returns FALSE with only one row", {
  df <- mk_rese_overlap(
    political_function = "NT_LE-LH_T3_NA_01",
    pers_id     = 1,
    start_dates = "2020-01-01",
    end_dates   = "2020-12-31"
  )
  expect_false(check_RESE_anynear_fulloverlap(df))
})

test_that("rows with NA dates do not count as overlaps", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01", NA),
    end_dates   = c("2020-12-31", "2020-12-31")
  )
  expect_false(check_RESE_anynear_fulloverlap(df))
})

test_that("returns FALSE on completely empty data", {
  df <- mk_rese_overlap(
    political_function = character(0),
    pers_id     = integer(0),
    start_dates = as.Date(character(0)),
    end_dates   = as.Date(character(0))
  )
  expect_false(check_RESE_anynear_fulloverlap(df))
})

# ==================================================================
# Block: Unit tests for all _details functions
# ==================================================================

# ------------------------------------------------------------------
# Tests for: check_RESE_persid_in_POLI_details()
# ------------------------------------------------------------------

test_that("check_RESE_persid_in_POLI_details returns detailed results when all IDs match", {
  RESE <- data.frame(pers_id = c(1,2,3,3), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = 1:5, stringsAsFactors = FALSE)
  result <- check_RESE_persid_in_POLI_details(RESE, POLI)
  
  expect_true(result$check_passed)
  expect_equal(length(result$missing_ids), 0)
  expect_equal(result$missing_count, 0)
  expect_equal(length(result$missing_rows), 0)
  expect_equal(result$total_unique_rese_ids, 3)
  expect_equal(result$total_unique_poli_ids, 5)
})

test_that("check_RESE_persid_in_POLI_details returns detailed results when some IDs missing", {
  RESE <- data.frame(pers_id = c(1,2,6,6,7), extra_col = letters[1:5], stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = 1:5, stringsAsFactors = FALSE)
  result <- check_RESE_persid_in_POLI_details(RESE, POLI)
  
  expect_false(result$check_passed)
  expect_equal(sort(result$missing_ids), c(6,7))
  expect_equal(result$missing_count, 2)
  expect_equal(nrow(result$missing_rows), 3)
  expect_true(all(result$missing_rows$pers_id %in% c(6,7)))
  expect_equal(result$total_unique_rese_ids, 4)
  expect_equal(result$total_unique_poli_ids, 5)
})

test_that("check_RESE_persid_in_POLI_details works with empty RESE", {
  RESE <- data.frame(pers_id = integer(0))
  POLI <- data.frame(pers_id = 1:5)
  result <- check_RESE_persid_in_POLI_details(RESE, POLI)
  
  expect_true(result$check_passed)
  expect_equal(length(result$missing_ids), 0)
  expect_equal(result$missing_count, 0)
  expect_equal(length(result$missing_rows), 0)
  expect_equal(result$total_unique_rese_ids, 0)
  expect_equal(result$total_unique_poli_ids, 5)
})

# ------------------------------------------------------------------
# Tests for: check_RESE_resentryid_unique_details()
# ------------------------------------------------------------------

test_that("check_RESE_resentryid_unique_details returns detailed results when all IDs unique", {
  RESE <- data.frame(res_entry_id = 101:105, stringsAsFactors = FALSE)
  result <- check_RESE_resentryid_unique_details(RESE)
  
  expect_true(result$check_passed)
  expect_equal(length(result$duplicate_ids), 0)
  expect_equal(result$duplicate_count, 0)
  expect_equal(length(result$duplicate_rows), 0)
  expect_equal(result$total_rows, 5)
})

test_that("check_RESE_resentryid_unique_details returns detailed results with duplicates", {
  RESE <- data.frame(res_entry_id = c(101, 102, 103, 102, 104, 103), extra_col = letters[1:6], stringsAsFactors = FALSE)
  result <- check_RESE_resentryid_unique_details(RESE)
  
  expect_false(result$check_passed)
  expect_equal(sort(result$duplicate_ids), c(102, 103))
  expect_equal(result$duplicate_count, 2)
  expect_equal(nrow(result$duplicate_rows), 4)
  expect_true(all(result$duplicate_rows$res_entry_id %in% c(102, 103)))
  expect_equal(result$total_rows, 6)
})

test_that("check_RESE_resentryid_unique_details handles single NA correctly", {
  RESE <- data.frame(res_entry_id = c(101, NA, 103), extra_col = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- check_RESE_resentryid_unique_details(RESE)
  
  expect_true(result$check_passed)
  expect_equal(length(result$duplicate_ids), 0)
  expect_equal(result$duplicate_count, 0)
  expect_equal(result$total_rows, 3)
})

test_that("check_RESE_resentryid_unique_details detects duplicate NAs", {
  RESE <- data.frame(res_entry_id = c(101, NA, 103, NA), extra_col = letters[1:4], stringsAsFactors = FALSE)
  result <- check_RESE_resentryid_unique_details(RESE)
  
  expect_false(result$check_passed)
  expect_true(is.na(result$duplicate_ids))
  expect_equal(result$duplicate_count, 1)
  expect_equal(nrow(result$duplicate_rows), 2)
  expect_equal(result$total_rows, 4)
})

# ------------------------------------------------------------------
# Tests for: check_anyNAinRESEdates_details()
# ------------------------------------------------------------------

test_that("check_anyNAinRESEdates_details returns detailed results when no NAs", {
  df <- mk_rese(c("01Jan2020","15Feb2021"), c("31Dec2020","28Feb2021"))
  processed <- preprocess_RESEdates(df)
  result <- check_anyNAinRESEdates_details(processed)
  
  expect_true(result$check_passed)
  expect_equal(result$na_start_count, 0)
  expect_equal(result$na_end_count, 0)
  expect_equal(length(result$na_start_rows), 0)
  expect_equal(length(result$na_end_rows), 0)
  expect_equal(length(result$na_either_rows), 0)
  expect_equal(nrow(result$full_rows_with_na_dates), 0)
  expect_equal(result$total_rows, 2)
})

test_that("check_anyNAinRESEdates_details returns detailed results with NAs", {
  df <- mk_rese(c("BADDATE", "15Feb2021", NA), c("31Dec2020", "NOTADATE", "28Feb2021"))
  processed <- preprocess_RESEdates(df)
  result <- check_anyNAinRESEdates_details(processed)
  
  expect_false(result$check_passed)
  expect_equal(result$na_start_count, 2)
  expect_equal(result$na_end_count, 1)
  expect_equal(result$na_start_rows, c(1, 3))
  expect_equal(result$na_end_rows, 2)
  expect_equal(sort(result$na_either_rows), c(1, 2, 3))
  expect_equal(nrow(result$full_rows_with_na_dates), 3)
  expect_equal(result$total_rows, 3)
})

test_that("check_anyNAinRESEdates_details errors on missing required columns", {
  df <- data.frame(start = "01Jan2020", end = "31Dec2020")
  expect_error(
    check_anyNAinRESEdates_details(df),
    "RESELOC is missing columns: res_entry_start_posoxctformat, res_entry_end_posoxctformat"
  )
})

# ------------------------------------------------------------------
# Tests for: check_RESE_parlmemeppisodes_anyfulloverlap_details()
# ------------------------------------------------------------------

test_that("check_RESE_parlmemeppisodes_anyfulloverlap_details returns detailed results with no overlaps", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE-LH_T3_NA_01"),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-02-01"),
    end_dates   = c("2020-12-31","2020-12-31")
  )
  result <- check_RESE_parlmemeppisodes_anyfulloverlap_details(df)
  
  expect_true(result$check_passed)
  expect_equal(nrow(result$overlapping_episodes), 0)
  expect_equal(result$overlap_count, 0)
  expect_equal(length(result$affected_persons), 0)
  expect_equal(result$total_parl_episodes, 2)
})

test_that("check_RESE_parlmemeppisodes_anyfulloverlap_details returns detailed results with overlaps", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE-LH_T3_NA_01","NT_LE-LH_T3_NA_01","OTHER"),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31","2020-12-31")
  )
  result <- check_RESE_parlmemeppisodes_anyfulloverlap_details(df)
  
  expect_false(result$check_passed)
  expect_equal(nrow(result$overlapping_episodes), 2)
  expect_equal(result$overlap_count, 2)
  expect_equal(result$affected_persons, 1)
  expect_equal(result$total_parl_episodes, 2)
})

test_that("check_RESE_parlmemeppisodes_anyfulloverlap_details handles no parliamentary episodes", {
  df <- mk_rese_overlap(
    political_function = c("OTHER","OTHER"),
    pers_id     = c(1,2),
    start_dates = c("2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31")
  )
  result <- check_RESE_parlmemeppisodes_anyfulloverlap_details(df)
  
  expect_false(result$check_passed)
  expect_equal(result$warning_message, "No parliamentary membership episodes found")
  expect_equal(nrow(result$overlapping_episodes), 0)
  expect_equal(result$overlap_count, 0)
  expect_equal(length(result$affected_persons), 0)
  expect_equal(result$total_parl_episodes, 0)
})

test_that("check_RESE_parlmemeppisodes_anyfulloverlap_details works with alternative political function codes", {
  df <- mk_rese_overlap(
    political_function = c("NT_LE_T3_NA_01","NT_LE_T3_NA_01"),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-01"),
    end_dates   = c("2020-12-31","2020-12-31")
  )
  result <- check_RESE_parlmemeppisodes_anyfulloverlap_details(df)
  
  expect_false(result$check_passed)
  expect_equal(nrow(result$overlapping_episodes), 2)
  expect_equal(result$total_parl_episodes, 2)
})

# ------------------------------------------------------------------
# Tests for: check_RESE_anynear_fulloverlap_details()
# ------------------------------------------------------------------

test_that("check_RESE_anynear_fulloverlap_details returns detailed results with no near overlaps", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-05"),
    end_dates   = c("2020-12-31","2021-01-06")
  )
  result <- check_RESE_anynear_fulloverlap_details(df, tolerance_days = 2)
  
  expect_true(result$check_passed)
  expect_equal(nrow(result$full_episode_pairs_near_overlapping), 0)
  expect_equal(result$near_overlap_count, 0)
  expect_equal(length(result$affected_persons), 0)
  expect_equal(result$tolerance_days, 2)
  expect_equal(result$total_rows, 2)
})

test_that("check_RESE_anynear_fulloverlap_details returns detailed results with near overlaps", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 3),
    pers_id     = c(1,1,2),
    start_dates = c("2020-01-01","2020-01-02","2020-01-01"),
    end_dates   = c("2020-12-31","2021-01-01","2020-12-31")
  )
  result <- check_RESE_anynear_fulloverlap_details(df, tolerance_days = 2)
  
  expect_false(result$check_passed)
  expect_equal(nrow(result$full_episode_pairs_near_overlapping), 1)
  expect_equal(result$near_overlap_count, 1)
  expect_equal(result$affected_persons, 1)
  expect_equal(result$tolerance_days, 2)
  expect_equal(result$total_rows, 3)
  
  pairs <- result$full_episode_pairs_near_overlapping
  expect_true("start_diff_days" %in% names(pairs))
  expect_true("end_diff_days" %in% names(pairs))
})

test_that("check_RESE_anynear_fulloverlap_details handles single row data", {
  df <- mk_rese_overlap(
    political_function = "NT_LE-LH_T3_NA_01",
    pers_id     = 1,
    start_dates = "2020-01-01",
    end_dates   = "2020-12-31"
  )
  result <- check_RESE_anynear_fulloverlap_details(df)
  
  expect_true(result$check_passed)
  expect_equal(nrow(result$full_episode_pairs_near_overlapping), 0)
  expect_equal(result$near_overlap_count, 0)
  expect_equal(result$total_rows, 1)
})

test_that("check_RESE_anynear_fulloverlap_details handles empty data", {
  df <- mk_rese_overlap(
    political_function = character(0),
    pers_id     = integer(0),
    start_dates = as.POSIXct(character(0)),
    end_dates   = as.POSIXct(character(0))
  )
  result <- check_RESE_anynear_fulloverlap_details(df)
  
  expect_true(result$check_passed)
  expect_equal(nrow(result$full_episode_pairs_near_overlapping), 0)
  expect_equal(result$near_overlap_count, 0)
  expect_equal(result$total_rows, 0)
})

test_that("check_RESE_anynear_fulloverlap_details validates tolerance parameter", {
  df <- mk_rese_overlap(
    political_function = rep("NT_LE-LH_T3_NA_01", 2),
    pers_id     = c(1,1),
    start_dates = c("2020-01-01","2020-01-04"),
    end_dates   = c("2020-12-31","2021-01-03")
  )
  
  result2 <- check_RESE_anynear_fulloverlap_details(df, tolerance_days = 2)
  result3 <- check_RESE_anynear_fulloverlap_details(df, tolerance_days = 3)
  
  expect_true(result2$check_passed)
  expect_false(result3$check_passed)
  expect_equal(result3$tolerance_days, 3)
})

test_that("check_RESE_anynear_fulloverlap_details errors on missing required columns", {
  df <- data.frame(id = 1, start = "2020-01-01", end = "2020-12-31")
  expect_error(
    check_RESE_anynear_fulloverlap_details(df),
    "RESE is missing columns:"
  )
})
