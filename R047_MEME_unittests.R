# R047_MEME_unittests.R
# ==================================================================
# Unit tests for MEME (party membership) data integrity functions
# ==================================================================

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
})

source("R047_MEME_functions.R")

# ------------------------------------------------------------------
# Common helpers
# ------------------------------------------------------------------

# Minimal helper for date preprocessing tests
mk_meme <- function(start, end) {
  data.frame(
    memep_startdate = start,
    memep_enddate   = end,
    stringsAsFactors = FALSE
  )
}

# Full helper with parsed date columns for check functions
mk_meme_full <- function(memep_id, pers_id, party_id, start_dates, end_dates) {
  n <- length(memep_id)
  data.frame(
    memep_id = memep_id,
    pers_id = pers_id,
    party_id = party_id,
    memep_startdate = start_dates,
    memep_enddate = end_dates,
    memep_startdate_posoxctformat = as.POSIXct(start_dates, tz = "UTC"),
    memep_enddate_posoxctformat   = as.POSIXct(end_dates, tz = "UTC"),
    memep_type_raw = rep("regular", n),
    stringsAsFactors = FALSE
  )
}


# ==================================================================
# Block: preprocess_MEMEdates
# ==================================================================

test_that("preprocess_MEMEdates adds parsed POSIXct columns", {
  m <- mk_meme("01Jan2020", "31Dec2020")
  result <- preprocess_MEMEdates(m)
  expect_true("memep_startdate_posoxctformat" %in% names(result))
  expect_true("memep_enddate_posoxctformat" %in% names(result))
  expect_false(is.na(result$memep_startdate_posoxctformat[1]))
  expect_false(is.na(result$memep_enddate_posoxctformat[1]))
})

test_that("preprocess_MEMEdates strips censor tags", {
  m <- mk_meme("[[lcen]]01Jan2020", "31Dec2020[[rcen]]")
  result <- preprocess_MEMEdates(m)
  expect_false(is.na(result$memep_startdate_posoxctformat[1]))
  expect_false(is.na(result$memep_enddate_posoxctformat[1]))
})

test_that("preprocess_MEMEdates converts empty strings to NA", {
  m <- mk_meme("01Jan2020", "")
  result <- suppressMessages(preprocess_MEMEdates(m))
  expect_true(is.na(result$memep_enddate_posoxctformat[1]))
})

test_that("preprocess_MEMEdates warns on unparseable dates", {
  m <- mk_meme("BADDATE", "01Jan2020")
  expect_message(preprocess_MEMEdates(m), "WARNING")
})

test_that("preprocess_MEMEdates is idempotent", {
  m <- mk_meme("01Jan2020", "31Dec2020")
  r1 <- preprocess_MEMEdates(m)
  r2 <- preprocess_MEMEdates(r1)
  expect_equal(r1$memep_startdate_posoxctformat, r2$memep_startdate_posoxctformat)
  expect_equal(r1$memep_enddate_posoxctformat, r2$memep_enddate_posoxctformat)
})


# ==================================================================
# Block: check_MEME_persid_in_POLI
# ==================================================================

test_that("returns TRUE when all MEME pers_id are present in POLI", {
  MEME <- data.frame(pers_id = c("A", "B", "B"), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_true(check_MEME_persid_in_POLI(MEME, POLI))
})

test_that("returns FALSE when some MEME pers_id are missing in POLI", {
  MEME <- data.frame(pers_id = c("A", "B", "D"), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_false(check_MEME_persid_in_POLI(MEME, POLI))
})

test_that("returns TRUE with empty MEME", {
  MEME <- data.frame(pers_id = character(0), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = c("A"), stringsAsFactors = FALSE)
  expect_true(check_MEME_persid_in_POLI(MEME, POLI))
})

test_that("errors when pers_id column missing", {
  expect_error(check_MEME_persid_in_POLI(data.frame(x = 1), data.frame(pers_id = "A")))
  expect_error(check_MEME_persid_in_POLI(data.frame(pers_id = "A"), data.frame(x = 1)))
})

# ------------------------------------------------------------------
# check_MEME_persid_in_POLI_details
# ------------------------------------------------------------------

test_that("details returns check_passed = TRUE when all match", {
  MEME <- data.frame(pers_id = c("A", "B"), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = c("A", "B"), stringsAsFactors = FALSE)
  d <- check_MEME_persid_in_POLI_details(MEME, POLI)
  expect_true(d$check_passed)
  expect_equal(d$missing_count, 0)
})

test_that("details returns missing IDs when some missing", {
  MEME <- data.frame(pers_id = c("A", "B", "D"), stringsAsFactors = FALSE)
  POLI <- data.frame(pers_id = c("A", "B"), stringsAsFactors = FALSE)
  d <- check_MEME_persid_in_POLI_details(MEME, POLI)
  expect_false(d$check_passed)
  expect_equal(d$missing_ids, "D")
  expect_equal(nrow(d$missing_rows), 1)
})


# ==================================================================
# Block: check_MEME_partyid_in_PART
# ==================================================================

test_that("returns TRUE when all MEME party_id are present in PART", {
  MEME <- data.frame(party_id = c("P1", "P2"), stringsAsFactors = FALSE)
  PART <- data.frame(party_id = c("P1", "P2", "P3"), stringsAsFactors = FALSE)
  expect_true(check_MEME_partyid_in_PART(MEME, PART))
})

test_that("returns FALSE when some party_id are missing in PART", {
  MEME <- data.frame(party_id = c("P1", "P999"), stringsAsFactors = FALSE)
  PART <- data.frame(party_id = c("P1", "P2"), stringsAsFactors = FALSE)
  expect_false(check_MEME_partyid_in_PART(MEME, PART))
})

test_that("returns TRUE with empty MEME", {
  MEME <- data.frame(party_id = character(0), stringsAsFactors = FALSE)
  PART <- data.frame(party_id = c("P1"), stringsAsFactors = FALSE)
  expect_true(check_MEME_partyid_in_PART(MEME, PART))
})

test_that("errors when party_id column missing", {
  expect_error(check_MEME_partyid_in_PART(data.frame(x = 1), data.frame(party_id = "P1")))
  expect_error(check_MEME_partyid_in_PART(data.frame(party_id = "P1"), data.frame(x = 1)))
})

# ------------------------------------------------------------------
# check_MEME_partyid_in_PART_details
# ------------------------------------------------------------------

test_that("details returns check_passed = TRUE when all match", {
  MEME <- data.frame(party_id = c("P1", "P2"), stringsAsFactors = FALSE)
  PART <- data.frame(party_id = c("P1", "P2"), stringsAsFactors = FALSE)
  d <- check_MEME_partyid_in_PART_details(MEME, PART)
  expect_true(d$check_passed)
  expect_equal(d$missing_count, 0)
})

test_that("details returns missing party IDs when some missing", {
  MEME <- data.frame(party_id = c("P1", "P999"), stringsAsFactors = FALSE)
  PART <- data.frame(party_id = c("P1"), stringsAsFactors = FALSE)
  d <- check_MEME_partyid_in_PART_details(MEME, PART)
  expect_false(d$check_passed)
  expect_equal(d$missing_ids, "P999")
  expect_equal(nrow(d$missing_rows), 1)
})


# ==================================================================
# Block: check_MEME_memepid_unique
# ==================================================================

test_that("returns TRUE when all memep_id are unique", {
  MEME <- data.frame(memep_id = c("m1", "m2", "m3"), stringsAsFactors = FALSE)
  expect_true(check_MEME_memepid_unique(MEME))
})

test_that("returns FALSE when duplicates exist", {
  MEME <- data.frame(memep_id = c("m1", "m2", "m1"), stringsAsFactors = FALSE)
  expect_false(check_MEME_memepid_unique(MEME))
})

test_that("single NA is OK but two NAs are not", {
  MEME_one_na <- data.frame(memep_id = c("m1", NA_character_), stringsAsFactors = FALSE)
  expect_true(check_MEME_memepid_unique(MEME_one_na))

  MEME_two_na <- data.frame(memep_id = c("m1", NA_character_, NA_character_), stringsAsFactors = FALSE)
  expect_false(check_MEME_memepid_unique(MEME_two_na))
})

test_that("returns TRUE with empty MEME", {
  MEME <- data.frame(memep_id = character(0), stringsAsFactors = FALSE)
  expect_true(check_MEME_memepid_unique(MEME))
})

test_that("errors when memep_id column missing", {
  expect_error(check_MEME_memepid_unique(data.frame(x = 1)))
})

# ------------------------------------------------------------------
# check_MEME_memepid_unique_details
# ------------------------------------------------------------------

test_that("details with all unique", {
  MEME <- data.frame(memep_id = c("m1", "m2"), stringsAsFactors = FALSE)
  d <- check_MEME_memepid_unique_details(MEME)
  expect_true(d$check_passed)
  expect_equal(d$duplicate_count, 0)
})

test_that("details with duplicates returns all rows containing dup IDs", {
  MEME <- data.frame(memep_id = c("m1", "m2", "m1"), stringsAsFactors = FALSE)
  d <- check_MEME_memepid_unique_details(MEME)
  expect_false(d$check_passed)
  expect_equal(nrow(d$duplicate_rows), 2)
})

test_that("details detects duplicate NAs", {
  MEME <- data.frame(memep_id = c("m1", NA_character_, NA_character_), stringsAsFactors = FALSE)
  d <- check_MEME_memepid_unique_details(MEME)
  expect_false(d$check_passed)
})


# ==================================================================
# Block: check_anyNAinMEMEdates
# ==================================================================

test_that("returns FALSE when no NA start dates", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", "2020-12-31")
  expect_false(check_anyNAinMEMEdates(m))
})

test_that("returns TRUE when NA start dates present", {
  m <- mk_meme_full("m1", "A", "P1", NA_character_, "2020-12-31")
  expect_true(check_anyNAinMEMEdates(m))
})

test_that("returns FALSE when only end dates are NA (ongoing memberships OK)", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", NA_character_)
  expect_false(check_anyNAinMEMEdates(m))
})

test_that("errors when parsed date column missing", {
  expect_error(check_anyNAinMEMEdates(data.frame(x = 1)))
})

# ------------------------------------------------------------------
# check_anyNAinMEMEdates_details
# ------------------------------------------------------------------

test_that("details when clean — check_passed is TRUE", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", "2020-12-31")
  d <- check_anyNAinMEMEdates_details(m)
  expect_true(d$check_passed)
  expect_equal(d$na_start_count, 0)
})

test_that("details with NA start dates — check_passed is FALSE", {
  m <- mk_meme_full("m1", "A", "P1", NA_character_, "2020-12-31")
  d <- check_anyNAinMEMEdates_details(m)
  expect_false(d$check_passed)
  expect_equal(d$na_start_count, 1)
})

test_that("details reports NA end dates informationally but still passes", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", NA_character_)
  d <- check_anyNAinMEMEdates_details(m)
  expect_true(d$check_passed)
  expect_equal(d$na_end_count, 1)
})


# ==================================================================
# Block: check_MEME_inverted_dates
# ==================================================================

test_that("returns FALSE when all dates ordered correctly", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", "2020-12-31")
  expect_false(check_MEME_inverted_dates(m))
})

test_that("returns TRUE when end < start", {
  m <- mk_meme_full("m1", "A", "P1", "2020-12-31", "2020-01-01")
  expect_true(check_MEME_inverted_dates(m))
})

test_that("returns FALSE when start equals end", {
  m <- mk_meme_full("m1", "A", "P1", "2020-06-15", "2020-06-15")
  expect_false(check_MEME_inverted_dates(m))
})

test_that("returns FALSE when all dates NA", {
  m <- mk_meme_full("m1", "A", "P1", NA_character_, NA_character_)
  expect_false(check_MEME_inverted_dates(m))
})

test_that("errors when parsed columns missing", {
  expect_error(check_MEME_inverted_dates(data.frame(x = 1)))
})

# ------------------------------------------------------------------
# check_MEME_inverted_dates_details
# ------------------------------------------------------------------

test_that("details with no inversions", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", "2020-12-31")
  d <- check_MEME_inverted_dates_details(m)
  expect_true(d$check_passed)
  expect_equal(d$inverted_count, 0)
})

test_that("details with inversions includes date_diff_days", {
  m <- mk_meme_full("m1", "A", "P1", "2020-12-31", "2020-01-01")
  d <- check_MEME_inverted_dates_details(m)
  expect_false(d$check_passed)
  expect_equal(d$inverted_count, 1)
  expect_true("date_diff_days" %in% names(d$inverted_rows))
  expect_true(d$inverted_rows$date_diff_days[1] < 0)
})

test_that("details handles NA dates correctly", {
  m <- mk_meme_full("m1", "A", "P1", "2020-01-01", NA_character_)
  d <- check_MEME_inverted_dates_details(m)
  expect_true(d$check_passed)
  expect_equal(d$valid_date_pairs, 0)
})


# ==================================================================
# Block: check_MEME_anyfulloverlap
# ==================================================================

test_that("returns TRUE when exact duplicates exist (same person, same party, same dates)", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "A"), c("P1", "P1"),
    c("2020-01-01", "2020-01-01"), c("2020-12-31", "2020-12-31")
  )
  expect_true(check_MEME_anyfulloverlap(m))
})

test_that("returns FALSE when no duplicates", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "A"), c("P1", "P1"),
    c("2020-01-01", "2021-01-01"), c("2020-12-31", "2021-12-31")
  )
  expect_false(check_MEME_anyfulloverlap(m))
})

test_that("returns FALSE with different persons having same party and dates", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "B"), c("P1", "P1"),
    c("2020-01-01", "2020-01-01"), c("2020-12-31", "2020-12-31")
  )
  expect_false(check_MEME_anyfulloverlap(m))
})

test_that("returns FALSE when same person in different parties with same dates", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "A"), c("P1", "P2"),
    c("2020-01-01", "2020-01-01"), c("2020-12-31", "2020-12-31")
  )
  expect_false(check_MEME_anyfulloverlap(m))
})

test_that("returns FALSE with empty data", {
  m <- mk_meme_full(character(0), character(0), character(0), character(0), character(0))
  expect_false(check_MEME_anyfulloverlap(m))
})

# ------------------------------------------------------------------
# check_MEME_anyfulloverlap_details
# ------------------------------------------------------------------

test_that("details with no overlaps", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "B"), c("P1", "P1"),
    c("2020-01-01", "2020-01-01"), c("2020-12-31", "2020-12-31")
  )
  d <- check_MEME_anyfulloverlap_details(m)
  expect_true(d$check_passed)
  expect_equal(d$overlap_count, 0)
})

test_that("details with overlaps returns affected persons", {
  m <- mk_meme_full(
    c("m1", "m2"), c("A", "A"), c("P1", "P1"),
    c("2020-01-01", "2020-01-01"), c("2020-12-31", "2020-12-31")
  )
  d <- check_MEME_anyfulloverlap_details(m)
  expect_false(d$check_passed)
  expect_equal(d$overlap_count, 2)
  expect_equal(d$affected_persons, "A")
})


# ==================================================================
# Block: check_MEME_parlmembers_have_party
# ==================================================================

test_that("returns TRUE when all MPs have party records", {
  RESE <- data.frame(
    pers_id = c("A", "B"),
    political_function = c("NT_LE_T3_NA_01", "NT_LE_T3_NA_01"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_true(check_MEME_parlmembers_have_party(RESE, MEME))
})

test_that("returns FALSE when some MPs lack party records", {
  RESE <- data.frame(
    pers_id = c("A", "B"),
    political_function = c("NT_LE_T3_NA_01", "NT_LE_T3_NA_01"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A"), stringsAsFactors = FALSE)
  expect_false(check_MEME_parlmembers_have_party(RESE, MEME))
})

test_that("returns TRUE with empty RESE (no MPs to check)", {
  RESE <- data.frame(
    pers_id = character(0),
    political_function = character(0),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A"), stringsAsFactors = FALSE)
  expect_true(check_MEME_parlmembers_have_party(RESE, MEME))
})

test_that("only checks parliamentary functions, ignores others", {
  RESE <- data.frame(
    pers_id = c("A", "B"),
    political_function = c("NT_LE_T3_NA_01", "OTHER_FUNCTION"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A"), stringsAsFactors = FALSE)
  expect_true(check_MEME_parlmembers_have_party(RESE, MEME))
})

test_that("errors when required columns missing", {
  expect_error(check_MEME_parlmembers_have_party(
    data.frame(pers_id = "A"), data.frame(pers_id = "A")
  ))
  expect_error(check_MEME_parlmembers_have_party(
    data.frame(political_function = "X"), data.frame(pers_id = "A")
  ))
})

# ------------------------------------------------------------------
# check_MEME_parlmembers_have_party_details
# ------------------------------------------------------------------

test_that("details when all MPs matched", {
  RESE <- data.frame(
    pers_id = c("A", "B"),
    political_function = c("NT_LE_T3_NA_01", "NT_LE_T3_NA_01"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A", "B"), stringsAsFactors = FALSE)
  d <- check_MEME_parlmembers_have_party_details(RESE, MEME)
  expect_true(d$check_passed)
  expect_equal(d$missing_count, 0)
  expect_equal(d$total_parlmembers, 2)
})

test_that("details showing missing MPs", {
  RESE <- data.frame(
    pers_id = c("A", "B"),
    political_function = c("NT_LE_T3_NA_01", "NT_LE_T3_NA_01"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = c("A"), stringsAsFactors = FALSE)
  d <- check_MEME_parlmembers_have_party_details(RESE, MEME)
  expect_false(d$check_passed)
  expect_equal(d$missing_ids, "B")
  expect_equal(nrow(d$missing_rese_rows), 1)
})

test_that("details when RESE has no parliamentary episodes", {
  RESE <- data.frame(
    pers_id = c("A"),
    political_function = c("OTHER"),
    stringsAsFactors = FALSE
  )
  MEME <- data.frame(pers_id = character(0), stringsAsFactors = FALSE)
  d <- check_MEME_parlmembers_have_party_details(RESE, MEME)
  expect_true(d$check_passed)
  expect_equal(d$total_parlmembers, 0)
})
