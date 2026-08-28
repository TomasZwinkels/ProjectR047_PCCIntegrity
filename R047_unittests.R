################################################################################
# Unit Tests for merge_episodes Function
################################################################################

# Load required libraries
library(testthat)
library(dplyr)
library(lubridate)

# Source the functions file to make all functions available for testing
source("R047_functions.R")

# Test 1: Overlapping and contiguous intervals
test_that("merge_episodes merges overlapping/contiguous intervals correctly", {
  # Create a sample data frame for person "test_1"
  # Episodes:
  #   1. 01jan2020 - 10jan2020
  #   2. 05jan2020 - 15jan2020  -> Should merge with episode 1 to become 01jan2020 - 15jan2020
  #   3. 01feb2020 - 05feb2020
  #   4. 05feb2020 - 10feb2020  -> Should merge with episode 3 to become 01feb2020 - 10feb2020
  test_df <- data.frame(
    pers_id         = rep("test_1", 4),
    res_entry_start = c("01jan2020", "05jan2020", "01feb2020", "05feb2020"),
    res_entry_end   = c("10jan2020", "15jan2020", "05feb2020", "10feb2020"),
    res_entry_raw   = c("raw1", "raw2", "raw3", "raw4"),
    country_abb     = "NL",
    stringsAsFactors = FALSE
  )
  
  # Convert date strings to POSIXct (day-level resolution)
  test_df$res_entry_start_posoxctformat <- as.POSIXct(test_df$res_entry_start, format="%d%b%Y")
  test_df$res_entry_end_posoxctformat   <- as.POSIXct(test_df$res_entry_end, format="%d%b%Y")
  
  # Call merge_episodes for "test_1" using test_df
  result <- merge_episodes(test_df, "test_1")
  
  # We expect two merged intervals:
  #   Interval 1: from 01jan2020 to 15jan2020
  #   Interval 2: from 01feb2020 to 10feb2020
  expect_equal(nrow(result), 2)
  
  # Verify that the merged intervals have the correct start and end dates
  expect_equal(result$res_entry_start[1], "01jan2020")
  expect_equal(result$res_entry_end[1],   "15jan2020")
  
  expect_equal(result$res_entry_start[2], "01feb2020")
  expect_equal(result$res_entry_end[2],   "10feb2020")
})

# Test 2: Non-overlapping episodes remain separate
test_that("merge_episodes handles non-overlapping episodes correctly", {
  # Create a sample data frame for person "test_non_overlap"
  # Episodes:
  #   Episode 1: 01apr2020 to 05apr2020
  #   Episode 2: 10apr2020 to 15apr2020 (no overlap)
  test_df <- data.frame(
    pers_id         = rep("test_non_overlap", 2),
    res_entry_start = c("01apr2020", "10apr2020"),
    res_entry_end   = c("05apr2020", "15apr2020"),
    res_entry_raw   = c("raw5", "raw6"),
    country_abb     = "NL",
    stringsAsFactors = FALSE
  )
  
  test_df$res_entry_start_posoxctformat <- as.POSIXct(test_df$res_entry_start, format="%d%b%Y")
  test_df$res_entry_end_posoxctformat   <- as.POSIXct(test_df$res_entry_end, format="%d%b%Y")
  
  result <- merge_episodes(test_df, "test_non_overlap")
  
  # Expect two separate intervals (unchanged)
  expect_equal(nrow(result), 2)
  expect_equal(result$res_entry_start[1], "01apr2020")
  expect_equal(result$res_entry_end[1],   "05apr2020")
  expect_equal(result$res_entry_start[2], "10apr2020")
  expect_equal(result$res_entry_end[2],   "15apr2020")
})

# Test 3: Nested intervals merge into a single interval
test_that("merge_episodes merges nested intervals correctly", {
  # Create a sample data frame for person "test_nested"
  # Episodes:
  #   Episode 1: 01may2020 to 15may2020
  #   Episode 2: 05may2020 to 10may2020 (nested within Episode 1)
  # They should merge into one interval: 01may2020 to 15may2020.
  test_df <- data.frame(
    pers_id         = rep("test_nested", 2),
    res_entry_start = c("01may2020", "05may2020"),
    res_entry_end   = c("15may2020", "10may2020"),
    res_entry_raw   = c("raw7", "raw8"),
    country_abb     = "NL",
    stringsAsFactors = FALSE
  )
  
  test_df$res_entry_start_posoxctformat <- as.POSIXct(test_df$res_entry_start, format="%d%b%Y")
  test_df$res_entry_end_posoxctformat   <- as.POSIXct(test_df$res_entry_end, format="%d%b%Y")
  
  result <- merge_episodes(test_df, "test_nested")
  
  # Expect one merged interval from 01may2020 to 15may2020.
  expect_equal(nrow(result), 1)
  expect_equal(result$res_entry_start[1], "01may2020")
  expect_equal(result$res_entry_end[1],   "15may2020")
})

# Test 4: Returns NULL when no episodes are found
test_that("merge_episodes returns NULL when no episodes found", {
  # Create a sample data frame that does not contain the requested pers_id
  test_df <- data.frame(
    pers_id         = rep("test_1", 2),
    res_entry_start = c("01jan2020", "05jan2020"),
    res_entry_end   = c("10jan2020", "15jan2020"),
    res_entry_raw   = c("raw1", "raw2"),
    country_abb     = "NL",
    stringsAsFactors = FALSE
  )
  
  test_df$res_entry_start_posoxctformat <- as.POSIXct(test_df$res_entry_start, format="%d%b%Y")
  test_df$res_entry_end_posoxctformat   <- as.POSIXct(test_df$res_entry_end, format="%d%b%Y")
  
  result <- merge_episodes(test_df, "non_existent")
  expect_null(result)
})

# Test 5: More than two resulting merged intervals
test_that("merge_episodes merges multiple intervals correctly", {
  # Create a sample data frame for person "test_multiple"
  # Episodes:
  #   Episode 1: 01jan2020 to 05jan2020
  #   Episode 2: 03jan2020 to 10jan2020  -> Merges with Episode 1 to: 01jan2020 to 10jan2020
  #   Episode 3: 15jan2020 to 20jan2020
  #   Episode 4: 18jan2020 to 25jan2020  -> Merges with Episode 3 to: 15jan2020 to 25jan2020
  #   Episode 5: 01feb2020 to 05feb2020  -> Remains as its own interval
  # Expected merged intervals:
  #   Interval 1: 01jan2020 to 10jan2020
  #   Interval 2: 15jan2020 to 25jan2020
  #   Interval 3: 01feb2020 to 05feb2020
  test_df <- data.frame(
    pers_id         = rep("test_multiple", 5),
    res_entry_start = c("01jan2020", "03jan2020", "15jan2020", "18jan2020", "01feb2020"),
    res_entry_end   = c("05jan2020", "10jan2020", "20jan2020", "25jan2020", "05feb2020"),
    res_entry_raw   = c("raw1", "raw2", "raw3", "raw4", "raw5"),
    country_abb     = "NL",
    stringsAsFactors = FALSE
  )
  
  test_df$res_entry_start_posoxctformat <- as.POSIXct(test_df$res_entry_start, format="%d%b%Y")
  test_df$res_entry_end_posoxctformat   <- as.POSIXct(test_df$res_entry_end, format="%d%b%Y")
  
  result <- merge_episodes(test_df, "test_multiple")
  
  # We expect three merged intervals
  expect_equal(nrow(result), 3)
  
  # Check the intervals:
  # Interval 1: 01jan2020 to 10jan2020
  expect_equal(result$res_entry_start[1], "01jan2020")
  expect_equal(result$res_entry_end[1],   "10jan2020")
  
  # Interval 2: 15jan2020 to 25jan2020
  expect_equal(result$res_entry_start[2], "15jan2020")
  expect_equal(result$res_entry_end[2],   "25jan2020")
  
  # Interval 3: 01feb2020 to 05feb2020
  expect_equal(result$res_entry_start[3], "01feb2020")
  expect_equal(result$res_entry_end[3],   "05feb2020")
})


################################################################################
# Unit Tests for find_gap_episodes Function
################################################################################

test_that("find_gap_episodes identifies episodes with problematic gaps", {
  # Create sample data for person "test_gap"
  test_data <- data.frame(
    pers_id = rep("test_gap", 3),
    res_entry_id = paste0("test_gap_", 1:3),
    res_entry_start = c("01jan2020", "07jan2020", "11jan2020"),
    res_entry_end   = c("05jan2020", "10jan2020", "15jan2020"),
    stringsAsFactors = FALSE
  )
  
  # Add POSIXct columns using day-level resolution
  test_data$res_entry_start_posoxctformat <- as.POSIXct(test_data$res_entry_start, format="%d%b%Y")
  test_data$res_entry_end_posoxctformat   <- as.POSIXct(test_data$res_entry_end, format="%d%b%Y")
  
  # Call the function with min_gap = 1 and gap_threshold = 3
  # Expected: 
  #   - Gap between Episode 1 (ends "05jan2020") and Episode 2 (starts "07jan2020") = 2 days (flagged)
  #   - Gap between Episode 2 and Episode 3 = 1 day (not flagged, because 1 is not > 1)
  result <- find_gap_episodes(test_data, min_gap = 1, gap_threshold = 3)
  
  # We expect one problematic gap (flagging Episode 2)
  expect_equal(nrow(result), 1)
  expect_equal(result$pers_id[1], "test_gap")
  expect_equal(result$previous_res_entry_id[1], "test_gap_1")
  expect_equal(result$current_res_entry_id[1], "test_gap_2")
  expect_equal(result$gap_days[1], 2)
  
  # Verify that previous_end and res_entry_start are consistently formatted
  expect_equal(result$previous_end[1], "05jan2020")
  expect_equal(result$res_entry_start[1], "07jan2020")
})

################################################################################
# Unit Tests for find_suspicious_start_dates and find_suspicious_end_dates
################################################################################

library(testthat)
library(dplyr)

test_that("find_suspicious_start_dates works as expected", {
  # Create sample PARL data (assumed to contain only NL data)
  PARL <- data.frame(
    parl_id = c("parl1", "parl2"),
    leg_period_start_posoxctformat = as.POSIXct(c("01Jan2020", "15Jan2020"), format = "%d%b%Y"),
    leg_period_end_posoxctformat   = as.POSIXct(c("31Jan2020", "15Feb2020"), format = "%d%b%Y"),
    stringsAsFactors = FALSE
  )
  
  # Create sample RESE data with various start dates.
  RESE <- data.frame(
    pers_id = c("p1", "p2", "p3", "p4"),
    res_entry_id = c("r1", "r2", "r3", "r4"),
    res_entry_start_posoxctformat = as.POSIXct(c("03Jan2020", "01Jan2020", "14Jan2020", "20Feb2020"), 
                                                format = "%d%b%Y"),
    res_entry_end_posoxctformat   = as.POSIXct(c("28Jan2020", "31Jan2020", "28Jan2020", "20Mar2020"), 
                                                format = "%d%b%Y"),
    stringsAsFactors = FALSE
  )
  
  # p1: diff = |03Jan2020 - 01Jan2020| = 2 days (suspicious)
  # p2: diff = |01Jan2020 - 01Jan2020| = 0 days (not suspicious)
  # p3: diff = min(|14Jan2020 - 01Jan2020| = 13, |14Jan2020 - 15Jan2020| = 1) => 1 day (suspicious)
  # p4: diff = min(|20Feb2020 - 01Jan2020| = 50, |20Feb2020 - 15Jan2020| = 36) => 36 days (outside threshold)
  
  result <- find_suspicious_start_dates(RESE, PARL, threshold_days = 14)
  
  # We expect entries for p1 and p3 only.
  expect_equal(nrow(result), 2)
  expect_true("p1" %in% result$pers_id)
  expect_true("p3" %in% result$pers_id)
  expect_false("p4" %in% result$pers_id)
  
  # Verify the computed differences:
  p1_row <- result[result$pers_id == "p1", ]
  expect_equal(p1_row$start_diff_days, 2)
  
  p3_row <- result[result$pers_id == "p3", ]
  expect_equal(p3_row$start_diff_days, 1)
})

test_that("find_suspicious_end_dates works as expected", {
  # Create sample PARL data (assumed to contain only NL data)
  PARL <- data.frame(
    parl_id = c("parl1", "parl2"),
    leg_period_start_posoxctformat = as.POSIXct(c("01Jan2020", "15Jan2020"), format = "%d%b%Y"),
    leg_period_end_posoxctformat   = as.POSIXct(c("31Jan2020", "15Feb2020"), format = "%d%b%Y"),
    stringsAsFactors = FALSE
  )
  
  # Create sample RESE data with various end dates.
  RESE <- data.frame(
    pers_id = c("p1", "p2", "p3", "p4"),
    res_entry_id = c("r1", "r2", "r3", "r4"),
    res_entry_start_posoxctformat = as.POSIXct(c("03Jan2020", "01Jan2020", "14Jan2020", "20Feb2020"), 
                                                format = "%d%b%Y"),
    res_entry_end_posoxctformat   = as.POSIXct(c("29Jan2020", "31Jan2020", "16Feb2020", "20Mar2020"), 
                                                format = "%d%b%Y"),
    stringsAsFactors = FALSE
  )
  
  # p1: end diff = |29Jan2020 - 31Jan2020| = 2 days (suspicious)
  # p2: end diff = |31Jan2020 - 31Jan2020| = 0 days (not suspicious)
  # p3: end diff = min(|16Feb2020 - 31Jan2020| = 16, |16Feb2020 - 15Feb2020| = 1) => 1 day (suspicious)
  # p4: end diff = min(|20Mar2020 - 31Jan2020| = 49, |20Mar2020 - 15Feb2020| = 33) => 33 days (outside threshold)
  
  result <- find_suspicious_end_dates(RESE, PARL, threshold_days = 14)
  
  # We expect entries for p1 and p3 only.
  expect_equal(nrow(result), 2)
  expect_true("p1" %in% result$pers_id)
  expect_true("p3" %in% result$pers_id)
  expect_false("p4" %in% result$pers_id)
  
  # Verify the computed differences:
  p1_row <- result[result$pers_id == "p1", ]
  expect_equal(p1_row$end_diff_days, 2)
  
  p3_row <- result[result$pers_id == "p3", ]
  expect_equal(p3_row$end_diff_days, 1)
})

################################################################################
# Unit Tests for format_pcc_date Function
################################################################################

test_that("format_pcc_date converts a single Date to PCC format", {
  expect_equal(format_pcc_date(as.Date("1959-11-05")), "05nov1959")
})

test_that("format_pcc_date uses lowercase month abbreviations", {
  expect_equal(format_pcc_date(as.Date("2020-03-15")), "15mar2020")
  expect_equal(format_pcc_date(as.Date("1946-01-01")), "01jan1946")
})

test_that("format_pcc_date is vectorised", {
  dates <- as.Date(c("2000-06-01", "2012-12-25"))
  expect_equal(format_pcc_date(dates), c("01jun2000", "25dec2012"))
})

test_that("format_pcc_date returns NA for NA input", {
  expect_true(is.na(format_pcc_date(NA)))
  result <- format_pcc_date(c(as.Date("2020-01-01"), NA))
  expect_equal(result[1], "01jan2020")
  expect_true(is.na(result[2]))
})

test_that("format_pcc_date works with POSIXct input", {
  expect_equal(format_pcc_date(as.POSIXct("1959-11-05 12:00:00")), "05nov1959")
})

################################################################################
# Unit Tests for check_special_chars_details (codebook: text must be ASCII)
################################################################################

test_that("clean ASCII data frame passes the special-char check", {
  df <- data.frame(
    pers_id   = c("NL_A_1", "NL_B_2"),
    last_name = c("Ellemeet", "Standerat"),
    note      = c("plain", "ascii only"),
    stringsAsFactors = FALSE
  )
  res <- check_special_chars_details(df, id_cols = c("pers_id"))
  expect_true(res$check_passed)
  expect_equal(res$special_char_count, 0L)
  expect_equal(nrow(res$special_char_rows), 0L)
})

test_that("diacritics and non-ASCII labels are flagged with their row + char", {
  df <- data.frame(
    pers_id   = c("CH_A_1", "CH_B_2", "CH_C_3"),
    last_name = c("Muëller", "Clean", "Zwïn"),      # e-diaeresis, i-diaeresis
    chamber   = c("Ständerat", "Nationalrat", "clean"),  # a-umlaut
    stringsAsFactors = FALSE
  )
  res <- check_special_chars_details(df, id_cols = c("pers_id"))
  expect_false(res$check_passed)
  # 2 offending last_name cells + 1 offending chamber cell = 3
  expect_equal(res$special_char_count, 3L)
  expect_setequal(unique(res$special_char_rows$column), c("last_name", "chamber"))
  # The offending row's id is carried through
  expect_true("CH_A_1" %in% res$special_char_rows$pers_id)
  # bad_chars isolates the offending glyph(s)
  chamber_row <- res$special_char_rows[res$special_char_rows$column == "chamber", ]
  expect_equal(chamber_row$bad_chars, "ä")
})

test_that("check_special_chars detection is encoding-tag independent", {
  # A UTF-8 byte sequence tagged as "unknown" (as read.csv often leaves it)
  # must still be caught: detection is byte-based, not encoding-based.
  x <- "çedilla"                 # c-cedilla + "edilla"
  Encoding(x) <- "unknown"
  df <- data.frame(pers_id = "X_1", first_name = x, stringsAsFactors = FALSE)
  res <- check_special_chars_details(df, id_cols = "pers_id")
  expect_false(res$check_passed)
  expect_equal(res$special_char_count, 1L)
})

test_that("check_special_chars scans factor columns and only text columns", {
  df <- data.frame(
    pers_id = c("A_1", "A_2"),
    faction = factor(c("Grüne", "SPD")),   # u-umlaut in a factor level
    seats   = c(10L, 20L),                        # numeric: never scanned
    stringsAsFactors = FALSE
  )
  res <- check_special_chars_details(df, id_cols = "pers_id")
  expect_false(res$check_passed)
  expect_equal(unique(res$special_char_rows$column), "faction")
  expect_false("seats" %in% res$special_char_rows$column)
})

test_that("check_special_chars ignores NA and empty strings", {
  df <- data.frame(
    pers_id = c("A_1", "A_2"),
    note    = c(NA_character_, ""),
    stringsAsFactors = FALSE
  )
  res <- check_special_chars_details(df, id_cols = "pers_id")
  expect_true(res$check_passed)
})

test_that("check_special_chars wrapper returns the logical only", {
  df <- data.frame(pers_id = "A_1", x = "Ständerat",
                   stringsAsFactors = FALSE)
  expect_false(check_special_chars(df, id_cols = "pers_id"))
  clean <- data.frame(pers_id = "A_1", x = "Standerat",
                      stringsAsFactors = FALSE)
  expect_true(check_special_chars(clean, id_cols = "pers_id"))
})

test_that("check_special_chars tolerates absent id_cols", {
  df <- data.frame(x = c("Ständerat", "clean"), stringsAsFactors = FALSE)
  res <- check_special_chars_details(df)          # no id_cols
  expect_false(res$check_passed)
  expect_equal(res$special_char_count, 1L)
  expect_true(all(c("column", "value", "bad_chars") %in%
                    names(res$special_char_rows)))
})

test_that("check_special_chars repairs stray latin1 bytes to valid UTF-8", {
  # read.csv leaves a stray latin1 byte (0xE7 = c-cedilla) tagged "unknown",
  # which is invalid UTF-8. Regression: this used to flow out unrepaired and
  # crash the on-click detail path (nchar -> "invalid multibyte string").
  x <- rawToChar(as.raw(c(0x46, 0x72, 0xE7, 0x6f)))   # "Fr<0xE7>o"
  expect_false(validUTF8(x))
  df <- data.frame(pers_id = "NL_X_1", first_name = x, stringsAsFactors = FALSE)
  res <- check_special_chars_details(df, id_cols = "pers_id")
  expect_false(res$check_passed)
  expect_equal(res$special_char_count, 1L)
  # value is now valid UTF-8 and the real glyph is recovered
  expect_true(all(validUTF8(res$special_char_rows$value)))
  expect_equal(res$special_char_rows$bad_chars, "ç")
  # the downstream ops that used to abort now succeed
  expect_silent(nchar(res$special_char_rows$value))
})
