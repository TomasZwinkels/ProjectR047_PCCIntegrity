################################################################################
# Unit Tests for merge_episodes Function
################################################################################

library(testthat)
library(dplyr)
library(lubridate)

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
