# NORWAY DATA QUALITY DEEP DIVE
# Country-specific detailed investigation of data integrity issues
# Pre-import verification for Stortinget data

# SETUP
country_code <- "NO"  # Norway focus

# Load required packages
library(sqldf)
library(stringr)
library(lubridate)
library(readr)
library(dplyr)
library(writexl)
library(openxlsx)
library(testthat)

# Set working directory and load functions
setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Load custom functions
source("R047_functions.R")
source("R047_RESE_functions.R")
source("R047_PARL_functions.R")

# Run unit tests to ensure functions work correctly
test_file("R047_unittests.R")
test_file("R047_RESE_unittests.R")
test_file("R047_PARL_unittests.R")

# LOAD DATA
# Import pre-import verification datasets (comma-separated CSVs from R052)
data_path <- "/home/tomas/projects/ProjectR047_PCCIntegrity/Pre-IMPORT_data_verification/Norway/"

POLI = read_csv_with_excel_sep(paste0(data_path, "POLI_import_ready.csv"), header = TRUE)
RESE = read_csv_with_excel_sep(paste0(data_path, "RESE_import_ready.csv"), header = TRUE)
PARL = read_csv_with_excel_sep(paste0(data_path, "PARL_import_ready.csv"), header = TRUE)

cat("=== NORWAY DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded from Pre-IMPORT_data_verification/Norway/:\n")
cat("- POLI:", nrow(POLI), "politicians\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# NOTE: Unlike other deepdive scripts, no country filtering needed here
# since the data is already Norway-specific from the pre-import folder

# SETTING
# Filter to membership episodes only? Toggle the next line on/off to focus
# on parliamentary membership episodes only (Norway's Storting is unicameral)
# NT_LE_T3_NA_01 = full representative (Stortingsrepresentant)
# NT_LE_T3_NA_09 = substitute representative (Vararepresentant)
resebeforepotentialresentryfilter <- nrow(RESE)
RESE <- RESE[which(RESE$political_function %in% c("NT_LE_T3_NA_01", "NT_LE_T3_NA_09")), ]
reseafterpotentialresentryfilter <- nrow(RESE)

cat("Further rese filtering details:\n")
cat(ifelse(resebeforepotentialresentryfilter == reseafterpotentialresentryfilter,
           "- NO filter applied\n",
           paste0("- Filter applied to parliamentary membership episodes only\n",
                  "  (includes both Stortingsrepresentant and Vararepresentant)\n")))
cat("- RESE now has: N=", nrow(RESE), "resume entries\n\n")

# PREPROCESS DATES (suppress validation messages - detailed analysis follows)
RESE <- suppressMessages(preprocess_RESEdates(RESE))
PARL <- suppressMessages(preprocess_PARLdates(PARL))

# =============================================================================
# DETAILED DATA QUALITY INVESTIGATIONS
# =============================================================================

#
# PARL checks
#
cat("=== 1. DATE PREPROCESSING VALIDATION (PARL) ===\n")
parl_date_details <- check_anyNAinPARLdates_details(PARL, level = "NT")
names(parl_date_details)
parl_date_details$check_passed
parl_date_details$full_rows_with_na_dates

cat("=== 2. PARLIAMENT SIZE VALIDATION ===\n")
parl_size_details <- check_PARL_parliament_size_meaningful_details(PARL, level = "NT")
names(parl_size_details)
parl_size_details$check_passed
parl_size_details$full_rows_with_problems

#
# RESE checks
#
cat("=== 3. PERSON ID VALIDATION ===\n")
person_id_details <- check_RESE_persid_in_POLI_details(RESE, POLI)
names(person_id_details)
person_id_details$check_passed
person_id_details$missing_ids

cat("=== 4. RESUME ENTRY ID UNIQUENESS ===\n")
entry_id_details <- check_RESE_resentryid_unique_details(RESE)
names(entry_id_details)
entry_id_details$check_passed
entry_id_details$duplicate_ids

cat("=== 5. DATE PREPROCESSING VALIDATION (RESE) ===\n")
rese_date_details <- check_anyNAinRESEdates_details(RESE)
names(rese_date_details)
rese_date_details$check_passed
rese_date_details$full_rows_with_na_dates

cat("=== 6. INVERTED DATES CHECK ===\n")
inverted_dates_details <- check_RESE_inverted_dates_details(RESE)
names(inverted_dates_details)
inverted_dates_details$check_passed
inverted_dates_details$inverted_rows

RESE[which(RESE$res_entry_start == "30sep1973"),]

cat("=== 7. PARLIAMENTARY MEMBERSHIP EPISODE OVERLAPS ===\n")
full_overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE)
names(full_overlap_details)
full_overlap_details$check_passed
full_overlap_details$overlapping_episodes

cat("=== 8. NEAR-OVERLAPPING EPISODES ===\n")
near_overlap_details <- check_RESE_anynear_fulloverlap_details(RESE, tolerance_days = 2)
names(near_overlap_details)
near_overlap_details$check_passed
near_overlap_details$full_episode_pairs_near_overlapping

cat("=== 9. EPISODES PAST DEATH DATE ===\n")
past_death_details <- check_RESE_episodes_past_death_details(RESE, POLI)
names(past_death_details)
past_death_details$check_passed
past_death_details$episodes_past_death

# =============================================================================
# NORWAY-SPECIFIC INVESTIGATIONS
# =============================================================================

cat("=== 10. REPRESENTATIVE TYPE DISTRIBUTION ===\n")
# Check distribution of representative types
RESE$rep_type <- ifelse(RESE$political_function == "NT_LE_T3_NA_01",
                        "Stortingsrepresentant",
                        "Vararepresentant")
cat("Distribution of representative types:\n")
print(table(RESE$rep_type))

cat("\n=== 11. DATE FORMAT VALIDATION ===\n")
# Check date format patterns (PCC format: DDmonYYYY, e.g., "30sep2021")
start_date_lengths <- table(nchar(RESE$res_entry_start))
end_date_lengths <- table(nchar(RESE$res_entry_end))
cat("Start date character lengths:\n")
print(start_date_lengths)
cat("\nEnd date character lengths:\n")
print(end_date_lengths)

cat("\n=== 12. DATE LOGIC VALIDATION ===\n")
# Check if start dates are before end dates
valid_date_order <- table(RESE$res_entry_start_posoxctformat < RESE$res_entry_end_posoxctformat)
cat("Start date < End date (TRUE = valid):\n")
print(valid_date_order)

# Episodes where start == end (potentially problematic single-day episodes)
same_day_episodes <- RESE[which(RESE$res_entry_start_posoxctformat == RESE$res_entry_end_posoxctformat), ]
if (nrow(same_day_episodes) > 0) {
  cat("\nWARNING: Found", nrow(same_day_episodes), "episodes where start date equals end date:\n")
  print(same_day_episodes[, c("res_entry_id", "pers_id", "res_entry_start", "res_entry_end", "rep_type")])
}

cat("\n=== 13. PARLIAMENT PERIOD COVERAGE ===\n")
# Check which parliament periods have RESE entries
rese_parliament_ids <- unique(RESE$parliament_id)
parl_parliament_ids <- PARL$parliament_id
missing_in_rese <- setdiff(parl_parliament_ids, rese_parliament_ids)
missing_in_parl <- setdiff(rese_parliament_ids, parl_parliament_ids)

cat("PARL periods with no RESE entries:",
    ifelse(length(missing_in_rese) == 0, "NONE (all covered)", paste(missing_in_rese, collapse = ", ")), "\n")
cat("RESE parliament_ids not in PARL:",
    ifelse(length(missing_in_parl) == 0, "NONE (all valid)", paste(missing_in_parl, collapse = ", ")), "\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n=== SUMMARY ===\n")
cat("Norway pre-import data quality checks completed.\n")
cat("Total politicians: ", nrow(POLI), "\n")
cat("Total resume entries: ", nrow(RESE), "\n")
cat("Total parliament periods: ", nrow(PARL), "\n")
cat("Date range: ", min(PARL$leg_period_start), " to ", max(PARL$leg_period_end), "\n")
