# GERMANY DATA QUALITY DEEP DIVE
# Country-specific detailed investigation of data integrity issues

# SETUP
country_code <- "DE"  # Germany focus

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
# Import PCC datasets
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")

cat("=== GERMANY DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded:\n")
cat("- POLI:", nrow(POLI), "politicians\n")
cat("- RESE:", nrow(RESE), "resume entries\n") 
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# Filter to Germany data
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]

cat("After filtering to Germany:\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# SETTING
# Filter to membership episodes only? Toggle the next line on/off to focus
# on lower house parliamentary membership episodes only
resebeforepotentialresentryfilter <- nrow(RESE)
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01")), ]
reseafterpotentialresentryfilter <- nrow(RESE)

cat("Further rese filtering details:\n")
cat(ifelse(resebeforepotentialresentryfilter == reseafterpotentialresentryfilter,
           "- NO filter applied\n",
           "- Filter applied to parliamentary membership episodes only\n"))
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
cat("=== 1. DATE PREPROCESSING VALIDATION ===\n")
parl_date_details <- check_anyNAinPARLdates_details(PARL, level = "NT")
names(parl_date_details)
parl_date_details$check_passed
parl_date_details$full_rows_with_na_dates

#
# RESE checks
#
cat("=== 2. PERSON ID VALIDATION ===\n")
person_id_details <- check_RESE_persid_in_POLI_details(RESE, POLI)
names(person_id_details)
person_id_details$check_passed
person_id_details$missing_ids

cat("=== 3. RESUME ENTRY ID UNIQUENESS ===\n")
entry_id_details <- check_RESE_resentryid_unique_details(RESE)
names(entry_id_details)
entry_id_details$check_passed
entry_id_details$duplicate_ids

cat("=== 4. DATE PREPROCESSING VALIDATION ===\n")
rese_date_details <- check_anyNAinRESEdates_details(RESE)
names(rese_date_details)
rese_date_details$check_passed
rese_date_details$full_rows_with_na_dates

cat("=== 5. PARLIAMENTARY MEMBERSHIP EPISODE OVERLAPS ===\n")
full_overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE)
names(full_overlap_details)
full_overlap_details$check_passed
full_overlap_details$overlapping_episodes

cat("=== 6. NEAR-OVERLAPPING EPISODES ===\n")
near_overlap_details <- check_RESE_anynear_fulloverlap_details(RESE, tolerance_days = 2)
names(near_overlap_details)
near_overlap_details$check_passed
near_overlap_details$full_episode_pairs_near_overlapping

# =============================================================================
# OTHER GERMANY-SPECIFIC INVESTIGATIONS  
# =============================================================================

cat("=== 7. GERMAN DATE FORMAT VALIDATION ===\n")
# Check date format patterns (German data may have specific formatting)
start_date_lengths <- table(nchar(RESE$res_entry_start))
end_date_lengths <- table(nchar(RESE$res_entry_end))
start_date_lengths
end_date_lengths

# Check for unusual date formats
unusual_start_dates <- RESE[which(nchar(RESE$res_entry_start) < 8), ]
unusual_end_dates <- RESE[which(nchar(RESE$res_entry_end) < 8), ]

cat("=== 8. DATE LOGIC VALIDATION ===\n")
# Check if start dates are before end dates
valid_date_order <- table(RESE$res_entry_start_posoxctformat < RESE$res_entry_end_posoxctformat)
valid_date_order

cat("=== 9. GAP DETECTION ===\n")
# Find episodes that are very close together (1-3 day gaps)
gap_episodes <- find_gap_episodes(RESE, 1, 3)

cat("=== 10. SUSPICIOUS DATE DETECTION ===\n")
# Find dates that don't align well with parliamentary periods
suspicious_start_dates <- find_suspicious_start_dates(RESE, PARL, threshold_days = 3)
suspicious_end_dates <- find_suspicious_end_dates(RESE, PARL, threshold_days = 3)

cat("=== 11. GERMAN POLITICAL FUNCTION ANALYSIS ===\n")
# Analyze political function codes specific to Germany
german_functions <- table(RESE$political_function)
german_functions

# Check for missing political function patterns
missing_function_codes <- RESE[is.na(RESE$political_function) | RESE$political_function == "", ]

cat("=== 12. BUNDESTAG EPISODE CONTINUITY ===\n")
# Check for gaps in Bundestag membership (German parliament specific)
# Sort by person and date to identify potential gaps
rese_sorted <- RESE[order(RESE$pers_id, RESE$res_entry_start_posoxctformat), ]

# Find potential gaps between consecutive episodes for same person
person_gaps <- find_person_episode_gaps(rese_sorted, max_gap_days = 30)

cat("=== GERMANY DEEP DIVE COMPLETE ===\n")
cat("Detailed investigation results available in workspace variables:\n")
cat("- parl_date_details, person_id_details, entry_id_details\n")
cat("- rese_date_details, full_overlap_details, near_overlap_details\n")
cat("- gap_episodes, suspicious_start_dates, suspicious_end_dates\n")
cat("- german_functions, person_gaps\n\n")
cat("For data fixes, use scripts in fixing_projects/ folder\n")