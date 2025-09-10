# SWITZERLAND DATA QUALITY DEEP DIVE
# Country-specific detailed investigation of data integrity issues

# SETUP
country_code <- "CH"  # Switzerland focus

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

cat("=== SWITZERLAND DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded:\n")
cat("- POLI: N=", nrow(POLI), "politicians\n")
cat("- RESE: N=", nrow(RESE), "resume entries\n") 
cat("- PARL: N=", nrow(PARL), "parliament periods\n\n")

# Filter to Switzerland data
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]

cat("After filtering to Switzerland:\n")
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
cat("=== 2. DATE PREPROCESSING VALIDATION ===\n")
parl_date_details <- check_anyNAinPARLdates_details(PARL, level = "NT")
names(parl_date_details)
parl_date_details$check_passed
parl_date_details$full_rows_with_na_dates

#
# RESE checks
#
cat("=== 1. PERSON ID VALIDATION ===\n")
person_id_details <- check_RESE_persid_in_POLI_details(RESE, POLI)
names(person_id_details)
person_id_details$check_passed
person_id_details$missing_ids

cat("=== 2. RESUME ENTRY ID UNIQUENESS ===\n")
entry_id_details <- check_RESE_resentryid_unique_details(RESE)
names(entry_id_details)
entry_id_details$check_passed
entry_id_details$duplicate_ids

cat("=== 3. DATE PREPROCESSING VALIDATION ===\n")
rese_date_details <- check_anyNAinRESEdates_details(RESE)
names(rese_date_details)
rese_date_details$check_passed
rese_date_details$full_rows_with_na_dates

cat("=== 4. PARLIAMENTARY MEMBERSHIP EPISODE OVERLAPS ===\n")
full_overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE)
names(full_overlap_details)
full_overlap_details$overlapping_episodes

cat("=== 5. NEAR-OVERLAPPING EPISODES ===\n")
near_overlap_details <- check_RESE_anynear_fulloverlap_details(RESE, tolerance_days = 2)
names(near_overlap_details)
near_overlap_details$check_passed
near_overlap_details$full_episode_pairs_near_overlapping

# =============================================================================
# ANOTHER DETAILLED ISSUE INSPECTION: Nationalrat and Staenderat don't seem to have a different political function code.
# =============================================================================

# Create chamber classifications
RESE$chamber <- ifelse(grepl("NT-NR", RESE$parliament_id), "NR", 
                       ifelse(grepl("NT-SR", RESE$parliament_id), "SR", "OTHER"))

table(RESE$chamber)

# Check for consistency with raw text
nationalrat_patterns <- c("Nationalrat", "National Council", "Lower House", "Erste Kammer", "\\\\bNR\\\\b", "Conseil national")
staenderat_patterns <- c("Ständerat", "Staenderat", "Council of States", "Upper House", "Zweite Kammer", "\\\\bSR\\\\b", "Conseil des États")

has_nationalrat_text <- grepl(paste(nationalrat_patterns, collapse = "|"), RESE$res_entry_raw, ignore.case = TRUE)
has_staenderat_text <- grepl(paste(staenderat_patterns, collapse = "|"), RESE$res_entry_raw, ignore.case = TRUE)

inconsistent_nr_rows <- RESE[RESE$chamber == "NR" & has_staenderat_text, ]
inconsistent_sr_rows <- RESE[RESE$chamber == "SR" & has_nationalrat_text, ]

# both rows below are empty, so we can safely use the NR or SR label from parliament_id to generate updated political functions code for an IMPORT.
# see the folder /fixing_projects
inconsistent_nr_rows 
inconsistent_sr_rows
