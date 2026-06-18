# NETHERLANDS DATA QUALITY DEEP DIVE
# Country-specific detailed investigation of data integrity issues

# SETUP
country_code <- "NL"  # Netherlands focus

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
source("R047_MEME_functions.R")

# Run unit tests to ensure functions work correctly
test_file("R047_unittests.R")
test_file("R047_RESE_unittests.R")
test_file("R047_PARL_unittests.R")
test_file("R047_MEME_unittests.R")

# LOAD DATA
# Import PCC datasets
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
MEME = read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
PART = read.csv("/home/tomas/projects/PCCdata/PART.csv", header = TRUE, sep = ";")

cat("=== NETHERLANDS DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded:\n")
cat("- POLI:", nrow(POLI), "politicians\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n")
cat("- MEME:", nrow(MEME), "party membership episodes\n")
cat("- PART:", nrow(PART), "party records\n\n")

# Filter to Netherlands data
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]
MEME <- MEME[which(substr(MEME$pers_id, 1, nchar(country_code)) == country_code), ]

cat("After filtering to Netherlands:\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n")
cat("- MEME:", nrow(MEME), "party membership episodes\n\n")

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
MEME <- suppressMessages(preprocess_MEMEdates(MEME))

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

cat("=== 5. DATE PREPROCESSING VALIDATION ===\n")
rese_date_details <- check_anyNAinRESEdates_details(RESE)
names(rese_date_details)
rese_date_details$check_passed
rese_date_details$full_rows_with_na_dates

cat("=== 6. INVERTED DATES CHECK ===\n")
inverted_dates_details <- check_RESE_inverted_dates_details(RESE)
names(inverted_dates_details)
inverted_dates_details$check_passed
inverted_dates_details$inverted_rows

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

# Pairs that have been manually verified as different people (not duplicates)
verified_not_duplicates <- data.frame(
  pers_id_1 = c("NL_Suurhoff_Ko_1905"),
  pers_id_2 = c("NL_Venverloo_Albert_1905"),
  stringsAsFactors = FALSE
)

cat("\n=== 8b. POTENTIAL POLI DUPLICATES (same birthday in same faction) ===\n")
birthdate_dup_details <- check_RESE_duplicate_birthdates_in_faction_details(
  RESE, POLI, PARL, MEME, "TK", verified_pairs = verified_not_duplicates)
cat("Check passed:", birthdate_dup_details$check_passed, "\n")
cat("MEME available:", birthdate_dup_details$meme_available, "\n")
cat("Parliaments checked:", birthdate_dup_details$parliaments_checked, "\n")
cat("Flagged pair-parliament instances:", birthdate_dup_details$flagged_count, "\n")
if (!birthdate_dup_details$check_passed) {
  unique_pairs <- unique(birthdate_dup_details$flagged_pairs[,
    c("party_id", "birth_date", "pers_id_1", "name_1", "pers_id_2", "name_2")])
  cat("Unique pairs (deduplicated across parliaments):", nrow(unique_pairs), "\n")
  cat("\nFlagged pairs (potential duplicates):\n")
  print(unique_pairs, row.names = FALSE)
}

cat("=== 9. EPISODES PAST DEATH DATE ===\n")
past_death_details <- check_RESE_episodes_past_death_details(RESE, POLI)
names(past_death_details)
past_death_details$check_passed
past_death_details$episodes_past_death
past_death_details$partial_death_date_count
past_death_details$episodes_partial_death_date

# =============================================================================
# OTHER NETHERLANDS-SPECIFIC INVESTIGATIONS
# =============================================================================

cat("=== 9. DUTCH DATE FORMAT VALIDATION ===\n")
# Check date format patterns (Dutch data has some 7-char dates like "aug2012")
start_date_lengths <- table(nchar(RESE$res_entry_start))
end_date_lengths <- table(nchar(RESE$res_entry_end))
start_date_lengths
end_date_lengths

# Check for 7-character end dates (common in NL data)
unusual_end_dates <- RESE[which(nchar(RESE$res_entry_end) == 7), ]

cat("=== 10. DATE LOGIC VALIDATION ===\n")
# Check if start dates are before end dates
valid_date_order <- table(RESE$res_entry_start_posoxctformat < RESE$res_entry_end_posoxctformat)
valid_date_order

cat("=== 11. GAP DETECTION ===\n")
# Find episodes that are very close together (1-3 day gaps)
gap_episodes <- find_gap_episodes(RESE, 1, 3)

cat("=== 12. SUSPICIOUS DATE DETECTION ===\n")
# Find dates that don't align well with parliamentary periods
suspicious_start_dates <- find_suspicious_start_dates(RESE, PARL, threshold_days = 3)
suspicious_end_dates <- find_suspicious_end_dates(RESE, PARL, threshold_days = 3)

# =============================================================================
# MEME (PARTY MEMBERSHIP) INVESTIGATIONS
# =============================================================================

cat("=== 13. MEME PERSON ID VALIDATION ===\n")
meme_persid_details <- check_MEME_persid_in_POLI_details(MEME, POLI)
cat("Check passed:", meme_persid_details$check_passed, "\n")
cat("Missing person IDs:", meme_persid_details$missing_count, "\n")
if (!meme_persid_details$check_passed) {
  cat("Person IDs in MEME but not in POLI:\n")
  print(meme_persid_details$missing_ids)
}

cat("\n=== 14. MEME PARTY ID REFERENTIAL INTEGRITY ===\n")
meme_party_details <- check_MEME_partyid_in_PART_details(MEME, PART)
cat("Check passed:", meme_party_details$check_passed, "\n")
cat("Missing party IDs:", meme_party_details$missing_count, "\n")
if (!meme_party_details$check_passed) {
  cat("Party IDs in MEME but not in PART:\n")
  print(meme_party_details$missing_ids)
}

cat("\n=== 15. MEME EPISODE ID UNIQUENESS ===\n")
meme_dup_details <- check_MEME_memepid_unique_details(MEME)
cat("Check passed:", meme_dup_details$check_passed, "\n")
cat("Duplicate IDs found:", meme_dup_details$duplicate_count, "\n")
if (!meme_dup_details$check_passed) {
  cat("\nDuplicate memep_id values:\n")
  print(meme_dup_details$duplicate_ids)
  cat("\nAll rows with duplicate IDs:\n")
  print(meme_dup_details$duplicate_rows[, c("memep_id", "pers_id", "party_id",
                                             "memep_startdate", "memep_enddate")])
}

cat("\n=== 16. MEME DATE PREPROCESSING VALIDATION ===\n")
meme_date_details <- check_anyNAinMEMEdates_details(MEME)
cat("Check passed (no NA start dates):", meme_date_details$check_passed, "\n")
cat("NA start dates:", meme_date_details$na_start_count, "\n")
cat("NA end dates (informational, ongoing memberships):", meme_date_details$na_end_count, "\n")
if (!meme_date_details$check_passed) {
  cat("\nRows with NA start dates:\n")
  print(meme_date_details$full_rows_with_na_startdates)
}

cat("\n=== 17. MEME INVERTED DATES CHECK ===\n")
meme_inverted_details <- check_MEME_inverted_dates_details(MEME)
cat("Check passed:", meme_inverted_details$check_passed, "\n")
cat("Inverted episodes:", meme_inverted_details$inverted_count, "\n")
if (!meme_inverted_details$check_passed) {
  cat("\nEpisodes where end < start:\n")
  print(meme_inverted_details$inverted_rows[, c("memep_id", "pers_id", "party_id",
                                                  "memep_startdate", "memep_enddate",
                                                  "date_diff_days")])
}

cat("\n=== 18. MEME DUPLICATE EPISODES ===\n")
meme_overlap_details <- check_MEME_anyfulloverlap_details(MEME)
cat("Check passed:", meme_overlap_details$check_passed, "\n")
cat("Duplicate episodes:", meme_overlap_details$overlap_count, "\n")
if (!meme_overlap_details$check_passed) {
  cat("Affected persons:", paste(meme_overlap_details$affected_persons, collapse = ", "), "\n")
  cat("\nDuplicate episode rows:\n")
  print(meme_overlap_details$overlapping_episodes[, c("memep_id", "pers_id", "party_id",
                                                       "memep_startdate", "memep_enddate")])
}

cat("\n=== 19. MEME PARTY COVERAGE FOR MPs ===\n")
meme_coverage_details <- check_MEME_parlmembers_have_party_details(RESE, MEME)
cat("Check passed:", meme_coverage_details$check_passed, "\n")
cat("MPs with party data:", meme_coverage_details$total_parlmembers - meme_coverage_details$missing_count,
    "/", meme_coverage_details$total_parlmembers, "\n")
if (!meme_coverage_details$check_passed) {
  cat("MPs missing from MEME:\n")
  print(meme_coverage_details$missing_ids)
}

