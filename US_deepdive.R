# UNITED STATES DATA QUALITY DEEP DIVE
# Country-specific detailed investigation of data integrity issues
# Pre-import verification for BioGuide (congress-legislators) data

# SETUP
country_code <- "US"  # United States focus

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
data_path <- "/home/tomas/projects/ProjectR047_PCCIntegrity/Pre-IMPORT_data_verification/UnitedStates/"

POLI = read_csv_with_excel_sep(paste0(data_path, "POLI_import_ready.csv"), header = TRUE)
RESE = read_csv_with_excel_sep(paste0(data_path, "RESE_parlmem_import_ready.csv"), header = TRUE)
PARL = read_csv_with_excel_sep(paste0(data_path, "PARL_import_ready.csv"), header = TRUE)

cat("=== UNITED STATES DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded from Pre-IMPORT_data_verification/UnitedStates/:\n")
cat("- POLI:", nrow(POLI), "politicians\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# NOTE: Unlike other deepdive scripts, no country filtering needed here
# since the data is already US-specific from the pre-import folder

# SETTING
# Filter to membership episodes only? Toggle the next line on/off to focus
# on parliamentary membership episodes only.
# The US Congress is bicameral:
#   NT_LE-LH_T3_NA_01 = House of Representatives member
#   NT_LE-UH_T3_NA_01 = Senate member
resebeforepotentialresentryfilter <- nrow(RESE)
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE-UH_T3_NA_01")), ]
reseafterpotentialresentryfilter <- nrow(RESE)

table(RESE$political_function)

cat("Further rese filtering details:\n")
cat(ifelse(resebeforepotentialresentryfilter == reseafterpotentialresentryfilter,
           "- NO filter applied\n",
           paste0("- Filter applied to parliamentary membership episodes only\n",
                  "  (includes both House and Senate members)\n")))
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

# -----------------------------------------------------------------------------
# BICAMERAL SPLIT: Run checks 5-9 separately for House and Senate
# A person can legitimately serve in both chambers (sequentially or even
# with overlapping transition dates), so overlap checks must be
# chamber-specific to avoid false positives.
# -----------------------------------------------------------------------------

RESE_HR <- RESE[RESE$political_function == "NT_LE-LH_T3_NA_01", ]
RESE_SE <- RESE[RESE$political_function == "NT_LE-UH_T3_NA_01", ]

cat(sprintf("  Split RESE by chamber: HR=%d, SE=%d\n\n",
            nrow(RESE_HR), nrow(RESE_SE)))

for (chamber_label in c("HOUSE OF REPRESENTATIVES", "SENATE")) {
  RESE_chamber <- if (chamber_label == "HOUSE OF REPRESENTATIVES") RESE_HR else RESE_SE
  abb <- if (chamber_label == "HOUSE OF REPRESENTATIVES") "HR" else "SE"

  cat(sprintf("===== %s (N=%d) =====\n\n", chamber_label, nrow(RESE_chamber)))

  cat(sprintf("=== 5.%s DATE PREPROCESSING VALIDATION (RESE-%s) ===\n", abb, abb))
  rese_date_details <- check_anyNAinRESEdates_details(RESE_chamber)
  rese_date_details$check_passed
  rese_date_details$full_rows_with_na_dates

  cat(sprintf("=== 6.%s INVERTED DATES CHECK (RESE-%s) ===\n", abb, abb))
  inverted_dates_details <- check_RESE_inverted_dates_details(RESE_chamber)
  inverted_dates_details$check_passed
  inverted_dates_details$inverted_rows

  cat(sprintf("=== 7.%s PARLIAMENTARY MEMBERSHIP EPISODE FULL OVERLAPS (%s) ===\n", abb, abb))
  full_overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE_chamber)
  cat("  Check passed:", full_overlap_details$check_passed, "\n")
  if (!full_overlap_details$check_passed) {
    cat("  Overlapping episodes found:\n")
    print(full_overlap_details$overlapping_episodes)
  }

  cat(sprintf("=== 8.%s NEAR-OVERLAPPING EPISODES (%s) ===\n", abb, abb))
  near_overlap_details <- check_RESE_anynear_fulloverlap_details(RESE_chamber, tolerance_days = 2)
  cat("  Check passed:", near_overlap_details$check_passed, "\n")
  if (!near_overlap_details$check_passed) {
    cat("  Near-overlapping episode pairs found:\n")
    print(near_overlap_details$full_episode_pairs_near_overlapping)
  }

  cat(sprintf("=== 9.%s EPISODES PAST DEATH DATE (%s) ===\n", abb, abb))
  past_death_details <- check_RESE_episodes_past_death_details(RESE_chamber, POLI)
  cat("  Check passed:", past_death_details$check_passed, "\n")
  if (!past_death_details$check_passed) {
    cat("  Episodes past death date:\n")
    print(past_death_details$episodes_past_death)
  }
  cat("  Partial death date count:", past_death_details$partial_death_date_count, "\n")

  cat("\n")
}

# =============================================================================
# US-SPECIFIC INVESTIGATIONS
# =============================================================================

cat("=== 10. CHAMBER DISTRIBUTION ===\n")
# Check distribution of chamber types (bicameral)
RESE$chamber <- ifelse(RESE$political_function == "NT_LE-LH_T3_NA_01",
                       "House of Representatives",
                       "Senate")
cat("Distribution of chamber types:\n")
print(table(RESE$chamber))

cat("\n  House episodes:", sum(RESE$chamber == "House of Representatives"), "\n")
cat("  Senate episodes:", sum(RESE$chamber == "Senate"), "\n")

cat("\n=== 11. DATE FORMAT VALIDATION ===\n")
# Check date format patterns (PCC format: DDmonYYYY, e.g., "03jan1789")
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
  print(same_day_episodes[, c("res_entry_id", "pers_id", "res_entry_start", "res_entry_end", "chamber")])
}

cat("\n=== 13. PARLIAMENT PERIOD COVERAGE ===\n")
# Check which parliament periods have RESE entries
# Note: RESE parliament_ids may be semicolon-separated (multi-Congress terms)
rese_parliament_ids <- unique(unlist(strsplit(RESE$parliament_id, ";")))
parl_parliament_ids <- PARL$parliament_id
missing_in_rese <- setdiff(parl_parliament_ids, rese_parliament_ids)
missing_in_parl <- setdiff(rese_parliament_ids, parl_parliament_ids)

cat("PARL periods with no RESE entries:",
    ifelse(length(missing_in_rese) == 0, "NONE (all covered)",
           paste(length(missing_in_rese), "periods")), "\n")
if (length(missing_in_rese) > 0) {
  cat("  Missing:", paste(head(missing_in_rese, 10), collapse = ", "), "\n")
}
cat("RESE parliament_ids not in PARL:",
    ifelse(length(missing_in_parl) == 0, "NONE (all valid)", paste(missing_in_parl, collapse = ", ")), "\n")

cat("\n=== 14. BICAMERAL PARL STRUCTURE ===\n")
# Verify PARL has exactly 2 rows per Congress (HR + SE)
parl_by_assembly <- table(PARL$assembly_abb)
cat("PARL rows by assembly:\n")
print(parl_by_assembly)
cat("\nTotal Congresses covered:", nrow(PARL) / 2, "\n")

cat("\n=== 15. MULTI-CONGRESS TERMS (SENATE) ===\n")
# Senate terms span multiple Congresses — check distribution
n_semicolons <- nchar(gsub("[^;]", "", RESE$parliament_id))
multi_congress <- table(n_semicolons + 1)
cat("Number of Congresses per RESE episode:\n")
print(multi_congress)

cat("\n=== 16. BIRTH DATE COVERAGE ===\n")
cat("POLI birth_date coverage:\n")
cat("  Known:", sum(!is.na(POLI$birth_date)), "/", nrow(POLI),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(POLI$birth_date))))
cat("  Missing:", sum(is.na(POLI$birth_date)), "\n")

cat("\nDeath date coverage:\n")
cat("  Known:", sum(!is.na(POLI$death_date)), "/", nrow(POLI),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(POLI$death_date))))
cat("  Missing:", sum(is.na(POLI$death_date)),
    "(expected — death dates not yet enriched from Wikidata)\n")

cat("\nWikidata QID coverage:\n")
cat("  Known:", sum(!is.na(POLI$wikidata_id)), "/", nrow(POLI),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(POLI$wikidata_id))))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n=== SUMMARY ===\n")
cat("United States pre-import data quality checks completed.\n")
cat("Source: unitedstates/congress-legislators (BioGuide)\n")
cat("Total politicians: ", nrow(POLI), "\n")
cat("Total resume entries: ", nrow(RESE), "\n")
cat("Total parliament periods: ", nrow(PARL), "\n")
cat("  House of Representatives: ", sum(PARL$assembly_abb == "HR"), " periods\n")
cat("  Senate: ", sum(PARL$assembly_abb == "SE"), " periods\n")
cat("Date range: ", min(PARL$leg_period_start), " to ", max(PARL$leg_period_end), "\n")
