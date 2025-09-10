# R047 STREAMLINED - BASIC INTEGRITY CHECKS ONLY
# Uses new modular architecture with TRUE/FALSE checks
# For detailed investigations, use country-specific deepdive scripts
# For data fixes, use separate fix scripts

# SETUP
country_code <- "CH"  # Options: "NL" (Netherlands), "CH" (Switzerland)

# Set language and date formatting to English
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English")
setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Load required packages
library(sqldf)
library(stringr)
library(lubridate)
library(readr)
library(dplyr)
library(writexl)
library(openxlsx)
library(testthat)

# Load functions and run tests
source("R047_functions.R")
source("R047_RESE_functions.R")
source("R047_PARL_functions.R")

test_file("R047_unittests.R")
test_file("R047_RESE_unittests.R")
test_file("R047_PARL_unittests.R")

cat("=== R047 STREAMLINED DATA INTEGRITY CHECKS ===\n")
cat("Country:", country_code, "\n\n")

# LOAD DATA
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")

cat("Data loaded:\n")
cat("- POLI: N=", nrow(POLI), "politicians\n")
cat("- RESE: N=", nrow(RESE), "resume entries\n")
cat("- PARL: N=", nrow(PARL), "parliament periods\n\n")

# Filter to selected country
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]

cat("After country filtering:\n")
cat("- RESE: N=", nrow(RESE), "resume entries\n")
cat("- PARL: N=", nrow(PARL), "parliament periods\n\n")

# DATA PREPROCESSING
RESE <- preprocess_RESEdates(RESE)
PARL <- preprocess_PARLdates(PARL)

# =============================================================================
# CORE INTEGRITY CHECKS - TRUE/FALSE ONLY
# =============================================================================
#
resebeforepotentialresentryfilter <- nrow(RESE)
# SETTING
# Filter to membership episodes only? Toggle the next line on/off to focus
# RESE (and thus its checks) on lower house parliamentary membership only
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01")),]
reseafterpotentialresentryfilter <- nrow(RESE)

cat("Further rese filtering details:\n")
cat(ifelse(resebeforepotentialresentryfilter == reseafterpotentialresentryfilter,
           "- NO filter applied\n",
           "- Filter applied to parliamentary membership episodes only\n"))
cat("- RESE now has: N=", nrow(RESE), "resume entries\n")


cat("=== CORE INTEGRITY CHECKS ===\n")

# 1. Person ID validation
person_id_check <- check_RESE_persid_in_POLI(RESE, POLI)
cat("All RESE person IDs exist in POLI:", ifelse(person_id_check, "✅ PASS", "❌ FAIL"), "\n")

# 2. Resume entry ID uniqueness
entry_id_check <- check_RESE_resentryid_unique(RESE)
cat("All resume entry IDs are unique:", ifelse(entry_id_check, "✅ PASS", "❌ FAIL"), "\n")

# 3. Date preprocessing validation
rese_dates_check <- !check_anyNAinRESEdates(RESE)  # Note: function returns TRUE if NA found
cat("All RESE dates parsed successfully:", ifelse(rese_dates_check, "✅ PASS", "❌ FAIL"), "\n")

parl_dates_check <- !check_anyNAinPARLdates(PARL)  # Note: function returns TRUE if NA found  
cat("All PARL dates parsed successfully:", ifelse(parl_dates_check, "✅ PASS", "❌ FAIL"), "\n")

# 4. Parliamentary episode overlap checks
full_overlap_check <- !check_RESE_parlmemeppisodes_anyfulloverlap(RESE)  # Note: function returns TRUE if overlaps found
cat("No fully overlapping parliamentary episodes:", ifelse(full_overlap_check, "✅ PASS", "❌ FAIL"), "\n")

near_overlap_check <- !check_RESE_anynear_fulloverlap(RESE, tolerance_days = 2)  # Note: function returns TRUE if overlaps found
cat("No near-overlapping episodes (2 days):", ifelse(near_overlap_check, "✅ PASS", "❌ FAIL"), "\n")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n=== INTEGRITY CHECK SUMMARY ===\n")

all_checks <- c(person_id_check, entry_id_check, rese_dates_check, parl_dates_check, full_overlap_check, near_overlap_check)
checks_passed <- sum(all_checks)
total_checks <- length(all_checks)

cat("Checks passed:", checks_passed, "/", total_checks, "\n")

if (checks_passed == total_checks) {
  cat("🎉 ALL CHECKS PASSED - Data integrity validated\n")
} else {
  cat("⚠️  ISSUES FOUND - Use detailed investigation scripts:\n")
  
  if (!person_id_check) cat("  - Run deepdive script for missing person ID details\n")
  if (!entry_id_check) cat("  - Run deepdive script for duplicate entry ID details\n")
  if (!rese_dates_check) cat("  - Run deepdive script for RESE date parsing details\n")
  if (!parl_dates_check) cat("  - Run deepdive script for PARL date parsing details\n")
  if (!full_overlap_check) cat("  - Run 'fixing_projects/overlapping_episodes_fixes.R' to generate merged episodes\n")
  if (!near_overlap_check) cat("  - Run deepdive script for near-overlap details\n")
  
  cat("\nCountry-specific deepdive scripts:\n")
  cat("  - CH_deepdive.R (for Switzerland detailed investigation)\n")
  cat("  - NL_deepdive.R (for Netherlands detailed investigation)\n")
  
  cat("\nData fix scripts (in fixing_projects/ folder):\n")
  cat("  - fixing_projects/CH_chamber_function_fixes.R (Swiss chamber political function corrections)\n")
  cat("  - fixing_projects/overlapping_episodes_fixes.R (merge overlapping episodes)\n")
}

cat("\n=== R047 STREAMLINED COMPLETE ===\n")
cat("For detailed investigations and fixes, use the more focussed scripts mentioned above.\n")