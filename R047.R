# R047 STREAMLINED - BASIC INTEGRITY CHECKS ONLY
# Uses new modular architecture with TRUE/FALSE checks
# For detailed investigations, use country-specific deepdive scripts
# For data fixes, use separate fix scripts

# SETUP
country_code <- "NL"  # Options: "NL" (Netherlands), "CH" (Switzerland), "DE" (Germany), "CA" (Canada), "US" (United States)
date_from    <- as.Date("1946-01-01") # currently used only by check_RESE_parlmem_coverage (#16)
date_to      <- Sys.Date() # currently used only by check_RESE_parlmem_coverage (#16)

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
source("R047_MEME_functions.R")

test_file("R047_unittests.R")
test_file("R047_RESE_unittests.R")
test_file("R047_PARL_unittests.R")
test_file("R047_MEME_unittests.R")

cat("=== R047 STREAMLINED DATA INTEGRITY CHECKS ===\n")
cat("Country:", country_code, "\n\n")

# LOAD DATA
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
MEME = read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
PART = read.csv("/home/tomas/projects/PCCdata/PART.csv", header = TRUE, sep = ";")

cat("Data loaded:\n")
cat("- POLI: N=", nrow(POLI), "politicians\n")
cat("- RESE: N=", nrow(RESE), "resume entries\n")
cat("- PARL: N=", nrow(PARL), "parliament periods\n")
cat("- MEME: N=", nrow(MEME), "party membership episodes\n")
cat("- PART: N=", nrow(PART), "party records\n\n")

# Filter to selected country
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]
MEME <- MEME[which(substr(MEME$pers_id, 1, nchar(country_code)) == country_code), ]

cat("After country filtering:\n")
cat("- RESE: N=", nrow(RESE), "resume entries\n")
cat("- PARL: N=", nrow(PARL), "parliament periods\n")
cat("- MEME: N=", nrow(MEME), "party membership episodes\n\n")

# DATA PREPROCESSING
RESE <- preprocess_RESEdates(RESE)
PARL <- preprocess_PARLdates(PARL)
MEME <- preprocess_MEMEdates(MEME)

# =============================================================================
# CORE INTEGRITY CHECKS - TRUE/FALSE ONLY
# =============================================================================
#
resebeforepotentialresentryfilter <- nrow(RESE)
# SETTING
# Filter to membership episodes only? Toggle the next line on/off to focus
# RESE (and thus its checks) on parliamentary membership only
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]
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

parl_dates_check <- !check_anyNAinPARLdates(PARL, level = "NT")  # Note: function returns TRUE if NA found, only check national level  
cat("All PARL dates parsed successfully:", ifelse(parl_dates_check, "✅ PASS", "❌ FAIL"), "\n")

# 4. Parliament size validation
parl_size_check <- check_PARL_parliament_size_meaningful(PARL, level = "NT")  # Note: function returns TRUE if all sizes are meaningful
cat("All PARL parliament sizes are meaningful:", ifelse(parl_size_check, "✅ PASS", "❌ FAIL"), "\n")

# 5. Parliamentary episode overlap checks
full_overlap_check <- !check_RESE_parlmemeppisodes_anyfulloverlap(RESE)  # Note: function returns TRUE if overlaps found
cat("No fully overlapping parliamentary episodes:", ifelse(full_overlap_check, "✅ PASS", "❌ FAIL"), "\n")

near_overlap_check <- !check_RESE_anynear_fulloverlap(RESE, tolerance_days = 2)  # Note: function returns TRUE if overlaps found
cat("No near-overlapping episodes (2 days):", ifelse(near_overlap_check, "✅ PASS", "❌ FAIL"), "\n")

# 6. Potential POLI duplicates (same birthday in same faction, requires MEME)
# Pairs verified as genuinely different people (not duplicates)
verified_not_duplicates <- data.frame(
  pers_id_1 = c("NL_Suurhoff_Ko_1905",
                 "NL_vanBuel_Ben_1913",
                 "NL_Pronk_Jan_1940",
                 "NL_Esselink_Berry_1944",
                 "NL_vanderWal_Christianne_1973"),
  pers_id_2 = c("NL_Venverloo_Albert_1905",
                 "NL_Walburg_Tjebbe_1913",
                 "NL_Schaefer_Jan_1940",
                 "NL_vanVoorsttotVoorst_Berend_1944",
                 "NL_Veltman_Hester_1973"),
  stringsAsFactors = FALSE
)
assembly_map <- c(CA = "HC", CH = "NR", DE = "BT", NL = "TK", NO = "ST", US = "HR")
birthdate_dup_check <- !check_RESE_duplicate_birthdates_in_faction(
  RESE, POLI, PARL, MEME, assembly_map[country_code],
  verified_pairs = verified_not_duplicates)
cat("No same-birthday duplicates in factions:", ifelse(birthdate_dup_check, "✅ PASS", "❌ FAIL"), "\n")

# 7. MEME integrity checks
cat("\n--- MEME (Party Membership) Checks ---\n")

meme_persid_check <- check_MEME_persid_in_POLI(MEME, POLI)
cat("All MEME person IDs exist in POLI:", ifelse(meme_persid_check, "✅ PASS", "❌ FAIL"), "\n")

meme_partyid_check <- check_MEME_partyid_in_PART(MEME, PART)
cat("All MEME party IDs exist in PART:", ifelse(meme_partyid_check, "✅ PASS", "❌ FAIL"), "\n")

meme_memepid_check <- check_MEME_memepid_unique(MEME)
cat("All MEME episode IDs are unique:", ifelse(meme_memepid_check, "✅ PASS", "❌ FAIL"), "\n")

meme_dates_check <- !check_anyNAinMEMEdates(MEME)  # Note: function returns TRUE if NA found
cat("All MEME start dates parsed successfully:", ifelse(meme_dates_check, "✅ PASS", "❌ FAIL"), "\n")

meme_inverted_check <- !check_MEME_inverted_dates(MEME)  # Note: function returns TRUE if inversions found
cat("No inverted MEME dates:", ifelse(meme_inverted_check, "✅ PASS", "❌ FAIL"), "\n")

meme_overlap_check <- !check_MEME_anyfulloverlap(MEME)  # Note: function returns TRUE if overlaps found
cat("No duplicate MEME episodes:", ifelse(meme_overlap_check, "✅ PASS", "❌ FAIL"), "\n")

meme_party_coverage_check <- check_MEME_parlmembers_have_party(RESE, MEME)
cat("All MPs have party membership data:", ifelse(meme_party_coverage_check, "✅ PASS", "❌ FAIL"), "\n")

# 16. Parliamentary membership data coverage across date range
parlmem_coverage_check <- check_RESE_parlmem_coverage(
  RESE, PARL, assembly_map[country_code], date_from, date_to)
cat("All parliaments in date range have membership data:",
    ifelse(parlmem_coverage_check, "✅ PASS", "❌ FAIL"), "\n")
if (!parlmem_coverage_check) {
  det <- check_RESE_parlmem_coverage_details(
    RESE, PARL, assembly_map[country_code], date_from, date_to)
  cat("  Parliaments with no data:", det$gap_count, "of", det$parliaments_checked, "\n")
  cat("  Parliament IDs missing data:",
      paste(det$parliaments_no_data$parliament_id, collapse = ", "), "\n")
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n=== INTEGRITY CHECK SUMMARY ===\n")

all_checks <- c(person_id_check, entry_id_check, rese_dates_check, parl_dates_check, parl_size_check, full_overlap_check, near_overlap_check,
                meme_persid_check, meme_partyid_check, meme_memepid_check, meme_dates_check, meme_inverted_check, meme_overlap_check, meme_party_coverage_check,
                birthdate_dup_check, parlmem_coverage_check)
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
  if (!parl_size_check) cat("  - Run deepdive script for PARL parliament size details\n")
  if (!full_overlap_check) cat("  - Run 'fixing_projects/overlapping_episodes_fixes.R' to generate merged episodes\n")
  if (!near_overlap_check) cat("  - Run deepdive script for near-overlap details\n")
  
  cat("\nCountry-specific deepdive scripts:\n")
  cat("  - CA_deepdive.R (for Canada detailed investigation)\n")
  cat("  - CH_deepdive.R (for Switzerland detailed investigation)\n")
  cat("  - NL_deepdive.R (for Netherlands detailed investigation)\n")
  cat("  - DE_deepdive.R (for Germany detailed investigation)\n")
  
  cat("\nData fix scripts (in fixing_projects/ folder):\n")
  cat("  - fixing_projects/CH_chamber_function_fixes.R (Swiss chamber political function corrections)\n")
  cat("  - fixing_projects/overlapping_episodes_fixes.R (merge overlapping episodes)\n")
}

cat("\n=== R047 STREAMLINED COMPLETE ===\n")
cat("For detailed investigations and fixes, use the more focussed scripts mentioned above.\n")