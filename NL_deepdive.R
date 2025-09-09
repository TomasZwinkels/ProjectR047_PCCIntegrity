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

# Run unit tests to ensure functions work correctly
test_file("R047_unittests.R")
test_file("R047_RESE_unittests.R")
test_file("R047_PARL_unittests.R")

# LOAD DATA
# Import PCC datasets
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")

cat("=== NETHERLANDS DATA QUALITY DEEP DIVE ===\n\n")
cat("Data loaded:\n")
cat("- POLI:", nrow(POLI), "politicians\n")
cat("- RESE:", nrow(RESE), "resume entries\n") 
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# Filter to Netherlands data
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]

cat("After filtering to Netherlands:\n")
cat("- RESE:", nrow(RESE), "resume entries\n")
cat("- PARL:", nrow(PARL), "parliament periods\n\n")

# Focus on parliamentary membership episodes  
RESE_parliamentary <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")), ]
cat("Parliamentary membership episodes:", nrow(RESE_parliamentary), "\n\n")

# PREPROCESS DATES
RESE <- preprocess_RESEdates(RESE)
PARL <- preprocess_PARLdates(PARL)

# =============================================================================
# DETAILED DATA QUALITY INVESTIGATIONS
# =============================================================================

cat("=== 1. PERSON ID VALIDATION ===\n")
# Check if all RESE person IDs exist in POLI
person_id_details <- check_RESE_persid_in_POLI_details(RESE, POLI)
if (!person_id_details$check_passed) {
  cat("❌ Found missing person IDs in POLI\n")
  cat("Missing person IDs:", paste(person_id_details$missing_ids, collapse = ", "), "\n")
  cat("Affected RESE rows:", nrow(person_id_details$missing_rows), "\n")
  
  # Show details of missing persons
  if (nrow(person_id_details$missing_rows) > 0) {
    cat("\nFirst 5 RESE entries with missing person IDs:\n")
    print(person_id_details$missing_rows[1:min(5, nrow(person_id_details$missing_rows)), 
                                         c("res_entry_id", "pers_id", "res_entry_raw")])
  }
} else {
  cat("✅ All RESE person IDs found in POLI\n")
}
cat("\n")

cat("=== 2. RESUME ENTRY ID UNIQUENESS ===\n")
# Check for duplicate resume entry IDs
entry_id_details <- check_RESE_resentryid_unique_details(RESE)
if (!entry_id_details$check_passed) {
  cat("❌ Found duplicate resume entry IDs\n")
  cat("Number of duplicate IDs:", entry_id_details$duplicate_count, "\n")
  cat("Total rows with duplicates:", nrow(entry_id_details$duplicate_rows), "\n")
  
  # Show details of duplicates
  if (nrow(entry_id_details$duplicate_rows) > 0) {
    cat("\nDuplicate resume entry IDs and their occurrences:\n")
    for (dup_id in entry_id_details$duplicate_ids[1:min(5, length(entry_id_details$duplicate_ids))]) {
      dup_rows <- entry_id_details$duplicate_rows[entry_id_details$duplicate_rows$res_entry_id == dup_id, ]
      cat("ID:", dup_id, "- appears", nrow(dup_rows), "times\n")
      cat("  Persons:", paste(unique(dup_rows$pers_id), collapse = ", "), "\n")
    }
  }
} else {
  cat("✅ All resume entry IDs are unique\n")
}
cat("\n")

cat("=== 3. DATE PREPROCESSING VALIDATION ===\n")
# Check for NA dates after preprocessing
rese_date_details <- check_anyNAinRESEdates_details(RESE)
if (!rese_date_details$check_passed) {
  cat("❌ Found NA dates after preprocessing\n")
  cat("NA start dates:", rese_date_details$na_start_count, "\n")
  cat("NA end dates:", rese_date_details$na_end_count, "\n")
  
  # Show problematic rows
  if (nrow(rese_date_details$rows_with_na_dates) > 0) {
    cat("\nFirst 5 rows with NA dates:\n")
    print(rese_date_details$rows_with_na_dates[1:min(5, nrow(rese_date_details$rows_with_na_dates)), 
                                               c("res_entry_id", "pers_id", "res_entry_start", "res_entry_end")])
  }
} else {
  cat("✅ All dates parsed successfully\n")
}

# Check PARL dates
parl_date_details <- check_anyNAinPARLdates_details(PARL)
if (!parl_date_details$check_passed) {
  cat("❌ Found NA parliament dates after preprocessing\n")
  cat("NA start dates:", parl_date_details$na_start_count, "\n") 
  cat("NA end dates:", parl_date_details$na_end_count, "\n")
} else {
  cat("✅ All parliament dates parsed successfully\n")
}
cat("\n")

cat("=== 4. PARLIAMENTARY EPISODE OVERLAPS ===\n")
# Check for fully overlapping parliamentary episodes
full_overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE)
if (!full_overlap_details$check_passed) {
  if ("warning_message" %in% names(full_overlap_details)) {
    cat("⚠️ ", full_overlap_details$warning_message, "\n")
  } else {
    cat("❌ Found fully overlapping parliamentary episodes\n")
    cat("Overlapping episodes:", full_overlap_details$overlap_count, "\n")
    cat("Affected persons:", length(full_overlap_details$affected_persons), "\n")
    
    # Show affected persons and their overlaps
    if (length(full_overlap_details$affected_persons) > 0) {
      cat("\nPersons with overlapping episodes:\n")
      for (person in full_overlap_details$affected_persons[1:min(5, length(full_overlap_details$affected_persons))]) {
        person_overlaps <- full_overlap_details$overlapping_episodes[
          full_overlap_details$overlapping_episodes$pers_id == person, ]
        cat("Person:", person, "- has", nrow(person_overlaps), "overlapping episodes\n")
      }
    }
  }
} else {
  cat("✅ No fully overlapping parliamentary episodes\n")
}
cat("\n")

cat("=== 5. NEAR-OVERLAPPING EPISODES ===\n")
# Check for nearly overlapping episodes (within 2 days)
near_overlap_details <- check_RESE_anynear_fulloverlap_details(RESE, tolerance_days = 2)
if (!near_overlap_details$check_passed) {
  cat("❌ Found near-overlapping episodes (within 2 days)\n")
  cat("Near-overlapping pairs:", near_overlap_details$near_overlap_count, "\n")
  cat("Affected persons:", length(near_overlap_details$affected_persons), "\n")
  
  # Show details of near overlaps
  if (nrow(near_overlap_details$near_overlapping_pairs) > 0) {
    cat("\nFirst 3 near-overlapping pairs:\n")
    pairs_sample <- near_overlap_details$near_overlapping_pairs[1:min(3, nrow(near_overlap_details$near_overlapping_pairs)), ]
    for (i in 1:nrow(pairs_sample)) {
      pair <- pairs_sample[i, ]
      cat("Person:", pair$pers_id, "\n")
      cat("  Episode 1:", pair$res_entry_start.x, "to", pair$res_entry_end.x, "\n")
      cat("  Episode 2:", pair$res_entry_start.y, "to", pair$res_entry_end.y, "\n") 
      cat("  Start diff:", pair$start_diff_days, "days, End diff:", pair$end_diff_days, "days\n\n")
    }
  }
} else {
  cat("✅ No near-overlapping episodes found\n")
}
cat("\n")

# =============================================================================
# NETHERLANDS-SPECIFIC INVESTIGATIONS
# =============================================================================

cat("=== 6. DUTCH PARLIAMENT ANALYSIS ===\n")

# Analyze parliamentary structure
if (nrow(RESE_parliamentary) > 0) {
  # Check parliament structure (Dutch parliament is unicameral - Tweede Kamer)
  parliament_dist <- table(RESE_parliamentary$parliament_id)
  cat("Parliament distribution:\n")
  print(parliament_dist)
  
  # Check for any unexpected chamber references
  tweede_kamer_patterns <- c("Tweede Kamer", "Second Chamber", "Lower House", "TK", "Kamer")
  eerste_kamer_patterns <- c("Eerste Kamer", "First Chamber", "Upper House", "EK", "Senaat")
  
  tk_in_raw <- grepl(paste(tweede_kamer_patterns, collapse = "|"), RESE_parliamentary$res_entry_raw, ignore.case = TRUE)
  ek_in_raw <- grepl(paste(eerste_kamer_patterns, collapse = "|"), RESE_parliamentary$res_entry_raw, ignore.case = TRUE)
  
  cat("\nChamber references in raw text:\n")
  cat("Tweede Kamer references:", sum(tk_in_raw), "\n")
  cat("Eerste Kamer references:", sum(ek_in_raw), "\n")
  
  # Check for unusual date patterns (Dutch elections typically every 4 years)
  if (nrow(PARL) > 0) {
    cat("\nDutch parliamentary periods:\n")
    parl_periods <- PARL[order(PARL$leg_period_start_posoxctformat), ]
    for (i in 1:min(5, nrow(parl_periods))) {
      cat("Period:", parl_periods$parliament_id[i], 
          "- Start:", parl_periods$leg_period_start[i], 
          "- End:", parl_periods$leg_period_end[i], "\n")
    }
  }
  
} else {
  cat("⚠️ No parliamentary episodes found for Netherlands analysis\n")
}
cat("\n")

# =============================================================================
# NETHERLANDS DATA PATTERNS ANALYSIS
# =============================================================================

cat("=== 7. DUTCH POLITICAL SYSTEM VALIDATION ===\n")

# Check for expected Dutch political functions
if (nrow(RESE) > 0) {
  political_functions <- table(RESE$political_function)
  cat("Political function distribution:\n")
  print(head(political_functions, 10))
  
  # Look for common Dutch political roles
  minister_roles <- sum(grepl("minister", RESE$res_entry_raw, ignore.case = TRUE))
  staatssecretaris_roles <- sum(grepl("staatssecretaris", RESE$res_entry_raw, ignore.case = TRUE))
  
  cat("\nCommon Dutch political roles found:\n")
  cat("Minister references:", minister_roles, "\n")
  cat("Staatssecretaris references:", staatssecretaris_roles, "\n")
}

# Check for expected Dutch party abbreviations
if (nrow(RESE) > 0) {
  dutch_parties <- c("VVD", "PvdA", "CDA", "D66", "SP", "PVV", "GL", "CU", "SGP", "DENK", "FvD", "JA21")
  party_mentions <- sapply(dutch_parties, function(party) {
    sum(grepl(paste0("\\b", party, "\\b"), RESE$res_entry_raw, ignore.case = FALSE))
  })
  
  cat("\nMajor Dutch party mentions in raw text:\n")
  for (party in names(party_mentions)[party_mentions > 0]) {
    cat(party, ":", party_mentions[party], "mentions\n")
  }
}
cat("\n")

cat("=== DEEP DIVE COMPLETE ===\n")
cat("Use the detail objects created above for further investigation:\n")
cat("- person_id_details: Missing person ID analysis\n")
cat("- entry_id_details: Duplicate entry ID analysis\n") 
cat("- rese_date_details: RESE date parsing issues\n")
cat("- parl_date_details: PARL date parsing issues\n")
cat("- full_overlap_details: Fully overlapping episodes\n")
cat("- near_overlap_details: Near-overlapping episodes\n")
cat("\nThese objects contain data.frames and vectors for detailed inspection.\n")