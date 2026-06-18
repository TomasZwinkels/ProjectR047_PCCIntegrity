# =============================================================================
# NL Parliament Overcount Investigation
#
# The 2012 and 2017 Dutch parliaments show more seated MPs than the official
# parliament size of 150. This script breaks down who is seated by party
# (using MEME data) to identify which party/parties have too many members.
# =============================================================================

library(dplyr)

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

source("R047_RESE_functions.R")
source("R047_PARL_functions.R")
source("R047_MEME_functions.R")

# Load data
POLI <- read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE <- read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL <- read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
MEME <- read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
PART <- read.csv("/home/tomas/projects/PCCdata/PART.csv", header = TRUE, sep = ";")

# Filter to NL
RESE <- RESE[which(RESE$country_abb == "NL"), ]
PARL <- PARL[which(PARL$country_abb == "NL" & PARL$level == "NT" & PARL$assembly_abb == "TK"), ]
MEME <- MEME[which(substr(MEME$pers_id, 1, 2) == "NL"), ]

# Preprocess dates
RESE <- suppressMessages(preprocess_RESEdates(RESE))
MEME <- suppressMessages(preprocess_MEMEdates(MEME))

RESE$start_date <- as.Date(RESE$res_entry_start_posoxctformat)
RESE$end_date <- as.Date(RESE$res_entry_end_posoxctformat)
MEME$start_date <- as.Date(MEME$memep_startdate_posoxctformat)
MEME$end_date <- as.Date(MEME$memep_enddate_posoxctformat)

# Filter RESE to parliamentary membership
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01")), ]

# Load cohort data
cohort <- read.csv("completeness/first_day_parlmem_cohort_NL.csv", stringsAsFactors = FALSE)
cohort$snapshot_day <- as.Date(cohort$snapshot_day)

# Function: for a given parliament, show party breakdown of seated MPs
party_breakdown <- function(parliament_id, cohort, MEME, POLI, PART) {
  c_rows <- cohort[cohort$parliament_id == parliament_id, ]
  snapshot <- c_rows$snapshot_day[1]
  seated_ids <- c_rows$pers_id

  cat(sprintf("\n=== %s (snapshot: %s) ===\n", parliament_id, format(snapshot)))
  cat(sprintf("Official size: %d | Actually seated: %d | Deviation: %+d\n",
              c_rows$parliament_size[1], length(seated_ids),
              length(seated_ids) - c_rows$parliament_size[1]))

  # Find active MEME episodes for seated MPs on snapshot day
  active_meme <- MEME[which(
    MEME$pers_id %in% seated_ids &
    MEME$start_date <= snapshot &
    (MEME$end_date >= snapshot | is.na(MEME$end_date))
  ), ]

  # Get party names from PART
  party_names <- PART[, c("party_id", "party_abb")]
  active_meme <- merge(active_meme, party_names, by = "party_id", all.x = TRUE)

  # Count per party
  party_counts <- active_meme |>
    group_by(party_id, party_abb) |>
    summarise(n_members = n_distinct(pers_id), .groups = "drop") |>
    arrange(desc(n_members))

  # Also find MPs without MEME
  no_party <- seated_ids[!seated_ids %in% unique(active_meme$pers_id)]

  cat("\nParty breakdown (from MEME):\n")
  print(as.data.frame(party_counts), row.names = FALSE)
  cat(sprintf("\nTotal accounted for via MEME: %d\n", sum(party_counts$n_members)))

  if (length(no_party) > 0) {
    cat(sprintf("MPs without active MEME episode: %d\n", length(no_party)))
    no_party_info <- POLI[POLI$pers_id %in% no_party, c("pers_id", "last_name", "first_name")]
    print(no_party_info, row.names = FALSE)
  }

  # Return the detailed per-person data for further inspection
  per_person <- merge(
    data.frame(pers_id = seated_ids, stringsAsFactors = FALSE),
    POLI[, c("pers_id", "last_name", "first_name")],
    by = "pers_id", all.x = TRUE
  )
  per_person <- merge(
    per_person,
    active_meme[, c("pers_id", "party_id", "party_abb", "memep_startdate", "memep_enddate")],
    by = "pers_id", all.x = TRUE
  )
  per_person <- per_person[order(per_person$party_abb, per_person$last_name), ]

  invisible(per_person)
}

# Run for both overcounted parliaments
details_2012 <- party_breakdown("NL_NT-TK_2012", cohort, MEME, POLI, PART)
details_2017 <- party_breakdown("NL_NT-TK_2017", cohort, MEME, POLI, PART)

# Also show breakdown for a "normal" parliament for comparison
cat("\n\n--- For comparison: a parliament with correct count ---\n")
details_2023 <- party_breakdown("NL_NT-TK_2023", cohort, MEME, POLI, PART)

# =============================================================================
# Official election results for comparison
# Source: https://en.wikipedia.org/wiki/2012_Dutch_general_election
#         https://en.wikipedia.org/wiki/2017_Dutch_general_election
# =============================================================================

cat("\n\n=== OFFICIAL ELECTION RESULTS (Wikipedia) vs DATA ===\n")

official_2012 <- data.frame(
  party_abb = c("VVD", "PvdA", "PVV", "SP", "CDA", "D66", "CU", "GL", "SGP", "PvdD", "50plus"),
  official_seats = c(41, 38, 15, 15, 13, 12, 5, 4, 3, 2, 2),
  stringsAsFactors = FALSE
)

official_2017 <- data.frame(
  party_abb = c("VVD", "PVV", "CDA", "D66", "GL", "SP", "PvdA", "CU", "PvdD", "50plus", "SGP", "DENK", "FvD"),
  official_seats = c(33, 20, 19, 19, 14, 14, 9, 5, 5, 4, 3, 3, 2),
  stringsAsFactors = FALSE
)

compare_election <- function(parliament_id, official, MEME, POLI, PART, cohort) {
  c_rows <- cohort[cohort$parliament_id == parliament_id, ]
  snapshot <- c_rows$snapshot_day[1]
  seated_ids <- c_rows$pers_id

  # Get party abbreviations for active MEME episodes
  PART_abb <- PART[, c("party_id", "party_abb")]
  active_meme <- MEME[which(
    MEME$pers_id %in% seated_ids &
    MEME$start_date <= snapshot &
    (MEME$end_date >= snapshot | is.na(MEME$end_date))
  ), ]
  active_meme <- merge(active_meme, PART_abb, by = "party_id", all.x = TRUE)

  data_counts <- active_meme |>
    group_by(party_abb) |>
    summarise(data_seats = n_distinct(pers_id), .groups = "drop")

  comparison <- merge(official, data_counts, by = "party_abb", all = TRUE)
  comparison$data_seats[is.na(comparison$data_seats)] <- 0
  comparison$official_seats[is.na(comparison$official_seats)] <- 0
  comparison$diff <- comparison$data_seats - comparison$official_seats
  comparison <- comparison[order(-abs(comparison$diff), -comparison$official_seats), ]

  cat(sprintf("\n=== %s: Official vs Data ===\n", parliament_id))
  print(as.data.frame(comparison), row.names = FALSE)
  cat(sprintf("\nOfficial total: %d | Data total (via MEME): %d | Seated (via RESE): %d\n",
              sum(comparison$official_seats), sum(comparison$data_seats), length(seated_ids)))

  # Show MPs without MEME
  no_party <- seated_ids[!seated_ids %in% unique(active_meme$pers_id)]
  if (length(no_party) > 0) {
    cat(sprintf("Seated MPs without active MEME episode: %d\n", length(no_party)))
    no_party_info <- POLI[POLI$pers_id %in% no_party, c("pers_id", "last_name", "first_name")]
    print(no_party_info, row.names = FALSE)
  }
}

compare_election("NL_NT-TK_2012", official_2012, MEME, POLI, PART, cohort)
compare_election("NL_NT-TK_2017", official_2017, MEME, POLI, PART, cohort)

# =============================================================================
# Detailed VVD member list for 2012 parliament
# =============================================================================

cat("\n\n=== VVD members seated on 2012-09-21 according to our data ===\n")

c_2012 <- cohort[cohort$parliament_id == "NL_NT-TK_2012", ]
snapshot_2012 <- c_2012$snapshot_day[1]
seated_2012 <- c_2012$pers_id

PART_abb <- PART[, c("party_id", "party_abb")]
active_meme_2012 <- MEME[which(
  MEME$pers_id %in% seated_2012 &
  MEME$start_date <= snapshot_2012 &
  (MEME$end_date >= snapshot_2012 | is.na(MEME$end_date))
), ]
active_meme_2012 <- merge(active_meme_2012, PART_abb, by = "party_id", all.x = TRUE)

vvd_ids <- unique(active_meme_2012$pers_id[active_meme_2012$party_abb == "VVD"])
vvd_members <- POLI[POLI$pers_id %in% vvd_ids, c("pers_id", "first_name", "last_name", "birth_date")]
vvd_members <- vvd_members[order(vvd_members$last_name), ]

cat("Count:", nrow(vvd_members), "(official: 41)\n\n")
print(vvd_members, row.names = FALSE)
