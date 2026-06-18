# =============================================================================
# NL Parliament Overcount Investigation
#
# The R051 graph shows structurally too many seated MPs (above the 150
# baseline) during certain periods — notably around the 2012 parliament.
# This script:
#   1. Computes daily MP counts directly from RESE (same logic as R051)
#   2. Identifies overcount periods (days where seated > 150)
#   3. For each overcount period, picks the peak day and shows:
#      - Per-party breakdown (from MEME) vs official election results
#      - The specific "extra" MPs who push the count above 150
#      - Full member list for parties with overcounts
# =============================================================================

library(dplyr)
library(data.table)

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

# Parse PARL dates
PARL$leg_period_start <- gsub("[[rcen]]", "", PARL$leg_period_start, fixed = TRUE)
PARL$leg_period_start <- gsub("[[lcen]]", "", PARL$leg_period_start, fixed = TRUE)
PARL$leg_period_end <- gsub("[[rcen]]", "", PARL$leg_period_end, fixed = TRUE)
PARL$leg_period_end <- gsub("[[lcen]]", "", PARL$leg_period_end, fixed = TRUE)
PARL$start_date <- as.Date(PARL$leg_period_start, format = "%d%b%Y")
PARL$end_date <- as.Date(PARL$leg_period_end, format = "%d%b%Y")

# =============================================================================
# Step 1: Find overcount periods directly from RESE (like R051 does)
# =============================================================================

RESE_dt <- as.data.table(RESE)

# For each PARL period, check daily counts within that term
cat("\n=== Scanning for overcount periods across all parliaments ===\n\n")

overcount_results <- list()

for (i in seq_len(nrow(PARL))) {
  parl_id <- PARL$parliament_id[i]
  parl_start <- PARL$start_date[i]
  parl_end <- PARL$end_date[i]
  parl_size <- as.numeric(PARL$parliament_size[i])

  if (is.na(parl_start) | is.na(parl_end) | is.na(parl_size)) next

  # Generate daily sequence for this term
  term_days <- seq(parl_start, parl_end, by = "day")

  # Count seated MPs per day using RESE intervals
  daily_count <- sapply(term_days, function(d) {
    RESE_dt[start_date <= d & end_date >= d, uniqueN(pers_id)]
  })

  overcount_days <- term_days[daily_count > parl_size]

  if (length(overcount_days) > 0) {
    # Find the peak day (max overcount)
    peak_idx <- which.max(daily_count[daily_count > parl_size])
    peak_day <- overcount_days[peak_idx]
    peak_count <- daily_count[term_days == peak_day]

    cat(sprintf("Parliament %s (size=%d): %d overcount days, peak=%d on %s\n",
                parl_id, parl_size, length(overcount_days), peak_count, peak_day))

    overcount_results[[parl_id]] <- list(
      parliament_id = parl_id,
      parl_size = parl_size,
      n_overcount_days = length(overcount_days),
      first_overcount = min(overcount_days),
      last_overcount = max(overcount_days),
      peak_day = peak_day,
      peak_count = peak_count
    )
  }
}

if (length(overcount_results) == 0) {
  cat("No overcount periods found — all parliaments are at or below their official size.\n")
} else {
  cat(sprintf("\nFound %d parliament(s) with overcounts.\n", length(overcount_results)))
}

# =============================================================================
# Step 2: Per-party breakdown on peak overcount day
# =============================================================================

# Official election results for comparison (extend as needed)
official_seats <- list(
  "NL_NT-TK_2010" = data.frame(
    party_abb = c("VVD", "PvdA", "PVV", "CDA", "SP", "D66", "GL", "CU", "SGP", "PvdD"),
    official_seats = c(31, 30, 24, 21, 15, 10, 10, 5, 2, 2),
    stringsAsFactors = FALSE
  ),
  "NL_NT-TK_2012" = data.frame(
    party_abb = c("VVD", "PvdA", "PVV", "SP", "CDA", "D66", "CU", "GL", "SGP", "PvdD", "50plus"),
    official_seats = c(41, 38, 15, 15, 13, 12, 5, 4, 3, 2, 2),
    stringsAsFactors = FALSE
  ),
  "NL_NT-TK_2017" = data.frame(
    party_abb = c("VVD", "PVV", "CDA", "D66", "GL", "SP", "PvdA", "CU", "PvdD", "50plus", "SGP", "DENK", "FvD"),
    official_seats = c(33, 20, 19, 19, 14, 14, 9, 5, 5, 4, 3, 3, 2),
    stringsAsFactors = FALSE
  )
)

PART_abb <- PART[, c("party_id", "party_abb")]

# Function: full breakdown for a given snapshot day
party_breakdown_on_day <- function(snapshot_day, parl_size, parliament_id,
                                   RESE_dt, MEME, POLI, PART_abb, official = NULL) {
  # Who is seated on this day according to RESE?
  seated <- RESE_dt[start_date <= snapshot_day & end_date >= snapshot_day]
  seated_ids <- unique(seated$pers_id)

  cat(sprintf("\n{'='*70}\n"))
  cat(sprintf("=== %s — Snapshot: %s ===\n", parliament_id, snapshot_day))
  cat(sprintf("Official size: %d | Seated (RESE): %d | Deviation: %+d\n",
              parl_size, length(seated_ids), length(seated_ids) - parl_size))

  # Active MEME episodes for seated MPs on this day
  active_meme <- MEME[which(
    MEME$pers_id %in% seated_ids &
    MEME$start_date <= snapshot_day &
    (MEME$end_date >= snapshot_day | is.na(MEME$end_date))
  ), ]
  active_meme <- merge(active_meme, PART_abb, by = "party_id", all.x = TRUE)

  # Per-party counts
  data_counts <- active_meme |>
    group_by(party_abb) |>
    summarise(data_seats = n_distinct(pers_id), .groups = "drop") |>
    arrange(desc(data_seats))

  # Compare to official if available
  if (!is.null(official)) {
    comparison <- merge(official, data_counts, by = "party_abb", all = TRUE)
    comparison$data_seats[is.na(comparison$data_seats)] <- 0
    comparison$official_seats[is.na(comparison$official_seats)] <- 0
    comparison$diff <- comparison$data_seats - comparison$official_seats
    comparison <- comparison[order(-abs(comparison$diff), -comparison$data_seats), ]

    cat("\nParty breakdown vs official election results:\n")
    print(as.data.frame(comparison), row.names = FALSE)
    cat(sprintf("\nOfficial total: %d | Data total (via MEME): %d\n",
                sum(comparison$official_seats), sum(comparison$data_seats)))

    # For parties with overcounts, list the specific members
    overcount_parties <- comparison$party_abb[comparison$diff > 0]
    if (length(overcount_parties) > 0) {
      cat("\n--- Members of parties with overcounts ---\n")
      for (party in overcount_parties) {
        party_ids <- unique(active_meme$pers_id[active_meme$party_abb == party])
        party_members <- POLI[POLI$pers_id %in% party_ids,
                              c("pers_id", "first_name", "last_name", "birth_date")]
        party_members <- party_members[order(party_members$last_name), ]

        # Also get their RESE episodes active on this day
        party_rese <- seated[seated$pers_id %in% party_ids,
                             c("pers_id", "res_entry_id", "res_entry_start", "res_entry_end")]

        party_detail <- merge(party_members, party_rese, by = "pers_id")
        party_detail <- party_detail[order(party_detail$last_name), ]

        off_count <- comparison$official_seats[comparison$party_abb == party]
        cat(sprintf("\n%s: %d in data, %d official (diff: %+d)\n",
                    party, length(party_ids), off_count, length(party_ids) - off_count))
        print(party_detail, row.names = FALSE)
      }
    }
  } else {
    cat("\nParty breakdown (no official data available for comparison):\n")
    print(as.data.frame(data_counts), row.names = FALSE)
  }

  # MPs without MEME (can't assign to a party)
  no_party <- seated_ids[!seated_ids %in% unique(active_meme$pers_id)]
  if (length(no_party) > 0) {
    cat(sprintf("\nSeated MPs WITHOUT active MEME episode: %d\n", length(no_party)))
    no_party_info <- POLI[POLI$pers_id %in% no_party, c("pers_id", "first_name", "last_name")]
    print(no_party_info, row.names = FALSE)
  }

  invisible(list(seated_ids = seated_ids, active_meme = active_meme, data_counts = data_counts))
}

# Run the breakdown for each overcounted parliament
for (parl_id in names(overcount_results)) {
  info <- overcount_results[[parl_id]]
  official <- official_seats[[parl_id]]  # NULL if we don't have it

  party_breakdown_on_day(
    snapshot_day = info$peak_day,
    parl_size = info$parl_size,
    parliament_id = parl_id,
    RESE_dt = RESE_dt,
    MEME = MEME,
    POLI = POLI,
    PART_abb = PART_abb,
    official = official
  )
}

# =============================================================================
# Step 3: Also check a "normal" parliament for comparison
# =============================================================================

cat("\n\n--- For comparison: a parliament with correct count (2023) ---\n")

parl_2023 <- PARL[PARL$parliament_id == "NL_NT-TK_2023", ]
if (nrow(parl_2023) > 0) {
  # Use a day 30 days into the term as snapshot (past any transition noise)
  snapshot_2023 <- parl_2023$start_date[1] + 30
  party_breakdown_on_day(
    snapshot_day = snapshot_2023,
    parl_size = as.numeric(parl_2023$parliament_size[1]),
    parliament_id = "NL_NT-TK_2023",
    RESE_dt = RESE_dt,
    MEME = MEME,
    POLI = POLI,
    PART_abb = PART_abb,
    official = NULL
  )
}

# =============================================================================
# Step 4: POLI duplicate scan — same name, different pers_id,
#         both with overlapping RESE episodes during the 2012 parliament
# =============================================================================

cat("\n\n=== POLI duplicate scan: same name, different pers_id, overlapping RESE ===\n")

# NL persons only
POLI_NL <- POLI[grepl("^NL", POLI$pers_id), ]

# Find name groups with >1 pers_id
name_groups <- POLI_NL |>
  group_by(first_name, last_name) |>
  filter(n_distinct(pers_id) > 1) |>
  arrange(last_name, first_name, pers_id) |>
  ungroup()

if (nrow(name_groups) == 0) {
  cat("No duplicate names found in POLI.\n")
} else {
  cat(sprintf("Found %d pers_ids sharing a name with at least one other pers_id.\n\n",
              nrow(name_groups)))

  # For each name group, check whether multiple pers_ids have RESE episodes
  # that overlap in time (i.e. both seated on the same day)
  dup_names <- name_groups |>
    distinct(first_name, last_name)

  suspect_pairs <- data.frame()

  for (r in seq_len(nrow(dup_names))) {
    fn <- dup_names$first_name[r]
    ln <- dup_names$last_name[r]
    ids <- name_groups$pers_id[name_groups$first_name == fn & name_groups$last_name == ln]

    # Get all RESE episodes for these pers_ids
    episodes <- RESE_dt[pers_id %in% ids, .(pers_id, res_entry_id, start_date, end_date)]
    if (nrow(episodes) < 2) next

    # Check all pairs of pers_ids for temporal overlap
    unique_ids <- unique(episodes$pers_id)
    for (i in seq_along(unique_ids)) {
      for (j in seq_along(unique_ids)) {
        if (j <= i) next
        ep_a <- episodes[pers_id == unique_ids[i]]
        ep_b <- episodes[pers_id == unique_ids[j]]

        # Cross-join to check all episode pairs
        for (a in seq_len(nrow(ep_a))) {
          for (b in seq_len(nrow(ep_b))) {
            # Overlap exists if start_a <= end_b AND start_b <= end_a
            if (ep_a$start_date[a] <= ep_b$end_date[b] &
                ep_b$start_date[b] <= ep_a$end_date[a]) {
              overlap_start <- max(ep_a$start_date[a], ep_b$start_date[b])
              overlap_end <- min(ep_a$end_date[a], ep_b$end_date[b])
              overlap_days <- as.numeric(overlap_end - overlap_start) + 1

              row <- data.frame(
                first_name = fn, last_name = ln,
                pers_id_A = unique_ids[i],
                pers_id_B = unique_ids[j],
                episode_A = ep_a$res_entry_id[a],
                episode_B = ep_b$res_entry_id[b],
                overlap_start = overlap_start,
                overlap_end = overlap_end,
                overlap_days = overlap_days,
                stringsAsFactors = FALSE
              )
              suspect_pairs <- rbind(suspect_pairs, row)
            }
          }
        }
      }
    }
  }

  if (nrow(suspect_pairs) == 0) {
    cat("No same-name pers_id pairs with overlapping RESE episodes found.\n")
  } else {
    # Add birth dates for easy comparison
    suspect_pairs <- merge(suspect_pairs,
      POLI_NL[, c("pers_id", "birth_date")],
      by.x = "pers_id_A", by.y = "pers_id", all.x = TRUE)
    names(suspect_pairs)[names(suspect_pairs) == "birth_date"] <- "birth_A"

    suspect_pairs <- merge(suspect_pairs,
      POLI_NL[, c("pers_id", "birth_date")],
      by.x = "pers_id_B", by.y = "pers_id", all.x = TRUE)
    names(suspect_pairs)[names(suspect_pairs) == "birth_date"] <- "birth_B"

    suspect_pairs <- suspect_pairs[order(-suspect_pairs$overlap_days), ]

    cat(sprintf("Found %d overlapping episode pair(s) between same-name pers_ids:\n\n",
                nrow(suspect_pairs)))
    print(suspect_pairs, row.names = FALSE)
  }
}
