# =============================================================================
# Generate First-Day Parliamentary Membership Cohort Snapshots
#
# For each parliament in a given country, determines who was seated at the
# start of each parliamentary term. Two snapshot methods are available:
#
#   "official_start" — use leg_period_start from PARL (simple, matches codebook)
#   "peak_entry"     — find the day with the most MP entries in RESE around
#                       each parliament start, then snapshot the day after
#                       (data-driven, robust to date misalignment)
#
# Outputs one row per person per parliament — the "first-day roster".
# This dataset is the foundation for completeness analyses.
# =============================================================================

library(dplyr)
library(data.table)

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Configuration
country_code <- "NL"
snapshot_method <- "peak_entry"  # Options: "official_start", "peak_entry"

source("R047_RESE_functions.R")
source("R047_PARL_functions.R")

###############################################################################
# Function: find_peak_entry_day
# For a given parliament, finds the day with the most MP entries (inflow)
# within a data-driven search window around the parliament start.
# Search window: midpoint of previous term to midpoint of next term.
#
# Inputs:
#   - parliament_id: character, the parliament to search for
#   - RESE: data.frame with res_entry_start (character, PCC format)
#   - PARL: data.frame with parliament_id, leg_period_start_dateformat,
#           leg_period_end_dateformat (Date columns)
#
# Returns: Date (the peak entry day), or NA if none found
###############################################################################
find_peak_entry_day <- function(parliament_id, RESE, PARL) {
  parl_df <- as.data.frame(PARL)
  parl_df <- parl_df[order(parl_df$leg_period_start_dateformat), ]
  parl_idx <- which(parl_df$parliament_id == parliament_id)
  if (length(parl_idx) == 0) {
    warning("parliament_id '", parliament_id, "' not found in PARL")
    return(as.Date(NA))
  }

  term_start <- parl_df$leg_period_start_dateformat[parl_idx]
  term_end <- parl_df$leg_period_end_dateformat[parl_idx]
  if (is.na(term_end)) term_end <- Sys.Date()

  # Search window: midpoint of previous→this term to midpoint of this→next term
  if (parl_idx > 1) {
    prev_start <- parl_df$leg_period_start_dateformat[parl_idx - 1]
    search_from <- prev_start + as.integer(difftime(term_start, prev_start, units = "days")) / 2
  } else {
    search_from <- term_start - 180
  }

  if (parl_idx < nrow(parl_df)) {
    next_start <- parl_df$leg_period_start_dateformat[parl_idx + 1]
    search_to <- term_start + as.integer(difftime(next_start, term_start, units = "days")) / 2
  } else {
    search_to <- term_end
  }

  # Parse RESE start dates and find the peak entry day
  starts <- as.Date(gsub("\\[\\[.*\\]\\]", "", RESE$res_entry_start), format = "%d%b%Y")
  entry_dates <- starts[!is.na(starts) & starts >= search_from & starts <= search_to]
  if (length(entry_dates) == 0) return(as.Date(NA))

  date_counts <- table(entry_dates)
  as.Date(names(which.max(date_counts)))
}

# Load data
POLI <- read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
RESE <- read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL <- read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")

# Filter to country
RESE <- RESE[which(RESE$country_abb == country_code), ]
PARL <- PARL[which(PARL$country_abb == country_code), ]

# Filter to parliamentary membership episodes
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")), ]

# Filter PARL to correct assembly per country (national lower house)
assembly_map <- c(CA = "HC", CH = "NR", DE = "BT", NL = "TK", NO = "ST", US = "HR", SJ = "SA")
if (!country_code %in% names(assembly_map)) {
  stop("Unsupported country_code: ", country_code)
}
PARL <- PARL[which(PARL$level == "NT" & PARL$assembly_abb == assembly_map[country_code]), ]

cat("=== First-Day Parliamentary Membership Cohort Generator ===\n")
cat("Country:", country_code, "\n")
cat("Snapshot method:", snapshot_method, "\n")
cat("RESE episodes:", nrow(RESE), "\n")
cat("Parliaments:", nrow(PARL), "\n\n")

# Preprocess dates
RESE <- suppressMessages(preprocess_RESEdates(RESE))
PARL <- suppressMessages(preprocess_PARLdates(PARL))

# Parse RESE dates to Date format for comparison
RESE$start_date <- as.Date(RESE$res_entry_start_posoxctformat)
RESE$end_date <- as.Date(RESE$res_entry_end_posoxctformat)

# Add Date columns to PARL for find_peak_entry_day
PARL$leg_period_start_dateformat <- as.Date(PARL$leg_period_start_posoxctformat)
PARL$leg_period_end_dateformat <- as.Date(PARL$leg_period_end_posoxctformat)

# Build cohort snapshots
PARL <- PARL[order(PARL$leg_period_start_posoxctformat), ]

cohort_list <- list()

for (i in seq_len(nrow(PARL))) {
  pid <- PARL$parliament_id[i]
  parl_size <- as.numeric(PARL$parliament_size[i])

  if (snapshot_method == "official_start") {
    snapshot_day <- as.Date(PARL$leg_period_start_posoxctformat[i])
  } else if (snapshot_method == "peak_entry") {
    peak_day <- find_peak_entry_day(pid, RESE, PARL)
    snapshot_day <- if (!is.na(peak_day)) peak_day + 1 else as.Date(NA)
  } else {
    stop("Unknown snapshot_method: ", snapshot_method)
  }

  if (is.na(snapshot_day)) {
    cat("WARNING: skipping parliament", pid, "-- no snapshot day\n")
    next
  }

  # Find all persons seated on snapshot_day
  seated <- RESE[which(RESE$start_date <= snapshot_day & RESE$end_date >= snapshot_day), ]
  seated_ids <- unique(seated$pers_id)

  if (length(seated_ids) == 0) {
    cat("WARNING: no one seated on", format(snapshot_day), "for parliament", pid, "\n")
  }

  cohort_list[[length(cohort_list) + 1]] <- data.frame(
    parliament_id = pid,
    snapshot_day = snapshot_day,
    pers_id = seated_ids,
    parliament_size = parl_size,
    stringsAsFactors = FALSE
  )
}

FIRST_DAY_PARLMEM_COHORT <- do.call(rbind, cohort_list)

# Summary: compare actual seated count vs official parliament size
cohort_summary <- FIRST_DAY_PARLMEM_COHORT |>
  group_by(parliament_id, snapshot_day, parliament_size) |>
  summarise(actual_seated = n(), .groups = "drop") |>
  mutate(deviation = actual_seated - parliament_size) |>
  arrange(snapshot_day)

cat("=== Cohort Summary ===\n")
print(as.data.frame(cohort_summary), row.names = FALSE)

cat("\n=== Deviation flags ===\n")
large_deviations <- cohort_summary[abs(cohort_summary$deviation) > 5, ]
if (nrow(large_deviations) > 0) {
  cat("Parliaments with >5 seat deviation from official size:\n")
  print(as.data.frame(large_deviations), row.names = FALSE)
} else {
  cat("No large deviations found.\n")
}

# Save
output_file <- file.path("completeness", paste0("first_day_parlmem_cohort_", country_code, ".csv"))
write.csv(FIRST_DAY_PARLMEM_COHORT, output_file, row.names = FALSE)
cat("\nSaved", nrow(FIRST_DAY_PARLMEM_COHORT), "rows to", output_file, "\n")
cat("Parliaments:", length(unique(FIRST_DAY_PARLMEM_COHORT$parliament_id)),
    "| Unique persons:", length(unique(FIRST_DAY_PARLMEM_COHORT$pers_id)), "\n")
