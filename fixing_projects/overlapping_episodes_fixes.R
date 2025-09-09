# OVERLAPPING EPISODES MERGER FIXES
# Generate merged episodes for overlapping parliamentary membership periods
# Country-agnostic script - specify country_code as needed

# SETUP - Configure country
country_code <- "NL"  # Change to "CH" for Switzerland, "NL" for Netherlands

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity/fixing_projects")

# Load functions and data (from parent directory)
source("../R047_RESE_functions.R")

RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")

cat("=== OVERLAPPING EPISODES MERGER FIXES ===\n")
cat("Country:", country_code, "\n")

# Filter to country and preprocess dates
RESE <- RESE[which(RESE$country_abb == country_code),]
RESE <- preprocess_RESEdates(RESE)

cat("Total RESE episodes:", nrow(RESE), "\n")

# Focus on parliamentary membership episodes
RESE_parl <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]
cat("Parliamentary episodes:", nrow(RESE_parl), "\n")

# Run data integrity checks
if(!check_RESE_persid_in_POLI(RESE, POLI)) {
  stop("FAILED: Not all person IDs in RESE exist in POLI")
}

if(!check_RESE_resentryid_unique(RESE)) {
  stop("FAILED: Resume entry IDs are not unique")
}

if(check_anyNAinRESEdates(RESE)) {
  stop("FAILED: NA dates found after preprocessing")
}

cat("✅ Basic data integrity checks passed\n")

# Check for overlapping episodes
if(!check_RESE_parlmemeppisodes_anyfulloverlap(RESE)) {
  cat("✅ No overlapping parliamentary episodes found\n")
  cat("No fixes needed - exiting\n")
  quit(save = "no")
}

cat("❌ Overlapping episodes detected - generating fixes\n")

# Get detailed overlap information
overlap_details <- check_RESE_parlmemeppisodes_anyfulloverlap_details(RESE)

if("warning_message" %in% names(overlap_details)) {
  stop("ERROR: ", overlap_details$warning_message)
}

affected_persons <- overlap_details$affected_persons
cat("Persons with overlapping episodes:", length(affected_persons), "\n")
cat("Total overlapping episodes:", overlap_details$overlap_count, "\n")

# Generate merged episodes for all affected persons
IMPORT_DATA <- NULL

for (pid in affected_persons) {
  cat("Processing person:", pid, "\n")
  merged_intervals <- merge_episodes(RESE, pid)
  if (!is.null(merged_intervals)) {
    IMPORT_DATA <- rbind(IMPORT_DATA, merged_intervals)
  }
}

cat("Generated", nrow(IMPORT_DATA), "merged episodes for", length(affected_persons), "persons\n")

# Validate merged results
IMPORT_DATA$res_entry_start_posoxctformat <- as.POSIXct(as.character(IMPORT_DATA$res_entry_start), format=c("%d%b%Y"))
IMPORT_DATA$res_entry_end_posoxctformat <- as.POSIXct(as.character(IMPORT_DATA$res_entry_end), format=c("%d%b%Y"))

# Check that merged data has no overlaps
validation_function <- function(df) {
  df %>%
    mutate(ROWID = row_number()) %>%
    inner_join(df %>% mutate(ROWID = row_number()), by = "pers_id", suffix = c(".x", ".y")) %>%
    filter(ROWID.x < ROWID.y,
           res_entry_start_posoxctformat.x <= res_entry_end_posoxctformat.y,
           res_entry_start_posoxctformat.y <= res_entry_end_posoxctformat.x) %>%
    select(res_entry_id.x, pers_id, res_entry_start.x, res_entry_end.x, 
           res_entry_id.y, res_entry_start.y, res_entry_end.y)
}

validation_result <- validation_function(IMPORT_DATA)

if(nrow(validation_result) > 0) {
  stop("ERROR: Merged episodes still contain overlaps!")
} else {
  cat("✅ Validation passed - no overlaps in merged episodes\n")
}

# Remove temporary validation columns
IMPORT_DATA <- IMPORT_DATA %>% select(-res_entry_start_posoxctformat, -res_entry_end_posoxctformat)

# Generate timestamped filename with country code
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste0(country_code, "_IMPORT_MERGED_EPISODES_", timestamp, ".xlsx")

# Export to Excel
write.xlsx(IMPORT_DATA, file = filename)

cat("Exported merged episodes to:", filename, "\n")
cat("Ready for central database import\n")
cat("\nOriginal overlapping episodes:", overlap_details$overlap_count, "\n")
cat("Merged into:", nrow(IMPORT_DATA), "clean episodes\n")