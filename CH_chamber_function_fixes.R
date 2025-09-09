# SWITZERLAND CHAMBER POLITICAL FUNCTION FIXES
# Generate corrected political function codes for Swiss parliamentary chambers

# SETUP
country_code <- "CH"
setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Load functions
source("R047_RESE_functions.R")

# Load data
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")

# Filter to Swiss parliamentary episodes
RESE <- RESE[which(RESE$country_abb == country_code & RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]

cat("=== SWISS CHAMBER FUNCTION CODE FIXES ===\n")
cat("Swiss parliamentary episodes to process:", nrow(RESE), "\n\n")

# Generate chamber classifications
RESE$chamber <- ifelse(grepl("NT-NR", RESE$parliament_id), "NR", 
                       ifelse(grepl("NT-SR", RESE$parliament_id), "SR", "OTHER"))

table(RESE$chamber)

# Validate chamber consistency with raw text
nationalrat_patterns <- c("Nationalrat", "National Council", "Lower House", "Erste Kammer", "\\bNR\\b", "Conseil national")
staenderat_patterns <- c("Ständerat", "Staenderat", "Council of States", "Upper House", "Zweite Kammer", "\\bSR\\b", "Conseil des États")

nr_in_raw <- grepl(paste(nationalrat_patterns, collapse = "|"), RESE$res_entry_raw, ignore.case = TRUE)
sr_in_raw <- grepl(paste(staenderat_patterns, collapse = "|"), RESE$res_entry_raw, ignore.case = TRUE)

# Check for inconsistencies
inconsistent_nr <- sum(RESE$chamber == "NR" & sr_in_raw)
inconsistent_sr <- sum(RESE$chamber == "SR" & nr_in_raw)

cat("Inconsistencies found - NR records with SR text:", inconsistent_nr, "\n")
cat("Inconsistencies found - SR records with NR text:", inconsistent_sr, "\n")

if(inconsistent_nr > 0 || inconsistent_sr > 0) {
  stop("INCONSISTENT CHAMBER DATA FOUND - stopping export generation")
}

# Generate corrected political function codes
RESE_export <- RESE[, c("res_entry_id", "political_function", "chamber")]
RESE_export$political_function_updated <- ifelse(
  RESE_export$chamber == "NR", "NT_LE-LH_T3_NA_01",
  ifelse(RESE_export$chamber == "SR", "NT_LE-UH_T3_NA_01", 
         RESE_export$political_function)
)

# Keep only the columns needed for update
RESE_export <- RESE_export[, c("res_entry_id", "political_function_updated")]
names(RESE_export) <- c("res_entry_id", "political_function")

# Generate timestamped filename
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste0("CH_RESE_chamber_updates_", timestamp, ".csv")

# Export to CSV
write.csv(RESE_export, filename, row.names = FALSE)

cat("Exported", nrow(RESE_export), "records to", filename, "\n")
cat("NR (Nationalrat) -> NT_LE-LH_T3_NA_01\n")
cat("SR (Ständerat) -> NT_LE-UH_T3_NA_01\n")
cat("\nFix ready for central database import\n")