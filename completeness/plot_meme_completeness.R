# =============================================================================
# Plot: MEME (Party Membership) Completeness per Parliamentary Cohort
#
# For each parliament's first-day cohort, checks what percentage of seated
# MPs have at least one active MEME episode on the snapshot day.
# =============================================================================

library(dplyr)
library(ggplot2)

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Configuration
country_code <- "NL"

country_name <- switch(country_code,
  CA = "Canada", CH = "Switzerland", DE = "Germany",
  NL = "Netherlands", NO = "Norway", US = "United States",
  country_code
)

source("R047_MEME_functions.R")

# Load cohort data
cohort_file <- file.path("completeness", paste0("first_day_parlmem_cohort_", country_code, ".csv"))
if (!file.exists(cohort_file)) {
  stop("Cohort file not found: ", cohort_file, "\nRun generate_first_day_parlmem_cohort.R first.")
}
cohort <- read.csv(cohort_file, stringsAsFactors = FALSE)
cohort$snapshot_day <- as.Date(cohort$snapshot_day)

# Load and preprocess MEME
MEME <- read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
MEME <- MEME[which(substr(MEME$pers_id, 1, nchar(country_code)) == country_code), ]
MEME <- suppressMessages(preprocess_MEMEdates(MEME))
MEME$start_date <- as.Date(MEME$memep_startdate_posoxctformat)
MEME$end_date <- as.Date(MEME$memep_enddate_posoxctformat)

cat("=== MEME Completeness Check ===\n")
cat("Country:", country_code, "\n")
cat("Cohort rows:", nrow(cohort), "\n")
cat("MEME episodes:", nrow(MEME), "\n\n")

# For each parliament, check how many cohort members have an active MEME episode
completeness_list <- list()

for (pid in unique(cohort$parliament_id)) {
  cohort_rows <- cohort[cohort$parliament_id == pid, ]
  snapshot_day <- cohort_rows$snapshot_day[1]
  parl_size <- cohort_rows$parliament_size[1]
  seated_ids <- cohort_rows$pers_id

  # Find MEME episodes active on snapshot_day (start <= day, end >= day or end is NA for ongoing)
  active_meme <- MEME[which(
    MEME$start_date <= snapshot_day &
    (MEME$end_date >= snapshot_day | is.na(MEME$end_date))
  ), ]
  has_party <- seated_ids %in% unique(active_meme$pers_id)

  completeness_list[[length(completeness_list) + 1]] <- data.frame(
    parliament_id = pid,
    snapshot_day = snapshot_day,
    parliament_size = parl_size,
    actual_seated = length(seated_ids),
    has_meme = sum(has_party),
    missing_meme = sum(!has_party),
    pct_complete = round(100 * sum(has_party) / length(seated_ids), 1),
    stringsAsFactors = FALSE
  )
}

meme_completeness <- do.call(rbind, completeness_list)
meme_completeness <- meme_completeness[order(meme_completeness$snapshot_day), ]

cat("=== MEME Completeness by Parliament ===\n")
print(as.data.frame(meme_completeness), row.names = FALSE)

# Plot
p <- ggplot(meme_completeness, aes(x = snapshot_day, y = pct_complete)) +
  geom_line(color = "purple", linewidth = 0.8) +
  geom_point(color = "purple", size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_x_date(name = "Parliament start date") +
  scale_y_continuous(name = "% of seated MPs with party data", limits = c(0, 105)) +
  labs(
    title = paste0("Party Membership (MEME) Completeness - ", country_name),
    subtitle = "% of first-day cohort MPs with an active MEME episode on the snapshot day"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

plot_file <- file.path("completeness", paste0("meme_completeness_", country_code, ".png"))
ggsave(plot_file, p, width = 14, height = 7, dpi = 150, bg = "white")
cat("\nPlot saved as", plot_file, "\n")

p
