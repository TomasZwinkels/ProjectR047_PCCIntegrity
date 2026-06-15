# =============================================================================
# Plot: First-Day Cohort Size vs Official Parliament Size
#
# Loads the cohort snapshot CSV and plots actual seated count against
# the official parliament_size from PARL, per parliamentary term.
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")

# Configuration
country_code <- "NO"

country_name <- switch(country_code,
  CA = "Canada", CH = "Switzerland", DE = "Germany",
  NL = "Netherlands", NO = "Norway", US = "United States",
  country_code
)

# Load cohort data
cohort_file <- file.path("completeness", paste0("first_day_parlmem_cohort_", country_code, ".csv"))
if (!file.exists(cohort_file)) {
  stop("Cohort file not found: ", cohort_file, "\nRun generate_first_day_parlmem_cohort.R first.")
}
cohort <- read.csv(cohort_file, stringsAsFactors = FALSE)
cohort$snapshot_day <- as.Date(cohort$snapshot_day)

# Summarise: actual seated vs official size per parliament
cohort_summary <- cohort |>
  group_by(parliament_id, snapshot_day, parliament_size) |>
  summarise(actual_seated = n(), .groups = "drop") |>
  arrange(snapshot_day)

# Pivot for plotting
plot_data <- cohort_summary |>
  pivot_longer(cols = c(parliament_size, actual_seated),
               names_to = "measure", values_to = "count") |>
  mutate(measure = ifelse(measure == "parliament_size", "Official size", "Actual seated"))

p <- ggplot(plot_data, aes(x = snapshot_day, y = count, color = measure)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Official size" = "black", "Actual seated" = "steelblue")) +
  scale_x_date(name = "Parliament start date") +
  scale_y_continuous(name = "Number of MPs") +
  labs(
    title = paste0("First-Day Cohort Size vs Official Parliament Size - ", country_name),
    subtitle = "Actual MPs seated on leg_period_start vs parliament_size from PARL",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "top")

plot_file <- file.path("completeness", paste0("cohort_size_vs_official_", country_code, ".png"))
ggsave(plot_file, p, width = 14, height = 7, dpi = 150, bg = "white")
cat("Plot saved as", plot_file, "\n")

p
