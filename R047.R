# SETUP

# Configuration: Set country code for analysis
country_code <- "CH"  # Options: "NL" (Netherlands), "CH" (Switzerland)

# INSTRUCTIONS
# Run this script 'top to bottom' and fix issues higher up before proceeding
# (e.g. fix fully overlapping membership episodes before checking partially overlapping ones)

# Set language and date formatting to English
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English")  # Required for POSIXct conversion
Sys.getlocale(category = "LC_ALL")

setwd("/home/tomas/projects/ProjectR047_PCCIntegrity")
getwd()
# Package installations (uncomment as needed)
# install.packages("sqldf")
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("writexl")
# install.packages("testthat")

# Load required packages
library(sqldf)
library(stringr)
library(lubridate)
library(readr)
library(dplyr)
library(writexl)
library(openxlsx)
library(testthat)

# Load custom functions and run unit tests
source("R047_functions.R")
test_file("R047_unittests.R")
	
# import and inspect all the PCC data-frames

# core

# import and inspect politician level information
POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
summary(POLI)
names(POLI)
				
# import and inspect all the resume entries
RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
summary(RESE)
names(RESE)	

# import and inspect parliamentary information
PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
summary(PARL)
names(PARL)
			
# Additional datasets available but not loaded:
# ELEN - Election list entries
# ELDI - Election districts  
# ELLI - Election lists
# FACT - Faction episode level info
# MEME - Party membership episodes
# ELEC - Election level information
# PART - Party level information
# QUOT - Quota level info
		
#### DATA PREPROCESSING AND VALIDATION #### 

# RESE
names(RESE)

# Focus on RESE episodes in selected country
nrow(RESE)
RESE <- RESE[which(RESE$country_abb == country_code & RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]
nrow(RESE)

# pre-process and check RESE
source("R047_RESE_functions.R")
test_file("R047_RESE_unittests.R")

# === INSPECTION: Swiss Parliamentary Chambers and Generat Better Values for an IMPORT ===

table(RESE$parliament_id)
sum(grepl("NT-NR", unique(RESE$parliament_id)) & grepl("NT-SR", unique(RESE$parliament_id)))
RESE$chamber <- ifelse(grepl("NT-NR", RESE$parliament_id), "NR", ifelse(grepl("NT-SR", RESE$parliament_id), "SR", "OTHER"))
table(RESE$chamber)

# Check res_entry_raw for chamber consistency
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

# Export chamber-specific political function codes for data update
RESE_export <- RESE[, c("res_entry_id", "political_function", "chamber")]
RESE_export$political_function_updated <- ifelse(
  RESE_export$chamber == "NR", "NT_LE-LH_T3_NA_01",
  ifelse(RESE_export$chamber == "SR", "NT_LE-UH_T3_NA_01", 
         RESE_export$political_function)
)

# Keep only the columns needed for update
RESE_export <- RESE_export[, c("res_entry_id", "political_function_updated")]
names(RESE_export) <- c("res_entry_id", "political_function")

# Export to CSV
write.csv(RESE_export, "RESE_chamber_updates.csv", row.names = FALSE)
cat("Exported", nrow(RESE_export), "records to RESE_chamber_updates.csv\n")
cat("NR (Nationalrat) -> NT_LE-LH_T3_NA_01\n")
cat("SR (Ständerat) -> NT_LE-UH_T3_NA_01\n")

# pre-process
RESE <- preprocess_RESEdates(RESE)

# no NA in dates
check_anyNAinRESEdates(RESE) # should return FALSE

# do all pers_ids occur?
check_RESE_persid_in_POLI(RESE,POLI)

# inspect!
RESE$pers_id[which(! RESE$pers_id %in% POLI$pers_id)]

# are all the res_entry_ids unique?
check_RESE_resentryid_unique(RESE)

# inspect
unique(RESE$res_entry_id[duplicated(RESE$res_entry_id)])
			
# PARL
names(PARL)

# Focus on PARL episodes in selected country
nrow(PARL)
PARL <- PARL[which(PARL$country_abb == country_code),]
nrow(PARL)

# pre-process and check all PARL dates
source("R047_PARL_functions.R")
test_file("R047_PARL_unittests.R")

# pre-process
PARL <- preprocess_PARLdates(PARL)

# no NA
check_anyNAinPARLdates(PARL) # should return FALSE
		
	
#### POLITICIAN LEVEL DATA CHECKS ####

## Check correct formatting of all the RESE dates

## RESE
# how many cases with non-typical length
# length is 9 for all complete entries
# length is 4 for all only year entries

table(nchar(RESE$res_entry_start))
table(nchar(RESE$res_entry_end))

# inspect some of the less usual cases
RESE[which(nchar(RESE$res_entry_end) == 7),] # for NL, mainly cases with aug2012 as enddate

# check the result
table(is.na(RESE$res_entry_start_posoxctformat)) # should return all FALSE
table(is.na(RESE$res_entry_end_posoxctformat)) # should return all FALSE

# are all the end dates after the start dates
table(RESE$res_entry_start_posoxctformat < RESE$res_entry_end_posoxctformat)

## Check for overlapping membership episodes for the same person
	
# check, are there any fully overlapping dates?

# functions and tests already loaded above

check_RESE_parlmemeppisodes_anyfulloverlap(RESE)
	
# full overlap: exact same start and end dates
# note that this is roughly the same code as in 'check_RESE_parlmemeppisodes_fulloverlap'
# but that in this script, we also want to be able to look at these data frames so they can be fixed.
# as such, the philosophy is that we put all the checks in the functions, and they simply return TRUE/FALSE,
# so we can easily run data integrity checks in the other scripts, while here we get all the details on WHAT
# exactly is wrong, so it can be fixed.
FDUBS <- RESE[
					duplicated(RESE[, c("pers_id",
									  "res_entry_start_posoxctformat",
									  "res_entry_end_posoxctformat")]) |
					duplicated(RESE[, c("pers_id",
										"res_entry_start_posoxctformat",
										"res_entry_end_posoxctformat")],
							   fromLast = TRUE),
				]
		
		nrow(FDUBS) 
		
		# show for inspection and so cases can be fixed.
		FDUBS[,c("res_entry_id","pers_id","res_entry_start","res_entry_end","res_entry_raw")] # returns an error if there are no issues.
	
	# almost the exact same start AND OR endate (say 2 day different)
	
	# check, are there any almost fully overlapping dates?
	
		# functions and tests already loaded above
		
		check_RESE_anynear_fulloverlap(RESE)
	
	# see comment above, also here some code is 'repeated' so this is the 'script version' of the function above
	# so we can do inspections!
		# focus on core variables
			RECO <- RESE[,c("res_entry_id","pers_id","res_entry_start","res_entry_start_posoxctformat","res_entry_end","res_entry_end_posoxctformat","res_entry_raw")]
		
			AFDUBS <- RECO %>%
				  # 1. Add a ROWID so we can avoid matching the same row to itself
				  mutate(ROWID = row_number()) %>%
				  
				  # 2. Self-join on pers_id to compare every row with every other row
				  inner_join(
					RECO %>% mutate(ROWID = row_number()),
					by = "pers_id",
					suffix = c(".x", ".y")
				  ) %>%
				  
				  # 3. Filter to keep only pairs of rows that
				  #    - are not the exact same row (ROWID.x < ROWID.y)
				  #    - have start dates within 2 days
				  #    - have end dates within 2 days
				  filter(
					ROWID.x < ROWID.y,
					abs(difftime(res_entry_start_posoxctformat.x, 
								 res_entry_start_posoxctformat.y, 
								 units = "days")) <= 2,
					abs(difftime(res_entry_end_posoxctformat.x, 
								 res_entry_end_posoxctformat.y, 
								 units = "days")) <= 2
				  )
			
			nrow(AFDUBS) 
			head(AFDUBS)
		
		# show for inspection and so cases can be fixed. # only one case left now: was already fixed in the excel file
		AFDUBS[,c("res_entry_id.x","pers_id","res_entry_start.x","res_entry_end.x","res_entry_start.y","res_entry_end.y","res_entry_raw.x")]
		
		# alright, so the next step is find episodes with any overal for the same person
		
		return_overlap <- function(df) {
			  df %>%
				mutate(ROWID = row_number()) %>%
				inner_join(df %>% mutate(ROWID = row_number()),
						   by = "pers_id",
						   suffix = c(".x", ".y")) %>%
				# Only compare each pair once (ROWID.x < ROWID.y) and check for overlapping intervals
				filter(ROWID.x < ROWID.y,
					   res_entry_start_posoxctformat.x <= res_entry_end_posoxctformat.y,
					   res_entry_start_posoxctformat.y <= res_entry_end_posoxctformat.x) %>%
				# Select key variables for inspection
				select(res_entry_id.x,
					   pers_id,
					   res_entry_start.x,
					   res_entry_end.x,
					   res_entry_raw.x,
					   res_entry_id.y,
					   res_entry_start.y,
					   res_entry_end.y,
					   res_entry_raw.y)
			}

	
		OVERLAP <- return_overlap(RESE) # please note that the 'many-to-many' error makes sense here
			
			# get a vector with all the unique pers_ids that have overlapping eppisodes
			allpersonswithoverlap <- unique(OVERLAP$pers_id)
			allpersonswithoverlap
			length(allpersonswithoverlap)
			
			
			# so I can check if the right amount of people got deleted: how many rows do these people all together have in RESE?
			nrow(RESE[which(RESE$pers_id %in% allpersonswithoverlap),])
			
			
		# alright, I have made a function that merges eppisodes on the basis of pers_ids, lets test it here -- please note that we use RESE as the input here! not OVERLAP
			# Replace "NL_Agema_Fleur_1976" with the pers_id you want to check.
			
			# I want to test and check a couple manually
				RESE[which(RESE$pers_id == "NL_Agema_Fleur_1976"),]
				merged_intervals <- merge_episodes(RESE,"NL_Agema_Fleur_1976")
				merged_intervals # alright, this looks correct.
			
				RESE[which(RESE$pers_id == "NL_Voordewind_Joel_1965"),]
				merged_intervals <- merge_episodes(RESE,"NL_Voordewind_Joel_1965")
				merged_intervals # alright, this looks correct.
			
				RESE[which(RESE$pers_id == "NL_Plasterk_Ronald_1957"),]
				merged_intervals <- merge_episodes(RESE,"NL_Plasterk_Ronald_1957")
				merged_intervals # alright, this looks correct.
				
				RESE[which(RESE$pers_id == "NL_Boreel_Willem_1800"),]
				merged_intervals <- merge_episodes(RESE,"NL_Boreel_Willem_1800")
				merged_intervals # alright, this looks correct.
				
		# export the whole shabang
		
			# Initialize IMPORT as an empty data frame
			IMPORT_DATA <- NULL

			# Loop over all persons with overlapping episodes and append merged intervals
			for (pid in allpersonswithoverlap) {
			  merged_intervals <- merge_episodes(RESE, pid)
			  if (!is.null(merged_intervals)) {
				IMPORT_DATA <- rbind(IMPORT_DATA, merged_intervals)
			  }
			}
			
			# some checks on import
			head(IMPORT_DATA)
			nrow(IMPORT_DATA)
			
			# for the function return_overlap to work we need posixdates in the dataframe
			IMPORT_DATA$res_entry_start_posoxctformat <- as.POSIXct(as.character(IMPORT_DATA$res_entry_start),format=c("%d%b%Y"))
			IMPORT_DATA$res_entry_end_posoxctformat <- as.POSIXct(as.character(IMPORT_DATA$res_entry_end),format=c("%d%b%Y"))
			
			# does it pass the check defined above of no overlapping intervals?
				return_overlap(IMPORT_DATA) # should return zero

				# Remove the POSIXct date columns from IMPORT as they are not needed in the export
				IMPORT_DATA <- IMPORT_DATA %>% select(-res_entry_start_posoxctformat, -res_entry_end_posoxctformat)

# Create a timestamped filename in one line and export to Excel
filename <- paste0("IMPORT_MERGED_NLRESE_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
# write.xlsx(IMPORT_DATA, file = filename)

#################################### GAP DETECTION: EPISODES THAT ARE VERY CLOSE TOGETHER ##################################

GAPS <- find_gap_episodes(RESE,1,3)
nrow(GAPS)

GAPS

#################################### SUSPICIOUS DATES ##################################

# Alright, so these are for sure not things that are a hard check.

# Run the two suspicious dates functions (for start and end dates)
SUSPICIOUS_START_DATES <- find_suspicious_start_dates(RESE, PARL, threshold_days = 3)
SUSPICIOUS_START_DATES

SUSPICIOUS_END_DATES <- find_suspicious_end_dates(RESE, PARL, threshold_days = 3)
SUSPICIOUS_END_DATES

# Optional: Export results to Excel
# write.xlsx(SUSPICIOUS_START_DATES, file = paste0("Suspicious_Start_Dates_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
# write.xlsx(SUSPICIOUS_END_DATES, file = paste0("Suspicious_End_Dates_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))