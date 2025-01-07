######################################################################################
#################################### SETUP ########################################### test
######################################################################################

	# INSTRUCTIONS
		# typically, one would run this script 'top to bottom' and fix issues higher up 
			# before going on to the subsequent lines of codes (e.g. fix fully overlapping
			# membership episodes before going into checking partially overlapping ones)



	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")

		setwd("F:/PolCa/Analysis/R/ProjectR047_PCCIntegrity")
		getwd()
	
		# install.packages("sqldf")
		# install.packages("stringr")
		# install.packages("lubridate")
		# install.packages("readr")
		# install.packages("dplyr")
		# install.packages("writexl")
	
	# packages
		library(sqldf)
		library(stringr)
		library(lubridate)
		library(readr)
		library(dplyr)
		library(writexl)
		library(openxlsx)

	substrRight <- function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}	
	
	# import and inspect all the PCC data-frames
				
			# core
			
				# import and inspect politician level information
				POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
				summary(POLI)
				names(POLI)
				
				# import and inspect all the resume entries
				RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
				summary(RESE)
				names(RESE)					
			
			# not core at the moment
			
				# import and inspect election list entries
				# ELEN = read.csv("PCC/ELEN.csv", header = TRUE, sep = ";")
				# summary(ELEN)
				# names(ELEN)

				# import and inspect election districts
				# ELDI = read.csv("PCC/ELDI.csv", header = TRUE, sep = ";")
				# summary(ELDI)
				# names(ELDI)
					
				# import and inspect election lists
				# ELLI = read.csv("PCC/ELLI.csv", header = TRUE, sep = ";")
				# summary(ELLI)
				# names(ELLI)
				
				
				# import and inspect faction eppisode level info
				# FACT = read.csv("PCC/FACT.csv", header = TRUE, sep = ";")
				# summary(FACT)
				# names(FACT)
				
				# import and inspect party membership eppisodes
				# MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
				# summary(MEME)
				# names(MEME)
				
				# import and inspect parliamentary information
				# PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
				# summary(PARL)
				# names(PARL)
				
				# import and inspect election level information
				# ELEC = read.csv("PCC/ELEC.csv", header = TRUE, sep = ";")
				# summary(ELEC)
				# names(ELEC)
				
				# import and inspect party level information
				# PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
				# summary(PART)
				# names(PART)
				
				# import and inspect quota level info
				# QUOT = read.csv("PCC/QUOT.csv", header = TRUE, sep = ";")
				# summary(QUOT)
				# names(QUOT)
				
				
## set active filters that define what data to focus on cleaning
	
	# parliamentary episodes in the Netherlands
	nrow(RESE)
	RESE <- RESE[which(RESE$country_abb == "NL" & RESE$political_function == "NT_LE-LH_T3_NA_01"),]	
	nrow(RESE)		


##### Politician Level #####

#### check correct formatting of all the dates

			names(RESE)
			
		# the start dates
		
			# first do the standard cleaning by getting rid off left and right censored dates
					RESE$res_entry_start <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
					RESE$res_entry_start <- gsub("[[lcen]]","",RESE$res_entry_start,fixed=TRUE)
					RESE$res_entry_end <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
					RESE$res_entry_end <- gsub("[[lcen]]","",RESE$res_entry_end,fixed=TRUE)
		
		# how many cases with none typical length 
			# length is 9 for all complete entries
			# length is 4 for all only year entries
			
				table(nchar(RESE$res_entry_start))
				table(nchar(RESE$res_entry_end))
				
			# inspect some of the less usual cases
				RESE[which(nchar(RESE$res_entry_end) == 7),] # so for NL, these are mainly a bunch of cases that have aug2012 as their enddate
		
		# transform to R date and check if all the dates make sense
		
			# transform
			RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start),format=c("%d%b%Y"))
			RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end),format=c("%d%b%Y"))

			# check the result
			table(is.na(RESE$res_entry_start_posoxctformat)) # should return all FALSE
			table(is.na(RESE$res_entry_end_posoxctformat)) # should return all FALSE

		# are all the end dates after the start dates
		
			table(RESE$res_entry_start_posoxctformat < RESE$res_entry_end_posoxctformat)

#### check  for overlapping membership episodes for the same person

	# full overlap: exact same start and end dates
		FDUBS <- RESE[
					duplicated(RESE[, c("pers_id",
									  "res_entry_start_posoxctformat",
									  "res_entry_end_posoxctformat")]) |
					duplicated(RESE[, c("pers_id",
										"res_entry_start_posoxctformat",
										"res_entry_end_posoxctformat")],
							   fromLast = TRUE),
				]
		
		nrow(FDUBS) # OK, almost all cases are fixed now.
		
		# show for inspection and so cases can be fixed.
		FDUBS[,c("res_entry_id","pers_id","res_entry_start","","res_entry_end","res_entry_raw")]
	
		
	
	# almost the exact same start AND OR endate (say 2 day different)
	
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
	
	nrow(AFDUBS) # alright, another 197 cases that need to be fixed!
	head(AFDUBS)
		
		# show for inspection and so cases can be fixed.
		AFDUBS[,c("res_entry_id.x","pers_id","res_entry_start.x","res_entry_end.x","res_entry_start.y","res_entry_end.y","res_entry_raw.x")]