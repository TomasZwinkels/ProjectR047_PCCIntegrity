######################################################################################
#################################### SETUP ########################################### test
######################################################################################

	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")

		setwd("F:/PolCa/Analysis/R/ProjectR046_GenderCompAtStartDutchPars")
		getwd()
	
	#	install.packages("sqldf")
	#	install.packages("stringr")
	#	install.packages("lubridate")
	#	install.packages("readr")
	#	install.packages("dplyr")
	#	install.packages("writexl")
	
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
			
				# import and inspect election list entries
				ELEN = read.csv("PCC/ELEN.csv", header = TRUE, sep = ";")
				summary(ELEN)
				names(ELEN)

				# import and inspect election districts
				ELDI = read.csv("PCC/ELDI.csv", header = TRUE, sep = ";")
				summary(ELDI)
				names(ELDI)
					
				# import and inspect election lists
				ELLI = read.csv("PCC/ELLI.csv", header = TRUE, sep = ";")
				summary(ELLI)
				names(ELLI)
				
				# import and inspect faction eppisode level info
				FACT = read.csv("PCC/FACT.csv", header = TRUE, sep = ";")
				summary(FACT)
				names(FACT)
				
				# import and inspect party membership eppisodes
				MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
				summary(MEME)
				names(MEME)
				
				# import and inspect parliamentary information
				PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
				summary(PARL)
				names(PARL)
				
				# import and inspect election level information
				ELEC = read.csv("PCC/ELEC.csv", header = TRUE, sep = ";")
				summary(ELEC)
				names(ELEC)
				
				# import and inspect party level information
				PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
				summary(PART)
				names(PART)
				
				# import and inspect politician level information
				POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
				summary(POLI)
				names(POLI)
			
				# import and inspect all the resume entries
				RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
				summary(RESE)
				names(RESE)
				
				# import and inspect quota level info
				QUOT = read.csv("PCC/QUOT.csv", header = TRUE, sep = ";")
				summary(QUOT)
				names(QUOT)
				
				
	# generate a dataframe that contains each (Dutch) MP that was there at the start of each parliamentary term
	
		# TODO
			# clean out the checks here e.t.c, only the start date is needed also.
	
		# lets do some generic RESE cleaning on the start and end dates, for now and for later
			
				# deal with left and right censored dates
					RESE$res_entry_start_cleaned <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
					RESE$res_entry_start_cleaned <- gsub("[[lcen]]","",RESE$res_entry_start_cleaned,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[lcen]]","",RESE$res_entry_end_cleaned,fixed=TRUE)
					
				# deal with dates that are only years (select 1th of June)			
					RESE$res_entry_start_cleaned <- ifelse(nchar(RESE$res_entry_start_cleaned) == 4,paste("01jun",RESE$res_entry_start_cleaned,sep=""),RESE$res_entry_start_cleaned)
					RESE$res_entry_end_cleaned <- ifelse(nchar(RESE$res_entry_end_cleaned) == 4,paste("01jun",RESE$res_entry_end_cleaned,sep=""),RESE$res_entry_end_cleaned)
				
				# deal with dates that are only month and yars
					RESE$res_entry_start_cleaned <- ifelse(nchar(RESE$res_entry_start_cleaned) == 7,paste("01",RESE$res_entry_start_cleaned,sep=""),RESE$res_entry_start_cleaned)
					RESE$res_entry_end_cleaned <- ifelse(nchar(RESE$res_entry_end_cleaned) == 7,paste("01",RESE$res_entry_end_cleaned,sep=""),RESE$res_entry_end_cleaned)
				
					RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start_cleaned),format=c("%d%b%Y"))
					RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end_cleaned),format=c("%d%b%Y"))
					summary(RESE$res_entry_start_posoxctformat)
					summary(RESE$res_entry_end_posoxctformat)
		
		# as a starting point here, we use a filtered subset of the RESE data which contains all the parliamentary membership eppisodes
		
			nrow(RESE)
			RESEBU <- RESE[which(RESE$country_abb == "NL" & RESE$political_function == "NT_LE-LH_T3_NA_01"),]
			nrow(RESEBU)
			head(RESEBU)	
			
			# do we now have valid dates for all? - summary should not contain any NA's anymore
			summary(RESEBU$res_entry_start_posoxctformat)
			summary(RESEBU$res_entry_end_posoxctformat)
			
		# reduce PARL to relevant eppisodes and set the dates to internal R-dates
		
			# reduce
			nrow(PARL)
			PARLBU <- PARL[which(PARL$country_abb == "NL" & PARL$level == "NT"),]
			nrow(PARLBU)
			head(PARLBU)
			
			# dates
			PARLBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_start),format=c("%d%b%Y"))
			PARLBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_end),format=c("%d%b%Y"))
			summary(PARLBU$leg_period_start_posoxctformat)
			summary(PARLBU$leg_period_end_posoxctformat)
			
			# only select the parliaments Thomas is interested in
			PARLBU <- PARLBU[which(PARLBU$leg_period_start_posoxctformat > as.POSIXct("07jun1977",format=c("%d%b%Y"))),]
			head(PARLBU)
			
			
		
	## do the query
	
		head(PARLBU)
		head(RESEBU)
		
		# please note that this now really only includes people that where there on the first day of parliamnent!
		
		# because the end date of some entries is the startdate of the next parliament, we actually look the day after the first day the parliament was in session
		PARLBU$leg_period_start_posoxctformat_daylater <- PARLBU$leg_period_start_posoxctformat + days(1)
		
		PAREBU <- sqldf("
						  SELECT p.parliament_id, p.leg_period_start_posoxctformat, p.leg_period_end_posoxctformat, p.leg_period_start_posoxctformat_daylater, r.pers_id, r.res_entry_start_posoxctformat, r.res_entry_end_posoxctformat
						  FROM PARLBU p
						  INNER JOIN RESEBU r
						  ON p.leg_period_start_posoxctformat_daylater BETWEEN r.res_entry_start_posoxctformat AND r.res_entry_end_posoxctformat
						")
						
		PAREBU[0:20,]
		
		# remove duplicates
		nrow(PAREBU)
		PAREBU <- PAREBU[!duplicated(PAREBU[,c("parliament_id","pers_id")]), ]
		nrow(PAREBU)
		
		PAREBU[0:20,]
		
		PAREBU$parlandpers <- paste0(PAREBU$pers_id,"__",PAREBU$parliament_id)

		PAREBU[which(duplicated(PAREBU$parlandpers)),] # should be empty
		
		
		
		# looks good, but has not be properly checked yet! Do that first the next time! -- in particular on the end dates!
		
		table(PAREBU$parliament_id) # we can see here that sometimes there are to many cases, sometimes to little as well. I think the difference is small enough for it to be OK for Thomas' thesis?
		

		PAREBU[which(PAREBU$parliament_id == "NL_NT-TK_2012"),]

	## merge in the party ids from MEME
	
		# do the merge of party ids from meme
	
			# prepare the dates
					MEME$memep_startdate_cleaned <- gsub("[[rcen]]","",MEME$memep_startdate,fixed=TRUE)
					MEME$memep_startdate_cleaned <- gsub("[[lcen]]","",MEME$memep_startdate_cleaned,fixed=TRUE)
					
					MEME$memep_enddate_cleaned <- gsub("[[rcen]]","",MEME$memep_enddate,fixed=TRUE)
					MEME$memep_enddate_cleaned <- gsub("[[lcen]]","",MEME$memep_enddate_cleaned,fixed=TRUE)
					
				# deal with dates that are only years (select 1th of June)			
					MEME$memep_startdate_cleaned <- ifelse(nchar(MEME$memep_startdate_cleaned) == 4,paste("01jun",MEME$memep_startdate_cleaned,sep=""),MEME$memep_startdate_cleaned)
					MEME$memep_enddate_cleaned <- ifelse(nchar(MEME$memep_enddate_cleaned) == 4,paste("01jun",MEME$memep_enddate_cleaned,sep=""),MEME$memep_enddate_cleaned)
				
				# deal with dates that are only month and yars
					MEME$memep_startdate_cleaned <- ifelse(nchar(MEME$memep_startdate_cleaned) == 7,paste("01",MEME$memep_startdate_cleaned,sep=""),MEME$memep_startdate_cleaned)
					MEME$memep_enddate_cleaned <- ifelse(nchar(MEME$memep_enddate_cleaned) == 7,paste("01",MEME$memep_enddate_cleaned,sep=""),MEME$memep_enddate_cleaned)
				
					MEME$memep_startdate_posoxctformat <- as.POSIXct(as.character(MEME$memep_startdate_cleaned),format=c("%d%b%Y"))
					MEME$memep_enddate_posoxctformat <- as.POSIXct(as.character(MEME$memep_enddate_cleaned),format=c("%d%b%Y"))
				
				# as a temporary workaround, I know that line 22999 in MEME is the first line that has new MEME
				
					MEME[22999,] # yes, him indeed,
					
					# before
					summary(MEME$memep_enddate_posoxctformat)
					
					# what cases are there in the lines before this that have dates past 19sep2012
						# Define a cutoff date and the new end date
						cutoff_date <- as.POSIXct("19sep2012", format="%d%b%Y")
						new_end_date <- as.POSIXct("19sep2012", format="%d%b%Y")  # Replace with your desired date

						# Get indices of rows up to 23000 where the date is after the cutoff
						indices <- which((1:nrow(MEME) <= 22999) & (MEME$memep_enddate_posoxctformat > cutoff_date))

						# Set those dates to the new end date
						MEME$memep_enddate_posoxctformat[indices] <- new_end_date
						
					# after
					summary(MEME$memep_enddate_posoxctformat)
					
				# as an other work-around, lets get rid of all the Frac- and Lid- entries, as we know these only apply to cases where people switched later anyways
			
					nrow(MEME)
					MEME <- MEME[which(!(grepl("Frac-",MEME$party_id,fixed=TRUE) | grepl("Lid-",MEME$party_id,fixed=TRUE))),]
					nrow(MEME)	
					
				# focus on NL
					MEME$country <- substr(MEME$memep_id,1,2)
					table(MEME$country)
					
					nrow(MEME)
					MEME <- MEME[which(MEME$country == "NL"),]
					nrow(MEME)
				
				# check the result in terms of NA's
					summary(MEME$memep_startdate_posoxctformat)
					summary(MEME$memep_enddate_posoxctformat)
					
				# whatsup with the NA values here?
				
					MEME[which(is.na(MEME$memep_startdate_posoxctformat)),] # looks like different time frame
					MEME[which(is.na(MEME$memep_enddate_posoxctformat)),] # looks like no relevant cases for now

		# more simple version with a lot of duplicates that would work with no overlapping meme eppisodes
		TEMP <- sqldf("SELECT PAREBU.*, MEME.party_id
					   FROM PAREBU LEFT JOIN MEME 
					   ON PAREBU.pers_id = MEME.pers_id AND PAREBU.leg_period_start_posoxctformat_daylater BETWEEN MEME.memep_startdate_posoxctformat and MEME.memep_enddate_posoxctformat")
				nrow(PAREBU)
				nrow(TEMP)
				head(TEMP) # hmm, two more, so something double happened here

		TEMP[which(duplicated(TEMP$parlandpers)),]
		
		
		# just give me the first hit that hits on the date as well
		TEMP <- sqldf("
						SELECT 
							PAREBU.*, 
							(
								SELECT MEME.party_id
								FROM MEME
								WHERE MEME.pers_id = PAREBU.pers_id 
								  AND PAREBU.leg_period_start_posoxctformat_daylater BETWEEN MEME.memep_startdate_posoxctformat AND MEME.memep_enddate_posoxctformat
								LIMIT 1
							) AS party_id
						FROM PAREBU
					")
			nrow(PAREBU)
			nrow(TEMP)
			TEMP[0:20,]
			
			table(TEMP$party_id)
			# good enough for now
			PAREBU <- TEMP
		
		# lets get gender etc in and focus in the variables we will actually need later
	
		nrow(PAREBU)
		PAREBU <- sqldf("SELECT PAREBU.parliament_id, PAREBU.pers_id, PAREBU.party_id, POLI.gender, POLI.birth_date
						 FROM PAREBU LEFT JOIN POLI
						 ON PAREBU.pers_id = POLI.pers_id
						")
		nrow(PAREBU)	
		
		PAREBU$parlandparty <- paste0(PAREBU$parliament_id,"__",PAREBU$party_id)
		
		PAREBU[0:20,]
		
		table(PAREBU$parliament_id)
		
	# now lets aggregate
	
		# Calculate the percentage of females per parlandparty
		GENP <- PAREBU %>%
		  group_by(parliament_id, party_id, parlandparty) %>%
		  summarise(
			total = n(),  # Total number of individuals per parlandparty
			females = sum(gender == 'f', na.rm = TRUE),  # Number of females
			percentage_female = (females / total) * 100  # Percentage of females
		  )

		GENP <- as.data.frame(GENP)

		# View the result
		GENP[0:20,]
		GENP

## export for Thomas
	
	timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
	write.xlsx(GENP, file = paste0("NL_Party-Parliament_GenderPercentagesAtStart", timestamp, ".xlsx"))
		
		