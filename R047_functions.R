###############################################################################
# Function: substrRight
#
# Description:
#   Returns the rightmost n characters from a given string.
#
###############################################################################

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

###############################################################################
# Function: merge_episodes
#
# Description:
#   For a given person (identified by pid) and a data frame containing membership
#   episodes, this function orders the episodes by start date and merges overlapping
#   or contiguous episodes into a minimal set of intervals. It returns a data frame
#   ready for database import with fields such as res_entry_id, pers_id, country_abb,
#   res_entry_type, res_entry_start, res_entry_end, res_entry_raw, political_function,
#   pf_geolevel, pf_instdomain, pf_orglevel, pf_policy_area, pf_position, and
#   res_entry_source. The res_entry_source field now includes a label indicating
#   whether the interval is the first, an intermediate, or the last.
#
# Inputs:
#   - data: A data frame that includes at least the following columns:
#         pers_id, res_entry_start_posoxctformat, res_entry_end_posoxctformat,
#         and optionally country_abb.
#   - pid: A character string representing the person's identifier.
#
# Output:
#   - A data frame of merged intervals.
###############################################################################
merge_episodes <- function(data, pid) {
  # Filter episodes for the given person and sort by start date
  df <- data %>%
    dplyr::filter(pers_id == pid) %>%
    dplyr::arrange(res_entry_start_posoxctformat)
  
  if (nrow(df) == 0) {
    message("No episodes found for pers_id: ", pid)
    return(NULL)
  }
  
  # Determine the country value; default to "NL" if the column is missing.
  country_value <- if ("country_abb" %in% names(df)) {
    unique(df$country_abb)[1]
  } else {
    "NL"
  }
  
  # Initialize the first interval using the first episode
  merged_start <- df$res_entry_start_posoxctformat[1]
  merged_end   <- df$res_entry_end_posoxctformat[1]
  
  # List to hold merged intervals
  merged_intervals <- list()
  
  # Loop through the remaining episodes
  for (i in 2:nrow(df)) {
    current_start <- df$res_entry_start_posoxctformat[i]
    current_end   <- df$res_entry_end_posoxctformat[i]
    
    # If overlapping or contiguous, extend the current interval (handle NA values)
    # For nested intervals, we need to check if the current episode is completely within the merged interval
    if (!is.na(current_start) && !is.na(merged_end) && 
        (current_start <= merged_end || 
         (current_start <= merged_start && current_end >= merged_start) || 
         (current_start >= merged_start && current_end <= merged_end))) {
      merged_end <- max(merged_end, current_end, na.rm = TRUE)
    } else {
      # Finalize the current interval and assign a label:
      # If no interval has been finalized yet, this is the first interval;
      # otherwise, it's an intermediate interval.
      label_text <- if (length(merged_intervals) == 0) {
        "first interval"
      } else {
        "intermediate interval"
      }
      
      merged_intervals[[length(merged_intervals) + 1]] <- data.frame(
        res_entry_id    = paste0(pid, "__[incr]"),
        pers_id         = pid,
        country_abb     = country_value,
        res_entry_type  = "pol",
        res_entry_start = tolower(format(merged_start, "%d%b%Y")),
        res_entry_end   = tolower(format(merged_end, "%d%b%Y")),
        res_entry_raw   = paste0("lid Tweede Kamer der Staten-Generaal van ",
                                 tolower(format(merged_start, "%d%b%Y")),
                                 " tot ",
                                 tolower(format(merged_end, "%d%b%Y"))),
        political_function = "NT_LE-LH_T3_NA_01",
        pf_geolevel        = "NT",
        pf_instdomain      = "LE-LH",
        pf_orglevel        = "T3",
        pf_policy_area     = "NA",
        pf_position        = "01",
        res_entry_source   = paste("Feb 2025 automatic merge of overlapping parliamentary membership episodes, so slack and R047", label_text),
        stringsAsFactors   = FALSE
      )
      
      # Reset the merged interval to start with the current episode
      merged_start <- current_start
      merged_end   <- current_end
    }
  }
  
  # Finalize the last interval.
  label_text <- if (length(merged_intervals) == 0) {
    "first interval, last interval"
  } else {
    "last interval"
  }
  
  merged_intervals[[length(merged_intervals) + 1]] <- data.frame(
    res_entry_id    = paste0(pid, "__[incr]"),
    pers_id         = pid,
    country_abb     = country_value,
    res_entry_type  = "pol",
    res_entry_start = tolower(format(merged_start, "%d%b%Y")),
    res_entry_end   = tolower(format(merged_end, "%d%b%Y")),
    res_entry_raw   = paste0("lid Tweede Kamer der Staten-Generaal van ",
                             tolower(format(merged_start, "%d%b%Y")),
                             " tot ",
                             tolower(format(merged_end, "%d%b%Y"))),
    political_function = "NT_LE-LH_T3_NA_01",
    pf_geolevel        = "NT",
    pf_instdomain      = "LE-LH",
    pf_orglevel        = "T3",
    pf_policy_area     = "NA",
    pf_position        = "01",
    res_entry_source   = paste("Feb 2025 automatic merge of overlapping parliamentary membership episodes, so slack and R047", label_text),
    stringsAsFactors   = FALSE
  )
  
  # Combine all merged intervals into a single data frame
  result_df <- do.call(rbind, merged_intervals)
  
  # Check if no merging occurred (more than one original episode and the merged intervals
  # count equals the number of original rows). If so, print a warning.
  if(nrow(df) > 1 && nrow(result_df) == nrow(df)) {
    warning("merge_episodes: episodes for pers_id '", pid, "' are already in their simplest form. No merging performed.")
  }
  
  return(result_df)
}

###############################################################################
# Function: find_gap_episodes
#
# Description:
#   For each person in the provided data frame, this function identifies 
#   consecutive episodes where there is a gap (in days) that is strictly greater 
#   than a specified minimum gap (min_gap) and strictly less than a specified 
#   gap threshold (gap_threshold). It adds a column for the previous episode's 
#   res_entry_id (previous_res_entry_id) and renames the current episode's 
#   res_entry_id as current_res_entry_id. Both the previous_end and res_entry_start 
#   dates are formatted consistently as "%d%b%Y" in lowercase.
#
# Inputs:
#   - data: A data frame that must include at least:
#         pers_id, res_entry_id, res_entry_start_posoxctformat, and 
#         res_entry_end_posoxctformat.
#   - min_gap: A numeric value (default = 1) specifying the minimum gap (in days)
#         to ignore (i.e. gaps equal to 1 day are not flagged).
#   - gap_threshold: A numeric value (default = 3) specifying the maximum gap 
#         (in days) to be flagged.
#
# Output:
#   - A base R data frame with columns: 
#         pers_id, previous_res_entry_id, current_res_entry_id, previous_end, 
#         res_entry_start, gap_days.
###############################################################################
find_gap_episodes <- function(data, min_gap = 1, gap_threshold = 3) {
  df <- data %>%
    group_by(pers_id) %>%
    arrange(res_entry_start_posoxctformat) %>%
    mutate(
      previous_end = lag(res_entry_end_posoxctformat),
      previous_res_entry_id = lag(res_entry_id),
      gap_days = round(as.numeric(difftime(res_entry_start_posoxctformat, previous_end, units = "days")), 0)
    ) %>%
    filter(!is.na(gap_days) & gap_days > min_gap & gap_days < gap_threshold) %>%
    ungroup() %>%
    mutate(
      previous_end = tolower(format(previous_end, "%d%b%Y")),
      res_entry_start = tolower(format(res_entry_start_posoxctformat, "%d%b%Y"))
    ) %>%
    select(pers_id, previous_res_entry_id, current_res_entry_id = res_entry_id, previous_end, res_entry_start, gap_days)
  
  as.data.frame(df)
}


###############################################################################
# Function: import_pcc_sample
#
# Description:
#   Imports the first n rows of all CSV files in the specified PCC directory.
#   This function is useful for examining the structure of the files without
#   loading all the data, which can be resource-intensive.
#
# Inputs:
#   - pcc_dir: A character string specifying the directory containing PCC CSV files
#              (default = "PCC")
#   - n_rows: A numeric value specifying the number of rows to import from each file
#             (default = 10)
#   - sep: A character specifying the separator used in the CSV files
#          (default = ";")
#
# Output:
#   - A named list of data frames, where each data frame contains the first n rows
#     of a CSV file, and the name of each element is the filename without extension.
###############################################################################
import_pcc_sample <- function(pcc_dir = "PCC", n_rows = 10, sep = ";") {
  # Get list of CSV files in the PCC directory
  csv_files <- list.files(path = pcc_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    warning("No CSV files found in directory: ", pcc_dir)
    return(NULL)
  }
  
  # Initialize empty list to store dataframes
  PCC_DATA <- list()
  
  # Import first n rows of each CSV file
  for (file_path in csv_files) {
    # Extract filename without extension to use as list name
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Read first n rows of the CSV file
    DF <- read.csv(file_path, header = TRUE, sep = sep, nrows = n_rows)
    
    # Add dataframe to list with filename as name
    PCC_DATA[[file_name]] <- DF
    
    # Print message about imported file
    message("Imported ", n_rows, " rows from ", file_path)
  }
  
  return(PCC_DATA)
}

###############################################################################
# Function: find_suspicious_start_dates
#
# Description:
#   Identifies RESE entries with suspicious start dates compared to the 
#   parliament dates in the PARL data frame. A start date is flagged as 
#   suspicious if the absolute difference between the RESE start date and the 
#   closest parliament start date (leg_period_start_posoxctformat) is greater than 
#   zero and less than or equal to threshold_days.
#
# Inputs:
#   - RESE: A data frame containing resume entries with at least:
#         pers_id, res_entry_id, res_entry_start_posoxctformat, res_entry_end_posoxctformat
#   - PARL: A data frame containing parliament data with at least:
#         leg_period_start_posoxctformat, leg_period_end_posoxctformat, (and optionally parl_id)
#   - threshold_days: A numeric value specifying the maximum number of days
#         difference to consider suspicious (default = 14)
#
# Output:
#   - A data frame containing entries with suspicious start dates with columns:
#         pers_id, res_entry_id, res_entry_start, res_entry_end,
#         closest_parl_start, closest_parl_end, start_diff_days, parl_id
###############################################################################
find_suspicious_start_dates <- function(RESE, PARL, threshold_days = 14) {
  suspicious_entries <- list()
  
  # Process each RESE entry (checking start dates)
  for (i in 1:nrow(RESE)) {
    entry <- RESE[i, ]
    
    # Skip if start date is missing
    if (is.na(entry$res_entry_start_posoxctformat)) {
      next
    }
    
    # Compute absolute differences (in days) for the start date
    start_diffs <- abs(as.numeric(difftime(entry$res_entry_start_posoxctformat,
                                             PARL$leg_period_start_posoxctformat,
                                             units = "days")))
    start_diffs <- round(start_diffs, 0)
    
    closest_start_idx <- which.min(start_diffs)
    min_start_diff <- start_diffs[closest_start_idx]
    
    # If the start date is suspicious, record it
    if (min_start_diff > 0 && min_start_diff <= threshold_days) {
      suspicious_entries[[length(suspicious_entries) + 1]] <- data.frame(
        pers_id = entry$pers_id,
        res_entry_id = entry$res_entry_id,
        res_entry_start = tolower(format(entry$res_entry_start_posoxctformat, "%d%b%Y")),
        res_entry_end = tolower(format(entry$res_entry_end_posoxctformat, "%d%b%Y")),
        closest_parl_start = tolower(format(PARL$leg_period_start_posoxctformat[closest_start_idx], "%d%b%Y")),
        closest_parl_end = tolower(format(PARL$leg_period_end_posoxctformat[closest_start_idx], "%d%b%Y")),
        start_diff_days = min_start_diff,
        parl_id = if("parl_id" %in% names(PARL)) PARL$parl_id[closest_start_idx] else NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(suspicious_entries) == 0) {
    message("No suspicious start dates found within threshold of ", threshold_days, " days.")
    return(NULL)
  }
  
  result_df <- do.call(rbind, suspicious_entries)
  return(result_df)
}

###############################################################################
# Function: find_suspicious_end_dates
#
# Description:
#   Identifies RESE entries with suspicious end dates compared to the 
#   parliament dates in the PARL data frame. An end date is flagged as 
#   suspicious if the absolute difference between the RESE end date and the 
#   closest parliament end date (leg_period_end_posoxctformat) is greater than 
#   zero and less than or equal to threshold_days.
#
# Inputs:
#   - RESE: A data frame containing resume entries with at least:
#         pers_id, res_entry_id, res_entry_start_posoxctformat, res_entry_end_posoxctformat
#   - PARL: A data frame containing parliament data with at least:
#         leg_period_start_posoxctformat, leg_period_end_posoxctformat, (and optionally parl_id)
#   - threshold_days: A numeric value specifying the maximum number of days
#         difference to consider suspicious (default = 14)
#
# Output:
#   - A data frame containing entries with suspicious end dates with columns:
#         pers_id, res_entry_id, res_entry_start, res_entry_end,
#         closest_parl_start, closest_parl_end, end_diff_days, parl_id
###############################################################################
find_suspicious_end_dates <- function(RESE, PARL, threshold_days = 14) {
  suspicious_entries <- list()
  
  # Process each RESE entry (checking end dates)
  for (i in 1:nrow(RESE)) {
    entry <- RESE[i, ]
    
    # Skip if end date is missing
    if (is.na(entry$res_entry_end_posoxctformat)) {
      next
    }
    
    # Compute absolute differences (in days) for the end date
    end_diffs <- abs(as.numeric(difftime(entry$res_entry_end_posoxctformat,
                                           PARL$leg_period_end_posoxctformat,
                                           units = "days")))
    end_diffs <- round(end_diffs, 0)
    
    closest_end_idx <- which.min(end_diffs)
    min_end_diff <- end_diffs[closest_end_idx]
    
    # If the end date is suspicious, record it
    if (min_end_diff > 0 && min_end_diff <= threshold_days) {
      suspicious_entries[[length(suspicious_entries) + 1]] <- data.frame(
        pers_id = entry$pers_id,
        res_entry_id = entry$res_entry_id,
        res_entry_start = tolower(format(entry$res_entry_start_posoxctformat, "%d%b%Y")),
        res_entry_end = tolower(format(entry$res_entry_end_posoxctformat, "%d%b%Y")),
        closest_parl_start = tolower(format(PARL$leg_period_start_posoxctformat[closest_end_idx], "%d%b%Y")),
        closest_parl_end = tolower(format(PARL$leg_period_end_posoxctformat[closest_end_idx], "%d%b%Y")),
        end_diff_days = min_end_diff,
        parl_id = if("parl_id" %in% names(PARL)) PARL$parl_id[closest_end_idx] else NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(suspicious_entries) == 0) {
    message("No suspicious end dates found within threshold of ", threshold_days, " days.")
    return(NULL)
  }
  
  result_df <- do.call(rbind, suspicious_entries)
  return(result_df)
}

