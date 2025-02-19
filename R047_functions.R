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
    
    # If overlapping or contiguous, extend the current interval
    if (current_start <= merged_end) {
      merged_end <- max(merged_end, current_end)
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
