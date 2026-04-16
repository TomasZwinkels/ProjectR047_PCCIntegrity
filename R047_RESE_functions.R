###############################################################################
# Function: check_RESE_persid_in_POLI
# Description:
#   Check whether all pers_id values in the RESE data.frame
#   also occur in the POLI data.frame.
#
# Inputs:
#   - RESE: data.frame with column pers_id
#   - POLI: data.frame with column pers_id
#
# Returns:
#   - TRUE  if all RESE$pers_id are present in POLI$pers_id
#   - FALSE if one or more RESE$pers_id are missing from POLI
###############################################################################
check_RESE_persid_in_POLI <- function(RESE, POLI) {
  # sanity: required cols
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")

  missing_ids <- setdiff(unique(RESE$pers_id), unique(POLI$pers_id))
  length(missing_ids) == 0
}

###############################################################################
# Function: check_RESE_resentryid_unique
# Description:
#   Check whether all res_entry_id values in RESE are unique.
#
# Inputs:
#   - RESE: data.frame with column res_entry_id
#
# Returns:
#   - TRUE  if no duplicates (including NA duplicates) are present
#   - FALSE if one or more duplicates exist
###############################################################################
check_RESE_resentryid_unique <- function(RESE) {
  if (!"res_entry_id" %in% names(RESE)) {
    stop("RESE is missing column res_entry_id")
  }
  !any(duplicated(RESE$res_entry_id))
}


###############################################################################
# Function: preprocess_RESEdates
# Description:
#   Turn RESE dates (PCC format like "01Jan2020", possibly with [[lcen]]/[[rcen]])
#   into POSIXct columns *_posoxctformat. Warn if any dates fail to parse.
# Input:
#   - RESELOC: data.frame with res_entry_start / res_entry_end (character)
# Output:
#   - RESELOC with res_entry_start_posoxctformat / res_entry_end_posoxctformat
###############################################################################

preprocess_RESEdates <- function(RESELOC) {

  # remember locale, switch to a safe one for %b month parsing, restore on exit
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  suppressWarnings({
    ok <- Sys.setlocale("LC_TIME", "C")
    if (is.na(ok)) Sys.setlocale("LC_TIME", "English")
  })

  # strip censor tags and normalize empties to NA
  strip_tags <- function(x) {
    x <- gsub("[[rcen]]", "", x, fixed = TRUE)
    x <- gsub("[[lcen]]", "", x, fixed = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }

  RESELOC$res_entry_start <- strip_tags(RESELOC$res_entry_start)
  RESELOC$res_entry_end   <- strip_tags(RESELOC$res_entry_end)

  # parse to POSIXct (PCC uses like 01Jan2020)
  RESELOC$res_entry_start_posoxctformat <-
    as.POSIXct(as.character(RESELOC$res_entry_start), format = "%d%b%Y", tz = "UTC")
  RESELOC$res_entry_end_posoxctformat   <-
    as.POSIXct(as.character(RESELOC$res_entry_end),   format = "%d%b%Y", tz = "UTC")

  # quick warning if any parse failed
  anystartdatesmissing <- sum(is.na(RESELOC$res_entry_start_posoxctformat)) > 0
  anyenddatesmissing   <- sum(is.na(RESELOC$res_entry_end_posoxctformat))   > 0

  if (anystartdatesmissing || anyenddatesmissing) {
    message(
      "WARNING: not all dates could be converted successfully. ",
      "Missing start: ", sum(is.na(RESELOC$res_entry_start_posoxctformat)),
      " | Missing end: ", sum(is.na(RESELOC$res_entry_end_posoxctformat))
    )
  }

  RESELOC
}

###############################################################################
# Function: check_anyNAinRESEdates
# Description:
#   Returns TRUE if there are any NAs in either parsed RESE date column.
#
# Inputs:
#   - RESELOC: data.frame with
#       res_entry_start_posoxctformat (POSIXct)
#       res_entry_end_posoxctformat   (POSIXct)
#
# Returns:
#   - TRUE  if there are any NA values in start or end columns
#   - FALSE if all values are non-missing
###############################################################################
check_anyNAinRESEdates <- function(RESELOC) {
  anystartdatesmissing <- sum(is.na(RESELOC$res_entry_start_posoxctformat)) > 0
  anyenddatesmissing   <- sum(is.na(RESELOC$res_entry_end_posoxctformat))   > 0
  anystartdatesmissing || anyenddatesmissing
}

###############################################################################
# Function: check_RESE_inverted_dates
# Description:
#   Check whether any RESE episodes have an end date that is before the start
#   date (inverted dates). This is a data integrity issue.
#
# Inputs:
#   - RESELOC: data.frame with
#       res_entry_start_posoxctformat (POSIXct)
#       res_entry_end_posoxctformat   (POSIXct)
#
# Returns:
#   - TRUE  if there are any episodes where end date < start date
#   - FALSE if all episodes have valid date order (start <= end)
###############################################################################
check_RESE_inverted_dates <- function(RESELOC) {
  req <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req, names(RESELOC))
  if (length(miss) > 0) {
    stop("RESELOC is missing columns: ", paste(miss, collapse = ", "))
  }

  # Compare dates, ignoring rows where either date is NA
  start_dates <- RESELOC$res_entry_start_posoxctformat
  end_dates <- RESELOC$res_entry_end_posoxctformat

  # Only check rows where both dates are non-NA
  valid_rows <- !is.na(start_dates) & !is.na(end_dates)

  if (sum(valid_rows) == 0) {
    return(FALSE)  # No valid date pairs to check

  }

  any(end_dates[valid_rows] < start_dates[valid_rows])
}


###############################################################################
# Function: check_RESE_parlmemeppisodes_anyfulloverlap
# Description:
#   Check whether there are any fully overlapping parliamentary membership
#   episodes in RESE. Two episodes are considered duplicates if they have
#   the same pers_id, res_entry_start_posoxctformat, and res_entry_end_posoxctformat.
#   Only rows with political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01") are checked.
#
# Returns:
#   - TRUE  if one or more full duplicates are found
#   - FALSE if no full duplicates are found
#   - TRUE  (with warning) if no parliamentary membership episodes exist
###############################################################################

check_RESE_parlmemeppisodes_anyfulloverlap <- function(RESE) {

  # filter on parliamentary membership episodes only
  RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")), ]  

  # no relevant rows -> warn + return TRUE (this IS a data integrity issue)
  if (nrow(RESE) == 0) {
    warning("No parliamentary membership episodes found in RESE")
    return(TRUE)
  }

  # get a dataframe with all the duplicates
  FDUBS <- RESE[
    duplicated(RESE[, c("pers_id",
                        "res_entry_start_posoxctformat",
                        "res_entry_end_posoxctformat")]) |
    duplicated(RESE[, c("pers_id",
                        "res_entry_start_posoxctformat",
                        "res_entry_end_posoxctformat")],
               fromLast = TRUE),
  ]

  # TRUE if any duplicates exist, FALSE otherwise
  nrow(FDUBS) > 0
}

###############################################################################
# Function: check_RESE_anynear_fulloverlap
# Description:
#   Check whether any pairs of RESE rows (per pers_id) have start and end dates
#   within a given tolerance (in days).
#
# Inputs:
#   - RESE: data.frame with at least:
#           pers_id,
#           res_entry_start_posoxctformat (POSIXct),
#           res_entry_end_posoxctformat   (POSIXct)
#   - tolerance_days: integer(1), inclusive window for abs difference (default 2)
#
# Returns:
#   - TRUE  if one or more near-full-overlap pairs exist
#   - FALSE otherwise (including empty input)
###############################################################################
check_RESE_anynear_fulloverlap <- function(RESE, tolerance_days = 2) {
  # sanity: required cols
  req <- c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req, names(RESE))
  if (length(miss)) stop("Missing required columns in RESE: ", paste(miss, collapse = ", "))

  if (nrow(RESE) < 2) return(FALSE)  # can't have overlaps with < 2 rows

  RECO <- RESE[, req, drop = FALSE]

  AFDUBS <- dplyr::mutate(RECO, ROWID = dplyr::row_number()) %>%
    dplyr::inner_join(
      dplyr::mutate(RECO, ROWID = dplyr::row_number()),
      by = "pers_id",
      relationship = "many-to-many",  # silence expected self-join warning
      suffix = c(".x", ".y")
    ) %>%
    dplyr::filter(
      ROWID.x < ROWID.y,
      !is.na(res_entry_start_posoxctformat.x),
      !is.na(res_entry_start_posoxctformat.y),
      !is.na(res_entry_end_posoxctformat.x),
      !is.na(res_entry_end_posoxctformat.y),
      abs(as.numeric(difftime(
        res_entry_start_posoxctformat.x,
        res_entry_start_posoxctformat.y,
        units = "days"
      ))) <= tolerance_days,
      abs(as.numeric(difftime(
        res_entry_end_posoxctformat.x,
        res_entry_end_posoxctformat.y,
        units = "days"
      ))) <= tolerance_days
    )

  nrow(AFDUBS) > 0
}

###############################################################################
# DETAILS FUNCTIONS - Return detailed data objects for inspection
###############################################################################

###############################################################################
# Function: check_RESE_persid_in_POLI_details
# Description: Return missing person IDs and related data for detailed inspection
# Returns: List with missing_ids vector, missing_count, and summary data
###############################################################################
check_RESE_persid_in_POLI_details <- function(RESE, POLI) {
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")

  missing_ids <- setdiff(unique(RESE$pers_id), unique(POLI$pers_id))
  
  # Return rows from RESE that have missing person IDs
  missing_rows <- if(length(missing_ids) > 0) {
    RESE[RESE$pers_id %in% missing_ids, , drop = FALSE]
  } else {
    RESE[0, , drop = FALSE]
  }
  
  list(
    check_passed = length(missing_ids) == 0,
    missing_ids = missing_ids,
    missing_count = length(missing_ids),
    missing_rows = missing_rows,
    total_unique_rese_ids = length(unique(RESE$pers_id)),
    total_unique_poli_ids = length(unique(POLI$pers_id))
  )
}

###############################################################################
# Function: check_RESE_resentryid_unique_details  
# Description: Return duplicate entry IDs and all rows containing duplicates
# Returns: List with duplicate_ids vector and duplicate_rows data.frame
###############################################################################
check_RESE_resentryid_unique_details <- function(RESE) {
  if (!"res_entry_id" %in% names(RESE)) {
    stop("RESE is missing column res_entry_id")
  }
  
  duplicated_logical <- duplicated(RESE$res_entry_id)
  duplicate_ids <- if(any(duplicated_logical)) {
    unique(RESE$res_entry_id[duplicated_logical])
  } else {
    character(0)
  }
  
  # Return ALL rows that contain any duplicate ID (not just the duplicated ones)
  duplicate_rows <- if(length(duplicate_ids) > 0) {
    RESE[RESE$res_entry_id %in% duplicate_ids, , drop = FALSE]
  } else {
    RESE[0, , drop = FALSE]
  }
  
  list(
    check_passed = !any(duplicated_logical),
    duplicate_ids = duplicate_ids,
    duplicate_count = length(duplicate_ids),
    duplicate_rows = duplicate_rows,
    total_rows = nrow(RESE)
  )
}

###############################################################################
# Function: check_anyNAinRESEdates_details
# Description: Return rows and indices with NA dates after preprocessing  
# Returns: List with NA row indices and the actual rows with problems
###############################################################################
check_anyNAinRESEdates_details <- function(RESELOC) {
  req <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req, names(RESELOC))
  if (length(miss) > 0) {
    stop("RESELOC is missing columns: ", paste(miss, collapse = ", "))
  }
  
  na_start <- is.na(RESELOC$res_entry_start_posoxctformat)
  na_end <- is.na(RESELOC$res_entry_end_posoxctformat)
  na_either <- na_start | na_end
  
  list(
    check_passed = !any(na_start) && !any(na_end),
    na_start_count = sum(na_start),
    na_end_count = sum(na_end),
    na_start_rows = which(na_start),
    na_end_rows = which(na_end),
    na_either_rows = which(na_either),
    full_rows_with_na_dates = RESELOC[na_either, , drop = FALSE],
    total_rows = nrow(RESELOC)
  )
}

###############################################################################
# Function: check_RESE_inverted_dates_details
# Description: Return rows where end date is before start date (inverted dates)
# Returns: List with inverted_rows data.frame and summary statistics
###############################################################################
check_RESE_inverted_dates_details <- function(RESELOC) {
  req <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req, names(RESELOC))
  if (length(miss) > 0) {
    stop("RESELOC is missing columns: ", paste(miss, collapse = ", "))
  }

  start_dates <- RESELOC$res_entry_start_posoxctformat
  end_dates <- RESELOC$res_entry_end_posoxctformat

  # Only check rows where both dates are non-NA
  valid_rows <- !is.na(start_dates) & !is.na(end_dates)

  # Find inverted rows (end < start)
  inverted <- valid_rows & (end_dates < start_dates)

  # Calculate the difference in days for inverted rows
  inverted_rows <- RESELOC[inverted, , drop = FALSE]
  if (nrow(inverted_rows) > 0) {
    inverted_rows$date_diff_days <- as.numeric(difftime(
      inverted_rows$res_entry_end_posoxctformat,
      inverted_rows$res_entry_start_posoxctformat,
      units = "days"
    ))
  }

  list(
    check_passed = !any(inverted),
    inverted_count = sum(inverted),
    inverted_row_indices = which(inverted),
    inverted_rows = inverted_rows,
    total_rows = nrow(RESELOC),
    valid_date_pairs = sum(valid_rows)
  )
}

###############################################################################
# Function: check_RESE_parlmemeppisodes_anyfulloverlap_details
# Description: Return all overlapping parliamentary episodes as data.frame
# Returns: List with overlapping episodes data and affected persons
###############################################################################
check_RESE_parlmemeppisodes_anyfulloverlap_details <- function(RESE) {
  parl_episodes <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01","NT_LE_T3_NA_09")), , drop = FALSE]  
  
  if (nrow(parl_episodes) == 0) {
    return(list(
      check_passed = FALSE,
      warning_message = "No parliamentary membership episodes found",
      overlapping_episodes = RESE[0, , drop = FALSE],
      overlap_count = 0,
      affected_persons = character(0),
      total_parl_episodes = 0
    ))
  }
  
  # Get all overlapping episodes
  overlap_episodes <- parl_episodes[
    duplicated(parl_episodes[, c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")]) |
    duplicated(parl_episodes[, c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")], fromLast = TRUE),
    , drop = FALSE]
  
  list(
    check_passed = nrow(overlap_episodes) == 0,
    overlapping_episodes = overlap_episodes,
    overlap_count = nrow(overlap_episodes),
    affected_persons = unique(overlap_episodes$pers_id),
    total_parl_episodes = nrow(parl_episodes),
    original_parl_episodes = parl_episodes
  )
}

###############################################################################
# Function: check_RESE_anynear_fulloverlap_details
# Description: Return detailed pairs of near-overlapping episodes
# Returns: List with paired data showing which episodes are nearly overlapping
###############################################################################
check_RESE_anynear_fulloverlap_details <- function(RESE, tolerance_days = 2) {
  req <- c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req, names(RESE))
  if (length(miss) > 0) {
    stop("RESE is missing columns: ", paste(miss, collapse = ", "))
  }
  
  if (nrow(RESE) < 2) {
    return(list(
      check_passed = TRUE,
      full_episode_pairs_near_overlapping = data.frame(),
      near_overlap_count = 0,
      affected_persons = character(0),
      tolerance_days = tolerance_days,
      total_rows = nrow(RESE)
    ))
  }
  
  # Create comparison pairs
  RECO <- RESE[, c("res_entry_id", "pers_id", "res_entry_start", 
                   "res_entry_start_posoxctformat", "res_entry_end", 
                   "res_entry_end_posoxctformat", "res_entry_raw"), drop = FALSE]
  
  near_pairs <- RECO %>%
    mutate(ROWID = row_number()) %>%
    inner_join(RECO %>% mutate(ROWID = row_number()), by = "pers_id", suffix = c(".x", ".y"), relationship = "many-to-many") %>%
    filter(
      ROWID.x < ROWID.y,
      abs(difftime(res_entry_start_posoxctformat.x, res_entry_start_posoxctformat.y, units = "days")) <= tolerance_days,
      abs(difftime(res_entry_end_posoxctformat.x, res_entry_end_posoxctformat.y, units = "days")) <= tolerance_days
    ) %>%
    mutate(
      start_diff_days = as.numeric(difftime(res_entry_start_posoxctformat.x, res_entry_start_posoxctformat.y, units = "days")),
      end_diff_days = as.numeric(difftime(res_entry_end_posoxctformat.x, res_entry_end_posoxctformat.y, units = "days"))
    )
  
  list(
    check_passed = nrow(near_pairs) == 0,
    full_episode_pairs_near_overlapping = near_pairs,
    near_overlap_count = nrow(near_pairs),
    affected_persons = unique(near_pairs$pers_id),
    tolerance_days = tolerance_days,
    total_rows = nrow(RESE)
  )
}

###############################################################################
# Function: parse_pcc_death_date_earliest
# Description:
#   Parse PCC-format death date strings into the earliest possible POSIXct
#   interpretation and a precision indicator. Handles three PCC date levels:
#     - Full:  "15jun1990" → 15 Jun 1990, precision = "full"
#     - Month: "jun1990"   → 01 Jun 1990, precision = "month"
#     - Year:  "1990"      → 01 Jan 1990, precision = "year"
#
# Inputs:
#   - dates_char: character vector of PCC-format death date strings
#
# Returns:
#   - List with:
#     - earliest: POSIXct vector — earliest possible date for each entry
#     - precision: character vector — "full", "month", or "year" (NA if unparseable)
###############################################################################
parse_pcc_death_date_earliest <- function(dates_char) {
  dates_char <- as.character(dates_char)
  n <- length(dates_char)
  earliest <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  precision <- rep(NA_character_, n)

  valid <- !is.na(dates_char) & dates_char != ""
  if (!any(valid)) return(list(earliest = earliest, precision = precision))

  # Try full format: DDmonYYYY (e.g., "15jun1990")
  full_parsed <- as.POSIXct(dates_char[valid], format = "%d%b%Y", tz = "UTC")
  is_full <- !is.na(full_parsed)
  idx_full <- which(valid)[is_full]
  earliest[idx_full] <- full_parsed[is_full]
  precision[idx_full] <- "full"

  # Try month+year: monYYYY (e.g., "jun1990") → 1st of month
  remaining <- valid & is.na(precision)
  if (any(remaining)) {
    month_parsed <- as.POSIXct(paste0("01", dates_char[remaining]),
                               format = "%d%b%Y", tz = "UTC")
    is_month <- !is.na(month_parsed)
    idx_month <- which(remaining)[is_month]
    earliest[idx_month] <- month_parsed[is_month]
    precision[idx_month] <- "month"
  }

  # Try year only: YYYY (e.g., "1990") → Jan 1
  remaining2 <- valid & is.na(precision)
  if (any(remaining2)) {
    year_parsed <- as.POSIXct(paste0("01jan", dates_char[remaining2]),
                              format = "%d%b%Y", tz = "UTC")
    is_year <- !is.na(year_parsed)
    idx_year <- which(remaining2)[is_year]
    earliest[idx_year] <- year_parsed[is_year]
    precision[idx_year] <- "year"
  }

  list(earliest = earliest, precision = precision)
}

###############################################################################
# Function: check_RESE_episodes_past_death
# Description:
#   Check whether any RESE episodes have an end date that is after the person's
#   death date (as recorded in POLI). This is a data integrity issue.
#
# Inputs:
#   - RESE: data.frame with pers_id and res_entry_end_posoxctformat (POSIXct)
#   - POLI: data.frame with pers_id and death_date (Date or POSIXct)
#
# Returns:
#   - TRUE  if there are any episodes where end date > death date, OR
#           where a partial death date makes the comparison ambiguous
#   - FALSE if all episodes end on or before death date (or person has no death date)
###############################################################################
check_RESE_episodes_past_death <- function(RESE, POLI) {
  # Validate required columns
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"res_entry_end_posoxctformat" %in% names(RESE)) {
    stop("RESE is missing column res_entry_end_posoxctformat")
  }
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")
  if (!"death_date" %in% names(POLI)) stop("POLI is missing column death_date")

  # Get death dates from POLI (only those with non-NA and non-empty death dates)
  # Handle both character strings (where "" means missing) and Date objects
  has_death_date <- !is.na(POLI$death_date)
  if (is.character(POLI$death_date)) {
    has_death_date <- has_death_date & POLI$death_date != ""
  }
  death_dates <- POLI[has_death_date, c("pers_id", "death_date"), drop = FALSE]

  if (nrow(death_dates) == 0) {
    return(FALSE)  # No one has died, nothing to check
  }

  # Join RESE with death dates
  merged <- merge(RESE, death_dates, by = "pers_id", all.x = FALSE)

  if (nrow(merged) == 0) {
    return(FALSE)  # No RESE entries for deceased persons
  }

  # Filter to rows where episode end date is not NA
  valid_rows <- !is.na(merged$res_entry_end_posoxctformat)
  if (sum(valid_rows) == 0) {
    return(FALSE)  # No valid end dates to check
  }

  # Convert death date to earliest possible POSIXct and get precision
  death_vals <- merged$death_date[valid_rows]
  if (inherits(death_vals, "Date") || inherits(death_vals, "POSIXt")) {
    death_posix <- as.POSIXct(death_vals, tz = "UTC")
    death_precision <- rep("full", length(death_posix))
  } else {
    parsed <- parse_pcc_death_date_earliest(as.character(death_vals))
    death_posix <- parsed$earliest
    death_precision <- parsed$precision
  }
  end_dates <- merged$res_entry_end_posoxctformat[valid_rows]

  # Full dates: episode ends strictly after death → definite violation
  is_full <- death_precision == "full" & !is.na(death_precision)
  full_violations <- is_full & (end_dates > death_posix)
  full_violations[is.na(full_violations)] <- FALSE

  # Partial dates: episode ends on or after earliest possible death → needs investigation
  is_partial <- !is_full & !is.na(death_precision)
  partial_flags <- is_partial & (end_dates >= death_posix)
  partial_flags[is.na(partial_flags)] <- FALSE

  any(full_violations) || any(partial_flags)
}

###############################################################################
# Function: check_RESE_episodes_past_death_details
# Description:
#   Return detailed information about RESE episodes that extend past the
#   person's death date. Handles partial PCC death dates (month+year, year-only)
#   by flagging episodes that end on or after the earliest possible death date
#   for human investigation.
#
# Inputs:
#   - RESE: data.frame with pers_id and res_entry_end_posoxctformat (POSIXct)
#   - POLI: data.frame with pers_id and death_date (Date, POSIXct, or PCC string)
#
# Returns:
#   - List with:
#     - check_passed: TRUE if no episodes past death AND no partial-date flags
#     - episodes_past_death: data.frame of definite violations (full death dates)
#     - past_death_count: number of definite violations
#     - episodes_partial_death_date: data.frame of episodes needing investigation
#       (partial death date makes comparison ambiguous)
#     - partial_death_date_count: number of episodes flagged for investigation
#     - affected_persons: unique pers_ids across both categories
#     - total_rese_rows: total number of RESE rows
#     - deceased_persons_in_rese: number of deceased persons found in RESE
###############################################################################
check_RESE_episodes_past_death_details <- function(RESE, POLI) {
  # Validate required columns
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"res_entry_end_posoxctformat" %in% names(RESE)) {
    stop("RESE is missing column res_entry_end_posoxctformat")
  }
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")
  if (!"death_date" %in% names(POLI)) stop("POLI is missing column death_date")

  # Early return template
  empty_return <- function(deceased = 0) {
    list(
      check_passed = TRUE,
      episodes_past_death = RESE[0, , drop = FALSE],
      past_death_count = 0,
      episodes_partial_death_date = RESE[0, , drop = FALSE],
      partial_death_date_count = 0,
      affected_persons = character(0),
      total_rese_rows = nrow(RESE),
      deceased_persons_in_rese = deceased
    )
  }

  # Get death dates from POLI (only those with non-NA and non-empty death dates)
  has_death_date <- !is.na(POLI$death_date)
  if (is.character(POLI$death_date)) {
    has_death_date <- has_death_date & POLI$death_date != ""
  }
  death_dates <- POLI[has_death_date, c("pers_id", "death_date"), drop = FALSE]

  if (nrow(death_dates) == 0) return(empty_return())

  # Join RESE with death dates
  merged <- merge(RESE, death_dates, by = "pers_id", all.x = FALSE)

  if (nrow(merged) == 0) return(empty_return())

  deceased_persons_count <- length(unique(merged$pers_id))

  # Filter to rows where episode end date is not NA
  valid_rows <- !is.na(merged$res_entry_end_posoxctformat)

  if (sum(valid_rows) == 0) return(empty_return(deceased_persons_count))

  # Parse death dates: get earliest possible date and precision level
  if (inherits(merged$death_date, "Date") || inherits(merged$death_date, "POSIXt")) {
    merged$death_posix <- as.POSIXct(merged$death_date, tz = "UTC")
    merged$death_precision <- "full"
  } else {
    parsed <- parse_pcc_death_date_earliest(as.character(merged$death_date))
    merged$death_posix <- parsed$earliest
    merged$death_precision <- parsed$precision
  }

  # Full dates: episode ends strictly after death → definite violation
  is_full <- merged$death_precision == "full" & !is.na(merged$death_precision)
  full_violation <- valid_rows & is_full &
    (merged$res_entry_end_posoxctformat > merged$death_posix)
  full_violation[is.na(full_violation)] <- FALSE

  # Partial dates: episode ends on or after earliest possible death → flag
  is_partial <- valid_rows & !is_full & !is.na(merged$death_precision)
  partial_flag <- is_partial &
    (merged$res_entry_end_posoxctformat >= merged$death_posix)
  partial_flag[is.na(partial_flag)] <- FALSE

  episodes_past_death <- merged[full_violation, , drop = FALSE]
  episodes_partial_death_date <- merged[partial_flag, , drop = FALSE]

  # Add days_past_death for definite violations
  if (nrow(episodes_past_death) > 0) {
    episodes_past_death$days_past_death <- as.numeric(difftime(
      episodes_past_death$res_entry_end_posoxctformat,
      episodes_past_death$death_posix,
      units = "days"
    ))
  }

  list(
    check_passed = !any(full_violation) && !any(partial_flag),
    episodes_past_death = episodes_past_death,
    past_death_count = sum(full_violation),
    episodes_partial_death_date = episodes_partial_death_date,
    partial_death_date_count = sum(partial_flag),
    affected_persons = unique(c(episodes_past_death$pers_id,
                                episodes_partial_death_date$pers_id)),
    total_rese_rows = nrow(RESE),
    deceased_persons_in_rese = deceased_persons_count
  )
}

