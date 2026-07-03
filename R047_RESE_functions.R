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
#   Only rows with political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09") are checked.
#
# Returns:
#   - TRUE  if one or more full duplicates are found
#   - FALSE if no full duplicates are found
#   - TRUE  (with warning) if no parliamentary membership episodes exist
###############################################################################

check_RESE_parlmemeppisodes_anyfulloverlap <- function(RESE) {

  # filter on parliamentary membership episodes only
  RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")), ]

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

###############################################################################
# Function: check_RESE_duplicate_birthdates_in_faction
# Description:
#   Detect potential POLI duplicates by finding people with the same birth_date
#   seated in the same parliamentary faction on the first day of any parliament.
#   Within a faction of ~20-40 people, a shared birthday is statistically very
#   unlikely and typically indicates the same person entered under two different
#   pers_ids (e.g., maiden vs married name, nickname vs full name).
#
#   Requires MEME data to determine faction membership. If MEME has 0 rows,
#   the check is skipped with a message and returns FALSE.
#
# Inputs:
#   - RESE: preprocessed data.frame (needs pers_id, political_function,
#           res_entry_start_posoxctformat, res_entry_end_posoxctformat)
#   - POLI: data.frame with pers_id, birth_date
#   - PARL: preprocessed data.frame (needs parliament_id,
#           leg_period_start_posoxctformat, level, assembly_abb)
#   - MEME: preprocessed data.frame (needs pers_id, party_id,
#           memep_startdate_posoxctformat, memep_enddate_posoxctformat)
#   - assembly_abb_filter: character, which assembly to check (e.g., "TK", "HR")
#   - verified_pairs: optional data.frame with columns pers_id_1 and pers_id_2,
#     listing pairs that have been manually verified as different people.
#     These pairs are excluded from the results (not flagged).
#
# Returns:
#   - TRUE  if any same-birthday pairs found within a faction
#   - FALSE if no duplicates found, or MEME data unavailable
###############################################################################
check_RESE_duplicate_birthdates_in_faction <- function(RESE, POLI, PARL, MEME,
                                                        assembly_abb_filter,
                                                        verified_pairs = NULL) {
  # Column validation
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"res_entry_start_posoxctformat" %in% names(RESE))
    stop("RESE is missing column res_entry_start_posoxctformat (run preprocess_RESEdates first)")
  if (!"res_entry_end_posoxctformat" %in% names(RESE))
    stop("RESE is missing column res_entry_end_posoxctformat")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")
  if (!"birth_date" %in% names(POLI)) stop("POLI is missing column birth_date")
  if (!"parliament_id" %in% names(PARL)) stop("PARL is missing column parliament_id")
  if (!"leg_period_start_posoxctformat" %in% names(PARL))
    stop("PARL is missing column leg_period_start_posoxctformat")

  # Check MEME availability
  if (nrow(MEME) == 0) {
    message("MEME data not available - skipping faction birthday duplicate check")
    return(FALSE)
  }
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")
  if (!"party_id" %in% names(MEME)) stop("MEME is missing column party_id")
  if (!"memep_startdate_posoxctformat" %in% names(MEME))
    stop("MEME is missing column memep_startdate_posoxctformat")

  # Filter PARL to the target assembly
  PARL <- PARL[which(PARL$level == "NT" & PARL$assembly_abb == assembly_abb_filter), ]
  if (nrow(PARL) == 0) return(FALSE)

  # Prepare date columns
  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end <- as.Date(RESE$res_entry_end_posoxctformat)
  meme_start <- as.Date(MEME$memep_startdate_posoxctformat)
  meme_end <- as.Date(MEME$memep_enddate_posoxctformat)

  # Check each parliament
  for (i in seq_len(nrow(PARL))) {
    snapshot <- as.Date(PARL$leg_period_start_posoxctformat[i])
    if (is.na(snapshot)) next

    # Who is seated?
    seated_idx <- which(rese_start <= snapshot & rese_end >= snapshot)
    seated_ids <- unique(RESE$pers_id[seated_idx])
    if (length(seated_ids) == 0) next

    # What party are they in? (active MEME episode on snapshot day)
    meme_active_idx <- which(
      MEME$pers_id %in% seated_ids &
      meme_start <= snapshot &
      (meme_end >= snapshot | is.na(meme_end))
    )
    if (length(meme_active_idx) == 0) next

    person_party <- data.frame(
      pers_id = MEME$pers_id[meme_active_idx],
      party_id = MEME$party_id[meme_active_idx],
      stringsAsFactors = FALSE
    )

    # Deduplicate: a person may have multiple MEME episodes for the same party
    person_party <- person_party[!duplicated(person_party[, c("pers_id", "party_id")]), ]

    # Add birth_date from POLI
    person_party <- merge(person_party,
                          POLI[, c("pers_id", "birth_date")],
                          by = "pers_id", all.x = TRUE)

    # Filter to rows with non-NA birth_date
    person_party <- person_party[!is.na(person_party$birth_date) &
                                  person_party$birth_date != "", ]
    if (nrow(person_party) == 0) next

    # Find different people with same birthday within same party
    dupes <- person_party[
      duplicated(person_party[, c("party_id", "birth_date")]) |
      duplicated(person_party[, c("party_id", "birth_date")], fromLast = TRUE),
    ]

    # Exclude verified pairs
    if (nrow(dupes) > 0 && !is.null(verified_pairs)) {
      dupe_groups <- split(dupes, paste(dupes$party_id, dupes$birth_date))
      any_unverified <- FALSE
      for (g in dupe_groups) {
        ids <- sort(g$pers_id)
        for (a in seq_len(length(ids) - 1)) {
          for (b in (a + 1):length(ids)) {
            pair_key <- paste(sort(c(ids[a], ids[b])), collapse = "|")
            vp_keys <- apply(verified_pairs[, c("pers_id_1", "pers_id_2")], 1,
                             function(r) paste(sort(r), collapse = "|"))
            if (!pair_key %in% vp_keys) any_unverified <- TRUE
          }
        }
      }
      if (any_unverified) return(TRUE)
    } else if (nrow(dupes) > 0) {
      return(TRUE)
    }
  }

  FALSE
}

###############################################################################
# Function: check_RESE_duplicate_birthdates_in_faction_details
###############################################################################
check_RESE_duplicate_birthdates_in_faction_details <- function(RESE, POLI, PARL, MEME,
                                                                assembly_abb_filter,
                                                                verified_pairs = NULL) {
  # Column validation (same as boolean version)
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"res_entry_start_posoxctformat" %in% names(RESE))
    stop("RESE is missing column res_entry_start_posoxctformat")
  if (!"res_entry_end_posoxctformat" %in% names(RESE))
    stop("RESE is missing column res_entry_end_posoxctformat")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")
  if (!"birth_date" %in% names(POLI)) stop("POLI is missing column birth_date")
  if (!"parliament_id" %in% names(PARL)) stop("PARL is missing column parliament_id")
  if (!"leg_period_start_posoxctformat" %in% names(PARL))
    stop("PARL is missing column leg_period_start_posoxctformat")

  empty_result <- list(
    check_passed = TRUE,
    flagged_pairs = data.frame(
      parliament_id = character(0), party_id = character(0),
      birth_date = character(0),
      pers_id_1 = character(0), name_1 = character(0),
      pers_id_2 = character(0), name_2 = character(0),
      stringsAsFactors = FALSE
    ),
    flagged_count = 0,
    parliaments_checked = 0,
    meme_available = FALSE
  )

  if (nrow(MEME) == 0) {
    message("MEME data not available - skipping faction birthday duplicate check")
    return(empty_result)
  }
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")
  if (!"party_id" %in% names(MEME)) stop("MEME is missing column party_id")
  if (!"memep_startdate_posoxctformat" %in% names(MEME))
    stop("MEME is missing column memep_startdate_posoxctformat")

  PARL <- PARL[which(PARL$level == "NT" & PARL$assembly_abb == assembly_abb_filter), ]
  if (nrow(PARL) == 0) {
    empty_result$meme_available <- TRUE
    return(empty_result)
  }

  # Build name lookup
  name_lookup <- POLI[, c("pers_id", "last_name", "first_name")]

  # Prepare date columns
  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end <- as.Date(RESE$res_entry_end_posoxctformat)
  meme_start <- as.Date(MEME$memep_startdate_posoxctformat)
  meme_end <- as.Date(MEME$memep_enddate_posoxctformat)

  all_flagged <- list()

  for (i in seq_len(nrow(PARL))) {
    pid <- PARL$parliament_id[i]
    snapshot <- as.Date(PARL$leg_period_start_posoxctformat[i])
    if (is.na(snapshot)) next

    seated_idx <- which(rese_start <= snapshot & rese_end >= snapshot)
    seated_ids <- unique(RESE$pers_id[seated_idx])
    if (length(seated_ids) == 0) next

    meme_active_idx <- which(
      MEME$pers_id %in% seated_ids &
      meme_start <= snapshot &
      (meme_end >= snapshot | is.na(meme_end))
    )
    if (length(meme_active_idx) == 0) next

    person_party <- data.frame(
      pers_id = MEME$pers_id[meme_active_idx],
      party_id = MEME$party_id[meme_active_idx],
      stringsAsFactors = FALSE
    )
    # Deduplicate: a person may have multiple MEME episodes for the same party
    person_party <- person_party[!duplicated(person_party[, c("pers_id", "party_id")]), ]
    person_party <- merge(person_party,
                          POLI[, c("pers_id", "birth_date")],
                          by = "pers_id", all.x = TRUE)
    person_party <- person_party[!is.na(person_party$birth_date) &
                                  person_party$birth_date != "", ]
    if (nrow(person_party) == 0) next

    dupes <- person_party[
      duplicated(person_party[, c("party_id", "birth_date")]) |
      duplicated(person_party[, c("party_id", "birth_date")], fromLast = TRUE),
    ]

    if (nrow(dupes) > 0) {
      # Build pairs
      dupe_groups <- split(dupes, paste(dupes$party_id, dupes$birth_date))
      for (g in dupe_groups) {
        ids <- sort(g$pers_id)
        # Generate all pairs within the group
        for (a in seq_len(length(ids) - 1)) {
          for (b in (a + 1):length(ids)) {
            n1 <- name_lookup[name_lookup$pers_id == ids[a], ]
            n2 <- name_lookup[name_lookup$pers_id == ids[b], ]
            all_flagged[[length(all_flagged) + 1]] <- data.frame(
              parliament_id = pid,
              party_id = g$party_id[1],
              birth_date = g$birth_date[1],
              pers_id_1 = ids[a],
              name_1 = if (nrow(n1) > 0) paste(n1$first_name[1], n1$last_name[1]) else NA_character_,
              pers_id_2 = ids[b],
              name_2 = if (nrow(n2) > 0) paste(n2$first_name[1], n2$last_name[1]) else NA_character_,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  flagged_pairs <- if (length(all_flagged) > 0) {
    unique(do.call(rbind, all_flagged))
  } else {
    empty_result$flagged_pairs
  }

  # Exclude verified pairs
  if (nrow(flagged_pairs) > 0 && !is.null(verified_pairs)) {
    flagged_pairs$pair_key <- apply(
      flagged_pairs[, c("pers_id_1", "pers_id_2")], 1,
      function(r) paste(sort(r), collapse = "|"))
    vp_keys <- apply(verified_pairs[, c("pers_id_1", "pers_id_2")], 1,
                     function(r) paste(sort(r), collapse = "|"))
    flagged_pairs <- flagged_pairs[!flagged_pairs$pair_key %in% vp_keys, ]
    flagged_pairs$pair_key <- NULL
  }

  list(
    check_passed = nrow(flagged_pairs) == 0,
    flagged_pairs = flagged_pairs,
    flagged_count = nrow(flagged_pairs),
    parliaments_checked = nrow(PARL),
    meme_available = TRUE
  )
}

###############################################################################
# Function: characterize_RESE_boundary
# Description:
#   Characterizes the RESE data boundary around a date: the last date with any
#   seated MP on/before it, the first date with any seated MP after it, how
#   many MPs were seated on those boundary dates, how many episodes end/start
#   exactly there, and the full RESE rows marking the boundary. Shared by the
#   three coverage checks (parlmem_coverage, coverage_at_date_from/date_to) so
#   a failure of any of them reports the same kind of diagnosis.
#
#   Naming is deliberate: "last date with any seated MP" means >= 1 MP was
#   seated on that date — NOT that the parliament was complete. Coverage can
#   erode gradually before a cliff; n_seated_on_last vs. the official
#   parliament size distinguishes a snapshot cutoff (full chamber ends on one
#   day) from erosion, and the day-by-day graph shows the full pattern.
#
# Returns: List with
#   - last_covered_date, next_covered_date  (Date, NA if none)
#   - n_seated_on_last, n_seated_on_next    MPs seated on those boundary dates
#   - n_ending_on_last, n_starting_on_next  episodes ending/starting exactly there
#   - n_ongoing                             episodes with no end date
#   - boundary_episodes                     full RESE rows at the boundary with a
#                                           leading boundary_side column
#                                           (last_end_before_date /
#                                            first_start_after_date)
###############################################################################
characterize_RESE_boundary <- function(RESE, date) {
  req_cols <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req_cols, names(RESE))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

  date       <- as.Date(date)
  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end   <- as.Date(RESE$res_entry_end_posoxctformat)

  n_seated_on <- function(d) {
    sum(rese_start <= d & (is.na(rese_end) | rese_end >= d), na.rm = TRUE)
  }

  # Last date with any seated MP on/before the checked date: the latest end
  # date among episodes that started on/before it. (If any such episode is
  # ongoing, coverage does not stop before the date and the "gap before"
  # concept does not apply.)
  ends_before <- rese_end[!is.na(rese_end) & rese_start <= date & rese_end <= date]
  last_covered_date <- if (length(ends_before) > 0) max(ends_before) else as.Date(NA)

  # First date with any seated MP after the checked date: the earliest start
  # date among episodes starting after it (relevant for a gap at the START of
  # a range).
  starts_after <- rese_start[!is.na(rese_start) & rese_start > date]
  next_covered_date <- if (length(starts_after) > 0) min(starts_after) else as.Date(NA)

  n_ending_on_last   <- if (!is.na(last_covered_date)) {
    sum(rese_end == last_covered_date, na.rm = TRUE)
  } else 0L
  n_starting_on_next <- if (!is.na(next_covered_date)) {
    sum(rese_start == next_covered_date, na.rm = TRUE)
  } else 0L

  boundary_last <- if (!is.na(last_covered_date)) {
    RESE[!is.na(rese_end) & rese_end == last_covered_date, , drop = FALSE]
  } else RESE[0, , drop = FALSE]
  boundary_next <- if (!is.na(next_covered_date)) {
    RESE[!is.na(rese_start) & rese_start == next_covered_date, , drop = FALSE]
  } else RESE[0, , drop = FALSE]

  boundary_episodes <- rbind(
    cbind(boundary_side = rep("last_end_before_date",  nrow(boundary_last)),
          boundary_last,  stringsAsFactors = FALSE),
    cbind(boundary_side = rep("first_start_after_date", nrow(boundary_next)),
          boundary_next, stringsAsFactors = FALSE)
  )

  list(
    last_covered_date  = last_covered_date,
    next_covered_date  = next_covered_date,
    n_seated_on_last   = if (is.na(last_covered_date)) NA_integer_
                         else as.integer(n_seated_on(last_covered_date)),
    n_seated_on_next   = if (is.na(next_covered_date)) NA_integer_
                         else as.integer(n_seated_on(next_covered_date)),
    n_ending_on_last   = as.integer(n_ending_on_last),
    n_starting_on_next = as.integer(n_starting_on_next),
    n_ongoing          = as.integer(sum(is.na(rese_end) & !is.na(rese_start))),
    boundary_episodes  = boundary_episodes
  )
}

###############################################################################
# Function: boundary_summary_stats
# Description:
#   Renders a characterize_RESE_boundary() result as the named character
#   vector of key facts shared by all three coverage checks (the dashboard
#   shows it as a "Key facts" block and it feeds the LLM issue description).
#   Labels avoid implying completeness: "last date with any seated MP" is
#   paired with the number of MPs actually seated on that date, so a snapshot
#   cutoff (n close to parliament size) and gradual erosion (small n) can be
#   told apart.
#
# Requires: format_pcc_date() from R047_functions.R (sourced by callers).
###############################################################################
boundary_summary_stats <- function(bnd, date) {
  date <- as.Date(date)
  fmt_or_na <- function(d) if (is.na(d)) "none" else format_pcc_date(d)
  cnt_or_na <- function(n) if (is.na(n)) "NA" else as.character(n)

  c(
    "last date with any seated MP before"   = fmt_or_na(bnd$last_covered_date),
    "MPs seated on that last date"          = cnt_or_na(bnd$n_seated_on_last),
    "episodes ending on that last date"     = as.character(bnd$n_ending_on_last),
    "zero seated MPs from"                  = if (is.na(bnd$last_covered_date)) "none"
                                              else format_pcc_date(bnd$last_covered_date + 1),
    "gap length before date (days)"         = if (is.na(bnd$last_covered_date)) "NA"
                                              else as.character(as.integer(date - bnd$last_covered_date)),
    "first date with any seated MP after"   = fmt_or_na(bnd$next_covered_date),
    "MPs seated on that first date"         = cnt_or_na(bnd$n_seated_on_next),
    "episodes starting on that first date"  = as.character(bnd$n_starting_on_next),
    "gap length after date (days)"          = if (is.na(bnd$next_covered_date)) "NA"
                                              else as.character(as.integer(bnd$next_covered_date - date)),
    "ongoing episodes (no end date)"        = as.character(bnd$n_ongoing)
  )
}

###############################################################################
# Function: check_RESE_parlmem_coverage
# Description:
#   For every parliament whose start date falls within [date_from, date_to],
#   check that at least one RESE parliamentary membership entry is active on
#   that day (i.e. n_seated > 0). Returns FALSE if any parliament in range
#   has zero seated MPs in RESE — a "data cliff" equivalent to the orange line
#   in the POLI completeness graph.
#
# Inputs:
#   - RESE: preprocessed data.frame with
#       res_entry_start_posoxctformat (POSIXct)
#       res_entry_end_posoxctformat   (POSIXct)
#   - PARL: preprocessed data.frame with
#       leg_period_start_posoxctformat (POSIXct), level, assembly_abb
#   - assembly_abb_filter: character — which assembly to check (e.g. "TK")
#   - date_from, date_to: Date — range of parliament start dates to include
#
# Returns:
#   - TRUE  if every parliament start date in range has >= 1 seated MP
#   - TRUE  if no parliaments fall within the date range (vacuous)
#   - FALSE if any parliament start date in range has 0 seated MPs
###############################################################################
check_RESE_parlmem_coverage <- function(RESE, PARL, assembly_abb_filter,
                                        date_from, date_to) {
  req_rese <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  req_parl <- c("leg_period_start_posoxctformat", "level", "assembly_abb")
  miss <- c(setdiff(req_rese, names(RESE)), setdiff(req_parl, names(PARL)))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

  parl_sub <- PARL[PARL$level == "NT" & PARL$assembly_abb == assembly_abb_filter, ]
  snapshots <- as.Date(parl_sub$leg_period_start_posoxctformat)
  in_range  <- !is.na(snapshots) & snapshots >= as.Date(date_from) & snapshots <= as.Date(date_to)
  parl_sub  <- parl_sub[in_range, ]

  if (nrow(parl_sub) == 0) return(TRUE)

  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end   <- as.Date(RESE$res_entry_end_posoxctformat)

  for (i in seq_len(nrow(parl_sub))) {
    snapshot <- as.Date(parl_sub$leg_period_start_posoxctformat[i])
    n_seated <- sum(rese_start <= snapshot & (is.na(rese_end) | rese_end >= snapshot),
                    na.rm = TRUE)
    if (n_seated == 0) return(FALSE)
  }
  TRUE
}

###############################################################################
# Function: check_RESE_parlmem_coverage_details
# Description:
#   Detailed version of check_RESE_parlmem_coverage. Returns which parliament
#   start dates within the date range had zero seated MPs in RESE, plus the
#   same data-boundary diagnosis the coverage_at_date checks report: the RESE
#   boundary is characterized around the FIRST failing parliament start date
#   (if several parliaments fall in one gap this describes that gap; separate
#   gaps are all listed in parliaments_no_data but only the first is
#   characterized).
#
# Returns: List with
#   - check_passed           (TRUE/FALSE)
#   - parliaments_checked    number of parliament start dates in range
#   - gap_count              number with n_seated == 0
#   - parliaments_no_data    data.frame of PARL rows with gap + n_seated column
#   - summary_stats          named character vector of key facts (NULL on
#                            pass); rendered as a "Key facts" block by the
#                            dashboard and in the GitHub-issue technical
#                            details
#   - boundary_episodes      full RESE rows marking the data boundary around
#                            the first failing parliament start date, with a
#                            leading boundary_side column
#
# Requires: format_pcc_date() from R047_functions.R (sourced by callers).
###############################################################################
check_RESE_parlmem_coverage_details <- function(RESE, PARL, assembly_abb_filter,
                                                 date_from, date_to) {
  req_rese <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  req_parl <- c("leg_period_start_posoxctformat", "level", "assembly_abb")
  miss <- c(setdiff(req_rese, names(RESE)), setdiff(req_parl, names(PARL)))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

  parl_sub <- PARL[PARL$level == "NT" & PARL$assembly_abb == assembly_abb_filter, ]
  snapshots <- as.Date(parl_sub$leg_period_start_posoxctformat)
  in_range  <- !is.na(snapshots) & snapshots >= as.Date(date_from) & snapshots <= as.Date(date_to)
  parl_sub  <- parl_sub[in_range, , drop = FALSE]

  empty_boundary <- cbind(boundary_side = character(0),
                          RESE[0, , drop = FALSE], stringsAsFactors = FALSE)
  empty <- list(
    check_passed        = TRUE,
    parliaments_checked = 0L,
    gap_count           = 0L,
    parliaments_no_data = parl_sub[0, , drop = FALSE],
    summary_stats       = NULL,
    boundary_episodes   = empty_boundary
  )
  if (nrow(parl_sub) == 0) return(empty)

  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end   <- as.Date(RESE$res_entry_end_posoxctformat)

  n_seated_vec <- vapply(seq_len(nrow(parl_sub)), function(i) {
    snapshot <- as.Date(parl_sub$leg_period_start_posoxctformat[i])
    sum(rese_start <= snapshot & (is.na(rese_end) | rese_end >= snapshot), na.rm = TRUE)
  }, integer(1))

  gap_rows <- parl_sub[n_seated_vec == 0, , drop = FALSE]
  gap_rows$n_seated <- rep(0L, nrow(gap_rows))

  summary_stats     <- NULL
  boundary_episodes <- empty_boundary
  if (nrow(gap_rows) > 0) {
    gap_dates      <- as.Date(gap_rows$leg_period_start_posoxctformat)
    first_gap_date <- min(gap_dates)
    bnd            <- characterize_RESE_boundary(RESE, first_gap_date)
    summary_stats <- c(
      "parliament start dates checked"         = as.character(nrow(parl_sub)),
      "parliament starts with zero seated MPs" = as.character(nrow(gap_rows)),
      "first parliament start with no data"    = format_pcc_date(first_gap_date),
      "last parliament start with no data"     = format_pcc_date(max(gap_dates)),
      "data boundary diagnosed around"         = format_pcc_date(first_gap_date),
      boundary_summary_stats(bnd, first_gap_date)
    )
    boundary_episodes <- bnd$boundary_episodes
  }

  list(
    check_passed        = nrow(gap_rows) == 0,
    parliaments_checked = nrow(parl_sub),
    gap_count           = nrow(gap_rows),
    parliaments_no_data = gap_rows,
    summary_stats       = summary_stats,
    boundary_episodes   = boundary_episodes
  )
}

###############################################################################
# Function: check_RESE_coverage_at_date
# Description:
#   Checks whether at least one RESE parliamentary membership entry is active
#   on a specific date (n_seated > 0). Intended to catch data gaps at the
#   boundaries of a date range — e.g. a parliament that started before
#   date_from should still have seated MPs in RESE on date_from itself.
#
# Inputs:
#   - RESE: preprocessed data.frame with
#       res_entry_start_posoxctformat (POSIXct)
#       res_entry_end_posoxctformat   (POSIXct)
#   - date: Date — the specific date to check
#
# Returns:
#   - TRUE  if at least one RESE entry is active on that date
#   - FALSE if n_seated == 0
###############################################################################
check_RESE_coverage_at_date <- function(RESE, date) {
  req_cols <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req_cols, names(RESE))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

  date       <- as.Date(date)
  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end   <- as.Date(RESE$res_entry_end_posoxctformat)

  n_seated <- sum(rese_start <= date & (is.na(rese_end) | rese_end >= date), na.rm = TRUE)
  n_seated > 0
}

###############################################################################
# Function: check_RESE_coverage_at_date_details
# Description:
#   Detailed version of check_RESE_coverage_at_date. Besides the pass/fail
#   verdict it characterizes the data boundary around the checked date (via
#   characterize_RESE_boundary, shared with check_RESE_parlmem_coverage), so a
#   failure report can say WHERE the data stops rather than just that it does:
#   the last date with any seated MP before the checked date (and how many
#   were seated then — NOT necessarily a complete chamber), the first date
#   with zero seated MPs, the gap lengths, and how many episodes end/start
#   exactly at those boundaries (a full chamber ending on one day is the
#   signature of a data snapshot cutoff, not of an empty parliament).
#
# Returns: List with
#   - check_passed      (TRUE/FALSE)
#   - n_seated          number of RESE entries active on that date
#   - snapshot_row      1-row data.frame with columns date_checked and n_seated
#   - summary_stats     named character vector of key facts (PCC-format dates);
#                       rendered as a "Key facts" block by the dashboard and in
#                       the GitHub-issue technical details
#   - boundary_episodes full RESE rows marking the data boundary, with a
#                       leading boundary_side column: episodes whose end date
#                       is the last date with any seated MP before the checked
#                       date ("last_end_before_date") and episodes whose start
#                       date is the first date with any seated MP after it
#                       ("first_start_after_date")
#
# Requires: format_pcc_date() from R047_functions.R (sourced by callers).
###############################################################################
check_RESE_coverage_at_date_details <- function(RESE, date) {
  req_cols <- c("res_entry_start_posoxctformat", "res_entry_end_posoxctformat")
  miss <- setdiff(req_cols, names(RESE))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

  date       <- as.Date(date)
  rese_start <- as.Date(RESE$res_entry_start_posoxctformat)
  rese_end   <- as.Date(RESE$res_entry_end_posoxctformat)

  n_seated <- sum(rese_start <= date & (is.na(rese_end) | rese_end >= date), na.rm = TRUE)

  bnd <- characterize_RESE_boundary(RESE, date)

  summary_stats <- c(
    "date checked"            = format_pcc_date(date),
    "MPs seated on that date" = as.character(n_seated),
    boundary_summary_stats(bnd, date)
  )

  list(
    check_passed      = n_seated > 0,
    n_seated          = as.integer(n_seated),
    snapshot_row      = data.frame(date_checked = date, n_seated = as.integer(n_seated)),
    summary_stats     = summary_stats,
    boundary_episodes = bnd$boundary_episodes
  )
}
