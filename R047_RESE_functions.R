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
    RESE[RESE$pers_id %in% missing_ids, ]
  } else {
    RESE[0, ]
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
    RESE[RESE$res_entry_id %in% duplicate_ids, ]
  } else {
    RESE[0, ]
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
    rows_with_na_dates = RESELOC[na_either, ],
    total_rows = nrow(RESELOC)
  )
}

###############################################################################
# Function: check_RESE_parlmemeppisodes_anyfulloverlap_details
# Description: Return all overlapping parliamentary episodes as data.frame
# Returns: List with overlapping episodes data and affected persons
###############################################################################
check_RESE_parlmemeppisodes_anyfulloverlap_details <- function(RESE) {
  parl_episodes <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")), ]  
  
  if (nrow(parl_episodes) == 0) {
    return(list(
      check_passed = FALSE,
      warning_message = "No parliamentary membership episodes found",
      overlapping_episodes = RESE[0, ],
      overlap_count = 0,
      affected_persons = character(0),
      total_parl_episodes = 0
    ))
  }
  
  # Get all overlapping episodes
  overlap_episodes <- parl_episodes[
    duplicated(parl_episodes[, c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")]) |
    duplicated(parl_episodes[, c("pers_id", "res_entry_start_posoxctformat", "res_entry_end_posoxctformat")], fromLast = TRUE),
  ]
  
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
      near_overlapping_pairs = data.frame(),
      near_overlap_count = 0,
      affected_persons = character(0),
      tolerance_days = tolerance_days,
      total_rows = nrow(RESE)
    ))
  }
  
  # Create comparison pairs
  RECO <- RESE[, c("res_entry_id", "pers_id", "res_entry_start", 
                   "res_entry_start_posoxctformat", "res_entry_end", 
                   "res_entry_end_posoxctformat", "res_entry_raw")]
  
  near_pairs <- RECO %>%
    mutate(ROWID = row_number()) %>%
    inner_join(RECO %>% mutate(ROWID = row_number()), by = "pers_id", suffix = c(".x", ".y")) %>%
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
    near_overlapping_pairs = near_pairs,
    near_overlap_count = nrow(near_pairs),
    affected_persons = unique(near_pairs$pers_id),
    tolerance_days = tolerance_days,
    total_rows = nrow(RESE)
  )
}


