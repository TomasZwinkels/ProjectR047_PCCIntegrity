###############################################################################
# R047_MEME_functions.R
# Data integrity check functions for the MEME (party membership episodes) table.
# Follows the same patterns as R047_RESE_functions.R and R047_PARL_functions.R.
###############################################################################

###############################################################################
# Function: preprocess_MEMEdates
# Description:
#   Turn MEME dates (PCC format like "01Jan2020", possibly with [[lcen]]/[[rcen]])
#   into POSIXct columns *_posoxctformat. Warn if any start dates fail to parse.
#   NA end dates are legitimate (ongoing party membership).
# Input:
#   - MEMELOC: data.frame with memep_startdate / memep_enddate (character)
# Output:
#   - MEMELOC with memep_startdate_posoxctformat / memep_enddate_posoxctformat
###############################################################################
preprocess_MEMEdates <- function(MEMELOC) {

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

  MEMELOC$memep_startdate <- strip_tags(MEMELOC$memep_startdate)
  MEMELOC$memep_enddate   <- strip_tags(MEMELOC$memep_enddate)

  # parse to POSIXct (PCC uses like 01Jan2020)
  MEMELOC$memep_startdate_posoxctformat <-
    as.POSIXct(as.character(MEMELOC$memep_startdate), format = "%d%b%Y", tz = "UTC")
  MEMELOC$memep_enddate_posoxctformat   <-
    as.POSIXct(as.character(MEMELOC$memep_enddate),   format = "%d%b%Y", tz = "UTC")

  # quick warning if any start dates failed to parse
  anystartdatesmissing <- sum(is.na(MEMELOC$memep_startdate_posoxctformat)) > 0
  anyenddatesmissing   <- sum(is.na(MEMELOC$memep_enddate_posoxctformat))   > 0

  if (anystartdatesmissing || anyenddatesmissing) {
    message(
      "WARNING: not all MEME dates could be converted successfully. ",
      "Missing start: ", sum(is.na(MEMELOC$memep_startdate_posoxctformat)),
      " | Missing end: ", sum(is.na(MEMELOC$memep_enddate_posoxctformat)),
      " (note: NA end dates may be legitimate for ongoing memberships)"
    )
  }

  MEMELOC
}

###############################################################################
# Function: check_MEME_persid_in_POLI
# Description:
#   Check whether all pers_id values in MEME also occur in POLI.
# Returns:
#   - TRUE  if all MEME$pers_id are present in POLI$pers_id
#   - FALSE if one or more are missing
###############################################################################
check_MEME_persid_in_POLI <- function(MEME, POLI) {
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")

  missing_ids <- setdiff(unique(MEME$pers_id), unique(POLI$pers_id))
  length(missing_ids) == 0
}

###############################################################################
# Function: check_MEME_partyid_in_PART
# Description:
#   Check whether all party_id values in MEME also occur in PART.
# Returns:
#   - TRUE  if all MEME$party_id are present in PART$party_id
#   - FALSE if one or more are missing
###############################################################################
check_MEME_partyid_in_PART <- function(MEME, PART) {
  if (!"party_id" %in% names(MEME)) stop("MEME is missing column party_id")
  if (!"party_id" %in% names(PART)) stop("PART is missing column party_id")

  missing_ids <- setdiff(unique(MEME$party_id), unique(PART$party_id))
  length(missing_ids) == 0
}

###############################################################################
# Function: check_MEME_memepid_unique
# Description:
#   Check whether all memep_id values in MEME are unique.
# Returns:
#   - TRUE  if no duplicates (including NA duplicates) are present
#   - FALSE if one or more duplicates exist
###############################################################################
check_MEME_memepid_unique <- function(MEME) {
  if (!"memep_id" %in% names(MEME)) {
    stop("MEME is missing column memep_id")
  }
  !any(duplicated(MEME$memep_id))
}

###############################################################################
# Function: check_anyNAinMEMEdates
# Description:
#   Returns TRUE if there are any NAs in the parsed MEME start date column.
#   NA end dates are NOT flagged — they represent ongoing memberships.
# Returns:
#   - TRUE  if there are any NA values in memep_startdate_posoxctformat
#   - FALSE if all start dates are non-missing
###############################################################################
check_anyNAinMEMEdates <- function(MEMELOC) {
  if (!"memep_startdate_posoxctformat" %in% names(MEMELOC)) {
    stop("MEMELOC is missing column memep_startdate_posoxctformat")
  }
  sum(is.na(MEMELOC$memep_startdate_posoxctformat)) > 0
}

###############################################################################
# Function: check_MEME_inverted_dates
# Description:
#   Check whether any MEME episodes have an end date before the start date.
#   Rows where either date is NA are skipped.
# Returns:
#   - TRUE  if there are any episodes where end date < start date
#   - FALSE if all episodes have valid date order (start <= end)
###############################################################################
check_MEME_inverted_dates <- function(MEMELOC) {
  req <- c("memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  miss <- setdiff(req, names(MEMELOC))
  if (length(miss) > 0) {
    stop("MEMELOC is missing columns: ", paste(miss, collapse = ", "))
  }

  start_dates <- MEMELOC$memep_startdate_posoxctformat
  end_dates <- MEMELOC$memep_enddate_posoxctformat

  valid_rows <- !is.na(start_dates) & !is.na(end_dates)

  if (sum(valid_rows) == 0) {
    return(FALSE)
  }

  any(end_dates[valid_rows] < start_dates[valid_rows])
}

###############################################################################
# Function: check_MEME_anyfulloverlap
# Description:
#   Check whether there are any exact duplicate party membership episodes.
#   Duplicates are identified by (pers_id, party_id, start date, end date).
#   Same person in different parties with overlapping dates is legitimate.
# Returns:
#   - TRUE  if one or more exact duplicates are found
#   - FALSE if no duplicates are found
###############################################################################
check_MEME_anyfulloverlap <- function(MEME) {
  req <- c("pers_id", "party_id", "memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  miss <- setdiff(req, names(MEME))
  if (length(miss) > 0) {
    stop("MEME is missing columns: ", paste(miss, collapse = ", "))
  }

  if (nrow(MEME) == 0) return(FALSE)

  key_cols <- c("pers_id", "party_id", "memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  FDUBS <- MEME[
    duplicated(MEME[, key_cols]) |
    duplicated(MEME[, key_cols], fromLast = TRUE),
  ]

  nrow(FDUBS) > 0
}

###############################################################################
# Function: check_MEME_parlmembers_have_party
# Description:
#   Check whether every person who appears as a parliamentary member in RESE
#   has at least one party membership episode in MEME.
# Inputs:
#   - RESE: data.frame with pers_id and political_function
#   - MEME: data.frame with pers_id
# Returns:
#   - TRUE  if all parliamentary members have at least one MEME record
#   - FALSE if one or more parliamentary members have no MEME record
###############################################################################
check_MEME_parlmembers_have_party <- function(RESE, MEME) {
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"political_function" %in% names(RESE)) stop("RESE is missing column political_function")
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")

  parl_persons <- unique(RESE$pers_id[
    RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")
  ])

  if (length(parl_persons) == 0) return(TRUE)

  missing <- setdiff(parl_persons, unique(MEME$pers_id))
  length(missing) == 0
}


###############################################################################
# DETAILS FUNCTIONS - Return detailed data objects for inspection
###############################################################################

###############################################################################
# Function: check_MEME_persid_in_POLI_details
###############################################################################
check_MEME_persid_in_POLI_details <- function(MEME, POLI) {
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")

  missing_ids <- setdiff(unique(MEME$pers_id), unique(POLI$pers_id))

  missing_rows <- if (length(missing_ids) > 0) {
    MEME[MEME$pers_id %in% missing_ids, , drop = FALSE]
  } else {
    MEME[0, , drop = FALSE]
  }

  list(
    check_passed = length(missing_ids) == 0,
    missing_ids = missing_ids,
    missing_count = length(missing_ids),
    missing_rows = missing_rows,
    total_unique_meme_ids = length(unique(MEME$pers_id)),
    total_unique_poli_ids = length(unique(POLI$pers_id))
  )
}

###############################################################################
# Function: check_MEME_partyid_in_PART_details
###############################################################################
check_MEME_partyid_in_PART_details <- function(MEME, PART) {
  if (!"party_id" %in% names(MEME)) stop("MEME is missing column party_id")
  if (!"party_id" %in% names(PART)) stop("PART is missing column party_id")

  missing_ids <- setdiff(unique(MEME$party_id), unique(PART$party_id))

  missing_rows <- if (length(missing_ids) > 0) {
    MEME[MEME$party_id %in% missing_ids, , drop = FALSE]
  } else {
    MEME[0, , drop = FALSE]
  }

  list(
    check_passed = length(missing_ids) == 0,
    missing_ids = missing_ids,
    missing_count = length(missing_ids),
    missing_rows = missing_rows,
    total_unique_meme_party_ids = length(unique(MEME$party_id)),
    total_unique_part_party_ids = length(unique(PART$party_id))
  )
}

###############################################################################
# Function: check_MEME_memepid_unique_details
###############################################################################
check_MEME_memepid_unique_details <- function(MEME) {
  if (!"memep_id" %in% names(MEME)) {
    stop("MEME is missing column memep_id")
  }

  duplicated_logical <- duplicated(MEME$memep_id)
  duplicate_ids <- if (any(duplicated_logical)) {
    unique(MEME$memep_id[duplicated_logical])
  } else {
    character(0)
  }

  duplicate_rows <- if (length(duplicate_ids) > 0) {
    MEME[MEME$memep_id %in% duplicate_ids, , drop = FALSE]
  } else {
    MEME[0, , drop = FALSE]
  }

  list(
    check_passed = !any(duplicated_logical),
    duplicate_ids = duplicate_ids,
    duplicate_count = length(duplicate_ids),
    duplicate_rows = duplicate_rows,
    total_rows = nrow(MEME)
  )
}

###############################################################################
# Function: check_anyNAinMEMEdates_details
# Note: Only NA start dates are treated as failures. NA end dates are
# informational only (ongoing memberships).
###############################################################################
check_anyNAinMEMEdates_details <- function(MEMELOC) {
  req <- c("memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  miss <- setdiff(req, names(MEMELOC))
  if (length(miss) > 0) {
    stop("MEMELOC is missing columns: ", paste(miss, collapse = ", "))
  }

  na_start <- is.na(MEMELOC$memep_startdate_posoxctformat)
  na_end <- is.na(MEMELOC$memep_enddate_posoxctformat)

  list(
    check_passed = !any(na_start),
    na_start_count = sum(na_start),
    na_end_count = sum(na_end),
    na_start_rows = which(na_start),
    full_rows_with_na_startdates = MEMELOC[na_start, , drop = FALSE],
    total_rows = nrow(MEMELOC)
  )
}

###############################################################################
# Function: check_MEME_inverted_dates_details
###############################################################################
check_MEME_inverted_dates_details <- function(MEMELOC) {
  req <- c("memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  miss <- setdiff(req, names(MEMELOC))
  if (length(miss) > 0) {
    stop("MEMELOC is missing columns: ", paste(miss, collapse = ", "))
  }

  start_dates <- MEMELOC$memep_startdate_posoxctformat
  end_dates <- MEMELOC$memep_enddate_posoxctformat

  valid_rows <- !is.na(start_dates) & !is.na(end_dates)
  inverted <- valid_rows & (end_dates < start_dates)

  inverted_rows <- MEMELOC[inverted, , drop = FALSE]
  if (nrow(inverted_rows) > 0) {
    inverted_rows$date_diff_days <- as.numeric(difftime(
      inverted_rows$memep_enddate_posoxctformat,
      inverted_rows$memep_startdate_posoxctformat,
      units = "days"
    ))
  }

  list(
    check_passed = !any(inverted),
    inverted_count = sum(inverted),
    inverted_row_indices = which(inverted),
    inverted_rows = inverted_rows,
    total_rows = nrow(MEMELOC),
    valid_date_pairs = sum(valid_rows)
  )
}

###############################################################################
# Function: check_MEME_anyfulloverlap_details
###############################################################################
check_MEME_anyfulloverlap_details <- function(MEME) {
  req <- c("pers_id", "party_id", "memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  miss <- setdiff(req, names(MEME))
  if (length(miss) > 0) {
    stop("MEME is missing columns: ", paste(miss, collapse = ", "))
  }

  if (nrow(MEME) == 0) {
    return(list(
      check_passed = TRUE,
      overlapping_episodes = MEME[0, , drop = FALSE],
      overlap_count = 0,
      affected_persons = character(0),
      total_episodes = 0
    ))
  }

  key_cols <- c("pers_id", "party_id", "memep_startdate_posoxctformat", "memep_enddate_posoxctformat")
  overlap_episodes <- MEME[
    duplicated(MEME[, key_cols]) |
    duplicated(MEME[, key_cols], fromLast = TRUE),
    , drop = FALSE]

  list(
    check_passed = nrow(overlap_episodes) == 0,
    overlapping_episodes = overlap_episodes,
    overlap_count = nrow(overlap_episodes),
    affected_persons = unique(overlap_episodes$pers_id),
    total_episodes = nrow(MEME)
  )
}

###############################################################################
# Function: check_MEME_parlmembers_have_party_details
###############################################################################
check_MEME_parlmembers_have_party_details <- function(RESE, MEME) {
  if (!"pers_id" %in% names(RESE)) stop("RESE is missing column pers_id")
  if (!"political_function" %in% names(RESE)) stop("RESE is missing column political_function")
  if (!"pers_id" %in% names(MEME)) stop("MEME is missing column pers_id")

  parl_rows <- RESE[RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09"), , drop = FALSE]
  parl_persons <- unique(parl_rows$pers_id)

  if (length(parl_persons) == 0) {
    return(list(
      check_passed = TRUE,
      missing_ids = character(0),
      missing_count = 0,
      missing_rese_rows = RESE[0, , drop = FALSE],
      total_parlmembers = 0,
      total_meme_persons = length(unique(MEME$pers_id))
    ))
  }

  missing <- setdiff(parl_persons, unique(MEME$pers_id))

  missing_rese_rows <- if (length(missing) > 0) {
    parl_rows[parl_rows$pers_id %in% missing, , drop = FALSE]
  } else {
    parl_rows[0, , drop = FALSE]
  }

  list(
    check_passed = length(missing) == 0,
    missing_ids = missing,
    missing_count = length(missing),
    missing_rese_rows = missing_rese_rows,
    total_parlmembers = length(parl_persons),
    total_meme_persons = length(unique(MEME$pers_id))
  )
}
