###############################################################################
# Function: preprocess_PARLdates
# Description:
#   Clean [[lcen]]/[[rcen]] tags on leg_period_* and parse to POSIXct.
# Input:
#   - PARLLOC: data.frame with leg_period_start / leg_period_end (character)
# Output:
#   - PARLLOC with leg_period_start_posoxctformat / leg_period_end_posoxctformat
###############################################################################
preprocess_PARLdates <- function(PARLLOC) {

  # remember locale, switch to safe one for %b month parsing, restore on exit
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  suppressWarnings({
    ok <- Sys.setlocale("LC_TIME", "C")
    if (is.na(ok)) Sys.setlocale("LC_TIME", "English")
  })

  # do the standard cleaning by getting rid of left/right censor tags
  PARLLOC$leg_period_start <- gsub("[[rcen]]","",PARLLOC$leg_period_start,fixed=TRUE)
  PARLLOC$leg_period_start <- gsub("[[lcen]]","",PARLLOC$leg_period_start,fixed=TRUE)
  PARLLOC$leg_period_end   <- gsub("[[rcen]]","",PARLLOC$leg_period_end,fixed=TRUE)
  PARLLOC$leg_period_end   <- gsub("[[lcen]]","",PARLLOC$leg_period_end,fixed=TRUE)

  # transform to R date
  PARLLOC$leg_period_start_posoxctformat <-
    as.POSIXct(as.character(PARLLOC$leg_period_start), format="%d%b%Y", tz="UTC")
  PARLLOC$leg_period_end_posoxctformat   <-
    as.POSIXct(as.character(PARLLOC$leg_period_end),   format="%d%b%Y", tz="UTC")

  # quick warning if any parse failed
  any_start_na <- sum(is.na(PARLLOC$leg_period_start_posoxctformat)) > 0
  any_end_na   <- sum(is.na(PARLLOC$leg_period_end_posoxctformat))   > 0
  if (any_start_na || any_end_na) {
    message(
      "WARNING: not all PARL dates parsed. ",
      "Missing start: ", sum(is.na(PARLLOC$leg_period_start_posoxctformat)),
      " | Missing end: ", sum(is.na(PARLLOC$leg_period_end_posoxctformat))
    )
  }

  PARLLOC
}

###############################################################################
# Function: check_anyNAinPARLdates
# Returns TRUE if there are any NAs in either parsed PARL date column.
###############################################################################
check_anyNAinPARLdates <- function(PARLLOC) {
  any_start_na <- sum(is.na(PARLLOC$leg_period_start_posoxctformat)) > 0
  any_end_na   <- sum(is.na(PARLLOC$leg_period_end_posoxctformat))   > 0
  any_start_na || any_end_na
}

###############################################################################
# DETAILS FUNCTIONS - Return detailed data objects for inspection
###############################################################################

###############################################################################
# Function: check_anyNAinPARLdates_details
# Description: Return rows and indices with NA dates after preprocessing
# Returns: List with NA row indices and the actual rows with problems
###############################################################################
check_anyNAinPARLdates_details <- function(PARLLOC) {
  req <- c("leg_period_start_posoxctformat", "leg_period_end_posoxctformat")
  miss <- setdiff(req, names(PARLLOC))
  if (length(miss) > 0) {
    stop("PARLLOC is missing columns: ", paste(miss, collapse = ", "))
  }
  
  na_start <- is.na(PARLLOC$leg_period_start_posoxctformat)
  na_end <- is.na(PARLLOC$leg_period_end_posoxctformat)
  na_either <- na_start | na_end
  
  list(
    check_passed = !any(na_start) && !any(na_end),
    na_start_count = sum(na_start),
    na_end_count = sum(na_end),
    na_start_rows = which(na_start),
    na_end_rows = which(na_end),
    na_either_rows = which(na_either),
    rows_with_na_dates = PARLLOC[na_either, ],
    total_rows = nrow(PARLLOC)
  )
}