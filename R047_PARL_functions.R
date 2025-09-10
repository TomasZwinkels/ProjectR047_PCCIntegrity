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
# Optional level parameter to filter by parliament level (e.g., "NT" for national)
###############################################################################
check_anyNAinPARLdates <- function(PARLLOC, level = NULL) {
  if (!is.null(level)) {
    if (!"level" %in% names(PARLLOC)) {
      stop("PARLLOC is missing 'level' column needed for filtering")
    }
    PARLLOC <- PARLLOC[PARLLOC$level == level, , drop = FALSE]
  }
  
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
# Optional level parameter to filter by parliament level (e.g., "NT" for national)
###############################################################################
check_anyNAinPARLdates_details <- function(PARLLOC, level = NULL) {
  req <- c("leg_period_start_posoxctformat", "leg_period_end_posoxctformat")
  miss <- setdiff(req, names(PARLLOC))
  if (length(miss) > 0) {
    stop("PARLLOC is missing columns: ", paste(miss, collapse = ", "))
  }
  
  if (!is.null(level)) {
    if (!"level" %in% names(PARLLOC)) {
      stop("PARLLOC is missing 'level' column needed for filtering")
    }
    PARLLOC <- PARLLOC[PARLLOC$level == level, , drop = FALSE]
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
    full_rows_with_na_dates = PARLLOC[na_either, , drop = FALSE],
    total_rows = nrow(PARLLOC)
  )
}

###############################################################################
# Function: check_PARL_parliament_size_meaningful
# Description: Check if all parliament_size values are meaningful integers (> 0)
# Returns: TRUE if all parliament_size values are meaningful, FALSE otherwise
# Optional level parameter to filter by parliament level (e.g., "NT" for national)
###############################################################################
check_PARL_parliament_size_meaningful <- function(PARLLOC, level = NULL) {
  if (!"parliament_size" %in% names(PARLLOC)) {
    stop("PARLLOC is missing 'parliament_size' column")
  }
  
  if (!is.null(level)) {
    if (!"level" %in% names(PARLLOC)) {
      stop("PARLLOC is missing 'level' column needed for filtering")
    }
    PARLLOC <- PARLLOC[PARLLOC$level == level, , drop = FALSE]
  }
  
  # Check for NA values
  has_na <- any(is.na(PARLLOC$parliament_size))
  
  # Check for non-numeric values (after converting to numeric)
  numeric_size <- suppressWarnings(as.numeric(PARLLOC$parliament_size))
  has_non_numeric <- any(is.na(numeric_size) & !is.na(PARLLOC$parliament_size))
  
  # Check for non-positive values
  has_non_positive <- any(numeric_size <= 0, na.rm = TRUE)
  
  # Check for non-integer values
  has_non_integer <- any(numeric_size != floor(numeric_size), na.rm = TRUE)
  
  # Return TRUE only if all checks pass
  !has_na && !has_non_numeric && !has_non_positive && !has_non_integer
}

###############################################################################
# Function: check_PARL_parliament_size_meaningful_details
# Description: Return detailed information about parliament_size validation issues
# Returns: List with validation results and problematic rows
# Optional level parameter to filter by parliament level (e.g., "NT" for national)
###############################################################################
check_PARL_parliament_size_meaningful_details <- function(PARLLOC, level = NULL) {
  if (!"parliament_size" %in% names(PARLLOC)) {
    stop("PARLLOC is missing 'parliament_size' column")
  }
  
  if (!is.null(level)) {
    if (!"level" %in% names(PARLLOC)) {
      stop("PARLLOC is missing 'level' column needed for filtering")
    }
    PARLLOC <- PARLLOC[PARLLOC$level == level, , drop = FALSE]
  }
  
  # Analyze different types of issues
  is_na <- is.na(PARLLOC$parliament_size)
  
  # Convert to numeric for further checks
  numeric_size <- suppressWarnings(as.numeric(PARLLOC$parliament_size))
  is_non_numeric <- is.na(numeric_size) & !is.na(PARLLOC$parliament_size)
  is_non_positive <- numeric_size <= 0 & !is.na(numeric_size)
  is_non_integer <- numeric_size != floor(numeric_size) & !is.na(numeric_size)
  
  # Combined problem rows
  has_problem <- is_na | is_non_numeric | is_non_positive | is_non_integer
  
  list(
    check_passed = !any(has_problem),
    total_rows = nrow(PARLLOC),
    na_count = sum(is_na),
    non_numeric_count = sum(is_non_numeric),
    non_positive_count = sum(is_non_positive),
    non_integer_count = sum(is_non_integer),
    na_rows = which(is_na),
    non_numeric_rows = which(is_non_numeric),
    non_positive_rows = which(is_non_positive),
    non_integer_rows = which(is_non_integer),
    problem_rows = which(has_problem),
    full_rows_with_problems = PARLLOC[has_problem, , drop = FALSE]
  )
}