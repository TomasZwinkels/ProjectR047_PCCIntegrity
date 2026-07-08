###############################################################################
# parliament_size may fluctuate within a term. Following the PCC convention for
# committee `seats` (codebook), a fluctuating seat count is written as the
# successive values separated by ';' in chronological order, e.g. "519;663" for
# DE_NT-BT_1987 (519 seated until German reunification on 03oct1990, 663 after).
#
# PARL stores only the term's start/end, NOT the intra-term changeover date(s),
# so those must be supplied manually here. A parliament_size with N values needs
# N-1 changeover dates, each the FIRST day the next size takes effect (midnight
# rule). Consumers that build a daily/step size series (dashboard daily counts,
# R051 baseline) STOP with an actionable error if a required date is missing.
#
# NOTE: this registry is intentionally duplicated in R051_functions.R
# (SIZE_CHANGE_DATES). Keep the two copies in sync.
###############################################################################
SIZE_CHANGE_DATES <- list(
  "DE_NT-BT_1949" = as.Date("1952-02-01"),  # West Berlin delegation 8 -> 19
  "DE_NT-BT_1987" = as.Date("1990-10-03")   # reunification: 519 -> 663
)

###############################################################################
# Function: is_valid_parliament_size
# Description:
#   TRUE if x is a single positive integer, or ';'-separated positive integers
#   (the fluctuating-seats form). Vectorised. NA -> FALSE. Needs no date.
###############################################################################
is_valid_parliament_size <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & grepl("^[0-9]{1,3}(;[0-9]{1,3})*$", x)
  # regex guarantees 1-3 digit segments; also reject any zero segment ("0")
  ok[ok] <- vapply(strsplit(x[ok], ";", fixed = TRUE), function(seg) {
    all(as.integer(seg) > 0)
  }, logical(1))
  ok
}

###############################################################################
# Function: parse_parliament_size_series
# Description:
#   Expand one parliament's (possibly ';'-separated) parliament_size into one
#   row per sub-period, splitting the term at the manually-registered changeover
#   date(s). Single-value sizes return one row spanning the whole term.
# Input:
#   - parliament_id : scalar character
#   - leg_start, leg_end : Date scalars (term bounds)
#   - parliament_size : scalar character, e.g. "518" or "519;663"
#   - registry : named list parliament_id -> Date (or Date vector) of changeovers
# Output:
#   - data.frame(parliament_id, seg_start, seg_end, size) with N rows for N values
# Stops:
#   - if N>1 values but the registry lacks exactly N-1 valid, ordered, in-range
#     changeover dates for this parliament_id.
###############################################################################
parse_parliament_size_series <- function(parliament_id, leg_start, leg_end,
                                         parliament_size,
                                         registry = SIZE_CHANGE_DATES) {
  leg_start <- as.Date(leg_start)
  leg_end   <- as.Date(leg_end)
  sizes <- as.integer(strsplit(as.character(parliament_size), ";", fixed = TRUE)[[1]])
  n <- length(sizes)

  if (n <= 1) {
    return(data.frame(parliament_id = parliament_id,
                      seg_start = leg_start, seg_end = leg_end,
                      size = sizes, stringsAsFactors = FALSE))
  }

  dates <- registry[[parliament_id]]
  need <- n - 1L
  example <- if (need > 1) {
    paste0("c(", paste(rep("as.Date(\"YYYY-MM-DD\")", need), collapse = ", "), ")")
  } else {
    "as.Date(\"YYYY-MM-DD\")"
  }
  hint <- paste0(
    "parliament_size \"", parliament_size, "\" for ", parliament_id,
    " changes mid-term; add its ", need, " changeover date",
    if (need > 1) "s" else "",
    " to SIZE_CHANGE_DATES in this script, e.g.\n",
    "  \"", parliament_id, "\" = ", example,
    "\nthen re-run."
  )
  if (is.null(dates)) stop(hint, call. = FALSE)
  dates <- as.Date(dates)
  if (length(dates) != need || any(is.na(dates)))
    stop(hint, call. = FALSE)
  if (is.unsorted(dates, strictly = TRUE))
    stop("Changeover dates for ", parliament_id,
         " must be strictly increasing.", call. = FALSE)
  if (dates[1] <= leg_start || dates[need] > leg_end)
    stop("Changeover dates for ", parliament_id,
         " must lie within (", format(leg_start), ", ", format(leg_end),
         "].", call. = FALSE)

  seg_start <- c(leg_start, dates)
  seg_end   <- c(dates - 1L, leg_end)
  data.frame(parliament_id = parliament_id,
             seg_start = seg_start, seg_end = seg_end,
             size = sizes, stringsAsFactors = FALSE)
}

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
# Classify a single parliament_size value into one of:
#   "ok" | "na" | "non_numeric" | "non_positive" | "non_integer"
# A ';'-separated value (fluctuating seats) is "ok" iff every segment is a
# positive integer; single values keep the original numeric semantics so that
# e.g. 150.5 -> "non_integer" and "INVALID" -> "non_numeric".
classify_parliament_size <- function(x) {
  if (is.na(x)) return("na")
  x <- as.character(x)
  if (grepl(";", x, fixed = TRUE)) {
    if (is_valid_parliament_size(x)) return("ok")
    # invalid ';'-form: a bad segment is non_numeric, else a <=0 segment
    seg <- strsplit(x, ";", fixed = TRUE)[[1]]
    if (!all(grepl("^[0-9]+$", seg))) return("non_numeric")
    return("non_positive")
  }
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num)) return("non_numeric")
  if (num <= 0) return("non_positive")
  if (num != floor(num)) return("non_integer")
  "ok"
}

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

  # A ';'-separated value (fluctuating seats, e.g. "519;663") is valid.
  classes <- vapply(PARLLOC$parliament_size, classify_parliament_size,
                    character(1), USE.NAMES = FALSE)
  all(classes == "ok")
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
  
  # Analyze different types of issues. A ';'-separated value (fluctuating
  # seats, e.g. "519;663") with all-positive-integer segments is valid.
  classes <- vapply(PARLLOC$parliament_size, classify_parliament_size,
                    character(1), USE.NAMES = FALSE)
  is_na           <- classes == "na"
  is_non_numeric  <- classes == "non_numeric"
  is_non_positive <- classes == "non_positive"
  is_non_integer  <- classes == "non_integer"

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