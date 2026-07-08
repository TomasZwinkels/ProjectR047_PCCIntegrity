# =============================================================================
# R047_POLI_functions.R
# Tier 2: POLI integrity check functions
# =============================================================================

# -----------------------------------------------------------------------------
# Check: All pers_id values in POLI are unique
# -----------------------------------------------------------------------------

check_POLI_persid_unique <- function(POLI) {
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")
  !any(duplicated(POLI$pers_id))
}

check_POLI_persid_unique_details <- function(POLI) {
  if (!"pers_id" %in% names(POLI)) stop("POLI is missing column pers_id")

  dup_ids <- unique(POLI$pers_id[duplicated(POLI$pers_id)])
  dup_rows <- POLI[POLI$pers_id %in% dup_ids, , drop = FALSE]

  list(
    check_passed       = length(dup_ids) == 0,
    duplicate_ids      = dup_ids,
    duplicate_count    = length(dup_ids),
    duplicate_rows     = dup_rows,
    total_rows         = nrow(POLI),
    total_unique_ids   = length(unique(POLI$pers_id))
  )
}

# -----------------------------------------------------------------------------
# Check: birth_date not over-concentrated on 01-Jan
#
# Unknown/missing dates of birth that were silently coerced to "01jan<year>"
# upstream produce an implausible spike of 01-Jan birth dates. Genuine births
# are not this concentrated on one day, so an excess of 01-Jan is a data-quality
# anomaly to flag for investigation (NOT an accepted placeholder convention).
#
# Only full "ddMMMyyyy" birth_date values are considered (the denominator);
# year-only entries ("1960"), "NC"/"NA"/"" placeholders and other dirty values
# are excluded from both numerator and denominator. Expected 01-Jan count under
# a uniform assumption is n_full / 365.25. FAIL when the observed count both
# clears an absolute floor (min_count, to avoid false alarms on tiny subsets)
# and exceeds mult * expected.
#
# Advisory / non-blocking: a FAIL reports an anomaly; it does not stop any run.
# -----------------------------------------------------------------------------

check_POLI_birthdate_jan01_excess <- function(POLI, mult = 3, min_count = 5) {
  check_POLI_birthdate_jan01_excess_details(POLI, mult = mult,
                                            min_count = min_count)$check_passed
}

check_POLI_birthdate_jan01_excess_details <- function(POLI, mult = 3, min_count = 5) {
  if (!"birth_date" %in% names(POLI)) stop("POLI is missing column birth_date")

  bd       <- tolower(trimws(as.character(POLI$birth_date)))
  is_full  <- grepl("^[0-9]{2}[a-z]{3}[0-9]{4}$", bd)   # ddMMMyyyy only
  is_jan01 <- is_full & substr(bd, 1, 5) == "01jan"

  n_full   <- sum(is_full)
  obs      <- sum(is_jan01)
  expected <- if (n_full > 0) n_full / 365.25 else 0
  ratio    <- if (expected > 0) obs / expected else NA_real_
  passed   <- !(obs >= min_count & obs > mult * expected)

  list(
    check_passed    = passed,
    jan01_rows      = POLI[is_jan01, , drop = FALSE],
    jan01_count     = obs,
    full_date_count = n_full,
    expected_count  = expected,
    ratio           = ratio,
    summary_stats   = c(
      "Full ddMMMyyyy dates"         = n_full,
      "Jan-01 births"                = obs,
      "Expected (n/365.25)"          = round(expected, 1),
      "Observed / Expected"          = if (is.na(ratio)) NA else round(ratio, 1),
      "Multiplier threshold"         = mult,
      "Year-only entries (excluded)" = sum(grepl("^[0-9]{4}$", bd))
    )
  )
}
