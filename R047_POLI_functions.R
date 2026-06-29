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
