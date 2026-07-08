# =============================================================================
# R047_POLI_unittests.R
# Unit tests for R047_POLI_functions.R
# =============================================================================

library(testthat)

# =============================================================================
# check_POLI_persid_unique
# =============================================================================

test_that("returns TRUE when all pers_id values are unique", {
  POLI <- data.frame(pers_id = c("A", "B", "C"), stringsAsFactors = FALSE)
  expect_true(check_POLI_persid_unique(POLI))
})

test_that("returns FALSE when duplicate pers_id values exist", {
  POLI <- data.frame(pers_id = c("A", "B", "A"), stringsAsFactors = FALSE)
  expect_false(check_POLI_persid_unique(POLI))
})

test_that("returns TRUE for single-row POLI", {
  POLI <- data.frame(pers_id = "A", stringsAsFactors = FALSE)
  expect_true(check_POLI_persid_unique(POLI))
})

test_that("errors when pers_id column is missing", {
  POLI <- data.frame(name = "A", stringsAsFactors = FALSE)
  expect_error(check_POLI_persid_unique(POLI), "missing column pers_id")
})

# =============================================================================
# check_POLI_persid_unique_details
# =============================================================================

test_that("details returns passing result when all unique", {
  POLI <- data.frame(pers_id = c("A", "B", "C"), x = 1:3, stringsAsFactors = FALSE)
  result <- check_POLI_persid_unique_details(POLI)

  expect_true(result$check_passed)
  expect_equal(result$duplicate_count, 0)
  expect_equal(length(result$duplicate_ids), 0)
  expect_equal(nrow(result$duplicate_rows), 0)
  expect_equal(result$total_rows, 3)
  expect_equal(result$total_unique_ids, 3)
})

test_that("details returns failing result with duplicate rows", {
  POLI <- data.frame(pers_id = c("A", "B", "A", "C", "B"),
                     val = 1:5, stringsAsFactors = FALSE)
  result <- check_POLI_persid_unique_details(POLI)

  expect_false(result$check_passed)
  expect_equal(sort(result$duplicate_ids), c("A", "B"))
  expect_equal(result$duplicate_count, 2)
  expect_equal(nrow(result$duplicate_rows), 4)
  expect_true(all(result$duplicate_rows$pers_id %in% c("A", "B")))
  expect_equal(result$total_rows, 5)
  expect_equal(result$total_unique_ids, 3)
})

test_that("details errors when pers_id column is missing", {
  POLI <- data.frame(name = "A", stringsAsFactors = FALSE)
  expect_error(check_POLI_persid_unique_details(POLI), "missing column pers_id")
})

# =============================================================================
# check_POLI_birthdate_jan01_excess
# =============================================================================

# Helper: POLI frame with `n_jan01` "01jan1970" births plus `n_other` births
# spread across distinct non-Jan-01 days, and optional extra raw values.
mk_dob_test_data <- function(n_jan01, n_other, extra = character(0)) {
  # distinct non-01jan full dates: 02jan.. onward, cycling months to stay valid
  months <- c("feb", "mar", "apr", "may", "jun", "jul",
              "aug", "sep", "oct", "nov", "dec")
  other <- character(0)
  if (n_other > 0) {
    days  <- sprintf("%02d", ((seq_len(n_other) - 1) %% 27) + 2)  # 02..28
    mos   <- months[((seq_len(n_other) - 1) %% length(months)) + 1]
    other <- paste0(days, mos, "1970")
  }
  bd <- c(rep("01jan1970", n_jan01), other, extra)
  data.frame(pers_id = paste0("P", seq_along(bd)),
             birth_date = bd, stringsAsFactors = FALSE)
}

test_that("FAILs when 01-Jan births are over-represented", {
  POLI <- mk_dob_test_data(n_jan01 = 100, n_other = 200)
  expect_false(check_POLI_birthdate_jan01_excess(POLI))
})

test_that("PASSes when births are spread across distinct days", {
  POLI <- mk_dob_test_data(n_jan01 = 1, n_other = 400)
  expect_true(check_POLI_birthdate_jan01_excess(POLI))
})

test_that("PASSes on a tiny sample below min_count even if concentrated", {
  POLI <- mk_dob_test_data(n_jan01 = 2, n_other = 1)
  expect_true(check_POLI_birthdate_jan01_excess(POLI))
})

test_that("errors when birth_date column is missing", {
  POLI <- data.frame(pers_id = "A", stringsAsFactors = FALSE)
  expect_error(check_POLI_birthdate_jan01_excess(POLI),
               "missing column birth_date")
})

# =============================================================================
# check_POLI_birthdate_jan01_excess_details
# =============================================================================

test_that("details count 01-Jan births and expose summary stats", {
  POLI <- mk_dob_test_data(n_jan01 = 50, n_other = 100)
  result <- check_POLI_birthdate_jan01_excess_details(POLI)

  expect_false(result$check_passed)
  expect_equal(result$jan01_count, 50)
  expect_equal(result$full_date_count, 150)
  expect_equal(nrow(result$jan01_rows), 50)
  expect_true(all(tolower(result$jan01_rows$birth_date) == "01jan1970"))
  expect_true("Jan-01 births" %in% names(result$summary_stats))
})

test_that("details exclude year-only and dirty values from full-date counts", {
  # 6 clean 01-Jan, 3 year-only, plus placeholder/dirty values
  POLI <- mk_dob_test_data(n_jan01 = 6, n_other = 0,
                           extra = c("1960", "1975", "1980",
                                     "NC", "NA", "", "m"))
  result <- check_POLI_birthdate_jan01_excess_details(POLI)

  expect_equal(result$full_date_count, 6)   # only the 6 ddMMMyyyy dates
  expect_equal(result$jan01_count, 6)
  expect_equal(unname(result$summary_stats["Year-only entries (excluded)"]), 3)
})

test_that("details PASS with NA ratio when there are no full dates", {
  POLI <- data.frame(pers_id = c("A", "B"),
                     birth_date = c("1960", "NC"), stringsAsFactors = FALSE)
  result <- check_POLI_birthdate_jan01_excess_details(POLI)

  expect_true(result$check_passed)
  expect_equal(result$full_date_count, 0)
  expect_equal(result$jan01_count, 0)
  expect_true(is.na(result$ratio))
})
