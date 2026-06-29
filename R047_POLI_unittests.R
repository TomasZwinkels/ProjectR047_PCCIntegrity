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
