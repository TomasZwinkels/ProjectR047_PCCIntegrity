# R047_dashboard_slow_unittests.R
# Slow unit tests for Dashboard/R047_dashboard_functions.R — these call
# external services (GitHub API via the gh CLI, Codex LLM), so they are
# NOT run at app startup. Fast, pure-logic tests live in
# R047_dashboard_unittests.R.

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/R047_dashboard_functions.R")

library(testthat)

# --- gh_list_issues (GitHub API) ---

test_that("gh_list_issues returns empty data.frame for nonexistent labels", {
  df <- gh_list_issues("TomasZwinkels/PCCdata",
                       "XX / FAKE / check / nonexistent_label_xyz")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
  expect_true(all(c("number", "title", "state", "url") %in% names(df)))
})

test_that("gh_list_issues returns correct columns for existing issues", {
  # This relies on the birth_place_raw issue we created earlier
  df <- gh_list_issues("TomasZwinkels/PCCdata",
                       "NL / POLI / completeness / birth_place_raw")
  expect_true(is.data.frame(df))
  expect_true(all(c("number", "title", "state", "url") %in% names(df)))
  if (nrow(df) > 0) {
    expect_true(is.integer(df$number) || is.numeric(df$number))
    expect_true(all(df$state %in% c("OPEN", "CLOSED")))
    expect_true(all(grepl("github.com", df$url)))
  }
})

test_that("gh_list_issues handles gracefully when repo doesn't exist", {
  df <- gh_list_issues("nonexistent/repo_xyz_999",
                       "NL / POLI / completeness / birth_date")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
})

# --- llm_generate_* fallbacks (call Codex when installed) ---

test_that("llm_generate_title falls back to path on failure", {
  # codex_query will fail if codex is not installed or key not set
  # but the function should gracefully return the path
  path <- "XX / TEST / check / fake_check"
  result <- llm_generate_title(path, "some summary")
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("llm_generate_description returns string on failure", {
  result <- llm_generate_description("XX / TEST / check / fake", "summary")
  expect_true(is.character(result))
})

# --- codex_query integration tests (run only if codex is available) ---

codex_available <- nchar(Sys.which("codex")) > 0

test_that("codex_query returns a response for a simple prompt", {
  skip_if(!codex_available, "codex CLI not installed")
  result <- codex_query("Reply with only the word hello")
  expect_false(is.null(result))
  expect_true(grepl("hello", tolower(result)))
})

test_that("llm_generate_title produces a readable title", {
  skip_if(!codex_available, "codex CLI not installed")
  path <- "NL / POLI / completeness / birth_date"
  summary <- "**Variable:** `birth_date`\n**Missing for:** 42 MPs"
  title <- llm_generate_title(path, summary)
  expect_true(nchar(title) > 0)
  expect_true(nchar(title) <= 120)
  # Should not just be the raw path
  expect_false(identical(title, path))
})

test_that("llm_generate_description produces a description", {
  skip_if(!codex_available, "codex CLI not installed")
  path <- "NL / POLI / completeness / birth_date"
  summary <- "**Variable:** `birth_date`\n**Missing for:** 42 MPs"
  desc <- llm_generate_description(path, summary)
  expect_true(nchar(desc) > 0)
  expect_true(nchar(desc) > 20)
})
