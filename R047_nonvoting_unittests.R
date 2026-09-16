# Voting totals exclude US delegates and West Berlin deputies; their RESE
# records remain available to generic identity, date and overlap validation.
library(testthat)
suppressPackageStartupMessages(library(dplyr))
source("R047_functions.R")
source("R047_RESE_functions.R")
source("R047_MEME_functions.R")
source("R047_PARL_functions.R")
source("Dashboard/R047_dashboard_functions.R")

nonvoting_fixture <- function() {
  data.frame(
    pers_id = c("US_voting", "US_delegate", "US_senator", "DE_voting", "DE_berlin", "DE_substitute"),
    country_abb = c("US", "US", "US", "DE", "DE", "DE"),
    political_function = c("NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_11",
                           "NT_LE-UH_T3_NA_01", "NT_LE_T3_NA_01",
                           "NT_LE_T3_NA_11", "NT_LE_T3_NA_09"),
    start_date = as.Date("2025-01-03"),
    end_date = as.Date("2025-01-04"),
    stringsAsFactors = FALSE
  )
}

load_app_functions <- function(names, env) {
  for (expr in parse("Dashboard/app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% names) eval(expr, env)
  }
}

test_that("nonvoting records retain generic overlap and ID validation", {
  rese <- nonvoting_fixture()
  rese$res_entry_id <- paste0(rese$pers_id, "__1")
  rese <- rbind(rese, rese[2, ], rese[5, ])
  rese$res_entry_start_posoxctformat <- as.POSIXct(rese$start_date)
  rese$res_entry_end_posoxctformat <- as.POSIXct(rese$end_date)
  expect_false(check_RESE_resentryid_unique(rese))
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(rese))
  detail <- check_RESE_parlmemeppisodes_anyfulloverlap_details(rese)
  expect_setequal(detail$affected_persons, c("US_delegate", "DE_berlin"))
  expect_setequal(rese$pers_id[rese$political_function %in% parliamentary_record_pf_codes],
                  c("US_voting", "US_delegate", "DE_voting", "DE_berlin", "DE_substitute"))
})

test_that("party coverage is required for voting members including serving substitutes", {
  rese <- nonvoting_fixture()
  meme <- data.frame(pers_id = c("US_voting", "DE_voting", "DE_substitute"))
  expect_true(check_MEME_parlmembers_have_party(rese, meme))
  expect_equal(check_MEME_parlmembers_have_party_details(rese, meme)$total_parlmembers, 3L)
  meme <- meme[meme$pers_id != "DE_substitute", , drop = FALSE]
  expect_false(check_MEME_parlmembers_have_party(rese, meme))
  expect_identical(check_MEME_parlmembers_have_party_details(rese, meme)$missing_ids, "DE_substitute")
})

test_that("US and German daily counts, cohorts and detail rosters exclude nonvoters", {
  app_env <- new.env(parent = environment())
  load_app_functions(c("build_cohort", "build_daily_counts"), app_env)
  app_env$Sys.Date <- function() as.Date("2025-01-04")
  app_env$RESE <- nonvoting_fixture()
  app_env$PARL <- data.frame(
    parliament_id = c("US_NT-HR_2025", "DE_NT-BT_2025"), country_abb = c("US", "DE"), level = "NT",
    assembly_abb = c("HR", "BT"), parliament_size = c("1", "2"),
    leg_period_start_date = as.Date("2025-01-03"),
    leg_period_end_date = as.Date("2025-01-04")
  )
  app_env$assembly_map <- c(US = "HR", DE = "BT")
  for (cc in c("US", "DE")) {
    expected <- if (cc == "US") "US_voting" else c("DE_voting", "DE_substitute")
    expect_setequal(rese_mp_rows(app_env$RESE, cc)$pers_id, expected)
    expect_setequal(app_env$build_cohort(cc)$pers_id, expected)
    counts <- app_env$build_daily_counts(cc)
    expect_equal(counts$n_seated, rep(length(expected), 2L))
    expect_equal(counts$parliament_size, rep(length(expected), 2L))
  }
  expect_equal(nrow(app_env$RESE), 6L)
})

test_that("standalone coverage and first-day scripts use the voting population", {
  for (script in c("R047.R", "completeness/generate_first_day_parlmem_cohort.R")) {
    exprs <- parse(script)
    target <- if (script == "R047.R") "RESE_voting" else "RESE"
    filters <- Filter(function(expr) {
      is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name(target)) &&
        all(c("political_function", "%in%") %in% all.names(expr[[3]]))
    }, as.list(exprs))
    expect_length(filters, 1L)
    fixture_env <- new.env(parent = environment())
    fixture_env$RESE <- nonvoting_fixture()
    eval(filters[[1]], fixture_env)
    expect_setequal(fixture_env[[target]]$pers_id,
                    c("US_voting", "DE_voting", "DE_substitute"))
    if (script == "R047.R") expect_equal(nrow(fixture_env$RESE), 6L)
  }
})

test_that("daily caches invalidate totals produced under the all-seat policy", {
  env <- new.env(parent = environment())
  load_app_functions("get_daily_counts", env)
  env$cache_dir <- tempfile("r47-voting-cache-")
  dir.create(env$cache_dir)
  on.exit(unlink(env$cache_dir, recursive = TRUE), add = TRUE)
  saveRDS(data.frame(n_seated = 2L), file.path(env$cache_dir, "daily_counts_US.rds"))
  writeLines("v1", file.path(env$cache_dir, "daily_counts_US_version.txt"))
  env$readLines <- function(con, ...) {
    if (endsWith(con, "dataversion.txt")) "v1" else base::readLines(con, ...)
  }
  calls <- 0L
  env$build_daily_counts <- function(cc) {
    calls <<- calls + 1L
    data.frame(n_seated = 1L)
  }
  expect_equal(env$get_daily_counts("US")$n_seated, 1L)
  expect_equal(env$get_daily_counts("US")$n_seated, 1L)
  expect_equal(calls, 1L)
})

test_that("dashboard keeps nonvoter ID checks but excludes nonvoter-only coverage", {
  env <- new.env(parent = environment())
  load_app_functions(c("checks_table", "run_rese_checks"), env)
  env$RESE <- nonvoting_fixture()[2, ]
  env$RESE$res_entry_id <- "US_delegate__1"
  env$RESE$res_entry_start <- "03jan2025"
  env$RESE$res_entry_end <- "04jan2025"
  env$RESE <- rbind(env$RESE, env$RESE)
  env$POLI <- data.frame(pers_id = "US_delegate")
  env$PARL <- data.frame()
  env$MEME <- data.frame()
  env$assembly_map <- c(US = "HR")
  env$verified_not_duplicates <- data.frame()
  # Keep real ID and boundary-coverage functions to verify population routing;
  # the other checks have their own suites and need wider data fixtures.
  stub <- function(...) list(check_passed = TRUE)
  for (name in c("check_RESE_persid_in_POLI_details", "check_anyNAinRESEdates_details",
                 "check_RESE_parlmemeppisodes_anyfulloverlap_details", "check_RESE_anynear_fulloverlap_details",
                 "check_RESE_duplicate_birthdates_in_faction_details", "check_RESE_parliament_id_matches_dates_details",
                 "check_RESE_parlmem_coverage_details", "check_special_chars_details",
                 "check_RESE_persid_present_details", "check_RESE_resentryid_no_whitespace_details",
                 "check_RESE_id_matches_persid_details")) env[[name]] <- stub
  result <- env$run_rese_checks("US", as.Date("2025-01-03"), as.Date("2025-01-04"))
  expect_false(result$details[[2]]$check_passed)
  expect_false(result$details[[9]]$check_passed)
  expect_false(result$details[[10]]$check_passed)
})
