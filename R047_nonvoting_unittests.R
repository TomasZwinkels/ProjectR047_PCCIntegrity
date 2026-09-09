# US House delegates and Berlin deputies belong in seated-member populations.
# Load pure functions and selected app functions, avoiding the app's data I/O.
library(testthat)
suppressPackageStartupMessages(library(dplyr))
source("R047_RESE_functions.R")
source("R047_MEME_functions.R")
source("R047_PARL_functions.R")
source("Dashboard/R047_dashboard_functions.R")

nonvoting_fixture <- function() {
  data.frame(
    pers_id = c("US_voting", "US_delegate", "US_senator", "DE_berlin"),
    country_abb = c("US", "US", "US", "DE"),
    political_function = c("NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_11",
                           "NT_LE-UH_T3_NA_01", "NT_LE_T3_NA_11"),
    start_date = as.Date("2025-01-03"),
    end_date = as.Date("2025-01-04"),
    stringsAsFactors = FALSE
  )
}

test_that("US delegates remain in parliamentary overlap and party checks", {
  rese <- nonvoting_fixture()
  rese <- rbind(rese, rese[2, ])
  rese$res_entry_start_posoxctformat <- as.POSIXct(rese$start_date)
  rese$res_entry_end_posoxctformat <- as.POSIXct(rese$end_date)
  expect_true(check_RESE_parlmemeppisodes_anyfulloverlap(rese))
  detail <- check_RESE_parlmemeppisodes_anyfulloverlap_details(rese)
  expect_identical(detail$affected_persons, "US_delegate")
  expect_equal(detail$total_parl_episodes, 4L)

  meme <- data.frame(pers_id = c("US_voting", "DE_berlin"))
  expect_false(check_MEME_parlmembers_have_party(rese, meme))
  detail <- check_MEME_parlmembers_have_party_details(rese, meme)
  expect_identical(detail$missing_ids, "US_delegate")
  expect_equal(detail$total_parlmembers, 3L)
  meme <- rbind(meme, data.frame(pers_id = "US_delegate"))
  expect_true(check_MEME_parlmembers_have_party(rese, meme))
})

test_that("US delegate appears in dashboard daily counts and cohort", {
  app_expressions <- parse("Dashboard/app.R")
  app_env <- new.env(parent = environment())
  for (expr in app_expressions) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% c("build_cohort", "build_daily_counts")) {
      eval(expr, app_env)
    }
  }
  app_env$RESE <- nonvoting_fixture()
  app_env$PARL <- data.frame(
    parliament_id = "US_NT-HR_2025", country_abb = "US", level = "NT",
    assembly_abb = "HR", parliament_size = "2",
    leg_period_start_date = as.Date("2025-01-03"),
    leg_period_end_date = as.Date("2025-01-04")
  )
  app_env$assembly_map <- c(US = "HR")
  expect_setequal(rese_mp_rows(app_env$RESE, "US")$pers_id,
                  c("US_voting", "US_delegate"))
  expect_identical(rese_mp_rows(app_env$RESE, "DE")$pers_id, "DE_berlin")
  cohort <- app_env$build_cohort("US")
  expect_setequal(cohort$pers_id, c("US_voting", "US_delegate"))
  counts <- app_env$build_daily_counts("US")
  expect_equal(counts$n_seated[1:2], c(2L, 2L))
  expect_equal(counts$parliament_size[1:2], c(2L, 2L))
})

test_that("standalone integrity and first-day scripts retain seated non-voters", {
  for (script in c("R047.R", "completeness/generate_first_day_parlmem_cohort.R")) {
    exprs <- parse(script)
    # Execute the actual pure membership-subset step with a mixed fixture;
    # sourcing the whole script would load and export production data.
    filters <- Filter(function(expr) {
      is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("RESE")) &&
        all(c("political_function", "%in%") %in% all.names(expr[[3]]))
    }, as.list(exprs))
    expect_length(filters, 1L)
    fixture_env <- new.env(parent = environment())
    fixture_env$RESE <- nonvoting_fixture()
    eval(filters[[1]], fixture_env)
    expect_setequal(fixture_env$RESE$pers_id,
                    c("US_voting", "US_delegate", "DE_berlin"))
  }
})
