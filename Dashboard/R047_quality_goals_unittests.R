# R047_quality_goals_unittests.R
# Fast unit tests for Dashboard/R047_quality_goals.R — pure logic only, safe to
# run at app startup. The evaluators read everything from an explicit `ctx`, so
# these tests build a tiny synthetic ctx with no external data or Shiny.

source("/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/R047_quality_goals.R")

library(testthat)

MP <- "NT_LE_T3_NA_01"   # one of the canonical MP codes

# A minimal ctx factory; callers override individual pieces as needed.
make_ctx <- function(POLI = NULL, RESE = NULL, PARL = NULL,
                     build_cohort_fn = function(cc) NULL,
                     daily_counts_fn = function(cc) NULL,
                     checks_fns = NULL) {
  if (is.null(checks_fns)) {
    empty <- function(cc) list(table = data.frame())
    checks_fns <- list(POLI = empty, PARL = empty, MEME = empty, RESE = empty)
  }
  list(
    POLI = POLI, RESE = RESE, PARL = PARL,
    mp_codes = c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01",
                 "NT_LE_T3_NA_09", "NT_LE_T3_NA_11"),
    period_start = as.Date("1946-01-01"),
    period_end   = as.Date("2025-12-31"),
    assembly_map = c(XX = "AA"),
    build_cohort_fn = build_cohort_fn,
    daily_counts_fn = daily_counts_fn,
    checks_fns = checks_fns
  )
}

# --- avail (mps_overall) ---

test_that("avail mps_overall passes at 100% and fails below target", {
  POLI <- data.frame(
    pers_id   = c("XX_A_1", "XX_B_2", "XX_C_3", "XX_D_4"),
    country   = "XX",
    last_name = c("A", "B", "C", ""),   # 3/4 present
    stringsAsFactors = FALSE)
  RESE <- data.frame(
    pers_id = c("XX_A_1", "XX_B_2", "XX_C_3", "XX_D_4"),
    country_abb = "XX", political_function = MP, stringsAsFactors = FALSE)
  ctx <- make_ctx(POLI = POLI, RESE = RESE)

  g100 <- goal("g", "POLI", "last", "avail", "mps_overall", 100, var = "last_name")
  res  <- evaluate_goal(g100, "XX", ctx)
  expect_equal(round(res$value, 1), 75.0)
  expect_false(res$pass)

  POLI$last_name <- c("A", "B", "C", "D")   # now 4/4
  ctx <- make_ctx(POLI = POLI, RESE = RESE)
  res <- evaluate_goal(g100, "XX", ctx)
  expect_equal(res$value, 100)
  expect_true(res$pass)
})

# --- avail_year uses a 4-digit-year predicate ---

test_that("avail_year counts bare years and full dates, not empties", {
  POLI <- data.frame(
    pers_id    = c("XX_A_1", "XX_B_2", "XX_C_3"),
    country    = "XX",
    birth_date = c("1960", "04jan1966", ""),   # 2/3 carry a year
    stringsAsFactors = FALSE)
  RESE <- data.frame(pers_id = POLI$pers_id, country_abb = "XX",
                     political_function = MP, stringsAsFactors = FALSE)
  ctx <- make_ctx(POLI = POLI, RESE = RESE)
  g <- goal("g", "POLI", "dob", "avail_year", "mps_overall", 97.5, var = "birth_date")
  res <- evaluate_goal(g, "XX", ctx)
  expect_equal(round(res$value, 1), 66.7)
  expect_false(res$pass)
})

# --- avail per_parliament: fails if any single parliament dips below target ---

test_that("per_parliament avail flags the failing parliament", {
  # Parliament P1: 2/2 present (100%). P2: 1/2 present (50%).
  cohort <- data.frame(
    parliament_id   = c("P1", "P1", "P2", "P2"),
    snapshot_day    = as.Date("2000-01-01"),
    parliament_size = 2,
    pers_id         = c("XX_A_1", "XX_B_2", "XX_C_3", "XX_D_4"),
    stringsAsFactors = FALSE)
  POLI <- data.frame(
    pers_id        = c("XX_A_1", "XX_B_2", "XX_C_3", "XX_D_4"),
    country        = "XX",
    birth_place_raw = c("Town", "City", "Village", ""),
    stringsAsFactors = FALSE)
  ctx <- make_ctx(POLI = POLI, build_cohort_fn = function(cc) cohort)
  g <- goal("g", "POLI", "bpl", "avail", "per_parliament", 97.5, var = "birth_place_raw")
  res <- evaluate_goal(g, "XX", ctx)
  expect_false(res$pass)
  expect_equal(res$value, 50)          # worst parliament
  expect_match(res$display, "1/2 parl fail")
})

# --- checks_pass ---

test_that("checks_pass reports k/n and fails on any FAIL", {
  ctx_fail <- make_ctx(checks_fns = list(RESE = function(cc) list(
    table = data.frame(Check = c("a", "b", "c"),
                       Status = c("PASS", "FAIL", "PASS")))))
  g <- goal("g", "RESE", "hard", "checks_pass", "country", NA)
  res <- evaluate_goal(g, "XX", ctx_fail)
  expect_false(res$pass)
  expect_equal(res$display, "2/3")

  ctx_ok <- make_ctx(checks_fns = list(RESE = function(cc) list(
    table = data.frame(Check = c("a", "b"), Status = c("PASS", "PASS")))))
  res <- evaluate_goal(g, "XX", ctx_ok)
  expect_true(res$pass)
  expect_equal(res$display, "2/2")
})

test_that("checks_pass dispatches on goal$df", {
  ctx <- make_ctx(checks_fns = list(
    POLI = function(cc) list(table = data.frame(Check = "p", Status = "PASS")),
    PARL = function(cc) list(table = data.frame(Check = c("a", "b"),
                                                Status = c("PASS", "FAIL")))))
  gp <- goal("gp", "POLI", "hard", "checks_pass", "country", NA)
  gq <- goal("gq", "PARL", "hard", "checks_pass", "country", NA)
  expect_true(evaluate_goal(gp, "XX", ctx)$pass)
  expect_equal(evaluate_goal(gp, "XX", ctx)$display, "1/1")
  expect_false(evaluate_goal(gq, "XX", ctx)$pass)
  expect_equal(evaluate_goal(gq, "XX", ctx)$display, "1/2")
})

# --- date_complete (start dates) ---

test_that("date_complete measures non-NA start dates over MP episodes", {
  RESE <- data.frame(
    pers_id = paste0("XX_", 1:4),
    country_abb = "XX", political_function = MP,
    start_date = as.Date(c("2000-01-01", "2004-01-01", NA, "2010-01-01")),
    end_date   = as.Date(NA),
    stringsAsFactors = FALSE)
  ctx <- make_ctx(RESE = RESE)
  g <- goal("g", "RESE", "start", "date_complete", "country", 100, var = "start_date")
  res <- evaluate_goal(g, "XX", ctx)
  expect_equal(res$value, 75)   # 3/4
  expect_false(res$pass)
})

# --- date_complete_ended excludes still-sitting memberships ---

test_that("end-date completeness ignores open-ended sitting MPs", {
  # Latest term starts 2020-01-01. Episode 3 starts in that term with a blank
  # end date -> legitimately open, must NOT count as a violation. Episode 2
  # (ended term, blank end date) IS a violation.
  PARL <- data.frame(
    country_abb = "XX", level = "NT", assembly_abb = "AA",
    leg_period_start_date = as.Date(c("2010-01-01", "2020-01-01")),
    stringsAsFactors = FALSE)
  RESE <- data.frame(
    pers_id = paste0("XX_", 1:3),
    country_abb = "XX", political_function = MP,
    start_date = as.Date(c("2010-01-01", "2010-06-01", "2020-02-01")),
    end_date   = as.Date(c("2013-01-01", NA, NA)),
    stringsAsFactors = FALSE)
  ctx <- make_ctx(RESE = RESE, PARL = PARL)
  g <- goal("g", "RESE", "end", "date_complete_ended", "country", 100, var = "end_date")
  res <- evaluate_goal(g, "XX", ctx)
  # Denominator = ended-term episodes (start < 2020) = episodes 1 and 2.
  # Violations = 1 (episode 2). value = 100*(1 - 1/2) = 50.
  expect_equal(res$value, 50)
  expect_false(res$pass)

  # If the sitting MP (episode 3) were the only open one and both ended-term
  # episodes had end dates, the goal passes.
  RESE$end_date <- as.Date(c("2013-01-01", "2014-01-01", NA))
  ctx <- make_ctx(RESE = RESE, PARL = PARL)
  res <- evaluate_goal(g, "XX", ctx)
  expect_equal(res$value, 100)
  expect_true(res$pass)
})

# --- overcount_free at the 99.5% boundary ---

test_that("overcount_free passes at >=99.5% overcount-free days", {
  # 1000 days, 4 overcount -> 99.6% free -> pass. 6 overcount -> 99.4% -> fail.
  mk_dc <- function(n_over) {
    n <- 1000
    data.frame(
      date = seq(as.Date("2000-01-01"), by = "day", length.out = n),
      n_seated = c(rep(101L, n_over), rep(100L, n - n_over)),
      parliament_size = 100L)
  }
  g <- goal("g", "RESE", "over", "overcount_free", "country", 99.5)

  ctx <- make_ctx(daily_counts_fn = function(cc) mk_dc(4))
  res <- evaluate_goal(g, "XX", ctx)
  expect_equal(round(res$value, 1), 99.6)
  expect_true(res$pass)

  ctx <- make_ctx(daily_counts_fn = function(cc) mk_dc(6))
  res <- evaluate_goal(g, "XX", ctx)
  expect_equal(round(res$value, 1), 99.4)
  expect_false(res$pass)
})

# --- build_goals_matrix shape ---

test_that("build_goals_matrix returns one row per goal and one cell per country", {
  ctx <- make_ctx()   # everything NA/empty -> n/a cells, but shape is what we test
  goals <- list(
    goal("g1", "RESE", "hard", "checks_pass", "country", NA),
    goal("g2", "RESE", "over", "overcount_free", "country", 99.5))
  m <- build_goals_matrix(goals, c("XX", "YY"), ctx)
  expect_length(m, 2)
  expect_equal(names(m[[1]]$cells), c("XX", "YY"))
  expect_true(all(vapply(m[[1]]$cells, function(c) c$na, logical(1))))
})
