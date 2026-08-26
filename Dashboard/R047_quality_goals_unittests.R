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

# --- undercount_structural: helpers -----------------------------------------

# Daily-counts frame from a relative-deficit vector (one value per day).
mk_daily <- function(start, rel, size = 100L) {
  data.frame(
    date            = seq(as.Date(start), by = "day", length.out = length(rel)),
    n_seated        = as.integer(round(size * (1 - rel))),
    parliament_size = size)
}

mk_parl <- function(ids, starts, ends) {
  data.frame(
    country_abb = "XX", level = "NT", assembly_abb = "AA",
    parliament_id = ids,
    leg_period_start_date = as.Date(starts),
    leg_period_end_date   = as.Date(ends),
    stringsAsFactors = FALSE)
}

# Full pipeline over synthetic daily counts; returns undercount_flag_legislatures().
uc_run <- function(dc, parl, coverage_end = as.Date(NA)) {
  daily <- undercount_daily(dc, coverage_end)
  stats <- undercount_legislature_stats(daily, parl, coverage_end)
  undercount_flag_legislatures(stats, median(daily$rel_deficit))
}

# --- undercount_coverage_end (vintage guard boundary) ---

test_that("coverage end is the last end date only when no membership is open", {
  rese <- data.frame(end_date = as.Date(c("2000-06-30", "1998-01-01")))
  expect_equal(undercount_coverage_end(rese), as.Date("2000-06-30"))
  rese$end_date[1] <- NA   # an open-ended sitting MP -> coverage runs to today
  expect_true(is.na(undercount_coverage_end(rese)))
  expect_true(is.na(undercount_coverage_end(rese[0, , drop = FALSE])))
})

# --- frictional vacancy never flags; deep chronic deficit always does ---

test_that("frictional floor and the self-masking cap both hold", {
  parl <- mk_parl("P1", "2000-01-01", "2000-12-31")

  # Constant 1-seat-in-100 vacancy: classic by-election friction -> clean.
  fl <- uc_run(mk_daily("2000-01-01", rep(0.01, 366)), parl)
  expect_false(any(fl$stats$flagged))

  # Uniform 3%: baseline self-masks (thr rises to the 5% cap) -> still clean.
  fl <- uc_run(mk_daily("2000-01-01", rep(0.03, 366)), parl)
  expect_false(any(fl$stats$flagged))

  # Uniform 6%: above the absolute cap -> flags even though the whole
  # country's baseline is equally bad (broken data can't excuse itself).
  fl <- uc_run(mk_daily("2000-01-01", rep(0.06, 366)), parl)
  expect_true(fl$stats$flagged)
  expect_equal(fl$stats$severity, "structural")
})

# --- baseline adaptivity: flag only the legislature worse than its country ---

test_that("chronic flag is relative to the country's own vacancy baseline", {
  parl <- mk_parl(c("P1", "P2", "P3"),
                  c("2000-01-01", "2001-01-01", "2002-01-01"),
                  c("2000-12-31", "2001-12-31", "2002-12-31"))
  dc <- rbind(mk_daily("2000-01-01", rep(0.020, 366), size = 1000L),
              mk_daily("2001-01-01", rep(0.020, 365), size = 1000L),
              mk_daily("2002-01-01", rep(0.045, 365), size = 1000L))
  fl <- uc_run(dc, parl)
  # Baseline 2% -> threshold 4%: the 2% legislatures are that country's
  # normal, the 4.5% one is structurally undercounted.
  expect_equal(fl$baseline, 0.02)
  expect_equal(fl$stats$flagged, c(FALSE, FALSE, TRUE))
  expect_equal(fl$stats$severity[3], "structural")
})

# --- acute statistic: a localized gap in an otherwise healthy legislature ---

test_that("worst-90-day window catches localized gaps the median misses", {
  parl <- mk_parl("P1", "2000-01-01", "2000-12-31")

  # 100 days at 15% missing, rest full: median is 0, acute is ~15% -> flag.
  fl <- uc_run(mk_daily("2000-01-01", c(rep(0.15, 100), rep(0, 266))), parl)
  expect_true(fl$stats$flagged)
  expect_equal(fl$stats$chronic, 0)
  expect_equal(fl$stats$acute, 0.15)
  # The worst window is located (first 90-day window inside the gap).
  expect_equal(fl$stats$acute_start, as.Date("2000-01-01"))
  expect_equal(fl$stats$acute_end,   as.Date("2000-03-30"))

  # A 30-day blip at 15% dilutes to exactly the 5% acute floor -> clean.
  fl <- uc_run(mk_daily("2000-01-01", c(rep(0.15, 30), rep(0, 336))), parl)
  expect_false(fl$stats$flagged)
})

# --- coverage cliff severity ---

test_that("a sustained >=50% deficit is labelled a coverage cliff", {
  parl <- mk_parl("P1", "2000-01-01", "2000-12-31")
  fl <- uc_run(mk_daily("2000-01-01", c(rep(1.0, 20), rep(0, 346))), parl)
  expect_true(fl$stats$flagged)
  expect_equal(fl$stats$severity, "coverage cliff")
})

# --- vintage guard: post-coverage days are scrape vintage, not undercount ---

test_that("days after the coverage end are excluded and marked truncated", {
  parl <- mk_parl("P1", "2000-01-01", "2000-12-31")
  # Everyone seated through June, nobody after (all memberships closed).
  dc <- mk_daily("2000-01-01", c(rep(0, 182), rep(1.0, 184)))

  # Without the guard this reads as a catastrophic undercount...
  fl <- uc_run(dc, parl)
  expect_true(fl$stats$flagged)

  # ...with it, the post-vintage days vanish and the legislature is clean.
  fl <- uc_run(dc, parl, coverage_end = as.Date("2000-06-30"))
  expect_false(fl$stats$flagged)
  expect_true(fl$stats$truncated)
  expect_equal(fl$stats$n_days, 182)
})

# --- legislatures with too few covered days are not scored ---

test_that("min_days keeps sliver legislatures out of the denominator", {
  parl <- mk_parl(c("P1", "P2"),
                  c("2000-01-01", "2001-01-01"),
                  c("2000-12-31", "2001-12-31"))
  # Daily data covers all of P1 but only 10 days of P2.
  dc <- mk_daily("2000-01-01", rep(0, 376))
  stats <- undercount_legislature_stats(undercount_daily(dc), parl)
  expect_equal(stats$evaluated, c(TRUE, FALSE))
})

# --- undercount_structural evaluator end-to-end ---

test_that("undercount_structural reports flagged/evaluated with tooltip detail", {
  PARL <- mk_parl(c("P1", "P2"),
                  c("2000-01-01", "2001-01-01"),
                  c("2000-12-31", "2001-12-31"))
  RESE <- data.frame(
    pers_id = "XX_A_1", country_abb = "XX", political_function = MP,
    start_date = as.Date("2000-01-01"), end_date = as.Date(NA),
    stringsAsFactors = FALSE)
  dc <- rbind(mk_daily("2000-01-01", rep(0, 366)),
              mk_daily("2001-01-01", rep(0.10, 365)))
  ctx <- make_ctx(RESE = RESE, PARL = PARL, daily_counts_fn = function(cc) dc)
  g <- goal("g", "RESE", "under", "undercount_structural", "country", NA)

  res <- evaluate_goal(g, "XX", ctx)
  expect_false(res$pass)
  expect_equal(res$value, 1)
  expect_equal(res$display, "1/2 parl")
  expect_match(res$tooltip, "P2 \\(chronic 10\\.0%")
  expect_match(res$tooltip, "structural")

  # No daily counts at all -> n/a.
  ctx0 <- make_ctx(RESE = RESE, PARL = PARL,
                   daily_counts_fn = function(cc) NULL)
  expect_true(evaluate_goal(g, "XX", ctx0)$na)
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
