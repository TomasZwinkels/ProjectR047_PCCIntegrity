# =============================================================================
# R047 project quality goals — configuration + evaluators + rendering
# =============================================================================
# This file is the single editable place to define a project's data-quality
# goals. Each goal is a data-driven row (dataframe, label, metric, scope,
# target) dispatched to a reusable, Shiny-free evaluator. The dashboard sources
# this file and renders `quality_goals` as a goals (rows) x countries (columns)
# scorecard on the "Quality goals" tab.
#
# To change the goals for a project, edit `quality_goals_project` and
# `quality_goals` below. To add a new project later, this can be generalised to
# a named list of goal sets with a selector in the UI (out of scope for now).
#
# Pure logic only (no reactives): everything reads from an explicit `ctx` list
# so the evaluators are unit-testable with synthetic data.
# =============================================================================

# --- Project metadata + goal definitions (the editable config) --------------

quality_goals_project <- list(
  name         = "VENI",
  period_start = as.Date("1946-01-01"),
  period_end   = as.Date("2025-12-31"),
  general = c(
    "Envisioned period: 1946-01-01 – 2025-12-31",
    "97.5% of data-points available",
    "99.5% of available data-points accurate (assessed during coding, not measurable here)"
  )
)

# Constructor for one goal row. `var` is optional (some metrics need no column).
goal <- function(id, df, label, metric, scope, target, var = NA_character_) {
  list(id = id, df = df, label = label, metric = metric,
       scope = scope, target = target, var = var)
}

quality_goals <- list(
  goal("poli_last",  "POLI", "Last name available (MPs)",   "avail",      "mps_overall",    100,  var = "last_name"),
  goal("poli_first", "POLI", "First name available (MPs)",  "avail",      "mps_overall",    100,  var = "first_name"),
  goal("poli_gen",   "POLI", "Gender available (MPs)",      "avail",      "per_parliament", 97.5, var = "gender"),
  goal("poli_dob",   "POLI", "Birth year available (MPs)",  "avail_year", "per_parliament", 97.5, var = "birth_date"),
  goal("poli_bpl",   "POLI", "Birth place available (MPs)", "avail",      "per_parliament", 97.5, var = "birth_place_raw"),
  goal("poli_hard",  "POLI", "POLI hard integrity checks pass", "checks_pass",     "country", NA),
  goal("parl_hard",  "PARL", "PARL hard integrity checks pass", "checks_pass",     "country", NA),
  goal("meme_hard",  "MEME", "MEME hard integrity checks pass", "checks_pass",     "country", NA),
  goal("rese_hard",  "RESE", "RESE hard integrity checks pass", "checks_pass",     "country", NA),
  goal("rese_start", "RESE", "Start dates complete",           "date_complete",       "country", 100, var = "start_date"),
  goal("rese_end",   "RESE", "End dates complete (excl. sitting)", "date_complete_ended", "country", 100, var = "end_date"),
  goal("rese_over",  "RESE", "Overcount-free parliament-days",  "overcount_free",     "country", 99.5),
  goal("rese_under", "RESE", "No structurally undercounted parliaments", "undercount_structural", "country", NA)
)

# --- Small helpers ----------------------------------------------------------

# Availability predicate: non-NA and non-empty. For "*_year" metrics a value
# counts only when it carries a 4-digit year (birth_date is stored as a bare
# year like "1960" or a full date like "04jan1966" — both contain a year).
.qg_present <- function(x, metric) {
  if (identical(metric, "avail_year")) {
    !is.na(x) & grepl("[0-9]{4}", x)
  } else {
    !is.na(x) & x != ""
  }
}

# Ever-MP person ids for a country (canonical 4-code MP set).
.qg_ever_mp_ids <- function(RESE, cc, mp_codes) {
  unique(RESE$pers_id[RESE$country_abb == cc &
                        RESE$political_function %in% mp_codes])
}

# MP resume rows for a country.
.qg_rese_mp <- function(RESE, cc, mp_codes) {
  RESE[RESE$country_abb == cc & RESE$political_function %in% mp_codes, , drop = FALSE]
}

# Format a computed pass/fail result into the cell display string. `tooltip`
# (optional) is rendered as the cell's hover title in render_goals_table().
.qg_result <- function(value, pass, display, tooltip = NULL) {
  list(value = value, pass = pass, display = display,
       na = is.na(pass), tooltip = tooltip)
}

.qg_na_result <- function() .qg_result(NA_real_, NA, "n/a")

# --- Undercount: structural per-legislature statistics -----------------------
# Undercount is not one phenomenon. Most countries carry a "frictional" vacancy
# floor by design (seats stay empty until by-elections), so day-level
# undercounting is normal and country-specific. What we flag is the
# *structural* case: a legislature whose seated count sits well below its own
# country's typical vacancy floor, either chronically (most of the term) or
# acutely (a sustained window). Detection is per legislature; contiguous-run
# episodes are a diagnosis tool, not a detector.
#
# Thresholds are baseline-relative — the country's own median relative deficit
# encodes its by-design floor — with two absolute bounds:
#   floor: never flag deficits any country would consider frictional
#   cap:   always flag deficits this deep, even where the whole series is bad
#          (a uniformly undercounted country would otherwise mask itself,
#          since its baseline is computed from the same broken data)
undercount_config <- list(
  min_days       = 30,     # legislatures with fewer covered days are not scored
  chronic_mult   = 2,      # flag when a stat exceeds this multiple of baseline
  chronic_floor  = 0.01,   # chronic threshold never below 1% of seats
  chronic_cap    = 0.05,   # median deficit >=5% of seats always flags
  acute_window   = 90,     # days in the worst-window (acute) statistic
  acute_floor    = 0.05,   # acute threshold never below 5% of seats
  acute_cap      = 0.10,   # a 90-day window >=10% of seats always flags
  cliff_rel      = 0.5,    # relative deficit that reads as missing data
  cliff_min_days = 7       # sustained days needed for the "coverage cliff" label
)

# Vintage guard: the date after which the RESE extract carries no information.
# When open-ended memberships exist, coverage runs to "today" (return NA = no
# bound). When every membership is closed, days after the last end date would
# read as a near-100% undercount that is really the scrape vintage, so
# undercount evaluation must stop at that date.
undercount_coverage_end <- function(rese_mp) {
  if (nrow(rese_mp) == 0 || any(is.na(rese_mp$end_date))) return(as.Date(NA))
  max(rese_mp$end_date)
}

# Daily relative deficit over the days that can be evaluated: parliament size
# known, and inside the coverage window. Overcount days clip to 0 deficit.
undercount_daily <- function(dc, coverage_end = as.Date(NA)) {
  dc <- dc[!is.na(dc$parliament_size) & dc$parliament_size > 0, , drop = FALSE]
  if (!is.na(coverage_end)) dc <- dc[dc$date <= coverage_end, , drop = FALSE]
  dc$rel_deficit <- pmax(dc$parliament_size - dc$n_seated, 0) / dc$parliament_size
  dc
}

# Worst mean over any `w` consecutive values (w shrinks to length(x) if
# needed). Returns the mean plus the window's start/end indices (first window
# when the maximum repeats) so callers can locate the worst stretch.
.qg_roll_worst_window <- function(x, w) {
  n <- length(x)
  w <- min(w, n)
  cs <- cumsum(c(0, x))
  means <- (cs[(w + 1):(n + 1)] - cs[1:(n + 1 - w)]) / w
  i <- which.max(means)
  list(mean = means[i], start = i, end = i + w - 1)
}

.qg_has_cliff <- function(rel, cliff_rel, min_days) {
  r <- rle(rel >= cliff_rel)
  any(r$values & r$lengths >= min_days)
}

# Per-legislature statistics over the (already coverage-guarded) daily series:
#   chronic         median relative deficit across the legislature's covered days
#   acute           worst `acute_window`-day mean relative deficit
#   acute_start/end dates of that worst window (first window if the max repeats)
#   cliff           TRUE if a >=cliff_rel deficit held for >=cliff_min_days
#   truncated       TRUE if the legislature extends past the coverage end (its
#                   stats cover only the days up to the vintage boundary)
# leg_start/leg_end carry the legislature period through for downstream
# consumers (plot shading, drill-down windows).
undercount_legislature_stats <- function(daily, parl_cc,
                                         coverage_end = as.Date(NA),
                                         config = undercount_config) {
  parl_cc <- parl_cc[!is.na(parl_cc$leg_period_start_date) &
                       !is.na(parl_cc$leg_period_end_date), , drop = FALSE]
  if (nrow(parl_cc) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(parl_cc)), function(i) {
    ls  <- parl_cc$leg_period_start_date[i]
    le  <- parl_cc$leg_period_end_date[i]
    d   <- daily[daily$date >= ls & daily$date <= le, , drop = FALSE]
    n   <- nrow(d)
    win <- if (n > 0) .qg_roll_worst_window(d$rel_deficit, config$acute_window)
    data.frame(
      parliament_id = parl_cc$parliament_id[i],
      leg_start = ls,
      leg_end   = le,
      n_days    = n,
      evaluated = n >= config$min_days,
      chronic   = if (n > 0) stats::median(d$rel_deficit) else NA_real_,
      acute     = if (n > 0) win$mean else NA_real_,
      acute_start = if (n > 0) d$date[win$start] else as.Date(NA),
      acute_end   = if (n > 0) d$date[win$end] else as.Date(NA),
      cliff     = if (n > 0) .qg_has_cliff(d$rel_deficit, config$cliff_rel,
                                           config$cliff_min_days) else NA,
      truncated = !is.na(coverage_end) && le > coverage_end,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Flag legislatures against the country baseline (its median relative deficit,
# i.e. its by-design vacancy floor). Severity: "coverage cliff" when the
# legislature contains a sustained >=50% deficit (missing data, not vacancy),
# otherwise "structural".
undercount_flag_legislatures <- function(stats, baseline,
                                         config = undercount_config) {
  chronic_thr <- min(max(config$chronic_mult * baseline, config$chronic_floor),
                     config$chronic_cap)
  acute_thr   <- min(max(config$chronic_mult * baseline, config$acute_floor),
                     config$acute_cap)
  stats$flagged <- stats$evaluated &
    (stats$chronic > chronic_thr | stats$acute > acute_thr)
  stats$severity <- ifelse(!stats$flagged, "",
                           ifelse(stats$cliff, "coverage cliff", "structural"))
  list(stats = stats, baseline = baseline,
       chronic_thr = chronic_thr, acute_thr = acute_thr)
}

# --- Evaluator: one goal for one country ------------------------------------
# Returns list(value, pass, display, na). Reads only from `ctx`:
#   POLI, RESE, PARL            data frames (POLI has $country; RESE/PARL have
#                               $country_abb, parsed $start_date/$end_date,
#                               $leg_period_start_date)
#   mp_codes                    character vector of MP political_function codes
#   period_start, period_end    Date bounds (the project's envisioned period)
#   assembly_map                named vector country -> assembly_abb
#   build_cohort_fn(cc)         -> data.frame(parliament_id, snapshot_day,
#                                             parliament_size, pers_id)
#   daily_counts_fn(cc)         -> data.frame(date, n_seated, parliament_size)
#   checks_fns                  named list keyed by df ("POLI"/"PARL"/"MEME"/
#                               "RESE"); each fn(cc) -> list(table =
#                               data.frame(Check, Status), ...)
evaluate_goal <- function(goal, cc, ctx) {
  m      <- goal$metric
  target <- goal$target

  # ---- POLI availability, whole-MP-cohort ----
  if (m %in% c("avail", "avail_year") && goal$scope == "mps_overall") {
    ids <- .qg_ever_mp_ids(ctx$RESE, cc, ctx$mp_codes)
    poli_mp <- ctx$POLI[ctx$POLI$country == cc & ctx$POLI$pers_id %in% ids, ,
                        drop = FALSE]
    if (nrow(poli_mp) == 0) return(.qg_na_result())
    value <- 100 * mean(.qg_present(poli_mp[[goal$var]], m))
    pass  <- value >= target - 1e-9
    return(.qg_result(value, pass, sprintf("%.1f%%", value)))
  }

  # ---- POLI availability, per-parliament (pass only if ALL parliaments meet) ----
  if (m %in% c("avail", "avail_year") && goal$scope == "per_parliament") {
    ch <- ctx$build_cohort_fn(cc)
    if (is.null(ch) || nrow(ch) == 0) return(.qg_na_result())
    ch <- ch[ch$snapshot_day >= ctx$period_start &
               ch$snapshot_day <= ctx$period_end, , drop = FALSE]
    if (nrow(ch) == 0) return(.qg_na_result())
    poli_var <- ctx$POLI[, c("pers_id", goal$var)]
    ch <- merge(ch, poli_var, by = "pers_id", all.x = TRUE)
    ch$present <- .qg_present(ch[[goal$var]], m)
    pct <- tapply(ch$present, ch$parliament_id, function(v) 100 * mean(v))
    min_pct <- min(pct)
    n_total <- length(pct)
    n_fail  <- sum(pct < target - 1e-9)
    pass    <- n_fail == 0
    display <- if (pass) {
      sprintf("%.1f%% min", min_pct)
    } else {
      sprintf("%.1f%% min (%d/%d parl fail)", min_pct, n_fail, n_total)
    }
    return(.qg_result(min_pct, pass, display))
  }

  # ---- Hard integrity checks pass (per dataframe, dispatched on goal$df) ----
  if (m == "checks_pass") {
    checks_fn <- ctx$checks_fns[[goal$df]]
    if (is.null(checks_fn)) return(.qg_na_result())
    res <- checks_fn(cc)
    st  <- res$table$Status
    if (length(st) == 0) return(.qg_na_result())
    n_pass <- sum(st == "PASS")
    n      <- length(st)
    pass   <- n_pass == n
    return(.qg_result(100 * n_pass / n, pass, sprintf("%d/%d", n_pass, n)))
  }

  # ---- RESE: start-date completeness (all MP episodes) ----
  if (m == "date_complete") {
    rese_mp <- .qg_rese_mp(ctx$RESE, cc, ctx$mp_codes)
    if (nrow(rese_mp) == 0) return(.qg_na_result())
    value <- 100 * mean(!is.na(rese_mp[[goal$var]]))
    pass  <- value >= target - 1e-9
    return(.qg_result(value, pass, sprintf("%.1f%%", value)))
  }

  # ---- RESE: end-date completeness, excluding still-sitting memberships ----
  # A blank end date is a violation only when the membership's parliamentary
  # term has already ended, i.e. its start precedes the country's most-recent
  # leg_period_start_date. Episodes in the currently-sitting term (start >=
  # latest term start) are legitimately open and excluded from the denominator.
  if (m == "date_complete_ended") {
    rese_mp <- .qg_rese_mp(ctx$RESE, cc, ctx$mp_codes)
    if (nrow(rese_mp) == 0) return(.qg_na_result())
    parl_cc <- ctx$PARL[ctx$PARL$country_abb == cc &
                          ctx$PARL$level == "NT" &
                          ctx$PARL$assembly_abb == ctx$assembly_map[[cc]], ,
                        drop = FALSE]
    latest_term_start <- suppressWarnings(
      max(parl_cc$leg_period_start_date, na.rm = TRUE))
    if (!is.finite(latest_term_start)) return(.qg_na_result())

    ended <- !is.na(rese_mp$start_date) &
      rese_mp$start_date < latest_term_start
    denom <- sum(ended)
    if (denom == 0) return(.qg_na_result())
    violations <- sum(ended & is.na(rese_mp$end_date))
    value <- 100 * (1 - violations / denom)
    pass  <- value >= target - 1e-9
    return(.qg_result(value, pass, sprintf("%.1f%%", value)))
  }

  # ---- RESE: overcount-free parliament-days ----
  if (m == "overcount_free") {
    dc <- ctx$daily_counts_fn(cc)
    if (is.null(dc) || nrow(dc) == 0) return(.qg_na_result())
    dc <- dc[!is.na(dc$parliament_size) &
               dc$date >= ctx$period_start & dc$date <= ctx$period_end, ,
             drop = FALSE]
    if (nrow(dc) == 0) return(.qg_na_result())
    value <- 100 * mean(dc$n_seated <= dc$parliament_size)
    pass  <- value >= target - 1e-9
    return(.qg_result(value, pass, sprintf("%.2f%%", value)))
  }

  # ---- RESE: no structurally undercounted parliaments ----
  # Per-legislature detection (chronic median + acute worst-window deficit)
  # against the country's own vacancy baseline; see undercount_config above.
  # Cell shows flagged/evaluated legislatures; hover lists the flagged ones.
  if (m == "undercount_structural") {
    dc <- ctx$daily_counts_fn(cc)
    if (is.null(dc) || nrow(dc) == 0) return(.qg_na_result())
    dc <- dc[dc$date >= ctx$period_start & dc$date <= ctx$period_end, ,
             drop = FALSE]
    coverage_end <- undercount_coverage_end(
      .qg_rese_mp(ctx$RESE, cc, ctx$mp_codes))
    daily <- undercount_daily(dc, coverage_end)
    if (nrow(daily) == 0) return(.qg_na_result())

    parl_cc <- ctx$PARL[ctx$PARL$country_abb == cc &
                          ctx$PARL$level == "NT" &
                          ctx$PARL$assembly_abb == ctx$assembly_map[[cc]], ,
                        drop = FALSE]
    stats <- undercount_legislature_stats(daily, parl_cc, coverage_end)
    if (is.null(stats) || !any(stats$evaluated)) return(.qg_na_result())

    fl <- undercount_flag_legislatures(stats, stats::median(daily$rel_deficit))
    st <- fl$stats[fl$stats$evaluated, , drop = FALSE]
    n_flag <- sum(st$flagged)
    pct_line <- function(x) sprintf("%.1f%%", 100 * x)
    tooltip <- paste0(
      "Baseline (median rel. deficit): ", pct_line(fl$baseline),
      " | thresholds: chronic >", pct_line(fl$chronic_thr),
      ", acute(", undercount_config$acute_window, "d) >",
      pct_line(fl$acute_thr),
      if (!is.na(coverage_end) && coverage_end < ctx$period_end)
        paste0(" | data coverage ends ", format(coverage_end)) else "",
      if (n_flag > 0) paste0(
        "\nFlagged: ",
        paste(sprintf("%s (chronic %s, acute %s, %s%s)",
                      st$parliament_id[st$flagged],
                      pct_line(st$chronic[st$flagged]),
                      pct_line(st$acute[st$flagged]),
                      st$severity[st$flagged],
                      ifelse(st$truncated[st$flagged],
                             ", coverage-truncated", "")),
              collapse = "; ")) else "")
    return(.qg_result(n_flag, n_flag == 0,
                      sprintf("%d/%d parl", n_flag, nrow(st)), tooltip))
  }

  stop(sprintf("evaluate_goal: unknown metric '%s'", m))
}

# --- Build the full matrix: one row per goal, one entry per country ----------
# Returns a list of rows; each row = list(goal = <goal>, cells = named list of
# per-country evaluate_goal() results, keyed by country code).
build_goals_matrix <- function(goals, countries, ctx) {
  lapply(goals, function(g) {
    cells <- lapply(countries, function(cc) {
      tryCatch(evaluate_goal(g, cc, ctx),
               error = function(e) .qg_na_result())
    })
    names(cells) <- countries
    list(goal = g, cells = cells)
  })
}

# --- Render the scorecard as an htmltools tagList ----------------------------
# `matrix` is the output of build_goals_matrix. `country_labels` is an optional
# named vector (code -> display name); codes are used when a label is missing.
render_goals_table <- function(project, goals, matrix, countries,
                               country_labels = NULL) {
  cell_bg <- function(res) {
    if (isTRUE(res$na) || is.na(res$pass)) "#eeeeee"
    else if (isTRUE(res$pass)) "#d4edda" else "#f8d7da"
  }
  cell_mark <- function(res) {
    if (isTRUE(res$na) || is.na(res$pass)) ""
    else if (isTRUE(res$pass)) " ✓" else " ✗"
  }
  col_label <- function(cc) {
    lab <- if (!is.null(country_labels)) country_labels[[cc]] else NULL
    if (is.null(lab) || is.na(lab)) cc else lab
  }

  th <- function(txt, extra = "") htmltools::tags$th(
    style = paste0("text-align:left; padding:6px 10px; border-bottom:2px solid #ccc;",
                   " font-size:0.9em;", extra), txt)

  header <- htmltools::tags$tr(
    th("Goal"),
    lapply(countries, function(cc) th(col_label(cc), " text-align:center;")))

  body <- lapply(matrix, function(row) {
    g <- row$goal
    target_txt <- if (is.na(g$target)) "all pass" else {
      if (g$target == 100) "100%" else sprintf("≥%.1f%%", g$target)
    }
    label_cell <- htmltools::tags$td(
      style = "padding:6px 10px; border-bottom:1px solid #eee;",
      htmltools::tags$span(g$label),
      htmltools::tags$span(
        style = "color:#888; font-size:0.8em; margin-left:6px;",
        paste0("(", g$df, ", target ", target_txt, ")")))
    value_cells <- lapply(countries, function(cc) {
      res <- row$cells[[cc]]
      htmltools::tags$td(
        style = paste0("padding:6px 10px; border-bottom:1px solid #eee;",
                       " text-align:center; font-family:monospace; font-size:0.85em;",
                       " background:", cell_bg(res), ";"),
        title = res$tooltip,
        paste0(res$display, cell_mark(res)))
    })
    htmltools::tags$tr(label_cell, value_cells)
  })

  legend <- htmltools::tags$p(
    style = "font-size:0.82em; color:#555; margin-top:8px;",
    htmltools::tags$span(style = "background:#d4edda; padding:1px 6px; margin-right:4px;", "✓ meets target"),
    htmltools::tags$span(style = "background:#f8d7da; padding:1px 6px; margin-right:4px;", "✗ misses target"),
    htmltools::tags$span(style = "background:#eeeeee; padding:1px 6px;", "n/a (no data)"))

  footnote <- htmltools::tags$div(
    style = "font-size:0.8em; color:#888; margin-top:2px;",
    htmltools::tags$p(
      style = "margin:0 0 4px;",
      htmltools::tags$b("Reading per-parliament goals (birth year, birth place): "),
      "these are scored one parliament at a time, and pass only if ",
      htmltools::tags$i("every"),
      " parliament in the envisioned period meets the threshold. The cell shows the ",
      htmltools::tags$b("worst (minimum) parliament"),
      ", not an overall average — so ",
      htmltools::tags$code("37.6% min (7/25 parl fail)"),
      " means the weakest of the country's 25 parliaments sits at 37.6%, and 7 of the 25 fall below target ",
      "(one failing parliament is enough to fail the goal). Country-wide MP availability is typically much higher."),
    htmltools::tags$p(
      style = "margin:0 0 4px;",
      htmltools::tags$b("Reading hard-check rows: "),
      htmltools::tags$code("k/n"),
      " is the number of that dataframe's integrity checks passing (matching its tab); the row is green only at ",
      htmltools::tags$code("n/n"), "."),
    htmltools::tags$p(
      style = "margin:0 0 4px;",
      htmltools::tags$b("Reading the undercount row: "),
      htmltools::tags$code("k/n parl"),
      " means k of the n scored legislatures are structurally undercounted — their median (chronic) ",
      "or worst-90-day (acute) seat deficit exceeds the country's own vacancy baseline ",
      "(frictional vacancy between by-elections is by design and never flags). ",
      "Hover a cell for the baseline, thresholds, and the flagged legislatures with severity ",
      "(", htmltools::tags$i("coverage cliff"), " = ≥50% of seats missing for ≥7 days, i.e. missing data). ",
      "Days after the country's last recorded membership end date are excluded as scrape vintage, not undercount."),
    htmltools::tags$p(
      style = "margin:0;",
      "Evaluated over ", format(project$period_start), " – ",
      format(project$period_end),
      " (the project's envisioned period), independent of the date filter above."))

  htmltools::tagList(
    htmltools::tags$h4(paste0("Project quality goals — ", project$name)),
    htmltools::tags$ul(
      style = "font-size:0.9em; color:#444;",
      lapply(project$general, function(g) htmltools::tags$li(g))),
    htmltools::tags$table(
      style = "border-collapse:collapse; margin-top:8px; min-width:60%;",
      htmltools::tags$thead(header),
      htmltools::tags$tbody(body)),
    legend,
    footnote
  )
}
