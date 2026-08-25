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
  goal("poli_dob",   "POLI", "Birth year available (MPs)",  "avail_year", "per_parliament", 97.5, var = "birth_date"),
  goal("poli_bpl",   "POLI", "Birth place available (MPs)", "avail",      "per_parliament", 97.5, var = "birth_place_raw"),
  goal("poli_hard",  "POLI", "POLI hard integrity checks pass", "checks_pass",     "country", NA),
  goal("parl_hard",  "PARL", "PARL hard integrity checks pass", "checks_pass",     "country", NA),
  goal("meme_hard",  "MEME", "MEME hard integrity checks pass", "checks_pass",     "country", NA),
  goal("rese_hard",  "RESE", "RESE hard integrity checks pass", "checks_pass",     "country", NA),
  goal("rese_start", "RESE", "Start dates complete",           "date_complete",       "country", 100, var = "start_date"),
  goal("rese_end",   "RESE", "End dates complete (excl. sitting)", "date_complete_ended", "country", 100, var = "end_date"),
  goal("rese_over",  "RESE", "Overcount-free parliament-days",  "overcount_free",     "country", 99.5)
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

# Format a computed pass/fail result into the cell display string.
.qg_result <- function(value, pass, display) {
  list(value = value, pass = pass, display = display,
       na = is.na(pass))
}

.qg_na_result <- function() .qg_result(NA_real_, NA, "n/a")

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
