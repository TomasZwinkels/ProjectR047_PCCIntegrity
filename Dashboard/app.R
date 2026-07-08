for (pkg in c("shiny", "dplyr", "ggplot2", "DT", "testthat")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_MEME_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_POLI_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/R047_dashboard_functions.R")

test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_MEME_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_POLI_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/R047_dashboard_unittests.R")
# test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/R047_dashboard_slow_unittests.R") # Calls external services (Codex LLM, GitHub API), so is slow, so commented out by default.

# Load data once at startup
POLI <- read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";") |>
  mutate(country = substr(pers_id, 1, 2))

RESE <- read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL <- read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
MEME <- read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
PART <- read.csv("/home/tomas/projects/PCCdata/PART.csv", header = TRUE, sep = ";")

# Preprocess dates for the full datasets
RESE <- suppressMessages(preprocess_RESEdates(RESE)) |>
  mutate(
    start_date = as.Date(res_entry_start_posoxctformat),
    end_date   = as.Date(res_entry_end_posoxctformat)
  )

PARL <- suppressMessages(preprocess_PARLdates(PARL)) |>
  mutate(
    leg_period_start_date = as.Date(leg_period_start_posoxctformat),
    leg_period_end_date   = as.Date(leg_period_end_posoxctformat)
  )
MEME <- suppressMessages(preprocess_MEMEdates(MEME))

assembly_map <- c(CA = "HC", CH = "NR", DE = "BT", NL = "TK", NO = "ST", US = "HR")
all_countries <- sort(intersect(names(assembly_map), unique(POLI$country)))

defaults_file <- "/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/defaults.rds"
saved <- if (file.exists(defaults_file)) {
  readRDS(defaults_file)
} else {
  list(country = all_countries[1], date_from = as.Date("1946-01-01"),
       date_to = as.Date("2025-12-31"), tab = "RESE_MP")
}

poli_base_vars <- c("last_name", "first_name", "birth_date", "death_date", "birth_place_raw", "wikidata_id")

country_labels <- c(
  CA = "Canada", CH = "Switzerland", DE = "Germany",
  NL = "Netherlands", NO = "Norway", US = "United States"
)

# Build first-day cohort for a country using the official parliament start date
build_cohort <- function(country_code) {
  if (!country_code %in% names(assembly_map)) return(NULL)

  rese <- RESE |>
    filter(
      country_abb == country_code,
      political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")
    )

  parl <- PARL |>
    filter(
      country_abb == country_code,
      level == "NT",
      assembly_abb == assembly_map[[country_code]]
    ) |>
    arrange(leg_period_start_date)

  if (nrow(parl) == 0 || nrow(rese) == 0) return(NULL)

  cohort_list <- lapply(seq_len(nrow(parl)), function(i) {
    snapshot_day <- parl$leg_period_start_date[i]
    if (is.na(snapshot_day)) return(NULL)

    seated <- rese |>
      filter(start_date <= snapshot_day & (is.na(end_date) | end_date >= snapshot_day))

    if (nrow(seated) == 0) return(NULL)

    data.frame(
      parliament_id   = parl$parliament_id[i],
      snapshot_day    = snapshot_day,
      parliament_size = as.numeric(parl$parliament_size[i]),
      pers_id         = unique(seated$pers_id),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), cohort_list))
}

# Day-by-day seated MP counts for a country (full data range, cached to disk)
cache_dir <- "/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard/cache"

build_daily_counts <- function(cc) {
  rese_cc <- RESE[RESE$country_abb == cc &
                    RESE$political_function %in%
                      c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09"), ]
  parl_cc <- PARL[PARL$country_abb == cc & PARL$level == "NT" &
                    PARL$assembly_abb == assembly_map[[cc]], ]
  parl_cc <- parl_cc[order(parl_cc$leg_period_start_date), ]

  if (nrow(parl_cc) == 0 || nrow(rese_cc) == 0) {
    return(data.frame(date = as.Date(character(0)),
                      n_seated = integer(0),
                      parliament_size = integer(0)))
  }

  date_seq <- seq(min(parl_cc$leg_period_start_date, na.rm = TRUE),
                  max(c(rese_cc$end_date[!is.na(rese_cc$end_date)], Sys.Date())),
                  by = "day")

  rese_start <- rese_cc$start_date
  rese_end   <- rese_cc$end_date

  n_seated <- vapply(date_seq, function(d) {
    sum(rese_start <= d & (is.na(rese_end) | rese_end >= d), na.rm = TRUE)
  }, integer(1))

  idx <- findInterval(as.numeric(date_seq), as.numeric(parl_cc$leg_period_start_date))
  parliament_size <- ifelse(idx == 0L, NA_integer_,
                            as.integer(parl_cc$parliament_size[idx]))

  data.frame(date = date_seq, n_seated = n_seated,
             parliament_size = parliament_size)
}

get_daily_counts <- function(cc, force = FALSE) {
  cache_rds <- file.path(cache_dir, paste0("daily_counts_", cc, ".rds"))
  cache_ver <- file.path(cache_dir, paste0("daily_counts_", cc, "_version.txt"))
  data_version <- trimws(readLines(
    "/home/tomas/projects/PCCdata/dataversion.txt")[1])

  if (!force && file.exists(cache_rds) && file.exists(cache_ver) &&
      trimws(readLines(cache_ver)[1]) == data_version) {
    return(readRDS(cache_rds))
  }

  result <- build_daily_counts(cc)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  saveRDS(result, cache_rds)
  writeLines(data_version, cache_ver)
  result
}

# Verified same-birthday pairs that are genuinely different people (from R047.R)
verified_not_duplicates <- data.frame(
  pers_id_1 = c("NL_Suurhoff_Ko_1905", "NL_vanBuel_Ben_1913",
                "NL_Pronk_Jan_1940", "NL_Esselink_Berry_1944",
                "NL_vanderWal_Christianne_1973"),
  pers_id_2 = c("NL_Venverloo_Albert_1905", "NL_Walburg_Tjebbe_1913",
                "NL_Schaefer_Jan_1940", "NL_vanVoorsttotVoorst_Berend_1944",
                "NL_Veltman_Hester_1973"),
  stringsAsFactors = FALSE
)

checks_table <- function(labels, results) {
  data.frame(
    Check  = labels,
    Status = ifelse(results, "PASS", "FAIL"),
    check.names = FALSE
  )
}

run_rese_checks <- function(cc, date_from, date_to) {
  rese_mp <- suppressMessages(preprocess_RESEdates(
    RESE[RESE$country_abb == cc, ]
  ))
  rese_mp <- rese_mp[rese_mp$political_function %in%
                       c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09"), ]

  labels <- c(
    "All RESE person IDs exist in POLI",
    "All resume entry IDs are unique",
    "All RESE dates parsed successfully",
    "No fully overlapping parl. episodes",
    "No near-overlapping episodes (\u22642 days)",
    "No same-birthday duplicates in factions",
    "All parliaments in date range have membership data",
    paste0("\u22651 seated MP in RESE on date_from (", format(date_from, "%Y-%m-%d"),
           ") \u2014 detects gap from parliament active before range start"),
    paste0("\u22651 seated MP in RESE on date_to (", format(date_to, "%Y-%m-%d"),
           ") \u2014 detects gap from parliament active at range end")
  )
  details <- list(
    check_RESE_persid_in_POLI_details(rese_mp, POLI),
    check_RESE_resentryid_unique_details(rese_mp),
    check_anyNAinRESEdates_details(rese_mp),
    check_RESE_parlmemeppisodes_anyfulloverlap_details(rese_mp),
    check_RESE_anynear_fulloverlap_details(rese_mp, tolerance_days = 2),
    check_RESE_duplicate_birthdates_in_faction_details(
      rese_mp, POLI, PARL, MEME, assembly_map[[cc]],
      verified_pairs = verified_not_duplicates),
    check_RESE_parlmem_coverage_details(
      rese_mp, PARL, assembly_map[[cc]], date_from, date_to),
    check_RESE_coverage_at_date_details(rese_mp, date_from),
    check_RESE_coverage_at_date_details(rese_mp, date_to)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

rese_detail_keys <- c(
  "missing_rows", "duplicate_rows", "full_rows_with_na_dates",
  "overlapping_episodes", "full_episode_pairs_near_overlapping", "flagged_pairs",
  "parliaments_no_data", "boundary_episodes", "boundary_episodes"
)

# Render the issue path with an "Open new issue" button and inline form.
# table_key: non-NULL when the panel has a problem table that can be uploaded
# as a CSV attachment; the key selects the table in issue_table_sources.
# plot_key: non-NULL when a graph on the panel's tab can be attached; the key
# selects the plot in issue_plot_sources.
issue_path_tag <- function(path, auto_summary = "", plot_key = NULL,
                           repo = github_defaults$repo, table_key = NULL) {
  form_id <- gsub("[^a-zA-Z0-9]", "_", path)
  path_js <- gsub("'", "\\\\'", path)
  settings_id <- paste0(form_id, "_settings")
  auto_id <- paste0(form_id, "_auto")

  # table_key may be a single registry key (historical: one CSV, no filename
  # suffix) or a NAMED vector where names are per-file suffixes and values are
  # registry keys (e.g. c(peak = "overcount_peak")). Flatten to a "key:suffix;..."
  # spec string the post handler splits; unnamed keys get an empty suffix.
  has_table   <- !is.null(table_key) && length(table_key) > 0
  n_tables    <- if (has_table) length(table_key) else 0
  table_specs <- if (has_table) {
    sfx <- names(table_key)
    if (is.null(sfx)) sfx <- rep("", n_tables)
    sfx[is.na(sfx)] <- ""
    paste0(unname(table_key), ":", sfx, collapse = ";")
  } else ""

  labels <- issue_path_to_labels(path)
  label_names <- c("country", "dataframe", "type", "identifier")
  issues_div_id <- paste0(form_id, "_issues")

  # Build label checkboxes with a JS function to re-query
  refresh_js <- sprintf(paste0(
    "var cbs = document.querySelectorAll('.%s_label_cb'); ",
    "var sel = []; ",
    "cbs.forEach(function(cb){ if(cb.checked) sel.push(cb.value); }); ",
    "Shiny.setInputValue('refresh_issues', {",
    "labels: sel, div_id: '%s', repo: document.getElementById('%s_repo').value || '%s', ",
    "nonce: Math.random()});"
  ), form_id, issues_div_id, settings_id, repo)

  label_checkboxes <- lapply(seq_along(labels), function(i) {
    cb_class <- paste0(form_id, "_label_cb")
    tags$label(
      style = "font-size:0.85em; color:#555; cursor:pointer; margin-right:10px;",
      tags$input(
        type = "checkbox",
        class = cb_class,
        value = labels[i],
        checked = "checked",
        style = "margin-right:3px;",
        onchange = refresh_js
      ),
      tags$span(style = "font-family:monospace;", labels[i])
    )
  })

  # Initial query with all labels
  existing <- gh_list_issues(repo, labels = labels)

  tags$div(
    tags$div(
      style = "font-family:monospace; font-size:0.9em; color:#555; background:#f0f0f0; padding:4px 8px; border-left:3px solid #2874a6; margin-top:8px; margin-bottom:4px;",
      tags$span(path)
    ),
    tags$div(
      style = "margin-top:4px; margin-bottom:4px;",
      tags$span(style = "font-size:0.8em; color:#888; margin-right:6px;", "Filter labels:"),
      label_checkboxes
    ),
    tags$div(id = issues_div_id),
    tags$script(HTML(sprintf(
      "Shiny.setInputValue('refresh_issues', {labels: %s, div_id: '%s', repo: '%s', nonce: Math.random()});",
      jsonlite::toJSON(labels), issues_div_id, repo
    ))),
    tags$button(
      "Open new issue",
      class = "btn btn-sm btn-outline-primary",
      style = "font-size:0.85em; margin-top:4px; margin-bottom:4px;",
      onclick = sprintf(
        "var el = document.getElementById('%s'); el.style.display = el.style.display === 'none' ? 'block' : 'none';",
        form_id
      )
    ),
    tags$div(
      id = form_id,
      style = "display:none; margin-top:8px; padding:10px; background:#fafafa; border:1px solid #ddd; border-radius:4px;",
      tags$label("Title:", style = "font-weight:bold; display:block; margin-bottom:4px;"),
      tags$input(
        id = paste0(form_id, "_title"),
        type = "text",
        value = path,
        style = "width:100%; padding:6px; border:1px solid #ccc; border-radius:3px; font-size:0.9em; margin-bottom:8px;"
      ),
      tags$label("Description:", style = "font-weight:bold; display:block; margin-bottom:4px;"),
      tags$textarea(
        id = paste0(form_id, "_text"),
        rows = "4",
        style = "width:100%; padding:6px; border:1px solid #ccc; border-radius:3px; font-size:0.9em;",
        placeholder = "Describe the issue..."
      ),
      tags$label("Technical details:", style = "font-weight:bold; display:block; margin-top:8px; margin-bottom:4px; color:#666;"),
      tags$textarea(
        id = auto_id,
        rows = "6",
        auto_summary,
        style = "width:100%; padding:6px; border:1px solid #ccc; border-radius:3px; font-size:0.85em; font-family:monospace; background:#f9f9f9; color:#444;"
      ),
      if (!is.null(plot_key)) tags$div(
        style = "margin-top:6px;",
        tags$label(
          style = "font-size:0.9em; color:#555; cursor:pointer;",
          tags$input(
            type = "checkbox",
            id = paste0(form_id, "_attach_plot"),
            checked = "checked",
            style = "margin-right:5px;"
          ),
          "Attach the graph shown above"
        )
      ),
      if (has_table) tags$div(
        style = "margin-top:6px;",
        tags$label(
          style = "font-size:0.9em; color:#555; cursor:pointer;",
          tags$input(
            type = "checkbox",
            id = paste0(form_id, "_attach_table"),
            checked = "checked",
            style = "margin-right:5px;"
          ),
          if (n_tables > 1)
            sprintf("Attach %d problem tables (CSV, all rows)", n_tables)
          else "Attach problem table (CSV, shown columns, all rows)"
        )
      ),
      tags$div(
        style = "margin-top:8px; display:flex; align-items:center; gap:8px;",
        tags$button(
          "Generate title and description with AI",
          class = "btn btn-sm btn-outline-info",
          title = "Use LLM to generate a title and description",
          onclick = sprintf(
            paste0(
              "this.disabled = true; this.innerText = 'Generating...'; ",
              "Shiny.setInputValue('llm_generate', {",
              "path: '%s', ",
              "auto_summary: document.getElementById('%s').value, ",
              "title_id: '%s_title', ",
              "desc_id: '%s_text', ",
              "plot_key: '%s', ",
              "btn_id: this.id, ",
              "nonce: Math.random()});"
            ),
            path_js, auto_id, form_id, form_id,
            if (is.null(plot_key)) "" else plot_key
          ),
          id = paste0(form_id, "_ai_btn")
        ),
        tags$button(
          "Post issue on GitHub",
          class = "btn btn-sm btn-success",
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue('post_github_issue', {",
              "path: '%s', ",
              "title: document.getElementById('%s_title').value, ",
              "description: document.getElementById('%s_text').value + ",
              "'\\n\\n---\\n\\n' + document.getElementById('%s').value, ",
              "repo: document.getElementById('%s_repo').value, ",
              "asset_repo: document.getElementById('%s_asset_repo').value, ",
              "has_plot: (function(){ var cb = document.getElementById('%s_attach_plot'); return cb ? cb.checked : false; })(), ",
              "plot_key: '%s', ",
              "table_keys: '%s', ",
              "attach_table: (function(){ var cb = document.getElementById('%s_attach_table'); return cb ? cb.checked : false; })(), ",
              "nonce: Math.random()});"
            ),
            path_js, form_id, form_id, auto_id, settings_id, settings_id,
            form_id, if (is.null(plot_key)) "" else plot_key,
            table_specs, form_id
          ),
          id = paste0(form_id, "_post_btn")
        ),
        tags$button(
          "\u2699",
          class = "btn btn-sm btn-outline-secondary",
          title = "GitHub settings",
          style = "font-size:1.1em; padding:2px 8px;",
          onclick = sprintf(
            "var el = document.getElementById('%s'); el.style.display = el.style.display === 'none' ? 'block' : 'none';",
            settings_id
          )
        )
      ),
      tags$div(
        id = settings_id,
        style = "display:none; margin-top:8px; padding:8px; background:#f5f5f5; border:1px solid #e0e0e0; border-radius:3px;",
        tags$label("Issue repository:", style = "font-weight:bold; font-size:0.85em; display:block; margin-bottom:4px;"),
        tags$input(
          id = paste0(settings_id, "_repo"),
          type = "text",
          value = github_defaults$repo,
          style = "width:100%; padding:4px 6px; border:1px solid #ccc; border-radius:3px; font-size:0.85em; font-family:monospace; margin-bottom:6px;"
        ),
        tags$label("Image asset repository:", style = "font-weight:bold; font-size:0.85em; display:block; margin-bottom:4px;"),
        tags$input(
          id = paste0(settings_id, "_asset_repo"),
          type = "text",
          value = github_defaults$asset_repo,
          style = "width:100%; padding:4px 6px; border:1px solid #ccc; border-radius:3px; font-size:0.85em; font-family:monospace;"
        )
      )
    )
  )
}

run_parl_checks <- function(cc) {
  parl <- suppressMessages(preprocess_PARLdates(
    PARL[PARL$country_abb == cc, ]
  ))
  labels <- c(
    "All PARL dates parsed successfully",
    "All parliament sizes are valid"
  )
  details <- list(
    check_anyNAinPARLdates_details(parl, level = "NT"),
    check_PARL_parliament_size_meaningful_details(parl, level = "NT")
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

parl_detail_keys <- c("full_rows_with_na_dates", "full_rows_with_problems")

run_meme_checks <- function(cc, date_from, date_to) {
  meme <- suppressMessages(preprocess_MEMEdates(
    MEME[substr(MEME$pers_id, 1, nchar(cc)) == cc, ]
  ))
  rese_mp <- RESE[RESE$country_abb == cc &
                    RESE$political_function %in%
                      c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09"), ]
  # For check #7 (party coverage): only MPs active within the date range
  rese_mp_in_range <- rese_mp[
    rese_mp$start_date <= date_to &
      (is.na(rese_mp$end_date) | rese_mp$end_date >= date_from), ]

  labels <- c(
    "All MEME person IDs exist in POLI",
    "All MEME party IDs exist in PART",
    "All MEME episode IDs are unique",
    "All MEME start dates parsed successfully",
    "No inverted MEME dates",
    "No duplicate MEME episodes",
    "All MPs have party membership data",
    "No same-party MEME overlaps during parliamentary service",
    "No different-party MEME overlaps during parliamentary service"
  )
  details <- list(
    check_MEME_persid_in_POLI_details(meme, POLI),
    check_MEME_partyid_in_PART_details(meme, PART),
    check_MEME_memepid_unique_details(meme),
    check_anyNAinMEMEdates_details(meme),
    check_MEME_inverted_dates_details(meme),
    check_MEME_anyfulloverlap_details(meme),
    check_MEME_parlmembers_have_party_details(rese_mp_in_range, meme),
    check_MEME_same_party_overlap_during_parliament_details(meme, rese_mp_in_range),
    check_MEME_diff_party_overlap_during_parliament_details(meme, rese_mp_in_range)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

meme_detail_keys <- c(
  "missing_rows", "missing_rows", "duplicate_rows",
  "full_rows_with_na_startdates", "inverted_rows",
  "overlapping_episodes", "missing_rese_rows",
  "overlapping_rows", "overlapping_rows"
)

run_poli_checks <- function(cc) {
  poli_cc <- POLI[POLI$country == cc, ]
  labels <- c(
    "All POLI person IDs are unique",
    "POLI birth dates not over-concentrated on 01-Jan"
  )
  details <- list(
    check_POLI_persid_unique_details(poli_cc),
    check_POLI_birthdate_jan01_excess_details(poli_cc)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

poli_check_detail_keys <- c("duplicate_rows", "jan01_rows")

checks_dt <- function(df) {
  DT::datatable(
    df,
    selection = "single",
    rownames  = FALSE,
    caption   = htmltools::tags$small(
      style = "color:#666;",
      "Click a FAIL row for details."
    ),
    options   = list(dom = "t", ordering = FALSE, paging = FALSE)
  ) |>
    DT::formatStyle(
      "Status",
      backgroundColor = DT::styleEqual(c("PASS", "FAIL"), c("#d4edda", "#f8d7da"))
    )
}

# Build the header tagList for a detail panel; the DT itself is injected by
# renderDT. cols_input_id/col_choices/col_selected add a column-selector above
# the table (curated default view; the user can add hidden columns back).
detail_header_ui <- function(result, row_idx, key_vec, dt_output_id,
                             issue_path_str = NULL, download_id = NULL,
                             cols_input_id = NULL, col_choices = NULL,
                             col_selected = NULL, table_key = NULL,
                             plot_key = NULL) {
  status <- result$table$Status[row_idx]
  label  <- result$table$Check[row_idx]

  auto_summary <- if (!is.null(issue_path_str)) {
    build_check_summary(result, row_idx, key_vec)
  } else ""

  det <- result$details[[row_idx]]
  df  <- det[[key_vec[row_idx]]]
  n   <- if (!is.null(df)) nrow(df) else 0

  path_tag <- if (!is.null(issue_path_str)) {
    issue_path_tag(issue_path_str, auto_summary,
                   # CSV attachment only offered when there is a table to attach
                   table_key = if (status == "FAIL" && n > 0) table_key,
                   plot_key  = plot_key)
  }

  # Key facts exposed by the check (e.g. data-boundary statistics) — shown
  # even when the problem table has no rows.
  stats_tag <- if (!is.null(det$summary_stats) && length(det$summary_stats) > 0) {
    tags$ul(
      style = "font-size:0.9em; color:#444; margin-top:4px;",
      lapply(seq_along(det$summary_stats), function(k) {
        tags$li(tags$b(paste0(names(det$summary_stats)[k], ": ")),
                unname(det$summary_stats[k]))
      })
    )
  }

  if (status == "PASS") {
    return(tagList(
      tags$hr(),
      tags$p(style = "color:#28a745; font-weight:bold;",
             paste0("\u2713 ", label, " — no issues found.")),
      path_tag
    ))
  }

  tagList(
    tags$hr(),
    tags$p(style = "font-weight:bold; color:#c0392b;",
           paste0("Details: ", label,
                  " (", n, " problem row", if (n != 1) "s", ")")),
    stats_tag,
    if (n == 0)
      tags$p(style = "color:#666;", "(No problem rows returned by details function.)")
    else
      tagList(
        if (!is.null(cols_input_id) && !is.null(col_choices))
          selectizeInput(cols_input_id, "Columns shown",
                         choices = col_choices, selected = col_selected,
                         multiple = TRUE, width = "100%",
                         options = list(plugins = list("remove_button"))),
        DT::DTOutput(dt_output_id),
        if (!is.null(download_id))
          downloadButton(download_id, "Export physical table",
                         class = "btn-sm btn-outline-secondary",
                         style = "margin-top:6px;")
      ),
    path_tag
  )
}

# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('fillField', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) el.value = msg.value;
    });
    Shiny.addCustomMessageHandler('resetButton', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) { el.disabled = false; el.innerText = msg.label; }
    });
    Shiny.addCustomMessageHandler('updateIssueList', function(msg) {
      var el = document.getElementById(msg.div_id);
      if (el) el.innerHTML = msg.html;
    });
    Shiny.addCustomMessageHandler('refreshIssueList', function(msg) {
      Shiny.setInputValue('refresh_issues', {
        labels: msg.labels, div_id: msg.div_id, repo: msg.repo,
        nonce: Math.random()
      });
    });
    Shiny.addCustomMessageHandler('markIssuePosted', function(msg) {
      var form = document.getElementById(msg.form_id);
      if (form) {
        form.style.border = '2px solid #28a745';
        form.style.background = '#f0fff4';
        var banner = document.getElementById(msg.form_id + '_posted_banner');
        if (!banner) {
          banner = document.createElement('div');
          banner.id = msg.form_id + '_posted_banner';
          banner.style.cssText = 'margin:-4px 0 10px 0; padding:8px 10px; ' +
            'background:#28a745; color:#fff; border-radius:3px; ' +
            'font-weight:bold; font-size:0.9em;';
          form.insertBefore(banner, form.firstChild);
        }
        banner.innerHTML = '✓ Issue posted' +
          (msg.number ? ' as #' + msg.number : '') +
          ' — see the open-issues list above.';
      }
      var btn = document.getElementById(msg.form_id + '_post_btn');
      if (btn) {
        btn.disabled = true;
        btn.innerText = 'Posted ✓';
        btn.classList.remove('btn-success');
        btn.classList.add('btn-secondary');
      }
    });
  "))),
  titlePanel("R047 PCC Data Dashboard"),
  fluidRow(
    column(2,
      selectInput(
        inputId  = "country_select",
        label    = "Country",
        choices  = all_countries,
        selected = saved$country,
        multiple = FALSE
      )
    ),
    column(4,
      dateRangeInput(
        inputId = "date_range",
        label   = "Date range",
        start   = saved$date_from,
        end     = saved$date_to,
        format  = "yyyy-mm-dd"
      )
    ),
    column(2,
      tags$label("\u00a0"),
      actionButton("set_default", "Set as default", class = "btn-sm btn-default",
                   style = "display:block; margin-top:1px;")
    )
  ),
  tabsetPanel(id = "main_tabs", selected = saved$tab,
    tabPanel("RESE_MP",
      DT::dataTableOutput("rese_checks"),
      tags$small(style = "color:#666; margin-top:4px; display:block;",
        "Checks run on country-filtered RESE data (parliamentary membership episodes only), matching R047.R logic."),
      uiOutput("rese_detail"),
      tags$hr(),
      fluidRow(
        column(10, tags$h5("Daily seated MPs (RESE) vs. official parliament size (PARL)")),
        column(2,  actionButton("recompute_daily", "Recompute", class = "btn-sm btn-default",
                                 style = "float:right; margin-top:4px;"))
      ),
      uiOutput("rese_daily_metrics"),
      plotOutput("rese_daily_plot", height = "700px", click = "daily_plot_click"),
      uiOutput("overcount_detail")
    ),
    tabPanel("PARL",
      DT::dataTableOutput("parl_checks"),
      tags$small(style = "color:#666; margin-top:4px; display:block;",
        "Checks run on country-filtered PARL data (national level), matching R047.R logic."),
      uiOutput("parl_detail")
    ),
    tabPanel("MEME",
      DT::dataTableOutput("meme_checks"),
      tags$small(style = "color:#666; margin-top:4px; display:block;",
        "Checks run on country-filtered MEME data, matching R047.R logic."),
      uiOutput("meme_detail")
    ),
    tabPanel(
      "POLI",
      DT::dataTableOutput("poli_checks"),
      tags$small(style = "color:#666; margin-top:4px; display:block;",
        "Checks run on country-filtered POLI data, matching R047.R logic."),
      uiOutput("poli_check_detail"),
      tags$hr(),
      DT::dataTableOutput("poli_completeness"),
      checkboxGroupInput(
        inputId  = "plot_layers",
        label    = "Graph layers:",
        choices  = c(
          "Completeness line"      = "line",
          "97.5% target"           = "target",
          "100% reference"         = "ref100",
          "Cohort size (RESE)"     = "cohort_n",
          "Parliament size (PARL)" = "parl_n",
          "No-data periods"        = "nodata"
        ),
        selected = c("line", "target", "ref100", "cohort_n", "parl_n", "nodata"),
        inline   = TRUE
      ),
      plotOutput("poli_plot"),
      uiOutput("poli_plot_note"),
      uiOutput("poli_missing_header"),
      uiOutput("poli_completeness_issue_path")
    )
  )
)

# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$set_default, {
    saveRDS(list(
      country   = input$country_select,
      date_from = input$date_range[1],
      date_to   = input$date_range[2],
      tab       = input$main_tabs
    ), defaults_file)
    showNotification("Default view saved.", type = "message", duration = 2)
  })

  # --- Open URL in system browser (works inside VS Code viewer) ---
  observeEvent(input$open_url, {
    browseURL(input$open_url$url)
  })

  # --- Refresh existing issues list (triggered by label checkbox changes) ---
  observeEvent(input$refresh_issues, {
    info <- input$refresh_issues
    labels <- unlist(info$labels)
    div_id <- info$div_id
    repo   <- info$repo

    if (length(labels) == 0) {
      session$sendCustomMessage("updateIssueList",
                                list(div_id = div_id, html = ""))
      return()
    }

    existing <- gh_list_issues(repo, labels = labels)

    if (nrow(existing) == 0) {
      html <- "<p style='font-size:0.85em; color:#999; margin:4px 0;'>No matching issues found.</p>"
    } else {
      render_row <- function(row) {
        sprintf(paste0(
          "<li style='margin-bottom:3px;'>",
          "<a href='#' ",
          "onclick=\"Shiny.setInputValue('open_url',{url:'%s',nonce:Math.random()});return false;\" ",
          "style='text-decoration:none;color:#0366d6;cursor:pointer;'>",
          "#%d %s</a> ",
          "<span style='color:#aaa;font-size:0.75em;'>%s</span>",
          "</li>"
        ), row$url, row$number,
          htmltools::htmlEscape(row$title), row$url)
      }

      open_issues   <- existing[existing$state == "OPEN", ]
      closed_issues <- existing[existing$state == "CLOSED", ]

      parts <- character(0)

      if (nrow(open_issues) > 0) {
        open_rows <- vapply(seq_len(nrow(open_issues)), function(i) {
          render_row(open_issues[i, ])
        }, character(1))
        parts <- c(parts, sprintf(paste0(
          "<div style='margin:6px 0;padding:6px 10px;background:#fefefe;",
          "border:1px solid #e8e8e8;border-radius:3px;'>",
          "<p style='font-size:0.85em;font-weight:bold;color:#28a745;margin-bottom:4px;'>",
          "&#9679; Open issues (%d):</p>",
          "<ul style='list-style:none;padding-left:0;margin:0;font-size:0.9em;'>%s</ul>",
          "</div>"
        ), nrow(open_issues), paste(open_rows, collapse = "\n")))
      }

      if (nrow(closed_issues) > 0) {
        closed_id <- paste0(div_id, "_closed")
        closed_rows <- vapply(seq_len(nrow(closed_issues)), function(i) {
          render_row(closed_issues[i, ])
        }, character(1))
        parts <- c(parts, sprintf(paste0(
          "<div style='margin:4px 0;'>",
          "<a href='#' onclick=\"var el=document.getElementById('%s');",
          "el.style.display=el.style.display==='none'?'block':'none';return false;\" ",
          "style='font-size:0.85em;color:#6f42c1;text-decoration:none;cursor:pointer;'>",
          "&#10003; Closed issues (%d) &#9662;</a>",
          "<div id='%s' style='display:none;margin-top:4px;padding:6px 10px;",
          "background:#fafafa;border:1px solid #e8e8e8;border-radius:3px;'>",
          "<ul style='list-style:none;padding-left:0;margin:0;font-size:0.9em;'>%s</ul>",
          "</div></div>"
        ), closed_id, nrow(closed_issues), closed_id,
          paste(closed_rows, collapse = "\n")))
      }

      html <- paste(parts, collapse = "\n")
    }

    session$sendCustomMessage("updateIssueList",
                              list(div_id = div_id, html = html))
  })

  # --- LLM generation for issue title & description ---
  observeEvent(input$llm_generate, {
    info <- input$llm_generate
    path <- info$path
    auto_summary <- info$auto_summary
    title_id <- info$title_id
    desc_id  <- info$desc_id
    btn_id   <- info$btn_id

    # If the panel has an associated dashboard graph, render it to a PNG and
    # attach it to the description call so the LLM diagnoses from the graph
    # itself (boundary key facts alone are easy to over-read).
    plot_key  <- if (is.null(info$plot_key)) "" else info$plot_key
    img_path  <- NULL
    img_capt  <- NULL
    if (nzchar(plot_key)) {
      plot_source <- issue_plot_sources[[plot_key]]
      plot_obj <- if (!is.null(plot_source)) plot_source()
      if (!is.null(plot_obj)) {
        img_path <- save_issue_plot(plot_obj)
        img_capt <- issue_plot_captions[[plot_key]]
      }
    }

    withProgress(message = "Generating with AI...", value = 0.3, {
      title <- llm_generate_title(path, auto_summary)
      setProgress(0.6, detail = "Generating description...")
      desc  <- llm_generate_description(path, auto_summary,
                                        image = img_path,
                                        graph_caption = img_capt)
    })
    if (!is.null(img_path)) unlink(img_path)

    # Push results back into the form fields via JS
    session$sendCustomMessage("fillField", list(id = title_id, value = title))
    if (nchar(desc) > 0) {
      session$sendCustomMessage("fillField", list(id = desc_id, value = desc))
    }

    # Re-enable the button
    session$sendCustomMessage("resetButton",
                              list(id = btn_id, label = "Generate title and description with AI"))

    if (nchar(desc) > 0) {
      showNotification("AI title and description generated.",
                       type = "message", duration = 3)
    } else {
      showNotification("AI generated title only (description failed).",
                       type = "warning", duration = 4)
    }
  })

  # --- GitHub issue posting ---
  observeEvent(input$post_github_issue, {
    info <- input$post_github_issue
    repo  <- trimws(info$repo)
    title <- trimws(info$title)
    desc  <- trimws(info$description)

    if (title == "") {
      showNotification("Issue title cannot be empty.", type = "error", duration = 4)
      return()
    }

    labels <- issue_path_to_labels(info$path)
    has_plot <- isTRUE(info$has_plot)
    asset_repo <- trimws(info$asset_repo)

    withProgress(message = "Posting issue...", value = 0.3, {
      result <- gh_post_issue(repo, title, desc, labels)

      if (!result$success) {
        showNotification(
          paste0("Failed to create issue: ", result$output),
          type = "error", duration = 8
        )
        return()
      }

      showNotification(
        paste0("Issue created: ", result$output),
        type = "message", duration = 8
      )

      # Mark the form as posted (green border + banner, disable Post button) so
      # the success is visually obvious and re-posting is blocked.
      session$sendCustomMessage("markIssuePosted", list(
        form_id = gsub("[^a-zA-Z0-9]", "_", info$path),
        number  = result$issue_number
      ))

      # If a plot is available, upload it and append to the issue
      plot_key <- if (is.null(info$plot_key)) "" else info$plot_key
      if (has_plot && nzchar(plot_key) && !is.na(result$issue_number)) {
        plot_source <- issue_plot_sources[[plot_key]]
        plot_obj <- if (!is.null(plot_source)) plot_source()
        if (!is.null(plot_obj)) {
          setProgress(0.6, detail = "Uploading graph...")
          png_path <- save_issue_plot(plot_obj)
          filename <- issue_image_filename(info$path, result$issue_number)
          image_url <- gh_upload_asset(png_path, asset_repo, filename)
          unlink(png_path)

          if (!is.null(image_url)) {
            setProgress(0.9, detail = "Attaching graph to issue...")
            gh_append_image_to_issue(
              repo, result$issue_number, image_url,
              caption = issue_plot_captions[[plot_key]]
            )
            showNotification(
              "Graph attached to issue.",
              type = "message", duration = 4
            )
          } else {
            showNotification(
              "Issue created but graph upload failed.",
              type = "warning", duration = 6
            )
          }
        }
      }

      # Attach one or more problem tables (displayed/curated columns x ALL rows)
      # as CSVs in the asset repo and link each in the issue body. table_keys is a
      # "key:suffix;..." spec: each key selects a snapshot in issue_table_sources,
      # each suffix disambiguates the deterministic filename
      # (<sanitized path>_issue<N>[_suffix].csv) so a coding agent can find every
      # table from the issue alone.
      if (isTRUE(info$attach_table) && nzchar(info$table_keys) &&
          !is.na(result$issue_number)) {
        specs <- strsplit(info$table_keys, ";", fixed = TRUE)[[1]]
        attached <- 0L
        for (spec in specs) {
          parts  <- strsplit(spec, ":", fixed = TRUE)[[1]]
          key    <- parts[1]
          suffix <- if (length(parts) > 1) parts[2] else ""
          snapshot <- issue_table_sources[[key]]
          df <- if (!is.null(snapshot)) {
            tryCatch(snapshot(), error = function(e) NULL)  # req()/freeze guards
          }
          if (is.null(df) || nrow(df) == 0) next
          setProgress(0.7, detail = paste0("Uploading table ",
                                           if (nzchar(suffix)) suffix else key, "..."))
          csv_path <- tempfile(fileext = ".csv")
          write_pcc_csv(df, csv_path)
          csv_filename <- issue_asset_filename(info$path, result$issue_number,
                                               "csv", suffix = suffix)
          csv_url <- gh_upload_asset(csv_path, asset_repo, csv_filename)
          unlink(csv_path)
          if (!is.null(csv_url)) {
            gh_append_data_link_to_issue(repo, result$issue_number,
                                         csv_url, csv_filename, nrow(df))
            attached <- attached + 1L
          }
        }
        if (attached > 0) {
          showNotification(
            paste0(attached, " problem table(s) attached."),
            type = "message", duration = 4)
        } else {
          showNotification(
            "Issue created but no problem table could be attached.",
            type = "warning", duration = 6)
        }
      }

      # Refresh the existing issues list
      path_labels <- issue_path_to_labels(info$path)
      div_id <- paste0(gsub("[^a-zA-Z0-9]", "_", info$path), "_issues")
      session$sendCustomMessage("refreshIssueList", list(
        labels = path_labels, div_id = div_id, repo = repo
      ))
    })
  })

  rese_check_results <- reactive({
    run_rese_checks(input$country_select, input$date_range[1], input$date_range[2])
  })
  parl_check_results <- reactive({ run_parl_checks(input$country_select) })
  meme_check_results <- reactive({
    run_meme_checks(input$country_select, input$date_range[1], input$date_range[2])
  })
  poli_check_results <- reactive({ run_poli_checks(input$country_select) })

  output$rese_checks <- DT::renderDataTable({ checks_dt(rese_check_results()$table) })
  output$parl_checks <- DT::renderDataTable({ checks_dt(parl_check_results()$table) })
  output$meme_checks <- DT::renderDataTable({ checks_dt(meme_check_results()$table) })
  output$poli_checks <- DT::renderDataTable({ checks_dt(poli_check_results()$table) })

  # Shared plumbing for the four check-detail tables. One reactive per tab
  # returns the selected FAIL row's problem data frame and feeds BOTH the DT
  # and the CSV download, so an export can never drift from the displayed table.
  make_detail_df <- function(results_reactive, keys, row_input) {
    reactive({
      i <- row_input()
      req(!is.null(i))
      r <- results_reactive()
      req(r$table$Status[i] == "FAIL")
      df <- r$details[[i]][[keys[i]]]
      req(!is.null(df) && nrow(df) > 0)
      df
    })
  }
  # Wire one problem-row table: a column-selector-driven DT plus an export that
  # reproduces exactly the visible columns x filtered rows in sort order.
  # aug_df: reactive returning the (POLI-id-augmented) data frame — row identity
  #   is frozen here, so DT's rows_all indices stay valid across column changes.
  # default_cols: reactive returning the curated default column set.
  # reset_events: reactive whose change means "this is a different table now" —
  #   resets the selector (via freeze) and the remembered filter state.
  # Returns a list:
  #   export   — content function for the table's downloadHandler
  #              (displayed columns x filtered rows: the physical table)
  #   snapshot — reactive: displayed columns x ALL rows (filters ignored);
  #              the GitHub-issue CSV attachment, so a forgotten on-screen
  #              filter can never truncate the evidence an agent relies on
  wire_curated_dt <- function(aug_df, default_cols, dt_id, cols_input_id,
                              reset_events) {
    state <- reactiveValues(global = "", filters = list())

    observeEvent(reset_events(), {
      # Suppress one render with the previous table's stale column selection
      # while the rebuilt selectize widget reports its fresh defaults.
      freezeReactiveValue(input, cols_input_id)
      state$global  <- ""          # never leak filters across different dfs
      state$filters <- list()
    }, ignoreInit = TRUE)

    displayed_cols <- reactive({
      avail <- names(aug_df())
      sel   <- input[[cols_input_id]]
      cols  <- sel[sel %in% avail]   # keep the selection's order: matches the
                                     # curated default order on first render, and
                                     # columns the user adds appear at the end
      if (length(cols) == 0) cols <- default_cols()
      cols
    })

    # Remember global search and per-column filters (keyed by column NAME —
    # positions shift when columns are added/removed) so they survive the
    # re-render caused by a column change.
    observeEvent(input[[paste0(dt_id, "_search")]], {
      state$global <- input[[paste0(dt_id, "_search")]]
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(dt_id, "_search_columns")]], {
      v    <- input[[paste0(dt_id, "_search_columns")]]
      cols <- isolate(displayed_cols())
      if (length(v) == length(cols))     # skip transient mid-rerender mismatches
        state$filters <- setNames(as.list(v), cols)
    }, ignoreInit = TRUE)

    output[[dt_id]] <- DT::renderDT({
      cols <- displayed_cols()
      df   <- aug_df()[, cols, drop = FALSE]
      opts <- list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                   order = list(list(0, "asc")))
      # isolate: typing in a filter box mutates `state`; without isolate that
      # would re-render the table and wipe the very filter being typed.
      st <- isolate(list(g = state$global, f = state$filters))
      if (nzchar(st$g)) opts$search <- list(search = st$g)
      sc <- lapply(cols, function(nm) {
        s <- st$f[[nm]]
        if (!is.null(s) && nzchar(s)) list(search = s) else NULL
      })
      if (!all(vapply(sc, is.null, logical(1)))) opts$searchCols <- sc
      DT::datatable(df, rownames = FALSE, filter = "top", options = opts)
    })

    list(
      # downloadHandler content: the "physical table" the user is looking at.
      export = function(file) {
        df   <- aug_df()
        cols <- displayed_cols()
        rows <- input[[paste0(dt_id, "_rows_all")]]
        if (is.null(rows)) rows <- seq_len(nrow(df))  # DT not yet reported
        rows <- clamp_export_rows(rows, nrow(df))     # stale-index race guard
        write_pcc_csv(df[rows, cols, drop = FALSE], file)
      },
      snapshot = reactive(aug_df()[, displayed_cols(), drop = FALSE])
    )
  }

  rese_detail_df <- make_detail_df(rese_check_results, rese_detail_keys,
                                   function() input$rese_checks_rows_selected)
  parl_detail_df <- make_detail_df(parl_check_results, parl_detail_keys,
                                   function() input$parl_checks_rows_selected)
  meme_detail_df <- make_detail_df(meme_check_results, meme_detail_keys,
                                   function() input$meme_checks_rows_selected)
  poli_check_detail_df <- make_detail_df(poli_check_results, poli_check_detail_keys,
                                   function() input$poli_checks_rows_selected)

  # Augmented detail frames: country id_<cc>_* columns joined in via pers_id so
  # they are available in the column selector (default-on for POLI tables only).
  rese_aug_df <- reactive(join_poli_ids(rese_detail_df(), POLI, input$country_select))
  parl_aug_df <- reactive(join_poli_ids(parl_detail_df(), POLI, input$country_select))
  meme_aug_df <- reactive(join_poli_ids(meme_detail_df(), POLI, input$country_select))
  poli_check_aug_df <- reactive(
    join_poli_ids(poli_check_detail_df(), POLI, input$country_select))

  rese_default_cols <- reactive({
    i <- req(input$rese_checks_rows_selected)
    default_detail_cols("RESE", rese_check_ids[i], names(rese_aug_df()),
                        country_id_cols(names(POLI), input$country_select))
  })
  parl_default_cols <- reactive({
    i <- req(input$parl_checks_rows_selected)
    default_detail_cols("PARL", parl_check_ids[i], names(parl_aug_df()),
                        country_id_cols(names(POLI), input$country_select))
  })
  meme_default_cols <- reactive({
    i <- req(input$meme_checks_rows_selected)
    default_detail_cols("MEME", meme_check_ids[i], names(meme_aug_df()),
                        country_id_cols(names(POLI), input$country_select))
  })
  poli_check_default_cols <- reactive({
    i <- req(input$poli_checks_rows_selected)
    default_detail_cols("POLI", poli_check_ids[i], names(poli_check_aug_df()),
                        country_id_cols(names(POLI), input$country_select))
  })

  rese_table <- wire_curated_dt(
    rese_aug_df, rese_default_cols, "rese_detail_dt", "rese_detail_cols",
    reset_events = reactive(list(input$rese_checks_rows_selected,
                                 input$country_select, input$date_range)))
  parl_table <- wire_curated_dt(
    parl_aug_df, parl_default_cols, "parl_detail_dt", "parl_detail_cols",
    reset_events = reactive(list(input$parl_checks_rows_selected,
                                 input$country_select)))
  meme_table <- wire_curated_dt(
    meme_aug_df, meme_default_cols, "meme_detail_dt", "meme_detail_cols",
    reset_events = reactive(list(input$meme_checks_rows_selected,
                                 input$country_select, input$date_range)))
  poli_check_table <- wire_curated_dt(
    poli_check_aug_df, poli_check_default_cols,
    "poli_check_detail_dt", "poli_check_detail_cols",
    reset_events = reactive(list(input$poli_checks_rows_selected,
                                 input$country_select)))

  # TRUE when the selected check row FAILed with >0 problem rows — only then do
  # the aug/default reactives resolve (they req() a non-empty FAIL df), so the
  # renderUIs below must not touch them otherwise or the whole panel vanishes.
  detail_has_rows <- function(result, i, keys) {
    if (result$table$Status[i] != "FAIL") return(FALSE)
    df <- result$details[[i]][[keys[i]]]
    !is.null(df) && nrow(df) > 0
  }

  output$rese_detail <- renderUI({
    req(input$rese_checks_rows_selected)
    i <- input$rese_checks_rows_selected
    r <- rese_check_results()
    path <- issue_path(input$country_select, "RESE", "check", rese_check_ids[i])
    has_rows <- detail_has_rows(r, i, rese_detail_keys)
    detail_header_ui(r, i, rese_detail_keys, "rese_detail_dt",
                     issue_path_str = path, download_id = "rese_detail_download",
                     cols_input_id = "rese_detail_cols",
                     col_choices   = if (has_rows) names(rese_aug_df()),
                     col_selected  = if (has_rows) rese_default_cols(),
                     table_key     = "rese_check",
                     plot_key      = "rese_daily")
  })
  output$rese_detail_download <- downloadHandler(
    filename = function() paste0(input$country_select, "_RESE_check_",
                                 rese_check_ids[input$rese_checks_rows_selected], ".csv"),
    content = rese_table$export
  )

  output$parl_detail <- renderUI({
    req(input$parl_checks_rows_selected)
    i <- input$parl_checks_rows_selected
    r <- parl_check_results()
    path <- issue_path(input$country_select, "PARL", "check", parl_check_ids[i])
    has_rows <- detail_has_rows(r, i, parl_detail_keys)
    detail_header_ui(r, i, parl_detail_keys, "parl_detail_dt",
                     issue_path_str = path, download_id = "parl_detail_download",
                     cols_input_id = "parl_detail_cols",
                     col_choices   = if (has_rows) names(parl_aug_df()),
                     col_selected  = if (has_rows) parl_default_cols(),
                     table_key     = "parl_check")
  })
  output$parl_detail_download <- downloadHandler(
    filename = function() paste0(input$country_select, "_PARL_check_",
                                 parl_check_ids[input$parl_checks_rows_selected], ".csv"),
    content = parl_table$export
  )

  output$meme_detail <- renderUI({
    req(input$meme_checks_rows_selected)
    i <- input$meme_checks_rows_selected
    r <- meme_check_results()
    path <- issue_path(input$country_select, "MEME", "check", meme_check_ids[i])
    has_rows <- detail_has_rows(r, i, meme_detail_keys)
    detail_header_ui(r, i, meme_detail_keys, "meme_detail_dt",
                     issue_path_str = path, download_id = "meme_detail_download",
                     cols_input_id = "meme_detail_cols",
                     col_choices   = if (has_rows) names(meme_aug_df()),
                     col_selected  = if (has_rows) meme_default_cols(),
                     table_key     = "meme_check")
  })
  output$meme_detail_download <- downloadHandler(
    filename = function() paste0(input$country_select, "_MEME_check_",
                                 meme_check_ids[input$meme_checks_rows_selected], ".csv"),
    content = meme_table$export
  )

  output$poli_check_detail <- renderUI({
    req(input$poli_checks_rows_selected)
    i <- input$poli_checks_rows_selected
    r <- poli_check_results()
    path <- issue_path(input$country_select, "POLI", "check", poli_check_ids[i])
    has_rows <- detail_has_rows(r, i, poli_check_detail_keys)
    detail_header_ui(r, i, poli_check_detail_keys, "poli_check_detail_dt",
                     issue_path_str = path, download_id = "poli_check_detail_download",
                     cols_input_id = "poli_check_detail_cols",
                     col_choices   = if (has_rows) names(poli_check_aug_df()),
                     col_selected  = if (has_rows) poli_check_default_cols(),
                     table_key     = "poli_check")
  })
  output$poli_check_detail_download <- downloadHandler(
    filename = function() paste0(input$country_select, "_POLI_check_",
                                 poli_check_ids[input$poli_checks_rows_selected], ".csv"),
    content = poli_check_table$export
  )

  # --- Daily MP counts graph (RESE_MP tab) ---

  last_poli_plot <- reactiveVal(NULL)
  last_rese_daily_plot <- reactiveVal(NULL)
  clicked_episode <- reactiveVal(NULL)

  daily_cache_version <- reactiveVal(0)

  observeEvent(input$recompute_daily, {
    cc <- input$country_select
    f_rds <- file.path(cache_dir, paste0("daily_counts_", cc, ".rds"))
    f_ver <- file.path(cache_dir, paste0("daily_counts_", cc, "_version.txt"))
    if (file.exists(f_rds)) file.remove(f_rds)
    if (file.exists(f_ver)) file.remove(f_ver)
    daily_cache_version(daily_cache_version() + 1)
  })

  daily_counts <- reactive({
    daily_cache_version()
    cc <- input$country_select
    withProgress(message = paste("Computing daily MP counts for", cc, "..."),
                 value = 0.5, {
      get_daily_counts(cc)
    })
  })

  output$rese_daily_plot <- renderPlot({
    dc <- daily_counts()
    req(nrow(dc) > 0)

    df <- dc[dc$date >= input$date_range[1] & dc$date <= input$date_range[2], ]
    req(nrow(df) > 0)

    parl_steps <- PARL |>
      filter(country_abb == input$country_select, level == "NT",
             assembly_abb == assembly_map[[input$country_select]],
             leg_period_start_date >= input$date_range[1],
             leg_period_start_date <= input$date_range[2]) |>
      arrange(leg_period_start_date)

    country_name <- country_labels[input$country_select]
    if (is.na(country_name)) country_name <- input$country_select

    parl_years <- data.frame(date = parl_steps$leg_period_start_date,
                             year = format(parl_steps$leg_period_start_date, "%Y"))
    y_min <- min(c(df$n_seated, df$parliament_size), na.rm = TRUE)

    # Flag days where seated count exceeds official parliament size
    df$overcount <- !is.na(df$parliament_size) & df$n_seated > df$parliament_size
    # Create run-length groups; duplicate boundary rows so segments overlap and connect
    df$segment <- cumsum(c(1, diff(df$overcount) != 0))
    seg_ids <- unique(df$segment)
    if (length(seg_ids) > 1) {
      bridges <- lapply(seq_len(length(seg_ids) - 1), function(k) {
        row <- df[df$segment == seg_ids[k + 1], ][1, ]
        row$segment  <- seg_ids[k]
        row$overcount <- df$overcount[df$segment == seg_ids[k]][1]
        row
      })
      df <- rbind(df, do.call(rbind, bridges))
      df <- df[order(df$date, df$segment), ]
    }

    p <- ggplot(df, aes(x = date)) +
      geom_vline(xintercept = parl_steps$leg_period_start_date,
                 color = "gray70", alpha = 0.5, linewidth = 0.3) +
      geom_text(data = parl_years, aes(x = date, y = y_min, label = year),
                angle = 90, size = 4.5, color = "gray50",
                hjust = 0, vjust = 0.5, inherit.aes = FALSE) +
      geom_step(aes(y = parliament_size), color = "gray40",
                linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = n_seated, color = overcount, group = segment),
                linewidth = 0.5) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                         guide = "none") +
      scale_x_date(name = "Date",
                   limits = c(input$date_range[1], input$date_range[2])) +
      scale_y_continuous(name = "MPs (count)") +
      labs(
        title    = paste0("Daily seated MPs \u2014 ", country_name),
        subtitle = "Blue: actual seated MPs (RESE)   Grey step: official parliament size (PARL)   Red: overcount"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.background  = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA))

    last_rese_daily_plot(p)   # kept for attaching to GitHub issues
    p
  })

  output$rese_daily_metrics <- renderUI({
    dc <- daily_counts()
    req(nrow(dc) > 0)
    df <- dc[dc$date >= input$date_range[1] & dc$date <= input$date_range[2] &
               !is.na(dc$parliament_size), ]
    req(nrow(df) > 0)

    n_total <- nrow(df)
    pct_over  <- round(100 * sum(df$n_seated > df$parliament_size) / n_total, 1)
    pct_under <- round(100 * sum(df$n_seated < df$parliament_size) / n_total, 1)

    tags$p(
      style = "margin-bottom:4px; font-size:0.95em;",
      tags$span(style = "color:#c0392b; font-weight:bold;",
                paste0("Overcount: ", pct_over, "% of days")),
      " | ",
      tags$span(style = "color:#2874a6; font-weight:bold;",
                paste0("Undercount: ", pct_under, "% of days")),
      tags$span(style = "color:#666;",
                paste0("  (", format(n_total, big.mark = ","), " days with parliament size data)"))
    )
  })

  # --- Overcount episode click detail (RESE_MP tab) ---

  overcount_episodes <- reactive({
    dc <- daily_counts()
    req(nrow(dc) > 0)
    dc <- dc[!is.na(dc$parliament_size), ]
    dc$over <- dc$n_seated > dc$parliament_size
    if (!any(dc$over)) return(NULL)

    # Identify contiguous runs of overcount days
    runs <- rle(dc$over)
    end_idx   <- cumsum(runs$lengths)
    start_idx <- end_idx - runs$lengths + 1
    which_over <- which(runs$values)

    episodes <- data.frame(
      start_date = dc$date[start_idx[which_over]],
      end_date   = dc$date[end_idx[which_over]],
      stringsAsFactors = FALSE
    )

    # Compute stats per episode
    episodes$duration_days <- as.integer(episodes$end_date - episodes$start_date) + 1L
    episodes$peak_date <- as.Date(NA)   # preallocate: [i]<- into NULL drops Date class
    for (i in seq_len(nrow(episodes))) {
      rows <- dc[dc$date >= episodes$start_date[i] & dc$date <= episodes$end_date[i], ]
      excess <- rows$n_seated - rows$parliament_size
      episodes$peak_excess[i]    <- max(excess)
      episodes$mean_excess[i]    <- round(mean(excess), 1)
      episodes$parliament_size[i] <- rows$parliament_size[1]
      # Day the overcount is worst: the roster on this day is the fullest
      # "find the extra head" context. First peak day when the max repeats.
      episodes$peak_date[i]      <- rows$date[which.max(excess)]
    }
    episodes
  })

  # Capture plot click into a stable reactiveVal (survives plot re-renders)
  observeEvent(input$daily_plot_click, {
    eps <- overcount_episodes()
    if (is.null(eps) || nrow(eps) == 0) return()

    clicked_date <- as.Date(input$daily_plot_click$x, origin = "1970-01-01")
    dist_to_ep <- pmax(
      as.numeric(eps$start_date - clicked_date),
      as.numeric(clicked_date - eps$end_date), 0
    )
    nearest <- which.min(dist_to_ep)
    if (dist_to_ep[nearest] > 365) return()

    clicked_episode(list(
      ep       = eps[nearest, ],
      country  = input$country_select
    ))
  })

  # Clear clicked episode when country changes
  observeEvent(input$country_select, { clicked_episode(NULL) })

  # RESE entries ending on the clicked episode's end date; shared by the detail
  # header, the DT, and the CSV download so all three stay in lockstep.
  # Full RESE rows: the default view narrows to overcount_default_cols, and the
  # column selector can bring any of the remaining columns back.
  overcount_rese_ending <- reactive({
    info <- clicked_episode()
    req(!is.null(info))
    ep <- info$ep
    cc <- info$country
    RESE[RESE$country_abb == cc &
           RESE$political_function %in%
             c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09") &
           !is.na(RESE$end_date) &
           RESE$end_date == ep$end_date, ]
  })

  overcount_aug_df <- reactive({
    info <- clicked_episode()
    req(!is.null(info))
    join_poli_ids(overcount_rese_ending(), POLI, info$country)
  })
  overcount_defaults <- reactive({
    intersect(overcount_default_cols, names(overcount_aug_df()))
  })
  overcount_table <- wire_curated_dt(
    overcount_aug_df, overcount_defaults, "overcount_rese_dt",
    "overcount_rese_cols", reset_events = reactive(clicked_episode()))

  # Diagnostic sets for the clicked episode, all keyed on pers_id and built from
  # the same MP predicate that produces n_seated. Bundled in one reactive so the
  # base rows and POLI bio join happen once and every downstream table/CSV/summary
  # stays consistent.
  #   closing    : end_date == episode end (current behaviour; who resolved it)
  #   opening    : start_date within [start, end] (who may have triggered it)
  #   throughout : seated on every day of the episode (persistent-overcount pool)
  #   peak       : everyone seated on the max-excess day (full context)
  #   clusters   : peak-day members sharing a birth_date (duplicate-person signal)
  overcount_sets <- reactive({
    info <- clicked_episode()
    req(!is.null(info))
    ep <- info$ep
    base <- join_poli_bio(rese_mp_rows(RESE, info$country), POLI)
    closing <- base[!is.na(base$end_date) & base$end_date == ep$end_date, ,
                    drop = FALSE]
    opening <- base[!is.na(base$start_date) & base$start_date >= ep$start_date &
                      base$start_date <= ep$end_date, , drop = FALSE]
    throughout <- base[!is.na(base$start_date) & base$start_date <= ep$start_date &
                         (is.na(base$end_date) | base$end_date >= ep$end_date), ,
                       drop = FALSE]
    peak <- seated_on(base, ep$peak_date)
    list(ep = ep, closing = closing, opening = opening,
         throughout = throughout, peak = peak,
         clusters = birthdate_clusters(peak))
  })

  # Curated-column snapshots feeding the per-set CSV attachments (all rows).
  overcount_snap <- function(which) reactive({
    df <- overcount_sets()[[which]]
    df[, intersect(overcount_context_cols, names(df)), drop = FALSE]
  })
  overcount_opening_snap    <- overcount_snap("opening")
  overcount_peak_snap       <- overcount_snap("peak")
  overcount_throughout_snap <- overcount_snap("throughout")

  output$overcount_detail <- renderUI({
    info <- clicked_episode()
    req(!is.null(info))

    ep <- info$ep
    cc <- info$country
    rese_ending <- overcount_rese_ending()
    sets <- overcount_sets()
    cur  <- function(df) df[, intersect(overcount_context_cols, names(df)),
                            drop = FALSE]

    # Determine parliament_id for the overcount episode (use midpoint)
    ep_mid <- ep$start_date + as.integer((ep$end_date - ep$start_date) / 2)
    parl_cc <- PARL[PARL$country_abb == cc & PARL$level == "NT" &
                      PARL$assembly_abb == assembly_map[[cc]], ]
    parl_cc <- parl_cc[order(parl_cc$leg_period_start_date), ]
    parl_idx <- max(which(parl_cc$leg_period_start_date <= ep_mid), 0)
    ep_parl_id <- if (parl_idx > 0) parl_cc$parliament_id[parl_idx] else "unknown"

    # Filter episodes to this parliament's period
    all_eps <- overcount_episodes()
    parl_episodes <- NULL
    if (!is.null(all_eps) && parl_idx > 0) {
      parl_start <- parl_cc$leg_period_start_date[parl_idx]
      parl_end <- if (parl_idx < nrow(parl_cc)) {
        parl_cc$leg_period_start_date[parl_idx + 1] - 1
      } else Sys.Date()
      parl_episodes <- all_eps[
        all_eps$start_date >= parl_start & all_eps$start_date <= parl_end, ]
    }

    path <- issue_path(cc, "RESE", "overcount", ep_parl_id)
    # Keep the GitHub-issue summary at the curated 6 columns; the on-screen
    # table now carries the full (augmented) RESE rows behind the selector.
    rese_ending_summary <- rese_ending[
      , intersect(overcount_default_cols, names(rese_ending)), drop = FALSE]
    auto_summary <- build_overcount_summary(
      ep, rese_ending_summary, all_episodes = parl_episodes,
      opening = cur(sets$opening), throughout = cur(sets$throughout),
      peak_roster = cur(sets$peak), birthdate_dupes = cur(sets$clusters))

    tagList(
      tags$hr(),
      tags$p(
        style = "font-weight:bold; color:#c0392b;",
        paste0("Overcount episode: ",
               format_pcc_date(ep$start_date), " \u2013 ",
               format_pcc_date(ep$end_date),
               " (", ep$duration_days, " days)")
      ),
      tags$p(
        paste0("Official parliament size: ", ep$parliament_size,
               "  |  Peak excess: +", ep$peak_excess,
               "  |  Mean excess: +", ep$mean_excess)
      ),
      if (nrow(rese_ending) > 0) {
        tagList(
          tags$p(style = "font-weight:bold; margin-top:8px;",
                 paste0("RESE entries ending on ", format_pcc_date(ep$end_date),
                        " (", nrow(rese_ending), "):")),
          selectizeInput("overcount_rese_cols", "Columns shown",
                         choices = names(overcount_aug_df()),
                         selected = overcount_defaults(),
                         multiple = TRUE, width = "100%",
                         options = list(plugins = list("remove_button"))),
          DT::DTOutput("overcount_rese_dt"),
          downloadButton("overcount_rese_download", "Export physical table",
                         class = "btn-sm btn-outline-secondary",
                         style = "margin-top:6px;")
        )
      },
      # Additional diagnostic sets (context beyond the closing set). Compact
      # read-only tables; the full rows travel in the attached CSVs.
      local({
        section <- function(title, df, note = NULL) {
          if (is.null(df) || nrow(df) == 0) return(NULL)
          tagList(
            tags$p(style = "font-weight:bold; margin-top:12px;", title),
            if (!is.null(note)) tags$p(
              style = "font-size:0.85em; color:#666; margin:0 0 4px;", note),
            df_to_html_table(cur(df)))
        }
        open_ended <- sum(is.na(sets$throughout$end_date))
        tagList(
          section(
            sprintf("Opening set – entries starting within the episode (%d):",
                    nrow(sets$opening)),
            sets$opening,
            "An arrival dated too early can trigger the overcount."),
          section(
            sprintf("Present throughout the episode (%d%s):",
                    nrow(sets$throughout),
                    if (open_ended > 0) sprintf("; %d open-ended", open_ended) else ""),
            sets$throughout,
            "Candidates for a persistent overcount – watch stray open-ended records."),
          if (nrow(sets$peak) > 0) tags$p(
            style = "margin-top:12px;",
            tags$b(sprintf("Peak-day roster: %d seated vs %d official on %s.",
                           nrow(sets$peak), ep$parliament_size,
                           format_pcc_date(ep$peak_date))),
            tags$span(style = "color:#666;", " Full roster in the attached peak CSV.")),
          section(
            sprintf("Shared birth dates on the peak day (%d):", nrow(sets$clusters)),
            sets$clusters,
            "Members sharing a birth_date – a strong \"same person, two records\" signal.")
        )
      }),
      issue_path_tag(path, auto_summary, plot_key = "rese_daily",
                     table_key = {
                       tk <- character(0)
                       if (nrow(rese_ending) > 0)     tk <- c(tk, closing    = "overcount")
                       if (nrow(sets$opening) > 0)    tk <- c(tk, opening    = "overcount_opening")
                       if (nrow(sets$peak) > 0)       tk <- c(tk, peak       = "overcount_peak")
                       if (nrow(sets$throughout) > 0) tk <- c(tk, throughout = "overcount_throughout")
                       if (length(tk) > 0) tk else NULL
                     })
    )
  })

  output$overcount_rese_download <- downloadHandler(
    filename = function() {
      info <- clicked_episode()
      paste0(info$country, "_RESE_overcount_", format(info$ep$end_date, "%Y-%m-%d"),
             ".csv")
    },
    content = overcount_table$export
  )

  # --- POLI tab ---

  poli_vars <- reactive({
    c(poli_base_vars, country_id_cols(names(POLI), input$country_select))
  })

  filtered <- reactive({
    POLI |> filter(country == input$country_select)
  })

  ever_mp_ids <- reactive({
    RESE |>
      filter(
        country_abb == input$country_select,
        political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")
      ) |>
      pull(pers_id) |>
      unique()
  })

  cohort <- reactive({
    build_cohort(input$country_select)
  })

  no_data_segs <- reactive({
    ch        <- cohort()
    date_from <- input$date_range[1]
    date_to   <- input$date_range[2]
    if (is.null(ch)) return(data.frame(x = date_from, xend = date_to))
    ps <- sort(unique(ch$snapshot_day))
    ps <- ps[ps >= date_from & ps <= date_to]
    if (length(ps) == 0) return(data.frame(x = date_from, xend = date_to))
    rbind(
      if (date_from < min(ps)) data.frame(x = date_from, xend = min(ps)) else NULL,
      if (date_to   > max(ps)) data.frame(x = max(ps),   xend = date_to) else NULL
    )
  })

  output$poli_completeness <- DT::renderDataTable({
    d     <- filtered()
    d_mp  <- d |> filter(pers_id %in% ever_mp_ids())
    avail <- function(x) !is.na(x) & x != ""

    vars <- poli_vars()
    df <- data.frame(
      Variable        = vars,
      `N (all)`       = nrow(d),
      `% Avail (all)` = sapply(vars, function(v) round(mean(avail(d[[v]])) * 100, 1)),
      `N (MPs)`       = nrow(d_mp),
      `% Avail (MPs)` = sapply(vars, function(v) round(mean(avail(d_mp[[v]])) * 100, 1)),
      check.names     = FALSE,
      row.names       = NULL
    )
    DT::datatable(
      df,
      selection = "single",
      rownames  = FALSE,
      options   = list(dom = "t", ordering = FALSE, paging = FALSE),
      caption   = htmltools::tags$div(
        htmltools::tags$span("Click a row to see completeness over time (MPs cohort)"),
        htmltools::tags$br(),
        htmltools::tags$small(
          style = "color: #666;",
          "% Avail (MPs) is highlighted ",
          htmltools::tags$span(style = "background-color:#d4edda; padding: 1px 4px;", "green"),
          " if \u2265 97.5% and ",
          htmltools::tags$span(style = "background-color:#f8d7da; padding: 1px 4px;", "red"),
          " if < 97.5%."
        )
      )
    ) |>
      DT::formatStyle(
        "% Avail (MPs)",
        backgroundColor = DT::styleInterval(97.5, c("#f8d7da", "#d4edda"))
      )
  })

  output$poli_plot <- renderPlot({
    req(input$poli_completeness_rows_selected)

    selected_var <- poli_vars()[input$poli_completeness_rows_selected]

    ch <- cohort()
    req(!is.null(ch))

    # Join cohort with the selected POLI variable and flag availability
    cohort_poli <- ch |>
      left_join(POLI[, c("pers_id", selected_var)], by = "pers_id") |>
      mutate(available = !is.na(.data[[selected_var]]) & .data[[selected_var]] != "")

    completeness <- cohort_poli |>
      group_by(parliament_id, snapshot_day) |>
      summarise(
        n_seated        = n(),
        parliament_size = first(parliament_size),
        pct_complete    = round(100 * mean(available), 1),
        .groups         = "drop"
      ) |>
      arrange(snapshot_day)

    date_from <- input$date_range[1]
    date_to   <- input$date_range[2]

    completeness <- completeness |>
      filter(snapshot_day >= date_from & snapshot_day <= date_to)

    completeness <- completeness |>
      mutate(below_threshold = pct_complete < 97.5)

    parl_starts <- sort(completeness$snapshot_day)
    parl_years  <- data.frame(date = parl_starts, year = format(parl_starts, "%Y"))

    segs   <- no_data_segs()
    layers <- input$plot_layers

    country_name <- country_labels[input$country_select]
    if (is.na(country_name)) country_name <- input$country_select

    p <- ggplot(completeness, aes(x = snapshot_day, y = pct_complete)) +
      geom_vline(xintercept = parl_starts, color = "gray70", alpha = 0.6, linewidth = 0.3) +
      geom_text(
        data  = parl_years,
        aes(x = date, y = 2, label = year),
        angle = 90, size = 4, color = "gray50", hjust = 0, vjust = 0.5
      )

    if ("line" %in% layers) {
      p <- p +
        geom_line(color = "steelblue", linewidth = 0.8) +
        geom_point(aes(color = below_threshold), size = 2.5) +
        scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"), guide = "none")
    }

    if ("ref100" %in% layers)
      p <- p + geom_hline(yintercept = 100, linetype = "dashed", color = "gray50")

    if ("target" %in% layers)
      p <- p + geom_hline(yintercept = 97.5, linetype = "dashed", color = "tomato", linewidth = 0.8)

    show_n <- ("cohort_n" %in% layers || "parl_n" %in% layers) && nrow(completeness) > 0
    if (show_n) {
      n_vals  <- c(
        if ("cohort_n" %in% layers) completeness$n_seated        else NULL,
        if ("parl_n"   %in% layers) completeness$parliament_size else NULL
      )
      max_n      <- max(n_vals, na.rm = TRUE)
      n_breaks   <- pretty(c(0, max_n), n = 5)
      n_breaks   <- n_breaks[n_breaks >= 0 & n_breaks <= max_n]
      n_breaks_y <- n_breaks / max_n * 90
      p <- p +
        geom_hline(
          yintercept  = n_breaks_y,
          color       = "#a8d5a2",
          linewidth   = 0.3,
          linetype    = "solid"
        )
      if ("cohort_n" %in% layers) {
        p <- p + geom_line(
          data        = completeness,
          aes(x = snapshot_day, y = n_seated / max_n * 90),
          color       = "darkgreen",
          linewidth   = 0.7,
          linetype    = "dotted",
          inherit.aes = FALSE
        )
      }
      if ("parl_n" %in% layers) {
        p <- p + geom_line(
          data        = completeness,
          aes(x = snapshot_day, y = parliament_size / max_n * 90),
          color       = "darkorchid",
          linewidth   = 0.7,
          linetype    = "dotted",
          inherit.aes = FALSE
        )
      }
      p <- p +
        scale_x_date(name = "Parliament start date", limits = c(date_from, date_to)) +
        scale_y_continuous(
          name     = "% available",
          limits   = c(0, 105),
          sec.axis = sec_axis(~ . * max_n / 90, name = "Cohort / parliament size (N)")
        )
    } else {
      p <- p +
        scale_x_date(name = "Parliament start date", limits = c(date_from, date_to)) +
        scale_y_continuous(name = "% available", limits = c(0, 105))
    }

    if ("nodata" %in% layers && !is.null(segs) && nrow(segs) > 0) {
      p <- p + geom_segment(
        data        = segs,
        aes(x = x, xend = xend, y = 0, yend = 0),
        color       = "orange",
        linewidth   = 1.2,
        inherit.aes = FALSE
      )
    }

    final_plot <- p +
      labs(
        title    = paste0("Completeness: ", selected_var, " \u2014 ", country_name),
        subtitle = "% of first-day cohort MPs for whom the variable is available; dashed red line = 97.5% target"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )

    last_poli_plot(final_plot)
    final_plot
  })

  output$poli_plot_note <- renderUI({
    req(input$poli_completeness_rows_selected)
    segs <- no_data_segs()
    if (is.null(segs) || nrow(segs) == 0) return(NULL)
    periods <- apply(segs, 1, function(r) {
      paste0(format(as.Date(r[["x"]]), "%d %b %Y"),
             " \u2013 ",
             format(as.Date(r[["xend"]]), "%d %b %Y"))
    })
    htmltools::tags$p(
      style = "color: orange; font-size: 0.85em; margin-top: 4px;",
      htmltools::tags$b("Note:"),
      paste0(
        "The orange line indicates period(s) with no parliamentary membership data: ",
        paste(periods, collapse = "; "), "."
      )
    )
  })

  # MPs missing the selected POLI variable, computed once and shared by the
  # header, the DT, the CSV download, and the issue-path summary below.
  # Full POLI rows: the default view narrows via poli_missing_default_cols()
  # (which INCLUDES the selected, empty variable so the table self-documents);
  # the column selector offers every POLI column.
  poli_missing_rows <- reactive({
    req(input$poli_completeness_rows_selected)
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    d_mp <- filtered() |> filter(pers_id %in% ever_mp_ids())
    d_mp[is.na(d_mp[[selected_var]]) | d_mp[[selected_var]] == "", ]
  })

  poli_missing_defaults <- reactive({
    selected_var <- poli_vars()[req(input$poli_completeness_rows_selected)]
    poli_missing_default_cols(selected_var, names(poli_missing_rows()),
                              country_id_cols(names(POLI), input$country_select))
  })
  poli_missing_table <- wire_curated_dt(
    poli_missing_rows, poli_missing_defaults, "poli_missing_dt",
    "poli_missing_cols",
    reset_events = reactive(list(input$poli_completeness_rows_selected,
                                 input$country_select)))

  # Snapshot reactives (displayed columns x ALL rows) per issue-form table_key;
  # read by the post_github_issue observer to build the CSV attachment.
  issue_table_sources <- list(
    rese_check   = rese_table$snapshot,
    parl_check   = parl_table$snapshot,
    meme_check   = meme_table$snapshot,
    poli_check   = poli_check_table$snapshot,
    overcount            = overcount_table$snapshot,   # closing set (curated DT)
    overcount_opening    = overcount_opening_snap,
    overcount_peak       = overcount_peak_snap,
    overcount_throughout = overcount_throughout_snap,
    poli_missing = poli_missing_table$snapshot
  )

  # Plot reactiveVals per issue-form plot_key, with the caption used when the
  # image is appended to the issue body.
  issue_plot_sources <- list(
    poli_completeness = last_poli_plot,
    rese_daily        = last_rese_daily_plot
  )
  issue_plot_captions <- c(
    poli_completeness = "Completeness over time",
    rese_daily        = "Daily seated MPs vs official parliament size"
  )

  output$poli_missing_header <- renderUI({
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    missing <- poli_missing_rows()
    if (nrow(missing) == 0) {
      return(tags$p(style = "color:#28a745; font-weight:bold; margin-top:8px;",
                    paste0("All MPs have ", selected_var, " available.")))
    }
    tagList(
      tags$p(style = "font-weight:bold; color:#c0392b; margin-top:8px;",
             paste0("MPs missing ", selected_var, " (", nrow(missing), "):")),
      selectizeInput("poli_missing_cols", "Columns shown",
                     choices = names(missing),
                     selected = poli_missing_defaults(),
                     multiple = TRUE, width = "100%",
                     options = list(plugins = list("remove_button"))),
      DT::DTOutput("poli_missing_dt"),
      downloadButton("poli_missing_download", "Export physical table",
                     class = "btn-sm btn-outline-secondary",
                     style = "margin-top:6px;")
    )
  })

  output$poli_missing_download <- downloadHandler(
    filename = function() {
      selected_var <- poli_vars()[input$poli_completeness_rows_selected]
      paste0(input$country_select, "_POLI_missing_", selected_var, ".csv")
    },
    content = poli_missing_table$export
  )

  output$poli_completeness_issue_path <- renderUI({
    req(input$poli_completeness_rows_selected)
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    missing <- poli_missing_rows()
    show_cols <- c("pers_id", intersect(c("last_name", "first_name"), names(missing)))
    missing_preview <- missing[, show_cols, drop = FALSE]

    # Build completeness time series for the LLM
    ch <- cohort()
    completeness_ts <- NULL
    if (!is.null(ch)) {
      cohort_poli <- ch |>
        left_join(POLI[, c("pers_id", selected_var)], by = "pers_id") |>
        mutate(available = !is.na(.data[[selected_var]]) & .data[[selected_var]] != "")
      completeness_ts <- cohort_poli |>
        group_by(parliament_id, snapshot_day) |>
        summarise(pct_complete = round(100 * mean(available), 1),
                  .groups = "drop") |>
        filter(snapshot_day >= input$date_range[1],
               snapshot_day <= input$date_range[2]) |>
        arrange(snapshot_day)
    }

    auto_summary <- build_completeness_summary(selected_var, missing_preview,
                                               completeness_ts)
    path <- issue_path(input$country_select, "POLI", "completeness", selected_var)
    issue_path_tag(path, auto_summary, plot_key = "poli_completeness",
                   table_key = if (nrow(missing) > 0) "poli_missing")
  })

}

shinyApp(ui, server)
