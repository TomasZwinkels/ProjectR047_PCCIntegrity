for (pkg in c("shiny", "dplyr", "ggplot2", "DT", "testthat")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_MEME_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_POLI_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_issue_functions.R")

test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_MEME_unittests.R")
test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_POLI_unittests.R")
# test_file("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_issue_unittests.R") # It contains generative AI calls, so is slow, so commented out by default. 

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
  "parliaments_no_data", "snapshot_row", "snapshot_row"
)

# Render the issue path with an "Open new issue" button and inline form
issue_path_tag <- function(path, auto_summary = "") {
  form_id <- gsub("[^a-zA-Z0-9]", "_", path)
  path_js <- gsub("'", "\\\\'", path)
  settings_id <- paste0(form_id, "_settings")
  auto_id <- paste0(form_id, "_auto")

  tags$div(
    tags$div(
      style = "font-family:monospace; font-size:0.9em; color:#555; background:#f0f0f0; padding:4px 8px; border-left:3px solid #2874a6; margin-top:8px; margin-bottom:4px; display:flex; align-items:center; justify-content:space-between;",
      tags$span(path),
      tags$button(
        "Open new issue",
        class = "btn btn-sm btn-outline-primary",
        style = "font-size:0.85em;",
        onclick = sprintf(
          "var el = document.getElementById('%s'); el.style.display = el.style.display === 'none' ? 'block' : 'none';",
          form_id
        )
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
      tags$label("Agent description:", style = "font-weight:bold; display:block; margin-top:8px; margin-bottom:4px; color:#666;"),
      tags$textarea(
        id = auto_id,
        rows = "6",
        auto_summary,
        style = "width:100%; padding:6px; border:1px solid #ccc; border-radius:3px; font-size:0.85em; font-family:monospace; background:#f9f9f9; color:#444;"
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
              "btn_id: this.id, ",
              "nonce: Math.random()});"
            ),
            path_js, auto_id, form_id, form_id
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
              "nonce: Math.random()});"
            ),
            path_js, form_id, form_id, auto_id, settings_id
          )
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
        tags$label("Repository:", style = "font-weight:bold; font-size:0.85em; display:block; margin-bottom:4px;"),
        tags$input(
          id = paste0(settings_id, "_repo"),
          type = "text",
          value = github_defaults$repo,
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
    "All POLI person IDs are unique"
  )
  details <- list(
    check_POLI_persid_unique_details(poli_cc)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

poli_check_detail_keys <- c("duplicate_rows")

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

# Build the header tagList for a detail panel; the DT itself is injected by renderDT.
detail_header_ui <- function(result, row_idx, key_vec, dt_output_id,
                             issue_path_str = NULL) {
  status <- result$table$Status[row_idx]
  label  <- result$table$Check[row_idx]

  auto_summary <- if (!is.null(issue_path_str)) {
    build_check_summary(result, row_idx, key_vec)
  } else ""
  path_tag <- if (!is.null(issue_path_str)) issue_path_tag(issue_path_str, auto_summary) else NULL

  if (status == "PASS") {
    return(tagList(
      tags$hr(),
      tags$p(style = "color:#28a745; font-weight:bold;",
             paste0("\u2713 ", label, " — no issues found.")),
      path_tag
    ))
  }

  det <- result$details[[row_idx]]
  df  <- det[[key_vec[row_idx]]]
  n   <- if (!is.null(df)) nrow(df) else 0

  tagList(
    tags$hr(),
    tags$p(style = "font-weight:bold; color:#c0392b;",
           paste0("Details: ", label,
                  " (", n, " problem row", if (n != 1) "s", ")")),
    if (n == 0)
      tags$p(style = "color:#666;", "(No problem rows returned by details function.)")
    else
      DT::DTOutput(dt_output_id),
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
      DT::DTOutput("poli_missing_dt"),
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

  # --- LLM generation for issue title & description ---
  observeEvent(input$llm_generate, {
    info <- input$llm_generate
    path <- info$path
    auto_summary <- info$auto_summary
    title_id <- info$title_id
    desc_id  <- info$desc_id
    btn_id   <- info$btn_id

    withProgress(message = "Generating with AI...", value = 0.3, {
      title <- llm_generate_title(path, auto_summary)
      setProgress(0.6, detail = "Generating description...")
      desc  <- llm_generate_description(path, auto_summary)
    })

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
    result <- gh_post_issue(repo, title, desc, labels)

    if (result$success) {
      showNotification(
        paste0("Issue created: ", result$output),
        type = "message", duration = 8
      )
    } else {
      showNotification(
        paste0("Failed to create issue: ", result$output),
        type = "error", duration = 8
      )
    }
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

  output$rese_detail <- renderUI({
    req(input$rese_checks_rows_selected)
    i <- input$rese_checks_rows_selected
    path <- issue_path(input$country_select, "RESE", "check", rese_check_ids[i])
    detail_header_ui(rese_check_results(), i, rese_detail_keys, "rese_detail_dt",
                     issue_path_str = path)
  })
  output$rese_detail_dt <- DT::renderDT({
    req(input$rese_checks_rows_selected)
    r <- rese_check_results()
    i <- input$rese_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[rese_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE, filter = "top",
                  options = list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                                order = list(list(0, "asc"))))
  })

  output$parl_detail <- renderUI({
    req(input$parl_checks_rows_selected)
    i <- input$parl_checks_rows_selected
    path <- issue_path(input$country_select, "PARL", "check", parl_check_ids[i])
    detail_header_ui(parl_check_results(), i, parl_detail_keys, "parl_detail_dt",
                     issue_path_str = path)
  })
  output$parl_detail_dt <- DT::renderDT({
    req(input$parl_checks_rows_selected)
    r <- parl_check_results()
    i <- input$parl_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[parl_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE, filter = "top",
                  options = list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                                order = list(list(0, "asc"))))
  })

  output$meme_detail <- renderUI({
    req(input$meme_checks_rows_selected)
    i <- input$meme_checks_rows_selected
    path <- issue_path(input$country_select, "MEME", "check", meme_check_ids[i])
    detail_header_ui(meme_check_results(), i, meme_detail_keys, "meme_detail_dt",
                     issue_path_str = path)
  })
  output$meme_detail_dt <- DT::renderDT({
    req(input$meme_checks_rows_selected)
    r <- meme_check_results()
    i <- input$meme_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[meme_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE, filter = "top",
                  options = list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                                order = list(list(0, "asc"))))
  })

  output$poli_check_detail <- renderUI({
    req(input$poli_checks_rows_selected)
    i <- input$poli_checks_rows_selected
    path <- issue_path(input$country_select, "POLI", "check", poli_check_ids[i])
    detail_header_ui(poli_check_results(), i, poli_check_detail_keys, "poli_check_detail_dt",
                     issue_path_str = path)
  })
  output$poli_check_detail_dt <- DT::renderDT({
    req(input$poli_checks_rows_selected)
    r <- poli_check_results()
    i <- input$poli_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[poli_check_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE, filter = "top",
                  options = list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                                order = list(list(0, "asc"))))
  })

  # --- Daily MP counts graph (RESE_MP tab) ---

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

    ggplot(df, aes(x = date)) +
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
    for (i in seq_len(nrow(episodes))) {
      rows <- dc[dc$date >= episodes$start_date[i] & dc$date <= episodes$end_date[i], ]
      excess <- rows$n_seated - rows$parliament_size
      episodes$peak_excess[i]    <- max(excess)
      episodes$mean_excess[i]    <- round(mean(excess), 1)
      episodes$parliament_size[i] <- rows$parliament_size[1]
    }
    episodes
  })

  output$overcount_detail <- renderUI({
    req(input$daily_plot_click)
    eps <- overcount_episodes()
    if (is.null(eps) || nrow(eps) == 0) return(NULL)

    clicked_date <- as.Date(input$daily_plot_click$x, origin = "1970-01-01")

    # Distance from click to each episode (0 if inside)
    dist_to_ep <- pmax(
      as.numeric(eps$start_date - clicked_date),
      as.numeric(clicked_date - eps$end_date),
      0
    )
    nearest <- which.min(dist_to_ep)
    if (dist_to_ep[nearest] > 365) return(NULL)

    ep <- eps[nearest, ]

    # Find RESE entries ending on the episode end date
    cc <- input$country_select
    rese_ending <- RESE[RESE$country_abb == cc &
                          RESE$political_function %in%
                            c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09") &
                          !is.na(RESE$end_date) &
                          RESE$end_date == ep$end_date,
                        c("res_entry_id", "pers_id", "res_entry_start", "res_entry_end",
                          "political_function", "parliament_id")]

    # Determine parliament_id for the overcount episode (use midpoint)
    ep_mid <- ep$start_date + as.integer((ep$end_date - ep$start_date) / 2)
    parl_cc <- PARL[PARL$country_abb == cc & PARL$level == "NT" &
                      PARL$assembly_abb == assembly_map[[cc]], ]
    parl_cc <- parl_cc[order(parl_cc$leg_period_start_date), ]
    parl_idx <- max(which(parl_cc$leg_period_start_date <= ep_mid), 0)
    ep_parl_id <- if (parl_idx > 0) parl_cc$parliament_id[parl_idx] else "unknown"

    path <- issue_path(cc, "RESE", "overcount", ep_parl_id)
    auto_summary <- build_overcount_summary(ep, rese_ending)

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
          DT::renderDataTable(
            DT::datatable(rese_ending, rownames = FALSE,
                          options = list(scrollX = TRUE, pageLength = 10, dom = "tip",
                                order = list(list(0, "asc"))))
          )
        )
      },
      issue_path_tag(path, auto_summary)
    )
  })

  # --- POLI tab ---

  poli_vars <- reactive({
    cc <- tolower(input$country_select)
    id_cols <- grep(paste0("^id_", cc, "_"), names(POLI), value = TRUE, ignore.case = TRUE)
    c(poli_base_vars, sort(id_cols))
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

    p +
      labs(
        title    = paste0("Completeness: ", selected_var, " \u2014 ", country_name),
        subtitle = "% of first-day cohort MPs for whom the variable is available; dashed red line = 97.5% target"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
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

  output$poli_missing_header <- renderUI({
    req(input$poli_completeness_rows_selected)
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    d_mp <- filtered() |> filter(pers_id %in% ever_mp_ids())
    missing <- d_mp[is.na(d_mp[[selected_var]]) | d_mp[[selected_var]] == "", ]
    if (nrow(missing) == 0) {
      return(tags$p(style = "color:#28a745; font-weight:bold; margin-top:8px;",
                    paste0("All MPs have ", selected_var, " available.")))
    }
    tags$p(style = "font-weight:bold; color:#c0392b; margin-top:8px;",
           paste0("MPs missing ", selected_var, " (", nrow(missing), "):"))
  })

  output$poli_missing_dt <- DT::renderDT({
    req(input$poli_completeness_rows_selected)
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    d_mp <- filtered() |> filter(pers_id %in% ever_mp_ids())
    missing <- d_mp[is.na(d_mp[[selected_var]]) | d_mp[[selected_var]] == "", ]
    req(nrow(missing) > 0)
    show_cols <- c("pers_id", setdiff(poli_vars(), selected_var))
    DT::datatable(missing[, show_cols, drop = FALSE], rownames = FALSE, filter = "top",
                  options = list(scrollX = TRUE, pageLength = 10, dom = "ltip",
                                order = list(list(0, "asc"))))
  })

  output$poli_completeness_issue_path <- renderUI({
    req(input$poli_completeness_rows_selected)
    selected_var <- poli_vars()[input$poli_completeness_rows_selected]
    d_mp <- filtered() |> filter(pers_id %in% ever_mp_ids())
    missing <- d_mp[is.na(d_mp[[selected_var]]) | d_mp[[selected_var]] == "", ]
    show_cols <- c("pers_id", intersect(c("last_name", "first_name"), names(missing)))
    missing_preview <- missing[, show_cols, drop = FALSE]
    auto_summary <- build_completeness_summary(selected_var, missing_preview)
    path <- issue_path(input$country_select, "POLI", "completeness", selected_var)
    issue_path_tag(path, auto_summary)
  })

}

shinyApp(ui, server)
