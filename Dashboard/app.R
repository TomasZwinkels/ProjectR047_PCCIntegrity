for (pkg in c("shiny", "dplyr", "ggplot2", "DT")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_MEME_functions.R")

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

poli_vars <- c("last_name", "first_name", "birth_date", "birth_place_raw")

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
      political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")
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

run_rese_checks <- function(cc) {
  rese_mp <- suppressMessages(preprocess_RESEdates(
    RESE[RESE$country_abb == cc, ]
  ))
  rese_mp <- rese_mp[rese_mp$political_function %in%
                       c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01"), ]

  labels <- c(
    "All RESE person IDs exist in POLI",
    "All resume entry IDs are unique",
    "All RESE dates parsed successfully",
    "No fully overlapping parl. episodes",
    "No near-overlapping episodes (\u22642 days)",
    "No same-birthday duplicates in factions"
  )
  details <- list(
    check_RESE_persid_in_POLI_details(rese_mp, POLI),
    check_RESE_resentryid_unique_details(rese_mp),
    check_anyNAinRESEdates_details(rese_mp),
    check_RESE_parlmemeppisodes_anyfulloverlap_details(rese_mp),
    check_RESE_anynear_fulloverlap_details(rese_mp, tolerance_days = 2),
    check_RESE_duplicate_birthdates_in_faction_details(
      rese_mp, POLI, PARL, MEME, assembly_map[[cc]],
      verified_pairs = verified_not_duplicates)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

rese_detail_keys <- c(
  "missing_rows", "duplicate_rows", "full_rows_with_na_dates",
  "overlapping_episodes", "full_episode_pairs_near_overlapping", "flagged_pairs"
)

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

run_meme_checks <- function(cc) {
  meme <- suppressMessages(preprocess_MEMEdates(
    MEME[substr(MEME$pers_id, 1, nchar(cc)) == cc, ]
  ))
  rese_mp <- RESE[RESE$country_abb == cc &
                    RESE$political_function %in%
                      c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01"), ]

  labels <- c(
    "All MEME person IDs exist in POLI",
    "All MEME party IDs exist in PART",
    "All MEME episode IDs are unique",
    "All MEME start dates parsed successfully",
    "No inverted MEME dates",
    "No duplicate MEME episodes",
    "All MPs have party membership data"
  )
  details <- list(
    check_MEME_persid_in_POLI_details(meme, POLI),
    check_MEME_partyid_in_PART_details(meme, PART),
    check_MEME_memepid_unique_details(meme),
    check_anyNAinMEMEdates_details(meme),
    check_MEME_inverted_dates_details(meme),
    check_MEME_anyfulloverlap_details(meme),
    check_MEME_parlmembers_have_party_details(rese_mp, meme)
  )
  list(
    table   = checks_table(labels, sapply(details, `[[`, "check_passed")),
    details = details
  )
}

meme_detail_keys <- c(
  "missing_rows", "missing_rows", "duplicate_rows",
  "full_rows_with_na_startdates", "inverted_rows",
  "overlapping_episodes", "missing_rese_rows"
)

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
detail_header_ui <- function(result, row_idx, key_vec, dt_output_id) {
  status <- result$table$Status[row_idx]
  label  <- result$table$Check[row_idx]

  if (status == "PASS") {
    return(tagList(
      tags$hr(),
      tags$p(style = "color:#28a745; font-weight:bold;",
             paste0("\u2713 ", label, " — no issues found."))
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
      DT::DTOutput(dt_output_id)
  )
}

# ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("R047 PCC Data Dashboard"),
  fluidRow(
    column(2,
      selectInput(
        inputId  = "country_select",
        label    = "Country",
        choices  = all_countries,
        selected = all_countries[1],
        multiple = FALSE
      )
    ),
    column(4,
      dateRangeInput(
        inputId = "date_range",
        label   = "Date range",
        start   = "1946-01-01",
        end     = "2025-12-31",
        format  = "yyyy-mm-dd"
      )
    )
  ),
  tabsetPanel(
    tabPanel("RESE",
      DT::dataTableOutput("rese_checks"),
      tags$small(style = "color:#666; margin-top:4px; display:block;",
        "Checks run on country-filtered RESE data (parliamentary membership episodes only), matching R047.R logic."),
      uiOutput("rese_detail")
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
      uiOutput("poli_plot_note")
    )
  )
)

# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  rese_check_results <- reactive({ run_rese_checks(input$country_select) })
  parl_check_results <- reactive({ run_parl_checks(input$country_select) })
  meme_check_results <- reactive({ run_meme_checks(input$country_select) })

  output$rese_checks <- DT::renderDataTable({ checks_dt(rese_check_results()$table) })
  output$parl_checks <- DT::renderDataTable({ checks_dt(parl_check_results()$table) })
  output$meme_checks <- DT::renderDataTable({ checks_dt(meme_check_results()$table) })

  output$rese_detail <- renderUI({
    req(input$rese_checks_rows_selected)
    detail_header_ui(rese_check_results(), input$rese_checks_rows_selected,
                     rese_detail_keys, "rese_detail_dt")
  })
  output$rese_detail_dt <- DT::renderDT({
    req(input$rese_checks_rows_selected)
    r <- rese_check_results()
    i <- input$rese_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[rese_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  output$parl_detail <- renderUI({
    req(input$parl_checks_rows_selected)
    detail_header_ui(parl_check_results(), input$parl_checks_rows_selected,
                     parl_detail_keys, "parl_detail_dt")
  })
  output$parl_detail_dt <- DT::renderDT({
    req(input$parl_checks_rows_selected)
    r <- parl_check_results()
    i <- input$parl_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[parl_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  output$meme_detail <- renderUI({
    req(input$meme_checks_rows_selected)
    detail_header_ui(meme_check_results(), input$meme_checks_rows_selected,
                     meme_detail_keys, "meme_detail_dt")
  })
  output$meme_detail_dt <- DT::renderDT({
    req(input$meme_checks_rows_selected)
    r <- meme_check_results()
    i <- input$meme_checks_rows_selected
    req(r$table$Status[i] == "FAIL")
    df <- r$details[[i]][[meme_detail_keys[i]]]
    req(!is.null(df) && nrow(df) > 0)
    DT::datatable(df, rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  filtered <- reactive({
    POLI |> filter(country == input$country_select)
  })

  ever_mp_ids <- reactive({
    RESE |>
      filter(
        country_abb == input$country_select,
        political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")
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

    df <- data.frame(
      Variable          = poli_vars,
      `N (all)`         = nrow(d),
      `% Avail (all)`   = c(
        round(mean(avail(d$last_name))       * 100, 1),
        round(mean(avail(d$first_name))      * 100, 1),
        round(mean(avail(d$birth_date))      * 100, 1),
        round(mean(avail(d$birth_place_raw)) * 100, 1)
      ),
      `N (MPs)`         = nrow(d_mp),
      `% Avail (MPs)`   = c(
        round(mean(avail(d_mp$last_name))       * 100, 1),
        round(mean(avail(d_mp$first_name))      * 100, 1),
        round(mean(avail(d_mp$birth_date))      * 100, 1),
        round(mean(avail(d_mp$birth_place_raw)) * 100, 1)
      ),
      check.names = FALSE
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

    selected_var <- poli_vars[input$poli_completeness_rows_selected]

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

}

shinyApp(ui, server)
