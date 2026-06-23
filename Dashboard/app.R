for (pkg in c("shiny", "dplyr", "ggplot2", "DT")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_RESE_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_PARL_functions.R")

# Load data once at startup
POLI <- read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";") |>
  mutate(country = substr(pers_id, 1, 2))

RESE <- read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
PARL <- read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")

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
    tabPanel(
      "POLI",
      DT::dataTableOutput("poli_completeness"),
      plotOutput("poli_plot"),
      uiOutput("poli_plot_note")
    )
  )
)

# ---------------------------------------------------------------------------

server <- function(input, output, session) {

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
        n_seated     = n(),
        pct_complete = round(100 * mean(available), 1),
        .groups      = "drop"
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

    segs <- no_data_segs()

    country_name <- country_labels[input$country_select]
    if (is.na(country_name)) country_name <- input$country_select

    p <- ggplot(completeness, aes(x = snapshot_day, y = pct_complete)) +
      geom_vline(xintercept = parl_starts, color = "gray70", alpha = 0.6, linewidth = 0.3) +
      geom_text(
        data  = parl_years,
        aes(x = date, y = 2, label = year),
        angle = 90, size = 4, color = "gray50", hjust = 0, vjust = 0.5
      ) +
      geom_line(color = "steelblue", linewidth = 0.8) +
      geom_point(aes(color = below_threshold), size = 2.5) +
      scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"), guide = "none") +
      geom_hline(yintercept = 100,  linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = 97.5, linetype = "dashed", color = "tomato", linewidth = 0.8) +
      scale_x_date(name = "Parliament start date", limits = c(date_from, date_to)) +
      scale_y_continuous(name = "% available", limits = c(0, 105)) +
      labs(
        title    = paste0("Completeness: ", selected_var, " \u2014 ", country_name),
        subtitle = "% of first-day cohort MPs for whom the variable is available; dashed red line = 97.5% target"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )

    if (!is.null(segs) && nrow(segs) > 0) {
      p <- p + geom_segment(
        data        = segs,
        aes(x = x, xend = xend, y = 0, yend = 0),
        color       = "orange",
        linewidth   = 1.2,
        inherit.aes = FALSE
      )
    }

    p
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
      style = "color: #c0392b; font-size: 0.85em; margin-top: 4px;",
      htmltools::tags$b("Note:"),
      paste0(
        "The orange line indicates period(s) with no parliamentary membership data: ",
        paste(periods, collapse = "; "), "."
      )
    )
  })

}

shinyApp(ui, server)
