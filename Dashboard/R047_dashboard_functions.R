# R047_dashboard_functions.R
# Functions used exclusively by the R047 dashboard (Dashboard/app.R):
# GitHub issue integration, LLM (Codex) helpers, and CSV export.
# Pure logic — no Shiny dependencies. Testable independently.
# Requires format_pcc_date() from R047_functions.R (sourced by app.R).

# --- CSV export ---

# Writes a data.frame to a semicolon-delimited CSV matching the PCCdata
# format (source files are read with sep = ";"). NA is written as an empty
# string so exports round-trip cleanly, and the file is UTF-8 encoded.
#
# The file is prefixed with a UTF-8 BOM and an Excel `sep=;` preamble line so
# that double-clicking it in a European Excel (list separator = ";") lands the
# data in the right columns and renders accented names (e.g. Ellemeet, Groningen
# with a diaeresis) correctly. The `sep=;` line is an Excel-specific hint, not
# part of the CSV standard; read the file back with read_csv_with_excel_sep()
# (R047_functions.R), which skips the preamble and strips the BOM.
#
# `file` is a path. (Both call sites pass a tempfile / downloadHandler path.)
#
# Because the file starts with a UTF-8 BOM, every byte written after it must be
# valid UTF-8 \u2014 source data occasionally carries stray latin1 bytes (e.g. a
# 0xE7 "\u00e7" in a POLI first_name broke the issue-#16 attachment: the then-current
# write.table(fileEncoding=) conversion truncated mid-field with only a
# warning, leaving an unbalanced quote that made quote-aware parsers silently
# drop ~450 following rows). All text is therefore repaired to valid UTF-8
# before writing.
write_pcc_csv <- function(df, file) {
  text_cols <- vapply(df, function(col) is.character(col) || is.factor(col),
                      logical(1))
  df[text_cols] <- lapply(df[text_cols],
                          function(col) repair_utf8(as.character(col)))
  names(df) <- repair_utf8(names(df))

  con <- base::file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\ufeff"), con)          # UTF-8 BOM
  writeLines("sep=;", con, useBytes = TRUE)   # Excel separator hint
  utils::write.table(df, con, sep = ";", row.names = FALSE, na = "",
                     qmethod = "double", fileEncoding = "UTF-8")
}

# Repair a character vector to valid UTF-8. Valid strings (and NA) pass
# through unchanged; strings with a declared latin1/native encoding are
# converted by enc2utf8(); any string still holding invalid byte sequences is
# reinterpreted as latin1 \u2014 every byte value is defined in latin1, so this
# never fails or drops data, and it recovers the common legacy case (single
# high bytes like 0xE7 "\u00e7" from pre-UTF-8 sources).
repair_utf8 <- function(x) {
  x <- enc2utf8(x)
  invalid <- !is.na(x) & !validUTF8(x)
  if (any(invalid)) x[invalid] <- iconv(x[invalid], from = "latin1", to = "UTF-8")
  x
}

# --- Issue identifier vectors (one per check, matching check order) ---
rese_check_ids <- c(
  "persid_in_POLI", "resentryid_unique", "dates_parsed",
  "full_overlap", "near_overlap", "birthday_duplicates",
  "parliament_id_dates",
  "parlmem_coverage", "coverage_at_date_from", "coverage_at_date_to",
  "special_chars"
)

parl_check_ids <- c("dates_parsed", "parliament_size_valid", "special_chars")

meme_check_ids <- c(
  "persid_in_POLI", "partyid_in_PART", "memepid_unique",
  "dates_parsed", "inverted_dates", "full_overlap",
  "parlmembers_have_party", "same_party_overlap_during_parliament",
  "diff_party_overlap_during_parliament", "special_chars"
)

poli_check_ids <- c("persid_unique", "birthdate_jan01_excess", "gender_valid",
                    "special_chars")

# --- Curated column views for the problem-row tables ---

# Primary key column per PCC data frame (used as the default-column fallback)
frame_primary_keys <- list(RESE = "res_entry_id", PARL = "parliament_id",
                           MEME = "memep_id",     POLI = "pers_id")

# Default visible columns per check, keyed by check-id slug (NOT by detail
# key — detail keys are duplicated across checks and cannot disambiguate).
# Every entry is intersect()ed with the actual names(df) at resolve time, so
# entries are robust to df shape drift; unmapped checks fall back to
# pers_id + the frame's primary key.
detail_default_cols_map <- list(
  RESE = list(
    persid_in_POLI        = c("res_entry_id", "pers_id", "res_entry_start",
                              "res_entry_end", "political_function"),
    resentryid_unique     = c("res_entry_id", "pers_id", "res_entry_start",
                              "res_entry_end", "political_function"),
    dates_parsed          = c("res_entry_id", "pers_id", "res_entry_start",
                              "res_entry_end", "res_entry_raw",
                              "start_date", "end_date"),
    full_overlap          = c("res_entry_id", "pers_id", "res_entry_start",
                              "res_entry_end", "political_function",
                              "parliament_id"),
    # Pair-joined df with .x/.y suffixes
    near_overlap          = c("pers_id", "res_entry_id.x", "res_entry_start.x",
                              "res_entry_end.x", "res_entry_id.y",
                              "res_entry_start.y", "res_entry_end.y",
                              "start_diff_days", "end_diff_days"),
    birthday_duplicates   = c("pers_id_1", "name_1", "pers_id_2", "name_2",
                              "birth_date", "party_id", "parliament_id"),
    # RESE rows + leading diagnosis columns from the parliament_id-vs-dates check
    parliament_id_dates   = c("res_entry_id", "pers_id", "res_entry_start",
                              "res_entry_end", "parliament_id",
                              "expected_parliament_ids", "mismatch_type",
                              "days_start_vs_period", "days_end_vs_period"),
    # PARL-shaped rows + n_seated
    parlmem_coverage      = c("parliament_id", "leg_period_start",
                              "leg_period_end", "assembly_abb",
                              "parliament_size", "n_seated"),
    # Boundary episodes: the RESE rows marking where the data stops/resumes
    coverage_at_date_from = c("boundary_side", "res_entry_id", "pers_id",
                              "res_entry_start", "res_entry_end",
                              "political_function", "parliament_id"),
    coverage_at_date_to   = c("boundary_side", "res_entry_id", "pers_id",
                              "res_entry_start", "res_entry_end",
                              "political_function", "parliament_id"),
    # Long-format special-char table: one row per offending cell
    special_chars         = c("res_entry_id", "pers_id",
                              "column", "value", "bad_chars")
  ),
  PARL = list(
    dates_parsed          = c("parliament_id", "leg_period_start",
                              "leg_period_end", "day_of_first_session"),
    parliament_size_valid = c("parliament_id", "leg_period_start",
                              "leg_period_end", "parliament_size",
                              "assembly_abb"),
    special_chars         = c("parliament_id", "assembly_abb",
                              "column", "value", "bad_chars")
  ),
  MEME = list(
    persid_in_POLI  = c("memep_id", "pers_id", "party_id",
                        "memep_startdate", "memep_enddate"),
    partyid_in_PART = c("memep_id", "pers_id", "party_id",
                        "memep_startdate", "memep_enddate"),
    memepid_unique  = c("memep_id", "pers_id", "party_id",
                        "memep_startdate", "memep_enddate"),
    dates_parsed    = c("memep_id", "pers_id", "memep_startdate",
                        "memep_enddate", "memep_type_raw"),
    inverted_dates  = c("memep_id", "pers_id", "memep_startdate", "memep_enddate"),
    full_overlap    = c("memep_id", "pers_id", "party_id",
                        "memep_startdate", "memep_enddate"),
    # RESE-shaped rows
    parlmembers_have_party = c("res_entry_id", "pers_id", "res_entry_start",
                               "res_entry_end", "political_function", "parliament_id"),
    same_party_overlap_during_parliament =
      c("pers_id", "memep_id_1", "memep_id_2", "party_id_1", "party_id_2",
        "overlap_start", "overlap_end", "one_day", "parliament_id"),
    diff_party_overlap_during_parliament =
      c("pers_id", "memep_id_1", "memep_id_2", "party_id_1", "party_id_2",
        "overlap_start", "overlap_end", "one_day", "parliament_id"),
    special_chars   = c("memep_id", "pers_id", "party_id",
                        "column", "value", "bad_chars")
  ),
  POLI = list(
    # id_<cc>_* columns are appended automatically for frame == "POLI"
    persid_unique          = c("pers_id", "last_name", "first_name", "birth_date", "wikidata_id"),
    birthdate_jan01_excess = c("pers_id", "last_name", "first_name", "birth_date"),
    gender_valid           = c("pers_id", "last_name", "first_name", "gender"),
    special_chars          = c("pers_id", "last_name", "first_name",
                               "column", "value", "bad_chars")
  )
)

# Overcount table (not a check): today's 6 hardcoded columns become the default view
overcount_default_cols <- c("res_entry_id", "pers_id", "res_entry_start",
                            "res_entry_end", "political_function", "parliament_id")

# The RESE political_function codes counted as parliamentary membership.
# Kept in one place so every over/undercount-detail set matches
# build_daily_counts() (which must use the same four codes).
mp_pf_codes <- c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09",
                 "NT_LE_T3_NA_11")

# Columns shown in the overcount context tables/CSVs (opening, peak roster,
# present-throughout). Wider than overcount_default_cols: adds name + birth_date
# so a reader can spot duplicate persons at a glance.
overcount_context_cols <- c("res_entry_id", "pers_id", "last_name", "first_name",
                            "birth_date", "res_entry_start", "res_entry_end",
                            "start_date", "end_date",
                            "political_function", "parliament_id")

# RESE MP rows for one country: the base for every overcount-detail set.
rese_mp_rows <- function(rese, cc) {
  rese[rese$country_abb == cc & rese$political_function %in% mp_pf_codes, ]
}

# Rows seated on a given day. Same predicate that builds n_seated in
# build_daily_counts(): started on/before the day and not yet ended (open-ended
# entries count as still seated). NA start never counts.
seated_on <- function(rese_cc, day) {
  rese_cc[!is.na(rese_cc$start_date) & rese_cc$start_date <= day &
            (is.na(rese_cc$end_date) | rese_cc$end_date >= day), , drop = FALSE]
}

# Left-join last_name/first_name/birth_date from POLI (deduped on pers_id, like
# join_poli_ids) so overcount sets carry enough identity to eyeball duplicates.
join_poli_bio <- function(df, poli) {
  if (is.null(df) || !"pers_id" %in% names(df)) return(df)
  cols <- setdiff(intersect(c("last_name", "first_name", "birth_date"),
                            names(poli)), names(df))
  if (length(cols) == 0) return(df)
  bio <- poli[!duplicated(poli$pers_id), c("pers_id", cols), drop = FALSE]
  dplyr::left_join(df, bio, by = "pers_id")
}

# Rows of a (bio-joined) roster whose birth_date is shared by another row: a
# strong "two records, one person" signal. Blank/NA dates are ignored. Returns
# the subset sorted by birth_date so duplicates sit together.
birthdate_clusters <- function(roster) {
  if (is.null(roster) || nrow(roster) == 0 || !"birth_date" %in% names(roster))
    return(roster[0, , drop = FALSE])
  bd  <- as.character(roster$birth_date)
  ok  <- !is.na(bd) & bd != ""
  dup <- ok & bd %in% bd[ok][duplicated(bd[ok])]
  out <- roster[dup, , drop = FALSE]
  out[order(as.character(out$birth_date)), , drop = FALSE]
}

# Country-specific POLI identifier columns, e.g. "NL" -> id_nl_pdc_num, ...
country_id_cols <- function(col_names, cc) {
  sort(grep(paste0("^id_", tolower(cc), "_"), col_names,
            value = TRUE, ignore.case = TRUE))
}

# Resolve the default visible columns for a problem-row table.
# frame: "RESE"|"PARL"|"MEME"|"POLI"; check_id: slug from <frame>_check_ids;
# df_names: names() of the (augmented) detail df; country_ids: id_<cc>_* cols.
# Fallback for unmapped checks (or entries sharing no names with the df):
# pers_id + frame primary key present in df, else the first 6 columns.
default_detail_cols <- function(frame, check_id, df_names, country_ids = character(0)) {
  wanted <- detail_default_cols_map[[frame]][[check_id]]
  if (is.null(wanted)) wanted <- character(0)
  cols <- intersect(wanted, df_names)
  if (identical(frame, "POLI"))
    cols <- c(cols, intersect(country_ids, df_names))
  if (length(cols) == 0) {
    cols <- intersect(c("pers_id", frame_primary_keys[[frame]]), df_names)
    if (length(cols) == 0) cols <- utils::head(df_names, 6)
  }
  unique(cols)
}

# Default columns for the POLI-missing table. Deliberately INCLUDES the selected
# (missing) variable so the table and its export are self-documenting.
poli_missing_default_cols <- function(selected_var, df_names, country_ids) {
  unique(intersect(c("pers_id", "last_name", "first_name",
                     selected_var, country_ids), df_names))
}

# Left-join the country's id_<cc>_* identifier columns from POLI onto a detail df.
# No-op when df lacks pers_id (PARL rows, coverage snapshot) or already has the id
# columns (POLI-shaped dfs). POLI is deduplicated on pers_id first: duplicate
# pers_ids (exactly what the persid_unique check detects) must not explode rows,
# or DT's rows_all indices would desynchronize from the rendered table.
join_poli_ids <- function(df, poli, cc) {
  if (is.null(df) || !"pers_id" %in% names(df)) return(df)
  id_cols <- setdiff(country_id_cols(names(poli), cc), names(df))
  if (length(id_cols) == 0) return(df)
  poli_ids <- poli[!duplicated(poli$pers_id), c("pers_id", id_cols), drop = FALSE]
  dplyr::left_join(df, poli_ids, by = "pers_id")
}

# Guard for "export what you see": drop row indices that are out of bounds for the
# current df (a stale rows_all can arrive if the user exports mid-table-switch),
# preserving order. integer(0) in -> integer(0) out (header-only CSV).
clamp_export_rows <- function(rows, n) {
  rows[rows >= 1 & rows <= n]
}

# Default GitHub settings
github_defaults <- list(
  repo       = "TomasZwinkels/PCCdata",
  asset_repo = "TomasZwinkels/pcc-issue-assets"
)

# Default label colours by hierarchy level (country, dataframe, issue_type, identifier)
github_label_colours <- c("#1d76db", "#0e8a16", "#e4e669", "#d93f0b")

# Build a hierarchical issue identifier path
issue_path <- function(country, dataframe, issue_type, identifier) {
  paste(country, dataframe, issue_type, identifier, sep = " / ")
}

# Parse an issue path back into its label components
issue_path_to_labels <- function(path) {
  trimws(strsplit(path, "\\s*/\\s*")[[1]])
}

# Convert a data.frame (first N rows) to a markdown table string
df_to_md_table <- function(df, max_rows = 10) {
  if (is.null(df) || nrow(df) == 0) return("")
  df <- head(df, max_rows)
  df[] <- lapply(df, function(col) {
    col <- as.character(col)
    ifelse(nchar(col) > 40, paste0(substr(col, 1, 37), "..."), col)
  })
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows   <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

# Compact read-only HTML table for the overcount context sets shown on screen
# (the interactive DT stays reserved for the closing set). Caps rows and notes
# the overflow so a big peak-day roster does not blow up the panel.
df_to_html_table <- function(df, max_rows = 15) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  n_total <- nrow(df)
  df <- head(df, max_rows)
  cell <- function(x, tag) {
    lapply(x, function(v) tag(as.character(v)))
  }
  header <- htmltools::tags$tr(cell(names(df), function(v)
    htmltools::tags$th(style = "text-align:left; padding:2px 8px; border-bottom:1px solid #ccc;", v)))
  body <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(cell(unlist(lapply(df[i, , drop = FALSE], as.character)),
      function(v) htmltools::tags$td(style = "padding:2px 8px; border-bottom:1px solid #eee;",
                                     ifelse(is.na(v) | v == "", "–", v))))
  })
  htmltools::tagList(
    htmltools::tags$table(
      style = "border-collapse:collapse; font-size:0.82em; font-family:monospace; margin:2px 0 6px;",
      htmltools::tags$thead(header),
      htmltools::tags$tbody(body)
    ),
    if (n_total > max_rows) htmltools::tags$p(
      style = "font-size:0.8em; color:#888; margin:0 0 6px;",
      paste0("… ", n_total - max_rows, " more row(s); full set in the attached CSV."))
  )
}

# Build auto-summary for check failures
build_check_summary <- function(detail_result, row_idx, key_vec) {
  det <- detail_result$details[[row_idx]]
  df  <- det[[key_vec[row_idx]]]
  n   <- if (!is.null(df)) nrow(df) else 0
  check_name <- detail_result$table$Check[row_idx]

  lines <- c(
    paste0("**Check:** ", check_name),
    paste0("**Status:** ", detail_result$table$Status[row_idx]),
    paste0("**Problem rows:** ", n)
  )
  # Checks can expose a named vector of key facts (det$summary_stats); render
  # them prominently so issue descriptions (and their LLM generator) see the
  # actual diagnosis, not just a row count.
  if (!is.null(det$summary_stats) && length(det$summary_stats) > 0) {
    lines <- c(lines, "", "**Key facts:**",
               paste0("- **", names(det$summary_stats), ":** ",
                      unname(det$summary_stats)))
  }
  if (n > 0) {
    lines <- c(lines, "",
               paste0("**First ", min(n, 10), " of ", n, " rows:**"),
               "", df_to_md_table(df))
  }
  paste(lines, collapse = "\n")
}

# Build auto-summary for completeness issues
# completeness_ts: optional data.frame with parliament_id, snapshot_day,
#   pct_complete columns (the time series behind the graph)
build_completeness_summary <- function(variable, missing_df,
                                       completeness_ts = NULL) {
  n <- nrow(missing_df)
  lines <- c(
    paste0("**Variable:** `", variable, "`"),
    paste0("**Missing for:** ", n, " MPs")
  )
  if (n > 0) {
    lines <- c(lines, "",
               paste0("**First ", min(n, 10), " of ", n, " MPs:**"),
               "", df_to_md_table(missing_df))
  }
  if (!is.null(completeness_ts) && nrow(completeness_ts) > 0) {
    ts_display <- data.frame(
      parliament = completeness_ts$parliament_id,
      year       = format(completeness_ts$snapshot_day, "%Y"),
      pct        = completeness_ts$pct_complete
    )
    lines <- c(lines, "",
               "**Completeness over time (per parliament start):**",
               "", df_to_md_table(ts_display, max_rows = 50))
  }
  paste(lines, collapse = "\n")
}

# Build auto-summary for overcount episodes
# Requires format_pcc_date() from R047_functions.R
# rese_ending  : the "closing" set (end_date == episode end) -- current behaviour
# opening      : entries whose start_date falls within the episode window
# throughout   : entries seated on every day of the episode
# peak_roster  : everyone seated on the peak (max-excess) day
# birthdate_dupes : peak-roster rows sharing a birth_date with another member
# all_episodes : optional data.frame with all overcount episodes for context
build_overcount_summary <- function(ep, rese_ending,
                                    all_episodes = NULL,
                                    opening = NULL, throughout = NULL,
                                    peak_roster = NULL, birthdate_dupes = NULL) {
  lines <- c(
    paste0("**Episode:** ", format_pcc_date(ep$start_date), " -- ",
           format_pcc_date(ep$end_date), " (", ep$duration_days, " days)"),
    paste0("**Official parliament size:** ", ep$parliament_size),
    paste0("**Peak excess:** +", ep$peak_excess,
           "  |  Mean excess: +", ep$mean_excess,
           if (!is.null(ep$peak_date) && !is.na(ep$peak_date))
             paste0("  |  Peak day: ", format_pcc_date(ep$peak_date)) else "")
  )
  if (nrow(rese_ending) > 0) {
    lines <- c(lines, "",
               paste0("**Closing set -- RESE entries ending on ",
                      format_pcc_date(ep$end_date), " (", nrow(rese_ending),
                      "):** their departure is what ends the overcount."),
               "", df_to_md_table(rese_ending))
  }
  if (!is.null(opening) && nrow(opening) > 0) {
    lines <- c(lines, "",
               paste0("**Opening set -- entries starting within the episode (",
                      nrow(opening), "):** an arrival too early can trigger the ",
                      "overcount."),
               "", df_to_md_table(opening))
  }
  if (!is.null(throughout) && nrow(throughout) > 0) {
    open_ended <- sum(is.na(throughout$end_date))
    lines <- c(lines, "",
               paste0("**Present throughout the episode (", nrow(throughout),
                      if (open_ended > 0)
                        paste0("; ", open_ended, " open-ended, i.e. no end_date")
                      else "",
                      "):** candidates for a *persistent* overcount, especially ",
                      "any stray open-ended record."),
               "", df_to_md_table(throughout))
  }
  if (!is.null(peak_roster) && nrow(peak_roster) > 0) {
    lines <- c(lines, "",
               paste0("**Peak-day roster:** ", nrow(peak_roster),
                      " seated vs ", ep$parliament_size, " official on ",
                      format_pcc_date(ep$peak_date),
                      " (full roster in the attached peak CSV)."))
  }
  if (!is.null(birthdate_dupes) && nrow(birthdate_dupes) > 0) {
    lines <- c(lines, "",
               paste0("**Shared birth dates on the peak day (",
                      nrow(birthdate_dupes),
                      "):** members sharing a birth_date -- a strong ",
                      "\"same person, two records\" signal."),
               "", df_to_md_table(birthdate_dupes))
  }
  lines <- c(lines, "",
             paste0("_Interpretation: an overcount is too many overlapping ",
                    "tenures. Likely error classes -- a late/ missing end_date, ",
                    "a too-early start_date, or one person recorded under two ",
                    "pers_ids. When possible, name the specific pers_id(s)._"))
  if (!is.null(all_episodes) && nrow(all_episodes) > 0) {
    ep_display <- data.frame(
      start      = sapply(all_episodes$start_date, format_pcc_date),
      end        = sapply(all_episodes$end_date, format_pcc_date),
      days       = all_episodes$duration_days,
      peak       = paste0("+", all_episodes$peak_excess),
      parl_size  = all_episodes$parliament_size
    )
    lines <- c(lines, "",
               paste0("**All overcount episodes for this parliament (",
                      nrow(all_episodes), "):**"),
               "", df_to_md_table(ep_display, max_rows = 30))
  }
  paste(lines, collapse = "\n")
}

# --- Structural-undercount drill-down (RESE_MP tab) --------------------------
# Detection lives in R047_quality_goals.R (per-legislature chronic/acute stats
# vs the country's vacancy baseline). These helpers diagnose a legislature the
# detector flagged: localize the deficit into contiguous runs, pull the RESE
# entries around the worst window, and classify the episode's shape.

# Contiguous runs of days whose relative deficit exceeds `threshold` within a
# legislature's daily slice. Returns one row per run with seat-level context.
undercount_runs <- function(daily_leg, threshold) {
  empty <- data.frame(start_date = as.Date(character(0)),
                      end_date = as.Date(character(0)),
                      days = integer(0), peak_deficit = integer(0),
                      mean_deficit = numeric(0))
  if (is.null(daily_leg) || nrow(daily_leg) == 0) return(empty)
  over <- daily_leg$rel_deficit > threshold
  runs <- rle(over)
  ends   <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  keep   <- which(runs$values)
  out <- lapply(keep, function(k) {
    d <- daily_leg[starts[k]:ends[k], , drop = FALSE]
    deficit <- d$parliament_size - d$n_seated
    data.frame(start_date = d$date[1], end_date = d$date[nrow(d)],
               days = nrow(d), peak_deficit = max(deficit),
               mean_deficit = round(mean(deficit), 1))
  })
  if (length(out) == 0) return(empty)
  df <- do.call(rbind, out)
  df[order(-df$days), , drop = FALSE]
}

# RESE entries around the worst (acute) window of a flagged legislature:
#   departure : entries ending in [win_start - margin, win_end] — the wave
#               whose (possibly wrong/truncated) end dates open the hole
#   arrival   : entries starting in [win_start, win_end + margin] — the wave
#               that eventually fills it; a late arrival wave at the
#               legislature opening is the start-date-convention signature
# `base` is a (bio-joined) rese_mp_rows() frame.
undercount_wave_sets <- function(base, win_start, win_end, margin = 14) {
  departure <- base[!is.na(base$end_date) &
                      base$end_date >= win_start - margin &
                      base$end_date <= win_end, , drop = FALSE]
  arrival <- base[!is.na(base$start_date) &
                    base$start_date >= win_start &
                    base$start_date <= win_end + margin, , drop = FALSE]
  list(departure = departure[order(departure$end_date), , drop = FALSE],
       arrival   = arrival[order(arrival$start_date), , drop = FALSE])
}

# Classify the shape of a flagged legislature's worst window into suspected
# cause(s). Multiple tags can apply (e.g. an early dissolution is near-total
# AND sits at the legislature end). `tol` = days of slack at the boundaries.
undercount_shape_label <- function(leg_start, leg_end, win_start, win_end,
                                   win_mean_rel, coverage_end = as.Date(NA),
                                   tol = 7) {
  tags <- character(0)
  if (win_mean_rel >= 0.5)
    tags <- c(tags, paste0("near-total absence (window mean ",
                           round(100 * win_mean_rel), "% of seats) - data ",
                           "missing wholesale, not vacancy"))
  if (win_start <= leg_start + tol)
    tags <- c(tags, paste0("opening gap - RESE start dates may follow a later ",
                           "convention (first session / oath) than the PARL ",
                           "leg_period_start_date (election)"))
  if (win_end >= leg_end - tol)
    tags <- c(tags, paste0("closing gap - RESE end dates may end before the ",
                           "PARL leg_period_end_date (early dissolution / ",
                           "truncated end dates)"))
  if (!is.na(coverage_end) && win_end >= coverage_end - tol)
    tags <- c(tags, "runs into the data-coverage boundary (scrape vintage)")
  if (length(tags) == 0)
    tags <- "mid-term deficit - departures without recorded replacements?"
  paste(tags, collapse = "; ")
}

# Build auto-summary for a structurally undercounted legislature.
# leg        : one row of undercount_legislature_stats() (+ flagged/severity)
# thresholds : list(baseline, chronic_thr, acute_thr) from
#              undercount_flag_legislatures()
# runs       : undercount_runs() output for this legislature
# departure/arrival : curated wave frames (undercount_wave_sets())
# shape      : undercount_shape_label() output
build_undercount_summary <- function(leg, thresholds, runs,
                                     departure = NULL, arrival = NULL,
                                     shape = NULL) {
  pct <- function(x) sprintf("%.1f%%", 100 * x)
  lines <- c(
    paste0("**Legislature:** ", leg$parliament_id, " (",
           format_pcc_date(leg$leg_start), " -- ",
           format_pcc_date(leg$leg_end), ", ", leg$n_days,
           " evaluated days", if (isTRUE(leg$truncated))
             "; truncated at the data-coverage boundary" else "", ")"),
    paste0("**Structural undercount, severity:** ", leg$severity),
    paste0("**Chronic (median) deficit:** ", pct(leg$chronic),
           " of seats  |  **Acute (worst ",
           "window) deficit:** ", pct(leg$acute), " over ",
           format_pcc_date(leg$acute_start), " -- ",
           format_pcc_date(leg$acute_end)),
    paste0("**Country baseline (by-design vacancy floor):** ",
           pct(thresholds$baseline), "  |  flag thresholds: chronic >",
           pct(thresholds$chronic_thr), ", acute >", pct(thresholds$acute_thr))
  )
  if (!is.null(shape) && nzchar(shape)) {
    lines <- c(lines, "", paste0("**Shape classification:** ", shape))
  }
  if (!is.null(runs) && nrow(runs) > 0) {
    runs_display <- data.frame(
      start = sapply(runs$start_date, format_pcc_date),
      end   = sapply(runs$end_date, format_pcc_date),
      days  = runs$days,
      peak_missing = runs$peak_deficit,
      mean_missing = runs$mean_deficit)
    lines <- c(lines, "",
               paste0("**Deficit runs above the flag threshold (",
                      nrow(runs), "):**"),
               "", df_to_md_table(runs_display, max_rows = 15))
  }
  if (!is.null(departure) && nrow(departure) > 0) {
    lines <- c(lines, "",
               paste0("**Departure wave -- entries ending around the worst ",
                      "window (", nrow(departure), "):** a cohort ending ",
                      "together suggests truncated or wrong end dates."),
               "", df_to_md_table(departure))
  }
  if (!is.null(arrival) && nrow(arrival) > 0) {
    lines <- c(lines, "",
               paste0("**Arrival wave -- entries starting around the worst ",
                      "window (", nrow(arrival), "):** if the deficit closes ",
                      "when this wave lands, the start dates likely follow a ",
                      "later convention than the parliament start."),
               "", df_to_md_table(arrival))
  }
  lines <- c(lines, "",
             paste0("_Interpretation: an undercount means fewer seated MPs in ",
                    "RESE than the official parliament size in PARL. Unlike ",
                    "overcounts, small undercounts can be real (seats vacant ",
                    "until by-elections); this legislature exceeds the ",
                    "country's own vacancy floor. Likely error classes -- ",
                    "start dates recorded at a later convention, end dates ",
                    "truncated, a cohort of missing RESE rows, or a wrong ",
                    "PARL parliament_size / leg_period boundary. When ",
                    "possible, name the specific pers_id(s)._"))
  paste(lines, collapse = "\n")
}

# Can pass either an issue_path_str (split into labels) or a labels vector.
# Returns a data.frame with number, title, state, url (or empty df on failure).
gh_list_issues <- function(repo, issue_path_str = NULL, labels = NULL) {
  if (is.null(labels)) labels <- issue_path_to_labels(issue_path_str)
  label_flags <- paste("--label", shQuote(labels), collapse = " ")
  cmd <- paste(
    "gh issue list",
    "--repo", shQuote(repo),
    "--state all",
    label_flags,
    "--json number,title,state,url",
    "--limit 50",
    "2>/dev/null"
  )
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = TRUE)),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) == 0) {
    return(data.frame(number = integer(0), title = character(0),
                      state = character(0), url = character(0),
                      stringsAsFactors = FALSE))
  }
  json_text <- paste(out, collapse = "\n")
  tryCatch({
    df <- jsonlite::fromJSON(json_text)
    if (length(df) == 0 || nrow(df) == 0) {
      return(data.frame(number = integer(0), title = character(0),
                        state = character(0), url = character(0),
                        stringsAsFactors = FALSE))
    }
    df[, c("number", "title", "state", "url")]
  }, error = function(e) {
    data.frame(number = integer(0), title = character(0),
               state = character(0), url = character(0),
               stringsAsFactors = FALSE)
  })
}

# Ensure GitHub labels exist on a repo (one per hierarchy level)
gh_ensure_labels <- function(labels, repo,
                             colours = github_label_colours) {
  for (i in seq_along(labels)) {
    colour <- colours[min(i, length(colours))]
    label_cmd <- paste("gh label create", shQuote(labels[i]),
                       "--repo", shQuote(repo),
                       "--color", shQuote(colour),
                       "--force", "2>/dev/null")
    system(label_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }
}

# Build the shell command string for gh issue create
gh_issue_create_cmd <- function(repo, title, body, labels) {
  label_flags <- paste("--label", shQuote(labels), collapse = " ")
  paste("gh issue create",
        "--repo", shQuote(repo),
        "--title", shQuote(title),
        "--body", shQuote(body),
        label_flags,
        "2>&1")
}

# Post a GitHub issue. Returns list(success, output, issue_number).
gh_post_issue <- function(repo, title, body, labels) {
  gh_ensure_labels(labels, repo)
  cmd <- gh_issue_create_cmd(repo, title, body, labels)
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = TRUE)),
    error = function(e) e$message
  )
  out_text <- paste(out, collapse = "\n")
  issue_number <- NA_integer_
  if (grepl("github.com", out_text)) {
    m <- regmatches(out_text, regexpr("/issues/([0-9]+)", out_text))
    if (length(m) > 0) {
      issue_number <- as.integer(sub("/issues/", "", m[1]))
    }
  }
  list(
    success      = grepl("github.com", out_text),
    output       = out_text,
    issue_number = issue_number
  )
}

# --- Plot saving and image upload for GitHub issues ---

# Save a ggplot object to a temporary PNG file. Returns the file path.
save_issue_plot <- function(plot_obj, width = 10, height = 5, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot = plot_obj,
                  width = width, height = height, dpi = dpi)
  tmp
}

# Build an asset filename from issue path, issue number, and extension.
# The sanitized-path + "_issue<N>" stem is the discoverability contract: a
# coding agent given an issue URL can reconstruct the filename from the
# issue's labels and number (e.g. "NL / POLI / completeness / death_date"
# + issue 42 -> "NL_POLI_completeness_death_date_issue42.csv").
issue_asset_filename <- function(issue_path_str, issue_number, ext, suffix = NULL) {
  sanitized <- gsub("[^a-zA-Z0-9_]", "_", issue_path_str)
  sanitized <- gsub("_+", "_", sanitized)
  sanitized <- gsub("^_|_$", "", sanitized)
  stem <- paste0(sanitized, "_issue", issue_number)
  # suffix distinguishes the several CSVs one panel can attach (e.g. "peak",
  # "opening"); empty/NULL keeps the historical single-file stem intact.
  if (!is.null(suffix) && nzchar(suffix)) stem <- paste0(stem, "_", suffix)
  paste0(stem, ".", ext)
}

# Build the image filename from issue path and issue number
issue_image_filename <- function(issue_path_str, issue_number) {
  issue_asset_filename(issue_path_str, issue_number, "png")
}

# Upload a file (image, CSV, ...) to a GitHub repo via the Contents API.
# Returns the raw URL on success, or NULL on failure.
gh_upload_asset <- function(file_path, repo, target_filename) {
  b64 <- base64enc::base64encode(file_path)
  body_json <- jsonlite::toJSON(list(
    message = paste("Add issue asset:", target_filename),
    content = b64
  ), auto_unbox = TRUE)

  body_file <- tempfile(fileext = ".json")
  writeLines(body_json, body_file)

  cmd <- paste(
    "gh api",
    "--method PUT",
    paste0("repos/", repo, "/contents/", target_filename),
    "--input", shQuote(body_file),
    "--jq '.content.download_url'",
    "2>/dev/null"
  )
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = TRUE)),
    error = function(e) NULL
  )
  unlink(body_file)

  if (is.null(out) || length(out) == 0 || !grepl("http", out[1])) {
    return(NULL)
  }
  trimws(out[1])
}

# Append a markdown snippet to an existing GitHub issue by editing its body.
gh_append_to_issue <- function(repo, issue_number, markdown_snippet) {
  get_cmd <- paste(
    "gh issue view", issue_number,
    "--repo", shQuote(repo),
    "--json body --jq '.body'",
    "2>/dev/null"
  )
  current_body <- tryCatch(
    suppressWarnings(system(get_cmd, intern = TRUE)),
    error = function(e) NULL
  )
  if (is.null(current_body)) return(FALSE)

  new_body <- paste0(paste(current_body, collapse = "\n"), markdown_snippet)
  body_file <- tempfile(fileext = ".md")
  writeLines(new_body, body_file)

  edit_cmd <- paste(
    "gh issue edit", issue_number,
    "--repo", shQuote(repo),
    "--body-file", shQuote(body_file),
    "2>/dev/null"
  )
  exit <- system(edit_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink(body_file)
  exit == 0
}

# Append an image to an existing GitHub issue by editing its body.
gh_append_image_to_issue <- function(repo, issue_number,
                                     image_url, caption = "Graph") {
  gh_append_to_issue(repo, issue_number,
                     paste0("\n\n![", caption, "](", image_url, ")"))
}

# Markdown line linking the uploaded problem-table CSV. Humans click it on the
# issue page; coding agents parse the body for "**Problem table:**".
issue_data_link_markdown <- function(csv_url, filename, n_rows) {
  paste0("\n\n**Problem table:** [", filename, "](", csv_url, ") (",
         n_rows, " row", if (n_rows != 1) "s", ", CSV, sep = \";\")")
}

# Append the problem-table CSV link to an existing GitHub issue.
gh_append_data_link_to_issue <- function(repo, issue_number, csv_url,
                                         filename, n_rows) {
  gh_append_to_issue(repo, issue_number,
                     issue_data_link_markdown(csv_url, filename, n_rows))
}

# --- LLM integration (OpenAI Codex CLI) ---

# NULL = use Codex CLI's default model (currently gpt-5.5)
codex_model <- NULL

# Low-level Codex query (adapted from R056 for long prompts).
# Writes the prompt to a temp file, then asks Codex to follow the
# instructions in that file. Returns the response string, or NULL on failure.
# image: optional path(s) to image files attached to the prompt (codex exec
# -i), so the model can actually look at a graph instead of only reading
# numbers about it.
codex_query <- function(prompt, model = codex_model, image = NULL) {
  out_file    <- tempfile(fileext = ".txt")
  prompt_file <- tempfile(fileext = ".md")
  tryCatch({
    writeLines(prompt, prompt_file)
    instruction <- paste0(
      "Read the file at ", prompt_file,
      " and follow the instructions in it exactly."
    )
    model_flag <- if (!is.null(model)) {
      paste("-m", shQuote(model))
    } else ""
    if (is.null(image)) image <- character(0)
    image <- image[file.exists(image)]
    image_flags <- if (length(image) > 0) {
      paste("-i", vapply(image, shQuote, character(1)), collapse = " ")
    } else ""
    cmd <- paste(
      "codex exec",
      "--dangerously-bypass-approvals-and-sandbox",
      "--skip-git-repo-check",
      "--ephemeral",
      model_flag,
      image_flags,
      "-o", shQuote(out_file),
      shQuote(instruction),
      "< /dev/null 2>/dev/null"
    )
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (!file.exists(out_file)) return(NULL)
    response <- trimws(paste(readLines(out_file, warn = FALSE),
                             collapse = "\n"))
    if (nchar(response) == 0) NULL else response
  }, error = function(e) NULL,
  finally = {
    if (file.exists(prompt_file)) unlink(prompt_file)
  })
}

# Build the prompt for generating a human-readable issue title
build_title_prompt <- function(issue_path_str, auto_summary) {
  paste0(
    "You are a data quality assistant for a parliamentary dataset ",
    "(the Political Careers In Comparison Project). ",
    "Given the following issue classification and technical details, ",
    "write a short, human-readable GitHub issue title (max 80 chars). ",
    "Do NOT include quotes around the title. ",
    "Reply with only the title, nothing else.\n\n",
    "Issue path: ", issue_path_str, "\n\n",
    "Technical details:\n", auto_summary
  )
}

# Build the prompt for generating an issue description.
# graph_caption: non-NULL when the dashboard graph is attached as an image to
# the LLM call. The instructions then anchor the diagnosis on the graph: the
# boundary key facts alone are easy to over-read (e.g. "last date with any
# seated MP" only means >= 1 seated MP, not a complete chamber), while the
# graph shows whether coverage was complete, eroding, or absent around the
# relevant dates.
build_description_prompt <- function(issue_path_str, auto_summary,
                                     graph_caption = NULL) {
  graph_part <- if (!is.null(graph_caption)) {
    paste0(
      "Attached is an image of the dashboard graph (", graph_caption, "). ",
      "Have a good look at this graph before writing: base your diagnosis ",
      "on what it actually shows around the relevant dates — e.g. whether ",
      "coverage is complete up to a sudden cliff, gradually eroding, or ",
      "absent for a whole period. ",
      "Be careful with boundary key facts such as the last date with any ",
      "seated MP: they only mean at least one MP was seated on that date, ",
      "NOT that the parliament was complete then — use the graph to tell ",
      "the difference. "
    )
  } else ""
  paste0(
    "You are a data quality assistant for a parliamentary dataset ",
    "(the Political Careers In Comparison Project / PCC). ",
    "Given the following issue classification and technical details, ",
    "write a GitHub issue description. ",
    graph_part,
    "Start with a concise 1-2 sentence summary (## Summary), ",
    "then provide a more elaborate description below (## Details) ",
    "covering the problem and affected data. ",
    "Include a ## Suggested fix section with ideas on how this might ",
    "be resolved, but make clear that these are suggestions that need ",
    "to be verified by the user. ",
    "Always refer to politicians by their full pers_id ",
    "(e.g. 'NL_Aartsen_Thierry_1989'), not by name alone. ",
    "Use markdown formatting. ",
    "Reply with only the description, nothing else.\n\n",
    "Issue path: ", issue_path_str, "\n\n",
    "Technical details:\n", auto_summary
  )
}

# Generate a human-readable title via LLM.
# Returns the LLM title, or the original path as fallback.
llm_generate_title <- function(issue_path_str, auto_summary) {
  result <- codex_query(
    build_title_prompt(issue_path_str, auto_summary)
  )
  if (is.null(result) || nchar(result) == 0 ||
      tolower(result) == "error") {
    return(issue_path_str)
  }
  # Strip surrounding quotes if the LLM added them
  gsub("^\"|\"$", "", result)
}

# Generate an issue description via LLM.
# image/graph_caption: optional PNG of the dashboard graph shown above the
# issue form; when given, the image is attached to the Codex call and the
# prompt tells the model to base its diagnosis on the graph.
# Returns the LLM description, or empty string as fallback.
llm_generate_description <- function(issue_path_str, auto_summary,
                                     image = NULL, graph_caption = NULL) {
  result <- codex_query(
    build_description_prompt(issue_path_str, auto_summary,
                             graph_caption = graph_caption),
    image = image
  )
  if (is.null(result) || nchar(result) == 0 ||
      tolower(result) == "error") {
    return("")
  }
  result
}
