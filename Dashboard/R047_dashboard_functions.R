# R047_dashboard_functions.R
# Functions used exclusively by the R047 dashboard (Dashboard/app.R):
# GitHub issue integration, LLM (Codex) helpers, and CSV export.
# Pure logic — no Shiny dependencies. Testable independently.
# Requires format_pcc_date() from R047_functions.R (sourced by app.R).

# --- CSV export ---

# Writes a data.frame to a semicolon-delimited CSV matching the PCCdata
# format (source files are read with sep = ";"). NA is written as an empty
# string so exports round-trip cleanly via read.csv(..., sep = ";"), and the
# file is UTF-8 encoded. `file` may be a path or a connection.
write_pcc_csv <- function(df, file) {
  write.table(df, file, sep = ";", row.names = FALSE, na = "",
              qmethod = "double", fileEncoding = "UTF-8")
}

# --- Issue identifier vectors (one per check, matching check order) ---
rese_check_ids <- c(
  "persid_in_POLI", "resentryid_unique", "dates_parsed",
  "full_overlap", "near_overlap", "birthday_duplicates",
  "parlmem_coverage", "coverage_at_date_from", "coverage_at_date_to"
)

parl_check_ids <- c("dates_parsed", "parliament_size_valid")

meme_check_ids <- c(
  "persid_in_POLI", "partyid_in_PART", "memepid_unique",
  "dates_parsed", "inverted_dates", "full_overlap",
  "parlmembers_have_party", "same_party_overlap_during_parliament",
  "diff_party_overlap_during_parliament"
)

poli_check_ids <- c("persid_unique")

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
# all_episodes: optional data.frame with all overcount episodes for context
build_overcount_summary <- function(ep, rese_ending,
                                    all_episodes = NULL) {
  lines <- c(
    paste0("**Episode:** ", format_pcc_date(ep$start_date), " -- ",
           format_pcc_date(ep$end_date), " (", ep$duration_days, " days)"),
    paste0("**Official parliament size:** ", ep$parliament_size),
    paste0("**Peak excess:** +", ep$peak_excess,
           "  |  Mean excess: +", ep$mean_excess)
  )
  if (nrow(rese_ending) > 0) {
    lines <- c(lines, "",
               paste0("**RESE entries ending on ", format_pcc_date(ep$end_date),
                      " (", nrow(rese_ending), "):**"),
               "", df_to_md_table(rese_ending))
  }
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

# Query GitHub issues matching the given labels.
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

# Build the image filename from issue path and issue number
issue_image_filename <- function(issue_path_str, issue_number) {
  sanitized <- gsub("[^a-zA-Z0-9_]", "_", issue_path_str)
  sanitized <- gsub("_+", "_", sanitized)
  sanitized <- gsub("^_|_$", "", sanitized)
  paste0(sanitized, "_issue", issue_number, ".png")
}

# Upload an image file to a GitHub repo via the Contents API.
# Returns the raw URL on success, or NULL on failure.
gh_upload_image <- function(image_path, repo, target_filename) {
  b64 <- base64enc::base64encode(image_path)
  body_json <- jsonlite::toJSON(list(
    message = paste("Add issue image:", target_filename),
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

# Append an image to an existing GitHub issue by editing its body.
gh_append_image_to_issue <- function(repo, issue_number,
                                     image_url, caption = "Graph") {
  image_md <- paste0("\n\n![", caption, "](", image_url, ")")
  cmd <- paste(
    "gh issue edit", issue_number,
    "--repo", shQuote(repo),
    "--body-file -",
    "2>/dev/null"
  )
  # Get current body, append image, pipe back
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

  new_body <- paste0(paste(current_body, collapse = "\n"), image_md)
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

# --- LLM integration (OpenAI Codex CLI) ---

# NULL = use Codex CLI's default model (currently gpt-5.5)
codex_model <- NULL

# Low-level Codex query (adapted from R056 for long prompts).
# Writes the prompt to a temp file, then asks Codex to follow the
# instructions in that file. Returns the response string, or NULL on failure.
codex_query <- function(prompt, model = codex_model) {
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
    cmd <- paste(
      "codex exec",
      "--dangerously-bypass-approvals-and-sandbox",
      "--skip-git-repo-check",
      "--ephemeral",
      model_flag,
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

# Build the prompt for generating an issue description
build_description_prompt <- function(issue_path_str, auto_summary) {
  paste0(
    "You are a data quality assistant for a parliamentary dataset ",
    "(the Political Careers In Comparison Project / PCC). ",
    "Given the following issue classification and technical details, ",
    "write a GitHub issue description. ",
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
# Returns the LLM description, or empty string as fallback.
llm_generate_description <- function(issue_path_str, auto_summary) {
  result <- codex_query(
    build_description_prompt(issue_path_str, auto_summary)
  )
  if (is.null(result) || nchar(result) == 0 ||
      tolower(result) == "error") {
    return("")
  }
  result
}
