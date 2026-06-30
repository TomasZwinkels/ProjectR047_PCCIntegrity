# R047_issue_functions.R
# Functions for GitHub issue integration in the R047 dashboard.
# Pure logic — no Shiny dependencies. Testable independently.

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
  repo = "TomasZwinkels/PCCdata"
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
build_completeness_summary <- function(variable, missing_df) {
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
  paste(lines, collapse = "\n")
}

# Build auto-summary for overcount episodes
# Requires format_pcc_date() from R047_functions.R
build_overcount_summary <- function(ep, rese_ending) {
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
  paste(lines, collapse = "\n")
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

# Post a GitHub issue. Returns list(success, output).
gh_post_issue <- function(repo, title, body, labels) {
  gh_ensure_labels(labels, repo)
  cmd <- gh_issue_create_cmd(repo, title, body, labels)
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = TRUE)),
    error = function(e) e$message
  )
  out_text <- paste(out, collapse = "\n")
  list(
    success = grepl("github.com", out_text),
    output  = out_text
  )
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
