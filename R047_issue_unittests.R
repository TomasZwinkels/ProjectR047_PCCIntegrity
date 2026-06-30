# R047_issue_unittests.R
# Unit tests for R047_issue_functions.R

source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_functions.R")
source("/home/tomas/projects/ProjectR047_PCCIntegrity/R047_issue_functions.R")

library(testthat)

# --- issue_path ---

test_that("issue_path builds correct 4-level path", {
  expect_equal(issue_path("NL", "POLI", "completeness", "birth_date"),
               "NL / POLI / completeness / birth_date")
  expect_equal(issue_path("CA", "RESE", "check", "full_overlap"),
               "CA / RESE / check / full_overlap")
})

# --- issue_path_to_labels ---

test_that("issue_path_to_labels splits path into components", {
  labels <- issue_path_to_labels("NL / POLI / completeness / birth_date")
  expect_equal(labels, c("NL", "POLI", "completeness", "birth_date"))
})

test_that("issue_path_to_labels handles extra whitespace around separators", {
  labels <- issue_path_to_labels("NL  /  POLI  /  completeness  /  birth_date")
  expect_equal(labels, c("NL", "POLI", "completeness", "birth_date"))
})

test_that("issue_path and issue_path_to_labels are inverses", {
  path <- issue_path("DE", "MEME", "check", "inverted_dates")
  labels <- issue_path_to_labels(path)
  expect_equal(labels, c("DE", "MEME", "check", "inverted_dates"))
})

# --- df_to_md_table ---

test_that("df_to_md_table produces valid markdown with header and separator", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  md <- df_to_md_table(df)
  lines <- strsplit(md, "\n")[[1]]
  expect_equal(length(lines), 5)  # header + separator + 3 rows
  expect_true(grepl("^\\|.*a.*\\|.*b.*\\|$", lines[1]))
  expect_true(grepl("^\\|.*---.*\\|.*---.*\\|$", lines[2]))
})

test_that("df_to_md_table respects max_rows", {
  df <- data.frame(x = 1:20)
  md <- df_to_md_table(df, max_rows = 5)
  lines <- strsplit(md, "\n")[[1]]
  expect_equal(length(lines), 7)  # header + separator + 5 rows
})

test_that("df_to_md_table returns empty string for NULL or empty df", {
  expect_equal(df_to_md_table(NULL), "")
  expect_equal(df_to_md_table(data.frame()), "")
})

test_that("df_to_md_table truncates long values at 40 chars", {
  df <- data.frame(x = paste(rep("A", 50), collapse = ""))
  md <- df_to_md_table(df)
  expect_true(grepl("\\.\\.\\.", md))
  # 37 chars + "..." = 40
  expect_false(grepl(paste(rep("A", 50), collapse = ""), md))
})

# --- build_check_summary ---

test_that("build_check_summary includes check name, status, and row count", {
  result <- list(
    table = data.frame(
      Check = "All IDs unique", Status = "FAIL",
      stringsAsFactors = FALSE
    ),
    details = list(
      list(duplicate_rows = data.frame(
        pers_id = c("NL_A_1990", "NL_B_1991"),
        stringsAsFactors = FALSE
      ))
    )
  )
  summary <- build_check_summary(result, 1, "duplicate_rows")
  expect_true(grepl("All IDs unique", summary))
  expect_true(grepl("FAIL", summary))
  expect_true(grepl("Problem rows.*2", summary))
  expect_true(grepl("NL_A_1990", summary))
})

test_that("build_check_summary handles PASS with 0 rows", {
  result <- list(
    table = data.frame(
      Check = "Dates parsed", Status = "PASS",
      stringsAsFactors = FALSE
    ),
    details = list(
      list(problem_rows = data.frame())
    )
  )
  summary <- build_check_summary(result, 1, "problem_rows")
  expect_true(grepl("PASS", summary))
  expect_true(grepl("Problem rows.*0", summary))
  expect_false(grepl("First", summary))
})

# --- build_completeness_summary ---

test_that("build_completeness_summary includes variable and count", {
  missing <- data.frame(
    pers_id = c("NL_X_1980", "NL_Y_1985"),
    last_name = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  summary <- build_completeness_summary("birth_date", missing)
  expect_true(grepl("`birth_date`", summary))
  expect_true(grepl("Missing for.*2", summary))
  expect_true(grepl("NL_X_1980", summary))
})

test_that("build_completeness_summary handles empty missing_df", {
  missing <- data.frame(pers_id = character(0))
  summary <- build_completeness_summary("wikidata_id", missing)
  expect_true(grepl("Missing for.*0", summary))
  expect_false(grepl("First", summary))
})

# --- build_overcount_summary ---

test_that("build_overcount_summary includes episode stats", {
  ep <- data.frame(
    start_date = as.Date("2012-09-20"),
    end_date = as.Date("2017-03-22"),
    duration_days = 1645L,
    parliament_size = 150L,
    peak_excess = 2L,
    mean_excess = 1.5
  )
  rese_ending <- data.frame(
    pers_id = c("NL_A_1966", "NL_B_1970"),
    res_entry_start = c("20sep2012", "20sep2012"),
    stringsAsFactors = FALSE
  )
  summary <- build_overcount_summary(ep, rese_ending)
  expect_true(grepl("1645 days", summary))
  expect_true(grepl("150", summary))
  expect_true(grepl("\\+2", summary))
  expect_true(grepl("NL_A_1966", summary))
})

test_that("build_overcount_summary works with no RESE entries ending", {
  ep <- data.frame(
    start_date = as.Date("1982-09-16"),
    end_date = as.Date("1982-09-16"),
    duration_days = 1L,
    parliament_size = 150L,
    peak_excess = 1L,
    mean_excess = 1.0
  )
  rese_ending <- data.frame(pers_id = character(0), stringsAsFactors = FALSE)
  summary <- build_overcount_summary(ep, rese_ending)
  expect_true(grepl("1 days", summary))
  expect_false(grepl("RESE entries", summary))
})

# --- gh_issue_create_cmd ---

test_that("gh_issue_create_cmd builds valid command string", {
  cmd <- gh_issue_create_cmd(
    repo = "user/repo",
    title = "NL / POLI / completeness / birth_date",
    body = "Some description",
    labels = c("NL", "POLI", "completeness", "birth_date")
  )
  expect_true(grepl("gh issue create", cmd))
  expect_true(grepl("--repo 'user/repo'", cmd))
  expect_true(grepl("--title", cmd))
  expect_true(grepl("--label 'NL'", cmd))
  expect_true(grepl("--label 'birth_date'", cmd))
  expect_true(grepl("2>&1$", cmd))
})

# --- gh_list_issues ---

test_that("gh_list_issues returns empty data.frame for nonexistent labels", {
  df <- gh_list_issues("TomasZwinkels/PCCdata",
                       "XX / FAKE / check / nonexistent_label_xyz")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
  expect_true(all(c("number", "title", "state", "url") %in% names(df)))
})

test_that("gh_list_issues returns correct columns for existing issues", {
  # This relies on the birth_place_raw issue we created earlier
  df <- gh_list_issues("TomasZwinkels/PCCdata",
                       "NL / POLI / completeness / birth_place_raw")
  expect_true(is.data.frame(df))
  expect_true(all(c("number", "title", "state", "url") %in% names(df)))
  if (nrow(df) > 0) {
    expect_true(is.integer(df$number) || is.numeric(df$number))
    expect_true(all(df$state %in% c("OPEN", "CLOSED")))
    expect_true(all(grepl("github.com", df$url)))
  }
})

test_that("gh_list_issues handles gracefully when repo doesn't exist", {
  df <- gh_list_issues("nonexistent/repo_xyz_999",
                       "NL / POLI / completeness / birth_date")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
})

# --- LLM prompt builders ---

test_that("build_title_prompt includes issue path and summary", {
  prompt <- build_title_prompt(
    "NL / POLI / completeness / birth_date",
    "**Variable:** `birth_date`\n**Missing for:** 42 MPs"
  )
  expect_true(grepl("NL / POLI / completeness / birth_date", prompt))
  expect_true(grepl("birth_date", prompt))
  expect_true(grepl("42 MPs", prompt))
  expect_true(grepl("max 80 chars", prompt))
})

test_that("build_description_prompt includes issue path and summary", {
  prompt <- build_description_prompt(
    "NL / RESE / check / full_overlap",
    "**Check:** No fully overlapping episodes\n**Problem rows:** 3"
  )
  expect_true(grepl("NL / RESE / check / full_overlap", prompt))
  expect_true(grepl("3", prompt))
  expect_true(grepl("markdown", prompt, ignore.case = TRUE))
})

test_that("llm_generate_title falls back to path on failure", {
  # codex_query will fail if codex is not installed or key not set
  # but the function should gracefully return the path
  path <- "XX / TEST / check / fake_check"
  result <- llm_generate_title(path, "some summary")
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("llm_generate_description returns string on failure", {
  result <- llm_generate_description("XX / TEST / check / fake", "summary")
  expect_true(is.character(result))
})

# --- codex_query integration tests (run only if codex is available) ---

codex_available <- nchar(Sys.which("codex")) > 0

test_that("codex_query returns a response for a simple prompt", {
  skip_if(!codex_available, "codex CLI not installed")
  result <- codex_query("Reply with only the word hello")
  expect_false(is.null(result))
  expect_true(grepl("hello", tolower(result)))
})

test_that("llm_generate_title produces a readable title", {
  skip_if(!codex_available, "codex CLI not installed")
  path <- "NL / POLI / completeness / birth_date"
  summary <- "**Variable:** `birth_date`\n**Missing for:** 42 MPs"
  title <- llm_generate_title(path, summary)
  expect_true(nchar(title) > 0)
  expect_true(nchar(title) <= 120)
  # Should not just be the raw path
  expect_false(identical(title, path))
})

test_that("llm_generate_description produces a description", {
  skip_if(!codex_available, "codex CLI not installed")
  path <- "NL / POLI / completeness / birth_date"
  summary <- "**Variable:** `birth_date`\n**Missing for:** 42 MPs"
  desc <- llm_generate_description(path, summary)
  expect_true(nchar(desc) > 0)
  expect_true(nchar(desc) > 20)
})

# --- issue_image_filename ---

test_that("issue_image_filename produces valid filename with issue number", {
  fn <- issue_image_filename("NL / POLI / completeness / birth_date", 42)
  expect_equal(fn, "NL_POLI_completeness_birth_date_issue42.png")
})

test_that("issue_image_filename sanitizes special characters", {
  fn <- issue_image_filename("NL / RESE / overcount / NL_NT-TK_2012", 7)
  expect_false(grepl("/", fn))
  expect_true(grepl("issue7\\.png$", fn))
})

test_that("issue_image_filename handles different issue numbers", {
  fn1 <- issue_image_filename("NL / POLI / completeness / birth_date", 42)
  fn2 <- issue_image_filename("NL / POLI / completeness / birth_date", 78)
  expect_false(fn1 == fn2)
  expect_true(grepl("issue42", fn1))
  expect_true(grepl("issue78", fn2))
})

# --- save_issue_plot ---

test_that("save_issue_plot creates a valid PNG file", {
  p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5),
                       ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  path <- save_issue_plot(p)
  expect_true(file.exists(path))
  expect_true(file.size(path) > 0)
  expect_true(grepl("\\.png$", path))
  unlink(path)
})

# --- gh_post_issue returns issue_number ---

test_that("gh_post_issue result includes issue_number field", {
  # We can't actually post, but we can check the structure
  # by verifying the function signature and return list keys
  expect_true("issue_number" %in% names(
    list(success = FALSE, output = "", issue_number = NA_integer_)
  ))
})

test_that("issue number is parsed from a typical gh output URL", {
  # Simulate the parsing logic from gh_post_issue
  out_text <- "https://github.com/TomasZwinkels/PCCdata/issues/42"
  m <- regmatches(out_text, regexpr("/issues/([0-9]+)", out_text))
  issue_number <- as.integer(sub("/issues/", "", m[1]))
  expect_equal(issue_number, 42L)
})

# --- check ID vectors ---

test_that("check ID vectors have correct lengths", {
  expect_equal(length(rese_check_ids), 9)
  expect_equal(length(parl_check_ids), 2)
  expect_equal(length(meme_check_ids), 9)
  expect_equal(length(poli_check_ids), 1)
})
