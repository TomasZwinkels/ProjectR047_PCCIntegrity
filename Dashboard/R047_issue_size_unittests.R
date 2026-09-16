# Offline integration tests for the actual issue-form JavaScript and observers.
# Run explicitly with testthat::test_file(); Node executes JS without a browser.
# App startup, data loading, AI calls and GitHub calls are never executed.

library(testthat)

issue_size_test_client <- function(confirmed = FALSE) {
  skip_if(Sys.which("node") == "", "Node is required to execute the form JavaScript")
  base_dir <- "/home/tomas/projects/ProjectR047_PCCIntegrity/Dashboard"
  app <- as.list(parse(file.path(base_dir, "app.R")))
  assignment <- function(name) Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name(name))
  }, app)[[1]]
  env <- new.env(parent = environment())
  sys.source(file.path(base_dir, "R047_dashboard_functions.R"), env)
  env$tags <- shiny::tags
  env$HTML <- shiny::HTML
  env$gh_list_issues <- function(...) data.frame()
  eval(assignment("issue_path_tag"), env)
  path <- "NL / POLI / completeness / birth_date"
  form_id <- gsub("[^a-zA-Z0-9]", "_", path)
  fields <- list()
  collect_fields <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      id <- tag$attribs$id
      if (!is.null(id) && id != form_id) fields[[id]] <<- tag
      lapply(tag$children, collect_fields)
    } else if (is.list(tag)) lapply(tag, collect_fields)
    invisible(NULL)
  }
  collect_fields(env$issue_path_tag(path, "1000 missing dates"))
  env$elements <- lapply(fields, function(tag) list(
    id = tag$attribs$id,
    value = if (!is.null(tag$attribs$value)) tag$attribs$value else "",
    checked = !is.null(tag$attribs$checked), disabled = FALSE
  ))
  id <- function(suffix) paste0(form_id, suffix)
  env$elements[[id("_size")]]$value <- "small"
  env$elements[[id("_size_confirmed")]]$checked <- confirmed
  env$elements[[id("_auto")]]$value <- "1000 missing dates"
  # ui -> fluidPage -> tags$head -> tags$script -> HTML -> script string.
  handlers <- assignment("ui")[[3]][[2]][[2]][[2]][[2]]
  node_script <- tempfile(fileext = ".js")
  writeLines(c(
    "const p = JSON.parse(require('fs').readFileSync(0, 'utf8'));",
    "const elements = p.elements, inputs = {}, handlers = {};",
    "for (const el of Object.values(elements)) {",
    "  el.style = {}; el.classList = {remove() {}, add() {}};",
    "}",
    "global.document = {getElementById: id => elements[id] || null};",
    "global.Shiny = {",
    "  addCustomMessageHandler: (type, fn) => handlers[type] = fn,",
    "  setInputValue: (name, value) => inputs[name] = value",
    "};",
    "eval(p.handlers);",
    "if (p.type) handlers[p.type](p.message);",
    "if (p.code) new Function(p.code).call(elements[p.element]);",
    "process.stdout.write(JSON.stringify({elements, inputs}));"
  ), node_script)
  env$js <- function(type = NULL, message = NULL, code = NULL, element = NULL) {
    input_file <- tempfile(fileext = ".json")
    on.exit(unlink(input_file), add = TRUE)
    jsonlite::write_json(list(elements = env$elements, handlers = handlers,
                              type = type, message = message, code = code,
                              element = element), input_file,
                         auto_unbox = TRUE, null = "null")
    result <- system2(Sys.which("node"), shQuote(node_script),
                       stdin = input_file, stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(result, "status"))) stop(paste(result, collapse = "\n"))
    result <- jsonlite::fromJSON(paste(result, collapse = "\n"),
                                simplifyVector = FALSE)
    env$elements <- result$elements
    for (name in names(result$inputs)) env$input[[name]] <- result$inputs[[name]]
  }
  env$input <- list()
  env$session <- list(sendCustomMessage = function(type, msg) env$js(type, msg))
  env$withProgress <- function(message, value, expr) {
    eval(substitute(expr), parent.frame())
  }
  env$setProgress <- function(...) invisible(NULL)
  env$notifications <- character()
  env$showNotification <- function(ui, ...) {
    env$notifications <- c(env$notifications, ui)
  }
  env$llm_generate_title <- function(...) "Missing birth dates"
  env$llm_suggest_issue_size <- function(...) "large"
  env$llm_generate_description <- function(issue_path_str, auto_summary,
                                          image, graph_caption, issue_size,
                                          size_confirmed,
                                          user_title = NULL, user_desc = NULL) {
    env$description_args <- list(size = issue_size, confirmed = size_confirmed)
    env$draft_args <- list(title = user_title, desc = user_desc)
    env$description_prompt <- env$build_description_prompt(
      issue_path_str, auto_summary, graph_caption, issue_size, size_confirmed,
      user_title = user_title, user_desc = user_desc)
    "Some birth dates are missing."
  }
  env$posted <- NULL
  env$gh_post_issue <- function(repo, title, body, labels) {
    env$posted <- list(body = body, labels = labels)
    list(success = TRUE, output = "Mock success", issue_number = 1L)
  }
  server_exprs <- as.list(assignment("server")[[3]][[3]])[-1]
  observer <- function(name) {
    expression <- Filter(function(x) {
      is.call(x) && identical(x[[1]], as.name("observeEvent")) &&
        identical(deparse(x[[2]]), paste0("input$", name))
    }, server_exprs)[[1]][[3]]
    fn <- function() NULL
    body(fn) <- expression
    environment(fn) <- env
    fn
  }
  env$generate <- observer("llm_generate")
  env$post <- observer("post_github_issue")
  env$click <- function(suffix) {
    env$js(code = fields[[id(suffix)]]$attribs$onclick, element = id(suffix))
  }
  env$change_size <- function(size) {
    env$elements[[id("_size")]]$value <- size
    env$js(code = fields[[id("_size")]]$attribs$onchange, element = id("_size"))
  }
  env$confirm <- function() env$elements[[id("_size_confirmed")]]$checked <- TRUE
  env$field <- function(suffix) env$elements[[id(suffix)]]
  env$set_field <- function(suffix, value) {
    env$elements[[id(suffix)]]$value <- value
  }
  env$suggest <- function(size) env$js("setIssueSizeSuggestion", list(
    text_id = id("_size_suggestion"), select_id = id("_size"),
    confirmed_id = id("_size_confirmed"), size = size, text = "AI suggestion"
  ))
  env$cleanup <- function() unlink(node_script)
  env
}

test_that("AI preserves confirmed size through description and posting", {
  client <- issue_size_test_client(confirmed = TRUE)
  on.exit(client$cleanup(), add = TRUE)
  client$suggest("large")
  expect_identical(client$field("_size")$value, "small")
  expect_true(client$field("_size_confirmed")$checked)
  client$click("_ai_btn")
  expect_true(client$field("_size")$disabled)
  expect_true(client$field("_size_confirmed")$disabled)
  client$generate()
  expect_identical(client$field("_size")$value, "small")
  expect_true(client$field("_size_confirmed")$checked)
  expect_identical(client$description_args, list(size = "small", confirmed = TRUE))
  expect_match(client$description_prompt, "maintainer-confirmed issue-size classification is `small`")
  expect_false(client$field("_size")$disabled)
  expect_false(client$field("_size_confirmed")$disabled)
  client$click("_post_btn")
  client$post()
  expect_contains(client$posted$labels, "size:small")
  expect_match(client$posted$body, "**Issue size:** `small`", fixed = TRUE)
})

test_that("unconfirmed size adopts suggestion and needs confirmation to post", {
  client <- issue_size_test_client()
  on.exit(client$cleanup(), add = TRUE)
  client$click("_ai_btn")
  client$generate()
  expect_identical(client$field("_size")$value, "large")
  expect_false(client$field("_size_confirmed")$checked)
  expect_identical(client$description_args, list(size = "large", confirmed = FALSE))
  expect_match(client$description_prompt, "provisional")
  expect_false(grepl("maintainer-confirmed", client$description_prompt, fixed = TRUE))
  client$click("_post_btn")
  client$post()
  expect_null(client$posted)
  expect_match(tail(client$notifications, 1), "tick User confirmed")
  client$confirm()
  client$click("_post_btn")
  client$post()
  expect_contains(client$posted$labels, "size:large")
  expect_match(client$posted$body, "**Issue size:** `large`", fixed = TRUE)
})

test_that("manually changing size clears confirmation and generation errors unlock controls", {
  client <- issue_size_test_client(confirmed = TRUE)
  on.exit(client$cleanup(), add = TRUE)
  client$change_size("medium")
  expect_false(client$field("_size_confirmed")$checked)
  client$click("_post_btn")
  client$post()
  expect_null(client$posted)
  client$click("_ai_btn")
  client$llm_suggest_issue_size <- function(...) stop("Mock generation failure")
  expect_error(client$generate(), "Mock generation failure")
  expect_false(client$field("_size")$disabled)
  expect_false(client$field("_size_confirmed")$disabled)
  expect_false(client$field("_ai_btn")$disabled)
})

test_that("draft text typed in the form is passed to the AI prompts", {
  client <- issue_size_test_client()
  on.exit(client$cleanup(), add = TRUE)
  # Title left at the auto-filled issue path (not a draft); description typed.
  client$set_field("_text", "Both rows share a birth date; looks like one person twice.")
  client$click("_ai_btn")
  client$generate()
  expect_identical(client$draft_args$desc,
                   "Both rows share a birth date; looks like one person twice.")
  expect_match(client$description_prompt,
               "Draft description:\nBoth rows share a birth date", fixed = TRUE)
  expect_match(client$description_prompt, "build on their draft", fixed = TRUE)
  expect_false(grepl("Draft title", client$description_prompt, fixed = TRUE))
  # An edited title is a draft too. (After the first round the fields hold the
  # generated text, which then becomes the draft for a regeneration.)
  client$set_field("_title", "Two records for one MP")
  client$set_field("_text", "Confirmed: same birth place as well.")
  client$click("_ai_btn")
  client$generate()
  expect_match(client$description_prompt,
               "Draft title: Two records for one MP", fixed = TRUE)
  expect_match(client$description_prompt,
               "Draft description:\nConfirmed: same birth place as well.", fixed = TRUE)
})

test_that("an untouched form sends no maintainer draft to the AI", {
  client <- issue_size_test_client()
  on.exit(client$cleanup(), add = TRUE)
  client$click("_ai_btn")
  client$generate()
  expect_identical(client$draft_args$desc, "")
  expect_false(grepl("Maintainer's draft", client$description_prompt, fixed = TRUE))
})
