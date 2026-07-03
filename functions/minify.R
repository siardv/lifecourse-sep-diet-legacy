minify <- function(file, to = NULL) {
  if (is.null(to)) {
    to <- gsub("\\.[a-z]+$", "", basename(rstudioapi::getActiveDocumentContext()$path))
    to <- paste0(to, "_files")
  }
  # get new directory from ...
  regex <- "([^.][a-z]+)$"
  file_type <- stringr::str_extract(file, regex)
  file_path <- fs::path_abs(file, getwd())

  file_content <- readLines(file_path, warn = FALSE)
  file_content <- paste(file_content, collapse = "\n")
  api_url <- sprintf(
    "https://www.toptal.com/developers/%sminifier/%sraw",
    ifelse(file_type == "css", "css", "javascript-"),
    ifelse(file_type == "css", "", "api/")
  )
  body <- list(input = file_content)
  response <- httr::POST(url = api_url, body = body, encode = "form")
  if (httr::status_code(response) == 200) {
    min_content <- rawToChar(response$content)
    min_path <- sub(regex, "min.\\1", file)
    if (missing(to)) {
      to <- fs::path_abs(fs::path_dir(file))
    }
    if (!fs::dir_exists(to)) {
      fs::dir_create(to)
    }
    min_path <- fs::path_abs(sub(regex, "min.\\1", fs::path_file(file)), to)
    writeLines(min_content, min_path)
    if (interactive()) {
      cli::cli_alert_success(paste0("Minified ", dQuote(file), " to ", dQuote(min_path)))
    }
  } else {
    error <- jsonlite::parse_json(rawToChar(response$content))
    if (length(error$error) && lengths(error$error) == 3) {
      with(error$error, {
        cli::cli_alert_danger(sprintf("Error %d: %s - %s", status, title, detail))
      })
    }
    cli::cli_abort(paste0("Failed to minify ", dQuote(file), "\n"))
  }
}
