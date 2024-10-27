load_most_recent_rds <- function(pattern, dir = getwd()) {
  # Construct the full pattern and ensure .Rds extension is included
  full_pattern <- paste0(".*", pattern, ".*\\.Rds$")

  # List files in the specified directory matching the pattern
  files <- list.files(
    path = dir,
    pattern = full_pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Handle case where no files are found
  if (length(files) == 0) {
    cli::cli_alert_warning(glue::glue("No files found matching the pattern: {pattern} in directory: {dir}"))
    return(NULL)
  }

  # Get the modification times and find the most recent file
  file_info <- file.info(files)
  most_recent_file <- files[which.max(file_info$mtime)]

  # Optionally alert the user in interactive mode
  if (interactive()) {
    cli::cli_alert_info(glue::glue("Loading {basename(most_recent_file)}"))
  }

  # Load and return the most recent RDS file
  return(readRDS(most_recent_file))
}