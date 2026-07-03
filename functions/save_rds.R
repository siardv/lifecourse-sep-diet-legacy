save_rds <- function(object, name = NULL, verbose = FALSE, use_date = TRUE, dir = NULL) {
  # Determine the object name
  if (is.null(name)) {
    object_name <- deparse(substitute(object))
  } else {
    object_name <- name
  }

  # Append the date to the file name if use_date is TRUE
  if (use_date) {
    mm_dd <- format(Sys.Date(), "%m_%d")
    file_name <- paste0(object_name, "_", mm_dd, ".Rds")
  } else {
    file_name <- paste0(object_name, ".Rds")
  }

  # Determine the directory where the file will be saved
  if (is.null(dir)) {
    dir <- getwd()
  }

  # Construct the full file path
  file_path_with_dir <- file.path(dir, file_name)

  # Save the object
  saveRDS(object, file_path_with_dir)

  # Check if the file exists and provide feedback
  if (file.exists(file_path_with_dir)) {
    if (verbose || interactive()) {
      cat(cli::col_silver(
        glue::glue(
          "Object {object_name} saved as ",
          cli::style_underline(file_path_with_dir)
        )
      ), "\n")
    }
  } else {
    cat(
      cli::cli_alert_danger(
        "Failed to save object {object_name} as ",
        cli::style_underline(file_path_with_dir)
      ), "\n"
    )
  }

  return(invisible(object))
}