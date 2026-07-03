save_step <- function(..., reset = FALSE, verbose = FALSE, parent_env = globalenv()) {
  args <- list(...)
  if (length(args) == 0) {
    return(invisible(NULL))
  }
  if (verbose) {
    verbose <- TRUE
  }
  # get options to override verbose
  if (exists("verbose", envir = parent_env)) {
    verbose <- get("verbose", envir = parent_env)
  }
  if (!exists("steps", envir = parent_env)) {
    steps <- new.env(parent = parent_env)
    assign("steps", steps, envir = parent_env)
  } else {
    steps <- get("steps", envir = parent_env)
  }
  if (reset) {
    rm(list = ls(steps), envir = steps)
    if (verbose) cat("Steps reset.\n")
  }
  new_step <- args[[1]]
  step_exists <- any(sapply(as.list(steps), function(x) identical(x, new_step)))
  if (!step_exists) {
    step_name <- paste0("step_", length(steps) + 1)
    steps[[step_name]] <- new_step
    if (verbose) {
      cat("Step", length(steps), "saved in", dQuote("steps"), "environment.\n")
    }
  } else {
    step_name <- paste0("step_", length(steps) + 1)
    steps[[step_name]] <- NULL
    if (verbose) {
      cat("Step already exists. NULL added as step", length(steps), "in", dQuote("steps"), "environment.\n")
    }
  }

  invisible(new_step)
}