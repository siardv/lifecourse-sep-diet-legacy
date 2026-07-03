rename_and_select <- function(data, suffix = "") {
  require(rlang, quietly = TRUE)
  suffix <- if (suffix == "") suffix else paste0("_", suffix)
  out <- data %>%
    dplyr::rename(
      !!paste0("isco08_old", suffix) := isco08_init,
      !!paste0("occupation", if (suffix == "") "" else suffix, "_cleaned") := occupation,
      !!paste0("isco08", suffix) := isco08,
      !!paste0("isco08_new", suffix) := isco08_new
    ) %>%
    dplyr::select(-source)

  names(out) <- gsub("[_]+", "_", names(out))
  return(out)
}