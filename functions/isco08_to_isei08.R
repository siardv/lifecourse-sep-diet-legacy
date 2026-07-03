isco08_to_isei08 <- function(x) {
  sapply(x, function(code) {
    if (is.na(code)) {
      return(NA)
    }
    tryCatch(
      ISCO08ConveRsions::isco08toisei08(as.character(code)),
      error = function(e) NA
    )
  })
}