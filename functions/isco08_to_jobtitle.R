isco08_to_jobtitle <- function(x) {
  sapply(x, function(code) {
    if (is.na(code)) {
      return(NA)
    }
    f <- ISCO08ConveRsions::isco08tojobtitle
    tryCatch(
      tryCatch(
        f(as.character(code)),
        error = function(e) {
          f(code)
        }
      ),
      error = function(e) NA
    )
  })
}