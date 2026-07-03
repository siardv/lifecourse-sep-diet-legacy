footnote <- function(tbl, text, escape = FALSE, footnote_as_chunk = TRUE, ...) {
  if (missing(text)) {
    text <- get0("footnote_text", envir = parent.frame())
    if (is.null(text)) {
      warning("No footnote text provided, returning the table without a footnote.")
      return(tbl)
    }
  }
  kableExtra::footnote(tbl, text, escape = escape, footnote_as_chunk = footnote_as_chunk, ...)
}