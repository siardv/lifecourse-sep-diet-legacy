dt_options <- function(footnote = NULL, ...) {
  other_options <- list(...)
  out <- list(
    other_options,
    columnDefs = list(list(
      targets = "_all",
      render = htmlwidgets::JS(
        "function(data, type, row, meta) {",
        "return data === null ? '—' : data;",
        "}"
      ),
      createdCell = htmlwidgets::JS(
        "function(td, cellData, rowData, rowIndex, colIndex) {",
        "$(td).css({'white-space': 'nowrap', 'min-height': '200px'});",
        "}"
      )
    )),
    dom = 't<"dt-footer"ip>',
    scrollY = "400px", # Example height for vertical scrolling
    scrollCollapse = TRUE, # Ensures table collapses when not full height
    paging = TRUE #,
  #  fixedHeader = TRUE # Keeps the header and footer elements fixed
  )

  if (!is.null(footnote)) {
    out$footerCallback <- htmlwidgets::JS(
      "function(row, data, start, end, display) {",
      "$('table tfoot').remove();",
      sprintf("$('table').append('<tfoot><tr><td colspan=\"%d\" style=\"text-align:center;\"><i>Note</i>: %s</td></tr></tfoot>');", length(other_options$columnDefs), footnote),
      "}"
    )
  }

  return(out)
}