highlight_function <- function(x) {
  highlight::highlight(
    renderer = highlight::renderer_html(),
    detect_indent = TRUE,
    file = sprintf("functions/%s.R", x)
  )
}