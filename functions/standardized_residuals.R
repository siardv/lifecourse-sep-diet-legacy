standardized_residuals <- function(model, assing) {
  if (inherits(model, "lmrob")) {
    stdres <- stats::resid(model) / model$scale
  } else {
    stdres <- stats::rstandard(model)
  }
  if (missing(assing)) {
    return(stdres)
  } else if (length(assing) == 1 & is.character(assing)) {
    assign(assing, stdres, envir = parent.frame())
  } else {

  }
}