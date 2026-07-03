add_style <- function(x) {
  return(kableExtra::kable_styling(kableExtra::kable_classic(x), bootstrap_options = c("hover", "condensed", "responsive")))
}