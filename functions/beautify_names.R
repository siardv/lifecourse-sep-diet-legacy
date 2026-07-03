beautify_names <- function(vec) {
  # replace underscores with spaces
  vec <- gsub("_", " ", vec)
  vec <- gsub(
    "bool children household",
    "Children in Household (yes/no)", vec
  )
  vec <- gsub(
    "household income eq",
    "Household Income (equivalized)", vec
  )
  vec <- gsub(
    "n members household",
    "Household Size", vec
  )

  vec <- gsub(
    "Partneryn",
    "Partner (yes/no)", vec
  )

  # replace specific phrases
  vec <- gsub("^n ", "number of ", vec)
  vec <- gsub(" cat$", " category", vec)
  vec <- gsub(" rec$", " (recoded)", vec)
  vec <- gsub(" numeric$", " (num)", vec)
  vec <- gsub(":", " × ", vec)
  vec <- gsub(" household$", " in household", vec)
  vec <- stringr::str_to_title(vec)
  # define words to always keep in lower case
  always_lower <- c(
    "of", "the", "and", "or", "in", "on", "at", "to",
    "by", "for", "with", "from", "as", "into", "onto",
    "upon", "out", "off", "over", "under", "up",
    "down", "about", "after", "before", "between",
    "through", "against", "above", "below", "around",
    "behind", "beside", "beyond", "during",
    "inside", "outside", "underneath", "without",
    "within", "recoded", "num", "numeric",
    "adj. for household size", "kcal"
  )

  # replace occurrences of `always_lower` with lower case
  for (word in always_lower) {
    vec <- gsub(paste0("\\b", word, "\\b"),
      tolower(word), vec,
      ignore.case = TRUE
    )
  }

  # define words to always keep in upper case
  always_upper <- c("DHD", "SEP", "Isco08", "Hh", "sd", "se")

  # replace occurrences of `always_upper` with upper case
  for (word in always_upper) {
    vec <- gsub(paste0("\\b", word, "\\b"),
      toupper(word), vec,
      ignore.case = TRUE
    )
  }
  vec <- ifelse(vec == "DHD Index", "DHD-index", vec)
  vec <- ifelse(vec == "DHD kcal", "DHD-kcal", vec)
  vec <- sapply(vec, function(x) {
    if (grepl("Isei08", x)) {
      x <- gsub("Isei08", "ISEI08", x)
    }
    return(x)
  })

  vec <- sapply(vec, function(x) {
    if (grepl("Isco08", x)) {
      x <- gsub("Isco08", "ISCO08", x)
    }
    return(x)
  })

  vec <- sapply(vec, function(x) {
    if (grepl("Brc14", x)) {
      x <- gsub("Brc14", "BRC14", x)
    }
    return(x)
  })
  vec <- ifelse(vec == "Children HH Yes", "Children HH (yes)", vec)

  return(vec)
}
