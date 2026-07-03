brc14_map <- function() {
  local_file <- "data/brc2014.xls"
  if (!file.exists(local_file)) {
    download_url <- "https://www.cbs.nl/-/media/imported/onze-diensten/methoden/classificaties/documents/2015/09/brc2014.xls"
    download.file(download_url, local_file, mode = "wb")
  }
  readxl::read_excel(local_file, sheet = 2)
}

isco08_to_brc14 <- function(x) {
  m <- brc14_map()
  purrr::map_chr(x, ~ m$BRC2014beroepsgroep[match(.x, m$ISCO2008unitgroup, nomatch = NA)])
}
