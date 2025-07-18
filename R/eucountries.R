
#' Eu countries
#'
#' @return EU countries and their alpha-3 codes
#'
#' @export
#'
get_eu_countries <- function(x='amber'){

  filepath <- br_path(dir = x)

  download.file("https://raw.githubusercontent.com/datumorphism/dataset-european-countries/master/dataset/european_countries.csv",
                destfile = "amber/eu.csv", quiet = TRUE)

  eu <- read.csv(paste0(filepath,"/eu.csv"))
}
