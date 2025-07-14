
#' Eu countries
#'
#' @return EU countries and their alpha-3 codes
#'
#' @export
#'
get_eu_countries <- function(){

  download.file("https://raw.githubusercontent.com/datumorphism/dataset-european-countries/master/dataset/european_countries.csv",
                destfile = "eu.csv", quiet = TRUE)

  eu <- read.csv("eu.csv")
}
