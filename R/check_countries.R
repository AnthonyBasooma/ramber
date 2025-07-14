#' Title
#'
#' @param x
#' @param amb
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#'
check_countries <- function(x, amb, y, ind){

  unx <- unique(unlist(amb[y]))

  out <- unx[which(unx%in%x==TRUE)]

  if(length(x)!=length(out)){

    #if not check with std df for eu ctries
    eu <- get_eu_countries()

    if(nchar(x[1])== 2) euin <- eu$alpha_2

    if(nchar(x[1])== 3) euin <- eu$alpha_3

    if(nchar(x[1])>3) euin <- eu$country_name

    if(missing(ind)){

      euout <- toupper(eu$country_name[which(euin%in%x ==TRUE)])
    }else{
    #   euout <- tolower(euout)
    #
    #   substr(euout , 1, 1) <- toupper(substr(euout , 1, 1))
     }

    out <- unx[which(unx%in% euout ==TRUE)]
  }
  return(out)
}
