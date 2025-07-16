#' Check countries
#'
#' @param x user input
#' @param y amber atlas or tracker data
#'
#' @return
#'
#' @export
#'
#' @examples
#'
check_countries <- function(x, y){

  ab <- 'Country'%in%colnames(y)

  cc <- if(isTRUE(ab))  'Country' else "country" #atlas vs tracker

  ccs <- unique(unlist(y[,cc]))

  ein <- ccs[which(ccs%in%x==TRUE)]

  if(length(ein)!=length(x) &&length(ein)<length(x)){

    #some countries not seen in the data
    u11 <- x[which(x%in%ein==FALSE)]

    #get countries
    eux <- get_eu_countries()

    if(nchar(u11[1])== 2) euin <- eux$alpha_2

    if(nchar(u11[1])== 3) euin <- eux$alpha_3

    if(nchar(u11[1])>3) euin <- eux$country_name

    euout <- eux$country_name[which(euin%in%u11 ==TRUE)]

    #check again in countries but upper case for atlas

    e5 <- if(isTRUE(ab)) toupper(euout) else euout

    e6 <- ccs[which(ccs%in%e5==TRUE)]

    if(isTRUE(length(e6)<length(u11)))warning("Countries ", paste0(u11[which(u11%in%euout==FALSE)], collapse = ", "), " not in AMBER database and will be removed.", call. = FALSE)

    ein <- c(ein, e6)
  }
  return(ein)
}
