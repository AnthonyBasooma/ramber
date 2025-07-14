
#' Downloading barriers AMBER database in the barrier tracker application.
#'
#' @param type \code{string} The type of barrier to filter out. The available options include
#'        \code{"ramp"    "culvert" "dam"     "sluice"  "other"   "weir"    "ford"}.
#' @param country \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param riverwidth \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param fishpass \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param depth \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param width \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param height \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param subtype \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param usefulness \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param extension \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param flow \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param rivername \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param year \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param month \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param day \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#'
#' @inheritParams .dataCache
#' @inheritParams get_barrieratlas
#'
#' @importFrom sf st_as_sf st_crs
#' @importFrom R.cache getCacheRootPath loadCache saveCache setCacheRootPath
#'
#' @return Return datatable or point geofile after filtering
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #dams from barrier tracker in Danube
#'
#' dd <- system.file("extdata", "danube.shp.zip", package = "ramber")
#'
#' db <- sf::st_read(dd, quiet=TRUE)
#'
#' btat <- get_barriertracker(country = "Austria", bbox = db )
#'
#' #use bounding box of danube to filter out data
#'
#' ddbox <- get_barriertracker(bbox = c(xmin = 8.15250, ymin = 42.08333,
#'                                 xmax= 29.73583,  ymax = 50.24500 ))
#'
#' }
#'
#'
get_barriertracker <- function(type, country, riverwidth, fishpass, depth, width, height,
                           subtype, usefulness, extension, flow, rivername,
                           year = NULL, month = NULL, day = NULL,
                           inform = FALSE, format ='data.table', cache= TRUE, bbox, reload = FALSE){
  if(isTRUE(cache)){

    cachedir <- br_path(dir='amber')

    setCacheRootPath(path= cachedir)

    cache.root = getCacheRootPath()

    out <- .dataCache(cache = cache, inform = inform, reload = reload)[[2]]

  }else{
    out <- .dataCache(cache = cache)[[2]]
  }

  btrack <- if(missing(bbox)) out else .crop(x=out, y = bbox)

  btype <- if(missing(type)) unique(btrack$type) else type

  ctype <- if(missing(country)) unique(btrack$country) else country

  rdw <-   if(missing(riverwidth)) unique(btrack$river_width) else riverwidth

  rivn <-   if(missing(rivername)) unique(btrack$river_name) else rivername

  fp  <-   if(missing(fishpass)) unique(btrack$fish_pass) else fishpass

  dp  <-   if(missing(depth)) unique(btrack$depth) else depth

  width  <-   if(missing(width)) unique(btrack$width) else width

  subtype  <-   if(missing(subtype)) unique(btrack$sub_type) else subtype

  use  <-   if(missing(usefulness)) unique(btrack$usefulness) else usefulness

  height  <-   if(missing(height)) unique(btrack$height) else height

  extension  <-   if(missing(extension)) unique(btrack$extension) else extension

  flow  <-   if(missing(flow)) unique(btrack$flow_conditions) else flow

  ww <- btrack[btrack$type%in%btype &
                 btrack$country%in%ctype &
                 btrack$river_width%in%rdw &
                 btrack$fish_pass%in%fp &
                 btrack$depth%in%dp &
                 btrack$width%in%width &
                 btrack$sub_type%in%subtype &
                 btrack$usefulness%in%use &
                 btrack$height%in%height &
                 btrack$extension%in%extension &
                 btrack$flow_conditions%in%flow &
                 btrack$river_name%in%rivn,]

  if(!is.null(year) | !is.null(month)| !is.null(day)){

    if(nrow(ww)<1)stop("No data available for the parameters used.", call. = FALSE)

    timstamp <- ww$created_at

    cdts <- clean_dates(timstamp)

    #if year is null
    if(is.null(year)) yearin <- unique(cdts$year) else yearin <- year

    if(is.null(month)) monthin <- unique(cdts$month) else monthin <- month

    if(is.null(day)) dayin <- unique(cdts$day) else dayin <- day

    #filter from the clean dates
    #print(month)
    dtfinal <- cdts[cdts$year%in%yearin & cdts$month %in%monthin & cdts$day%in%dayin,]

    #check from the time stamps: what appears in the clean timestamp

    timesin <- timstamp%in% unique(dtfinal$x)

    #extract the filtered data
    wwdout <- ww[timesin==TRUE,]

    if(nrow(wwdout)<1)stop("No data for the available dates selected.", call. = FALSE)

  }else{
    wwdout <- ww
  }

  if(format=='shp') {

    out <- wwdout[complete.cases(wwdout[, c("lng", "lat")]), ]

    wwdout <- out |> st_as_sf(coords = c("lng", "lat"), crs = st_crs(4326))
  }

  return(wwdout)
}
