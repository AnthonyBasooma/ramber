#' For downloading and processing barrier atlas data from AMBER database.
#'
#' @param type \code{string} The type of barrier to filter out. The available options include
#'        \code{"ramp"    "culvert" "dam"     "sluice"  "other"   "weir"    "ford"}.
#' @param country \code{string} Provide the country/s to be filtered. Default is all will be returned if left unfilled.
#' @param depth \code{string} Provide the depth/s to be filtered. Default is all will be returned if left unfilled.
#' @param dbname \code{string} Provide the AMBER database at country level to be filtered. Default is all will be returned if left unfilled.
#' @param height \code{string} Provide the height to be filtered. Default is all will be returned if left unfilled.
#' @param outlet \code{string} Provide the outlet to be filtered. Default is all will be returned if left unfilled.
#' @param rivername \code{string} Provide the river name/s to be filtered. Default is all will be returned if left unfilled.
#' @param basinname \code{string} Provide the river basin name/s to be filtered. Default is all will be returned if left unfilled.
#'@param  hclass \code{string} Provide the river Hclass/es to be filtered. Default is all will be returned if left unfilled.
#' @param format \code{string} Either a data.table or coordinates converted into a point geofile
#'        is returned. Default \code{data.table}.
#' @param bbox \code{string} Either provide a bounding box with an xmin, ymin, xmax, ymax pattern. OR Provide
#'        shapefile where the bounding box will be selected to crop the records.
#' @inheritParams .dataCache
#'
#' @return Return datatable or point geofile after filtering
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'   dams_austria <- get_barrieratlas(country = "AUSTRIA", type = "dam")
#'
#'   #Only from Danube
#'   dd <- system.file("extdata", "danube.shp.zip", package = "ramber")
#'
#'  db <- sf::st_read(dd, quiet=TRUE)
#'
#'  d1 <- get_barrieratlas(country = "AUSTRIA", type = "dam", bbox = db)
#'
#' }
get_barrieratlas <- function(type, country, depth, dbname, height,
                         outlet, rivername,basinname, hclass,
                         inform = FALSE,
                         format ='data.table',
                         cache= TRUE, bbox, reload = FALSE, mask = FALSE){

  if(isTRUE(mask) &missing(bbox))stop("To implement masking, provide a shapefile to delineate out the data or set mask to FALSE.", call. = FALSE)

  if(isTRUE(cache)){

    cachedir <- br_path(dir='amber')

    setCacheRootPath(path= cachedir)

    cache.root = getCacheRootPath()

    out <- .dataCache(cache = cache, inform = inform, reload = reload)[[1]]

  }else{
    out <- .dataCache(cache = cache, inform = inform)[[1]]
  }

  batlas <- if(missing(bbox)) out else .crop(x=out, y = bbox, mask = mask)

  btype   <-   if(missing(type)) unique(batlas$type) else type

  ctype   <-   if(missing(country)) unique(batlas$Country) else country

  rivn    <-   if(missing(rivername)) unique(batlas$RiverName) else rivername

  hclass  <-   if(missing(hclass)) unique(batlas$HClass) else hclass

  outlet  <-   if(missing(outlet)) unique(batlas$Outlet) else outlet

  dbname  <-   if(missing(dbname)) unique(batlas$DBName) else dbname

  height  <-   if(missing(height)) unique(batlas$Height) else height

  bname   <-   if(missing(basinname)) unique(batlas$BasinName) else basinname

  ww <- batlas[batlas$type%in%btype &
                 batlas$Country%in%ctype &
                 batlas$BasinName%in%bname &
                 batlas$HClass %in%hclass &
                 batlas$Outlet%in%outlet &
                 batlas$Height%in%height &
                 batlas$DBName%in%dbname &
                 batlas$RiverName%in%rivn,]

  if(format=='geom' & missing(bbox)) {

    out <- ww[complete.cases(ww[, c("Longitude_WGS84", "Latitude_WGS84")]), ]

    ww <- out |> st_as_sf(coords = c("Longitude_WGS84", "Latitude_WGS84"), crs = st_crs(4326))
  }

  return(ww)
}
