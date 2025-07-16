#' @noRd
#'
br_path <- function(dir = 'cache'){

  wd <- getwd()

  if(is.null(dir)){

    stop('Provide the cache directory.')

  }else{

    cachedir <- paste0(wd,'/',dir)

    if(dir.exists(cachedir)==FALSE){

      dir.create(dir)

      path = paste0(wd, '/', dir)

    }else{

      path = cachedir
    }
  }
  return(path)
}

#' Data download
#'
#' @param x Download folder
#'
#' @importFrom utils download.file read.csv unzip
#'
#'
#' @return Two download files
#'
downloadDirect <- function(x='amber'){

  download.file("https://storage.googleapis.com/amber-exports/1751967734649-amber-project-data.zip",
                destfile =  "amber.zip", mode = 'wb', quiet = TRUE)

  filepath <- br_path(dir = x)

  unzip( "amber.zip", exdir = filepath)

  unlink("amber.zip")

  atlas <- read.csv(paste0(filepath,"/atlas.csv"))

  barriertracker <- read.csv(paste0(filepath,"/amber-barrier-tracker.csv"))

  return(list(atlas = atlas, barriers =barriertracker))
}


#' For caching data
#'
#' @param cache caching data TRUE or FALSE
#' @param inform show messages of downloading data.
#' @param reload To refresh the database.
#'
#' @return cache or not cache data

.dataCache <- function(cache= TRUE, inform=FALSE, reload = FALSE){

  if(isTRUE(cache)){

    bdkey <- list(reload)

    amberData <- loadCache(bdkey)

    if(!is.null(amberData)){

      if(isTRUE(inform)) message('Barrier atlas and tracker data already downloaded succesfully.')

      return(amberData)
    }else{

      amberData <- downloadDirect()

      saveCache(amberData, key= bdkey, comment="Data downloading.", compress = TRUE)

      #remove csv files already compressed
      unlink(list.files(path = br_path('amber'), pattern = "\\.csv$", full.names = TRUE ))

      return(amberData)
    }

  }else{
    amberData <- downloadDirect()

    unlink(list.files(path = br_path('amber'), pattern = "\\.csv$", full.names = TRUE ))

    return(amberData)
  }
}


#' Formating date sin barrier track
#'
#' @param x Formatted dates

formatdate <- function(x){
  xd <- as.Date(x)
  out <- data.frame(dateout = x,year = format(xd, "%Y"),month = as.numeric(format(xd, "%m")),
                    day = as.numeric(format(xd, "%d")))
}


#' Cleaning time stamps from barrier track data
#'
#' @param ts timestamps from barrier tracker

clean_dates <- function(ts){

  extractdate <- lapply(ts, function(d) {
    dd <- as.POSIXct(d, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    date <- data.frame(x = d, dateout = format(dd, "%Y-%m-%d"))
  })

  datedf <- do.call(rbind, extractdate)

  xout  <- do.call(rbind, lapply(unique(datedf$dateout), formatdate))

  mergedf <- merge(datedf, xout, by='dateout')

}

#filtering using bounding box

#' Geo filtering
#'
#' @param x the data from barrier track or atlas
#' @param y shapefile or bounding box
#' @param mask Delineate the area by shapefile not bounding box.
#'
#' @importFrom stats complete.cases
#' @importFrom sf st_bbox st_filter st_set_crs st_as_sfc
#'
#' @return filter data cropped by bounding box

.crop <- function(x, y, mask = FALSE){

  #remove missing values from coordinates in barrier data and convert to sf

  btc <- c("lng", "lat")

  tf <- all(btc %in% colnames(x))

  if(isFALSE(tf)) crds <- c("Longitude_WGS84", "Latitude_WGS84") else crds <- btc

  b1 <-  x[complete.cases(x[, crds]), ] |> st_as_sf(coords = crds, crs = st_crs(4326))

  #if y is a shapefile, filter out records#first check if crs of x == crs y

  if(inherits(y, "sf")){

    if(isFALSE(st_crs(b1) == st_crs(y))){

      warning("The crs of the shapefile is not in WGS84. Trying to transform")

      y <- sf::st_transform(y, 4326)
    }
    yout <- if(isTRUE(mask))y else st_bbox(y)
  }else{

    if(length(y)!=4 && !is.numeric(y))stop("The bounding box should be 4 describing the bounding box in a form or xmin, ymin, xmax, ymax")

    class(y) <- "bbox"

    yout <- y
  }

  bbf <- st_as_sfc(yout) |> st_set_crs(sf::st_crs(b1))

  b2 <- st_filter(b1, bbf)
}



