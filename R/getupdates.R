

#' Get updates from AMBER databases in the data changes.

#' @param details \code{logical} If TRUE, then a printed message on the summary
#'        of the data changes will be provided. Otherwise the number of rows
#'        and columns added will be provided.
#' @param colcheck \code{string} The where the checks needs to be viewed. Otherwise all the columns
#'        will be checked.
#' @param data.table \code{logical} If TRUE, then provide a dataframe name since it will be provided.
#'      Default is a print message of the changes.
#'  @param tracker \code{logical} If TRUE, then barrier data will be checked. If FALSE, the
#'          barrier atlas changes will be provided. Default \code{FALSE}
#'
#' @return Dataframe or print message of the changes in the AMBER database.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #print messages
#' get_amber_updates(tracker= TRUE)
#'
#' }
get_amber_updates <- function(a = NULL , b = NULL, details = TRUE, colcheck,
                              data.table = FALSE, tracker= TRUE){

  cachedir <- br_path(dir='amber')

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  if(isTRUE(tracker))ind <- 2 else ind <- 1

  if(is.null(a)){
    #check if the old cached data is there to continue

    dd <- list.files(path = br_path('amber'), pattern = "\\.gz$", full.names = TRUE )

    if(length(dd)<1)stop('No data was cached, so no updates can be obtained', call. = FALSE)

    oldcached <- .dataCache(cache = TRUE)[[ind]]

    updated <- downloadDirect()[[ind]]

    unlink(list.files(path = br_path('amber'), pattern = "\\.csv$", full.names = TRUE ))
  }else{
    oldcached <- a

    updated <- b
  }
  if(isTRUE(nrow(updated)>nrow(oldcached) | ncol(updated)>ncol(oldcached))){

    ndr <- nrow(updated)- nrow(oldcached)

    ndc <- ncol(updated) - ncol(oldcached)

    if(isTRUE(tracker)) xt <- 'BT' else xt <- 'BAtlas'

    cat(ndr,' rows ', 'and ', ndc, ' cols added to ', xt,' data. Reload data: set reload to TRUE',"\n")

    if(isTRUE(details) & isTRUE(ncol(updated)==ncol(oldcached))){

      dtout <- updated[!duplicated(rbind(oldcached, updated))[-seq_len(nrow(oldcached))], ]

      if(nrow(dtout)<1)stop("More data was added but was the same across all columns.", call. = FALSE)

      dtout[dtout==""] <- "Unknown"

      cc <- if(missing(colcheck)) colnames(oldcached) else colcheck

      if(isTRUE(data.table)){

        xp <- lapply(cc, function(xx) {

          tout <- as.data.frame(table(dtout[[xx]]))

          colnames(tout) <- c('parameters', "frequency")

          tsort <- tout[order(-tout$frequency), ]

          tsort[,'colchecked'] <- xx

          tsort
        })

        return(do.call(rbind,xp))

      }else{
        for (i in cc) {

          cat("-----------------------------------", '\n')
          cat(i,"data added", '\n')
          cat("===================================", '\n')

          tout <- as.data.frame(table(dtout[[i]]))

          colnames(tout) <- c(i, "frequency")

          tsort <- tout[order(-tout$frequency), ]

          print(tsort)

          cat("===================================", '\n')
        }
      }

    }else{
      stop('You cannot get details as the column names changed since data was downloaded. set details to FALSE.')
    }
  }else{
    if(isTRUE(tracker)) x <- 'barrier tracker' else x <- 'barrier atlas'

    message('The ', x ,' has not updated since the last download.')
  }

}
