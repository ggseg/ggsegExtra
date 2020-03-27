#' #' Turn dpv to map
#' #' 
#' #' some information
#' #' 
#' #' 
#' #' mapsize defaults are:
#' #'    - Vertexwise data:  2^16    (max allowed = 2^24)
#' #'    - Facewise data:    2^15-1  (max allowed = 2^15-1 = 32767)
#' #'    
#' #' coption details:
#' #' For 'dual' = TRUE:
#' #'       If coption is false, don't rescale the extremities of the
#' #'       colourmap. Default is true, i.e., produce a higher contrast.
#' #' For 'dual' = FALSE:
#' #'       If coption is false, show the out-of-range values with the
#' #'       colour specified by 'colourgap'. Default is true, so the
#' #'       out-of-range values are shown with the extremities of the
#' #'       colourmap.
#' #'
#' #' @param dpv Data-per-vertex file, in ASCII format
#' #' @param surface Surface file, in ASCII format
#' #' @param range Range to plot
#' #' @param mapsize Maximum number of colours in the colourmap. 
#' #' @param dual 
#' #' @param coption logical. The behavior varies if 'dual' is true or not. See details.
#' #' @template verbose 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' dpv2map <- function(dpv = "~/Desktop/test2/fsaverage5/atlas/aparc.a2009s/ascii/rh.aparc.a2009s.dpv", 
#'                     surface = "~/Desktop/test2/fsaverage5/srf/ascii/rh.inflated.dpv", 
#'                     range = NULL, 
#'                     mapsize = 2^16,
#'                     dual = FALSE,
#'                     coption = TRUE,
#'                     na.color = "darkgrey",
#'                     verbose = TRUE){
#'   
#'   # Read the data file and the surface geometry
#'   dpx_full <- read.table(dpv)
#'   dpx <- dpx_full$V5
#'   # Remove NA, NaN, and Inf
#'   didx0 <- which(!is.na(dpx) | !is.infinite(dpx))
#'   
#'   surf <- read_dpv(surface)
#'   
#'   facewise <- is_facewise(dpx_full, surf, verbose)
#'   
#'   dpxrange <- c(min = min(dpx[didx0]),
#'                 max = max(dpx[didx0]))
#'   
#'   # fix ranges to show
#'   if(is.null(range)){
#'     showrange <- dpxrange
#'   }else{
#'     showrange <- c(
#'       ifelse(range[1] < dpxrange["min"],
#'              unname(dpxrange["min"]), 
#'              range[1]),
#'       ifelse(range[2] > dpxrange["max"],
#'              unname(dpxrange["max"]), 
#'              range[2])
#'     )
#'   }
#'   
#'   # Make the infinites with the same colour as the max/min
#'   dpx[is.infinite(dpx) & dpx < 0] <- dpxrange["min"]
#'   dpx[is.infinite(dpx) & dpx > 0] <- dpxrange["max"]
#'   didx = !is.na(dpx) # indices to be used (not Inf or NaN)
#'   
#'   # Some sanity checks
#'   if (dpxrange["min"] == dpxrange["max"] | showrange["min"] == showrange["max"]){
#'     stop('Invalid intervals for datarange and/or range', call. = FALSE)
#'   } # end sanity
#'   
#'   # Set mapsizes to meaningful size
#'   if(!facewise & mapsize > 2^24)  mapsize = 2^24
#'   if( facewise & mapsize > 32767) mapsize = 32767
#'   
#'   # Define the actual colourmap size.
#'   if(dual){
#'     mapsz = (dpxrange["max"] - dpxrange["min"]) * mapsize / (showrange["max"] - showrange["min"])
#'   }else{
#'     mapsz = (dpxrange["max"] - dpxrange["min"]) * (mapsize - 1) / 
#'       ((dpxrange["max"] - dpxrange["min"]) - (showrange["max"] - showrange["min"]))
#'   }
#'   
#'   mapsz <- unname(ceiling(mapsz))
#'   
#'   
#'   # Define the colourmap
#'   cols <- terrain.colors(mapsz)
#'   if(any(!didx) | dual | !coption){
#'     na.color <- col2rgb("darkgrey")
#'     na.color <- rgb(na.color[1], na.color[2], na.color[3], maxColorValue = 255)
#'     cols <- rbind(cols, na.color)
#'   }
#'   
#'   
#'     
#' }
#' 
