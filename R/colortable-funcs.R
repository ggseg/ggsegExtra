# Exported functions ----
#' Write colourtab
#' 
#' write a colortab to file, will be in a
#' format that is accessible by FreeSurfer.
#'
#' @param x colourtab data
#' @param path path to write to
#' @return returns nothing, writes file
#' @export
write_ctab <- function(x, path){
  lls <- apply(x, 1, function(c)
    ctab_line(c[1], c[2], c[3], c[4], c[5], c[6])
  )
  
  # add empty row at the end
  lls[length(lls) + 1 ] = ""
  
  k <- writeLines(lls, path)
  
  invisible(lls)
}

#' Read colourtab
#' 
#' Read in a FreeSurfer colortab file.
#'
#' @param path path to read from
#' @return a data.frame with index, label name and RGBA colours 
#' @export
read_ctab <- function(path){
  x <- utils::read.table(path)
  names(x) <- c("idx", "label", "R", "G", "B", "A")
  
  return(x)
}

#' Check if object is colourtable
#'
#' @param colourtable data frame with colour table
#'
#' @return logical
#' @export
is_ctab <- function(colourtable){
  k <- is.data.frame(colourtable)
  j <- names(read_ctab) %in%  c("idx", "label", "R", "G", "B", "A")
  
  all(c(j,k))
}


# Non-exported ----
#' Get colour lut table
#' 
#' Create a color table based on a 
#' FreeSurfer color lut. 
#'
#' @param color_lut path to lut or data.frame that \code{\link{is_ctab}}
#'
#' @return colour table as a data.frame, with hex colours.
#' @noRd
get_ctab <- function(color_lut){
  
  colourtable <- if(is.character(color_lut)){
     read_ctab(color_lut)
  }else{
    color_lut  
  }
  
  if(!is_ctab(colourtable)) cat("color_lut does not have the correct information.\n",
                                "Check if the colour_lut has the correct information:\n",
                                "data.frame with names ", 
                                paste(c("idx", "label", "R", "G", "B", "A"), collapse = ", "))
  
  colourtable$roi <- sprintf("%04d", colourtable$idx)
  colourtable$color <- grDevices::rgb(colourtable$R, colourtable$G, colourtable$B, 
                                     maxColorValue = 255)
  
  return(colourtable)
}

#' Make a string for ctab writing
#'
#' @param idx roi index
#' @param name roi name
#' @param R roi red value
#' @param G roi green value
#' @param B roi blue value
#' @param A roi opacity/alpha value
#' @noRd
ctab_line <- function(idx, name, R, G, B, A){
  
  if(nchar(name) > 29){
    name <- paste0(strsplit(name, "")[[1]][1:29], collapse = "")
  }
  
  sprintf("% 3s  % -30s  % 3s % 3s % 3s % 3s", idx, name, R, G, B, A)
}
