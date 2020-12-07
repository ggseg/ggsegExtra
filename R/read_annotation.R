#' Read Freesurfer annotation file
#'
#' Reads Freesurfer binary annotation files
#' that contain information on vertex labels
#' and colours for use in analyses and
#' brain area lookups. 
#' 
#' This function is heavily
#' based on Freesurfer's read_annotation.m
#' Original Author: Bruce Fischl
#' CVS Revision Info:
#'     $Author: greve $
#'     $Date: 2014/02/25 19:54:10 $
#'     $Revision: 1.10 $
#' 
#' Copyright © 2011 The General Hospital Corporation (Boston, MA) "MGH"
#' Terms and conditions for use, reproduction, distribution and contribution
#' are found in the 'FreeSurfer Software License Agreement' contained
#' in the file 'LICENSE' found in the FreeSurfer distribution, and here:
#' https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense
#' Reporting: freesurfer@nmr.mgh.harvard.edu
#' 
#'  # TODO: replace with freesurfer::read_annotation when its incorporated on CRAN
#'  
#' @param path path to annotation file
#' @param verbose logical. 
#'
#' @return dataframe with 6 variables
#' @export
read_annotation <- function(path, verbose = TRUE){
  
  # indicate this is binary
  ff <- file(path, "rb")
  on.exit(close(ff))
  
  annot <- readBin(ff, integer(), endian = "big")
  
  tmp <- readBin(ff, integer(), n=2*annot, endian = "big")
  
  vertices <- tmp[seq(1, by=2, length.out = length(tmp)/2)]
  label <- tmp[seq(2, by=2, length.out = length(tmp)/2)]
  
  bool <- readBin(ff, integer(), endian = "big")
  
  if(is.null(bool)){
    colortable <- data.frame(matrix(NA, ncol=6, nrow = 0))
    names(colortable) <- c("label", "R", "G", "B", "A", "code")
    if(verbose) cat('No colortable in file.\n')
    
  }else if(bool == 1){
    
    # Read colortable
    numEntries <- readBin(ff, integer(), endian = "big")
    
    if(numEntries > 0){
      
      if(verbose) cat('Reading from Original Version\n')
      
    } else { # if (! numEntries > 0)
      
      version <- -numEntries
      
      if(verbose){
        if(version != 2){    
          cat(paste('Error! Does not handle version', version, '\n'))
          return()
        }else{
          cat(paste('Reading from version', version, '\n'))
        }
      }
    }
    
    numEntries <- readBin(ff, integer(), endian = "big")
    colortable.numEntries <- numEntries;
    len <- readBin(ff, integer(), endian = "big")
    
    colortable.orig_tab <- readBin(ff, character(), n = 1, endian = "big")
    colortable.orig_tab <- t(colortable.orig_tab)
    
    numEntriesToRead <- readBin(ff, integer(), endian = "big")
    
    colortable <- data.frame(matrix(NA, ncol=6, nrow = numEntriesToRead))
    names(colortable) <- c("label", "R", "G", "B", "A", "code")
    
    for(i in 1:numEntriesToRead){
      
      structure <- readBin(ff, integer(), endian = "big") + 1
      
      if (structure < 0 & verbose) cat(paste('Error! Read entry, index', structure, '\n'))
      
      if( (structure %in% colortable$label) & verbose) 
        cat(paste('Error! Duplicate Structure', structure, '\n'))
      
      len <- readBin(ff, integer(), endian = "big")
      colortable$label[structure] = t( readBin(ff, character(), n = 1, endian = "big"))
      
      colortable$R[structure] <- readBin(ff, integer(), endian = "big")
      colortable$G[structure] <- readBin(ff, integer(), endian = "big")
      colortable$B[structure] <- readBin(ff, integer(), endian = "big")
      colortable$A[structure] <- readBin(ff, integer(), endian = "big")
      
      colortable$code[structure] <- colortable$R[structure] + 
        colortable$G[structure]*2^8 + 
        colortable$B[structure]*2^16;
    } # for i
    
    if(verbose){ 
      cat(paste('colortable with', colortable.numEntries, 
                  'entries read (originally', colortable.orig_tab, ')\n'))
    }
  }else{
    if(verbose) cat('Error! Should not be expecting bool == 0\n')   
    stop(call. = FALSE)
  }
  
  # This makes it so that each empty entry at least has a string, even
  # if it is an empty string. This can happen with average subjects.
  if( any(is.na(colortable$label))){
    colortable$label[is.na(colortable$label)] = ""
  }
  
  return(
    annotation(vertices = vertices,
         label = label,
         colortable = colortable)
  )
}

#' Make object into class freesurfer_annotation
#' 
#' @param x list of three: vertices, labels and colortable
#'
#' @export
as_annotation <- function(x){
  stopifnot(class(x) == "list")
  stopifnot(all(names(x) %in% c("vertices", "labels", "colortable")))
  structure(
    x,
  class = "freesurfer_annotation"
  )
}

#' Contructor for freesurfer_annotation-class
#' 
#' freesurfer_annotation is a special object
#' containing the three components of a 
#' FreeSurfer annotation file: vertices, labels
#' and the colortable
#'  
#' @param vertices vector of vertices
#' @param labels vector of labels
#' @param colortable color table
#'
#' @export
annotation <- function(vertices, labels, colortable = NULL){
  x <- list(vertices = vertices,
            labels = labels,
            colortable = colortable)
  as_annotation(x)
}

is.annotation <- function(x) inherits(x, "freesurfer_annotation")
is_annotation <- is.annotation
  
#' @export
format.freesurfer_annotation <- function(x, ...){
  c(sprintf("# Freesurfer annotation"),
    utils::capture.output(utils::str(x))[c(-1, -5)])
}

#' @export
print.freesurfer_annotation <- function(x, ...){
  cat(format(x), sep="\n")
  invisible(x)
}
