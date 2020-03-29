make_ggseg_repo <- function(atlas_name, 
                            package_name,
                            directory = package_name,
                            ggseg = TRUE,
                            ggseg3d = TRUE
                            ){
  
  system(paste0("git clone https://github.com/LCBC-UiO/ggsegTracula ", directory))
  
  # remove raw and data. they must create new
  dd <- list.files(file.path(directory, c("data-raw/", "data")), 
                   recursive = TRUE, full.names = TRUE)
  k <- sapply(dd, file.remove)
  
  # rename atlas name files
  dd <- list.files(directory, "tracula", recursive = TRUE, full.names = TRUE)
  k <- sapply(dd, function(x) 
    file.rename(from = x, to = gsub("tracula", atlas_name, x))
    )
  
  # rename package name files
  dd <- list.files(directory, "ggsegTracula", recursive = TRUE, full.names = TRUE)
  k <- sapply(dd, function(x) 
    file.rename(from = x, to = gsub("ggsegTracula", package_name, x))
  )
  
  # find strings in files and rename them
  dd <- list.files(directory, recursive = TRUE, full.names = TRUE)
  d2 <- lapply(dd, readLines)

  d2 <- lapply(d2, function(x) gsub("tracula", atlas_name, x))
  d2 <- lapply(d2, function(x) gsub("ggsegTracula", package_name, x))
  
  j <- mapply(writeLines, d2, dd)

  
  # remove some extra files
  dd <- c(file.path(directory, "LICENSE", "LICENSE.md"))
  k <- sapply(dd, file.remove)
  
  # remove .git to reset git history
  unlink(file.path(directory, ".git"), recursive = TRUE, force = TRUE)
  
}



# unlink(directory, recursive = TRUE)
