#' Initiate a ggseg atlas repo
#' 
#' This function will clone an existing
#' ggseg atlas, and rename files and substitute
#' strings in files to prepare a repository for
#' ggseg atlas constribution. 
#' 
#' This process is not perfect, and you will need
#' to go through files and make sure the information
#' is correct, add data to the data-folder, alter
#' atlas description in R-folder, make sure
#' tests run in tests/testthat (comment out what is
#' not necessary), etc. 
#' 
#' At the end, a complete atlas ready for addition to
#' ggsegExtra has noe errors and no warnings,
#' but notes on data size are acceptable.
#'
#' @param atlas_name name for atlases, should be lower snake-case
#' @param package_name package name in snake case
#' @param directory directory to place the atlas in, defaults to package name
#' @param init_git should git repo be initiated
#' @param remote_repo a remote git repository to push to (must already exist)
#'
#' @export
make_ggseg_repo <- function(atlas_name, 
                            package_name,
                            directory = package_name,
                            git_init = TRUE,
                            remote_repo = NULL
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
  dd <- c(file.path(directory, c("LICENSE", "LICENSE.md", "R/sysdata.rda")))
  k <- sapply(dd, file.remove)
  
  # remove .git to reset git history
  unlink(file.path(directory, ".git"), recursive = TRUE, force = TRUE)
  
  cat(crayon::cyan("Files created and updated.\n"))
  cat(crayon::cyan("\tAdd atlas data in", crayon::italic("data/"), "\n"))
  cat(crayon::cyan("\tAlter atlas description in", crayon::italic("R/"), ".\n"))
  cat(crayon::cyan("\tAlter atlas description in", crayon::italic("R/"), ".\n"))
  cat(crayon::cyan("\tGet package checks to have no errors & no warnings .\n"))
  
  if(git_init){
  
    system("git init; git add .; git commit -m 'setting up'")
    cat(crayon::cyan("\tgit repository initiated.\n"))
    
    if(!is.null(remote_repo)){
      system(paste("git remote add origin", remote_repo))
      system("git push -u origin master")
      cat(crayon::cyan("\trepository pushed to", crayon::italic(remote_repo), ".\n"))
      
    }
  }
  

}



# unlink(directory, recursive = TRUE)
