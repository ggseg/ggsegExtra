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
#' @param git_init should git repo be initiated
#' @param remote_repo a remote git repository to push to (must already exist)
#'
#' @export
make_ggseg_repo <- function(atlas_name, 
                            package_name,
                            directory = package_name,
                            git_init = TRUE,
                            remote_repo = NULL
){
  
  if(dir.exists(directory)) stop("Directory already exists. Aborting.", call. = FALSE)
  
  usethis::ui_todo(paste("\tSetting up package skeleton.\n"))
  system(paste0("git clone https://github.com/LCBC-UiO/ggsegTracula ", directory))
  
  # remove raw and data. they must create new
  dd <- list.files(file.path(directory, c("data-raw/", "data")), 
                   recursive = TRUE, full.names = TRUE)
  # Dont remove palettes, can help people figure out how to add one
  dd <- dd[!grepl("palette", dd)]
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

  if(git_init){
    code_new <- "usethis::use_git()"

    if(!is.null(remote_repo)){
      code_new <- c(code_new, 
                    paste("usethis::use_git_remote(name = 'origin', '",
                          remote_repo, "', overwrite = TRUE)")
      )
    }
  }else{
    code_new <- c()
  }
  
  usethis::ui_done("Folder and files created and updated.")
  usethis::ui_info("Opening new Rstudio project.")
  usethis::proj_activate(directory)
  
  usethis::ui_todo("\tAdd atlas data with {usethis::ui_code('usethis::use_data()')}")
  usethis::ui_todo("\tAdd license with {usethis::ui_code('usethis::use_mit_license()')}")
  usethis::ui_todo("\tAlter atlas description in {usethis::ui_code('R/')}")
  usethis::ui_todo("\tGet package checks to have no errors & no warnings")
  usethis::ui_info("See help at {usethis::ui_code('https://lcbc-uio.github.io/ggsegExtra/articles/ggsegrepo.html')}")
  
  code_new <- c(code_new,
                "file.edit('DESCRIPTION')",
                paste0("file.edit('R/", atlas_name, ".R')"),
                "file.edit('tests/testthat/test-atlas.R')",
                "file.edit('data-raw/palettes.R')"
  )
  
  usethis::ui_todo("Run these lines of code in the new project:\n
                   {usethis::ui_code_block(code_new)}")

}



# unlink(directory, recursive = TRUE)
