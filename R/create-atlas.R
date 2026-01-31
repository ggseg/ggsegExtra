# adapted from https://github.com/rstudio/distill/blob/master/R/create.R
# So that we can easily create new distill pages with uio templateing.
# Lots of internal calling, just get it working for now
create_atlas_repo <- function(dir, atlas_name, edit = interactive()) {
  type <- "create-ggseg-atlas"
  atlas_name <- tolower(atlas_name)
  repo_name <- sprintf("ggseg%s", tools::toTitleCase(atlas_name))
  mkdir(dir)

  # copy template files
  template_dir <- system.file(
    file.path("rstudio", "templates", "project", type),
    package = "ggsegExtra"
  )
  dirs <- list.dirs(template_dir, full.names = FALSE)
  k <- lapply(
    dirs,
    function(x) mkdir(file.path(dir, x))
  )

  files <- list.files(template_dir, recursive = TRUE)
  k <- lapply(files, function(x) {
    file.copy(file.path(template_dir, x), file.path(dir, x))
  })

  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  k <- lapply(files, template_replace, atlas_name = atlas_name)
}


new_project_create_atlas_repo <- function(dir, ...) {
  params <- list(...)
  create_atlas_repo(dir, params$atlas_name, edit = FALSE)
}

template_replace <- function(file, atlas_name) {
  repo_name <- sprintf("ggseg%s", tools::toTitleCase(atlas_name))
  input <- readLines(file)
  output <- gsub("\\{GGSEG\\}", atlas_name, input)
  output <- gsub("\\{REPO\\}", repo_name, output)
  writeLines(output, file)
}
