library(dplyr)
testthat::test_that("Check that ggseg:::adapt_scales is working", {
  
  if (requireNamespace("ggsegYeo2011", quietly = TRUE)) {
    library(ggsegYeo2011)
    testthat::expect_is(ggseg::ggseg(
      atlas = yeo17,
      mapping = aes(fill=area),
      adapt_scales = FALSE ) +
        ggseg::scale_brain("yeo17", package = "ggsegYeo2011"),
      c("gg","ggplot"))
    
    testthat::expect_is(ggseg::ggseg(
      atlas = yeo7, 
      mapping = aes(color=area),
      adapt_scales = FALSE ) +
        ggseg::scale_color_brain("yeo7", 
                                 package = "ggsegYeo2011"),
      c("gg","ggplot"))
  }
  
  atlases = list(
    "ggsegYeo2011" = "yeo7", 
    "ggsegYeo2011" = "yeo17", 
    "ggsegGlasser" = "glasser", 
    "ggsegTracula" = "tracula", 
    "ggsegJHU" = "jhu", 
    "ggsegHO" = "hoCort")
  for (iatlas in seq_along(atlases)) {
    atlas = atlases[[iatlas]]
    atlas_package = names(atlases)[[iatlas]]
    if (requireNamespace(atlas_package, quietly = TRUE)) {
      atlas = get(atlas, envir = asNamespace(atlas_package))
      testthat::expect_equal(
        mode(ggseg:::adapt_scales(atlas %>% tidyr::unnest(ggseg))), "list")
    } else {
      message(paste0("Cannot test ", atlas, 
                     ", need to install ", atlas_package))
    }
  }
})
