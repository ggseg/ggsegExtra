test_that("ggseg_atlas_repos works", {
  repos <- ggseg_atlas_repos()
  
  testthat::expect_equal(names(repos),
               c("repo", "ggseg", "ggseg3d", "source", "comment", "package"))
  
  testthat::expect_equal(nrow(repos), 10)
    
})
