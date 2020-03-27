test_that("ggseg_atlas_repos works", {
  repos <- ggseg_atlas_repos()
  
  expect_equal(names(repos),
               c("repo", "ggseg", "ggseg3d", "source", "comment"))
  
  expect_equal(nrow(repos), 10)
    
})
