pkg <- "ggsegExtra"
test_that("check new palettes work", {
  expect_equal(length(brain_pal("yeo7", package = pkg)), 
               7)
  expect_equal(length(brain_pal("yeo17", package = pkg)), 
               17)
  expect_equal(length(brain_pal("tracula", package = pkg)), 
               11)
  expect_equal(length(brain_pal("jhu", package = pkg)), 
               12)
  expect_equal(length(brain_pal("glasser", package = pkg)), 
               180)
})
