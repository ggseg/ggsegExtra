context("test-brain_pals")
#library(ggseg)

test_that("check new palettes work", {
  expect_equal(length(brain_pal("yeo7")), 7)
  expect_equal(length(brain_pal("yeo17")), 17)
  expect_equal(length(brain_pal("tracula")), 11)
  expect_equal(length(brain_pal("midsagittal")), 11)
  expect_equal(length(brain_pal("jhu")), 12)
  expect_equal(length(brain_pal("glasser")), 180)
})
