library(ggseg)
library(ggseg3d)

# ggseg ----
context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("brainnetome", package = "ggsegBrainnetome")), 210)

  expect_error(brain_pal("brainnetome"), "not a valid")

  expect_true(all(brain_regions(brainnetome) %in% names(brain_pal("brainnetome", package = "ggsegBrainnetome"))))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_brain_atlas(brainnetome))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = brainnetome),c("gg","ggplot"))

  expect_is(ggseg(atlas = brainnetome, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = brainnetome, mapping = aes(fill = region)) +
              scale_fill_brain("brainnetome", package = "ggsegBrainnetome"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = brainnetome, mapping = aes(fill = region)) +
              scale_fill_brain("brainnetome", package = "ggsegBrainnetome"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = brainnetome, mapping=aes(fill=region), adapt_scales = FALSE ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=brainnetome_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas(brainnetome_3d))

})
