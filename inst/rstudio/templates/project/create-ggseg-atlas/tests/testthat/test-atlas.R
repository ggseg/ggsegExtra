library(ggseg)
library(ggseg3d)
library(ggplot2)

# ggseg ----
context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("{GGSEG}", package = "{REPO}")), 210)

  expect_error(brain_pal("{GGSEG}"), "not a valid")

  expect_true(all(brain_regions({GGSEG}) %in% names(brain_pal("{GGSEG}", package = "{REPO}"))))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_brain_atlas({GGSEG}))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = {GGSEG}),c("gg","ggplot"))

  expect_is(ggseg(atlas = {GGSEG}, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = {GGSEG}, mapping = aes(fill = region)) +
              scale_fill_brain("{GGSEG}", package = "{REPO}"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = {GGSEG}, mapping = aes(fill = region)) +
              scale_fill_brain("{GGSEG}", package = "{REPO}"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = {GGSEG}, mapping=aes(fill=region), adapt_scales = FALSE ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas={GGSEG}_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas({GGSEG}_3d))

})
