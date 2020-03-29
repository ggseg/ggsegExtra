# ggseg ----
context("test-adapt_scales")
test_that("Check that ggseg:::adapt_scales is working", {
  expect_equal(mode(ggseg:::adapt_scales(unnest(cc200, ggseg))), "list")
})

context("test-palettes")
test_that("check new palettes work", {
  expect_equal(length(brain_pal("cc200", package = "ggsegCC200")), 11)

  expect_error(brain_pal("cc200"), "not a valid")

  expect_true(all(names(brain_pal("cc200", package = "ggsegCC200")) %in% cc200$region))
})

context("test-ggseg-atlas")
test_that("atlases are true ggseg atlases", {

  expect_true(is_ggseg_atlas(cc200))

})

context("test-ggseg")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = cc200),c("gg","ggplot"))

  expect_is(ggseg(atlas = cc200, mapping = aes(fill = region)),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = cc200, mapping = aes(fill = region)) +
              scale_fill_brain("cc200", package = "ggsegCC200"),
            c("gg","ggplot"))

  expect_is(ggseg(atlas = cc200, mapping = aes(fill = region)) +
              scale_fill_brain("cc200", package = "ggsegCC200"),
            c("gg","ggplot"))

  expect_warning(ggseg(atlas = cc200, mapping=aes(fill=area),
                  position="stacked"), "Cannot stack")

  expect_is(ggseg(atlas = cc200, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))

})


# ggseg3d ----
context("test-ggseg3d")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=cc200_3d),
    c("plotly", "htmlwidget")
  )
})



context("test-ggseg3d-atlas")
test_that("atlases are true ggseg3d atlases", {

  expect_true(is_ggseg3d_atlas(cc200_3d))

})
