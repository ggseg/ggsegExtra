library(ggseg)

context("new polygon atlases")
test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = jhu),c("gg","ggplot"))
  expect_is(ggseg(atlas = midsagittal, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))
  expect_is(ggseg(atlas = glasser, mapping=aes(fill=area), position="stacked"),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))
})

test_that("Check that adapt_scales is working", {
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt_scales = F ) +
              scale_brain("yeo17"),c("gg","ggplot"))
  
  expect_is(ggseg(atlas = yeo7, mapping=aes(color=area),adapt_scales = F ) +
              scale_color_brain("yeo7"),c("gg","ggplot"))
  
  expect_equal(mode(adapt_scales(yeo7)), "list")
  expect_equal(mode(adapt_scales(yeo17)), "list")
})

context("new mesh atlases")
test_that("Check that mesh atlases are working", {
  expect_is(
    ggseg3d(atlas=glasser_3d),
    c("plotly", "htmlwidget")
  )
  
  expect_is(
    ggseg3d(atlas=desterieux_3d),
    c("plotly", "htmlwidget")
  )
  
  expect_is(
    ggseg3d(atlas=schaefer7_3d),
    c("plotly", "htmlwidget")
  )
  
  expect_is(
    ggseg3d(atlas=schaefer17_3d),
    c("plotly", "htmlwidget")
  )

})


# covr::zero_coverage(covr::package_coverage("."))

