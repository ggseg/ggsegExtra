

test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = jhu),c("gg","ggplot"))
  expect_is(ggseg(atlas = midsagittal, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))
  expect_is(ggseg(atlas = glasser, mapping=aes(fill=area), position="stacked"),c("gg","ggplot"))
})

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


#covr::zero_coverage(covr::package_coverage("."))

