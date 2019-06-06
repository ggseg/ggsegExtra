context("test-ggseg3d")

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
  
  expect_is(
    ggseg3d(atlas=jhu_3d),
    c("plotly", "htmlwidget")
  )
  
  expect_is(
    ggseg3d(atlas=tracula_3d),
    c("plotly", "htmlwidget")
  )
  
  expect_is(
    ggseg3d(atlas=icbm_3d),
    c("plotly", "htmlwidget")
  )
  
})
