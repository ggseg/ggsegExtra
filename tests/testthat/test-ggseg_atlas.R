test_that("atlases are true ggseg atlases", {

  expect_true(is_ggseg_atlas(yeo17))
  expect_true(is_ggseg_atlas(yeo7))
  expect_true(is_ggseg_atlas(glasser))
  expect_true(is_ggseg_atlas(jhu))
  expect_true(is_ggseg_atlas(tracula))
  expect_true(is_ggseg_atlas(hoCort))
  
})


test_that("atlases are true ggseg3d atlases", {
  
  expect_true(is_ggseg3d_atlas(yeo7_3d))
  expect_true(is_ggseg3d_atlas(yeo17_3d))
  expect_true(is_ggseg3d_atlas(glasser_3d))
  expect_true(is_ggseg3d_atlas(tracula_3d))
  expect_true(is_ggseg3d_atlas(jhu_3d))
  expect_true(is_ggseg3d_atlas(desterieux_3d))
  expect_true(is_ggseg3d_atlas(hcpa_3d))
  expect_true(is_ggseg3d_atlas(icbm_3d))
  expect_true(is_ggseg3d_atlas(schaefer17_3d))
  expect_true(is_ggseg3d_atlas(schaefer7_3d))

})
