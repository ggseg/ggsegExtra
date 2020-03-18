context("test-ggseg")

test_that("Check that polygon atlases are working", {
  expect_is(ggseg(atlas = jhu),c("gg","ggplot"))
  expect_is(ggseg(atlas = glasser, mapping=aes(fill=area), 
                  position="stacked"),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))
  
  
  expect_is(ggseg(atlas = yeo17),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo7),c("gg","ggplot"))
  expect_is(ggseg(atlas = glasser),c("gg","ggplot"))
  expect_is(ggseg(atlas = jhu),c("gg","ggplot"))
  expect_is(ggseg(atlas = tracula),c("gg","ggplot"))
  expect_is(ggseg(atlas = hoCort),c("gg","ggplot"))
})
