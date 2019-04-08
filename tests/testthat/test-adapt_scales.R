context("test-adapt_scales")

test_that("Check that adapt_scales is working", {
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt_scales = F ) +
              scale_brain("yeo17"),c("gg","ggplot"))
  
  expect_is(ggseg(atlas = yeo7, mapping=aes(color=area),adapt_scales = F ) +
              scale_color_brain("yeo7"),c("gg","ggplot"))
  
  expect_equal(mode(adapt_scales(yeo7 %>% unnest(ggseg))), "list")
  expect_equal(mode(adapt_scales(yeo17 %>% unnest(ggseg))), "list")
 # expect_equal(mode(adapt_scales(glasser %>% unnest(ggseg))), "list")
  expect_equal(mode(adapt_scales(tracula %>% unnest(ggseg))), "list")
  expect_equal(mode(adapt_scales(jhu %>% unnest(ggseg))), "list")
})
