test_that("Check that ggseg:::adapt_scales is working", {
  expect_is(
    ggseg(atlas = yeo17, mapping=aes(fill=area),adapt_scales = F ) +
              scale_brain("yeo17", package="ggsegExtra"),
    c("gg","ggplot"))
  
  expect_is(
    ggseg(atlas = yeo7, mapping=aes(color=area),adapt_scales = F ) +
              scale_color_brain("yeo7", package="ggsegExtra"),
            c("gg","ggplot"))
  
  expect_equal(mode(ggseg:::adapt_scales(yeo7 %>% unnest(ggseg))), "list")
  expect_equal(mode(ggseg:::adapt_scales(yeo17 %>% unnest(ggseg))), "list")
  expect_equal(mode(ggseg:::adapt_scales(glasser %>% unnest(ggseg))), "list")
  expect_equal(mode(ggseg:::adapt_scales(tracula %>% unnest(ggseg))), "list")
  expect_equal(mode(ggseg:::adapt_scales(jhu %>% unnest(ggseg))), "list")
  expect_equal(mode(ggseg:::adapt_scales(hoCort %>% unnest(ggseg))), "list")
})
