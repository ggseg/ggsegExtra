yeo7 <- yeo7 %>% 
  rename(region = area)
yeo17 <- yeo17 %>% 
  rename(region = area)
chenAr <- chenAr %>% 
  rename(region = area)
chenTh <- chenTh %>% 
  rename(region = area)
glasser <- glasser %>% 
  rename(region = area)
hoCort <- hoCort %>% 
  rename(region = area)
tracula <- tracula %>% 
  rename(region = area)
jhu <- jhu %>% 
  rename(region = area)

usethis::use_data(yeo7, yeo17, chenAr, chenTh, glasser, hoCort, tracula, jhu,
                  internal = FALSE, overwrite = TRUE, compress = "xz")


rename_3d <- function(data){
  data %>% 
    unnest(ggseg_3d) %>% 
    rename(region = area) %>%
    group_by(atlas, surf, hemi) %>% 
    nest() %>% 
    rename(ggseg_3d = data) %>% 
    as_ggseg3d_atlas()
}

yeo7_3d <- rename_3d(yeo7_3d)
yeo17_3d <- rename_3d(yeo17_3d)
glasser_3d <- rename_3d(glasser_3d)
tracula_3d <- rename_3d(tracula_3d)
jhu_3d <- rename_3d(jhu_3d)
desterieux_3d <- rename_3d(desterieux_3d)
hcpa_3d <- rename_3d(hcpa_3d) %>% ungroup() %>% mutate(atlas = "hcpa_3d")
icbm_3d <- rename_3d(icbm_3d)
schaefer17_3d <- rename_3d(schaefer17_3d)
schaefer7_3d <- rename_3d(schaefer7_3d)

usethis::use_data(yeo7_3d, yeo17_3d, jhu_3d, glasser_3d, tracula_3d, desterieux_3d, 
                  hcpa_3d, icbm_3d, schaefer17_3d, schaefer7_3d,
                  internal = FALSE, overwrite = TRUE, compress = "xz")
