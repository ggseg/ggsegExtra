# Midsagittal ----
midsagittal <- midsagittal %>%
  select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(midsagittal)){
midsagittal$pos[[i]] = list(
  dispersed = list(x = list(breaks = NULL,
                            labels = NULL),
                   y = list(breaks = NULL,
                            labels = NULL),
                   labs = list(x = "midline sagittal",
                               y = NULL))
)
usethis::use_data(midsagittal,
                  internal = FALSE, overwrite = TRUE)


# yeo7 ----
yeo7 <- yeo7 %>%
  select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(yeo7)){
  stacked = list(x = list(breaks = c(0.55, 2.1),
                          labels = c("lateral","medial")),
                 y = list(breaks = c(0.45, 1.8),
                          labels = c("left","right")),
                 labs = list(x = "side",
                             y = "hemisphere")),
  dispersed = list(x = list(breaks = c(1.42, 4.25),
                            labels = c("left","right")),
                   y = list(breaks = NULL,
                            labels = NULL),
                   labs = list(x = "hemisphere",
                               y = NULL))
)
usethis::use_data(yeo7,
                  internal = FALSE, overwrite = TRUE)

# yeo17 ----
yeo17 <- yeo17 %>%
  select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(yeo17)){
  yeo17$pos[[i]] = list(
  stacked = list(x = list(breaks = c(0.35, 1.4),
                          labels = c("lateral","medial")),
                 y = list(breaks = c(0.3, 1.35),
                          labels = c("left","right")),
                 labs = list(x = "side",
                             y = "hemisphere")),
  dispersed = list(x = list(breaks = c(.9, 2.75),
                            labels = c("left","right")),
                   y = list(breaks = NULL,
                            labels = NULL),
                   labs = list(x = "hemisphere",
                               y = NULL))
)
}
usethis::use_data(yeo17,
                  internal = FALSE, overwrite = TRUE)
