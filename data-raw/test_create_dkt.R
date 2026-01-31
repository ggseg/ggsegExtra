devtools::load_all(".")
library(tidyverse)
progressr::handlers(global = TRUE)

future::plan(
  future::multisession,
  workers = future::availableCores() - 2
)

# cortical ----
# convert DKT to fsaverage5
mri_surf2surf_rereg(
  subject = "bert",
  annot = "aparc.DKTatlas",
  hemi = "lh",
  output_dir = "data-raw/"
)

mri_surf2surf_rereg(
  subject = "bert",
  annot = "aparc.DKTatlas",
  hemi = "rh",
  output_dir = "data-raw/"
)

# Make  3d ----
dkt_3d_init <- make_aparc_2_3datlas(
  annot = "aparc.DKTatlas",
  annot_dir = "data-raw/",
  output_dir = "~/Desktop/test1"
)
dkt_3d <- dkt_3d_init |>
  mutate(atlas = "dkt_3d") |>
  unnest(ggseg_3d) |>
  select(-region) |>
  left_join(
    select(dk$data, hemi, region, label),
    relationship = "many-to-many"
  ) |>
  mutate(random = "test column") |>
  nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
  as_ggseg3d_atlas()
ggseg3d(atlas = dkt_3d)

# make atlas ----
dkt <- make_ggseg3d_2_ggseg(
  dkt_3d,
  steps = 1:7,
  smoothness = 2,
  tolerance = .8,
  output_dir = "~/Desktop/test1",
  view = c("medial", "lateral", "ventral", "dorsal")
)
plot(dkt, show.legend = FALSE)
