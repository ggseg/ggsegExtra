devtools::load_all(".")
options(freesurfer.verbose = FALSE)
progressr::handlers(global = TRUE)
future::plan(
  future::multisession,
  workers = future::availableCores() - 2
)

# subcortical ----

# Make  3d ----

aseg2_3d <- make_volumetric_2_3datlas(
  subject = "fsaverage5",
  subjects_dir = freesurfer::fs_subj_dir(),
  template = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz"),
  output_dir = "~/Desktop/dkt3dtest1"
)

ggseg3d(atlas = aseg2_3d)

# make atlas ----
aseg2 <- make_volumetric_ggseg(
  subject = "fsaverage5",
  steps = 1:8,
  skip_existing = FALSE,
  tolerance = 0.6,
  subjects_dir = freesurfer::fs_subj_dir(),
  vertex_size_limits = c(10, NA),
  label_file = file.path(
    freesurfer::fs_subj_dir(),
    "fsaverage5",
    "mri/aseg.mgz"
  ),
  output_dir = "~/Desktop/aseg1"
)

aseg2$data <- filter(
  aseg2$data,
  !grepl("Unknown", label, ignore.case = TRUE),
  !grepl("White-matter", label, ignore.case = TRUE)
) |>
  mutate(region = ifelse(grepl("cortex", region), NA, region))
plot(aseg2, alpha = .8)
