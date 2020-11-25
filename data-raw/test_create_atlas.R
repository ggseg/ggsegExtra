library(ggsegExtra)
library(tidyverse)

# cortical ----
# convert DKT to fsaverage5
mri_surf2surf_rereg(subject = "bert",
                    annot = "aparc.DKTatlas",
                    hemi = "lh",
                    output_dir = "data-raw/")

mri_surf2surf_rereg(subject = "bert",
                    annot = "aparc.DKTatlas",
                    hemi = "rh",
                    output_dir = "data-raw/")


# Make  3d ----

dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
                               annot_dir = "data-raw/",
                               output_dir = "~/Desktop") %>%
  mutate(atlas = "dkt_3d")%>%
  unnest(ggseg_3d) %>%
  select(-region) %>%
  left_join(select(dk$data, hemi, region, label)) %>%
  nest_by(atlas, surf, hemi, .key = "ggseg_3d") %>%
  as_ggseg3d_atlas()

# make atlas ----
dkt <- make_ggseg3d_2_ggseg(dkt_3d,
                            steps = 1:7,
                            smoothness = 2,
                            tolerance = .5,
                            output_dir = "~/Desktop/")
plot(dkt)


# subcortical ----

# Make  3d ----
aseg2_3d <- make_aseg_2_3datlas(
  subject = "fsaverage5",
  subjects_dir = freesurfer::fs_subj_dir(),
  template = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz")
)

ggseg3d(atlas = aseg2_3d)

# make atlas ----
aseg2 <- make_volumetric_ggseg(subject = "fsaverage5",
                               steps = 1:8,
                               skip_existing = FALSE,
                               subjects_dir = freesurfer::fs_subj_dir(),
                               vertex_size_limits = c(10, NA),
                               label_file = file.path(freesurfer::fs_subj_dir(), "fsaverage5", "mri/aseg.mgz"))

aseg2$data <- filter(aseg2$data, 
                     !grepl("Unknown", label, ignore.case = TRUE),
                     !grepl("White-matter", label, ignore.case = TRUE)
) %>% 
  mutate(region = ifelse(grepl("cortex", region), NA, region))
plot(aseg2, alpha = .8)
