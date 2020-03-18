
# Set input file/atlas
infile <- paste0(fslr::fsldir(), "/data/atlases/HarvardOxford/HarvardOxford-cort-maxprob-thr25-2mm.nii.gz")

# Set output folder for all outputs
outdir <- "~/Desktop/ho-cort1/"
verbose <- TRUE
projfrac = 0
smoothing = 2
eroding = 

# create colour annotation file
if(!dir.exists(paste0(outdir, "annots/"))) dir.create(paste0(outdir, "annots/"), recursive = TRUE)
annot_lab <- get_fsl_colour("HarvardOxford-Cortical",
                           paste0(outdir, "annots/annots.ctab"))

# tt <- nifti_2_atlas(infile, outdir, annot_lab, "ho", steps = 5)

atlas_vol2surf(infile, outdir, projfrac, verbose)

# labels ----
k <- atlas_vol2label(annot_lab, outdir, verbose)

# ctab ----
atlas_lab2ctab(outdir, verbose)

# convert to gifti
atlas_labelgii(
  paste0(outdir, "/labels/"), 
  paste0(outdir, "/annots/")
)

# mris_convert --annot ../label/rh.ho.annot ../surf/rh.inflated ./fsaverage_rh.label.gii
# mris_convert --annot ../label/lh.ho.annot ../surf/lh.inflated ./fsaverage_lh.label.gii
# 
# mris_convert --annot ../label/rh.aparc.annot ../surf/rh.inflated ./fsaverage_aparc_rh.label.gii
# mris_convert --annot ../label/lh.aparc.annot ../surf/lh.inflated ./fsaverage_aparc_lh.label.gii
# 
# ./smooth_labels.sh fsaverage_lh_10.label.gii inflated_lh.surf.gii fsaverage_lh_10.smooth.label.gii
# ./smooth_labels.sh fsaverage_rh_10.label.gii inflated_rh.surf.gii fsaverage_rh_10.smooth.label.gii
# 

# tcl ----
atlas_tcl(annot_lab, outdir, verbose)

# isolate colour ----
atlas_isolate(outdir, smoothing, verbose = verbose)

# raster ----
rasterobjs <- atlas_raster(outdir)

# combine to make df
atlas_df <- atlas_raster2sf(rasterobjs)

atlas_df_gg <- atlas_sf2gg(atlas_df, "ho")

ggseg(atlas = atlas_df_gg, mapping=aes(fill=area), 
      colour="grey", alpha = .7,
      position = "stacked")

# save(atlas_df_gg,  file="ho_atlases.Rda")

# k <- nifti_2_atlas(infile, outdir, annot_lab, atlas_name = "ho-cort", smoothing = 6)

ggseg(atlas = atlas_df_gg, mapping=aes(fill=area), colour="grey",
        position = "stacked")

# save(atlas_df_gg,  file="ho_atlases.Rda")
