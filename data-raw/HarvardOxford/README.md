# Converting the Harvard-Oxford cortical atlas to ggseg

The Harvard-Oxford atlas is available with FSL. Here is a process for converting the cortical
parts, via FreeSurfer, to a ggseg format.

## Prerequisites

FSL, FreeSurfer, connectome workbench, parallel, ImageMagick, R.

## Converting to FreeSurfer and fixing

1. Take a copy of the fsaverage folder, and set up SUBJECTS_DIR to point to the folder containing the fsaverage copy.

1. Import the atlas as a surface
```bash
cd ${SUBJECTS_DIR}/fsaverage/mri
mri_vol2surf --sd $(pwd)/../.. --src ${FSLDIR}/data/atlases/HarvardOxford/HarvardOxford-cort-maxprob-thr25-1mm.nii.gz --mni152reg --out  harvard_oxford_surf_rh.mgh --hemi rh --projfrac 0.5

mri_vol2surf --sd $(pwd)/../.. --src ${FSLDIR}/data/atlases/HarvardOxford/HarvardOxford-cort-maxprob-thr25-1mm.nii.gz --mni152reg --out harvard_oxford_surf_lh.mgh --hemi lh --projfrac 0.5

```

1. Create a color table. Run an R session in `${SUBJECTS_DIR}/fsaverage/mri`
```r
source("ho_ctab.R")
```

1. Create a series of individual label files
```
mkdir Labels
seq 1 48 | parallel mri_vol2label --c harvard_oxford_surf_rh.mgh --id {} --surf fsaverage rh --l ./Labels/rh_HO_{}.label
seq 1 48 | parallel mri_vol2label --c harvard_oxford_surf_lh.mgh --id {} --surf fsaverage lh --l ./Labels/lh_HO_{}.label

```

1. Create annotation files
``` bash
LABS=$(seq 1 48 | parallel echo -n '\ --l ./Labels/lh_HO_{}.label\ ')
mris_label2annot --sd ${SUBJECTS_DIR} --s fsaverage --ctab ../label/ho.annot.ctab ${LABS} --h lh --a ho

LABS=$(seq 1 48 | parallel echo -n '\ --l ./Labels/rh_HO_{}.label\ ')

mris_label2annot --sd ${SUBJECTS_DIR} --s fsaverage --ctab ../label/ho.annot.ctab ${LABS} --h rh --a ho
```

1. Check how the surface looks - note lots of holes etc. Using the 0 prob threshold leads to more
bleed to neighboring sulci, which I think is harder to fix.
```bash
vglrun freeview  --surface ../surf/rh.inflated:annot=../label/rh.ho.annot --surface ../surf/lh.inflated:annot=../label/lh.ho.annot

```

1. Fill and smooth
``` bash
# convert to gifti
mris_convert --annot ../label/rh.ho.annot ../surf/rh.inflated ./fsaverage_rh.label.gii
mris_convert --annot ../label/lh.ho.annot ../surf/lh.inflated ./fsaverage_lh.label.gii

mris_convert --annot ../label/rh.aparc.annot ../surf/rh.inflated ./fsaverage_aparc_rh.label.gii
mris_convert --annot ../label/lh.aparc.annot ../surf/lh.inflated ./fsaverage_aparc_lh.label.gii

./smooth_labels.sh fsaverage_lh_10.label.gii inflated_lh.surf.gii fsaverage_lh_10.smooth.label.gii
./smooth_labels.sh fsaverage_rh_10.label.gii inflated_rh.surf.gii fsaverage_rh_10.smooth.label.gii

## Back to viewing with freesurfer
for hemi in lh rh ; do
mris_convert --annot fsaverage_${hemi}_10.smooth.label.gii inflated_${hemi}.surf.gii ./${hemi}.ho.smooth.annot
done
```

1. Screengrabs with tksurfer. This can be reun somewhere other than fsaverage.

``` bash
./mkPics.sh
```

1. Rename the unknowns to medial wall
```bash
mv PicsHarvardOxford/lh_\?\?\?_med.tif PicsHarvardOxford/lh_medialwall_med.tif
mv PicsHarvardOxford/rh_\?\?\?_med.tif PicsHarvardOxford/rh_medialwall_med.tif

rm PicsHarvardOxford/*_\?*
```

1. Finally, let the R spatial tools work their magic...
```R
source("mkHO.R")
```

This creates `ho_atlas.Rda` which contains a couple of data frames, almost ready for inclusion in ggsegExtra.

# Comments

I suspect something similar will be possible with tkedit, or other viewers that are able to look at the subcortical structures. Otherwise maximal projections
after thresholding will probably suffice.
