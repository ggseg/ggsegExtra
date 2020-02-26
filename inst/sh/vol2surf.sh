#!/bin/bash

# split in case this has sub atlas
atlas=${1%-*}

# get subatlas
subatlas=${1#*-}

# get only first three characters
subatlas=${subatlas:0:3}

# force lower letters
subatlas=$(echo $subatlas | tr '[:upper:]' '[:lower:]')

src_file=$(ls /usr/local/fsl/data/atlases/${atlas}/*${subatlas}*maxprob-thr25-1mm*)

echo "Converting $atlas $subatlas volume to surface for $src_file"

mri_vol2surf --sd ${SUBJECTS_DIR}  --src ${src_file}  --mni152reg  --out ${atlas}_surf_rh.mgh  --hemi rh --projfrac 0.5

mri_vol2surf --sd ${SUBJECTS_DIR} --src ${src_file}  --mni152reg  --out ${atlas}_surf_lh.mgh  --hemi lh --projfrac 0.5
