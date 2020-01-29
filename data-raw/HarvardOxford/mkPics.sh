#!/bin/bash

TD=$(mktemp -d)
trap 'rm -rf ${TD}; exit 0' 0 1 2 3 14 15

# script to create a heap of images of the inflated freesurfer brain
# with one label each, and in a bright colour. Idea is to make
# import into ggseg easy

# Desikan atlas version
#export DEST=PicsDesikan
export DEST=pics/${pics}

mkdir -p ${DEST}

# modules for local setup
# Note, need a recent version of imagemagick for connected component stuff
# to work properly.
#module load freesurfer/6.0 imagemagick/7.0.8.23-native

for lab in labels/lh.*.label ; do
    region=$(basename $lab)
    region=${region/.label/}
    region=${region/lh./}
    export REGNAME=${region}
    for hemi in lh rh ; do
	export INLABFILE=$(dirname $lab)/${hemi}.${region}.label
	echo ${INLABFILE}
	tksurfer fsaverage ${hemi} inflated  -tcl grab.tcl
	convert ${DEST}/fsaverage_infl_${hemi}_${region}_lat.tif -colorspace HSL -separate ${TD}/lat_%d.tif
	convert ${TD}/lat_1.tif -threshold 10%  -define connected-components:verbose=true -define connected-components:area-threshold=20 -connected-components 8 -auto-level -depth 8 $DEST/${hemi}_${region}_lat.tif
	convert ${DEST}/fsaverage_infl_${hemi}_${region}_med.tif -colorspace HSL -separate ${TD}/med_%d.tif
	convert ${TD}/med_1.tif -threshold 10%  -define connected-components:verbose=true -define connected-components:area-threshold=20 -connected-components 8 -auto-level -depth 8 ${DEST}/${hemi}_${region}_med.tif
    done
    #convert ${DEST}/fsaverage_infl_lh_${region}_lat.tif  ${DEST}/fsaverage_infl_lh_${region}_med.tif ${DEST}/fsaverage_infl_rh_${region}_med.tif ${DEST}/fsaverage_infl_rh_${region}_lat.tif +append $DEST/${region}.png
    #convert $DEST/${region}.png -colorspace HSL -separate $DEST/${region}_%d.png
done

