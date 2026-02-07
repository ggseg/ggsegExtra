# Volumetric Test Data

Test data for volumetric (subcortical) atlas creation pipelines.

## Files

- `aseg.mgz` - Cropped subcortical segmentation (74x56x43 voxels)
- `lut.txt` - Color lookup table for the segmentation labels

## Contents

The `aseg.mgz` file contains a cropped region from the FreeSurfer fsaverage5 
aseg.mgz, including only:

| Label | Structure       | Hemisphere |
|-------|-----------------|------------|
| 10    | Thalamus        | Left       |
| 18    | Amygdala        | Left       |
| 49    | Thalamus        | Right      |
| 54    | Amygdala        | Right      |

**Source:** Derived from FreeSurfer fsaverage5 subject 
(`$FREESURFER_HOME/subjects/fsaverage5/mri/aseg.mgz`)

**Citation:** Fischl B, Salat DH, Busa E, Albert M, Dieterich M, Haselgrove C, 
van der Kouwe A, Killiany R, Kennedy D, Klaveness S, Montillo A, Makris N, 
Rosen B, Dale AM. Whole brain segmentation: automated labeling of 
neuroanatomical structures in the human brain. Neuron. 2002 Jan 31;33(3):341-55.
