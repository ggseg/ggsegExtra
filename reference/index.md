# Package index

## Atlas Discovery & Installation

Find and install ggseg atlas packages

- [`ggseg_atlas_repos()`](https://ggseg.github.io/ggsegExtra/reference/ggseg_atlas_repos.md)
  : List all online repositories with ggseg-atlases
- [`install_ggseg_atlas()`](https://ggseg.github.io/ggsegExtra/reference/install_ggseg_atlas.md)
  : Install ggseg-atlas from repo
- [`install_ggseg_atlas_all()`](https://ggseg.github.io/ggsegExtra/reference/install_ggseg_atlas_all.md)
  : Install all registered ggseg-atlases

## Create 3D Atlases

Functions to create ggseg3d-compatible atlases

- [`make_aparc_2_3datlas()`](https://ggseg.github.io/ggsegExtra/reference/make_aparc_2_3datlas.md)
  : Create cortical ggseg3d-atlas from annot-file
- [`make_volumetric_2_3datlas()`](https://ggseg.github.io/ggsegExtra/reference/make_volumetric_2_3datlas.md)
  : Volumetric segmentation to 3d-atlas

## Create 2D Atlases

Functions to create ggseg-compatible atlases

- [`make_ggseg3d_2_ggseg()`](https://ggseg.github.io/ggsegExtra/reference/make_ggseg3d_2_ggseg.md)
  : Turn ggseg3d-atlas to ggseg
- [`make_volumetric_ggseg()`](https://ggseg.github.io/ggsegExtra/reference/make_volumetric_ggseg.md)
  : Make ggseg atlas from volumetric template
- [`make_palette_ggseg()`](https://ggseg.github.io/ggsegExtra/reference/make_palette_ggseg.md)
  : Create ggseg palette from ggseg3d-atlas

## Volume & Surface Conversion

Convert between volumetric and surface representations

- [`atlas_vol2surf()`](https://ggseg.github.io/ggsegExtra/reference/atlas_vol2surf.md)
  : Nifti volume to surface
- [`atlas_vol2label()`](https://ggseg.github.io/ggsegExtra/reference/atlas_vol2label.md)
  : Volume to label
- [`mri_vol2label()`](https://ggseg.github.io/ggsegExtra/reference/mri_vol2label.md)
  : Convert volume to label

## Color Tables

Read, write, and manipulate FreeSurfer color tables

- [`read_ctab()`](https://ggseg.github.io/ggsegExtra/reference/read_ctab.md)
  : Read colourtab
- [`write_ctab()`](https://ggseg.github.io/ggsegExtra/reference/write_ctab.md)
  : Write colourtab
- [`is_ctab()`](https://ggseg.github.io/ggsegExtra/reference/is_ctab.md)
  : Check if object is colourtable
- [`atlas_lab2ctab()`](https://ggseg.github.io/ggsegExtra/reference/atlas_lab2ctab.md)
  : Label to ctab

## Mesh Functions

Work with 3D mesh data

- [`get_mesh()`](https://ggseg.github.io/ggsegExtra/reference/get_mesh.md)
  : Extract mesh data from ply
- [`aparc_2_mesh()`](https://ggseg.github.io/ggsegExtra/reference/aparc_2_mesh.md)
  : Converts annotations to atlas
- [`surf2ply()`](https://ggseg.github.io/ggsegExtra/reference/surf2ply.md)
  : Convert Freesurfer surface file into ply
- [`curv2ply()`](https://ggseg.github.io/ggsegExtra/reference/curv2ply.md)
  : Convert Freesurfer curvature file to ply

## FreeSurfer Wrappers

R wrappers for FreeSurfer command-line tools

- [`mris_label2annot()`](https://ggseg.github.io/ggsegExtra/reference/mris_label2annot.md)
  : Convert Label to Annotation
- [`mri_annotation2label()`](https://ggseg.github.io/ggsegExtra/reference/mri_annotation2label.md)
  : Convert annotation to label
- [`mris_ca_label()`](https://ggseg.github.io/ggsegExtra/reference/mris_ca_label.md)
  : MRI ca label
- [`mri_surf2surf_rereg()`](https://ggseg.github.io/ggsegExtra/reference/mri_surf2surf_rereg.md)
  : Re-register an annotation file
- [`subject_2_ascii()`](https://ggseg.github.io/ggsegExtra/reference/subject_2_ascii.md)
  : Convert subject surface files to ascii

## FreeSurfer Constants

Available FreeSurfer surfaces and curvatures

- [`fs_surfaces()`](https://ggseg.github.io/ggsegExtra/reference/fs_surfaces.md)
  : Available Freesurfer surfaces
- [`fs_curvatures()`](https://ggseg.github.io/ggsegExtra/reference/fs_curvatures.md)
  : Available Freesurfer curvatures
- [`fs_nofixcurv()`](https://ggseg.github.io/ggsegExtra/reference/fs_nofixcurv.md)
  : Available Freesurfer no fix curvatures
