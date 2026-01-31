pkgname <- "ggsegExtra"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggsegExtra')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aparc_2_mesh")
### * aparc_2_mesh

flush(stderr()); flush(stdout())

### Name: aparc_2_mesh
### Title: Converts annotations to atlas
### Aliases: aparc_2_mesh

### ** Examples

## Not run: 
##D dt <- aparc_2_mesh()
##D dt <- aparc_2_mesh(surface = "white")
##D dt <- aparc_2_mesh(hemisphere = "lh")
##D dt <- aparc_2_mesh(annot = "aparc.a2009s")
## End(Not run)




cleanEx()
nameEx("fs_curvatures")
### * fs_curvatures

flush(stderr()); flush(stdout())

### Name: fs_curvatures
### Title: Available Freesurfer curvatures
### Aliases: fs_curvatures

### ** Examples

fs_curvatures()



cleanEx()
nameEx("fs_nofixcurv")
### * fs_nofixcurv

flush(stderr()); flush(stdout())

### Name: fs_nofixcurv
### Title: Available Freesurfer no fix curvatures
### Aliases: fs_nofixcurv

### ** Examples

fs_nofixcurv()



cleanEx()
nameEx("fs_surfaces")
### * fs_surfaces

flush(stderr()); flush(stdout())

### Name: fs_surfaces
### Title: Available Freesurfer surfaces
### Aliases: fs_surfaces

### ** Examples

fs_surfaces()



cleanEx()
nameEx("get_mesh")
### * get_mesh

flush(stderr()); flush(stdout())

### Name: get_mesh
### Title: Extract mesh data from ply
### Aliases: get_mesh

### ** Examples

## Not run: 
##D get_mesh("path/to/surface.ply")
##D 
##D # Turn off showing the ply when reading
##D get_mesh("path/to/surface.ply", ShowSpecimen = FALSE)
## End(Not run)



cleanEx()
nameEx("ggseg_atlas_repos")
### * ggseg_atlas_repos

flush(stderr()); flush(stdout())

### Name: ggseg_atlas_repos
### Title: List all online repositories with ggseg-atlases
### Aliases: ggseg_atlas_repos

### ** Examples

## Not run: 
##D ggseg_atlas_repos()
##D 
##D ggseg_atlas_repos("yeo")
## End(Not run)



cleanEx()
nameEx("install_ggseg_atlas")
### * install_ggseg_atlas

flush(stderr()); flush(stdout())

### Name: install_ggseg_atlas
### Title: Install ggseg-atlas from repo
### Aliases: install_ggseg_atlas

### ** Examples

## Not run: 
##D ggseg_atlas_repos("yeo")
##D install_ggseg_atlas("ggsegYeo2011")
## End(Not run)



cleanEx()
nameEx("install_ggseg_atlas_all")
### * install_ggseg_atlas_all

flush(stderr()); flush(stdout())

### Name: install_ggseg_atlas_all
### Title: Install all registered ggseg-atlases
### Aliases: install_ggseg_atlas_all

### ** Examples

## Not run: 
##D install_ggseg_atlas_all()
## End(Not run)



cleanEx()
nameEx("make_aparc_2_3datlas")
### * make_aparc_2_3datlas

flush(stderr()); flush(stdout())

### Name: make_aparc_2_3datlas
### Title: Create cortical ggseg3d-atlas from annot-file
### Aliases: make_aparc_2_3datlas

### ** Examples

## Not run: 
##D dt <- aparc_2_3datlas(annot = "aparc.a2009s")
##D dt <- aparc_2_3datlas(annot = "aparc.a2009s",
##D                       surface = "sphere")
## End(Not run)



cleanEx()
nameEx("make_ggseg3d_2_ggseg")
### * make_ggseg3d_2_ggseg

flush(stderr()); flush(stdout())

### Name: make_ggseg3d_2_ggseg
### Title: Turn ggseg3d-atlas to ggseg
### Aliases: make_ggseg3d_2_ggseg

### ** Examples

## Not run: 
##D 
##D # Create the DKT atlas as found in the FreeSurfer Subjects directory
##D # And output the temporary files to the Desktop.
##D dkt_3d <- make_aparc_2_3datlas(annot = "aparc.DKTatlas",
##D     output_dir = "~/Desktop/")
## End(Not run)



cleanEx()
nameEx("make_palette_ggseg")
### * make_palette_ggseg

flush(stderr()); flush(stdout())

### Name: make_palette_ggseg
### Title: Create ggseg palette from ggseg3d-atlas
### Aliases: make_palette_ggseg

### ** Examples

make_palette_ggseg(dk_3d)



cleanEx()
nameEx("make_volumetric_2_3datlas")
### * make_volumetric_2_3datlas

flush(stderr()); flush(stdout())

### Name: make_volumetric_2_3datlas
### Title: Volumetric segmentation to 3d-atlas
### Aliases: make_volumetric_2_3datlas

### ** Examples

## Not run: 
##D 
##D fs_subject_dir <- freesurfer::fs_dir()
##D aseg_temp <- file.path(fs_subject_dir, "fsaverage5/mri/aseg.mgz")
##D colorlut <- file.path(fs_subject_dir, "ASegStatsLUT.txt")
##D 
##D make_volumetric_2_3datlas(aseg_temp, colorlut)
## End(Not run)



cleanEx()
nameEx("make_volumetric_ggseg")
### * make_volumetric_ggseg

flush(stderr()); flush(stdout())

### Name: make_volumetric_ggseg
### Title: Make ggseg atlas from volumetric template
### Aliases: make_volumetric_ggseg

### ** Examples

## Not run: 
##D 
##D    label_file <- file.path(fs_subj_dir(), subject, "mri/aseg.mgz")
##D    slices = data.frame(x=130, y=130, z=130, view="axial", stringsAsFactors = FALSE)
##D 
##D    aseg2 <- make_volumetric_ggseg(
##D       label_file =  label_file,
##D       slices = slices
##D    )
##D 
##D    # Have a look at the atlas
##D    plot(aseg2)
## End(Not run)



cleanEx()
nameEx("mri_annotation2label")
### * mri_annotation2label

flush(stderr()); flush(stdout())

### Name: mri_annotation2label
### Title: Convert annotation to label
### Aliases: mri_annotation2label

### ** Examples

## Don't show: 
if (freesurfer::have_fs()) withAutoprint({ # examplesIf
## End(Don't show)
# for freesurfer help see:
freesurfer::fs_help("mri_annotation2label")
mri_annotation2label(annot_name = "aparc")

mri_annotation2label(annot_name = "aparc.a2009s")

mri_annotation2label(subject = "fsaverage", annot_name = "aparc.a2009s")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("mri_surf2surf_rereg")
### * mri_surf2surf_rereg

flush(stderr()); flush(stdout())

### Name: mri_surf2surf_rereg
### Title: Re-register an annotation file
### Aliases: mri_surf2surf_rereg

### ** Examples

## Don't show: 
if (freesurfer::have_fs()) withAutoprint({ # examplesIf
## End(Don't show)
# For help see:
freesurfer::fs_help("mri_surf2surf")

mri_surf2surf_rereg(subject = "bert",
                    annot = "aparc.DKTatlas",
                    target_subject = "fsaverage5")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("mri_vol2label")
### * mri_vol2label

flush(stderr()); flush(stdout())

### Name: mri_vol2label
### Title: Convert volume to label
### Aliases: mri_vol2label

### ** Examples

## Don't show: 
if (freesurfer::have_fs()) withAutoprint({ # examplesIf
## End(Don't show)
# for freesurfer help see:
freesurfer::fs_help("mri_vol2label")

out_dir <- tempdir()
vol <- file.path(freesurfer::fs_subj_dir(),
                 "fsaverage5/mri/aseg.mgz")

mri_vol2label(vol,
     label_id = 2,
     hemisphere = "rh",
     output_dir = out_dir)

 # delete output dir when not needed
 unlink(out_dir)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("mris_ca_label")
### * mris_ca_label

flush(stderr()); flush(stdout())

### Name: mris_ca_label
### Title: MRI ca label
### Aliases: mris_ca_label

### ** Examples

## Don't show: 
if (freesurfer::have_fs()) withAutoprint({ # examplesIf
## End(Don't show)
# for freesurfer help see:
freesurfer::fs_help("mris_ca_label")
mris_ca_label(output_file = "test.lh.annot")

mris_ca_label(hemisphere = "rh", output_file = "test.rh.annot")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("mris_label2annot")
### * mris_label2annot

flush(stderr()); flush(stdout())

### Name: mris_label2annot
### Title: Convert Label to Annotation
### Aliases: mris_label2annot

### ** Examples

## Not run: 
##D # for freesurfer help see:
##D freesurfer::fs_help("mris_label2annot")
##D 
##D subj_dir <- freesurfer::fs_subj_dir()
##D # Split up aparc annot into labels
##D mri_annotation2label(annot_name = "aparc")
##D 
##D # get annot for colour labels
##D annot <- freesurfer::read_annotation(
##D               file.path(subj_dir,
##D                         "fsaverage5/label/rh.aparc.annot"))
##D 
##D labels <- list.files(
##D    file.path(subj_dir, "fsaverage5/label/aparc"),
##D     full.names = TRUE)
##D 
##D # Write colortable to file
##D ctab_file <- tempfile(fileext = ".ctab")
##D write_ctab(annot$colortable, ctab_file)
##D 
##D # Combine labels back into annotation
##D mris_label2annot(labels, hemisphere = "rh", ctab = ctab_file)
## End(Not run)



cleanEx()
nameEx("subject_2_ascii")
### * subject_2_ascii

flush(stderr()); flush(stdout())

### Name: subject_2_ascii
### Title: Convert subject surface files to ascii
### Aliases: subject_2_ascii

### ** Examples

## Not run: 
##D subject_2_ascii()
##D subject_2_ascii("fsaverage")
##D subject_2_ascii("bert")
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
