% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fs-utils.R
\name{mri_surf2surf_rereg}
\alias{mri_surf2surf_rereg}
\title{Re-register an annotation file}
\usage{
mri_surf2surf_rereg(
  subject,
  annot,
  hemi = c("lh", "rh"),
  target_subject = "fsaverage5",
  output_dir = file.path(fs_subj_dir(), subject, "label"),
  verbose = TRUE
)
}
\arguments{
\item{subject}{subject the original annotation file is registered to}

\item{annot}{annotation file name (as found in subjects_dir)}

\item{hemi}{hemisphere (one of "lh" or "rh")}

\item{target_subject}{subject to re-register the annotation (default fsaverage5)}

\item{output_dir}{output directory path}

\item{verbose}{logical indicating to be verbose or not}
}
\value{
nothing
}
\description{
Annotation files are subject specific.
Most are registered for fsaverage, but
we recommend using fsaverage5 for the mesh
plots in ggseg3d, as these contain a decent
balance in number of vertices for detailed
rendering and speed.
}
\examples{
if(freesurfer::have_fs()){

# For help see:
freesurfer::fs_help("mri_surf2surf")

mri_surf2surf_rereg(subject = "bert", 
                    annot = "aparc.DKTatlas",
                    target_subject = "fsaverage5")
}

}
