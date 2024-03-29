% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fs-utils.R
\name{mris_label2annot}
\alias{mris_label2annot}
\title{Convert Label to Annotation}
\usage{
mris_label2annot(
  labels,
  hemisphere = "rh",
  ctab,
  subject = "fsaverage5",
  subjects_dir = fs_subj_dir(),
  output_dir = subjects_dir,
  opts = NULL,
  verbose = TRUE
)
}
\arguments{
\item{labels}{label file path vector}

\item{hemisphere}{hemisphere, either "rh" or "lh"}

\item{ctab}{colourtable file}

\item{subject}{Freesurfer subject, must exist in whatever subject directory
specified or set in the environment with $SUBJECTS_DIR}

\item{subjects_dir}{Freesurfer subject directory}

\item{output_dir}{output directory path}

\item{opts}{other arguments to freesurfer command}

\item{verbose}{logical indicating to be verbose or not}
}
\description{
If you have labels rather than a full annotation
file, these can be combined with FreeSurfer's
\code{mris_label2annot}.
}
\examples{
if(freesurfer::have_fs()){
# for freesurfer help see:
freesurfer::fs_help("mris_label2annot")

subj_dir <- freesurfer::fs_subj_dir()
# Split up aparc annot into labels
mri_annotation2label(annot_name = "aparc")

# get annot for colour labels
annot <- freesurfer::read_annotation(
              file.path(subj_dir, 
                        "fsaverage5/label/rh.aparc.annot"))

labels <- list.files(
   file.path(subj_dir, "fsaverage5/label/aparc"), 
    full.names = TRUE)

# Combine them again into annot.
mris_label2annot(labels, annot$colortable)

}
}
