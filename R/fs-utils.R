mri_vol2surf <- function(infile , outfile,
                         opts = c("--mni152reg", "--hemi rh", "--projfrac 0.5"),
                         verbose = TRUE){
  
  cmd <- paste0(freesurfer::get_fs(), "mri_vol2surf",
         " --mov ", infile,
         " --o ", outfile,
         " ", paste0(opts, collapse=" "))

  k <- system(cmd, intern=!verbose)
  invisible(k)
}
