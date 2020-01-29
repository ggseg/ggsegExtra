mri_vol2surf <- function(infile , outfile,
                         hemisphere,
                         projfrac = .5,
                         verbose = TRUE){

  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mri_vol2surf")

  cmd <- paste(fs_cmd,
                "--mov", infile,
                "--o", outfile,
               "--mni152reg",
               "--hemi", hemisphere,
               "--projfrac", projfrac)

  k <- system(cmd, intern=!verbose)
  invisible(k)
}

mri_vol2label <- function(infile, label_id, hemisphere, outdir, verbose = TRUE){
  
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  outfile <- paste0(outdir, "/", hemisphere, "_", 
                    stringr::str_pad(label_id, 3, side="left", pad="0"), 
                    ".label")
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mri_vol2label")
  
  cmd <-  paste(fs_cmd,
               " --c ", infile,
               " --id ", label_id,
               " --surf fsaverage ", hemisphere,
               " --l ", outfile)
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}

mris_label2annot <- function(labels, hemisphere, ctab, outdir, verbose=TRUE){
  hemisphere <- match.arg(hemisphere, c("rh", "lh"))
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  out_file <- paste0(outdir, "/", hemisphere)
  
  labs <- paste("--l", labels, collapse=" ")
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "mris_label2annot")
  
  cmd <-  paste(fs_cmd,
                "--sd ${SUBJECTS_DIR}",
                "--s fsaverage",
                "--ctab", ctab,
                labs,
                "--h", hemisphere, 
                "--annot-path", out_file
                # "--a tmp"
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}


write_ctab <- function(x, path){
  lls <- apply(x, 1, function(c)
    ctab_line(c[1], c[2], c[3], c[4], c[5], c[6])
  )
  
  # add empty row at the end
  lls[length(lls) + 1 ] = ""
  
  k <- writeLines(lls, path)
  
  invisible(lls)
}

ctab_line <- function(idx, name, R, G, B, A){
  
  if(nchar(name) > 29){
    name <- paste0(strsplit(name, "")[[1]][1:29], collapse = "")
  }
  
  sprintf("% 3s  % -30s  % 3s % 3s % 3s % 3s", idx, name, R, G, B, A)
}

run_tcl <- function(region, indir, hemisphere, outdir, verbose = TRUE){

  labfile <- paste0(indir, "/", hemisphere, "_", region, ".label")
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  xpts <- paste0("export INLABFILE=", labfile,
                 "; export REGNAME=", region,
                 "; export TD=", outdir, "; ")

  
  tcl_script <- system.file("bash_scripts", "snapshot.tcl", package = "ggsegExtra")
  
  fs_cmd <- paste0(freesurfer::get_fs(),
                   "tksurfer")
  
  cmd <- paste(
    xpts,
    fs_cmd,
    "fsaverage ",
    hemisphere, "inflated",
    "-tcl", tcl_script,
    "-title", region
  )
  
  k <- system(cmd, intern=!verbose)
  
  invisible(k)
}
