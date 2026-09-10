#' Extract subcortical labels from a CIFTI file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' CIFTI dense label files in grayordinate space (such as fsLR 91k) store
#' subcortical parcels as voxels rather than surface vertices, so
#' [read_cifti_annotation()] and [create_cortical_from_cifti()] cannot use
#' them. `read_cifti_subcortical()` writes those voxels to a NIfTI label volume
#' and builds the matching colour table from the CIFTI label table.
#'
#' The volume keeps the CIFTI's voxel grid and transform. Grayordinate files
#' use 2 mm MNI152NLin6Asym (FSL's MNI152), so the result can go to
#' [prepare_subcortical_mni152()] for grey-brain context, or straight to
#' [create_subcortical_from_volume()].
#'
#' @param cifti_file Path to a `.dlabel.nii` CIFTI file with subcortical
#'   voxels.
#' @param output_file Path for the NIfTI label volume. Defaults to a temporary
#'   `.nii.gz` file.
#'
#' @return A list with `volume`, the path to the written NIfTI, and `lut`, a
#'   data.frame with columns `idx`, `label`, `R`, `G`, `B` and `A` covering the
#'   labels present in the volume.
#' @seealso [prepare_subcortical_mni152()], [create_subcortical_from_volume()]
#' @export
#' @examples
#' \dontrun{
#' subcortex <- read_cifti_subcortical("atlas.dlabel.nii")
#' merged <- prepare_subcortical_mni152(
#'   input_volume = subcortex$volume,
#'   labels = subcortex$lut$idx,
#'   lut = subcortex$lut
#' )
#' atlas <- create_subcortical_from_volume(input_volume = merged)
#' }
read_cifti_subcortical <- function(
  cifti_file,
  output_file = tempfile(fileext = ".nii.gz")
) {
  rlang::check_installed(
    c("ciftiTools", "RNifti"),
    reason = "to extract subcortical labels from CIFTI files"
  )

  if (!file.exists(cifti_file)) {
    cli::cli_abort("CIFTI file not found: {.path {cifti_file}}")
  }

  cii <- ciftiTools::read_cifti(cifti_file)

  if (is.null(cii$data$subcort)) {
    cli::cli_abort(c(
      "{.path {cifti_file}} has no subcortical voxels.",
      "i" = "Use {.fn create_cortical_from_cifti} for surface-only files."
    ))
  }

  volume <- cifti_subcortical_volume(cii)
  image <- RNifti::asNifti(volume)
  RNifti::sform(image) <- structure(cii$meta$subcort$trans_mat, code = 4L)
  RNifti::writeNifti(image, output_file)

  list(
    volume = output_file,
    lut = cifti_subcortical_lut(cii, unique(volume[volume != 0L]))
  )
}

#' @noRd
cifti_subcortical_volume <- function(cii) {
  mask <- cii$meta$subcort$mask
  volume <- array(0L, dim = dim(mask))
  volume[mask] <- as.integer(cii$data$subcort[, 1])
  volume
}

#' @noRd
cifti_subcortical_lut <- function(cii, keys) {
  label_table <- cifti_label_table(cii)
  label_table <- label_table[label_table$key %in% keys, ]

  data.frame(
    idx = label_table$key,
    label = label_table$name,
    R = as.integer(round(label_table$red * 255)),
    G = as.integer(round(label_table$green * 255)),
    B = as.integer(round(label_table$blue * 255)),
    A = 0L
  )
}

#' @noRd
warn_dropped_cifti_subcortex <- function(cii, cifti_file) {
  subcort <- cii$data$subcort
  if (is.null(subcort)) {
    return(invisible(0L))
  }

  n_labelled <- sum(subcort[, 1] != 0)
  if (n_labelled > 0) {
    cli::cli_warn(c(
      "{.path {cifti_file}} has {n_labelled} labelled subcortical \\
       voxel{?s}, which the cortical pipeline ignores.",
      "i" = "Extract them with {.fn read_cifti_subcortical} and build a \\
             subcortical atlas with {.fn create_subcortical_from_volume}."
    ))
  }
  invisible(n_labelled)
}
