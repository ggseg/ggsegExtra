devtools::load_all(".")

surfs <- expand.grid(
  hemisphere = c("lh", "rh"),
  surface = c("inflated", "pial"),
  stringsAsFactors = FALSE
)

meshes <- mapply(
  get_brain_mesh,
  hemisphere = surfs$hemisphere,
  surface = surfs$surface,
  MoreArgs = list(
    subject = "fsaverage5",
    subjects_dir = fs_subj_dir()
  ),
  SIMPLIFY = FALSE
)
names(meshes) <- apply(surfs, 1, paste, collapse = "_")

for (k in names(meshes)) {
  assign(k, meshes[[k]])
  usethis::use_data(eval(parse(text = k)))
}
