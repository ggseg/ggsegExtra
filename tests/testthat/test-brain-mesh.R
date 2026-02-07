describe("get_brain_mesh", {
  it("requires FreeSurfer", {
    skip_if(freesurfer::have_fs(), "FreeSurfer installed - testing skip")
    expect_error(get_brain_mesh(), "Freesurfer")
  })

  it("validates hemisphere argument", {
    skip_if_no_freesurfer()

    expect_error(
      get_brain_mesh(hemisphere = "invalid"),
      "arg"
    )
  })

  it("validates surface argument", {
    skip_if_no_freesurfer()

    expect_error(
      get_brain_mesh(surface = "invalid"),
      "arg"
    )
  })

  it("returns mesh structure", {
    skip_if_no_freesurfer()

    mesh <- get_brain_mesh(hemisphere = "lh", surface = "inflated")

    expect_type(mesh, "list")
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_s3_class(mesh$vertices, "data.frame")
    expect_s3_class(mesh$faces, "data.frame")
    expect_equal(names(mesh$vertices), c("x", "y", "z"))
    expect_equal(names(mesh$faces), c("i", "j", "k"))
  })

  it("includes metadata", {
    skip_if_no_freesurfer()

    mesh <- get_brain_mesh(hemisphere = "rh", surface = "white")

    expect_equal(mesh$hemisphere, "rh")
    expect_equal(mesh$surface, "white")
    expect_equal(mesh$subject, "fsaverage5")
  })
})


describe("make_brain_meshes", {
  it("requires FreeSurfer", {
    skip_if(freesurfer::have_fs(), "FreeSurfer installed - testing skip")
    expect_error(make_brain_meshes(), "Freesurfer")
  })

  it("creates meshes for all hemisphere/surface combos", {
    skip_if_no_freesurfer()

    meshes <- make_brain_meshes(surfaces = "inflated")

    expect_s3_class(meshes, "brain_meshes")
    expect_true("lh_inflated" %in% names(meshes))
    expect_true("rh_inflated" %in% names(meshes))
  })
})


