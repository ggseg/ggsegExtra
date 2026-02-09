describe("read_fs_mesh", {
  it("requires FreeSurfer", {
    skip_if(freesurfer::have_fs(), "FreeSurfer installed - testing skip")
    expect_error(ggsegExtra:::read_fs_mesh(), "Freesurfer")
  })

  it("validates hemisphere argument", {
    skip_if_no_freesurfer()

    expect_error(
      ggsegExtra:::read_fs_mesh(hemisphere = "invalid"),
      "arg"
    )
  })

  it("validates surface argument", {
    skip_if_no_freesurfer()

    expect_error(
      ggsegExtra:::read_fs_mesh(surface = "invalid"),
      "arg"
    )
  })

  it("returns mesh structure", {
    skip_if_no_freesurfer()

    mesh <- ggsegExtra:::read_fs_mesh(hemisphere = "lh", surface = "inflated")

    expect_type(mesh, "list")
    expect_true(all(c("vertices", "faces") %in% names(mesh)))
    expect_s3_class(mesh$vertices, "data.frame")
    expect_s3_class(mesh$faces, "data.frame")
    expect_equal(names(mesh$vertices), c("x", "y", "z"))
    expect_equal(names(mesh$faces), c("i", "j", "k"))
  })

  it("includes metadata", {
    skip_if_no_freesurfer()

    mesh <- ggsegExtra:::read_fs_mesh(hemisphere = "rh", surface = "white")

    expect_equal(mesh$hemisphere, "rh")
    expect_equal(mesh$surface, "white")
    expect_equal(mesh$subject, "fsaverage5")
  })
})


describe("make_brain_meshes", {
  it("requires FreeSurfer", {
    skip_if(freesurfer::have_fs(), "FreeSurfer installed - testing skip")
    expect_error(ggsegExtra:::make_brain_meshes(), "Freesurfer")
  })

  it("creates meshes for all hemisphere/surface combos", {
    skip_if_no_freesurfer()

    meshes <- ggsegExtra:::make_brain_meshes(surfaces = "inflated")

    expect_s3_class(meshes, "brain_meshes")
    expect_true("lh_inflated" %in% names(meshes))
    expect_true("rh_inflated" %in% names(meshes))
  })
})


describe("read_fs_mesh surface file not found", {
  it("errors when surface file does not exist", {
    local_mocked_bindings(
      check_fs = function(...) TRUE
    )

    fake_dir <- withr::local_tempdir("fake_fs_")
    dir.create(file.path(fake_dir, "fsaverage5", "surf"), recursive = TRUE)

    expect_error(
      ggsegExtra:::read_fs_mesh(
        subject = "fsaverage5",
        hemisphere = "lh",
        surface = "inflated",
        subjects_dir = fake_dir
      ),
      "Surface file not found"
    )
  })
})
