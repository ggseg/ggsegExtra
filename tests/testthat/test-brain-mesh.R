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


describe("atlas_to_vertex_colors", {
  it("errors for non-brain_atlas input", {
    mock_mesh <- list(vertices = data.frame(x = 1:10, y = 1:10, z = 1:10))

    expect_error(
      atlas_to_vertex_colors("not_atlas", mock_mesh),
      "brain_atlas"
    )
  })

  it("errors when atlas lacks vertices", {
    mock_atlas <- structure(
      list(
        data = list(sf = NULL)
      ),
      class = "brain_atlas"
    )
    mock_mesh <- list(vertices = data.frame(x = 1:10, y = 1:10, z = 1:10))

    expect_error(
      atlas_to_vertex_colors(mock_atlas, mock_mesh),
      "vertices"
    )
  })

  it("errors when atlas lacks colour", {
    mock_atlas <- structure(
      list(
        data = list(
          vertices = data.frame(
            label = "test",
            vertices = I(list(1:5))
          )
        )
      ),
      class = "brain_atlas"
    )
    mock_mesh <- list(vertices = data.frame(x = 1:10, y = 1:10, z = 1:10))

    expect_error(
      atlas_to_vertex_colors(mock_atlas, mock_mesh),
      "colour"
    )
  })

  it("returns color vector matching mesh vertices", {
    mock_atlas <- structure(
      list(
        data = data.frame(
          hemi = c("left", "left"),
          label = c("lh_region1", "lh_region2"),
          colour = c("#FF0000", "#00FF00"),
          stringsAsFactors = FALSE
        )
      ),
      class = "brain_atlas"
    )
    mock_atlas$data$vertices <- list(c(0L, 1L, 2L), c(3L, 4L))
    mock_mesh <- list(vertices = data.frame(x = 1:10, y = 1:10, z = 1:10))

    result <- atlas_to_vertex_colors(mock_atlas, mock_mesh, hemisphere = "left")

    expect_type(result, "character")
    expect_length(result, 10)
    expect_equal(result[1], "#FF0000")
    expect_equal(result[4], "#00FF00")
  })

  it("uses na_colour for unmapped vertices", {
    mock_atlas <- structure(
      list(
        data = data.frame(
          hemi = "left",
          label = "lh_region1",
          colour = "#FF0000",
          stringsAsFactors = FALSE
        )
      ),
      class = "brain_atlas"
    )
    mock_atlas$data$vertices <- list(c(0L, 1L))
    mock_mesh <- list(vertices = data.frame(x = 1:10, y = 1:10, z = 1:10))

    result <- atlas_to_vertex_colors(
      mock_atlas, mock_mesh,
      hemisphere = "left",
      na_colour = "#AAAAAA"
    )

    expect_equal(result[3], "#AAAAAA")
    expect_equal(result[10], "#AAAAAA")
  })
})
