describe("is_volume_file", {
  it("detects MGZ files", {
    expect_true(is_volume_file("atlas.mgz"))
    expect_true(is_volume_file("/path/to/aseg.mgz"))
  })

  it("detects NIfTI files", {
    expect_true(is_volume_file("atlas.nii"))
    expect_true(is_volume_file("atlas.nii.gz"))
    expect_true(is_volume_file("/path/to/data.nii.gz"))
  })

  it("rejects non-volume files", {
    expect_false(is_volume_file("atlas.txt"))
    expect_false(is_volume_file("atlas.label"))
    expect_false(is_volume_file("atlas.png"))
  })

  it("is case insensitive", {
    expect_true(is_volume_file("atlas.MGZ"))
    expect_true(is_volume_file("atlas.NII"))
    expect_true(is_volume_file("atlas.NII.GZ"))
  })
})

describe("read_volume", {
  it("errors on missing file", {
    expect_error(
      read_volume("/nonexistent/file.mgz"),
      "not found"
    )
  })

  it("errors on unsupported format", {
    tmp <- withr::local_tempfile(fileext = ".txt")
    writeLines("test", tmp)

    expect_error(
      read_volume(tmp),
      "Unsupported volume format"
    )
  })

  it("reads MGZ files", {
    skip_if_no_freesurfer()

    mgz_file <- test_mgz_file()
    skip_if(!file.exists(mgz_file), "Test MGZ file not found")

    vol <- read_volume(mgz_file)
    expect_true(is.array(vol))
    expect_equal(length(dim(vol)), 3)
  })

  it("reads NIfTI files when RNifti available", {
    skip_if_not_installed("RNifti")

    tmp <- withr::local_tempfile(fileext = ".nii.gz")
    vol <- array(1:27, dim = c(3, 3, 3))
    RNifti::writeNifti(vol, tmp)

    result <- read_volume(tmp)
    expect_true(is.array(result))
    expect_equal(dim(result), c(3, 3, 3))
  })
})


describe("read_ply_mesh", {
  it("converts PLY object to mesh format", {
    mock_ply <- list(
      vb = matrix(
        c(
          0,
          1,
          0,
          1,
          0,
          0,
          1,
          1,
          0,
          0,
          0,
          0,
          1,
          1,
          1,
          1
        ),
        nrow = 4,
        byrow = TRUE
      ),
      it = matrix(
        c(
          1,
          1,
          2,
          3,
          3,
          4
        ),
        nrow = 3,
        byrow = TRUE
      )
    )

    result <- read_ply_mesh(mock_ply)

    expect_type(result, "list")
    expect_named(result, c("vertices", "faces"))
    expect_s3_class(result$vertices, "data.frame")
    expect_s3_class(result$faces, "data.frame")
    expect_equal(nrow(result$vertices), 4)
    expect_equal(nrow(result$faces), 2)
    expect_equal(names(result$vertices), c("x", "y", "z"))
    expect_equal(names(result$faces), c("i", "j", "k"))
  })

  it("extracts correct vertex coordinates", {
    mock_ply <- list(
      vb = matrix(
        c(
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          1,
          1,
          1
        ),
        nrow = 4,
        byrow = TRUE
      ),
      it = matrix(c(1, 2, 3), nrow = 3)
    )

    result <- read_ply_mesh(mock_ply)

    expect_equal(result$vertices$x, c(1, 2, 3))
    expect_equal(result$vertices$y, c(4, 5, 6))
    expect_equal(result$vertices$z, c(7, 8, 9))
  })

  it("extracts correct face indices", {
    mock_ply <- list(
      vb = matrix(1:12, nrow = 4),
      it = matrix(
        c(
          1,
          2,
          2,
          3,
          3,
          1
        ),
        nrow = 3,
        byrow = TRUE
      )
    )

    result <- read_ply_mesh(mock_ply)

    expect_equal(result$faces$i, c(1, 2))
    expect_equal(result$faces$j, c(2, 3))
    expect_equal(result$faces$k, c(3, 1))
  })
})


describe("read_ctab", {
  it("reads color table from file", {
    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    result <- read_ctab(lut_file)

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("idx", "label", "R", "G", "B", "A"))
    expect_true(nrow(result) > 0)
  })
})


describe("write_ctab", {
  it("writes color table to file", {
    ctab <- data.frame(
      idx = c(1, 2, 3),
      label = c("Region1", "Region2", "Region3"),
      R = c(255, 0, 0),
      G = c(0, 255, 0),
      B = c(0, 0, 255),
      A = c(0, 0, 0)
    )

    tmp <- withr::local_tempfile(fileext = ".txt")
    write_ctab(ctab, tmp)

    expect_true(file.exists(tmp))

    read_back <- read_ctab(tmp)
    expect_equal(nrow(read_back), 3)
    expect_equal(read_back$idx, ctab$idx)
  })

  it("truncates long label names to 29 characters", {
    long_label <- paste(rep("a", 40), collapse = "")
    ctab <- data.frame(
      idx = 1,
      label = long_label,
      R = 255, G = 0, B = 0, A = 0
    )

    tmp <- withr::local_tempfile(fileext = ".txt")
    write_ctab(ctab, tmp)

    content <- readLines(tmp)
    expect_true(all(nchar(content) < 60))
  })
})


describe("is_ctab", {
  it("returns TRUE for valid color table", {
    ctab <- data.frame(
      idx = 1:3,
      label = c("a", "b", "c"),
      R = c(255, 0, 0),
      G = c(0, 255, 0),
      B = c(0, 0, 255),
      A = c(0, 0, 0)
    )

    expect_true(is_ctab(ctab))
  })

  it("returns FALSE for non-data.frame", {
    expect_false(is_ctab(list(idx = 1, label = "a")))
    expect_false(is_ctab("not a data.frame"))
    expect_false(is_ctab(NULL))
  })

  it("returns FALSE for missing columns", {
    partial <- data.frame(idx = 1, label = "a", R = 255)
    expect_false(is_ctab(partial))
  })
})


describe("get_ctab", {
  it("reads and adds hex colors from file path", {
    lut_file <- test_lut_file()
    skip_if(!file.exists(lut_file), "Test LUT file not found")

    result <- get_ctab(lut_file)

    expect_true("color" %in% names(result))
    expect_true("roi" %in% names(result))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result$color)))
  })

  it("accepts data.frame input", {
    ctab <- data.frame(
      idx = c(1, 2),
      label = c("Region1", "Region2"),
      R = c(255, 0),
      G = c(0, 255),
      B = c(0, 0),
      A = c(0, 0)
    )

    result <- get_ctab(ctab)

    expect_equal(result$color, c("#FF0000", "#00FF00"))
    expect_equal(result$roi, c("0001", "0002"))
  })

  it("errors for invalid color table format", {
    invalid <- data.frame(x = 1, y = 2)

    expect_error(get_ctab(invalid), "correct format")
  })
})


describe("read_label_vertices", {
  it("returns empty integer for empty file", {
    tmp <- withr::local_tempfile(fileext = ".label")
    writeLines("", tmp)

    expect_warning(
      result <- read_label_vertices(tmp),
      "Empty or malformed"
    )

    expect_type(result, "integer")
    expect_length(result, 0)
  })

  it("handles file with comment header", {
    tmp <- withr::local_tempfile(fileext = ".label")
    content <- c(
      "#!ascii label from subject",
      "3",
      "100  0.0  0.0  0.0  0.0",
      "101  1.0  0.0  0.0  0.0",
      "102  2.0  0.0  0.0  0.0"
    )
    writeLines(content, tmp)

    result <- read_label_vertices(tmp)

    expect_equal(result, c(100L, 101L, 102L))
  })

  it("handles file without comment header", {
    tmp <- withr::local_tempfile(fileext = ".label")
    content <- c(
      "2",
      "50  0.0  0.0  0.0  0.0",
      "51  1.0  0.0  0.0  0.0"
    )
    writeLines(content, tmp)

    result <- read_label_vertices(tmp)

    expect_equal(result, c(50L, 51L))
  })

  it("handles zero vertices gracefully", {
    tmp <- withr::local_tempfile(fileext = ".label")
    content <- c(
      "#!ascii label",
      "0"
    )
    writeLines(content, tmp)

    result <- read_label_vertices(tmp)

    expect_length(result, 0)
  })
})


describe("write_dpv and read_dpv", {
  it("round-trips vertex and face data", {
    vertices <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 0))
    faces <- data.frame(i = 1, j = 2, k = 3)

    tmp <- withr::local_tempfile(fileext = ".dpv")
    write_dpv(tmp, vertices, faces)

    result <- read_dpv(tmp)

    expect_true(is.list(result))
    expect_true(all(c("vertices", "faces") %in% names(result)))
  })

  it("converts 1-indexed faces to 0-indexed", {
    vertices <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 0))
    faces <- data.frame(i = 1, j = 2, k = 3)

    tmp <- withr::local_tempfile(fileext = ".dpv")
    write_dpv(tmp, vertices, faces)

    content <- readLines(tmp)
    last_line <- content[length(content)]
    face_values <- as.numeric(strsplit(trimws(last_line), " ")[[1]])

    expect_true(all(face_values[1:3] >= 0))
    expect_true(all(face_values[1:3] < 3))
  })
})
