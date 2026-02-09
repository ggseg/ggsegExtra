describe("read_tractography", {
  it("errors on unsupported format", {
    tmp <- withr::local_tempfile(fileext = ".xyz")
    writeLines("dummy", tmp)

    expect_error(read_tractography(tmp), "Unsupported tractography format")
  })

  it("dispatches to read_trk for .trk files", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(1L, raw(), size = 4)
    writeBin(header, con)
    writeBin(3L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6, 7, 8, 9)), con, size = 4)
    close(con)

    result <- read_tractography(tmp)

    expect_type(result, "list")
    expect_length(result, 1)
  })

  it("dispatches to read_tck for .tck files", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c("mrtrix tracks", "datatype: Float32LE", "END"), con)
    writeBin(as.numeric(c(1, 2, 3)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tractography(tmp)

    expect_type(result, "list")
  })
})


describe("read_trk", {
  it("errors on invalid TRK file", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    writeBin(charToRaw("INVALID HEADER DATA"), tmp)

    expect_error(read_trk(tmp), "Invalid TRK")
  })

  it("reads valid TRK file with one streamline", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(1L, raw(), size = 4)
    writeBin(header, con)
    writeBin(3L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6, 7, 8, 9)), con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_type(result, "list")
    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 3)
    expect_equal(colnames(result[[1]]), c("x", "y", "z"))
  })

  it("reads TRK file with multiple streamlines", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(2L, raw(), size = 4)
    writeBin(header, con)
    writeBin(2L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6)), con, size = 4)
    writeBin(2L, con, size = 4)
    writeBin(as.numeric(c(7, 8, 9, 10, 11, 12)), con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_length(result, 2)
    expect_equal(nrow(result[[1]]), 2)
    expect_equal(nrow(result[[2]]), 2)
  })

  it("handles TRK with scalars", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(1L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(1L, raw(), size = 4)
    writeBin(header, con)
    writeBin(2L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 0.5, 4, 5, 6, 0.8)), con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_length(result, 1)
    expect_equal(ncol(result[[1]]), 3)
  })

  it("handles TRK with properties", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(1L, raw(), size = 2)
    header[989:992] <- writeBin(1L, raw(), size = 4)
    writeBin(header, con)
    writeBin(2L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6)), con, size = 4)
    writeBin(as.numeric(42.0), con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 2)
  })
})


describe("read_tck", {
  it("reads valid TCK format with header", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(
      c(
        "mrtrix tracks",
        "datatype: Float32LE",
        "END"
      ),
      con
    )
    writeBin(as.numeric(c(1, 2, 3)), con, size = 4)
    writeBin(as.numeric(c(NaN, NaN, NaN)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_type(result, "list")
  })

  it("reads multiple streamlines separated by NaN", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c("mrtrix tracks", "datatype: Float32LE", "END"), con)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6)), con, size = 4)
    writeBin(as.numeric(c(NaN, NaN, NaN)), con, size = 4)
    writeBin(as.numeric(c(7, 8, 9)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_length(result, 2)
    expect_equal(nrow(result[[1]]), 2)
    expect_equal(nrow(result[[2]]), 1)
  })

  it("handles trailing streamline without NaN separator", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c("mrtrix tracks", "datatype: Float32LE", "END"), con)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 2)
  })

  it("handles Float64LE datatype", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c("mrtrix tracks", "datatype: Float64LE", "END"), con)
    writeBin(as.numeric(c(1, 2, 3)), con, size = 8)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 8)
    close(con)

    result <- read_tck(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 1)
  })

  it("breaks when file is truncated (coords length < 3)", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c("mrtrix tracks", "datatype: Float32LE", "END"), con)
    writeBin(as.numeric(c(1, 2, 3)), con, size = 4)
    writeBin(as.numeric(c(NaN, NaN, NaN)), con, size = 4)
    writeBin(as.numeric(c(7)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 1)
  })
})


describe("read_trk early break", {
  it("breaks when n_pts is zero", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(2L, raw(), size = 4)
    writeBin(header, con)
    writeBin(3L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6, 7, 8, 9)), con, size = 4)
    writeBin(0L, con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 3)
  })

  it("breaks when n_pts is negative", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    con <- file(tmp, "wb")
    header <- raw(1000)
    header[1:6] <- c(charToRaw("TRACK"), as.raw(0))
    header[37:38] <- writeBin(0L, raw(), size = 2)
    header[239:240] <- writeBin(0L, raw(), size = 2)
    header[989:992] <- writeBin(2L, raw(), size = 4)
    writeBin(header, con)
    writeBin(3L, con, size = 4)
    writeBin(as.numeric(c(1, 2, 3, 4, 5, 6, 7, 8, 9)), con, size = 4)
    writeBin(-1L, con, size = 4)
    close(con)

    result <- read_trk(tmp)

    expect_length(result, 1)
    expect_equal(nrow(result[[1]]), 3)
  })
})
