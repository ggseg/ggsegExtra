describe("read_tractography", {
  it("errors on unsupported format", {
    tmp <- withr::local_tempfile(fileext = ".xyz")
    writeLines("dummy", tmp)

    expect_error(read_tractography(tmp), "Unsupported tractography format")
  })
})


describe("read_trk", {
  it("errors on invalid TRK file", {
    tmp <- withr::local_tempfile(fileext = ".trk")
    writeBin(charToRaw("INVALID HEADER DATA"), tmp)

    expect_error(read_trk(tmp), "Invalid TRK")
  })
})


describe("read_tck", {
  it("reads valid TCK format with header", {
    tmp <- withr::local_tempfile(fileext = ".tck")
    con <- file(tmp, "wb")
    writeLines(c(
      "mrtrix tracks",
      "datatype: Float32LE",
      "END"
    ), con)
    writeBin(as.numeric(c(1, 2, 3)), con, size = 4)
    writeBin(as.numeric(c(NaN, NaN, NaN)), con, size = 4)
    writeBin(as.numeric(c(Inf, Inf, Inf)), con, size = 4)
    close(con)

    result <- read_tck(tmp)

    expect_type(result, "list")
  })
})
