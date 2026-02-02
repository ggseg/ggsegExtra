describe("read_label_vertices", {
  it("reads vertex indices from label file", {
    label_file <- test_label_files()$lh_region1
    vertices <- ggsegExtra:::read_label_vertices(label_file)

    expect_type(vertices, "integer")
    expect_equal(length(vertices), 5)
    expect_equal(vertices, c(100L, 101L, 102L, 150L, 151L))
  })

  it("handles different label files", {
    label_file <- test_label_files()$lh_region2
    vertices <- ggsegExtra:::read_label_vertices(label_file)

    expect_equal(length(vertices), 3)
    expect_equal(vertices, c(200L, 201L, 202L))
  })

  it("handles right hemisphere labels", {
    label_file <- test_label_files()$rh_region1
    vertices <- ggsegExtra:::read_label_vertices(label_file)

    expect_equal(length(vertices), 4)
  })
})
