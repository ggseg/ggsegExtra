describe("re-exported conversion functions", {
  it("re-exports unify_legacy_atlases from ggseg.formats", {
    expect_identical(
      unify_legacy_atlases,
      ggseg.formats::unify_legacy_atlases
    )
  })
})
