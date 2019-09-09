test_that("Our demo curv file can be read", {
    curvfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE)
    ct = read.fs.curv(curvfile)
    known_vertex_count = 149244

    expect_equal(length(ct), known_vertex_count)
})
