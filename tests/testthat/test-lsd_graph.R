test_that("lsd_graph creates expected output for Variety", {
    skip_if_no_asreml()
    vdiffr::expect_doppelganger(
        "oats_lsd_graph_variety",
        lsd_graph(get_oats_model(), "Variety")
    )
})

test_that("lsd_graph creates expected output for Nitrogen", {
    skip_if_no_asreml()
    vdiffr::expect_doppelganger(
        "oats_lsd_graph_nitrogen",
        lsd_graph(get_oats_model(), "Nitrogen")
    )
})
