model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = oats
)

test_that("lsd_graph creates expected outputs", {
    oats_lsd_graph_variety <- lsd_graph(model, "Variety")
    vdiffr::expect_doppelganger(
        "oats_lsd_graph_variety",
        oats_lsd_graph_variety
    )
    oats_lsd_graph_nitrogen <- lsd_graph(model, "Nitrogen")
    vdiffr::expect_doppelganger(
        "oats_lsd_graph_nitrogen",
        oats_lsd_graph_nitrogen
    )
})
