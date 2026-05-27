test_that("expected names", {
    skip_if_no_asreml()
    table <- report_tables(get_oats_model(), classify = "Nitrogen:Variety")
    expect_named(table, c("Anova", "Nitrogen", "Variety", "Nitrogen:Variety"))
})

test_that("expected Nitrogen table", {
    skip_if_no_asreml()
    table <- report_tables(get_oats_model(), classify = "Nitrogen:Variety")
    expect_identical(
        table[["Nitrogen"]],
        lsd_table(get_oats_model(), classify = "Nitrogen")
    )
})
