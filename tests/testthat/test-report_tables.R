model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = oats,
    trace = FALSE
)

table <- report_tables(
    model,
    classify = "Nitrogen:Variety"
)

test_that("expected names", {
    expect_equal(
        names(table),
        c("Anova", "Nitrogen", "Variety", "Nitrogen:Variety")
    )
})

test_that("expected table", {
    expect_equal(
        table[["Nitrogen"]],
        lsd_table(model, classify = "Nitrogen")
    )
})
