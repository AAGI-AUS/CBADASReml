library(asreml)
model <- asreml::asreml(
    fixed = yield ~ Variety,
    random = ~units,
    residual = ~ ar1v(Row):ar1(Column),
    data = shf,
    trace = FALSE
)

test_that("groups are same as agricolae::orderPvalue", {
    agricolae_groups <- c(
        "a",
        "a",
        "ab",
        "abc",
        "abcd",
        "abcde",
        "bcde",
        "bcdef",
        "bcdef",
        "bcdef",
        "cdef",
        "defg",
        "defg",
        "defg",
        "efgh",
        "fghi",
        "fghi",
        "ghij",
        "hijk",
        "ijkl",
        "ijkl",
        "ijkl",
        "jkl",
        "kl",
        "l"
    )

    classify <- "Variety"

    capture.output(
        pred <- asremlPlus::predictPlus.asreml(
            model,
            classify = classify,
            wald.tab = as.data.frame(
                asreml::wald(model, denDF = "algebraic")$Wald
            )
        )
    )

    prob.matrix <- ifelse(is.na(pred$p.differences), 1, pred$p.differences)
    treatments <- colnames(prob.matrix)
    means <- pred$predictions$predicted.value
    alpha <- 0.05

    lsdmeantab <-
        lsd_group(
            treatments,
            means,
            alpha,
            prob.matrix
        )

    expect_equal(lsdmeantab$group, agricolae_groups)
})
