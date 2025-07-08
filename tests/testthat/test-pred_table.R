library(asreml)
model <- asreml(
    fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
    random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
    residual = ~ idv(units),
    data = oats,
    trace = FALSE
)

test_that("we get expected pred table for simple data", {
    p_tab <- data.frame(
        Treatment = c("Golden_rain", "Marvellous", "Victory"),
        Mean = c(104.5000, 109.7917, 97.6250),
        `Standard Error` = c(7.7975, 7.7975, 7.7975),
        `Lower CL` = c(88.9026, 94.1943, 82.0276),
        `Upper CL` = c(120.0974, 125.3891, 113.2224)
    )
    pt <- pred_table(model, classify = "Variety")
    pt[, 2] <- round(pt[, 2], 4)
    pt[, 3] <- round(pt[, 3], 4)
    pt[, 4] <- round(pt[, 4], 4)
    pt[, 5] <- round(pt[, 5], 4)

    expect_true(all(pt == p_tab))
})
