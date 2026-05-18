skip_on_cran()
library(asreml)
library(glmmTMB)

test_that("we get expected pred table for simple data with ASReml", {
    mod1 <- asreml(
        fixed = yield ~ Variety + Nitrogen + Variety:Nitrogen,
        random = ~ idv(Blocks) + idv(Blocks):idv(Wplots),
        residual = ~ idv(units),
        data = oats,
        trace = FALSE
    )
    p_tab <- data.frame(
        Treatment = c("Golden_rain", "Marvellous", "Victory"),
        Mean = c(104.5000, 109.7917, 97.6250),
        `Standard Error` = c(7.7975, 7.7975, 7.7975),
        `Lower CL` = c(87.1260, 92.4177, 80.2510),
        `Upper CL` = c(121.8740, 127.1657, 114.9990)
    )
    pt <- pred_table(mod1, classify = "Variety")
    pt[, 2] <- round(pt[, 2], 4)
    pt[, 3] <- round(pt[, 3], 4)
    pt[, 4] <- round(pt[, 4], 4)
    pt[, 5] <- round(pt[, 5], 4)

    expect_true(all(pt == p_tab))
})

test_that("we get expected pred table for simple data with glmmTMB", {
    mod2 <- glmmTMB(
        count ~ spp,
        data = Salamanders,
        family = nbinom2
    )
    p_tab <- data.frame(
        Treatment = c("GP", "PR", "DM", "EC-A", "EC-L", "DES-L", "DF"),
        Mean = c(1.1739, 0.2935, 1.4783, 0.5435, 2.1848, 2.3152, 1.2717),
        `Standard Error` = c(
            0.2240, 0.0744, 0.2746, 0.1180, 0.3917, 0.4132, 0.2403
        ),
        `Lower CL` = c(0.8076, 0.1786, 1.0271, 0.3551, 1.5375, 1.6318, 0.8781),
        `Upper CL` = c(1.7064, 0.4822, 2.1276, 0.8319, 3.1046, 3.2849, 1.8418)
    )
    pt <- pred_table(mod2, classify = "spp")
    pt[, 2] <- round(pt[, 2], 4)
    pt[, 3] <- round(pt[, 3], 4)
    pt[, 4] <- round(pt[, 4], 4)
    pt[, 5] <- round(pt[, 5], 4)

    expect_true(all(pt == p_tab))
})
